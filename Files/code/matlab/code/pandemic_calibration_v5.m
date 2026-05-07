%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & VALIDATION (V5, aligned with V4 spec)
%
%  STEP 1 ONLY: Calibration + Validation.
%  Scenarios, iLQR solver, and counterfactuals are in separate files.
%
%  CHANGES from V4 (06.05.2026):
%    1. Coefficients updated to V4 frozen values
%       (output: N=380; debt: N=494 with sample extension to Q4.2022).
%    2. Output equation: psi*S*y_lag1 interaction REMOVED (V3 deletion).
%       alpha_F_CP REMOVED (V3 deletion). The eta_p channel is now the
%       SOLE CP persistence channel and enters as a recession spline:
%           eta_p * F^CP_{k-2} * y_{k-1} * I(y_{k-1} < 0)
%    3. Debt equation: F^DI now enters at LAG 1 (not contemporaneous).
%       Linear year trend (-0.250 per year) added.
%    4. CP three-way decomposition uses CSV columns directly:
%       F_CP_above_3, F_CP_loans, F_CP_guar_adj. The 50/50 below-the-line
%       proxy is dropped.
%    5. NEW informative result: kappa_above_q75 = 1.009 (tail-conditional)
%       loaded for piecewise iLQR sensitivity, NOT used in main forward
%       roll.
%
%  SPECIFICATION (matches paper Tables 3 col 3 / V4 main, and Table 8 col 5):
%    Output (V4):
%       y_{ik} = rho_y*y_{i,k-1}
%              + alpha_S*S_{ik}
%              + eta_p*F^CP_{i,k-2}*y_{i,k-1}*I(y_{i,k-1}<0)
%              + alpha_DI*F^DI_{i,k-2}
%              + beta_d*d_{ik}
%              + mu_i^y + delta_k^y
%
%    Debt (V4, Country FE only, linear year trend):
%       b_{ik} = -gamma_y*y_{ik}
%              + kappa_above*F^CP,above_{ik}
%              + kappa_loans*F^CP,loans_{ik}
%              + kappa_guar*F^CP,guar_{ik}
%              + kappa_DI*F^DI_{i,k-1}                    [LAG 1]
%              + phi_t * (year(k) - 2019)                  [year trend]
%              + mu_i^b
%        (b is first-differenced quarterly change, pp of 2019 GDP)
%
%  STATE-SPACE REPRESENTATION
%  --------------------------
%  Forecasting y_{k+1} from info at time k. Re-indexing by t = k+1:
%    y_{k+1} = rho_y*y_k + alpha_S*S_k
%              + eta_p*F^CP_{k-1}*y_k*I(y_k<0)
%              + alpha_DI*F^DI_{k-1}
%              + beta_d*d_k
%              + mu_i^y + eps_y(k+1)
%
%  State w_k = F^CP_{k-1}, z_k = F^DI_{k-1}.
%
%  State:   x = (y, b, w, z)   where w = F^CP_{k-1}, z = F^DI_{k-1}
%  Control: u = (F^CP_k, F^DI_k)
%
%  UNIT CONVENTION
%  ---------------
%  ALL variables in percentage points (pp), matching the regression.
%  Coefficients used exactly as reported in paper tables.
%
%  IMPORTANT: Sub-component CP variables (F_CP_above_3, F_CP_loans,
%  F_CP_guar_adj) are stored in CSV scaled by *100 relative to F_CP.
%  We rescale on import to match F_CP units (divide by 100).
%
%  FIXED EFFECTS
%  -------------
%  Country FE values below are PLACEHOLDERS from N=380 V3 spec. They
%  MUST be re-extracted from the V4 R regression. See snippet at the
%  bottom of this file.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V5 (V4 spec aligned) ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS — V4 FROZEN VALUES
% =========================================================================

% --- Output equation (V4 frozen, N=380, Q1.2020-Q2.2022) ---
rho_y      =  0.408;    % baseline persistence (was 0.311 in V3)
eta_p      = -0.010;    % F^CP_lag2 * y_lag1 * I(y_lag1<0); recession spline
alpha_S    = -0.041;    % S_mean_tw level (was -0.034)
alpha_F_DI =  0.193;    % F^DI_lag2 level (was 0.185)
beta_fear  = -0.023;    % p_proj_all_ages (was -0.021)
% NB: psi (S*y interaction) and alpha_F_CP (CP level) REMOVED in V3.

% --- Debt equation (V4 main, N=494, Q4.2019-Q4.2022) ---
r_int        =  0.001;   % real quarterly interest rate (near zero in sample)
gamma_y      =  0.188;   % automatic stabilizer (coef on y_ik is -gamma_y)
kappa_above  =  0.434;   % above-the-line CP (wage subsidies, grants)
kappa_loans  =  0.365;   % government loans
kappa_guar   =  0.107;   % credit guarantees (35% take-up adj, n.s.)
kappa_F_DI   =  0.427;   % DI cost at LAG 1 (p=0.08); was 0.181 (contemp, n.s.)
phi_t        = -0.250;   % linear year trend per year_only

% --- Pooled CP for sensitivity / iLQR alternative ---
kappa_CP_pooled = 0.173;  % V4 main pooled spec
kappa_CP_lo     = 0.134;  % narrow window lower bound (across 10 specs)
kappa_CP_hi     = 0.241;  % net-effect upper bound

% --- Tail-conditional cost (informative; NOT used in main forward roll) ---
kappa_above_q75 = 1.009;  % above-the-line at tau=0.75 (bootstrap p<0.001)
% Use for piecewise / tail-conditional iLQR sensitivity:
%   kappa_above_eff(F^above) = 0.434 if F^above < threshold else 1.009.

% --- Health spending (calibrated, full pass-through) ---
kappa_H      = 1.000;

% --- Country ISO list ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Quarter FE for OUTPUT (pp), N=380 sample, ref delta_{Q1.2020} = 0 ---
qfe_pp = [ 0.000, -7.626,  2.585,  0.849,  1.777, ...
           2.149,  2.121,  2.602,  1.399,  1.059];

% Test: setze Quarter FE auf 0
%qfe_pp = zeros(1, 10);
%eps_y_vec = [0, qfe_pp];
%P.eps_y_vec = eps_y_vec;


% --- Country FE: Output (pp),  --- 
cfe_y_val = [-0.3639, -2.1325, -0.7051, -1.7267, +0.2141, -0.3140, +0.3177, -1.2524, ...
             -3.9365, -2.1263, -0.5655, -5.3127, -2.0566, -2.0269, -2.5061, -3.8640, ...
             -1.3358, -2.8375, +5.5229, -5.2402, +0.0604, -1.1385, -1.4375, -0.5740, ...
             -0.4405, +0.5560, -1.2271, -3.4843, -0.4524, -0.1301, -1.8321, -1.6764, ...
             -3.8525, -1.0973, -2.4952, -0.3698, +2.7234, -0.2353];
%To check if the model also works without FE sub in: cfe_y_val = zeros(1,38)
% --- Country FE: Debt (pp), V4 N=494 demeaned ---
cfe_b_val = [-0.6462, -0.5190, -0.5147, -0.1395, -0.3089, -0.9431, +0.6842, +0.0991, ...
             -0.9315, -0.9908, -1.4190, -1.1902, -0.5864, -0.5923, -0.8557, -0.9631, ...
             -0.7381, -0.9150, +0.6430, -1.0453, -0.0485, -0.7098, -0.8563, -0.4437, ...
             -0.5257, +0.2124, +0.0169, -1.4596, -0.3687, -0.0728, +0.3738, -0.8068, ...
             -1.8202, -0.2072, -0.8612, -0.4815, +0.1780, +0.4707];
%To check if the model also works without FE sub in: cfe_b_val = zeros(1,38)
% Add grand mean (R regression has implicit intercept absorbed in FE)
b_grand_mean =0.0;   % per-quarter shift %add 0.11 for perfect fir
cfe_b_val = cfe_b_val + b_grand_mean;
% --- Year index, centered (trend contributes 0 cumulatively) ---
phi_t = -0.250;
year_idx_raw = [1, 1, 1, 1, 2, 2, 2, 2, 3, 3];
year_idx_vec = year_idx_raw - mean(year_idx_raw);

% --- Objective weights ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions ---
N = 10;  K_act = 10;  nx = 4;  nu = 2;

% --- Build eps_y vector in pp ---
eps_y_vec = [0, qfe_pp];
if length(eps_y_vec) < N+1
    eps_y_vec(end+1:N+1) = 0;
end

% --- Lookup maps ---
cfe_y_map  = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map  = containers.Map(cfe_iso, cfe_b_val);

% --- Control bounds (pp of GDP) ---
u_lo = [0; 0];  u_hi = [20.0; 10.0];
% --- Pack parameters ---
P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, ...
    'eta_p',eta_p, 'alpha_F_DI',alpha_F_DI, 'beta_fear',beta_fear, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, 'kappa_F_DI',kappa_F_DI, ...
    'kappa_H',kappa_H, 'phi_t',phi_t, ...
    'kappa_CP_pooled',kappa_CP_pooled, ...
    'kappa_CP_lo',kappa_CP_lo, 'kappa_CP_hi',kappa_CP_hi, ...
    'kappa_above_q75',kappa_above_q75, ...
    'eps_y_vec',eps_y_vec, 'year_idx_vec',year_idx_vec, ...
    'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   nx = %d (y, b, w, z)\n', N, K_act, nx);
fprintf('  Units: ALL variables in percentage points (pp)\n\n');
fprintf('  Output equation coefficients (V4 frozen, N=380):\n');
fprintf('    rho_y      = %+.4f\n', rho_y);
fprintf('    eta_p      = %+.4f   (F^CP_lag2*y_lag1*I(y_lag1<0), recession spline)\n', eta_p);
fprintf('    alpha_S    = %+.4f   (S level; negative = contractionary)\n', alpha_S);
fprintf('    alpha_DI   = %+.4f   (F^DI_lag2 level)\n', alpha_F_DI);
fprintf('    beta_d     = %+.4f   (excess mortality)\n', beta_fear);
fprintf('    [psi and alpha_F_CP REMOVED in V3]\n');

% Effective persistence at OECD averages (recession only)
S_avg  = 40.19;
FCP_avg = 1.31;
rho_eff_recession = rho_y + eta_p*FCP_avg;
fprintf('\n  Effective y persistence at OECD averages (recession, y_lag1<0):\n');
fprintf('    rho_eff = rho_y + eta_p*F^CP_avg = %.4f + (%.4f)*%.2f = %.4f\n', ...
    rho_y, eta_p, FCP_avg, rho_eff_recession);
fprintf('    (At y_lag1>=0: rho_eff = rho_y = %.4f, spline inactive)\n', rho_y);

fprintf('\n  Debt equation coefficients (V4 main, N=494):\n');
fprintf('    gamma_y       = %+.3f   (automatic stabilizer)\n', gamma_y);
fprintf('    kappa_above   = %+.3f   (CP above-the-line)\n', kappa_above);
fprintf('    kappa_loans   = %+.3f   (CP loans)\n', kappa_loans);
fprintf('    kappa_guar    = %+.3f   (CP guarantees adj 35%%, n.s.)\n', kappa_guar);
fprintf('    kappa_DI      = %+.3f   (DI LAG 1, p=0.08)\n', kappa_F_DI);
fprintf('    phi_t         = %+.3f   (linear year trend)\n', phi_t);
fprintf('    kappa_H       = %+.3f   (calibrated, full pass-through)\n', kappa_H);
fprintf('  Pooled CP alternative: kappa_CP = %.3f [%.3f, %.3f] across 10 specs\n', ...
    kappa_CP_pooled, kappa_CP_lo, kappa_CP_hi);
fprintf('  Tail-conditional: kappa_above_q75 = %.3f (informative, piecewise iLQR)\n\n', ...
    kappa_above_q75);


%% ========================================================================
%  LOAD DATA
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');

qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022'};
qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S          = zeros(1,N);
    cdata(i).FCP        = zeros(1,N);
    cdata(i).FCP_above  = zeros(1,N);
    cdata(i).FCP_loans  = zeros(1,N);
    cdata(i).FCP_guar   = zeros(1,N);
    cdata(i).FDI        = zeros(1,N);
    cdata(i).FH         = zeros(1,N);
    cdata(i).y          = zeros(1,N);
    cdata(i).theta      = zeros(1,N);
    cdata(i).b          = zeros(1,N);
    cdata(i).d          = zeros(1,N);
    cdata(i).mu_y = 0;  cdata(i).mu_b = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end

    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k)           = row.S_mean_tw;
        cdata(i).FCP(k)         = row.F_CP;
        % All CP variables in CSV are on the same scale (pp of 2019 GDP).
        % Verified: F_CP = F_CP_above_3 + F_CP_loans + F_CP_guar (face value).
        cdata(i).FCP_above(k)   = row.F_CP_above_3;
        cdata(i).FCP_loans(k)   = row.F_CP_loans;
        cdata(i).FCP_guar(k)    = row.F_CP_guar_adj;  % 35% take-up adjusted
        cdata(i).FDI(k)         = row.F_DI;
        if ismember('F_H', T.Properties.VariableNames) && ~ismissing(row.F_H)
            cdata(i).FH(k)      = row.F_H;
        end
        cdata(i).y(k)           = row.y_t_pct;
        cdata(i).theta(k)       = row.theta_pct;
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b(k)       = row.debt_dR;
        end
        if ismember('excess_mortality', T.Properties.VariableNames) ...
                && ~ismissing(row.excess_mortality)
            cdata(i).d(k)       = row.excess_mortality;
        end
    end
end
fprintf('  %d countries x %d quarters (all variables in pp)\n\n', n_c, N);

% Sanity check: F_CP should equal sum of components (face value, within rounding)
recon_err = zeros(n_c,1);
for i = 1:n_c
    % Reconstruct face-value F_CP from components (guar_adj at 35% -> /0.35)
    recon = cdata(i).FCP_above + cdata(i).FCP_loans + cdata(i).FCP_guar/0.35;
    recon_err(i) = max(abs(cdata(i).FCP - recon));
end
fprintf('  CP decomposition reconciliation: max abs error = %.4f pp\n', max(recon_err));
fprintf('  (F_CP should equal above + loans + guar/0.35; tiny error => OK)\n\n');


%% ========================================================================
%  STEP 1: VALIDATION — Forward-simulate with observed controls
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation (N = %d, nx = %d)\n', N, nx);
fprintf('========================================\n');

for i = 1:n_c
    xs = forward_roll(cdata(i).FCP, ...
        cdata(i).FCP_above, cdata(i).FCP_loans, cdata(i).FCP_guar, ...
        cdata(i).FDI, cdata(i).FH, ...
        cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, P);
    cdata(i).sim_y     = xs(1, 2:end);
    cdata(i).sim_b     = xs(2, 2:end);
    cdata(i).obs_b_cum = cumsum(cdata(i).b);
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y(1:K_act) - cdata(i).y(1:K_act)).^2));
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b(1:K_act) - cdata(i).obs_b_cum(1:K_act)).^2));
end

fprintf('  Output RMSE (k=1:%d) — Median: %.2f pp   Mean: %.2f pp\n', ...
    K_act, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d) — Median: %.2f pp   Mean: %.2f pp\n\n', ...
    K_act, median([cdata.rmse_b]), mean([cdata.rmse_b]));

% --- OECD median trajectory ---
fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s %9s\n', 'Quarter', 'Observed', 'Model', 'rho_eff');
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    rho_effs = zeros(n_c, 1);
    for i = 1:n_c
        wk = 0; if k >= 3, wk = cdata(i).FCP(k-2); end
        % Recession-only spline: only active when y_lag1 < 0
        y_lag1 = 0; if k >= 2, y_lag1 = cdata(i).y(k-1); end
        spline_active = (y_lag1 < 0);
        rho_effs(i) = P.rho_y + spline_active * P.eta_p * wk;
    end
    fprintf('  %8s %+9.2f %+9.2f %9.3f\n', qlbl{k}, ...
        median(obs_k), median(sim_k), median(rho_effs));
end

% --- Fiscal contribution ---
fprintf('\n  Fiscal contribution (Full - NoFiscal, cum %dQ):\n', K_act);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), ...
        zeros(1,N), zeros(1,N), zeros(1,N), ...
        zeros(1,N), cdata(i).FH, ...
        cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, P);
    diffs(i) = sum(cdata(i).sim_y(1:K_act)) - sum(xs_nof(1, 2:K_act+1));
end
fprintf('    Mean:   %+.2f pp\n', mean(diffs));
fprintf('    Median: %+.2f pp\n', median(diffs));
fprintf('    F > 0:  %d / %d\n', sum(diffs > 0), n_c);


%% ========================================================================
%  STEP 1b: NON-TARGETED MOMENTS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1b: Non-Targeted Moments\n');
fprintf('========================================\n');

% --- (1) Cross-country SD ---
fprintf('\n  (1) Cross-Country SD of Output Gap:\n');
fprintf('  %10s %8s %8s %8s\n', 'Quarter', 'SD_obs', 'SD_sim', 'Ratio');
sd_ratios = zeros(1, K_act);
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    sd_obs = std(obs_k);  sd_sim = std(sim_k);
    sd_ratios(k) = sd_sim / max(sd_obs, 1e-10);
    fprintf('  %10s %8.2f %8.2f %8.3f\n', qlbl{k}, sd_obs, sd_sim, sd_ratios(k));
end
fprintf('  %10s %8s %8s %8.3f\n', 'Mean', '', '', mean(sd_ratios));

% --- (2) Within-country AC(1) ---
fprintf('\n  (2) Within-Country Autocorrelation:\n');
ac1_obs = zeros(n_c,1);  ac1_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_act);  ys = cdata(i).sim_y(1:K_act);
    co1 = corrcoef(yo(1:end-1), yo(2:end));  ac1_obs(i) = co1(1,2);
    cs1 = corrcoef(ys(1:end-1), ys(2:end));  ac1_sim(i) = cs1(1,2);
end
fprintf('  %20s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %20s %10.3f %10.3f\n', 'AC(1) mean', mean(ac1_obs), mean(ac1_sim));

% --- (3) ICC ---
y_obs_p = reshape([cdata.y], N, n_c)';
y_sim_p = reshape([cdata.sim_y], N, n_c)';
icc_y_obs = var(mean(y_obs_p(:,1:K_act),2)) / var(y_obs_p(:,1:K_act),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_act),2)) / var(y_sim_p(:,1:K_act),0,'all');
b_obs_p = zeros(n_c, K_act);
b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_act); end
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_act),2)) / var(b_sim_p(:,1:K_act),0,'all');
fprintf('\n  (3) Intraclass Correlation Coefficient:\n');
fprintf('  %20s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %20s %10.3f %10.3f\n', 'ICC output', icc_y_obs, icc_y_sim);
fprintf('  %20s %10.3f %10.3f\n', 'ICC debt',   icc_b_obs, icc_b_sim);


%% ========================================================================
%  STEP 1c: DEBT RESIDUAL DIAGNOSTICS (now three-way)
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1c: Debt Residual Diagnostics\n');
fprintf('========================================\n');

iso_list   = {cdata.iso};
resid_b    = zeros(n_c, 1);
sim_b_cum  = zeros(n_c, 1);
obs_b_cum  = zeros(n_c, 1);
frac_above = zeros(n_c, 1);
frac_loans = zeros(n_c, 1);
frac_guar  = zeros(n_c, 1);
cp_total   = zeros(n_c, 1);
for i = 1:n_c
    sim_b_cum(i) = cdata(i).sim_b(K_act);
    obs_b_cum(i) = cdata(i).obs_b_cum(K_act);
    resid_b(i)   = obs_b_cum(i) - sim_b_cum(i);
    above_tot = sum(cdata(i).FCP_above(1:K_act));
    loans_tot = sum(cdata(i).FCP_loans(1:K_act));
    guar_tot  = sum(cdata(i).FCP_guar(1:K_act));
    cp_total(i) = above_tot + loans_tot + guar_tot;
    if cp_total(i) > 0.1
        frac_above(i) = above_tot / cp_total(i);
        frac_loans(i) = loans_tot / cp_total(i);
        frac_guar(i)  = guar_tot  / cp_total(i);
    end
end

% (1) Top/bottom residuals
[~, sort_idx] = sort(resid_b);
fprintf('\n  (1) Largest negative residuals (model OVER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s %8s %8s %8s\n', ...
    'ISO', 'Obs', 'Sim', 'Resid', '%above', '%loans', '%guar');
for j = 1:5
    i = sort_idx(j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %8.2f %8.2f %8.2f\n', ...
        iso_list{i}, obs_b_cum(i), sim_b_cum(i), resid_b(i), ...
        frac_above(i)*100, frac_loans(i)*100, frac_guar(i)*100);
end

fprintf('\n  Largest positive residuals (model UNDER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s %8s %8s %8s\n', ...
    'ISO', 'Obs', 'Sim', 'Resid', '%above', '%loans', '%guar');
for j = 0:4
    i = sort_idx(n_c - j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %8.2f %8.2f %8.2f\n', ...
        iso_list{i}, obs_b_cum(i), sim_b_cum(i), resid_b(i), ...
        frac_above(i)*100, frac_loans(i)*100, frac_guar(i)*100);
end

% (2) Correlations (three-way)
corr_above = corrcoef(resid_b, frac_above);
corr_loans = corrcoef(resid_b, frac_loans);
corr_guar  = corrcoef(resid_b, frac_guar);
corr_cptot = corrcoef(resid_b, cp_total);
fprintf('\n  (2) Residual correlations (diagnostic for systematic bias):\n');
fprintf('    Residual vs %% above-the-line: r = %+.3f\n', corr_above(1,2));
fprintf('    Residual vs %% loans:          r = %+.3f\n', corr_loans(1,2));
fprintf('    Residual vs %% guarantees:     r = %+.3f\n', corr_guar(1,2));
fprintf('    Residual vs total CP:         r = %+.3f\n', corr_cptot(1,2));
fprintf('\n  Interpretation:\n');
fprintf('    If |r| < 0.2: residuals are random, calibration is sound.\n');
fprintf('    If |r| > 0.3: kappa_{above,loans,guar} are systematically biased.\n');

% (3) Residual summary
fprintf('\n  (3) Residual summary:\n');
fprintf('    Mean:   %+.2f pp\n', mean(resid_b));
fprintf('    Median: %+.2f pp\n', median(resid_b));
fprintf('    SD:     %+.2f pp\n', std(resid_b));
fprintf('    Range:  [%+.2f, %+.2f] pp\n', min(resid_b), max(resid_b));


%% ========================================================================
%  VISUALIZATION
% =========================================================================
sim_y_all = reshape([cdata.sim_y], N, n_c)';
obs_y_all = reshape([cdata.y],     N, n_c)';
sim_b_all = reshape([cdata.sim_b], N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum; end

figure('Name','Calibration V5 (V4 spec)','Color','w','Position',[50 50 1000 400]);

subplot(1,2,1); hold on;
fill_iqr(1:N, sim_y_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_y_all, [.5 .5 .5], .12);
plot(1:N, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title('Output Gap');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.05, sprintf('RMSE = %.2f pp', median([cdata.rmse_y])), ...
    'Units','normalized','FontSize',8,'BackgroundColor','w');

subplot(1,2,2); hold on;
fill_iqr(1:N, sim_b_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_b_all, [.5 .5 .5], .12);
plot(1:N, median(sim_b_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_b_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Cumulative Debt');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp', median([cdata.rmse_b])), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

sgtitle('Calibration V5 — V4 frozen spec','FontWeight','bold');

fprintf('\n=== CALIBRATION COMPLETE ===\n');
fprintf('Next steps: scenario analysis, solver verification, counterfactuals.\n\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
% Forward-rolls the state (y, b, w, z) given controls and exogenous inputs.
%
% V4 OUTPUT EQUATION:
%   y_{k+1} = mu_y + rho_y*y_k + alpha_S*S_k
%             + eta_p*w_k*y_k*I(y_k<0)        % recession spline
%             + alpha_DI*z_k + beta_d*d_k + eps_y(k+1)
%
% V4 DEBT EQUATION (Country FE + linear year trend):
%   b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%             + kappa_above*F^CP,above_k
%             + kappa_loans*F^CP,loans_k
%             + kappa_guar*F^CP,guar_k        % already 35% take-up adjusted
%             + kappa_DI*z_k                   % LAG 1 via state z = F^DI_{k-1}
%             + kappa_H*F^H_k
%             + phi_t*year_idx_k               % linear year trend
%
% STATE: x = (y, b, w, z) with w = F^CP_{k-1}, z = F^DI_{k-1}.

    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    for k = 1:N_
        y = xs(1,k); b = xs(2,k); w = xs(3,k); z = xs(4,k);
        fk = 0; fa = 0; fl = 0; fg = 0; gk = 0; hk = 0;
        Sk = 0; thk = 0; dk = 0; ey = 0;  yr_idx = 0;
        if k <= length(fcp),       fk  = fcp(k);       end
        if k <= length(fcp_above), fa  = fcp_above(k); end
        if k <= length(fcp_loans), fl  = fcp_loans(k); end
        if k <= length(fcp_guar),  fg  = fcp_guar(k);  end
        if k <= length(fdi),       gk  = fdi(k);       end
        if k <= length(fh),        hk  = fh(k);        end
        if k <= length(S),         Sk  = S(k);         end
        if k <= length(theta),     thk = theta(k);     end
        if k <= length(d),         dk  = d(k);         end
        if k+1 <= length(P.eps_y_vec),  ey     = P.eps_y_vec(k+1);     end
        if k <= length(P.year_idx_vec), yr_idx = P.year_idx_vec(k);    end

        % --- Output (V4): no psi term, recession spline on eta_p ---
        spline_active = (y < 0);
        xs(1,k+1) = mu_y + P.rho_y*y + P.alpha_S*Sk ...
                  + spline_active * P.eta_p * w * y ...
                  + P.alpha_F_DI*z + P.beta_fear*dk + ey;

        % --- Debt (V4): three-way CP, DI lag 1, year trend ---
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
                  + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
                  + P.kappa_F_DI*z ...                     % LAG 1: was gk in V4 prev
                  + P.kappa_H*hk ...
                  + P.phi_t*yr_idx;                        % year trend

        % Lagged fiscal states
        xs(3,k+1) = fk;   % w_{k+1} = F^CP_k (aggregate)
        xs(4,k+1) = gk;   % z_{k+1} = F^DI_k
    end
end

function fill_iqr(x, data, col, alpha)
    sd = sort(data);  n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end


%% ========================================================================
%  R SNIPPET FOR EXTRACTING V4 FIXED EFFECTS
%  Run in R after the V4 main spec is estimated; paste output below.
% =========================================================================
%
% # --- Output equation V4 (N=380) ---
% m_y <- feols(y_t_pct ~ y_lag1 + S_mean_tw +
%              I(F_CP_lag2 * y_lag1 * (y_lag1 < 0)) +
%              F_DI_lag2 + p_proj_all_ages | Country + Quarter,
%              data = pdataY, subset = ~ t_idx >= 5 & t_idx <= 14,
%              cluster = ~Country)
% cfe_y <- fixef(m_y)$Country
% cat(paste(sprintf("%+.4f", cfe_y[cfe_iso]), collapse=", "))
%
% # --- Debt equation V4 (N=494, three-way) ---
% m_b <- feols(debt_dR ~ y_t_pct + F_CP_above_3 + F_CP_loans + F_CP_guar_adj
%              + F_DI_lag1 + as.numeric(year_only) | Country,
%              data = pdataD, cluster = ~Country)
% cfe_b <- fixef(m_b)$Country
% cat(paste(sprintf("%+.4f", cfe_b[cfe_iso]), collapse=", "))
%
% # --- Output equation Quarter FE (N=380) ---
% qfe_y <- fixef(m_y)$Quarter
% cat(paste(sprintf("%+.3f", qfe_y - qfe_y["Q1.2020"]), collapse=", "))





%%
% 1. Ohne year trend — wie groß ist der Beitrag?
P2 = P; P2.phi_t = 0;
% Re-run forward_roll und vergleiche

% 2. Beitrag jeder Komponente isolieren
for i = 1:n_c
    above_contrib(i) = P.kappa_above * sum(cdata(i).FCP_above);
    loans_contrib(i) = P.kappa_loans * sum(cdata(i).FCP_loans);
    guar_contrib(i)  = P.kappa_guar  * sum(cdata(i).FCP_guar);
    di_contrib(i)    = P.kappa_F_DI  * sum(cdata(i).FDI(1:end-1));  % lag 1
    h_contrib(i)     = P.kappa_H     * sum(cdata(i).FH);
    stab_contrib(i)  = -P.gamma_y    * sum(cdata(i).y);
    trend_contrib(i) = P.phi_t       * sum(P.year_idx_vec);
end
fprintf('Median contributions (cum 10Q):\n');
fprintf('  Above:  %+.2f\n', median(above_contrib));
fprintf('  Loans:  %+.2f\n', median(loans_contrib));
fprintf('  Guar:   %+.2f\n', median(guar_contrib));
fprintf('  DI:     %+.2f\n', median(di_contrib));
fprintf('  Health: %+.2f\n', median(h_contrib));
fprintf('  Stab:   %+.2f\n', median(stab_contrib));
fprintf('  Trend:  %+.2f\n', median(trend_contrib));

%% ========================================================================
%  POLICY LEVER INTENSITY: OECD Median Trajectories
% =========================================================================
fprintf('\n========================================\n');
fprintf('  POLICY LEVER INTENSITY (OECD Median)\n');
fprintf('========================================\n');

S_med    = zeros(1, N);
FCP_med  = zeros(1, N);
FCPa_med = zeros(1, N);
FCPl_med = zeros(1, N);
FCPg_med = zeros(1, N);
FDI_med  = zeros(1, N);
FH_med   = zeros(1, N);

for k = 1:N
    S_med(k)    = median(arrayfun(@(c) c.S(k),         cdata));
    FCP_med(k)  = median(arrayfun(@(c) c.FCP(k),       cdata));
    FCPa_med(k) = median(arrayfun(@(c) c.FCP_above(k), cdata));
    FCPl_med(k) = median(arrayfun(@(c) c.FCP_loans(k), cdata));
    FCPg_med(k) = median(arrayfun(@(c) c.FCP_guar(k),  cdata));   % 35% adj
    FDI_med(k)  = median(arrayfun(@(c) c.FDI(k),       cdata));
    FH_med(k)   = median(arrayfun(@(c) c.FH(k),        cdata));
end

fprintf('\n  Containment & fiscal deployment (median across 38 OECD):\n');
fprintf('  %8s %7s %7s %7s %7s %7s %7s %7s\n', ...
    'Quarter', 'S', 'F^CP', '  above', '  loans', ' guar35', 'F^DI', 'F^H');
for k = 1:N
    fprintf('  %8s %7.2f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n', ...
        qlbl{k}, S_med(k), FCP_med(k), FCPa_med(k), FCPl_med(k), ...
        FCPg_med(k), FDI_med(k), FH_med(k));
end

% Cumulative deployment over 10Q
fprintf('\n  Cumulative median deployment (10Q sum, pp of 2019 GDP):\n');
fprintf('    F^CP total       : %6.2f\n', sum(FCP_med));
fprintf('      above-the-line : %6.2f\n', sum(FCPa_med));
fprintf('      loans          : %6.2f\n', sum(FCPl_med));
fprintf('      guarantees(35%%): %6.2f\n', sum(FCPg_med));
fprintf('    F^DI             : %6.2f\n', sum(FDI_med));
fprintf('    F^H              : %6.2f\n', sum(FH_med));
fprintf('    F total          : %6.2f\n', sum(FCP_med + FDI_med + FH_med));
fprintf('  Mean stringency S  : %6.2f (0-100 scale)\n', mean(S_med));
fprintf('  Peak stringency S  : %6.2f (Q2.20 typically)\n', max(S_med));

% Visualization
figure('Name','Policy Lever Intensity','Color','w','Position',[100 100 1100 600]);

subplot(2,2,1);
plot(1:N, S_med, 'r-o', 'LineWidth', 1.8); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('Stringency Index'); title('Containment Intensity (S)');

subplot(2,2,2); hold on;
plot(1:N, FCP_med, 'b-o', 'LineWidth', 1.8);
plot(1:N, FDI_med, 'g-s', 'LineWidth', 1.8);
plot(1:N, FH_med,  'm-^', 'LineWidth', 1.8); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Aggregate Fiscal Deployment');
legend('F^{CP}','F^{DI}','F^H', 'Location','NE');

subplot(2,2,3);
bar(1:N, [FCPa_med', FCPl_med', FCPg_med'], 'stacked');
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('CP Decomposition (median)');
legend('above-the-line','loans','guarantees (35%)', 'Location','NE');

subplot(2,2,4);
bar(1:N, [FCP_med', FDI_med', FH_med'], 'stacked');
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Total Fiscal Stack (median)');
legend('CP','DI','Health', 'Location','NE');

sgtitle('OECD Median Policy Levers — Q1.2020-Q2.2022','FontWeight','bold');


%% ========================================================================
%  POLICY LEVER VALIDATION: Observed vs. Used in Forward Roll
%  Side-by-side comparison of OECD median policy levers (data) vs. the
%  exact same levers fed into the calibrated forward roll. Trivially
%  identical (calibration uses observed inputs); the panel documents the
%  policy-deployment context that produced the output/debt fits.
% =========================================================================

% Recompute medians (in case section is run standalone)
S_med    = zeros(1, N);  FCP_med  = zeros(1, N);
FCPa_med = zeros(1, N);  FCPl_med = zeros(1, N);  FCPg_med = zeros(1, N);
FDI_med  = zeros(1, N);  FH_med   = zeros(1, N);
for k = 1:N
    S_med(k)    = median(arrayfun(@(c) c.S(k),         cdata));
    FCP_med(k)  = median(arrayfun(@(c) c.FCP(k),       cdata));
    FCPa_med(k) = median(arrayfun(@(c) c.FCP_above(k), cdata));
    FCPl_med(k) = median(arrayfun(@(c) c.FCP_loans(k), cdata));
    FCPg_med(k) = median(arrayfun(@(c) c.FCP_guar(k),  cdata));
    FDI_med(k)  = median(arrayfun(@(c) c.FDI(k),       cdata));
    FH_med(k)   = median(arrayfun(@(c) c.FH(k),        cdata));
end

% IQR bands for observed
Sq    = zeros(2,N); FCPq  = zeros(2,N); FDIq = zeros(2,N);
FCPaq = zeros(2,N); FCPlq = zeros(2,N); FCPgq= zeros(2,N);
for k = 1:N
    Sq(:,k)    = quantile(arrayfun(@(c) c.S(k),         cdata), [0.25 0.75]);
    FCPq(:,k)  = quantile(arrayfun(@(c) c.FCP(k),       cdata), [0.25 0.75]);
    FDIq(:,k)  = quantile(arrayfun(@(c) c.FDI(k),       cdata), [0.25 0.75]);
    FCPaq(:,k) = quantile(arrayfun(@(c) c.FCP_above(k), cdata), [0.25 0.75]);
    FCPlq(:,k) = quantile(arrayfun(@(c) c.FCP_loans(k), cdata), [0.25 0.75]);
    FCPgq(:,k) = quantile(arrayfun(@(c) c.FCP_guar(k),  cdata), [0.25 0.75]);
end

figure('Name','Policy Lever Validation','Color','w','Position',[100 100 1200 700]);

% --- Stringency ---
subplot(2,3,1); hold on;
fill([1:N, fliplr(1:N)], [Sq(1,:), fliplr(Sq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, S_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, S_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('Stringency Index (0-100)');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Containment (S)'); legend('IQR obs','Observed','Used in roll','Location','SE');

% --- Aggregate F^CP ---
subplot(2,3,2); hold on;
fill([1:N, fliplr(1:N)], [FCPq(1,:), fliplr(FCPq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCP_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCP_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Capacity Preservation (F^{CP})'); legend('IQR obs','Observed','Used in roll','Location','NE');

% --- Aggregate F^DI ---
subplot(2,3,3); hold on;
fill([1:N, fliplr(1:N)], [FDIq(1,:), fliplr(FDIq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FDI_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FDI_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Demand Injection (F^{DI})'); legend('IQR obs','Observed','Used in roll','Location','NE');

% --- CP above ---
subplot(2,3,4); hold on;
fill([1:N, fliplr(1:N)], [FCPaq(1,:), fliplr(FCPaq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCPa_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCPa_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('CP above-the-line'); legend('IQR obs','Observed','Used in roll','Location','NE');

% --- CP loans ---
subplot(2,3,5); hold on;
fill([1:N, fliplr(1:N)], [FCPlq(1,:), fliplr(FCPlq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCPl_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCPl_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('CP loans'); legend('IQR obs','Observed','Used in roll','Location','NE');

% --- CP guarantees (35% adj) ---
subplot(2,3,6); hold on;
fill([1:N, fliplr(1:N)], [FCPgq(1,:), fliplr(FCPgq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCPg_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCPg_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('CP guarantees (35% adj.)'); legend('IQR obs','Observed','Used in roll','Location','NE');

sgtitle('Policy Lever Validation: OECD Median Observed vs. Used in Forward Roll', ...
        'FontWeight','bold','FontSize',11);