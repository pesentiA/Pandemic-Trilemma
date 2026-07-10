%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & VALIDATION (V4, N=380)
%
%  STEP 1 ONLY: Calibration + Validation.
%  Scenarios, iLQR solver, and counterfactuals are in separate files.
%
%  SPECIFICATION (matches paper Tables 3 col 3 and 8 col 5):
%    Output:  y_{ik} = rho_y*y_{i,k-1}
%                     + psi*S_{ik}*y_{i,k-1}
%                     + eta_p*F^CP_{i,k-2}*y_{i,k-1}
%                     + alpha_S*S_{ik}
%                     + alpha_DI*F^DI_{i,k-2}
%                     + beta_d*d_{ik}
%                     + mu_i^y + delta_k^y
%
%    Debt:    b_{ik} = -gamma_y*y_{ik}
%                     + kappa_above*F^CP,above_{ik}
%                     + kappa_loans*F^CP,loans_{ik}
%                     + kappa_guar*F^CP,guar_{ik}
%                     + kappa_DI*F^DI_{ik}
%                     + mu_i^b
%             (b is first-differenced quarterly change, pp of 2019 GDP)
%
%  STATE-SPACE REPRESENTATION
%  --------------------------
%  Forecasting y_{k+1} from info at time k:
%    y_{k+1} = rho_y*y_k + psi*S_k*y_k + eta_p*F^CP_{k-1}*y_k
%              - alpha_S*S_k + alpha_DI*F^DI_{k-1} + beta_d*d_k
%              + mu_i^y + eps_y(k+1)
%
%  The F^CP_{k-2}*y_{k-1} term in the paper, re-indexed by t = k+1,
%  becomes F^CP_{k-1}*y_k. Hence state w_k = F^CP_{k-1}.
%  Similarly z_k = F^DI_{k-1}.
%
%  State:   x = (y, b, w, z)   where w = F^CP_{k-1}, z = F^DI_{k-1}
%  Control: u = (F^CP_k, F^DI_k)
%
%  UNIT CONVENTION
%  ---------------
%  ALL variables in percentage points (pp), matching the regression.
%    y, b, d, F^CP, F^DI all in pp. S in 0-100 index.
%  Coefficients used exactly as reported in paper tables.
%  NO division by 100 at any point. This is the transparent convention.
%
%  FIXED EFFECTS
%  -------------
%  Country FE (mu_i): level shift in the within-sample period. Standard
%  practice in dynamic panel validation (e.g., Blanchard & Perotti 2002)
%  is to treat them as time-invariant intercepts, applied each period.
%  This is mathematically identical to demeaning in estimation and
%  reproduces the observed country-level mean.
%
%  Quarter FE (delta_k): global shocks common to all countries in
%  period k. Applied as additive shift eps_y(k+1) to y_{k+1}.
%  In counterfactual analyses these are held fixed (standard: global
%  shocks do not respond to individual-country fiscal composition).
%
%  Reference: quarter FE normalized to delta_{Q1.2020} = 0 (first
%  pandemic quarter, N=380 sample starts here).
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V4 (N=380) ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS — PAPER VALUES (N=380, Q1.2020–Q2.2022)
% =========================================================================

% --- Output equation: Table 3, column 3 (TWFE) ---
rho_y      =  0.311;    % baseline persistence
psi        =  0.002;    % S * y_lag1 interaction (lockdown amplification)
eta_p      = -0.011;    % F^CP_lag2 * y_lag1 (CP persistence channel)
alpha_S    = -0.034;    % S level (direct output cost; sign: negative)
alpha_F_DI =  0.185;    % F^DI_lag2 level (demand injection multiplier)
beta_fear  = -0.021;    % d (fear term)

% --- Debt equation: Table 8, column 5 (three-way CP decomposition) ---
r_int        = 0.001;   % real quarterly interest rate (near zero in sample)
gamma_y      = 0.210;   % budget semi-elasticity (coef on y_ik is -gamma_y)
kappa_above  = 0.423;   % above-the-line CP (wage subsidies, grants)
kappa_loans  = 0.352;   % government loans (actual disbursements)
kappa_guar   = 0.095;   % credit guarantees (35% take-up, n.s.)
kappa_F_DI   = 0.181;   % demand injection fiscal cost (n.s.)
% --- Health spending (calibrated, not estimated) ---
%  kappa_H set to 1.0 (full pass-through): health expenditures enter the
%  budget as direct outlays. The estimated coefficient on F^H is not
%  statistically distinguishable from zero due to low within-sample
%  variance and co-deployment with CP. Economic consistency requires a
%  positive cost; the IMF Fiscal Monitor classifies health expenditures
%  as above-the-line direct outlays, which map to kappa_H = 1.0.
%
%  Because F^H is held constant across all counterfactual scenarios,
%  the calibrated value of kappa_H affects only the debt baseline,
%  not the relative comparison between fiscal compositions.
kappa_H      = 1.000;

% --- Country ISO list (defines ordering for FE lookup) ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Quarter FE (pp), N=380 sample, reference delta_{Q1.2020} = 0 ---
%  From feols TWFE estimation on N=380 sample. Source: paper regression.
%  Ordering: Q1.2020, Q2.2020, ..., Q2.2022.
qfe_pp = [ 0.000, -7.770,  1.398,  0.395,  1.471, ...
           1.789,  1.749,  2.308,  1.220,  0.730];

% --- Country FE: Output (pp), N=380 TWFE estimation ---
%  From feols regression on paper specification, in cfe_iso order.
cfe_y_val = [-0.6047, -2.4119, -0.9373, -1.9822, -0.0418, -0.4771, +0.1527, -1.4938, ...
             -4.3042, -2.4173, -0.7601, -5.6249, -2.3740, -2.3512, -2.7989, -4.0574, ...
             -1.5275, -3.1608, +5.3869, -5.6901, -0.1596, -1.3319, -1.7441, -0.8606, ...
             -0.7382, +0.3109, -1.5276, -3.8257, -0.6917, -0.3567, -2.1031, -1.9743, ...
             -4.0511, -1.4476, -2.7772, -0.5934, +2.5178, -0.4942];

% --- Country FE: Debt (pp), N=380 estimation (Country FE only spec) ---
cfe_b_val = [-0.7764, -0.2810, -0.3591, -0.2788, -0.5442, -0.8710, +0.2171, +0.1229, ...
             -1.2251, -1.0595, -1.3429, -1.3779, -0.5795, -0.5259, -0.6096, -1.2125, ...
             -0.8123, -0.7376, +1.4615, -1.7152, +0.2409, -0.5304, +0.1057, -0.2255, ...
             -0.6522, -0.1292, -0.0637, -1.9053, -0.3678, -0.7119, +0.5692, -0.6873, ...
             -1.5216, -0.0485, -0.2423, -0.5208, +0.1770, +0.5954];

% --- Objective weights (for later optimization; calibrate via revealed pref) ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions ---
N = 10;  K_act = 10;  nx = 4;  nu = 2;

% --- Build eps_y vector in pp ---
%  eps_y_vec(k+1) is the quarter FE affecting y_{k+1}. Index shift:
%  qfe_pp(k) is delta_k, applied when forecasting y_{k+1}.
eps_y_vec = [0, qfe_pp];   % eps_y_vec(1) = pre-sample; eps_y_vec(k+1) = qfe_pp(k)
if length(eps_y_vec) < N+1
    eps_y_vec(end+1:N+1) = 0;
end

% --- Build lookup maps (for FE by ISO) ---
cfe_y_map  = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map  = containers.Map(cfe_iso, cfe_b_val);

% --- Control bounds (pp of GDP) ---
u_lo = [0; 0];  u_hi = [20.0; 10.0];

% --- Pack parameters ---
P = struct( ...
    'rho_y',rho_y, 'psi',psi, 'alpha_S',alpha_S, ...
    'eta_p',eta_p, 'alpha_F_DI',alpha_F_DI, 'beta_fear',beta_fear, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, 'kappa_F_DI',kappa_F_DI, ...
    'kappa_H',kappa_H, ...
    'eps_y_vec',eps_y_vec, 'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   nx = %d (y, b, w, z)\n', N, K_act, nx);
fprintf('  Units: ALL variables in percentage points (pp)\n\n');
fprintf('  Output equation coefficients (paper Table 3, col 3):\n');
fprintf('    rho_y      = %+.4f\n', rho_y);
fprintf('    psi        = %+.4f   (S*y_lag1 interaction)\n', psi);
fprintf('    eta_p      = %+.4f   (F^CP_lag2*y_lag1, persistence channel)\n', eta_p);
fprintf('    alpha_S    = %+.4f   (S level; negative = contractionary)\n', alpha_S);
fprintf('    alpha_DI   = %+.4f   (F^DI_lag2 level)\n', alpha_F_DI);
fprintf('    beta_d     = %+.4f   (fear term)\n', beta_fear);

% Effective persistence at OECD averages
S_avg  = 40.19;     % pp, Panel A mean
FCP_avg = 1.31;     % pp, Panel A mean
rho_eff_avg = rho_y + psi*S_avg + eta_p*FCP_avg;
fprintf('\n  Effective persistence at OECD averages (S=%.1f, F^CP=%.2f):\n', ...
    S_avg, FCP_avg);
fprintf('    rho_eff = %.4f + %.4f*%.1f + (%.4f)*%.2f = %.4f\n', ...
    rho_y, psi, S_avg, eta_p, FCP_avg, rho_eff_avg);
fprintf('    Without CP: rho = %.4f + %.4f*%.1f = %.4f\n', ...
    rho_y, psi, S_avg, rho_y + psi*S_avg);

fprintf('\n  Debt equation coefficients (paper Table 8, col 5):\n');
fprintf('    gamma_y       = %+.3f   (automatic stabilizer)\n', gamma_y);
fprintf('    kappa_above   = %+.3f   (CP above-the-line, estimated)\n', kappa_above);
fprintf('    kappa_loans   = %+.3f   (CP loans, estimated)\n', kappa_loans);
fprintf('    kappa_guar    = %+.3f   (CP guarantees 35%% adj, n.s.)\n', kappa_guar);
fprintf('    kappa_DI      = %+.3f   (DI cost, n.s.)\n', kappa_F_DI);
fprintf('    kappa_H       = %+.3f   (calibrated, full pass-through)\n\n', kappa_H);


%% ========================================================================
%  LOAD DATA
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');

% N=380 sample: Q1.2020 through Q2.2022 (10 quarters per country)
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
    cdata(i).S    = zeros(1,N);
    cdata(i).FCP  = zeros(1,N);   % aggregate CP (used in output equation)
    cdata(i).FCP_above  = zeros(1,N);
    cdata(i).FCP_below  = zeros(1,N);   % below-the-line (adj at 35% take-up)
    cdata(i).FCP_loans  = zeros(1,N);   % proxy: assumed 50% of below-the-line
    cdata(i).FCP_guar   = zeros(1,N);   % proxy: assumed 50% of below-the-line
    cdata(i).FDI  = zeros(1,N);
    cdata(i).FH   = zeros(1,N);
    cdata(i).y    = zeros(1,N);
    cdata(i).theta= zeros(1,N);
    cdata(i).b    = zeros(1,N);
    cdata(i).d    = zeros(1,N);
    cdata(i).mu_y = 0;  cdata(i).mu_b = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end

    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        % CSV values are in percent (e.g., y_t_pct = -5.2 means -5.2 pp).
        % We keep them in pp throughout: NO division by 100.
        cdata(i).S(k)           = row.S_mean_tw;         % 0-100 index
        cdata(i).FCP(k)         = row.F_CP;              % pp of 2019 GDP
        cdata(i).FCP_above(k)   = row.F_CP_above;        % pp
        cdata(i).FCP_below(k)   = row.F_CP_below_adj_mid; % pp (35% take-up adj)
        % Below-the-line split: loans vs guarantees.
        % Paper reports separate coefficients, but CSV only provides
        % aggregated below-the-line. Without external data we apportion
        % 50/50 as a working assumption. Refine when disaggregated data available.
        cdata(i).FCP_loans(k)   = 0.50 * row.F_CP_below_adj_mid;
        cdata(i).FCP_guar(k)    = 0.50 * row.F_CP_below_adj_mid;
        cdata(i).FDI(k)         = row.F_DI;              % pp of 2019 GDP
        if ismember('F_H', T.Properties.VariableNames) && ~ismissing(row.F_H)
            cdata(i).FH(k)      = row.F_H;               % pp of 2019 GDP
        end
        cdata(i).y(k)           = row.y_t_pct;           % pp of potential GDP
        cdata(i).theta(k)       = row.theta_pct;         % pp of population
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b(k)       = row.debt_dR;           % pp of 2019 GDP (first diff)
        end
        if ismember('excess_mortality', T.Properties.VariableNames) ...
                && ~ismissing(row.excess_mortality)
            cdata(i).d(k)       = row.excess_mortality;  % P-score in pp
        end
    end
end
fprintf('  %d countries x %d quarters (all variables in pp)\n\n', n_c, N);


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
        rho_effs(i) = P.rho_y + P.psi * cdata(i).S(k) + P.eta_p * wk;
    end
    fprintf('  %8s %+9.2f %+9.2f %9.3f\n', qlbl{k}, ...
        median(obs_k), median(sim_k), median(rho_effs));
end

% --- Fiscal contribution ---
%  Note: F^H is held constant in the no-fiscal counterfactual, because
%  health spending is exogenous to the fiscal-composition decision.
%  Setting F^H = 0 would conflate the fiscal decomposition with the
%  unrelated health baseline.
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
%  STEP 1c: DEBT RESIDUAL DIAGNOSTICS
%  Test whether the debt-simulation gap is random or correlates with
%  fiscal composition. Critical for Counterfactual credibility.
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1c: Debt Residual Diagnostics\n');
fprintf('========================================\n');

% Compute per-country cumulative debt residuals and CP composition
iso_list   = {cdata.iso};
resid_b    = zeros(n_c, 1);  % observed - simulated, cumulative over K_act
sim_b_cum  = zeros(n_c, 1);
obs_b_cum  = zeros(n_c, 1);
frac_above = zeros(n_c, 1);  % above-the-line share of total CP
frac_below = zeros(n_c, 1);  % below-the-line share of total CP
cp_total   = zeros(n_c, 1);
for i = 1:n_c
    sim_b_cum(i) = cdata(i).sim_b(K_act);
    obs_b_cum(i) = cdata(i).obs_b_cum(K_act);
    resid_b(i)   = obs_b_cum(i) - sim_b_cum(i);
    above_tot = sum(cdata(i).FCP_above(1:K_act));
    below_tot = sum(cdata(i).FCP_below(1:K_act));
    cp_total(i) = above_tot + below_tot;
    if cp_total(i) > 0.1
        frac_above(i) = above_tot / cp_total(i);
        frac_below(i) = below_tot / cp_total(i);
    end
end

% (1) Top/bottom residuals
[resid_sorted, sort_idx] = sort(resid_b);
fprintf('\n  (1) Largest negative residuals (model OVER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s %10s %10s\n', 'ISO', 'Obs', 'Sim', 'Resid', '%above', '%below');
for j = 1:5
    i = sort_idx(j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %10.2f %10.2f\n', ...
        iso_list{i}, obs_b_cum(i), sim_b_cum(i), resid_b(i), ...
        frac_above(i)*100, frac_below(i)*100);
end

fprintf('\n  Largest positive residuals (model UNDER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s %10s %10s\n', 'ISO', 'Obs', 'Sim', 'Resid', '%above', '%below');
for j = 0:4
    i = sort_idx(n_c - j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %10.2f %10.2f\n', ...
        iso_list{i}, obs_b_cum(i), sim_b_cum(i), resid_b(i), ...
        frac_above(i)*100, frac_below(i)*100);
end

% (2) Correlation: residual vs. composition
corr_above = corrcoef(resid_b, frac_above);
corr_below = corrcoef(resid_b, frac_below);
corr_cptot = corrcoef(resid_b, cp_total);
fprintf('\n  (2) Residual correlations (diagnostic for systematic bias):\n');
fprintf('    Residual vs %% above-the-line: r = %+.3f\n', corr_above(1,2));
fprintf('    Residual vs %% below-the-line: r = %+.3f\n', corr_below(1,2));
fprintf('    Residual vs total CP:         r = %+.3f\n', corr_cptot(1,2));
fprintf('\n  Interpretation:\n');
fprintf('    If |r| < 0.2: residuals are random, calibration is sound.\n');
fprintf('    If |r| > 0.3: the kappa coefficients are systematically\n');
fprintf('                  biased with respect to CP composition.\n');

% (3) Summary statistics of residual
fprintf('\n  (3) Residual summary:\n');
fprintf('    Mean:   %+.2f pp (systematic bias)\n', mean(resid_b));
fprintf('    Median: %+.2f pp\n', median(resid_b));
fprintf('    SD:     %+.2f pp (unexplained heterogeneity)\n', std(resid_b));
fprintf('    Range:  [%+.2f, %+.2f] pp\n', min(resid_b), max(resid_b));


%% ========================================================================
%  VISUALIZATION
% =========================================================================

% --- Fig 1: Validation (output + debt) ---
sim_y_all = reshape([cdata.sim_y], N, n_c)';
obs_y_all = reshape([cdata.y],     N, n_c)';
sim_b_all = reshape([cdata.sim_b], N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum; end

figure('Name','Calibration V2.1','Color','w','Position',[50 50 1000 400]);

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

sgtitle('Calibration V2.1','FontWeight','bold');

fprintf('\n=== CALIBRATION COMPLETE ===\n');
fprintf('Next steps: scenario analysis, solver verification, counterfactuals.\n');
fprintf('Run these in separate files once validation targets are hit.\n\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
% Forward-rolls the state (y, b, w, z) given controls and exogenous inputs.
%
% OUTPUT EQUATION (paper Table 3, col 3):
%   y_{k+1} = mu_y + (rho_y + psi*S_k + eta_p*w_k)*y_k
%             + alpha_S*S_k + alpha_DI*z_k + beta_d*d_k + eps_y(k+1)
%
%   Here w_k = F^CP_{k-1} (lagged by one period). CP enters only through
%   the persistence channel: it has no contemporaneous level effect on y.
%   The aggregate F^CP drives persistence; disaggregation is used only
%   for debt accounting.
%
% DEBT EQUATION (paper Table 8, col 5, with kappa_H calibrated to 1.0):
%   b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%             + kappa_above*F^CP,above_k
%             + kappa_loans*F^CP,loans_k
%             + kappa_guar*F^CP,guar_k
%             + kappa_DI*F^DI_k
%             + kappa_H*F^H_k
%
%   Health spending F^H uses a calibrated pass-through of 1.0 (full
%   budgetary outlay). See kappa_H documentation in the parameter block.
%
% STATE: x = (y, b, w, z)
%   w = F^CP_{k-1} (aggregate, for persistence channel)
%   z = F^DI_{k-1} (for demand-injection level channel)
%
% All variables in pp. No /100 conversions.

    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    for k = 1:N_
        y = xs(1,k); b = xs(2,k); w = xs(3,k); z = xs(4,k);
        fk = 0; fa = 0; fl = 0; fg = 0; gk = 0; hk = 0;
        Sk = 0; thk = 0; dk = 0; ey = 0;
        if k <= length(fcp),       fk  = fcp(k);       end
        if k <= length(fcp_above), fa  = fcp_above(k); end
        if k <= length(fcp_loans), fl  = fcp_loans(k); end
        if k <= length(fcp_guar),  fg  = fcp_guar(k);  end
        if k <= length(fdi),       gk  = fdi(k);       end
        if k <= length(fh),        hk  = fh(k);        end
        if k <= length(S),         Sk  = S(k);         end
        if k <= length(theta),     thk = theta(k);     end
        if k <= length(d),         dk  = d(k);         end
        if k+1 <= length(P.eps_y_vec), ey = P.eps_y_vec(k+1); end

        % Effective persistence (aggregate CP enters via w = F^CP_{k-1})
        rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;

        % Output: CP only through persistence; DI level effect via z = F^DI_{k-1}
        xs(1,k+1) = mu_y + rho_eff*y + P.alpha_S*Sk ...
                  + P.alpha_F_DI*z + P.beta_fear*dk + ey;

        % Debt: three-way CP disaggregation + DI + health (kappa_H = 1.0)
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
                  + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
                  + P.kappa_F_DI*gk ...
                  + P.kappa_H*hk;

        % Lagged fiscal states (for next period's persistence/level channels)
        xs(3,k+1) = fk;   % w_{k+1} = F^CP_k (aggregate)
        xs(4,k+1) = gk;   % z_{k+1} = F^DI_k
    end
end

function fill_iqr(x, data, col, alpha)
% Fill inter-quartile range band around a time series.
    sd = sort(data);  n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end

%We validate the debt calibration against observed cumulative debt accumulation. The model reproduces the cross-country distribution without systematic bias (mean residual = −0.01 pp) and without correlation between residuals and fiscal composition (r = −0.15 for above-the-line share). The residual standard deviation of 1.61 pp captures unmodeled heterogeneity in pandemic-era financing sources outside the discretionary fiscal database — including EU backstops (SURE, NGEU-RRF), central bank purchase programs, and contingent liabilities from state-owned enterprises. These sources are orthogonal to fiscal composition choices and therefore do not confound counterfactual comparisons."