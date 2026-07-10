%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION V5b (EXTENDED N=494, Q4.2019-Q4.2022)
%
%  EXTENSION rationale: Match the debt equation estimation sample exactly
%  (N=494, T=13). Eliminates the N=380 vs N=494 sample-mismatch artifact
%  (b_grand_mean tuning); allows a clean validation against the regression's
%  own data window.
%
%  CHANGES from V5 (06.05.2026):
%    1. N = 13 quarters: Q4.2019 + 10 pandemic + Q3.2022 + Q4.2022
%    2. Q4.2019 included (lag source for output equation; debt sample anchor)
%    3. QFE for Q3.2022 / Q4.2022 set to 0 (post-trilemma, no common shock
%       beyond what's already absorbed by structural channels)
%    4. b_grand_mean = 0 (no tuning needed; sample matches regression)
%    5. year_idx_vec extended for 2019 (idx=0) and full 2022 (idx=3)
%
% Werte sind neu
%    - Q4.2019 QFE: aus plm_test, effect = "time" -> Wert für "Q4.2019"
%      bereits gegeben: +0.247 (relativ zu Q1.2020 zentriert)
%    - Output Country FE für N=380: bereits eingesetzt (V4 N=380)
%      ALTERNATIVE: Output equation auf N=494 schätzen + FE re-extract
%
%  All other parameters and structure: identical to V5.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V5b (Extended N=494) ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS — V4 FROZEN VALUES (unchanged)
% =========================================================================
%Wenn finales Modell steht noch genauer angeben
% --- Output equation (V4 frozen, N=380 estimation) ---
rho_y      =  0.408;
eta_p      = -0.010;
alpha_S    = -0.041;
alpha_F_DI =  0.193;
beta_fear  = -0.023;

% --- Debt equation (V4 main, N=494) ---
r_int        =  0.001;
gamma_y      =  0.188;
kappa_above  =  0.434;
kappa_loans  =  0.365;
kappa_guar   =  0.107;
kappa_F_DI   =  0.427;
phi_t        = -0.250;

% --- Pooled CP / sensitivity ---
kappa_CP_pooled = 0.173;
kappa_CP_lo     = 0.134;
kappa_CP_hi     = 0.241;
kappa_above_q75 = 1.009;
kappa_H         = 1.000;

% --- Country ISO list ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Quarter FE for OUTPUT (pp), 13 quarters Q4.2019-Q4.2022 ---
%  Q4.2019 = +0.247 (aus plm_test, effect="time" für N=380 spec).
%  Q1.2020-Q2.2022 = aus V4 plm_test, zentriert auf Q1.2020 (+0.000).
%  Q3.2022, Q4.2022 = 0 (post-trilemma; NPI/F=0, kein common shock).
%
%  
%    Re-extract qfe_pp via: fixef(plm_test_N494, effect="time")
qfe_pp = [+0.247, ...                              % Q4.2019
           0.000, -7.626,  2.585,  0.849, ...      % Q1.20 - Q4.20
           1.777,  2.149,  2.121,  2.602, ...      % Q1.21 - Q4.21
           1.399,  1.059, ...                       % Q1.22 - Q2.22
           0.000,  0.000];                          % Q3.22, Q4.22 (post-trilemma)

% --- Country FE: Output (pp), V4 N=380 ---
cfe_y_val = [-0.3639, -2.1325, -0.7051, -1.7267, +0.2141, -0.3140, +0.3177, -1.2524, ...
             -3.9365, -2.1263, -0.5655, -5.3127, -2.0566, -2.0269, -2.5061, -3.8640, ...
             -1.3358, -2.8375, +5.5229, -5.2402, +0.0604, -1.1385, -1.4375, -0.5740, ...
             -0.4405, +0.5560, -1.2271, -3.4843, -0.4524, -0.1301, -1.8321, -1.6764, ...
             -3.8525, -1.0973, -2.4952, -0.3698, +2.7234, -0.2353];

% --- Country FE: Debt (pp), V4 N=494 demeaned ---
%  Sample MATCHES regression sample => b_grand_mean = 0 (no tuning needed).
cfe_b_val = [-0.6462, -0.5190, -0.5147, -0.1395, -0.3089, -0.9431, +0.6842, +0.0991, ...
             -0.9315, -0.9908, -1.4190, -1.1902, -0.5864, -0.5923, -0.8557, -0.9631, ...
             -0.7381, -0.9150, +0.6430, -1.0453, -0.0485, -0.7098, -0.8563, -0.4437, ...
             -0.5257, +0.2124, +0.0169, -1.4596, -0.3687, -0.0728, +0.3738, -0.8068, ...
             -1.8202, -0.2072, -0.8612, -0.4815, +0.1780, +0.4707];
b_grand_mean = 0;   % Sample matches => no offset needed
cfe_b_val = cfe_b_val + b_grand_mean;

% --- Year index, centered (13 quarters: 2019 + 2020*4 + 2021*4 + 2022*4) ---
phi_t = -0.250;
year_idx_raw = [0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3];
year_idx_vec = year_idx_raw - mean(year_idx_raw);

%Werden hier och nciht gebraucht, für später relevant-> mache ich aber mit
%Pareto Frontiers
% --- Objective weights ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions (EXTENDED) ---
N = 13;  K_act = 13;  nx = 4;  nu = 2;

% --- Build eps_y vector ---
eps_y_vec = [0, qfe_pp];
if length(eps_y_vec) < N+1
    eps_y_vec(end+1:N+1) = 0;
end

% --- Lookup maps ---
cfe_y_map  = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map  = containers.Map(cfe_iso, cfe_b_val);

% --- Control bounds ---
u_lo = [0; 0];  u_hi = [20.0; 10.0];

% --- Pack parameters into P ---
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
fprintf('  Sample: Q4.2019 - Q4.2022 (matches debt regression N=494)\n\n');


%% ========================================================================
%  LOAD DATA (extended quarter list)
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');

qord = {'Q4.2019', ...
        'Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl = {'Q4.19', ...
        'Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22','Q3.22','Q4.22'};
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
        cdata(i).FCP_above(k)   = row.F_CP_above_3;
        cdata(i).FCP_loans(k)   = row.F_CP_loans;
        cdata(i).FCP_guar(k)    = row.F_CP_guar_adj;
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
fprintf('  %d countries x %d quarters\n\n', n_c, N);

% Reconciliation check
recon_err = zeros(n_c,1);
for i = 1:n_c
    recon = cdata(i).FCP_above + cdata(i).FCP_loans + cdata(i).FCP_guar/0.35;
    recon_err(i) = max(abs(cdata(i).FCP - recon));
end
fprintf('  CP decomposition reconciliation: max abs error = %.4f pp\n\n', max(recon_err));


%% ========================================================================
%  STEP 1: VALIDATION
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

fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s %9s\n', 'Quarter', 'Observed', 'Model', 'rho_eff');
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    rho_effs = zeros(n_c, 1);
    for i = 1:n_c
        wk = 0; if k >= 3, wk = cdata(i).FCP(k-2); end
        y_lag1 = 0; if k >= 2, y_lag1 = cdata(i).y(k-1); end
        spline_active = (y_lag1 < 0);
        rho_effs(i) = P.rho_y + spline_active * P.eta_p * wk;
    end
    fprintf('  %8s %+9.2f %+9.2f %9.3f\n', qlbl{k}, ...
        median(obs_k), median(sim_k), median(rho_effs));
end

% Fiscal contribution
fprintf('\n  Fiscal contribution (Full - NoFiscal, cum %dQ):\n', K_act);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), zeros(1,N), zeros(1,N), zeros(1,N), ...
        zeros(1,N), cdata(i).FH, cdata(i).S, cdata(i).theta, cdata(i).d, ...
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

fprintf('\n  (2) Within-Country Autocorrelation:\n');
ac1_obs = zeros(n_c,1);  ac1_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_act);  ys = cdata(i).sim_y(1:K_act);
    co1 = corrcoef(yo(1:end-1), yo(2:end));  ac1_obs(i) = co1(1,2);
    cs1 = corrcoef(ys(1:end-1), ys(2:end));  ac1_sim(i) = cs1(1,2);
end
fprintf('  %20s %10.3f %10.3f\n', 'AC(1) mean', mean(ac1_obs), mean(ac1_sim));

y_obs_p = reshape([cdata.y], N, n_c)';
y_sim_p = reshape([cdata.sim_y], N, n_c)';
icc_y_obs = var(mean(y_obs_p(:,1:K_act),2)) / var(y_obs_p(:,1:K_act),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_act),2)) / var(y_sim_p(:,1:K_act),0,'all');
b_obs_p = zeros(n_c, K_act);
b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_act); end
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_act),2)) / var(b_sim_p(:,1:K_act),0,'all');
fprintf('\n  (3) ICC:\n');
fprintf('  %20s %10.3f %10.3f\n', 'ICC output', icc_y_obs, icc_y_sim);
fprintf('  %20s %10.3f %10.3f\n', 'ICC debt',   icc_b_obs, icc_b_sim);


%% ========================================================================
%  STEP 1c: DEBT RESIDUAL DIAGNOSTICS
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

corr_above = corrcoef(resid_b, frac_above);
corr_loans = corrcoef(resid_b, frac_loans);
corr_guar  = corrcoef(resid_b, frac_guar);
corr_cptot = corrcoef(resid_b, cp_total);
fprintf('\n  (2) Residual correlations:\n');
fprintf('    Residual vs %% above-the-line: r = %+.3f\n', corr_above(1,2));
fprintf('    Residual vs %% loans:          r = %+.3f\n', corr_loans(1,2));
fprintf('    Residual vs %% guarantees:     r = %+.3f\n', corr_guar(1,2));
fprintf('    Residual vs total CP:         r = %+.3f\n', corr_cptot(1,2));

fprintf('\n  (3) Residual summary:\n');
fprintf('    Mean:   %+.2f pp\n', mean(resid_b));
fprintf('    Median: %+.2f pp\n', median(resid_b));
fprintf('    SD:     %+.2f pp\n', std(resid_b));
fprintf('    Range:  [%+.2f, %+.2f] pp\n', min(resid_b), max(resid_b));

% --- Mortality drag (non-targeted) ---
d_contrib_y = zeros(n_c,1);
d_total     = zeros(n_c,1);
for i = 1:n_c
    d_contrib_y(i) = P.beta_fear * sum(cdata(i).d(1:K_act));
    d_total(i)     = sum(cdata(i).d(1:K_act));
end
fprintf('\n  (4) Mortality channel:\n');
fprintf('    Cum excess mortality (median): %.2f pp\n', median(d_total));
fprintf('    Implied drag on y (median):    %.2f pp\n', median(d_contrib_y));
corr_d_resid_b = corrcoef(d_total, resid_b);
fprintf('    Corr(cum d, debt resid):       r = %+.3f\n', corr_d_resid_b(1,2));



%% ========================================================================
%  STEP 1d: MORTALITY EQUATION VALIDATION (Section 5-6 plausibility)
%
%  Forward-roll theta + d transition equations from Stage 1 calibration.
%  Compares simulated to observed for plausibility check (NOT estimation).
%
%  Stage 1 calibration sources:
%    - rho_theta:    Liu & Rocklöv (2021), generation time ~5d, R_0 ~2.5
%    - phi_S:        Haug et al. (2020 NHB), NPI effectiveness on R_eff
%    - delta_theta:  IHME/COVID-19 Forecasting Team (2022, Lancet), IFR
%
%  Requirements (Stage 2 identification):
%    1. Correct ranking over time/countries
%    2. Correct ranking over waves
%    3. No systematic correlation of measurement errors with F
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1d: Mortality Equation Validation\n');
fprintf('========================================\n');

% --- Stage 1 calibration ---
rho_theta   = 0.85;
phi_S       = 0.012;
delta_theta = 0.025;

fprintf('  Calibration: rho_theta=%.3f, phi_S=%.4f, delta_theta=%.4f\n', ...
    rho_theta, phi_S, delta_theta);

% --- Forward-roll, anchored at first non-zero theta ---
% --- Wave anchoring: re-anchor theta_sim at start of each wave ---
%  Wave starts identified by theta_obs > previous theta_obs * 1.5 (relative jump)
for i = 1:n_c
    theta_sim = zeros(1, N);
    d_sim     = zeros(1, N);
    
    % Identify wave-start quarters (theta jumps up significantly)
    is_wave_start = false(1, N);
    is_wave_start(1) = true;  % always anchor at k=1
    for k = 2:N
        if cdata(i).theta(k) > max(cdata(i).theta(1:k-1)) * 1.2 ...
                && cdata(i).theta(k) > 0.05
            is_wave_start(k) = true;
        end
    end
    
    % Forward-roll, re-anchoring at each wave start
    theta_sim(1) = cdata(i).theta(1);
    d_sim(1)     = cdata(i).d(1);
    for k = 1:N-1
        if is_wave_start(k+1)
            theta_sim(k+1) = cdata(i).theta(k+1);   % re-anchor
            d_sim(k+1)     = cdata(i).d(k+1);
        else
            theta_sim(k+1) = rho_theta * (1 - phi_S * cdata(i).S(k)) * theta_sim(k);
            d_sim(k+1)     = delta_theta * theta_sim(k);
        end
    end
    
    cdata(i).theta_sim  = theta_sim;
    cdata(i).d_sim      = d_sim;
    cdata(i).rmse_theta = sqrt(mean((theta_sim - cdata(i).theta).^2));
    cdata(i).rmse_d     = sqrt(mean((d_sim     - cdata(i).d).^2));
end

fprintf('\n  Theta RMSE (median): %.3f pp\n', median([cdata.rmse_theta]));
fprintf('  d     RMSE (median): %.3f pp\n', median([cdata.rmse_d]));

% --- OECD median trajectory ---
fprintf('\n  OECD Median Trajectory:\n');
fprintf('  %8s %10s %10s %10s %10s\n', 'Quarter', 'theta_obs', 'theta_sim', 'd_obs', 'd_sim');
for k = 1:K_act
    th_o = median(arrayfun(@(c) c.theta(k),     cdata));
    th_s = median(arrayfun(@(c) c.theta_sim(k), cdata));
    d_o  = median(arrayfun(@(c) c.d(k),         cdata));
    d_s  = median(arrayfun(@(c) c.d_sim(k),     cdata));
    fprintf('  %8s %10.3f %10.3f %10.3f %10.3f\n', qlbl{k}, th_o, th_s, d_o, d_s);
end

% --- Cross-country ranking ---
theta_obs_cum = arrayfun(@(c) sum(c.theta(1:K_act)),     cdata)';
theta_sim_cum = arrayfun(@(c) sum(c.theta_sim(1:K_act)), cdata)';
d_obs_cum     = arrayfun(@(c) sum(c.d(1:K_act)),         cdata)';
d_sim_cum     = arrayfun(@(c) sum(c.d_sim(1:K_act)),     cdata)';

corr_theta = corrcoef(theta_obs_cum, theta_sim_cum);
corr_d     = corrcoef(d_obs_cum,     d_sim_cum);

manual_rank = @(x) arrayfun(@(v) sum(x < v) + 0.5*sum(x == v) + 0.5, x);
tmp = corrcoef(manual_rank(theta_obs_cum), manual_rank(theta_sim_cum));  spear_theta = tmp(1,2);
tmp = corrcoef(manual_rank(d_obs_cum),     manual_rank(d_sim_cum));      spear_d     = tmp(1,2);

fprintf('\n  Cross-country validation (cumulative over %dQ):\n', K_act);
fprintf('  %20s   Pearson    Spearman\n', '');
fprintf('  %20s   %+.3f     %+.3f\n', 'theta ranking', corr_theta(1,2), spear_theta);
fprintf('  %20s   %+.3f     %+.3f\n', 'd ranking',     corr_d(1,2),     spear_d);

% --- Orthogonality check ---
F_total_cum = arrayfun(@(c) sum(c.FCP(1:K_act) + c.FDI(1:K_act)), cdata)';
resid_theta = theta_obs_cum - theta_sim_cum;
resid_d     = d_obs_cum     - d_sim_cum;
corr_rt_F = corrcoef(resid_theta, F_total_cum);
corr_rd_F = corrcoef(resid_d,     F_total_cum);
fprintf('\n  Orthogonality (Stage 2 requirement):\n');
fprintf('    Corr(theta resid, total F): r = %+.3f\n', corr_rt_F(1,2));
fprintf('    Corr(d resid,     total F): r = %+.3f\n', corr_rd_F(1,2));
fprintf('    (|r| < 0.2 required for measurement-error orthogonality)\n');


%% ========================================================================
%  VISUALIZATION: theta and d, observed vs simulated
% =========================================================================
theta_obs_all = reshape([cdata.theta],     N, n_c)';
theta_sim_all = reshape([cdata.theta_sim], N, n_c)';
d_obs_all     = reshape([cdata.d],         N, n_c)';
d_sim_all     = reshape([cdata.d_sim],     N, n_c)';

figure('Name','Mortality Eqn Validation (Stage 1)','Color','w','Position',[100 100 1100 400]);

subplot(1,2,1); hold on;
fill_iqr(1:N, theta_sim_all, [0 .4 .8], .15);
fill_iqr(1:N, theta_obs_all, [.5 .5 .5], .12);
plot(1:N, median(theta_sim_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(theta_obs_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('\theta (pp)'); title('Pandemic Prevalence \theta');
legend('','','Simulated','Observed','Location','NW','FontSize',7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp\nSpearman = %+.2f', ...
    median([cdata.rmse_theta]), spear_theta), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

subplot(1,2,2); hold on;
fill_iqr(1:N, d_sim_all, [0 .4 .8], .15);
fill_iqr(1:N, d_obs_all, [.5 .5 .5], .12);
plot(1:N, median(d_sim_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(d_obs_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('d (pp)'); title('Excess Mortality d');
legend('','','Simulated','Observed','Location','NW','FontSize',7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp\nSpearman = %+.2f', ...
    median([cdata.rmse_d]), spear_d), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

sgtitle('Stage 1 Mortality Equation Validation (non-targeted)','FontWeight','bold');




%% ========================================================================
%  VISUALIZATION
% =========================================================================
sim_y_all = reshape([cdata.sim_y], N, n_c)';
obs_y_all = reshape([cdata.y],     N, n_c)';
sim_b_all = reshape([cdata.sim_b], N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum; end

figure('Name','Calibration V5b (Extended N=494)','Color','w','Position',[50 50 1100 400]);

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

sgtitle('Calibration V5b — Extended N=494 (Q4.2019-Q4.2022)','FontWeight','bold');

fprintf('\n=== CALIBRATION COMPLETE ===\n\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
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

        spline_active = (y < 0);
        xs(1,k+1) = mu_y + P.rho_y*y + P.alpha_S*Sk ...
                  + spline_active * P.eta_p * w * y ...
                  + P.alpha_F_DI*z + P.beta_fear*dk + ey;

        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
                  + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
                  + P.kappa_F_DI*z ...
                  + P.kappa_H*hk ...
                  + P.phi_t*yr_idx;

        xs(3,k+1) = fk;
        xs(4,k+1) = gk;
    end
end

function fill_iqr(x, data, col, alpha)
    sd = sort(data);  n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end
