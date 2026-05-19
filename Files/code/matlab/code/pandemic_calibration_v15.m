%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V15 (main COVID period, Option B,
%  wave-specific delta_theta)
%
%  Health block is descriptive (sim_theta, sim_d reported) but does NOT
%  feed into the output equation: the death drag uses observed d_obs
%  directly (one-quarter lag, preserved from prior spec).
%
%  The stock-to-mortality coefficient delta_theta is wave-indexed to
%  mirror the wave-specific IFRs used in the upstream construction of
%  theta_obs in R (see descriptives.R, section 04_stage1_theta_imputation).
%  Without this, sim_d would inherit the shape of sim_theta unchanged and
%  miss the IFR collapse across variants (W1 0.9% -> W4 0.04%).
%
%  Units:
%    theta : share of population currently infected (built in R with
%            wave-specific IFR: W1=0.9% ... W4_omicron=0.04%)
%    d     : excess deaths per million per week (quarterly mean of weekly)
%    delta_theta : wave-indexed vector, deaths/10^6/wk per unit theta
%                  (see ifr_by_wave / wave_idx_q in Step 1)
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V15 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STEP 1: PARAMETERS
% =========================================================================

% --- Output equation ---
rho_y         =  0.231;
alpha_S       = -0.0952;
alpha_above   =  0.544;
alpha_below   =  0.261165;
alpha_DI_lag1 =  1.470;
alpha_S_DI    = -0.0406;
beta_d        =  0.0115;       % deaths/10^6/week -> pp output drag

% --- Take-up adjustments ---
takeup_loans  =  0.40;
takeup_guar   =  0.25;

% --- Debt equation ---
r_int       =  0.001;
gamma_y     =  0.176;
kappa_above =  0.392;
kappa_loans =  0.891;
kappa_guar  =  0.111;
kappa_DI    =  0.396;
phi_t       = -0.076;

% --- Health block (descriptive; theta_obs / d_obs enter as inputs) ---
rho_theta   =  1.035;
phi_S       =  0.314;        % S in [0,1]

% Wave-specific IFR (mirrors descriptives.R:4548-4552):
%   W1=0.9%  W1_summer=0.7%  W2_wt=0.6%  W2_alpha=0.4%  W3_delta=0.3%  W4_omicron=0.04%
ifr_by_wave         = [0.009, 0.007, 0.006, 0.004, 0.003, 0.0004];
delta_theta_by_wave = ifr_by_wave * 1e6;   % -> deaths/10^6/week per unit theta

% Quarterly variant-dominance assignment (OECD-median), Q4.19 ... Q4.22:
%   1=W1  2=W1_summer  3=W2_wt  4=W2_alpha  5=W3_delta  6=W4_omicron
wave_idx_q  = [1 1 1 2 3 3 4 5 5 6 6 6 6];
delta_theta = delta_theta_by_wave(wave_idx_q);   % 1xN row vector

%% ========================================================================
%  STEP 2: COUNTRY-LEVEL CONSTANTS
% =========================================================================

cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

cfe_y_val = [+1.1057, -1.0400, +0.3009, -0.0979, +1.2987, +1.3246, +1.9894, +0.1466, ...
             -3.5623, -1.7381, -0.0832, -4.8958, -1.8327, -1.4833, -1.8693, -3.3268, ...
             +0.2908, -2.0146, +8.3187, -4.8488, +2.3672, -0.5014, -1.9561, +0.6141, ...
             +0.7002, +2.3568, -0.6830, -3.2057, +1.0604, +1.2218, -1.0616, -0.5966, ...
             -2.7751, -0.2578, -1.8284, +1.0349, +4.0658, +1.0987];

cfe_b_val = [+0.2296, +0.3707, +0.3387, +0.7393, +0.5104, -0.0655, +1.4904, +0.9267, ...
             -0.0006, -0.0780, -0.5517, -0.2551, +0.3079, +0.2583, +0.0484, -0.0142, ...
             +0.1481, -0.0351, +1.3739, -0.1310, +0.7809, +0.1886, +0.0381, +0.3889, ...
             +0.3229, +1.0169, +0.8753, -0.5943, +0.4736, +0.7201, +1.2750, +0.0755, ...
             -0.9275, +0.6452, +0.0694, +0.3347, +0.9441, +1.3417];

b0_val = [ 37.6,  69.7,  77.9,  44.2,  16.3,  29.6,  47.7,  53.2, ...
           30.2,  34.0,  34.1,  86.0,  11.8,  54.5,  85.4, 107.0, ...
          205.0,  52.1,  54.3,  69.9,  56.9, 122.0, 199.0,  35.1, ...
           29.3,  26.6,  39.7,   7.25, 43.9,  16.6,  73.0,  45.6, ...
          111.0,  48.4,  72.1,  27.7,  54.0,  97.3];

eps_v14_val = [-3.62, -8.55, -6.46, -7.79, -4.73, -10.10, -11.20, -3.75, ...
               -5.10, -5.98, -1.95, -9.57, -2.43, -1.97, -8.49, -12.50, ...
               -9.94, -8.99, -1.10, -4.70, -0.66, -10.90, -6.82, -1.83, ...
               -0.03, -5.31, -7.43, -7.97, -5.17, -2.45, -5.49, -1.80, ...
               -9.75, -4.87, -6.78, -4.70, -10.40, -4.79];

cfe_y_map   = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map   = containers.Map(cfe_iso, cfe_b_val);
b0_map      = containers.Map(cfe_iso, b0_val);
eps_v14_map = containers.Map(cfe_iso, eps_v14_val);

%% ========================================================================
%  STEP 3: HORIZON & CONFIG
% =========================================================================

N = 13;
K_y = 11;           % Q4.19-Q2.22 (main COVID period; covers 2Q above-line lag)
K_b = 13;           % Q4.19-Q4.22 (full horizon, deterministic terminal costs)
K_theta = 11;       % Q4.19-Q2.22; theta_obs is wave-IFR consistent (R)
nx = 4;
nu = 5;

t_idx_raw = 4:16;
year_idx_vec = t_idx_raw;

P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, ...
    'alpha_above',alpha_above, 'alpha_below',alpha_below, ...
    'alpha_DI_lag1',alpha_DI_lag1, 'alpha_S_DI',alpha_S_DI, ...
    'beta_d',beta_d, ...
    'takeup_loans',takeup_loans, 'takeup_guar',takeup_guar, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, 'kappa_DI',kappa_DI, 'phi_t',phi_t, ...
    'rho_theta',rho_theta, 'phi_S',phi_S, 'delta_theta',delta_theta, ...
    'year_idx_vec',year_idx_vec, ...
    'N',N, 'K_y',K_y, 'K_b',K_b, 'K_theta',K_theta, 'nx',nx, 'nu',nu);

%% ========================================================================
%  STEP 4: LOAD MACRO DATA
% =========================================================================
fprintf('--- Loading macro data ---\n');
T = readtable('country_data_for_matlab.csv');

qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl = {'Q4.19','Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22','Q3.22','Q4.22'};

countries = unique(T.Country, 'stable');
n_c = length(countries);
cdata = struct();

for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;

    cdata(i).S         = zeros(1,N);
    cdata(i).FCP_above = zeros(1,N);
    cdata(i).FCP_loans = zeros(1,N);
    cdata(i).FCP_guar  = zeros(1,N);
    cdata(i).FDI       = zeros(1,N);

    cdata(i).y         = zeros(1,N);
    cdata(i).b_delta   = zeros(1,N);
    cdata(i).theta_obs = zeros(1,N);
    cdata(i).d_obs     = zeros(1,N);

    cdata(i).mu_y = 0; cdata(i).mu_b = 0; cdata(i).b0 = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end
    if isKey(b0_map, iso),    cdata(i).b0   = b0_map(iso);   end

    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k)         = row.S_mean_tw;
        cdata(i).FCP_above(k) = row.F_CP_above_3;
        cdata(i).FCP_loans(k) = row.F_CP_loans;
        cdata(i).FCP_guar(k)  = row.F_CP_guar_adj;
        cdata(i).FDI(k)       = row.F_DI;
        cdata(i).y(k)         = row.y_t_pct;
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b_delta(k) = row.debt_dR;
        end
    end

    cdata(i).FCP_loans_adj   = takeup_loans * cdata(i).FCP_loans;
    cdata(i).FCP_guar_adj    = (takeup_guar / 0.35) * cdata(i).FCP_guar;
    cdata(i).FCP_below_flow  = cdata(i).FCP_loans_adj + cdata(i).FCP_guar_adj;
    cdata(i).FCP_below_stock = cumsum(cdata(i).FCP_below_flow);

    cdata(i).obs_b_level = cdata(i).b0 + cumsum(cdata(i).b_delta);

    cdata(i).eps_y_vec = zeros(1, N+1);
    eps_q220 = -5.40;
    if isKey(eps_v14_map, iso), eps_q220 = eps_v14_map(iso); end
    cdata(i).eps_y_vec(4) = eps_q220;

    cdata(i).eps_theta_vec = zeros(1, N+1);
end

fprintf('  %d countries x %d quarters\n', n_c, N);
fprintf('  Take-up: loans %.0f%%, guarantees %.0f%%\n\n', ...
        takeup_loans*100, takeup_guar*100);

%% ========================================================================
%  STEP 4b: LOAD MORTALITY DATA -> theta_obs, d_obs (per million per week)
% =========================================================================
fprintf('--- Loading mortality data ---\n');
M = readtable('weekly_mortality_matlab.csv');
M.date = datetime(M.date);

M.qy = year(M.date);
M.qq = quarter(M.date);
M.qstr = strings(height(M),1);
for r = 1:height(M)
    M.qstr(r) = sprintf('Q%d.%d', M.qq(r), M.qy(r));
end

% Weekly deaths per million
M.d_pmw = M.deaths_w ./ M.pop * 1e6;

[gid, gC, gQ] = findgroups(M.Country, M.qstr);
theta_q = splitapply(@nanmean, M.theta_hat, gid);
d_q     = splitapply(@nanmean, M.d_pmw,     gid);

theta_map = containers.Map();
d_map     = containers.Map();
for r = 1:length(gC)
    key = sprintf('%s_%s', gC{r}, gQ{r});
    theta_map(key) = theta_q(r);
    d_map(key)     = d_q(r);
end
for i = 1:n_c
    iso = cdata(i).iso;
    for k = 1:N
        key = sprintf('%s_%s', iso, qord{k});
        if isKey(theta_map, key)
            v = theta_map(key); if ~isnan(v), cdata(i).theta_obs(k) = v; end
        end
        if isKey(d_map, key)
            v = d_map(key);     if ~isnan(v), cdata(i).d_obs(k) = v; end
        end
    end
    % Innovation shocks (theta_obs - expected)
    cdata(i).eps_theta_vec = zeros(1, N+1);
    for k = 2:N
        prev_theta = cdata(i).theta_obs(k-1);
        Sk_norm    = cdata(i).S(k) / 100; %to fit infections perfect: Sk_norm = cdata(i).S(k) / 100;
        expected   = P.rho_theta * (1 - P.phi_S * Sk_norm) * prev_theta;
        cdata(i).eps_theta_vec(k+1) = cdata(i).theta_obs(k) - expected;
    end
end

q220_th = arrayfun(@(c) c.theta_obs(3), cdata);
q220_d  = arrayfun(@(c) c.d_obs(3),     cdata);
fprintf('  Q2.20 theta: range [%.5f, %.5f], median %.5f\n', ...
    min(q220_th), max(q220_th), median(q220_th));
fprintf('  Q2.20 d (deaths/10^6/wk): range [%.2f, %.2f], median %.2f\n', ...
    min(q220_d), max(q220_d), median(q220_d));
fprintf('  Stationarity: rho_theta=%.3f, S/100 must be > %.3f\n\n', ...
    rho_theta, (1 - 1/rho_theta) / phi_S);

%% ========================================================================
%  STEP 5: FORWARD ROLL
% =========================================================================
for i = 1:n_c
    xs = forward_roll_v15(cdata(i), P);
    cdata(i).sim_y     = xs(1, 2:end);
    cdata(i).sim_b     = xs(2, 2:end);
    cdata(i).sim_theta = xs(3, 2:end);
    cdata(i).sim_d     = xs(4, 2:end);

    cdata(i).rmse_y     = sqrt(mean((cdata(i).sim_y(1:K_y)         - cdata(i).y(1:K_y)).^2));
    cdata(i).rmse_b     = sqrt(mean((cdata(i).sim_b(1:K_b)         - cdata(i).obs_b_level(1:K_b)).^2));
    cdata(i).rmse_theta = sqrt(mean((cdata(i).sim_theta(1:K_theta) - cdata(i).theta_obs(1:K_theta)).^2));
    cdata(i).rmse_d     = sqrt(mean((cdata(i).sim_d(1:K_theta)     - cdata(i).d_obs(1:K_theta)).^2));
end

%% ========================================================================
%  STEP 6: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 6: Validation (Wave 1, K_theta=%d)\n', K_theta);
fprintf('========================================\n');

fprintf('  Output RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n', ...
    K_y, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n', ...
    K_b, median([cdata.rmse_b]), mean([cdata.rmse_b]));
fprintf('  Theta  RMSE (k=1:%d) -- Median: %.5f      Mean: %.5f\n', ...
    K_theta, median([cdata.rmse_theta]), mean([cdata.rmse_theta]));
fprintf('  d      RMSE (k=1:%d) -- Median: %.2f      Mean: %.2f\n\n', ...
    K_theta, median([cdata.rmse_d]), mean([cdata.rmse_d]));

fprintf('  OECD Median Trajectory:\n');
fprintf('  %8s %8s %8s %7s %7s %9s %9s %8s %8s\n', ...
    'Quarter','y_obs','y_sim','b_obs','b_sim','th_obs','th_sim','d_obs','d_sim');
for k = 1:N
    yo  = median(arrayfun(@(c) c.y(k),            cdata));
    ys  = median(arrayfun(@(c) c.sim_y(k),        cdata));
    bo  = median(arrayfun(@(c) c.obs_b_level(k),  cdata));
    bs  = median(arrayfun(@(c) c.sim_b(k),        cdata));
    tho = median(arrayfun(@(c) c.theta_obs(k),    cdata));
    ths = median(arrayfun(@(c) c.sim_theta(k),    cdata));
    do  = median(arrayfun(@(c) c.d_obs(k),        cdata));
    ds  = median(arrayfun(@(c) c.sim_d(k),        cdata));
    marker = ' '; if k > K_theta, marker = '*'; end
    fprintf('  %8s %+8.2f %+8.2f %+7.2f %+7.2f %+9.5f %+9.5f %+8.2f %+8.2f %s\n', ...
        qlbl{k}, yo, ys, bo, bs, tho, ths, do, ds, marker);
end
fprintf('  (* outside Wave 1 window, not targeted)\n');

%% ========================================================================
%  STEP 7: NON-TARGETED MOMENTS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 7: Non-Targeted Moments\n');
fprintf('========================================\n');

sd_ratios = zeros(1, K_y);
for k = 1:K_y
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    sd_ratios(k) = std(sim_k) / max(std(obs_k), 1e-10);
end
fprintf('  SD ratio mean (output): %.3f\n', mean(sd_ratios));

ac1_obs = zeros(n_c,1); ac1_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_y); ys = cdata(i).sim_y(1:K_y);
    co = corrcoef(yo(1:end-1), yo(2:end)); ac1_obs(i) = co(1,2);
    cs = corrcoef(ys(1:end-1), ys(2:end)); ac1_sim(i) = cs(1,2);
end
fprintf('  AC(1) output:  obs %.3f / sim %.3f\n', mean(ac1_obs), mean(ac1_sim));

y_obs_p = reshape([cdata.y],     N, n_c)';
y_sim_p = reshape([cdata.sim_y], N, n_c)';
b_obs_p = zeros(n_c, K_b); b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_level(1:K_b); end
icc_y_obs = var(mean(y_obs_p(:,1:K_y),2)) / var(y_obs_p(:,1:K_y),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_y),2)) / var(y_sim_p(:,1:K_y),0,'all');
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_b),2)) / var(b_sim_p(:,1:K_b),0,'all');
fprintf('  ICC y:  obs %.3f / sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('  ICC b:  obs %.3f / sim %.3f\n', icc_b_obs, icc_b_sim);

%% ========================================================================
%  STEP 8: CHANNEL DECOMPOSITION & DEBT RESIDUALS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 8: Channels & Debt Residuals\n');
fprintf('========================================\n');

above_contrib  = zeros(n_c, 1);
below_contrib  = zeros(n_c, 1);
di_contrib     = zeros(n_c, 1);
health_contrib = zeros(n_c, 1);
total_fiscal   = zeros(n_c, 1);

for i = 1:n_c
    c = cdata(i);
    base_y = sum(c.sim_y(1:K_y));

    c_noab = c; c_noab.FCP_above = zeros(1,N);
    c_nobe = c; c_nobe.FCP_loans_adj = zeros(1,N); c_nobe.FCP_guar_adj = zeros(1,N);
                c_nobe.FCP_below_stock = zeros(1,N);
    c_nodi = c; c_nodi.FDI = zeros(1,N);
    c_nofi = c_noab; c_nofi.FCP_loans_adj = zeros(1,N); c_nofi.FCP_guar_adj = zeros(1,N);
                     c_nofi.FCP_below_stock = zeros(1,N); c_nofi.FDI = zeros(1,N);
    c_nohe = c; c_nohe.d_obs = zeros(1,N);   % Option B: zero observed death drag

    xs_noab = forward_roll_v15(c_noab, P);
    xs_nobe = forward_roll_v15(c_nobe, P);
    xs_nodi = forward_roll_v15(c_nodi, P);
    xs_nofi = forward_roll_v15(c_nofi, P);
    xs_nohe = forward_roll_v15(c_nohe, P);

    above_contrib(i)  = base_y - sum(xs_noab(1, 2:K_y+1));
    below_contrib(i)  = base_y - sum(xs_nobe(1, 2:K_y+1));
    di_contrib(i)     = base_y - sum(xs_nodi(1, 2:K_y+1));
    total_fiscal(i)   = base_y - sum(xs_nofi(1, 2:K_y+1));
    health_contrib(i) = base_y - sum(xs_nohe(1, 2:K_y+1));
end

fprintf('\n  Channel decomposition (cum %dQ, median):\n', K_y);
fprintf('    Above-Flow:   %+6.2f pp  (>0: %d/%d)\n', median(above_contrib), sum(above_contrib>0), n_c);
fprintf('    Below-Stock:  %+6.2f pp  (>0: %d/%d)\n', median(below_contrib), sum(below_contrib>0), n_c);
fprintf('    DI:           %+6.2f pp  (>0: %d/%d)\n', median(di_contrib),    sum(di_contrib>0),    n_c);
fprintf('    Total fiscal: %+6.2f pp  (>0: %d/%d)\n', median(total_fiscal),  sum(total_fiscal>0),  n_c);
fprintf('    Health drag:  %+6.2f pp  (<0: %d/%d)\n', median(health_contrib),sum(health_contrib<0),n_c);

resid_b = zeros(n_c, 1);
for i = 1:n_c
    resid_b(i) = cdata(i).obs_b_level(K_b) - cdata(i).sim_b(K_b);
end
[~, sort_idx] = sort(resid_b);
fprintf('\n  Debt Residuals (final period):\n');
fprintf('    Largest negative (model over-estimates):\n');
for j = 1:5
    i = sort_idx(j);
    fprintf('      %s: obs %+7.2f  sim %+7.2f  resid %+7.2f\n', ...
        cdata(i).iso, cdata(i).obs_b_level(K_b), cdata(i).sim_b(K_b), resid_b(i));
end
fprintf('    Largest positive (model under-estimates):\n');
for j = 0:4
    i = sort_idx(n_c - j);
    fprintf('      %s: obs %+7.2f  sim %+7.2f  resid %+7.2f\n', ...
        cdata(i).iso, cdata(i).obs_b_level(K_b), cdata(i).sim_b(K_b), resid_b(i));
end
fprintf('    Mean: %+.2f, Median: %+.2f, SD: %.2f\n', ...
        mean(resid_b), median(resid_b), std(resid_b));

 resid_y_end = zeros(n_c, 1);
  for i = 1:n_c
      resid_y_end(i) = cdata(i).y(K_y) - cdata(i).sim_y(K_y);
  end
  fprintf('\n  Output endpoint residual (k=K_y=%d, Q2.22):\n', K_y);
  fprintf('    Mean: %+.2f, Median: %+.2f, SD: %.2f\n', ...
          mean(resid_y_end), median(resid_y_end), std(resid_y_end));

%% ========================================================================
%  STEP 9: VISUALIZATION
% =========================================================================
sim_y_all     = reshape([cdata.sim_y],     N, n_c)';
obs_y_all     = reshape([cdata.y],         N, n_c)';
sim_b_all     = reshape([cdata.sim_b],     N, n_c)';
obs_b_all     = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_level; end
sim_theta_all = reshape([cdata.sim_theta], N, n_c)';
obs_theta_all = zeros(n_c, N);
for i = 1:n_c, obs_theta_all(i,:) = cdata(i).theta_obs; end
sim_d_all     = reshape([cdata.sim_d],     N, n_c)';
obs_d_all     = zeros(n_c, N);
for i = 1:n_c, obs_d_all(i,:) = cdata(i).d_obs; end

figure('Name','Calibration V15','Color','w','Position',[50 50 1300 700]);

subplot(2,2,1); hold on;
fill_iqr(1:K_y, sim_y_all(:,1:K_y), [0 .4 .8], .15);
fill_iqr(1:K_y, obs_y_all(:,1:K_y), [.5 .5 .5], .12);
plot(1:K_y, median(sim_y_all(:,1:K_y)), 'b-o', 'LineWidth', 2);
plot(1:K_y, median(obs_y_all(:,1:K_y)), 'k--s', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:K_y, 'XTickLabel', qlbl(1:K_y), 'XTickLabelRotation', 45);
ylabel('pp potential GDP'); title('Output Gap');
legend('','','Sim','Obs','Location','SE');

subplot(2,2,2); hold on;
fill_iqr(1:K_b, sim_b_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, obs_b_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(sim_b_all(:,1:K_b)), 'b-o', 'LineWidth', 2);
plot(1:K_b, median(obs_b_all(:,1:K_b)), 'k--s', 'LineWidth', 2);
grid on;
set(gca, 'XTick', 1:K_b, 'XTickLabel', qlbl(1:K_b), 'XTickLabelRotation', 45);
ylabel('% GDP'); title('Debt Level');
legend('','','Sim','Obs','Location','SE');

subplot(2,2,3); hold on;
fill_iqr(1:K_theta, sim_theta_all(:,1:K_theta), [.8 .2 .2], .15);
fill_iqr(1:K_theta, obs_theta_all(:,1:K_theta), [.5 .5 .5], .12);
plot(1:K_theta, median(sim_theta_all(:,1:K_theta)), 'r-o', 'LineWidth', 2);
plot(1:K_theta, median(obs_theta_all(:,1:K_theta)), 'k--s', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:K_theta, 'XTickLabel', qlbl(1:K_theta), 'XTickLabelRotation', 45);
ylabel('infection (share of population)'); title('\theta: Sim vs Obs (Wave 1)');
legend('','','Sim','Obs','Location','NE');

subplot(2,2,4); hold on;
fill_iqr(1:K_theta, sim_d_all(:,1:K_theta), [.6 .1 .6], .15);
fill_iqr(1:K_theta, obs_d_all(:,1:K_theta), [.5 .5 .5], .12);
plot(1:K_theta, median(sim_d_all(:,1:K_theta)), 'm-o', 'LineWidth', 2);
plot(1:K_theta, median(obs_d_all(:,1:K_theta)), 'k--s', 'LineWidth', 2);
grid on;
set(gca, 'XTick', 1:K_theta, 'XTickLabel', qlbl(1:K_theta), 'XTickLabelRotation', 45);
ylabel('deaths/10^6/week'); title('d: Sim vs Obs (Wave 1)');
legend('','','Sim','Obs','Location','NE');

sgtitle('Calibration V15 - Trilemma (y, b, \theta, d) | Wave 1','FontWeight','bold');

%% ========================================================================
%  STEP 10: CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V15                                     #\n');
fprintf('################################################################\n\n');

y_obs_sd  = std(reshape([cdata.y], 1, []));
b_obs_sd  = std(arrayfun(@(c) c.obs_b_level(K_b), cdata));
th_obs_sd = std(reshape([cdata.theta_obs], 1, []));
d_obs_sd  = std(reshape([cdata.d_obs], 1, []));
rmse_y_md = median([cdata.rmse_y]);
rmse_b_md = median([cdata.rmse_b]);
rmse_t_md = median([cdata.rmse_theta]);
rmse_d_md = median([cdata.rmse_d]);

fprintf('1. TARGETED MOMENTS\n');
fprintf('   Output RMSE: %.2f pp     (ratio %.2f)\n', rmse_y_md, rmse_y_md/y_obs_sd);
fprintf('   Debt   RMSE: %.2f pp     (ratio %.2f)\n', rmse_b_md, rmse_b_md/b_obs_sd);
fprintf('   Theta  RMSE: %.5f        (ratio %.2f)\n', rmse_t_md, rmse_t_md/th_obs_sd);
fprintf('   d      RMSE: %.2f        (ratio %.2f)\n', rmse_d_md, rmse_d_md/d_obs_sd);
fprintf('   Mean debt resid: %+.2f pp\n', mean(resid_b));

fprintf('\n2. NON-TARGETED MOMENTS\n');
fprintf('   SD ratio (y): %.3f\n', mean(sd_ratios));
fprintf('   AC(1) y: obs %.3f / sim %.3f\n', mean(ac1_obs), mean(ac1_sim));
fprintf('   ICC y: obs %.3f / sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('   ICC b: obs %.3f / sim %.3f\n', icc_b_obs, icc_b_sim);

fprintf('\n3. CHANNEL DECOMPOSITION (median pp)\n');
fprintf('   Above:  %+.2f\n', median(above_contrib));
fprintf('   Below:  %+.2f\n', median(below_contrib));
fprintf('   DI:     %+.2f\n', median(di_contrib));
fprintf('   Total F:%+.2f\n', median(total_fiscal));
fprintf('   Health: %+.2f (drag)\n', median(health_contrib));

fprintf('\n4. CHECKLIST\n');
checks = {
    'Output RMSE/SD < 0.7',       rmse_y_md/y_obs_sd < 0.7;
    'Debt RMSE/SD < 0.7',         rmse_b_md/b_obs_sd < 0.7;
    'Theta RMSE/SD < 1.0',        rmse_t_md/th_obs_sd < 1.0;
    'd RMSE/SD < 1.0',            rmse_d_md/d_obs_sd  < 1.0;
    '|Mean debt resid| < 2pp',    abs(mean(resid_b)) < 2;
    '|Median final debt resid| < 3 pp', abs(median(resid_b)) < 3;
    '|Median endpoint y resid| < 1pp',  abs(median(resid_y_end)) < 1;
    'SD ratio in [0.7, 1.3]',     mean(sd_ratios)>0.7 && mean(sd_ratios)<1.3;
    'AC(1) gap < 0.1',            abs(mean(ac1_obs)-mean(ac1_sim))<0.1;
    'Total fiscal > 0 (median)',  median(total_fiscal)>0;
    'Health drag < 0 (median)',   median(health_contrib)<0;
};
for i = 1:size(checks,1)
    status = '[FAIL]'; if checks{i,2}, status = '[ OK ]'; end
    fprintf('   %s  %s\n', status, checks{i,1});
end
fprintf('\n   PASSED: %d / %d\n\n', sum([checks{:,2}]), size(checks,1));

%% ========================================================================
%  FUNCTIONS
% =========================================================================
function xs = forward_roll_v15(c, P)
    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    xs(2,1) = c.b0;

    for k = 1:N_
        y     = xs(1,k);
        b     = xs(2,k);
        theta = xs(3,k);

        Sk     = idxget(c.S, k);
        fab_k  = idxget(c.FCP_above, k);
        floa_k = idxget(c.FCP_loans_adj, k);
        fgua_k = idxget(c.FCP_guar_adj, k);
        fab_l2 = 0; if k >= 3, fab_l2 = idxget(c.FCP_above, k-2); end
        fdi_l1 = 0; if k >= 2, fdi_l1 = idxget(c.FDI, k-1);       end
        kbe_k  = idxget(c.FCP_below_stock, k);

        ey   = idxget(c.eps_y_vec, k+1);
        eth  = idxget(c.eps_theta_vec, k+1);
        yr_k = idxget(P.year_idx_vec, k);

% --- Health Block ---
Sk_norm = Sk / 100;
xs(3,k+1) = P.rho_theta * (1 - P.phi_S * Sk_norm) * theta + eth;
xs(4,k+1) = P.delta_theta(k) * xs(3,k);   % wave-specific IFR mapping

        % --- Output (Option B: death drag from observed d, lag-1) ---
        d_obs_lag1 = idxget(c.d_obs, k-1);
        xs(1,k+1) = c.mu_y + P.rho_y * y + P.alpha_S * Sk ...
                  + P.alpha_above   * fab_l2 ...
                  + P.alpha_below   * kbe_k ...
                  + P.alpha_DI_lag1 * fdi_l1 ...
                  + P.alpha_S_DI    * Sk * fdi_l1 ...
                  - P.beta_d        * d_obs_lag1 ...
                  + ey;

        % --- Debt ---
        xs(2,k+1) = c.mu_b + (1+P.r_int) * b - P.gamma_y * y ...
                  + P.kappa_above * fab_k ...
                  + P.kappa_loans * floa_k ...
                  + P.kappa_guar  * fgua_k ...
                  + P.kappa_DI    * fdi_l1 ...
                  + P.phi_t       * yr_k;
    end
end

function v = idxget(vec, k)
    if k < 1 || k > length(vec), v = 0; else, v = vec(k); end
end

function fill_iqr(x, data, col, alpha)
    sd = sort(data); n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end