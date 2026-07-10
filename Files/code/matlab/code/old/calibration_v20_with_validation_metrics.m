%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V20 (main COVID period, Option B,
%  wave-specific delta_theta)
%
%  CHANGES vs V18 (alignment to solver V17, theta-block only):
%   (A) rho_theta is now QUARTER-VARYING with a vaccination break, matching
%       solver V17: rho_th_q = [rho_theta_pre x (q_vax-1), rho_theta_post x
%       (N-q_vax+1)] with rho_theta_pre=1.5, rho_theta_post=0.5, q_vax=8
%       (break at Q3.2021). Previously rho_theta was a constant 1.5.
%       Rationale: post-vaccination halving of the effective reproduction
%       term (de Gier et al., Eurosurveillance 2021 + vaccination coverage),
%       consistent with the solver's epidemic transition. Because the back-
%       fitted eps_theta absorb the entire theta path (theta is exogenous/
%       variant-driven), the level of rho leaves the fitted theta and d
%       paths numerically invariant; the break only re-labels how much of
%       theta_{k} is attributed to endogenous persistence vs. innovation.
%   (B) Wave-to-quarter mapping aligned to solver V17 in the MEDIAN block
%       (wave_idx_q). 
%         %         Mean block   (SP.delta_q):  [1 1 2 3 4 5 5 6 6 7 7 7 7]
%    
%  --- Inherited V18 documentation (unchanged) ---------------------------
%   (1) DEBT VALIDATION tracks BOTH the level and the quarterly CHANGE.
%       The level RMSE/SD ratio is mechanical: b starts at the imposed b0
%       and r~=0, so the simulated level is ~b0+cumsum(flows) and the ratio
%       is dominated by cross-country b0 dispersion, not by fiscal dynamics.
%       Additionally, because the debt equation has no active consolidation
%       term, the level drifts up in 2022 when observed debt is paid down;
%       this is outside the fiscal-buildup mechanism the model targets. The
%       CHANGE-based RMSE (on debt_dR) is the honest metric for whether the
%       fiscal pass-throughs reproduce debt DYNAMICS. Both are reported; the
%       checklist keys on the change metric.
%   (2) DEBT ROLL includes the exogenous HEALTH term kappa_H * F_H,
%       consistent with the debt estimating equation. Health is NOT
%       optimized; it enters debt only as an exogenous component. If the
%       CSV has no F_H column, the term falls to zero (V15 behavior).
%   (3) Guarantee take-up handled as Option B: face value in CSV,
%       takeup_guar * F_CP_guar in MATLAB (0.25x); loans takeup_loans (0.60x).
%       The Q2.2020 debt-change spike is partly missed because the largest
%       below-the-line packages enter take-up-scaled, consistent with the
%       feols pass-throughs; this is specification, not a fit error.
%
%  Health block is descriptive (sim_theta, sim_d reported) but does NOT
%  feed into the output equation: the death drag uses observed d_obs
%  directly (one-quarter lag, preserved from prior spec).
%
%  The stock-to-mortality coefficient delta_theta is wave-indexed to
%  mirror the wave-specific IFRs used in the upstream construction of
%  theta_obs in R (see descriptives.R, section 04_stage1_theta_imputation).
%
%  Units:
%    theta : share of population currently infected (built in R with the
%            wave-specific effective IFRs; see the IFR appendix)
%    d     : excess deaths per million per week (quarterly mean of weekly)
%    delta_theta : wave-indexed vector, deaths/10^6/wk per unit theta.
%                  Calibration uses ifr_by_wave (Step 1) =
%                  [0.5 0.2 0.7 0.4 0.2 0.2 0.02] %, mapped to quarters via
%                  wave_idx_q (see Step 1) for the median block and SP.delta_q
%                  (Step 10b) for the mean block.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V20 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STEP 1: PARAMETERS
% =========================================================================

% --- Output equation ---
rho_y         =  0.231;
alpha_S       = -0.095;
alpha_above   =  0.544; 
alpha_below   =  0.261; 
alpha_DI_lag1 =  1.470;
alpha_S_DI    = -0.041;
beta_d        =  0.0;       % nicht identifiziert auf Quartalsdaten

% --- Take-up adjustments ---
takeup_loans  =  0.6;
takeup_guar   =  0.25;

% --- Debt equation ---
r_int       =  0.001; 
gamma_y     =  0.117; 
kappa_above =  0.664;
kappa_loans =  0.836;
kappa_guar  =  0.536;
kappa_DI    =  0.526;
kappa_H     =  0.908;      
phi_t       = 0;     %kein Zeittrend

%all from data

% --- Health block (descriptive; theta_obs / d_obs enter as inputs) ---
% rho_theta is quarter-varying with a vaccination break
rho_theta_pre  =  1.5;     % pre-vaccinatin-> siehe Appendix für Diskussion + Vergleich mit Schätzung
rho_theta_post =  0.5;     % post-vaccination (de Gier et al. 2021 + coverage)
q_vax          =  8;       % break quarter (Q3.2021)
phi_S          =  0.8;     % S in [0,1]  (matches solver V17) # Central estimate (Brauner et al. 2021; Flaxman et al. 2020) and own calculation

% Wave-specific IFR (matches solver V17):
%   W1=0.5%  W1_summer=0.7%  W2_wt=0.5%  W2_alpha=0.4%  W3_delta=0.25%  W4_omicron=0.028%
ifr_by_wave         = [0.005 0.002 0.007 0.004 0.002 0.002 0.0002];  %%Quellen siehe Latex
delta_theta_by_wave = ifr_by_wave * 1e6;   % -> deaths/10^6/week per unit theta

% Quarterly variant-dominance assignment (OECD-median), Q4.19 ... Q4.22:
%   1=W1  2=W1_summer  3=W2_wt  4=W2_alpha  5=W3_delta  6=W4_omicron
wave_idx_q  = [1 1 2 3 4 4 5 6 6 6 7 7 7];   % aligned to solver V17
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

cfe_b_val = [-0.8525, -0.6001, -0.8731, -0.2648, -0.4315, -1.0291, +0.5492, +0.1213, ...
             -0.6570, -1.0947, -1.6607, -0.9624, -0.4810, -0.6315, -0.9667, -1.3722, ...
             -1.3041, -0.9670, -0.2702, -0.8165, -0.3418, -1.1930, -1.6567, -0.5136, ...
             -0.4824, +0.1193, +0.0475, -0.9082, -0.4272, -0.1091, +0.0389, -0.6929, ...
             -1.7242, -0.1307, -0.9910, -0.5114, -0.3150, -0.2251];

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

%everything from data

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

% Build quarter-varying rho vector (matches solver V17 construction):
%   [rho_pre x (q_vax-1), rho_post x (N-q_vax+1)]  -> length N.
rho_th_q = [repmat(rho_theta_pre, 1, q_vax-1), ...
            repmat(rho_theta_post, 1, N-q_vax+1)];
assert(numel(rho_th_q) == N, 'rho_th_q must have length N=%d', N);

P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, ...
    'alpha_above',alpha_above, 'alpha_below',alpha_below, ...
    'alpha_DI_lag1',alpha_DI_lag1, 'alpha_S_DI',alpha_S_DI, ...
    'beta_d',beta_d, ...
    'takeup_loans',takeup_loans, 'takeup_guar',takeup_guar, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, 'kappa_DI',kappa_DI, 'kappa_H',kappa_H, 'phi_t',phi_t, ...
    'rho_th_q',rho_th_q, 'q_vax',q_vax, 'phi_S',phi_S, 'delta_theta',delta_theta, ...
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

% --- Country exclusion for sensitivity analysis ---
excl_iso = {};                           % leer = alle 38; zum Testen z.B. {'MEX','COL','CRI'}
if ~isempty(excl_iso)
    keep = ~ismember(countries, excl_iso);
    fprintf('  Excluded: %s\n', strjoin(countries(~keep), ', '));
    countries = countries(keep);
end

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
    cdata(i).FH        = zeros(1,N);   % exogenous health spending (% GDP)

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
        cdata(i).FCP_guar(k)  = row.F_CP_guar;
        cdata(i).FDI(k)       = row.F_DI;
        cdata(i).y(k)         = row.y_t_pct;
        % Exogenous health spending: read if column exists, else stays 0.
        if ismember('F_H', T.Properties.VariableNames) && ~ismissing(row.F_H)
            cdata(i).FH(k)    = row.F_H;
        end
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b_delta(k) = row.debt_dR;
        end
    end

    cdata(i).FCP_loans_adj   = takeup_loans * cdata(i).FCP_loans;
    cdata(i).FCP_guar_adj    = takeup_guar * cdata(i).FCP_guar;
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
%Monthly Data for CRI, JPN and CRI
theta_map = containers.Map();
d_map     = containers.Map();
for r = 1:length(gC)
    key = sprintf('%s_%s', gC{r}, gQ{r});
    theta_map(key) = theta_q(r);
    d_map(key)     = d_q(r);
end

% Monthly-derived theta patch for CRI, JPN, TUR
T_theta_patch = readtable('theta_quarterly_CRI_JPN_TUR_frommonthly.csv');
for r = 1:height(T_theta_patch)
    key = sprintf('%s_%s', T_theta_patch.Country{r}, T_theta_patch.Quarter{r});
    theta_map(key) = T_theta_patch.theta_hat(r);
end
fprintf('  [patch] theta ueberschrieben fuer CRI/JPN/TUR (theta_map)\n');

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
    % Innovation shocks (theta_obs - expected); rho is now quarter-varying.
    cdata(i).eps_theta_vec = zeros(1, N+1);
    for k = 2:N
        prev_theta = cdata(i).theta_obs(k-1);
        Sk_norm    = cdata(i).S(k) / 100;
        expected   = P.rho_th_q(k) * (1 - P.phi_S * Sk_norm) * prev_theta;
        cdata(i).eps_theta_vec(k+1) = cdata(i).theta_obs(k) - expected;
    end
end

q220_th = arrayfun(@(c) c.theta_obs(3), cdata);
q220_d  = arrayfun(@(c) c.d_obs(3),     cdata);
fprintf('  Q2.20 theta: range [%.5f, %.5f], median %.5f\n', ...
    min(q220_th), max(q220_th), median(q220_th));
fprintf('  Q2.20 d (deaths/10^6/wk): range [%.2f, %.2f], median %.2f\n', ...
    min(q220_d), max(q220_d), median(q220_d));
% Stationarity is tightest at the largest rho (pre-vaccination). At rho<=1
% (post-vax) the bound (1-1/rho)/phi_S is non-positive, so any S>=0 suffices.
rho_max = max(P.rho_th_q);
fprintf('  Stationarity (binding at rho_max=%.3f, pre-vax): S/100 must be > %.3f\n\n', ...
    rho_max, (1 - 1/rho_max) / phi_S);

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
    % Debt: two metrics.
    %  (a) rmse_b_level: on the cumulated level (mechanical, ~0; kept for reference)
    %  (b) rmse_b_dlt  : on the quarterly CHANGE (honest: tests fiscal dynamics)
    cdata(i).rmse_b_level = sqrt(mean((cdata(i).sim_b(1:K_b) - cdata(i).obs_b_level(1:K_b)).^2));
    sim_db = [cdata(i).sim_b(1) - cdata(i).b0, diff(cdata(i).sim_b)];  % simulated dB_k
    cdata(i).sim_db   = sim_db;
    cdata(i).rmse_b_dlt = sqrt(mean((sim_db(1:K_b) - cdata(i).b_delta(1:K_b)).^2));
    cdata(i).rmse_b   = cdata(i).rmse_b_dlt;   % primary debt metric = change-based
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
fprintf('  Debt  dRMSE (change, k=1:%d) -- Median: %.2f pp   Mean: %.2f pp  [PRIMARY]\n', ...
    K_b, median([cdata.rmse_b_dlt]), mean([cdata.rmse_b_dlt]));
fprintf('  Debt  RMSE  (level,  k=1:%d) -- Median: %.2f pp   Mean: %.2f pp  [mechanical]\n', ...
    K_b, median([cdata.rmse_b_level]), mean([cdata.rmse_b_level]));
fprintf('  Theta  RMSE (k=1:%d) -- Median: %.5f      Mean: %.5f\n', ...
    K_theta, median([cdata.rmse_theta]), mean([cdata.rmse_theta]));
fprintf('  d      RMSE (k=1:%d) -- Median: %.2f      Mean: %.2f\n\n', ...
    K_theta, median([cdata.rmse_d]), mean([cdata.rmse_d]));

fprintf('  OECD Median Trajectory:\n');
fprintf('  %8s %8s %8s %7s %7s %7s %7s %9s %9s %8s %8s\n', ...
    'Quarter','y_obs','y_sim','b_obs','b_sim','dB_obs','dB_sim','th_obs','th_sim','d_obs','d_sim');
for k = 1:N
    yo  = median(arrayfun(@(c) c.y(k),            cdata));
    ys  = median(arrayfun(@(c) c.sim_y(k),        cdata));
    bo  = median(arrayfun(@(c) c.obs_b_level(k),  cdata));
    bs  = median(arrayfun(@(c) c.sim_b(k),        cdata));
    dbo = median(arrayfun(@(c) c.b_delta(k),      cdata));
    dbs = median(arrayfun(@(c) c.sim_db(k),       cdata));
    tho = median(arrayfun(@(c) c.theta_obs(k),    cdata));
    ths = median(arrayfun(@(c) c.sim_theta(k),    cdata));
    do  = median(arrayfun(@(c) c.d_obs(k),        cdata));
    ds  = median(arrayfun(@(c) c.sim_d(k),        cdata));
    marker = ' '; if k > K_theta, marker = '*'; end
    fprintf('  %8s %+8.2f %+8.2f %+7.2f %+7.2f %+7.2f %+7.2f %+9.5f %+9.5f %+8.2f %+8.2f %s\n', ...
        qlbl{k}, yo, ys, bo, bs, dbo, dbs, tho, ths, do, ds, marker);
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

figure('Name','Calibration V19','Color','w','Position',[50 50 1300 700]);

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

sgtitle('Calibration V19 - Trilemma (y, b, \theta, d) | Wave 1','FontWeight','bold');

% --- Separate figure: DEBT CHANGE tracking (the honest debt metric) ---
sim_db_all = zeros(n_c, N); obs_db_all = zeros(n_c, N);
for i = 1:n_c
    sim_db_all(i,:) = cdata(i).sim_db;
    obs_db_all(i,:) = cdata(i).b_delta;
end
figure('Name','Debt change V19','Color','w','Position',[80 80 700 420]);
hold on;
fill_iqr(1:K_b, sim_db_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, obs_db_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(sim_db_all(:,1:K_b)), 'b-o', 'LineWidth', 2);
plot(1:K_b, median(obs_db_all(:,1:K_b)), 'k--s', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:K_b, 'XTickLabel', qlbl(1:K_b), 'XTickLabelRotation', 45);
ylabel('\Delta debt, pp of 2019 GDP');
title('Quarterly Debt Change: Sim vs Obs (PRIMARY debt metric)');
legend('','','Sim \DeltaB','Obs \DeltaB','Location','NE');

%% ========================================================================
%  STEP 10: CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V19                                     #\n');
fprintf('################################################################\n\n');

y_obs_sd  = std(reshape([cdata.y], 1, []));
% Two debt SD denominators, matching the two RMSE metrics:
all_b_delta = [];
for i = 1:n_c, all_b_delta = [all_b_delta, cdata(i).b_delta(1:K_b)]; end
b_dlt_sd   = std(all_b_delta);                                    % change SD (primary)
b_lvl_sd   = std(arrayfun(@(c) c.obs_b_level(K_b), cdata));       % final-level cross-country SD (reference)
th_obs_sd = std(reshape([cdata.theta_obs], 1, []));
d_obs_sd  = std(reshape([cdata.d_obs], 1, []));
rmse_y_md = median([cdata.rmse_y]);
rmse_b_dlt_md = median([cdata.rmse_b_dlt]);
rmse_b_lvl_md = median([cdata.rmse_b_level]);
rmse_t_md = median([cdata.rmse_theta]);
rmse_d_md = median([cdata.rmse_d]);

fprintf('1. TARGETED MOMENTS\n');
fprintf('   Output RMSE:        %.2f pp   (ratio %.2f)\n', rmse_y_md, rmse_y_md/y_obs_sd);
fprintf('   Debt dRMSE (change):%.2f pp   (ratio %.2f)  [PRIMARY: tests dynamics]\n', ...
        rmse_b_dlt_md, rmse_b_dlt_md/b_dlt_sd);
fprintf('   Debt RMSE  (level): %.2f pp   (ratio %.2f)  [mechanical: b0 imposed]\n', ...
        rmse_b_lvl_md, rmse_b_lvl_md/b_lvl_sd);
fprintf('   Theta  RMSE:        %.5f      (ratio %.2f)\n', rmse_t_md, rmse_t_md/th_obs_sd);
fprintf('   d      RMSE:        %.2f      (ratio %.2f)\n', rmse_d_md, rmse_d_md/d_obs_sd);
fprintf('   Mean debt resid (final level): %+.2f pp\n', mean(resid_b));

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
    'Output RMSE/SD < 0.7',                rmse_y_md/y_obs_sd < 0.7;
    'Debt dRMSE/SD < 0.7 (change,PRIMARY)',rmse_b_dlt_md/b_dlt_sd < 0.7;
    'Theta RMSE/SD < 1.0',                 rmse_t_md/th_obs_sd < 1.0;
    'd RMSE/SD < 1.0',                     rmse_d_md/d_obs_sd  < 1.0;
    '|Mean debt resid (level)| < 2pp',     abs(mean(resid_b)) < 2;
    '|Median final debt resid| < 3 pp',    abs(median(resid_b)) < 3;
    '|Median endpoint y resid| < 1pp',     abs(median(resid_y_end)) < 1;
    'SD ratio in [0.7, 1.3]',              mean(sd_ratios)>0.7 && mean(sd_ratios)<1.3;
    'AC(1) gap < 0.1',                     abs(mean(ac1_obs)-mean(ac1_sim))<0.1;
    'Total fiscal > 0 (median)',           median(total_fiscal)>0;
};
for i = 1:size(checks,1)
    status = '[FAIL]'; if checks{i,2}, status = '[ OK ]'; end
    fprintf('   %s  %s\n', status, checks{i,1});
end
fprintf('\n   PASSED: %d / %d\n\n', sum([checks{:,2}]), size(checks,1));


%% ========================================================================
%  STEP 10b: MEAN VALIDATION — REPRESENTATIVE (AVERAGE) OECD ECONOMY
%  -------------------------------------------------------------------------
%  PURPOSE. The Median validation above (Steps 6, 9) checks the CALIBRATION
%  model country by country and aggregates by the cross-country MEDIAN. The
%  optimal-control benchmark (solver V17), however, optimizes a single
%  REPRESENTATIVE economy built from cross-country MEANS of all inputs, with
%  country fixed effects set to zero. This block validates exactly THAT
%  object: one forward roll on mean-aggregated inputs, mu_y = mu_b = 0,
%  compared against the MEAN of the observed data.
%
%  CLOSURE. By explicit design decision this roll uses the SOLVER closure,
%  not the calibration closure, so that the validated object is the exact
%  solver economy. Concretely it differs from forward_roll_v15 in:
%     - below-the-line: stock with geometric decay (decay_K) and solver
%       take-up coefficients c_lo, c_gu (NOT the calibration cumsum with
%       take-up 0.60/0.25);
%     - NO exogenous health term kappa_H in the debt equation;
%     - theta innovations entered with the solver guard eth = max(eth,0)
%       before vaccination (q < q_vax), and read at slot (q+2);
%     - delta_q wave mapping identical to the (now aligned) wave_idx_q.
%  CONSEQUENCE. This block therefore validates a DIFFERENT model than the
%  Median block: the SOLVER model. That is intentional — it demonstrates
%  that the object actually optimized reproduces the mean OECD data.
%
%  AGGREGATION. Inputs are cross-country means (matching solver U_obs =
%  [mean(S_o); ...]). theta innovations follow the solver exactly:
%  constructed per country, then averaged (C.eps_th = mean(eps_th,1)).
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  STEP 10b: MEAN VALIDATION (representative / solver economy)   #\n');
fprintf('################################################################\n\n');

% --- Solver-closure parameters (independent of the calibration P struct) --
SP = struct();
SP.rho_y=0.231; SP.alpha_S=-0.095; SP.alpha_above=0.544; SP.alpha_below=0.261;
SP.alpha_DI=1.470; SP.alpha_SDI=-0.041; SP.beta_d=0;
SP.c_lo=0.4; SP.c_gu=0.25; SP.r=0.001; SP.gamma_y=0.117;
SP.k_ab=0.664; SP.k_lo=0.836; SP.k_gu=0.536; SP.k_di=0.526; SP.phi_t=0;
SP.decay_K=0.1; SP.phi_S=0.8; SP.th_max=Inf;
SP.N=N; SP.n=8; SP.m=5; SP.q_vax=q_vax;
SP.rho_th_q = rho_th_q;                       % aligned vector built in Step 3
SP.delta_q = ifr_by_wave([1 1 2 3 4 5 5 6 6 7 7 7 7]) * 1e6;
SP.yr = 4:16;

% --- Cross-country MEAN inputs (matching solver aggregation) --------------
S_bar   = mean(reshape([cdata.S],         N, n_c)', 1);
Fab_bar = mean(reshape([cdata.FCP_above], N, n_c)', 1);
Flo_bar = mean(reshape([cdata.FCP_loans], N, n_c)', 1);   % face value; take-up in roll
Fgu_bar = mean(reshape([cdata.FCP_guar],  N, n_c)', 1);
Fdi_bar = mean(reshape([cdata.FDI],       N, n_c)', 1);
U_bar   = [S_bar; Fab_bar; Flo_bar; Fgu_bar; Fdi_bar];     % 5 x N

b0_bar  = mean(arrayfun(@(c) c.b0, cdata));

% --- theta innovations: solver convention (per-country, then averaged) ----
% eps_th(i, k+1) = theta_obs_i(k) - rho_th_q(k)*(1-phi_S*S_i(k)/100)*theta_obs_i(k-1)
eps_th_mat = zeros(n_c, N+1);
for i = 1:n_c
    for k = 2:N
        Sk = cdata(i).S(k);
        expct = SP.rho_th_q(k) * (1 - SP.phi_S*Sk/100) * cdata(i).theta_obs(k-1);
        eps_th_mat(i, k+1) = cdata(i).theta_obs(k) - expct;
    end
end
SC.eps_th = mean(eps_th_mat, 1);              % 1 x (N+1), solver C.eps_th

% --- output shock: solver split possibility across Q1.20 / Q2.20 ----------
mean_eps_v14 = mean(arrayfun(@(c) c.eps_y_vec(4), cdata));
fprintf('  mean_eps_v14 (kept sample): %.3f\n', mean_eps_v14);
SC.eps_y = zeros(1, N+1);
SC.eps_y(3) = 0 * mean_eps_v14;            % Q1.20 onset
SC.eps_y(4) = 1 * mean_eps_v14;            % Q2.20 through
SC.mu_y = 0; SC.mu_b = 0; SC.b0 = b0_bar;

% --- ONE representative roll (solver closure) -----------------------------
x0 = zeros(SP.n, 1); x0(2) = b0_bar;
Xbar = rollout_solver_closure(U_bar, x0, SP, SC);   % 8 x (N+1)

sim_y_bar     = Xbar(1, 2:end);
sim_b_bar     = Xbar(2, 2:end);
sim_theta_bar = Xbar(3, 1:end);
sim_d_bar     = Xbar(4, 1:end);

% --- MEAN of observed data (validation target) ----------------------------
y_obs_bar     = mean(reshape([cdata.y],         N, n_c)', 1);
th_obs_bar    = mean(reshape([cdata.theta_obs], N, n_c)', 1);
d_obs_bar     = mean(reshape([cdata.d_obs],     N, n_c)', 1);
b_obs_bar     = zeros(1, N);
b_lvl_mat     = zeros(n_c, N);
for i = 1:n_c, b_lvl_mat(i,:) = cdata(i).obs_b_level; end
b_obs_bar     = mean(b_lvl_mat, 1);
% observed mean quarterly debt change (for the change-based metric)
db_obs_bar    = mean(reshape([cdata.b_delta], N, n_c)', 1);
sim_db_bar    = [sim_b_bar(1) - b0_bar, diff(sim_b_bar)];

% --- RMSE of the representative roll vs mean data --------------------------
rmse_y_bar     = sqrt(mean((sim_y_bar(1:K_y)         - y_obs_bar(1:K_y)).^2));
rmse_b_dlt_bar = sqrt(mean((sim_db_bar(1:K_b)        - db_obs_bar(1:K_b)).^2));
rmse_b_lvl_bar = sqrt(mean((sim_b_bar(1:K_b)         - b_obs_bar(1:K_b)).^2));
rmse_theta_bar = sqrt(mean((sim_theta_bar(1:K_theta) - th_obs_bar(1:K_theta)).^2));
rmse_d_bar     = sqrt(mean((sim_d_bar(1:K_theta)     - d_obs_bar(1:K_theta)).^2));

fprintf('  Representative-economy RMSE (solver closure, mean inputs, mu=0):\n');
fprintf('    Output  RMSE (k=1:%d):        %.2f pp\n',  K_y,     rmse_y_bar);
fprintf('    Debt    dRMSE (change,1:%d):  %.2f pp  [PRIMARY]\n', K_b, rmse_b_dlt_bar);
fprintf('    Debt    RMSE  (level, 1:%d):  %.2f pp  [mechanical]\n', K_b, rmse_b_lvl_bar);
fprintf('    Theta   RMSE (k=1:%d):        %.5f\n', K_theta, rmse_theta_bar);
fprintf('    d       RMSE (k=1:%d):        %.2f\n\n', K_theta, rmse_d_bar);

fprintf('  Representative-economy trajectory (sim = one roll on mean inputs):\n');
fprintf('  %8s %8s %8s %8s %8s %9s %9s %8s %8s\n', ...
    'Quarter','y_obs','y_sim','b_obs','b_sim','th_obs','th_sim','d_obs','d_sim');
for k = 1:N
    marker = ' '; if k > K_theta, marker = '*'; end
    fprintf('  %8s %+8.2f %+8.2f %+8.2f %+8.2f %+9.5f %+9.5f %+8.2f %+8.2f %s\n', ...
        qlbl{k}, y_obs_bar(k), sim_y_bar(k), b_obs_bar(k), sim_b_bar(k), ...
        th_obs_bar(k), sim_theta_bar(k), d_obs_bar(k), sim_d_bar(k), marker);
end
fprintf('  (* outside Wave 1 window, not targeted)\n');

% --- Figure: representative-economy validation ----------------------------
figure('Name','Mean validation (representative economy)','Color','w', ...
       'Position',[60 60 1300 700]);

subplot(2,2,1); hold on;
plot(1:K_y, sim_y_bar(1:K_y), 'b-o','LineWidth',2);
plot(1:K_y, y_obs_bar(1:K_y), 'k--s','LineWidth',2);
yline(0,':'); grid on;
set(gca,'XTick',1:K_y,'XTickLabel',qlbl(1:K_y),'XTickLabelRotation',45);
ylabel('pp potential GDP'); title('Output gap (mean / representative)');
legend('Sim (repr.)','Obs (mean)','Location','SE');

subplot(2,2,2); hold on;
plot(1:K_b, sim_b_bar(1:K_b), 'b-o','LineWidth',2);
plot(1:K_b, b_obs_bar(1:K_b), 'k--s','LineWidth',2);
grid on;
set(gca,'XTick',1:K_b,'XTickLabel',qlbl(1:K_b),'XTickLabelRotation',45);
ylabel('% GDP'); title('Debt level (mean / representative)');
legend('Sim (repr.)','Obs (mean)','Location','SE');

subplot(2,2,3); hold on;
plot(1:K_theta, sim_theta_bar(1:K_theta), 'r-o','LineWidth',2);
plot(1:K_theta, th_obs_bar(1:K_theta),    'k--s','LineWidth',2);
yline(0,':'); grid on;
set(gca,'XTick',1:K_theta,'XTickLabel',qlbl(1:K_theta),'XTickLabelRotation',45);
ylabel('infection (share of population)'); title('\theta (mean / representative)');
legend('Sim (repr.)','Obs (mean)','Location','NE');

subplot(2,2,4); hold on;
plot(1:K_theta, sim_d_bar(1:K_theta), 'm-o','LineWidth',2);
plot(1:K_theta, d_obs_bar(1:K_theta), 'k--s','LineWidth',2);
grid on;
set(gca,'XTick',1:K_theta,'XTickLabel',qlbl(1:K_theta),'XTickLabelRotation',45);
ylabel('deaths/10^6/week'); title('d (mean / representative)');
legend('Sim (repr.)','Obs (mean)','Location','NE');

sgtitle('Mean validation: representative OECD economy (solver closure) vs mean data', ...
        'FontWeight','bold');


%% ========================================================================
%  FUNCTIONS
% =========================================================================

%% ========================================================================
%  STEP 5b: FORWARD ROLL — EPIDEMIC BLOCK ONLY, NO INNOVATIONS (quarterly)
%  -------------------------------------------------------------------------
%  Pure deterministic dynamics of the two epidemic transition equations:
%     theta_{k+1} = rho_th_q(k) * (1 - phi_S * S_k/100) * theta_k   (eps=0)
%     d_{k+1}     = delta_theta(k) * theta_k                        (wave IFR)
%  Seeded at the FIRST observed theta>0 (Q4.19 has theta=0 by construction,
%  which the multiplicative map cannot leave). This isolates how much of the
%  observed theta path the transition equation reproduces WITHOUT the
%  back-fitted eps_theta — i.e. how much is endogenous dynamics vs. exogenous
%  (variant-driven) innovations.
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 5b: Epidemic roll, NO innovations (quarterly)\n');
fprintf('========================================\n');

for i = 1:n_c
    th_free = zeros(1, N);     % theta with eps_theta = 0
    d_free  = zeros(1, N);     % d implied by th_free

    % Seed at first quarter with positive observed theta (skip Q4.19=0).
    k_seed = find(cdata(i).theta_obs > 0, 1, 'first');
    if isempty(k_seed), cdata(i).sim_theta_free = th_free;
                        cdata(i).sim_d_free = d_free; continue; end
    th_free(k_seed) = cdata(i).theta_obs(k_seed);   % seed from data

    for k = k_seed:N
        Sk_norm = cdata(i).S(k) / 100;
        % deaths in quarter k from theta in quarter k (same convention as
        % your forward_roll_v15: xs(4,k+1) = delta_theta(k)*xs(3,k))
        d_free(k) = P.delta_theta(k) * th_free(k);
        % theta transition WITHOUT eps (the honest test); rho quarter-varying
        if k < N
            th_free(k+1) = P.rho_th_q(k) * (1 - P.phi_S * Sk_norm) * th_free(k);
        end
    end

    cdata(i).sim_theta_free = th_free;
    cdata(i).sim_d_free     = d_free;

    % RMSE of the innovation-free path vs observed (Wave-1 window)
    rng = k_seed:K_theta;
    cdata(i).rmse_theta_free = sqrt(mean((th_free(rng) - cdata(i).theta_obs(rng)).^2));
    cdata(i).rmse_d_free     = sqrt(mean((d_free(rng)  - cdata(i).d_obs(rng)).^2));
end

% --- Compare: innovation-free vs full (with eps) vs observed ---------------
fprintf('  Median RMSE theta:  full (with eps) %.5f  |  free (no eps) %.5f\n', ...
    median([cdata.rmse_theta]), median([cdata.rmse_theta_free]));
fprintf('  Median RMSE d:      full (with eps) %.2f  |  free (no eps) %.2f\n', ...
    median([cdata.rmse_d]),     median([cdata.rmse_d_free]));
fprintf('  (free >> full  =>  the eps_theta carry the dynamics; theta is\n');
fprintf('   largely exogenous/variant-driven, not endogenous to S.)\n\n');

fprintf('  OECD median: observed vs free (no-innovation) epidemic path\n');
fprintf('  %8s %10s %10s %9s %9s\n', ...
    'Quarter','th_obs','th_free','d_obs','d_free');
for k = 1:N
    tho = median(arrayfun(@(c) c.theta_obs(k),      cdata));
    thf = median(arrayfun(@(c) c.sim_theta_free(k), cdata));
    do  = median(arrayfun(@(c) c.d_obs(k),          cdata));
    df  = median(arrayfun(@(c) c.sim_d_free(k),     cdata));
    marker = ' '; if k > K_theta, marker = '*'; end
    fprintf('  %8s %+10.5f %+10.5f %+9.2f %+9.2f %s\n', ...
        qlbl{k}, tho, thf, do, df, marker);
end
fprintf('  (* outside Wave-1 window)\n');

% --- Optional: plot the free vs observed theta path ------------------------
sim_thfree_all = reshape([cdata.sim_theta_free], N, n_c)';
obs_theta_all2 = zeros(n_c, N);
for i = 1:n_c, obs_theta_all2(i,:) = cdata(i).theta_obs; end
figure('Name','Theta: no-innovation roll','Color','w','Position',[100 100 720 430]);
hold on;
plot(1:K_theta, median(sim_thfree_all(:,1:K_theta)), 'r-o','LineWidth',2);
plot(1:K_theta, median(obs_theta_all2(:,1:K_theta)), 'k--s','LineWidth',2);
yline(0,':'); grid on;
set(gca,'XTick',1:K_theta,'XTickLabel',qlbl(1:K_theta),'XTickLabelRotation',45);
ylabel('\theta (share infected)');
title('\theta: pure transition (no \epsilon) vs observed');
legend('Free (\epsilon=0)','Observed','Location','NE');




%% ========================================================================
%  ADDED VALIDATION METRICS EXPORT BLOCK
%  ------------------------------------------------------------------------
%  This block only reports and exports validation metrics. It does not change
%  parameters, states, forward rolls, timing conventions, figures, or any
%  previously computed quantities.
% =========================================================================

fprintf('\n################################################################\n');
fprintf('#  ADDED VALIDATION METRICS EXPORTS                             #\n');
fprintf('################################################################\n\n');

% -------------------------------------------------------------------------
% A. Targeted fit metrics: median and mean RMSEs with dispersion ratios
% -------------------------------------------------------------------------
Metric = [
    "Output gap";
    "Debt change";
    "Debt level";
    "Infection state theta";
    "Excess mortality d"
];

Horizon = [
    sprintf("k=1:%d", K_y);
    sprintf("k=1:%d", K_b);
    sprintf("k=1:%d", K_b);
    sprintf("k=1:%d", K_theta);
    sprintf("k=1:%d", K_theta)
];

Median_RMSE = [
    median([cdata.rmse_y]);
    median([cdata.rmse_b_dlt]);
    median([cdata.rmse_b_level]);
    median([cdata.rmse_theta]);
    median([cdata.rmse_d])
];

Mean_RMSE = [
    mean([cdata.rmse_y]);
    mean([cdata.rmse_b_dlt]);
    mean([cdata.rmse_b_level]);
    mean([cdata.rmse_theta]);
    mean([cdata.rmse_d])
];

SD_Denominator = [
    y_obs_sd;
    b_dlt_sd;
    b_lvl_sd;
    th_obs_sd;
    d_obs_sd
];

Median_RMSE_over_SD = Median_RMSE ./ SD_Denominator;
Mean_RMSE_over_SD   = Mean_RMSE   ./ SD_Denominator;

Primary_Metric = [
    true;
    true;
    false;
    true;
    true
];

T_validation_targeted = table( ...
    Metric, Horizon, Median_RMSE, Mean_RMSE, SD_Denominator, ...
    Median_RMSE_over_SD, Mean_RMSE_over_SD, Primary_Metric ...
);

disp(T_validation_targeted);
writetable(T_validation_targeted, 'validation_v20_targeted_metrics.csv');

% -------------------------------------------------------------------------
% B. Non-targeted dynamics and dispersion moments
% -------------------------------------------------------------------------
Moment = [
    "Output SD ratio, mean across quarters";
    "Output AC(1), observed mean";
    "Output AC(1), simulated mean";
    "Output AC(1), absolute gap";
    "Output ICC, observed";
    "Output ICC, simulated";
    "Debt ICC, observed";
    "Debt ICC, simulated"
];

Value = [
    mean(sd_ratios);
    mean(ac1_obs);
    mean(ac1_sim);
    abs(mean(ac1_obs) - mean(ac1_sim));
    icc_y_obs;
    icc_y_sim;
    icc_b_obs;
    icc_b_sim
];

T_validation_nontargeted = table(Moment, Value);
disp(T_validation_nontargeted);
writetable(T_validation_nontargeted, 'validation_v20_nontargeted_moments.csv');

% -------------------------------------------------------------------------
% C. Endpoint residuals by country and summary statistics
%     residual = observed - simulated
% -------------------------------------------------------------------------
iso = strings(n_c,1);
resid_b_end     = zeros(n_c,1);
resid_y_endpoint = zeros(n_c,1);
resid_theta_end = zeros(n_c,1);
resid_d_end     = zeros(n_c,1);

for i = 1:n_c
    iso(i) = string(cdata(i).iso);
    resid_b_end(i)      = cdata(i).obs_b_level(K_b) - cdata(i).sim_b(K_b);
    resid_y_endpoint(i) = cdata(i).y(K_y)           - cdata(i).sim_y(K_y);
    resid_theta_end(i)  = cdata(i).theta_obs(K_theta) - cdata(i).sim_theta(K_theta);
    resid_d_end(i)      = cdata(i).d_obs(K_theta)     - cdata(i).sim_d(K_theta);
end

y_obs_end = arrayfun(@(c) c.y(K_y),            cdata)';
y_sim_end = arrayfun(@(c) c.sim_y(K_y),        cdata)';
d_obs_end = arrayfun(@(c) c.d_obs(K_theta),    cdata)';
d_sim_end = arrayfun(@(c) c.sim_d(K_theta),    cdata)';
b_obs_end = arrayfun(@(c) c.obs_b_level(K_b),  cdata)';
b_sim_end = arrayfun(@(c) c.sim_b(K_b),        cdata)';

T_endpoint_full = table(iso, ...
    y_obs_end, y_sim_end, y_obs_end - y_sim_end, ...
    d_obs_end, d_sim_end, d_obs_end - d_sim_end, ...
    b_obs_end, b_sim_end, b_obs_end - b_sim_end, ...
    'VariableNames', {'Country', ...
      'y_obs','y_sim','y_resid', ...
      'd_obs','d_sim','d_resid', ...
      'b_obs','b_sim','b_resid'});
writetable(T_endpoint_full, 'validation_v20_endpoint_full.csv');

T_validation_endpoint_by_country = table( ...
    iso, resid_y_endpoint, resid_b_end, resid_theta_end, resid_d_end, ...
    'VariableNames', {'Country','Output_resid_endpoint','Debt_resid_terminal', ...
                      'Theta_resid_endpoint','Deaths_resid_endpoint'} ...
);

disp(T_validation_endpoint_by_country);
writetable(T_validation_endpoint_by_country, 'validation_v20_endpoint_residuals_by_country.csv');

Endpoint_Metric = [
    "Output endpoint residual";
    "Terminal debt residual";
    "Theta endpoint residual";
    "Deaths endpoint residual"
];

Endpoint_Horizon = [
    string(qlbl{K_y});
    string(qlbl{K_b});
    string(qlbl{K_theta});
    string(qlbl{K_theta})
];

Endpoint_Mean = [
    mean(resid_y_endpoint);
    mean(resid_b_end);
    mean(resid_theta_end);
    mean(resid_d_end)
];

Endpoint_Median = [
    median(resid_y_endpoint);
    median(resid_b_end);
    median(resid_theta_end);
    median(resid_d_end)
];

Endpoint_SD = [
    std(resid_y_endpoint);
    std(resid_b_end);
    std(resid_theta_end);
    std(resid_d_end)
];

Endpoint_P25 = [
    prctile(resid_y_endpoint,25);
    prctile(resid_b_end,25);
    prctile(resid_theta_end,25);
    prctile(resid_d_end,25)
];

Endpoint_P75 = [
    prctile(resid_y_endpoint,75);
    prctile(resid_b_end,75);
    prctile(resid_theta_end,75);
    prctile(resid_d_end,75)
];

T_validation_endpoint_summary = table( ...
    Endpoint_Metric, Endpoint_Horizon, Endpoint_Mean, Endpoint_Median, ...
    Endpoint_SD, Endpoint_P25, Endpoint_P75 ...
);

disp(T_validation_endpoint_summary);
writetable(T_validation_endpoint_summary, 'validation_v20_endpoint_residuals_summary.csv');

% -------------------------------------------------------------------------
% D. Channel decomposition summary and country-level exports
% -------------------------------------------------------------------------
T_validation_channels_by_country = table( ...
    iso, above_contrib, below_contrib, di_contrib, total_fiscal, health_contrib, ...
    'VariableNames', {'Country','Above_contribution','Below_contribution', ...
                      'DI_contribution','Total_fiscal_contribution','Health_drag'} ...
);

disp(T_validation_channels_by_country);
writetable(T_validation_channels_by_country, 'validation_v20_channel_decomposition_by_country.csv');

Channel = [
    "Above-line fiscal support";
    "Below-line liquidity support";
    "Demand injection";
    "Total fiscal support";
    "Health drag"
];

Channel_Mean = [
    mean(above_contrib);
    mean(below_contrib);
    mean(di_contrib);
    mean(total_fiscal);
    mean(health_contrib)
];

Channel_Median = [
    median(above_contrib);
    median(below_contrib);
    median(di_contrib);
    median(total_fiscal);
    median(health_contrib)
];

Channel_Positive_Count = [
    sum(above_contrib > 0);
    sum(below_contrib > 0);
    sum(di_contrib > 0);
    sum(total_fiscal > 0);
    sum(health_contrib > 0)
];

Channel_Negative_Count = [
    sum(above_contrib < 0);
    sum(below_contrib < 0);
    sum(di_contrib < 0);
    sum(total_fiscal < 0);
    sum(health_contrib < 0)
];

T_validation_channels_summary = table( ...
    Channel, Channel_Mean, Channel_Median, ...
    Channel_Positive_Count, Channel_Negative_Count ...
);

disp(T_validation_channels_summary);
writetable(T_validation_channels_summary, 'validation_v20_channel_decomposition_summary.csv');

% -------------------------------------------------------------------------
% E. Representative-economy validation metrics from Step 10b
% -------------------------------------------------------------------------
Rep_Metric = [
    "Output gap";
    "Debt change";
    "Debt level";
    "Infection state theta";
    "Excess mortality d"
];

Rep_Horizon = [
    sprintf("k=1:%d", K_y);
    sprintf("k=1:%d", K_b);
    sprintf("k=1:%d", K_b);
    sprintf("k=1:%d", K_theta);
    sprintf("k=1:%d", K_theta)
];

Rep_RMSE = [
    rmse_y_bar;
    rmse_b_dlt_bar;
    rmse_b_lvl_bar;
    rmse_theta_bar;
    rmse_d_bar
];

T_validation_representative = table(Rep_Metric, Rep_Horizon, Rep_RMSE);
disp(T_validation_representative);
writetable(T_validation_representative, 'validation_v20_representative_metrics.csv');

% -------------------------------------------------------------------------
% F. Checklist export
% -------------------------------------------------------------------------
Checklist_Item = string(checks(:,1));
Checklist_Passed = logical(cell2mat(checks(:,2)));
T_validation_checklist = table(Checklist_Item, Checklist_Passed);
disp(T_validation_checklist);
writetable(T_validation_checklist, 'validation_v20_checklist.csv');

fprintf('\nSaved added validation outputs:\n');
fprintf('  validation_v20_targeted_metrics.csv\n');
fprintf('  validation_v20_nontargeted_moments.csv\n');
fprintf('  validation_v20_endpoint_residuals_by_country.csv\n');
fprintf('  validation_v20_endpoint_residuals_summary.csv\n');
fprintf('  validation_v20_channel_decomposition_by_country.csv\n');
fprintf('  validation_v20_channel_decomposition_summary.csv\n');
fprintf('  validation_v20_representative_metrics.csv\n');
fprintf('  validation_v20_checklist.csv\n\n');


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

% --- Health Block (rho is quarter-varying with the vaccination break) ---
Sk_norm = Sk / 100;
xs(3,k+1) = P.rho_th_q(k) * (1 - P.phi_S * Sk_norm) * theta + eth;
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
        % Health enters exogenously with one-quarter lag (matches eq:debt_est,
        % F^H_{k-1}). Not optimized; pure exogenous debt component.
        fh_l1 = idxget(c.FH, k-1);
        xs(2,k+1) = c.mu_b + (1+P.r_int) * b - P.gamma_y * y ...
                  + P.kappa_above * fab_k ...
                  + P.kappa_loans * floa_k ...
                  + P.kappa_guar  * fgua_k ...
                  + P.kappa_DI    * fdi_l1 ...
                  + P.kappa_H     * fh_l1 ...
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

%% ========================================================================
%  INSERTION POINT 1 of 2
%  -------------------------------------------------------------------------
%  Paste this ENTIRE block into your V20 calibration script directly AFTER
%  the last line of Step 10b, i.e. right after:
%
%      sgtitle('Mean validation: representative OECD economy (solver closure) vs mean data', ...
%              'FontWeight','bold');
%
%  and BEFORE the line:
%
%      %% ========================================================================
%      %  FUNCTIONS
%
%  It uses variables already in scope at that point: N, n_c, cdata, countries,
%  rho_th_q, q_vax, phi_S, ifr_by_wave, year_idx_vec, K_y, K_b, K_theta.
% =========================================================================

%% ========================================================================
%  STEP 10c: SOLVER-CLOSURE (V21, n=9) VALIDATION AT COUNTRY LEVEL
%  -------------------------------------------------------------------------
%  PURPOSE. Step 10b validates a solver closure that mirrors V17 (n=8, no
%  capacity-preservation stock). The closure actually used by solver V21 --
%  the one under which the per-country planner problems are solved -- adds
%  a capacity stock and a bilinear above/below complementarity term, and
%  drops the exogenous kappa_H health term from the debt equation. This
%  block re-implements THAT exact closure (f_step_v21, mirrored verbatim
%  from the V21 solver script) and validates it at country level, using the
%  SAME structural parameters as the panel-wide solve (shared, not
%  re-estimated) and only country-specific inputs (S, F*, eps_th, eps_y,
%  b0, mu_y, mu_b).
%
%  ASSUMPTION TO CONFIRM. Country-level rolls below use the country fixed
%  effects (mu_y_i, mu_b_i) from cfe_y_map / cfe_b_map, matching how Step 5's
%  additive-closure country validation is set up. If your actual per-country
%  V21 solver run uses mu_y = mu_b = 0 (treating heterogeneity as entering
%  only through inputs and b0, as the representative-economy solve does),
%  change C21_i.mu_y / C21_i.mu_b below to 0 accordingly.
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  STEP 10c: SOLVER-CLOSURE (V21) VALIDATION, COUNTRY LEVEL       #\n');
fprintf('################################################################\n\n');

% --- Shared V21 structural parameters (identical to the solver script) ----
P21 = struct();
P21.rho_y = 0.231; P21.alpha_S = -0.095; P21.alpha_above = 0.544;
P21.alpha_below = 0.261; P21.alpha_DI = 1.470; P21.alpha_SDI = -0.041;
P21.beta_d = 0; P21.r = 0.001; P21.gamma_y = 0.117;
P21.k_ab = 0.664; P21.k_lo = 0.836; P21.k_gu = 0.536; P21.k_di = 0.526;
P21.phi_t = 0;
P21.rho_th_q = rho_th_q;                 % from Step 3, quarter-varying, length N
P21.q_vax = q_vax; P21.phi_S = phi_S; P21.th_max = Inf;
P21.delta_q = ifr_by_wave([1 1 2 3 4 5 5 6 6 7 7 7 7]) * 1e6;  % mean-block mapping
P21.N = N; P21.n = 9; P21.m = 5; P21.yr = year_idx_vec;

target_half_life_cap_q = 6;
P21.decay_cap   = 1 - 0.5^(1/target_half_life_cap_q);
P21.alpha_cap   = 0.30 * P21.alpha_above;
P21.chi_cap_liq = 0.50;
P21.decay_K     = 0.1;

% cap_scale: 99th percentile of positive above-line spend, pooled across the
% FULL panel -- a shared/technical scale parameter, not re-fit per country,
% exactly as in the panel-wide V21 solve.
Fa_pool = arrayfun(@(c) c.FCP_above, cdata, 'UniformOutput', false);
Fa_pool = [Fa_pool{:}]; Fa_pool = Fa_pool(Fa_pool > 0);
P21.cap_scale = max(pctile21(Fa_pool, 99), 1);
fprintf('  P21.cap_scale = %.3f (99th pct of pooled above-line spend, n=%d obs)\n\n', ...
        P21.cap_scale, numel(Fa_pool));

country_list = {cdata.iso};   % robust to insertion order; do not depend on
                               % the later Added-Metrics block's `iso` var

% ---------------------------------------------------------------------
% Country-level solver-closure validation
% ---------------------------------------------------------------------
fprintf('  --- Country-level solver-closure (V21) validation ---\n');
rmse21_y  = zeros(n_c,1); rmse21_b  = zeros(n_c,1);
rmse21_th = zeros(n_c,1); rmse21_d  = zeros(n_c,1);

for i = 1:n_c
    c = cdata(i);
    U_i  = [c.S; c.FCP_above; c.FCP_loans_adj; c.FCP_guar_adj; c.FDI];  % take-up already applied
    x0_i = zeros(P21.n,1); x0_i(2) = c.b0;

    C21_i.mu_y  = c.mu_y;             % <-- confirm vs. your actual country-level solve
    C21_i.mu_b  = c.mu_b;             % <-- confirm vs. your actual country-level solve
    C21_i.eps_y  = c.eps_y_vec;       % length N+1, Q2.20 shock in slot 4
    C21_i.eps_th = c.eps_theta_vec;   % length N+1, same (q+2) convention as the solver

    X21_i = rollout_v21(U_i, x0_i, P21, C21_i);

    sim_y_i  = X21_i(1,2:end); sim_b_i  = X21_i(2,2:end);
    sim_th_i = X21_i(3,1:end); sim_d_i  = X21_i(4,1:end);

    rmse21_y(i)  = sqrt(mean((sim_y_i(1:K_y)      - c.y(1:K_y)).^2));
    rmse21_b(i)  = sqrt(mean((sim_b_i(1:K_b)      - c.obs_b_level(1:K_b)).^2));
    rmse21_th(i) = sqrt(mean((sim_th_i(1:K_theta) - c.theta_obs(1:K_theta)).^2));
    rmse21_d(i)  = sqrt(mean((sim_d_i(1:K_theta)  - c.d_obs(1:K_theta)).^2));
end

fprintf('  Median RMSE (solver closure)  y: %.2f  b(level): %.2f  theta: %.5f  d: %.2f\n', ...
        median(rmse21_y), median(rmse21_b), median(rmse21_th), median(rmse21_d));
fprintf('  For comparison, Step 6 additive-closure medians were  y: 1.85  b(level): 2.53  theta: 0.00000  d: 13.13\n\n');

T_solver_country = table(string(country_list)', rmse21_y, rmse21_b, rmse21_th, rmse21_d, ...
    'VariableNames', {'Country','RMSE_y_solver','RMSE_b_solver','RMSE_theta_solver','RMSE_d_solver'});
disp(T_solver_country);
writetable(T_solver_country, 'validation_v20_solver_closure_by_country.csv');

fprintf('\nSaved:\n  validation_v20_solver_closure_by_country.csv\n\n');


%% ========================================================================
%  INSERTION POINT 2 of 2
%  -------------------------------------------------------------------------
%  Paste this ENTIRE block into the LOCAL FUNCTIONS section at the very end
%  of your file, directly AFTER your existing `rollout_solver_closure`
%  function (which mirrors V17). These three functions are new and do not
%  replace anything already there.
% =========================================================================

function xp = f_step_v21(x, u, q, P, C)
% Exact mirror of solver V21's f_step (n=9: capacity-preservation stock +
% bilinear above/below complementarity). Used ONLY for validation rollouts
% with OBSERVED U as input -- never for optimization.
    S=u(1); fab=u(2); flo=u(3); fgu=u(4); fdi=u(5);
    y=x(1); b=x(2); th=x(3); d=x(4); a1=x(5); di1=x(7); st_liq=x(8); st_cap=x(9);

    liq_used = (1-P.decay_K)*st_liq + flo + fgu;
    cap_used = (1-P.decay_cap)*st_cap + fab;
    cap_multiplier = 1 + P.chi_cap_liq * cap_used / (cap_used + P.cap_scale);

    eth = 0;
    if (q+2) <= numel(C.eps_th), eth = C.eps_th(q+2); end
    if q < P.q_vax, eth = max(eth, 0); end

    xp = zeros(P.n,1);
    xp(3) = P.rho_th_q(q)*(1-P.phi_S*S/100)*th*(1 - th/P.th_max) + eth;
    xp(4) = P.delta_q(q)*th;
    xp(1) = C.mu_y + P.rho_y*y + P.alpha_S*S ...
          + P.alpha_cap*cap_used + P.alpha_below*liq_used*cap_multiplier ...
          + P.alpha_DI*di1 + P.alpha_SDI*S*di1 - P.beta_d*d + C.eps_y(q+1);
    xp(2) = C.mu_b + (1+P.r)*b - P.gamma_y*y + P.k_ab*fab ...
          + P.k_lo*flo + P.k_gu*fgu + P.k_di*di1 + P.phi_t*P.yr(q);
    xp(5) = fab; xp(6) = a1; xp(7) = fdi; xp(8) = liq_used; xp(9) = cap_used;
end

function X = rollout_v21(U, x0, P, C)
    X = zeros(P.n, P.N+1); X(:,1) = x0;
    for q = 1:P.N, X(:,q+1) = f_step_v21(X(:,q), U(:,q), q, P, C); end
end

function v = pctile21(x, p)
    x = sort(x(:)); n = numel(x);
    idx = 1 + (n-1)*p/100;
    lo = floor(idx); hi = ceil(idx);
    v = x(lo) + (idx-lo)*(x(hi)-x(lo));
end

%% ========================================================================
%  LOCAL FUNCTION FOR STEP 10b: solver-closure rollout (mirrors solver f_step)
% =========================================================================
function X = rollout_solver_closure(U, x0, SP, SC)
% One deterministic roll under the SOLVER closure (n=8 state), used by the
% Step 10b mean validation. Mirrors solver V17 f_step exactly:
%   state x = [y; b; theta; d; fab_l1; fab_l2; fdi_l1; below_stock]
%   control u = [S; fab; flo; fgu; fdi]
    X = zeros(SP.n, SP.N+1); X(:,1) = x0;
    for q = 1:SP.N
        x = X(:,q); u = U(:,q);
        S=u(1); fab=u(2); flo=u(3); fgu=u(4); fdi=u(5);
        y=x(1); b=x(2); th=x(3); d=x(4); a1=x(5); a2=x(6); di1=x(7); st=x(8);

        st_used = (1-SP.decay_K)*st + SP.c_lo*flo + SP.c_gu*fgu;

        % theta innovation: solver convention, read at slot (q+2), with the
        % pre-vaccination guard eth = max(eth,0) for q < q_vax.
        eth = 0;
        if (q+2) <= numel(SC.eps_th)
            eth = SC.eps_th(q+2);
        end
        if q < SP.q_vax
            eth = max(eth, 0);
        end

        xp = zeros(SP.n,1);
        xp(3) = SP.rho_th_q(q)*(1 - SP.phi_S*S/100)*th*(1 - th/SP.th_max) + eth;
        xp(4) = SP.delta_q(q)*th;
        xp(1) = SC.mu_y + SP.rho_y*y + SP.alpha_S*S + SP.alpha_above*a2 ...
              + SP.alpha_below*st_used + SP.alpha_DI*di1 ...
              + SP.alpha_SDI*S*di1 - SP.beta_d*d + SC.eps_y(q+1);
        xp(2) = SC.mu_b + (1+SP.r)*b - SP.gamma_y*y + SP.k_ab*fab ...
              + SP.k_lo*SP.c_lo*flo + SP.k_gu*SP.c_gu*fgu + SP.k_di*di1 ...
              + SP.phi_t*SP.yr(q);
        xp(5) = fab; xp(6) = a1; xp(7) = fdi; xp(8) = st_used;

        X(:,q+1) = xp;
    end
end