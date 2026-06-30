%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V21
%  ------------------------------------------------------------------------
%  Purpose:
%    Validate the exact V21 transition system used by the iLQR planner.
%
%  Main changes relative to V20:
%    (1) Above-line CP no longer enters output as a lag-2 flow. It accumulates
%        into a persistent capacity-preservation stock K_cap.
%    (2) Below-line loans and guarantees enter output through a decaying
%        liquidity stock K_liq, using effective/take-up-adjusted values.
%    (3) Output includes a bounded complementarity term:
%          alpha_below * K_liq * (1 + chi_cap_liq*K_cap/(K_cap+cap_scale)).
%    (4) Debt closure matches the final solver: no exogenous health-spending
%        term in the optimized transition, and loans/guarantees are already
%        take-up-adjusted before entering the dynamics.
%    (5) The script reports both panel validation and the representative
%        OECD mean-economy validation used by the planner.
%
%  Required input files:
%    country_data_for_matlab.csv
%    weekly_mortality_matlab.csv
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V21 capacity-stock closure ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STEP 1: PARAMETERS - EXACT V21 SOLVER CLOSURE
% =========================================================================

% --- Output equation ---
P.rho_y       =  0.231;
P.alpha_S     = -0.095;
P.alpha_above =  0.544;     % calibration anchor, not direct V21 output channel
P.alpha_below =  0.261;
P.alpha_DI    =  1.470;
P.alpha_SDI   = -0.041;
P.beta_d      =  0.0;

% --- Take-up adjustments ---
% Fl and Fg used below are effective values. Do not multiply them again in
% forward_roll_v21().
P.c_lo = 0.60;
P.c_gu = 0.25;

% --- Debt equation ---
P.r      = 0.001;
P.gamma_y = 0.117;
P.k_ab    = 0.664;
P.k_lo    = 0.836;
P.k_gu    = 0.536;
P.k_di    = 0.526;
P.phi_t   = 0.0;

% --- Capacity-stock extension ---
target_half_life_cap_q = 6;
P.decay_cap   = 1 - 0.5^(1/target_half_life_cap_q);
P.alpha_cap   = 0.30 * P.alpha_above;
P.chi_cap_liq = 0.50;
P.cap_scale   = NaN;        % set after empirical policy caps are computed

% --- Below-line liquidity stock ---
P.decay_K = 0.10;

% --- Infection/mortality block ---
rho_theta_pre  = 1.50;
rho_theta_post = 0.75;
q_vax          = 8;         % break at Q3.2021 in qord below
P.q_vax        = q_vax;
P.rho_th_q     = [repmat(rho_theta_pre,1,q_vax-1), ...
                  repmat(rho_theta_post,1,13-q_vax+1)];
P.th_max       = Inf;
P.phi_S        = 0.80;

ifr_by_wave = [0.005 0.002 0.007 0.004 0.002 0.002 0.0002];
wave_idx_q_solver = [1 1 2 3 4 5 5 6 6 7 7 7 7];
P.delta_q = ifr_by_wave(wave_idx_q_solver) * 1e6;

% --- Horizon and dimensions ---
P.N  = 13;
P.n  = 9;                   % [y,b,theta,d,fab_l1,fab_l2,fdi_l1,K_liq,K_cap]
P.m  = 5;                   % [S,F_above,F_loans_eff,F_guar_eff,F_DI]
P.yr = 4:16;

% Fit windows used in reporting
K_y     = 11;               % Q4.2019-Q2.2022
K_b     = 13;               % Q4.2019-Q4.2022
K_theta = 11;               % Q4.2019-Q2.2022

qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl = {'Q4.19','Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22','Q3.22','Q4.22'};

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

%% ========================================================================
%  STEP 3: LOAD MACRO DATA
% =========================================================================
fprintf('--- Loading macro data ---\n');
T = readtable('country_data_for_matlab.csv');

countries = unique(T.Country, 'stable');

% Main sample: keep all 38 economies. Fill excl_iso only for sensitivity.
excl_iso = {};
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

    cdata(i).S          = zeros(1,P.N);
    cdata(i).FCP_above  = zeros(1,P.N);
    cdata(i).Flo_eff    = zeros(1,P.N);
    cdata(i).Fgu_eff    = zeros(1,P.N);
    cdata(i).FDI        = zeros(1,P.N);
    cdata(i).y          = zeros(1,P.N);
    cdata(i).b_delta    = zeros(1,P.N);
    cdata(i).theta_obs  = zeros(1,P.N);
    cdata(i).d_obs      = zeros(1,P.N);

    cdata(i).mu_y = 0;
    cdata(i).mu_b = 0;
    cdata(i).b0   = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end
    if isKey(b0_map, iso),    cdata(i).b0   = b0_map(iso);   end

    for k = 1:P.N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end

        cdata(i).S(k)         = row.S_mean_tw;
        cdata(i).FCP_above(k) = row.F_CP_above_3;

        % Effective below-line instruments. Prefer explicitly adjusted
        % columns if they exist; otherwise apply V21 take-up rates.
        if ismember('F_CP_loans_mid', T.Properties.VariableNames) && ~ismissing(row.F_CP_loans_mid)
            cdata(i).Flo_eff(k) = row.F_CP_loans_mid;
        else
            cdata(i).Flo_eff(k) = P.c_lo * row.F_CP_loans;
        end

        if ismember('F_CP_guar_lo', T.Properties.VariableNames) && ~ismissing(row.F_CP_guar_lo)
            cdata(i).Fgu_eff(k) = row.F_CP_guar_lo;
        else
            cdata(i).Fgu_eff(k) = P.c_gu * row.F_CP_guar;
        end

        cdata(i).FDI(k) = row.F_DI;
        cdata(i).y(k)   = row.y_t_pct;

        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b_delta(k) = row.debt_dR;
        end
    end

    cdata(i).obs_b_level = cdata(i).b0 + cumsum(cdata(i).b_delta);

    cdata(i).eps_y_vec = zeros(1, P.N+1);
    eps_q220 = -5.40;
    if isKey(eps_v14_map, iso), eps_q220 = eps_v14_map(iso); end
    cdata(i).eps_y_vec(4) = eps_q220;     % Q2.2020 shock, solver convention
    cdata(i).eps_theta_vec = zeros(1, P.N+1);
end

fprintf('  %d countries x %d quarters\n', n_c, P.N);
fprintf('  Below-line instruments are effective/take-up-adjusted before entering V21 dynamics.\n\n');

%% ========================================================================
%  STEP 3b: EMPIRICAL CAPS FOR V21 NORMALIZATION
% =========================================================================
Fa_all  = reshape([cdata.FCP_above], P.N, n_c)';
Flo_all = reshape([cdata.Flo_eff],   P.N, n_c)';
Fgu_all = reshape([cdata.Fgu_eff],   P.N, n_c)';
S_all   = reshape([cdata.S],         P.N, n_c)';
FDI_all = reshape([cdata.FDI],       P.N, n_c)';

ub_cap = [pctile_pos(S_all(:),99); pctile_pos(Fa_all(:),99); ...
          pctile_pos(Flo_all(:),99); pctile_pos(Fgu_all(:),99); ...
          pctile_pos(FDI_all(:),99)];
P.cap_scale = max(ub_cap(2), 1);

fprintf('  V21 caps / scales: S<=%.1f, F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f\n', ub_cap);
fprintf('  Capacity scale Kbar = %.3f; decay_cap = %.3f; alpha_cap = %.3f; chi = %.2f\n\n', ...
        P.cap_scale, P.decay_cap, P.alpha_cap, P.chi_cap_liq);

%% ========================================================================
%  STEP 4: LOAD MORTALITY DATA -> theta_obs, d_obs
% =========================================================================
fprintf('--- Loading mortality data ---\n');
M = readtable('weekly_mortality_matlab.csv');
M.date = datetime(M.date);
M.qstr = strings(height(M),1);
for r = 1:height(M)
    M.qstr(r) = sprintf('Q%d.%d', quarter(M.date(r)), year(M.date(r)));
end
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
    for k = 1:P.N
        key = sprintf('%s_%s', iso, qord{k});
        if isKey(theta_map,key)
            v = theta_map(key); if ~isnan(v), cdata(i).theta_obs(k) = v; end
        end
        if isKey(d_map,key)
            v = d_map(key); if ~isnan(v), cdata(i).d_obs(k) = v; end
        end
    end

    % Solver convention: eps_theta at slot k+1, read by f_step at q+2.
    for k = 2:P.N
        th_prev = cdata(i).theta_obs(k-1);
        Sk      = cdata(i).S(k);
        expct   = P.rho_th_q(k) * (1 - P.phi_S*Sk/100) * th_prev * (1 - th_prev/P.th_max);
        cdata(i).eps_theta_vec(k+1) = cdata(i).theta_obs(k) - expct;
    end
end

q220_th = arrayfun(@(c) c.theta_obs(3), cdata);
q220_d  = arrayfun(@(c) c.d_obs(3),     cdata);
fprintf('  Q2.20 theta: range [%.5f, %.5f], median %.5f\n', min(q220_th), max(q220_th), median(q220_th));
fprintf('  Q2.20 d:     range [%.2f, %.2f], median %.2f deaths/million/week\n', min(q220_d), max(q220_d), median(q220_d));
rho_max = max(P.rho_th_q);
fprintf('  Stationarity threshold at rho_max=%.2f: S/100 > %.3f\n\n', ...
        rho_max, (1 - 1/rho_max) / P.phi_S);

%% ========================================================================
%  STEP 5: PANEL FORWARD ROLL UNDER OBSERVED POLICY, V21 CLOSURE
% =========================================================================
fprintf('--- Running panel validation under observed policy, V21 closure ---\n');

for i = 1:n_c
    X = forward_roll_v21(cdata(i), P, true);   % true => use country fixed effects
    cdata(i).sim_y     = X(1,2:end);
    cdata(i).sim_b     = X(2,2:end);
    cdata(i).sim_theta = X(3,2:end);
    cdata(i).sim_d     = X(4,2:end);
    cdata(i).sim_Kliq  = X(8,2:end);
    cdata(i).sim_Kcap  = X(9,2:end);

    sim_db = [cdata(i).sim_b(1) - cdata(i).b0, diff(cdata(i).sim_b)];
    cdata(i).sim_db = sim_db;

    cdata(i).rmse_y       = sqrt(mean((cdata(i).sim_y(1:K_y)     - cdata(i).y(1:K_y)).^2));
    cdata(i).rmse_b_dlt   = sqrt(mean((sim_db(1:K_b)             - cdata(i).b_delta(1:K_b)).^2));
    cdata(i).rmse_b_level = sqrt(mean((cdata(i).sim_b(1:K_b)     - cdata(i).obs_b_level(1:K_b)).^2));
    cdata(i).rmse_theta   = sqrt(mean((cdata(i).sim_theta(1:K_theta) - cdata(i).theta_obs(1:K_theta)).^2));
    cdata(i).rmse_d       = sqrt(mean((cdata(i).sim_d(1:K_theta)     - cdata(i).d_obs(1:K_theta)).^2));
end

%% ========================================================================
%  STEP 6: TARGETED FIT METRICS
% =========================================================================
y_obs_all  = reshape([cdata.y],     P.N, n_c)';
y_sim_all  = reshape([cdata.sim_y], P.N, n_c)';

b_obs_all = zeros(n_c, P.N);
for i = 1:n_c, b_obs_all(i,:) = cdata(i).obs_b_level; end
b_sim_all = reshape([cdata.sim_b], P.N, n_c)';

db_obs_all  = reshape([cdata.b_delta], P.N, n_c)';
db_sim_all  = reshape([cdata.sim_db],  P.N, n_c)';
th_obs_all  = reshape([cdata.theta_obs], P.N, n_c)';
th_sim_all  = reshape([cdata.sim_theta], P.N, n_c)';
d_obs_all   = reshape([cdata.d_obs], P.N, n_c)';
d_sim_all   = reshape([cdata.sim_d], P.N, n_c)';
Kliq_all    = reshape([cdata.sim_Kliq], P.N, n_c)';
Kcap_all    = reshape([cdata.sim_Kcap], P.N, n_c)';

sd_y      = std(y_obs_all(:,1:K_y), 0, 'all');
sd_db     = std(db_obs_all(:,1:K_b), 0, 'all');
sd_blevel = std(b_obs_all(:,K_b), 0, 'all');
sd_theta  = std(th_obs_all(:,1:K_theta), 0, 'all');
sd_d      = std(d_obs_all(:,1:K_theta), 0, 'all');

rmse_y_md     = median([cdata.rmse_y]);
rmse_y_mean   = mean([cdata.rmse_y]);
rmse_db_md    = median([cdata.rmse_b_dlt]);
rmse_db_mean  = mean([cdata.rmse_b_dlt]);
rmse_bl_md    = median([cdata.rmse_b_level]);
rmse_bl_mean  = mean([cdata.rmse_b_level]);
rmse_th_md    = median([cdata.rmse_theta]);
rmse_th_mean  = mean([cdata.rmse_theta]);
rmse_d_md     = median([cdata.rmse_d]);
rmse_d_mean   = mean([cdata.rmse_d]);

fprintf('\n========================================\n');
fprintf('  STEP 6: Targeted fit metrics, V21 panel roll\n');
fprintf('========================================\n');
fprintf('  Output RMSE (k=1:%d):        median %.2f | mean %.2f | RMSE/SD %.2f\n', ...
        K_y, rmse_y_md, rmse_y_mean, rmse_y_md/sd_y);
fprintf('  Debt dRMSE change (1:%d):    median %.2f | mean %.2f | RMSE/SD %.2f  [PRIMARY]\n', ...
        K_b, rmse_db_md, rmse_db_mean, rmse_db_md/sd_db);
fprintf('  Debt RMSE level (1:%d):      median %.2f | mean %.2f | RMSE/final-SD %.2f  [REFERENCE]\n', ...
        K_b, rmse_bl_md, rmse_bl_mean, rmse_bl_md/sd_blevel);
fprintf('  Theta RMSE (k=1:%d):         median %.5f | mean %.5f | RMSE/SD %.2f\n', ...
        K_theta, rmse_th_md, rmse_th_mean, rmse_th_md/sd_theta);
fprintf('  Deaths RMSE (k=1:%d):        median %.2f | mean %.2f | RMSE/SD %.2f\n\n', ...
        K_theta, rmse_d_md, rmse_d_mean, rmse_d_md/sd_d);

Metric = ["Output RMSE"; "Debt dRMSE change"; "Debt RMSE level"; "Theta RMSE"; "Deaths RMSE"];
Window = ["1:K_y"; "1:K_b"; "1:K_b"; "1:K_theta"; "1:K_theta"];
Median = [rmse_y_md; rmse_db_md; rmse_bl_md; rmse_th_md; rmse_d_md];
Mean   = [rmse_y_mean; rmse_db_mean; rmse_bl_mean; rmse_th_mean; rmse_d_mean];
Denominator_SD = [sd_y; sd_db; sd_blevel; sd_theta; sd_d];
Ratio = Median ./ Denominator_SD;
T_targeted = table(Metric, Window, Median, Mean, Denominator_SD, Ratio);
disp(T_targeted);
writetable(T_targeted, 'calib_v21_targeted_metrics.csv');

%% ========================================================================
%  STEP 7: NON-TARGETED MOMENTS AND ENDPOINT RESIDUALS
% =========================================================================
sd_ratios = zeros(1,K_y);
for k = 1:K_y
    sd_ratios(k) = std(y_sim_all(:,k)) / max(std(y_obs_all(:,k)), 1e-10);
end

ac1_obs = zeros(n_c,1);
ac1_sim = zeros(n_c,1);
for i = 1:n_c
    ac1_obs(i) = safe_ac1(y_obs_all(i,1:K_y));
    ac1_sim(i) = safe_ac1(y_sim_all(i,1:K_y));
end

icc_y_obs = icc_country_share(y_obs_all(:,1:K_y));
icc_y_sim = icc_country_share(y_sim_all(:,1:K_y));
icc_b_obs = icc_country_share(b_obs_all(:,1:K_b));
icc_b_sim = icc_country_share(b_sim_all(:,1:K_b));

resid_b_T = b_obs_all(:,K_b) - b_sim_all(:,K_b);
resid_y_T = y_obs_all(:,K_y) - y_sim_all(:,K_y);
resid_d_T = d_obs_all(:,K_theta) - d_sim_all(:,K_theta);

fprintf('\n========================================\n');
fprintf('  STEP 7: Non-targeted moments and endpoint residuals\n');
fprintf('========================================\n');
fprintf('  Output SD ratio mean: %.3f\n', mean(sd_ratios));
fprintf('  Output AC(1): obs %.3f | sim %.3f | gap %.3f\n', ...
        mean(ac1_obs,'omitnan'), mean(ac1_sim,'omitnan'), ...
        abs(mean(ac1_obs,'omitnan') - mean(ac1_sim,'omitnan')));
fprintf('  ICC y: obs %.3f | sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('  ICC b: obs %.3f | sim %.3f\n', icc_b_obs, icc_b_sim);
fprintf('  Terminal debt residual Q4.22: mean %+.2f | median %+.2f | SD %.2f\n', ...
        mean(resid_b_T), median(resid_b_T), std(resid_b_T));
fprintf('  Output endpoint residual Q2.22: mean %+.2f | median %+.2f | SD %.2f\n', ...
        mean(resid_y_T), median(resid_y_T), std(resid_y_T));
fprintf('  Deaths endpoint residual Q2.22: mean %+.2f | median %+.2f | SD %.2f\n\n', ...
        mean(resid_d_T), median(resid_d_T), std(resid_d_T));

Moment = ["Output SD ratio mean"; "Output AC1 observed"; "Output AC1 simulated"; "Output AC1 gap"; ...
          "ICC output observed"; "ICC output simulated"; "ICC debt observed"; "ICC debt simulated"];
Value = [mean(sd_ratios); mean(ac1_obs,'omitnan'); mean(ac1_sim,'omitnan'); ...
         abs(mean(ac1_obs,'omitnan') - mean(ac1_sim,'omitnan')); ...
         icc_y_obs; icc_y_sim; icc_b_obs; icc_b_sim];
T_moments = table(Moment, Value);
disp(T_moments);
writetable(T_moments, 'calib_v21_nontargeted_moments.csv');

Country = string({cdata.iso})';
T_resid = table(Country, resid_y_T, resid_b_T, resid_d_T, ...
    'VariableNames', {'Country','Output_resid_Q2_2022','Debt_resid_Q4_2022','Deaths_resid_Q2_2022'});
writetable(T_resid, 'calib_v21_endpoint_residuals_by_country.csv');

%% ========================================================================
%  STEP 8: CHANNEL DECOMPOSITION, V21 LEAVE-ONE-OUT
% =========================================================================
above_contrib = zeros(n_c,1);
below_contrib = zeros(n_c,1);
di_contrib    = zeros(n_c,1);
total_fiscal  = zeros(n_c,1);

for i = 1:n_c
    c = cdata(i);
    base_y = sum(c.sim_y(1:K_y));

    c_noab = c;
    c_noab.FCP_above = zeros(1,P.N);

    c_nobe = c;
    c_nobe.Flo_eff = zeros(1,P.N);
    c_nobe.Fgu_eff = zeros(1,P.N);

    c_nodi = c;
    c_nodi.FDI = zeros(1,P.N);

    c_nofi = c;
    c_nofi.FCP_above = zeros(1,P.N);
    c_nofi.Flo_eff   = zeros(1,P.N);
    c_nofi.Fgu_eff   = zeros(1,P.N);
    c_nofi.FDI       = zeros(1,P.N);

    X_noab = forward_roll_v21(c_noab, P, true);
    X_nobe = forward_roll_v21(c_nobe, P, true);
    X_nodi = forward_roll_v21(c_nodi, P, true);
    X_nofi = forward_roll_v21(c_nofi, P, true);

    above_contrib(i) = base_y - sum(X_noab(1,2:K_y+1));
    below_contrib(i) = base_y - sum(X_nobe(1,2:K_y+1));
    di_contrib(i)    = base_y - sum(X_nodi(1,2:K_y+1));
    total_fiscal(i)  = base_y - sum(X_nofi(1,2:K_y+1));
end

fprintf('\n========================================\n');
fprintf('  STEP 8: Channel decomposition, leave-one-out cumulative output\n');
fprintf('========================================\n');
fprintf('  Above capacity-stock channel:  median %+6.2f ppQ  (>0: %d/%d)\n', ...
        median(above_contrib), sum(above_contrib>0), n_c);
fprintf('  Below liquidity-stock channel: median %+6.2f ppQ  (>0: %d/%d)\n', ...
        median(below_contrib), sum(below_contrib>0), n_c);
fprintf('  DI channel:                    median %+6.2f ppQ  (>0: %d/%d)\n', ...
        median(di_contrib), sum(di_contrib>0), n_c);
fprintf('  Total fiscal channels:          median %+6.2f ppQ  (>0: %d/%d)\n\n', ...
        median(total_fiscal), sum(total_fiscal>0), n_c);

T_channels = table(Country, above_contrib, below_contrib, di_contrib, total_fiscal, ...
    'VariableNames', {'Country','Above_capacity_contribution','Below_liquidity_contribution','DI_contribution','Total_fiscal_contribution'});
writetable(T_channels, 'calib_v21_channel_decomposition_by_country.csv');

Summary = ["Median"; "Mean"; "Share positive"];
Above_capacity = [median(above_contrib); mean(above_contrib); mean(above_contrib>0)];
Below_liquidity = [median(below_contrib); mean(below_contrib); mean(below_contrib>0)];
DI = [median(di_contrib); mean(di_contrib); mean(di_contrib>0)];
Total_fiscal = [median(total_fiscal); mean(total_fiscal); mean(total_fiscal>0)];
T_channels_summary = table(Summary, Above_capacity, Below_liquidity, DI, Total_fiscal);
disp(T_channels_summary);
writetable(T_channels_summary, 'calib_v21_channel_decomposition_summary.csv');

%% ========================================================================
%  STEP 9: REPRESENTATIVE OECD MEAN ECONOMY VALIDATION
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 9: Representative mean-economy validation, V21 solver object\n');
fprintf('========================================\n');

cbar.iso = 'OECD_mean';
cbar.S          = mean(S_all, 1);
cbar.FCP_above  = mean(Fa_all, 1);
cbar.Flo_eff    = mean(Flo_all, 1);
cbar.Fgu_eff    = mean(Fgu_all, 1);
cbar.FDI        = mean(FDI_all, 1);
cbar.y          = mean(y_obs_all, 1);
cbar.b_delta    = mean(db_obs_all, 1);
cbar.obs_b_level = mean(b_obs_all, 1);
cbar.theta_obs  = mean(th_obs_all, 1);
cbar.d_obs      = mean(d_obs_all, 1);
cbar.b0         = mean(arrayfun(@(c) c.b0, cdata));
cbar.mu_y       = 0;
cbar.mu_b       = 0;
cbar.eps_y_vec  = zeros(1,P.N+1);
cbar.eps_y_vec(4) = mean(arrayfun(@(c) c.eps_y_vec(4), cdata));
cbar.eps_theta_vec = mean(reshape([cdata.eps_theta_vec], P.N+1, n_c)', 1);

Xbar = forward_roll_v21(cbar, P, false);  % false => mu_y=mu_b=0, exact planner representative object
sim_y_bar     = Xbar(1,2:end);
sim_b_bar     = Xbar(2,2:end);
sim_theta_bar = Xbar(3,2:end);
sim_d_bar     = Xbar(4,2:end);
sim_db_bar    = [sim_b_bar(1) - cbar.b0, diff(sim_b_bar)];

rmse_y_bar  = sqrt(mean((sim_y_bar(1:K_y)      - cbar.y(1:K_y)).^2));
rmse_db_bar = sqrt(mean((sim_db_bar(1:K_b)     - cbar.b_delta(1:K_b)).^2));
rmse_bl_bar = sqrt(mean((sim_b_bar(1:K_b)      - cbar.obs_b_level(1:K_b)).^2));
rmse_th_bar = sqrt(mean((sim_theta_bar(1:K_theta) - cbar.theta_obs(1:K_theta)).^2));
rmse_d_bar  = sqrt(mean((sim_d_bar(1:K_theta)     - cbar.d_obs(1:K_theta)).^2));

fprintf('  Representative output RMSE:      %.2f pp\n', rmse_y_bar);
fprintf('  Representative debt dRMSE:      %.2f pp [PRIMARY]\n', rmse_db_bar);
fprintf('  Representative debt level RMSE: %.2f pp [REFERENCE]\n', rmse_bl_bar);
fprintf('  Representative theta RMSE:      %.5f\n', rmse_th_bar);
fprintf('  Representative deaths RMSE:     %.2f\n', rmse_d_bar);
fprintf('  Representative terminal debt residual: %+6.2f pp\n\n', cbar.obs_b_level(K_b)-sim_b_bar(K_b));

Metric = ["Output RMSE"; "Debt dRMSE change"; "Debt RMSE level"; "Theta RMSE"; "Deaths RMSE"; "Terminal debt residual"];
Value = [rmse_y_bar; rmse_db_bar; rmse_bl_bar; rmse_th_bar; rmse_d_bar; cbar.obs_b_level(K_b)-sim_b_bar(K_b)];
T_repr = table(Metric, Value);
disp(T_repr);
writetable(T_repr, 'calib_v21_representative_metrics.csv');

fprintf('  Representative trajectory:\n');
fprintf('  %8s %8s %8s %8s %8s %9s %9s %8s %8s %8s %8s\n', ...
    'Quarter','y_obs','y_sim','b_obs','b_sim','th_obs','th_sim','d_obs','d_sim','Kliq','Kcap');
for k = 1:P.N
    fprintf('  %8s %+8.2f %+8.2f %+8.2f %+8.2f %+9.5f %+9.5f %+8.2f %+8.2f %+8.2f %+8.2f\n', ...
        qlbl{k}, cbar.y(k), sim_y_bar(k), cbar.obs_b_level(k), sim_b_bar(k), ...
        cbar.theta_obs(k), sim_theta_bar(k), cbar.d_obs(k), sim_d_bar(k), ...
        Xbar(8,k+1), Xbar(9,k+1));
end

%% ========================================================================
%  STEP 10: VALIDATION FIGURES
% =========================================================================
figure('Name','Calibration V21 panel median/IQR','Color','w','Position',[50 50 1300 760]);

subplot(2,2,1); hold on;
fill_iqr(1:K_y, y_sim_all(:,1:K_y), [0 .4 .8], .15);
fill_iqr(1:K_y, y_obs_all(:,1:K_y), [.5 .5 .5], .12);
plot(1:K_y, median(y_sim_all(:,1:K_y)), 'b-o', 'LineWidth', 2);
plot(1:K_y, median(y_obs_all(:,1:K_y)), 'k--s', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:K_y, 'XTickLabel', qlbl(1:K_y), 'XTickLabelRotation', 45);
ylabel('pp potential GDP'); title('Output gap'); legend('','','Sim','Obs','Location','SE');

subplot(2,2,2); hold on;
fill_iqr(1:K_b, b_sim_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, b_obs_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(b_sim_all(:,1:K_b)), 'b-o', 'LineWidth', 2);
plot(1:K_b, median(b_obs_all(:,1:K_b)), 'k--s', 'LineWidth', 2);
grid on;
set(gca, 'XTick', 1:K_b, 'XTickLabel', qlbl(1:K_b), 'XTickLabelRotation', 45);
ylabel('% GDP'); title('Debt level'); legend('','','Sim','Obs','Location','SE');

subplot(2,2,3); hold on;
fill_iqr(1:K_theta, th_sim_all(:,1:K_theta), [.8 .2 .2], .15);
fill_iqr(1:K_theta, th_obs_all(:,1:K_theta), [.5 .5 .5], .12);
plot(1:K_theta, median(th_sim_all(:,1:K_theta)), 'r-o', 'LineWidth', 2);
plot(1:K_theta, median(th_obs_all(:,1:K_theta)), 'k--s', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:K_theta, 'XTickLabel', qlbl(1:K_theta), 'XTickLabelRotation', 45);
ylabel('infection share'); title('\theta'); legend('','','Sim','Obs','Location','NE');

subplot(2,2,4); hold on;
fill_iqr(1:K_theta, d_sim_all(:,1:K_theta), [.6 .1 .6], .15);
fill_iqr(1:K_theta, d_obs_all(:,1:K_theta), [.5 .5 .5], .12);
plot(1:K_theta, median(d_sim_all(:,1:K_theta)), 'm-o', 'LineWidth', 2);
plot(1:K_theta, median(d_obs_all(:,1:K_theta)), 'k--s', 'LineWidth', 2);
grid on;
set(gca, 'XTick', 1:K_theta, 'XTickLabel', qlbl(1:K_theta), 'XTickLabelRotation', 45);
ylabel('deaths / million / week'); title('Excess mortality'); legend('','','Sim','Obs','Location','NE');

sgtitle('Calibration V21: panel median/IQR, observed-policy forward roll','FontWeight','bold');
exportgraphics(gcf, 'calib_v21_panel_fit.pdf', 'ContentType','vector');
exportgraphics(gcf, 'calib_v21_panel_fit.png', 'Resolution',300);

figure('Name','Calibration V21 debt change','Color','w','Position',[80 80 760 460]); hold on;
fill_iqr(1:K_b, db_sim_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, db_obs_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(db_sim_all(:,1:K_b)), 'b-o','LineWidth',2);
plot(1:K_b, median(db_obs_all(:,1:K_b)), 'k--s','LineWidth',2);
yline(0,':'); grid on;
set(gca,'XTick',1:K_b,'XTickLabel',qlbl(1:K_b),'XTickLabelRotation',45);
ylabel('\Delta debt, pp of 2019 GDP');
title('Quarterly debt change: V21 sim vs observed');
legend('','','Sim \DeltaB','Obs \DeltaB','Location','NE');
exportgraphics(gcf, 'calib_v21_debt_change.pdf', 'ContentType','vector');
exportgraphics(gcf, 'calib_v21_debt_change.png', 'Resolution',300);

figure('Name','Calibration V21 representative mean economy','Color','w','Position',[60 60 1300 760]);
subplot(2,2,1); hold on;
plot(1:K_y, sim_y_bar(1:K_y), 'b-o','LineWidth',2);
plot(1:K_y, cbar.y(1:K_y), 'k--s','LineWidth',2);
yline(0,':'); grid on;
set(gca,'XTick',1:K_y,'XTickLabel',qlbl(1:K_y),'XTickLabelRotation',45);
ylabel('pp potential GDP'); title('Output gap'); legend('Sim repr.','Obs mean','Location','SE');
subplot(2,2,2); hold on;
plot(1:K_b, sim_b_bar(1:K_b), 'b-o','LineWidth',2);
plot(1:K_b, cbar.obs_b_level(1:K_b), 'k--s','LineWidth',2);
grid on; set(gca,'XTick',1:K_b,'XTickLabel',qlbl(1:K_b),'XTickLabelRotation',45);
ylabel('% GDP'); title('Debt level'); legend('Sim repr.','Obs mean','Location','SE');
subplot(2,2,3); hold on;
plot(1:K_theta, sim_theta_bar(1:K_theta), 'r-o','LineWidth',2);
plot(1:K_theta, cbar.theta_obs(1:K_theta), 'k--s','LineWidth',2);
yline(0,':'); grid on; set(gca,'XTick',1:K_theta,'XTickLabel',qlbl(1:K_theta),'XTickLabelRotation',45);
ylabel('infection share'); title('\theta'); legend('Sim repr.','Obs mean','Location','NE');
subplot(2,2,4); hold on;
plot(1:K_theta, sim_d_bar(1:K_theta), 'm-o','LineWidth',2);
plot(1:K_theta, cbar.d_obs(1:K_theta), 'k--s','LineWidth',2);
grid on; set(gca,'XTick',1:K_theta,'XTickLabel',qlbl(1:K_theta),'XTickLabelRotation',45);
ylabel('deaths / million / week'); title('Excess mortality'); legend('Sim repr.','Obs mean','Location','NE');
sgtitle('Calibration V21: representative OECD mean economy','FontWeight','bold');
exportgraphics(gcf, 'calib_v21_representative_fit.pdf', 'ContentType','vector');
exportgraphics(gcf, 'calib_v21_representative_fit.png', 'Resolution',300);

%% ========================================================================
%  STEP 11: CHECKLIST
% =========================================================================
checks = {
    'Output RMSE/SD < 0.7',                         rmse_y_md/sd_y < 0.7;
    'Debt dRMSE/SD < 0.9 (change, primary)',         rmse_db_md/sd_db < 0.9;
    'Theta RMSE/SD < 1.0',                           rmse_th_md/sd_theta < 1.0;
    'Deaths RMSE/SD < 1.0',                          rmse_d_md/sd_d < 1.0;
    '|Mean final debt residual| < 2pp',              abs(mean(resid_b_T)) < 2;
    '|Median final debt residual| < 3pp',            abs(median(resid_b_T)) < 3;
    '|Median endpoint output residual| < 1pp',       abs(median(resid_y_T)) < 1;
    'Output SD ratio in [0.7,1.3]',                  mean(sd_ratios)>0.7 && mean(sd_ratios)<1.3;
    'Output AC(1) gap < 0.1',                        abs(mean(ac1_obs,'omitnan') - mean(ac1_sim,'omitnan')) < 0.1;
    'Median total fiscal contribution > 0',          median(total_fiscal) > 0;
};

fprintf('\n========================================\n');
fprintf('  STEP 11: Calibration checklist\n');
fprintf('========================================\n');
for j = 1:size(checks,1)
    status = '[FAIL]';
    if checks{j,2}, status = '[ OK ]'; end
    fprintf('  %s  %s\n', status, checks{j,1});
end
fprintf('  PASSED: %d / %d\n\n', sum([checks{:,2}]), size(checks,1));

Check = string(checks(:,1));
Pass = logical([checks{:,2}]');
T_check = table(Check, Pass);
disp(T_check);
writetable(T_check, 'calib_v21_checklist.csv');

fprintf('Saved V21 calibration outputs:\n');
fprintf('  calib_v21_targeted_metrics.csv\n');
fprintf('  calib_v21_nontargeted_moments.csv\n');
fprintf('  calib_v21_endpoint_residuals_by_country.csv\n');
fprintf('  calib_v21_channel_decomposition_by_country.csv\n');
fprintf('  calib_v21_channel_decomposition_summary.csv\n');
fprintf('  calib_v21_representative_metrics.csv\n');
fprintf('  calib_v21_checklist.csv\n');
fprintf('  calib_v21_panel_fit.pdf/png\n');
fprintf('  calib_v21_debt_change.pdf/png\n');
fprintf('  calib_v21_representative_fit.pdf/png\n');

%% ========================================================================
%  LOCAL FUNCTIONS
% =========================================================================

function X = forward_roll_v21(c, P, use_country_fe)
    X = zeros(P.n, P.N+1);
    X(2,1) = c.b0;

    if use_country_fe
        mu_y = c.mu_y;
        mu_b = c.mu_b;
    else
        mu_y = 0;
        mu_b = 0;
    end

    for q = 1:P.N
        x = X(:,q);
        S   = idxget(c.S, q);
        fab = idxget(c.FCP_above, q);
        flo = idxget(c.Flo_eff, q);
        fgu = idxget(c.Fgu_eff, q);
        fdi = idxget(c.FDI, q);

        y      = x(1);
        b      = x(2);
        th     = x(3);
        d      = x(4); %#ok<NASGU>
        a1     = x(5);
        di1    = x(7);
        st_liq = x(8);
        st_cap = x(9);

        liq_used = (1-P.decay_K)   * st_liq + flo + fgu;
        cap_used = (1-P.decay_cap) * st_cap + fab;
        cap_multiplier = 1 + P.chi_cap_liq * cap_used / (cap_used + P.cap_scale);

        eth = 0;
        if (q+2) <= numel(c.eps_theta_vec)
            eth = c.eps_theta_vec(q+2);
        end
        if q < P.q_vax
            eth = max(eth, 0);
        end

        ey = idxget(c.eps_y_vec, q+1);

        xp = zeros(P.n,1);
        xp(3) = P.rho_th_q(q) * (1 - P.phi_S*S/100) * th * (1 - th/P.th_max) + eth;
        xp(4) = P.delta_q(q) * th;

        xp(1) = mu_y + P.rho_y*y + P.alpha_S*S ...
              + P.alpha_cap*cap_used ...
              + P.alpha_below*liq_used*cap_multiplier ...
              + P.alpha_DI*di1 ...
              + P.alpha_SDI*S*di1 ...
              - P.beta_d*x(4) + ey;

        xp(2) = mu_b + (1+P.r)*b - P.gamma_y*y ...
              + P.k_ab*fab + P.k_lo*flo + P.k_gu*fgu + P.k_di*di1 ...
              + P.phi_t*P.yr(q);

        xp(5) = fab;
        xp(6) = a1;
        xp(7) = fdi;
        xp(8) = liq_used;
        xp(9) = cap_used;

        X(:,q+1) = xp;
    end
end

function v = idxget(vec, k)
    if k < 1 || k > numel(vec) || isnan(vec(k))
        v = 0;
    else
        v = vec(k);
    end
end

function p = pctile_pos(v, prc)
    v = v(:);
    v = v(v > 0 & ~isnan(v) & ~isinf(v));
    if isempty(v)
        p = 1;
    else
        p = prctile(v, prc);
    end
end

function r = safe_ac1(x)
    x = x(:);
    if numel(x) < 3 || std(x(1:end-1)) < 1e-12 || std(x(2:end)) < 1e-12
        r = NaN;
        return;
    end
    C = corrcoef(x(1:end-1), x(2:end));
    r = C(1,2);
end

function icc = icc_country_share(M)
    % Share of total variance explained by country-level means.
    country_means = mean(M, 2, 'omitnan');
    total_var = var(M(:), 0, 'omitnan');
    if total_var < 1e-12
        icc = NaN;
    else
        icc = var(country_means, 0, 'omitnan') / total_var;
    end
end

function fill_iqr(x, data, col, alpha)
    sd = sort(data, 1);
    n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end
