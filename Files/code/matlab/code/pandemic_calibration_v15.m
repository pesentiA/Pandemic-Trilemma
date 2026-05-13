%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V15
%  Extensions vs V14:
%    (1) Health Block: theta (excess mortality), d (deaths) as states
%    (2) Health-to-Output channel: -beta_d * d_k in output equation
%    (3) Initial debt LEVEL b0 per country (Q4.2019), not delta-from-zero
%    (4) State vector explicit: x = [y, b, theta, d]
%    (5) Stock-flow adjustment trend phi_t * t (Bohn 1998, Weber 2012)
%
%  TRANSITION SYSTEM:
%    y_{k+1}   = mu_y + rho_y * y_k + alpha_S * S_k
%              + alpha_above * F_above_{k-2}
%              + alpha_below * K_below_k
%              + alpha_DI    * F_DI_{k-1}
%              + alpha_S_DI  * S_k * F_DI_{k-1}
%              - beta_d * d_k
%              + eps^y_k
%
%    b_{k+1}   = mu_b + (1+r) * b_k - gamma_y * y_k
%              + kappa_above * F_above_k
%              + kappa_loans * F_loans_adj_k
%              + kappa_guar  * F_guar_adj_k
%              + kappa_DI    * F_DI_{k-1}
%              + phi_t * t_k
%
%    theta_{k+1} = rho_theta * (1 - phi_S * S_k) * theta_k + eps^theta_k
%
%    d_{k+1}   = delta_theta * theta_k
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V15 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STEP 1: PARAMETERS
% =========================================================================

% --- Output equation (V14 OLS, take-up applied) ---
rho_y         =  0.231;
alpha_S       = -0.0952;
alpha_above   =  0.544;
alpha_below   =  0.261165;
alpha_DI_lag1 =  1.470;
alpha_S_DI    = -0.0406;
beta_d        =  0.15;       % EXTERNAL: deaths -> output (placeholder; Brainerd/Siegler-type magnitudes)

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
phi_t       = -0.076;       % SFA trend (Bohn 1998, Weber 2012)

% --- Health block (EXTERNAL CALIBRATION - to be replaced by your estimates) ---
rho_theta    =  0.80;        % Flaxman et al. (2020), Hsiang et al. (2020)
phi_S        =  0.50;        % Stringency damping (max-lockdown ~50%)
delta_theta  =  1.00;        % theta -> d identity

% --- Welfare weights placeholder (for normative step later) ---
% (Q diag: y, b, theta, d; R diag: S, F's) - filled at iLQR stage

%% ========================================================================
%  STEP 2: COUNTRY-LEVEL CONSTANTS
% =========================================================================

cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% Country FE: Output (V14, with F_H excluded)
cfe_y_val = [+1.1057, -1.0400, +0.3009, -0.0979, +1.2987, +1.3246, +1.9894, +0.1466, ...
             -3.5623, -1.7381, -0.0832, -4.8958, -1.8327, -1.4833, -1.8693, -3.3268, ...
             +0.2908, -2.0146, +8.3187, -4.8488, +2.3672, -0.5014, -1.9561, +0.6141, ...
             +0.7002, +2.3568, -0.6830, -3.2057, +1.0604, +1.2218, -1.0616, -0.5966, ...
             -2.7751, -0.2578, -1.8284, +1.0349, +4.0658, +1.0987];

% Country FE: Debt
cfe_b_val = [+0.2296, +0.3707, +0.3387, +0.7393, +0.5104, -0.0655, +1.4904, +0.9267, ...
             -0.0006, -0.0780, -0.5517, -0.2551, +0.3079, +0.2583, +0.0484, -0.0142, ...
             +0.1481, -0.0351, +1.3739, -0.1310, +0.7809, +0.1886, +0.0381, +0.3889, ...
             +0.3229, +1.0169, +0.8753, -0.5943, +0.4736, +0.7201, +1.2750, +0.0755, ...
             -0.9275, +0.6452, +0.0694, +0.3347, +0.9441, +1.3417];

% Initial Debt/GDP per Q4.2019 (% of GDP) - NEW
b0_val = [ 37.6,  69.7,  77.9,  44.2,  16.3,  29.6,  47.7,  53.2, ...
           30.2,  34.0,  34.1,  86.0,  11.8,  54.5,  85.4, 107.0, ...
          205.0,  52.1,  54.3,  69.9,  56.9, 122.0, 199.0,  35.1, ...
           29.3,  26.6,  39.7,   7.25, 43.9,  16.6,  73.0,  45.6, ...
          111.0,  48.4,  72.1,  27.7,  54.0,  97.3];

% V14 Q2.20 residuals (output shock, country-specific)
eps_v14_val = [-3.62, -8.55, -6.46, -7.79, -4.73, -10.10, -11.20, -3.75, ...
               -5.10, -5.98, -1.95, -9.57, -2.43, -1.97, -8.49, -12.50, ...
               -9.94, -8.99, -1.10, -4.70, -0.66, -10.90, -6.82, -1.83, ...
               -0.03, -5.31, -7.43, -7.97, -5.17, -2.45, -5.49, -1.80, ...
               -9.75, -4.87, -6.78, -4.70, -10.40, -4.79];

% Excess mortality Q2.20 shock (PLACEHOLDER - replace with data when health block estimated)
% Common shock for now (~30% excess mortality at peak); refine country-specific later
eps_theta_q220 = 0.30 * ones(1, length(cfe_iso));

cfe_y_map     = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map     = containers.Map(cfe_iso, cfe_b_val);
b0_map        = containers.Map(cfe_iso, b0_val);
eps_v14_map   = containers.Map(cfe_iso, eps_v14_val);
eps_theta_map = containers.Map(cfe_iso, eps_theta_q220);
%% ========================================================================
%  STEP 3: HORIZON & VALIDATION CONFIG
% =========================================================================

N = 13;          % Q4.19 to Q4.22
K_y = 10;        % output target window
K_b = 13;        % debt target window
K_theta = 10;    % mortality target window (Q4.19 - Q1.22 covers main waves)
nx = 4;          % [y, b, theta, d]
nu = 5;          % [S, F_above, F_loans, F_guar, F_DI]

t_idx_raw = 4:16;
year_idx_vec = t_idx_raw;

% Shock vectors initialized per country in cdata loop
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
%  STEP 4: LOAD DATA & BUILD cdata
% =========================================================================
fprintf('--- Loading data ---\n');
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

    % Controls (observed)
    cdata(i).S         = zeros(1,N);
    cdata(i).FCP_above = zeros(1,N);
    cdata(i).FCP_loans = zeros(1,N);
    cdata(i).FCP_guar  = zeros(1,N);
    cdata(i).FDI       = zeros(1,N);

    % Observed targets
    cdata(i).y         = zeros(1,N);
    cdata(i).b_delta   = zeros(1,N);
    cdata(i).theta_obs = NaN(1,N);   % populate when health data ready

    % Country FE & initial debt
    cdata(i).mu_y = 0; cdata(i).mu_b = 0; cdata(i).b0 = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end
    if isKey(b0_map, iso),    cdata(i).b0   = b0_map(iso);   end

    % Pull data
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

    % Take-up adjustments
    cdata(i).FCP_loans_adj   = takeup_loans * cdata(i).FCP_loans;
    cdata(i).FCP_guar_adj    = (takeup_guar / 0.35) * cdata(i).FCP_guar;
    cdata(i).FCP_below_flow  = cdata(i).FCP_loans_adj + cdata(i).FCP_guar_adj;
    cdata(i).FCP_below_stock = cumsum(cdata(i).FCP_below_flow);

    % Observed debt LEVEL = b0 + cumulative delta
    cdata(i).obs_b_level = cdata(i).b0 + cumsum(cdata(i).b_delta);

    % Output shock vector (Q2.20 fires)
    cdata(i).eps_y_vec = zeros(1, N+1);
    eps_q220 = -5.40;
    if isKey(eps_v14_map, iso), eps_q220 = eps_v14_map(iso); end
    cdata(i).eps_y_vec(4) = eps_q220;

    % Mortality shock vector (Q2.20)
    cdata(i).eps_theta_vec = zeros(1, N+1);
    eps_th_q220 = 0.30;
    if isKey(eps_theta_map, iso), eps_th_q220 = eps_theta_map(iso); end
    cdata(i).eps_theta_vec(4) = eps_th_q220;
end

fprintf('  %d countries x %d quarters\n', n_c, N);
fprintf('  Take-up: loans %.0f%%, guarantees %.0f%%\n', ...
        takeup_loans*100, takeup_guar*100);
fprintf('  Initial debt range: [%.1f, %.1f] (%% GDP)\n\n', ...
        min(b0_val), max(b0_val));

%% ========================================================================
%  STEP 5: FORWARD ROLL (block-recursive: health -> output -> debt)
% =========================================================================
% Defined at end of file; see function forward_roll_v15

for i = 1:n_c
    xs = forward_roll_v15(cdata(i), P);
    cdata(i).sim_y     = xs(1, 2:end);
    cdata(i).sim_b     = xs(2, 2:end);
    cdata(i).sim_theta = xs(3, 2:end);
    cdata(i).sim_d     = xs(4, 2:end);

    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y(1:K_y)   - cdata(i).y(1:K_y)).^2));
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b(1:K_b)   - cdata(i).obs_b_level(1:K_b)).^2));
    % --- Health Block (S normalized to [0,1]) ---
end

%% ========================================================================
%  STEP 6: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 6: Validation\n');
fprintf('========================================\n');

fprintf('  Output RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n', ...
    K_y, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n\n', ...
    K_b, median([cdata.rmse_b]), mean([cdata.rmse_b]));

fprintf('  OECD Median Trajectory:\n');
fprintf('  %8s %9s %9s %9s %9s\n', 'Quarter', 'y_obs', 'y_sim', 'b_obs', 'b_sim');
for k = 1:N
    yo = median(arrayfun(@(c) c.y(k),            cdata));
    ys = median(arrayfun(@(c) c.sim_y(k),        cdata));
    bo = median(arrayfun(@(c) c.obs_b_level(k),  cdata));
    bs = median(arrayfun(@(c) c.sim_b(k),        cdata));
    marker = ' '; if k > K_y, marker = '*'; end
    fprintf('  %8s %+9.2f %+9.2f %+9.2f %+9.2f %s\n', qlbl{k}, yo, ys, bo, bs, marker);
end

% Health-block simulated trajectory (no data validation yet)
fprintf('\n  Simulated Health Trajectory (median):\n');
fprintf('  %8s %9s %9s\n', 'Quarter', 'theta', 'd');
for k = 1:N
    ths = median(arrayfun(@(c) c.sim_theta(k), cdata));
    ds  = median(arrayfun(@(c) c.sim_d(k),     cdata));
    fprintf('  %8s %+9.3f %+9.3f\n', qlbl{k}, ths, ds);
end

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

above_contrib = zeros(n_c, 1);
below_contrib = zeros(n_c, 1);
di_contrib    = zeros(n_c, 1);
health_contrib= zeros(n_c, 1);   % NEW: deaths -> output drag
total_fiscal  = zeros(n_c, 1);

for i = 1:n_c
    c = cdata(i);
    base_y = sum(c.sim_y(1:K_y));

    % Zero each channel in turn
    c_noab = c; c_noab.FCP_above = zeros(1,N);
    c_nobe = c; c_nobe.FCP_loans_adj = zeros(1,N); c_nobe.FCP_guar_adj = zeros(1,N);
                c_nobe.FCP_below_stock = zeros(1,N);
    c_nodi = c; c_nodi.FDI = zeros(1,N);
    c_nofi = c_noab; c_nofi.FCP_loans_adj = zeros(1,N); c_nofi.FCP_guar_adj = zeros(1,N);
                     c_nofi.FCP_below_stock = zeros(1,N); c_nofi.FDI = zeros(1,N);
    c_nohe = c; c_nohe.eps_theta_vec = zeros(1,N+1);  % no pandemic mortality

    xs_noab = forward_roll_v15(c_noab, P);
    xs_nobe = forward_roll_v15(c_nobe, P);
    xs_nodi = forward_roll_v15(c_nodi, P);
    xs_nofi = forward_roll_v15(c_nofi, P);
    xs_nohe = forward_roll_v15(c_nohe, P);

    above_contrib(i)  = base_y - sum(xs_noab(1, 2:K_y+1));
    below_contrib(i)  = base_y - sum(xs_nobe(1, 2:K_y+1));
    di_contrib(i)     = base_y - sum(xs_nodi(1, 2:K_y+1));
    total_fiscal(i)   = base_y - sum(xs_nofi(1, 2:K_y+1));
    health_contrib(i) = base_y - sum(xs_nohe(1, 2:K_y+1));  % usually < 0
end

fprintf('\n  Channel decomposition (cum %dQ, median):\n', K_y);
fprintf('    Above-Flow:   %+6.2f pp  (>0: %d/%d)\n', median(above_contrib), sum(above_contrib>0), n_c);
fprintf('    Below-Stock:  %+6.2f pp  (>0: %d/%d)\n', median(below_contrib), sum(below_contrib>0), n_c);
fprintf('    DI:           %+6.2f pp  (>0: %d/%d)\n', median(di_contrib),    sum(di_contrib>0),    n_c);
fprintf('    Total fiscal: %+6.2f pp  (>0: %d/%d)\n', median(total_fiscal),  sum(total_fiscal>0),  n_c);
fprintf('    Health drag:  %+6.2f pp  (<0: %d/%d)\n', median(health_contrib),sum(health_contrib<0),n_c);

% Debt residuals
fprintf('\n  Debt Residuals (final period):\n');
resid_b = zeros(n_c, 1);
for i = 1:n_c
    resid_b(i) = cdata(i).obs_b_level(K_b) - cdata(i).sim_b(K_b);
end
[~, sort_idx] = sort(resid_b);
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

%% ========================================================================
%  STEP 9: VISUALIZATION
% =========================================================================
sim_y_all = reshape([cdata.sim_y],     N, n_c)';
obs_y_all = reshape([cdata.y],         N, n_c)';
sim_b_all = reshape([cdata.sim_b],     N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_level; end
sim_theta_all = reshape([cdata.sim_theta], N, n_c)';
sim_d_all     = reshape([cdata.sim_d],     N, n_c)';

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
fill_iqr(1:N, sim_theta_all, [.8 .2 .2], .15);
plot(1:N, median(sim_theta_all), 'r-o', 'LineWidth', 2);
yline(0, ':'); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('excess mortality (share)'); title('\theta (simulated)');

subplot(2,2,4); hold on;
fill_iqr(1:N, sim_d_all, [.6 .1 .6], .15);
plot(1:N, median(sim_d_all), 'm-o', 'LineWidth', 2);
grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('deaths (share)'); title('d (simulated)');

sgtitle('Calibration V15 - Trilemma (y, b, \theta, d)','FontWeight','bold');

%% ========================================================================
%  STEP 10: CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V15                                     #\n');
fprintf('################################################################\n\n');

y_obs_sd  = std(reshape([cdata.y], 1, []));
b_obs_sd  = std(arrayfun(@(c) c.obs_b_level(K_b), cdata));
rmse_y_md = median([cdata.rmse_y]);
rmse_b_md = median([cdata.rmse_b]);

fprintf('1. TARGETED MOMENTS\n');
fprintf('   Output RMSE: %.2f pp (ratio %.2f)\n', rmse_y_md, rmse_y_md/y_obs_sd);
fprintf('   Debt   RMSE: %.2f pp (ratio %.2f)\n', rmse_b_md, rmse_b_md/b_obs_sd);
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
    '|Mean debt resid| < 2pp',    abs(mean(resid_b)) < 2;
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
    % State: x = [y, b, theta, d]
    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    xs(2,1) = c.b0;       % initial debt LEVEL
    % xs(1,1) = 0;        % output gap SS
    % xs(3,1) = 0;        % theta SS
    % xs(4,1) = 0;        % d SS

    for k = 1:N_
        y     = xs(1,k);
        b     = xs(2,k);
        theta = xs(3,k);
        d     = xs(4,k);

        Sk     = idxget(c.S, k);
        fab_k  = idxget(c.FCP_above, k);
        floa_k = idxget(c.FCP_loans_adj, k);
        fgua_k = idxget(c.FCP_guar_adj, k);
        fab_l2 = (k>=3) * idxget(c.FCP_above, max(k-2,1)) * (k>=3);
        fdi_l1 = (k>=2) * idxget(c.FDI, max(k-1,1)) * (k>=2);
        kbe_k  = idxget(c.FCP_below_stock, k);

        ey   = idxget(c.eps_y_vec, k+1);
        eth  = idxget(c.eps_theta_vec, k+1);
        yr_k = idxget(P.year_idx_vec, k);

        % --- Health Block (S normalized to [0,1]) ---
Sk_norm = Sk / 100;
xs(3,k+1) = P.rho_theta * (1 - P.phi_S * Sk_norm) * theta + eth;
xs(4,k+1) = P.delta_theta * theta;

        % --- Output ---
        xs(1,k+1) = c.mu_y + P.rho_y * y + P.alpha_S * Sk ...
                  + P.alpha_above   * fab_l2 ...
                  + P.alpha_below   * kbe_k ...
                  + P.alpha_DI_lag1 * fdi_l1 ...
                  + P.alpha_S_DI    * Sk * fdi_l1 ...
                  - P.beta_d        * d ...
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