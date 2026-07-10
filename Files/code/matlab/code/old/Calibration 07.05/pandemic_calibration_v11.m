%%
%Neu, ohne Country FE, neue Spezifikation siehe R


%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V11 (Country-FE only spec)
%
%  OUTPUT EQUATION (V11 frozen):
%    y_t = mu_country + rho_y * y_{t-1} + alpha_S * S_t
%        + alpha_CP_cum * F_CP_cum_t + alpha_CP_lag2 * F_CP_{t-2}
%        + alpha_DI_lag1 * F_DI_{t-1} + alpha_S_DI * S_t * F_DI_{t-1}
%        + eps_t
%
%  CHANGES from V5b:
%    - No Quarter-FE / no QFE vector (Country-FE only spec)
%    - No recession spline (eta_p removed)
%    - No mortality channel (beta_fear/p_proj insignificant)
%    - F_CP via cumulative stock (Strukturerhaltung) + Lag-2 direct effect
%    - F_DI via Lag-1 with S-interaction (push-on-string)
%    - Optional: single initial Q2.20 pandemic shock
%
%  All other parameters and structure: identical to V5b.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V11 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  PARAMETERS - V11 FROZEN VALUES
% =========================================================================

% --- Output equation (V11, N=418, Country-FE) ---
rho_y          =  0.217;   % y_lag1
alpha_S        = -0.098;   % S_mean_tw direkt
alpha_CP_cum   =  0.103;   % F_CP_cum (Strukturerhaltung Stock)
alpha_CP_lag2  =  0.108;   % F_CP_lag2 (Direkt-Effekt verzoegert)
alpha_DI_lag1  =  1.417;   % F_DI_lag1 main
alpha_S_DI     = -0.0396;  % S_mean_tw * F_DI_lag1 (push-on-string)
delta_cp       =  1.0;     % Stock-Abschreibung pro Quartal (1.0 = keine Erosion)

% --- Debt equation (V4 main, N=494, unchanged) ---
r_int        =  0.001;
gamma_y      =  0.188;
kappa_above  =  0.434;
kappa_loans  =  0.365;
kappa_guar   =  0.107;
kappa_F_DI   =  0.427;
kappa_H      =  1.000;
phi_t        = -0.216;

% --- Pooled CP / sensitivity (unchanged) ---
kappa_CP_pooled = 0.173;
kappa_CP_lo     = 0.134;
kappa_CP_hi     = 0.241;
kappa_above_q75 = 1.009;

% --- Country ISO list ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Country FE: Output (pp), V11 estimation (N=418, fixef) ---
cfe_y_val = [+0.6427, -1.3381, -0.2827, -0.4194, +0.9259, +0.9304, +1.9562, +0.0634, ...
             -4.3329, -2.7234, -0.5551, -5.5692, -2.3823, -1.7928, -2.6562, -4.3291, ...
             -0.0135, -2.2699, +8.3140, -5.1074, +2.2691, -1.4562, -2.5599, +0.4606, ...
             +0.5079, +2.1832, -1.1012, -3.2366, +0.7695, +1.0932, -1.5983, -0.9368, ...
             -3.2343, -0.3393, -2.4286, +0.8199, +3.7357, +0.8253];

% --- Country FE: Debt (unchanged from V5b) ---
cfe_b_val = [-0.6462, -0.5190, -0.5147, -0.1395, -0.3089, -0.9431, +0.6842, +0.0991, ...
             -0.9315, -0.9908, -1.4190, -1.1902, -0.5864, -0.5923, -0.8557, -0.9631, ...
             -0.7381, -0.9150, +0.6430, -1.0453, -0.0485, -0.7098, -0.8563, -0.4437, ...
             -0.5257, +0.2124, +0.0169, -1.4596, -0.3687, -0.0728, +0.3738, -0.8068, ...
             -1.8202, -0.2072, -0.8612, -0.4815, +0.1780, +0.4707];
b_grand_mean = 0;
cfe_b_val = cfe_b_val + b_grand_mean;

% --- Year index, centered (for debt equation only) ---
year_idx_raw = [0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3];
year_idx_vec = year_idx_raw - mean(year_idx_raw);

% --- Dimensions ---
N = 13;  K_act = 13;  nx = 2;  nu = 2;
K_y = 10;   % Output-Validation Q4.19-Q1.22 (COVID-Periode, vor 2022-Schocks)
K_b = 13;   % Debt-Validation Q4.19-Q4.22 (volle Periode)
u_lo = [0; 0];  u_hi = [20.0; 10.0];

% --- Initial Q2.20 shock (residual nicht durch S erfassbar) ---
%  V11 hat keine Quarter-FE - residueller globaler Schock fliesst in eps.
%  Setze eps_init nur fuer Q2.20 (k=3 in 13-Quartal-Sample).
eps_y_vec = zeros(1, N+1);
eps_y_vec(4) = -5.54;   % Q2.20 Median-Residuum aus V11-Schätzung

% --- Objective weights (fuer iLQR) ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Maps ---
cfe_y_map = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map = containers.Map(cfe_iso, cfe_b_val);

% --- Pack parameters ---
P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, ...
    'alpha_CP_cum',alpha_CP_cum, 'alpha_CP_lag2',alpha_CP_lag2, ...
    'alpha_DI_lag1',alpha_DI_lag1, 'alpha_S_DI',alpha_S_DI, ...
    'delta_cp',delta_cp, ...
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
    'N',N, 'K_act',K_act, 'K_y',K_y, 'K_b',K_b, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   nx = %d (y, b)\n', N, K_act, nx);
fprintf('  Spec: V11 Country-FE only (kein QFE, kein Recession-Spline)\n\n');


%% ========================================================================
%  LOAD DATA
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

    % Pre-compute F_CP cumulative stock
    cdata(i).FCP_cum = cumsum(cdata(i).FCP);
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
fprintf('  STEP 1: Validation V11 (N = %d, nx = %d)\n', N, nx);
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
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y(1:K_y) - cdata(i).y(1:K_y)).^2));
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b(1:K_b) - cdata(i).obs_b_cum(1:K_b)).^2));
end

fprintf('  Output RMSE (k=1:%d, COVID) -- Median: %.2f pp   Mean: %.2f pp\n', ...
    K_y, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d, full)  -- Median: %.2f pp   Mean: %.2f pp\n\n', ...
    K_b, median([cdata.rmse_b]), mean([cdata.rmse_b]));

% OECD Median
fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s\n', 'Quarter', 'Observed', 'Model');
for k = 1:N
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    marker = ' '; if k > K_y, marker = '*'; end
    fprintf('  %8s %+9.2f %+9.2f %s\n', qlbl{k}, median(obs_k), median(sim_k), marker);
end
fprintf('  (* = ausserhalb Output-Validation-Window)\n');

% Fiscal contribution (ueber K_y, COVID-Periode)
fprintf('\n  Fiscal contribution (Full - NoFiscal, cum %dQ COVID):\n', K_y);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), zeros(1,N), zeros(1,N), zeros(1,N), ...
        zeros(1,N), cdata(i).FH, cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, P);
    diffs(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nof(1, 2:K_y+1));
end
fprintf('    Mean:   %+.2f pp\n', mean(diffs));
fprintf('    Median: %+.2f pp\n', median(diffs));
fprintf('    F > 0:  %d / %d\n', sum(diffs > 0), n_c);

% Channel decomposition: CP vs DI
fprintf('\n  Channel decomposition (cum %dQ COVID):\n', K_y);
cp_contrib = zeros(n_c, 1);
di_contrib = zeros(n_c, 1);
for i = 1:n_c
    % No-CP (alle CP-Komponenten = 0, DI bleibt)
    xs_nocp = forward_roll(zeros(1,N), zeros(1,N), zeros(1,N), zeros(1,N), ...
        cdata(i).FDI, cdata(i).FH, cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, P);
    % No-DI (CP bleibt, DI = 0)
    xs_nodi = forward_roll(cdata(i).FCP, cdata(i).FCP_above, cdata(i).FCP_loans, ...
        cdata(i).FCP_guar, zeros(1,N), cdata(i).FH, cdata(i).S, cdata(i).theta, ...
        cdata(i).d, cdata(i).mu_y, cdata(i).mu_b, P);
    cp_contrib(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nocp(1, 2:K_y+1));
    di_contrib(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nodi(1, 2:K_y+1));
end
fprintf('    CP-Beitrag:  median %+.2f pp,  mean %+.2f pp,  (CP > 0: %d/%d)\n', ...
    median(cp_contrib), mean(cp_contrib), sum(cp_contrib > 0), n_c);
fprintf('    DI-Beitrag:  median %+.2f pp,  mean %+.2f pp,  (DI > 0: %d/%d)\n', ...
    median(di_contrib), mean(di_contrib), sum(di_contrib > 0), n_c);
fprintf('    Sum CP+DI:   median %+.2f pp  (vs Total F: %+.2f pp - Differenz = Interaktion)\n', ...
    median(cp_contrib + di_contrib), median(diffs));


%% ========================================================================
%  STEP 1b: NON-TARGETED MOMENTS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1b: Non-Targeted Moments\n');
fprintf('========================================\n');

fprintf('\n  (1) Cross-Country SD of Output Gap:\n');
fprintf('  %10s %8s %8s %8s\n', 'Quarter', 'SD_obs', 'SD_sim', 'Ratio');
sd_ratios = zeros(1, K_y);
for k = 1:K_y
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    sd_obs = std(obs_k);  sd_sim = std(sim_k);
    sd_ratios(k) = sd_sim / max(sd_obs, 1e-10);
    fprintf('  %10s %8.2f %8.2f %8.3f\n', qlbl{k}, sd_obs, sd_sim, sd_ratios(k));
end
fprintf('  %10s %8s %8s %8.3f\n', 'Mean', '', '', mean(sd_ratios));

ac1_obs = zeros(n_c,1);  ac1_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_y);  ys = cdata(i).sim_y(1:K_y);
    co1 = corrcoef(yo(1:end-1), yo(2:end));  ac1_obs(i) = co1(1,2);
    cs1 = corrcoef(ys(1:end-1), ys(2:end));  ac1_sim(i) = cs1(1,2);
end
fprintf('\n  (2) Within-Country AC(1):\n');
fprintf('  %20s %10.3f %10.3f\n', 'AC(1) mean', mean(ac1_obs), mean(ac1_sim));

y_obs_p = reshape([cdata.y], N, n_c)';
y_sim_p = reshape([cdata.sim_y], N, n_c)';
icc_y_obs = var(mean(y_obs_p(:,1:K_y),2)) / var(y_obs_p(:,1:K_y),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_y),2)) / var(y_sim_p(:,1:K_y),0,'all');
b_obs_p = zeros(n_c, K_b);
b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_b); end
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_b),2)) / var(b_sim_p(:,1:K_b),0,'all');
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
    sim_b_cum(i) = cdata(i).sim_b(K_b);
    obs_b_cum(i) = cdata(i).obs_b_cum(K_b);
    resid_b(i)   = obs_b_cum(i) - sim_b_cum(i);
    above_tot = sum(cdata(i).FCP_above(1:K_b));
    loans_tot = sum(cdata(i).FCP_loans(1:K_b));
    guar_tot  = sum(cdata(i).FCP_guar(1:K_b));
    cp_total(i) = above_tot + loans_tot + guar_tot;
    if cp_total(i) > 0.1
        frac_above(i) = above_tot / cp_total(i);
        frac_loans(i) = loans_tot / cp_total(i);
        frac_guar(i)  = guar_tot  / cp_total(i);
    end
end

[~, sort_idx] = sort(resid_b);
fprintf('\n  (1) Largest negative residuals (model OVER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s\n', 'ISO', 'Obs', 'Sim', 'Resid');
for j = 1:5
    i = sort_idx(j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f\n', iso_list{i}, ...
        obs_b_cum(i), sim_b_cum(i), resid_b(i));
end
fprintf('\n  Largest positive residuals (model UNDER-estimates debt):\n');
fprintf('  %5s %10s %10s %10s\n', 'ISO', 'Obs', 'Sim', 'Resid');
for j = 0:4
    i = sort_idx(n_c - j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f\n', iso_list{i}, ...
        obs_b_cum(i), sim_b_cum(i), resid_b(i));
end

corr_above = corrcoef(resid_b, frac_above);
corr_loans = corrcoef(resid_b, frac_loans);
corr_guar  = corrcoef(resid_b, frac_guar);
corr_cptot = corrcoef(resid_b, cp_total);
fprintf('\n  (2) Residual correlations:\n');
fprintf('    Residual vs %% above:    r = %+.3f\n', corr_above(1,2));
fprintf('    Residual vs %% loans:    r = %+.3f\n', corr_loans(1,2));
fprintf('    Residual vs %% guar:     r = %+.3f\n', corr_guar(1,2));
fprintf('    Residual vs total CP:   r = %+.3f\n', corr_cptot(1,2));

fprintf('\n  (3) Residual summary:\n');
fprintf('    Mean:   %+.2f pp\n', mean(resid_b));
fprintf('    Median: %+.2f pp\n', median(resid_b));
fprintf('    SD:     %+.2f pp\n', std(resid_b));
fprintf('    Range:  [%+.2f, %+.2f] pp\n', min(resid_b), max(resid_b));

% (4) Mortality channel as non-targeted moment
%  V11 hat keinen beta_fear-Term mehr (Mortality-Channel raus aus Output-Eq).
%  Wir testen aber weiterhin: korreliert exzessive Mortalitaet mit Debt-Resid?
d_total = zeros(n_c,1);
for i = 1:n_c
    d_total(i) = sum(cdata(i).d(1:K_b));
end
corr_d_resid_b = corrcoef(d_total, resid_b);
fprintf('\n  (4) Mortality channel (non-targeted):\n');
fprintf('    Cum excess mortality (median): %.2f pp\n', median(d_total));
fprintf('    Corr(cum d, debt resid):       r = %+.3f\n', corr_d_resid_b(1,2));


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
    FCPg_med(k) = median(arrayfun(@(c) c.FCP_guar(k),  cdata));
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

fprintf('\n  Cumulative median deployment (sum, pp of 2019 GDP):\n');
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

sgtitle('OECD Median Policy Levers - Q4.2019-Q4.2022','FontWeight','bold');


%% ========================================================================
%  POLICY LEVER VALIDATION: Observed vs. Used in Forward Roll
% =========================================================================
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

subplot(2,3,1); hold on;
fill([1:N, fliplr(1:N)], [Sq(1,:), fliplr(Sq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, S_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, S_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('Stringency Index (0-100)');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Containment (S)'); legend('IQR obs','Observed','Used in roll','Location','SE');

subplot(2,3,2); hold on;
fill([1:N, fliplr(1:N)], [FCPq(1,:), fliplr(FCPq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCP_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCP_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Capacity Preservation (F^{CP})'); legend('IQR obs','Observed','Used in roll','Location','NE');

subplot(2,3,3); hold on;
fill([1:N, fliplr(1:N)], [FDIq(1,:), fliplr(FDIq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FDI_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FDI_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('Demand Injection (F^{DI})'); legend('IQR obs','Observed','Used in roll','Location','NE');

subplot(2,3,4); hold on;
fill([1:N, fliplr(1:N)], [FCPaq(1,:), fliplr(FCPaq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCPa_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCPa_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('CP above-the-line'); legend('IQR obs','Observed','Used in roll','Location','NE');

subplot(2,3,5); hold on;
fill([1:N, fliplr(1:N)], [FCPlq(1,:), fliplr(FCPlq(2,:))], ...
     [.5 .5 .5], 'FaceAlpha', .15, 'EdgeColor','none');
plot(1:N, FCPl_med, 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:N, FCPl_med, 'b-o',  'LineWidth', 2, 'MarkerSize', 4);
grid on; ylabel('pp of 2019 GDP');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45,'FontSize',8);
title('CP loans'); legend('IQR obs','Observed','Used in roll','Location','NE');

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


%% ========================================================================
%  STEP 1d: MORTALITY EQUATION VALIDATION (Stage 1 plausibility check)
%
%  Forward-roll der Stage-1-Transitionsgleichungen fuer theta (pandemische
%  Praevalenz) und d (excess mortality). Stage-1-Kalibrierung aus Literatur:
%    - rho_theta:    Liu & Roecklov (2021)
%    - phi_S:        Haug et al. (2020 NHB), NPI-Wirkung
%    - delta_theta:  IHME COVID-19 Forecasting Team (2022, Lancet), IFR
%
%  Wave-Anchoring: theta wird zu Beginn jeder Welle auf beobachteten Wert
%  zurueckgesetzt, weil Modell keine endogene Variant-Onset-Mechanik hat.
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1d: Mortality Equation Validation\n');
fprintf('========================================\n');

rho_theta   = 0.85;
phi_S       = 0.012;
delta_theta = 0.025;

fprintf('  Calibration: rho_theta=%.3f, phi_S=%.4f, delta_theta=%.4f\n', ...
    rho_theta, phi_S, delta_theta);

for i = 1:n_c
    theta_sim = zeros(1, N);
    d_sim     = zeros(1, N);
    is_wave_start = false(1, N);
    is_wave_start(1) = true;
    for k = 2:N
        if cdata(i).theta(k) > max(cdata(i).theta(1:k-1)) * 1.2 ...
                && cdata(i).theta(k) > 0.05
            is_wave_start(k) = true;
        end
    end
    theta_sim(1) = cdata(i).theta(1);
    d_sim(1)     = cdata(i).d(1);
    for k = 1:N-1
        if is_wave_start(k+1)
            theta_sim(k+1) = cdata(i).theta(k+1);
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

fprintf('\n  OECD Median Trajectory:\n');
fprintf('  %8s %10s %10s %10s %10s\n', 'Quarter', 'theta_obs', 'theta_sim', 'd_obs', 'd_sim');
for k = 1:N
    th_o = median(arrayfun(@(c) c.theta(k),     cdata));
    th_s = median(arrayfun(@(c) c.theta_sim(k), cdata));
    d_o  = median(arrayfun(@(c) c.d(k),         cdata));
    d_s  = median(arrayfun(@(c) c.d_sim(k),     cdata));
    fprintf('  %8s %10.3f %10.3f %10.3f %10.3f\n', qlbl{k}, th_o, th_s, d_o, d_s);
end

theta_obs_cum = arrayfun(@(c) sum(c.theta(1:N)),     cdata)';
theta_sim_cum = arrayfun(@(c) sum(c.theta_sim(1:N)), cdata)';
d_obs_cum     = arrayfun(@(c) sum(c.d(1:N)),         cdata)';
d_sim_cum     = arrayfun(@(c) sum(c.d_sim(1:N)),     cdata)';

corr_theta = corrcoef(theta_obs_cum, theta_sim_cum);
corr_d     = corrcoef(d_obs_cum,     d_sim_cum);

manual_rank = @(x) arrayfun(@(v) sum(x < v) + 0.5*sum(x == v) + 0.5, x);
tmp = corrcoef(manual_rank(theta_obs_cum), manual_rank(theta_sim_cum));  spear_theta = tmp(1,2);
tmp = corrcoef(manual_rank(d_obs_cum),     manual_rank(d_sim_cum));      spear_d     = tmp(1,2);

fprintf('\n  Cross-country validation (cumulative over %dQ):\n', N);
fprintf('  %20s   Pearson    Spearman\n', '');
fprintf('  %20s   %+.3f     %+.3f\n', 'theta ranking', corr_theta(1,2), spear_theta);
fprintf('  %20s   %+.3f     %+.3f\n', 'd ranking',     corr_d(1,2),     spear_d);

F_total_cum = arrayfun(@(c) sum(c.FCP(1:N) + c.FDI(1:N)), cdata)';
resid_theta = theta_obs_cum - theta_sim_cum;
resid_d     = d_obs_cum     - d_sim_cum;
corr_rt_F = corrcoef(resid_theta, F_total_cum);
corr_rd_F = corrcoef(resid_d,     F_total_cum);
fprintf('\n  Orthogonality (Stage 2 requirement):\n');
fprintf('    Corr(theta resid, total F): r = %+.3f\n', corr_rt_F(1,2));
fprintf('    Corr(d resid,     total F): r = %+.3f\n', corr_rd_F(1,2));
fprintf('    (|r| < 0.2 required for measurement-error orthogonality)\n');

% Visualization
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

figure('Name','Calibration V11','Color','w','Position',[50 50 1100 400]);

% Output Subplot - nur Validation-Window (Q4.19 - Q1.22)
subplot(1,2,1); hold on;
fill_iqr(1:K_y, sim_y_all(:,1:K_y), [0 .4 .8], .15);
fill_iqr(1:K_y, obs_y_all(:,1:K_y), [.5 .5 .5], .12);
plot(1:K_y, median(sim_y_all(:,1:K_y)), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:K_y, median(obs_y_all(:,1:K_y)), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:K_y, 'XTickLabel', qlbl(1:K_y), 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title(sprintf('Output Gap (Q4.19-Q1.22, K_y=%d)', K_y));
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.05, sprintf('RMSE = %.2f pp', median([cdata.rmse_y])), ...
    'Units','normalized','FontSize',8,'BackgroundColor','w');

% Debt Subplot - voller Bereich (K_b = N)
subplot(1,2,2); hold on;
fill_iqr(1:K_b, sim_b_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, obs_b_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(sim_b_all(:,1:K_b)), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:K_b, median(obs_b_all(:,1:K_b)), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
grid on;
set(gca, 'XTick', 1:K_b, 'XTickLabel', qlbl(1:K_b), 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title(sprintf('Cumulative Debt (Q4.19-Q4.22, K_b=%d)', K_b));
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp', median([cdata.rmse_b])), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

sgtitle('Calibration V11 - Country-FE only spec','FontWeight','bold');

fprintf('\n=== CALIBRATION COMPLETE ===\n\n');


%% ========================================================================
%  CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V11 (Country-FE only)                   #\n');
fprintf('################################################################\n\n');

y_obs_sd = std(reshape([cdata.y], 1, []));
b_obs_sd = std(arrayfun(@(c) c.obs_b_cum(K_b), cdata));
rmse_y_med = median([cdata.rmse_y]);
rmse_b_med = median([cdata.rmse_b]);

fprintf('1. TARGETED MOMENTS\n');
fprintf('   Output RMSE (median):   %.2f pp   (Obs SD: %.2f, ratio: %.2f)\n', ...
    rmse_y_med, y_obs_sd, rmse_y_med/y_obs_sd);
fprintf('   Debt   RMSE (median):   %.2f pp   (Obs SD: %.2f, ratio: %.2f)\n', ...
    rmse_b_med, b_obs_sd, rmse_b_med/b_obs_sd);
fprintf('   Mean Debt Resid:        %+.2f pp\n', mean(resid_b));

fprintf('\n2. NON-TARGETED MOMENTS\n');
fprintf('   SD ratio (mean):    %.3f   (target: ~1)\n', mean(sd_ratios));
fprintf('   AC(1):  obs %.3f / sim %.3f\n', mean(ac1_obs), mean(ac1_sim));
fprintf('   ICC y:  obs %.3f / sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('   ICC b:  obs %.3f / sim %.3f\n', icc_b_obs, icc_b_sim);

fprintf('\n3. FISCAL CONTRIBUTION\n');
fprintf('   Median y-gain:    %+.2f pp\n', median(diffs));
fprintf('   Mean y-gain:      %+.2f pp\n', mean(diffs));
fprintf('   Sign consistency: %d / %d positive\n', sum(diffs>0), n_c);

fprintf('\n4. CHECKLIST\n');
checks = {
    'Output RMSE / Obs SD < 0.7',  rmse_y_med/y_obs_sd < 0.7;
    'Debt RMSE / Obs SD < 0.7',    rmse_b_med/b_obs_sd < 0.7;
    'Mean Debt Resid < 1 pp',       abs(mean(resid_b)) < 1;
    'SD ratio in [0.7, 1.3]',       mean(sd_ratios) > 0.7 && mean(sd_ratios) < 1.3;
    'AC(1) gap < 0.1',              abs(mean(ac1_obs)-mean(ac1_sim)) < 0.1;
    'Fiscal contribution > 0',      median(diffs) > 0;
    'Sign consistency 38/38',       sum(diffs>0) == n_c;
};
for i = 1:size(checks,1)
    status = '[FAIL]'; if checks{i,2}, status = '[ OK ]'; end
    fprintf('   %s  %s\n', status, checks{i,1});
end
fprintf('\n   PASSED: %d / %d\n', sum([checks{:,2}]), size(checks,1));
fprintf('\n################################################################\n\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

% =========================================================================
%  forward_roll: V11 spec
%
%  STATE EQUATIONS:
%    Output:  y_{k+1} = mu_y + rho_y*y_k + alpha_S*S_k
%                     + alpha_CP_cum * F_CP_cum_k
%                     + alpha_CP_lag2 * F_CP_{k-1}    (Lag-2 in time means
%                                                      using F_CP at iter k-1
%                                                      -> 1-period state lag)
%                     + alpha_DI_lag1 * F_DI_k
%                     + alpha_S_DI * S_k * F_DI_k
%                     + eps_y_{k+1}
%
%    Debt:    b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%                     + kappa_above*F^CP,above_k + kappa_loans*F^CP,loans_k
%                     + kappa_guar*F^CP,guar_k + kappa_F_DI*F^DI_{k-1}
%                     + kappa_H*F^H_k + phi_t*year_idx_k
%
%  NOTE on output equation lag convention:
%    In iter k, computing y at time k. We need:
%      - y_{t-1} = y at end of iter k-1 = xs(1,k)
%      - F_CP_lag2 = F_CP at time t-2 = F_CP at iter k-1 (since iter k = time k)
%        Wait - if iter k computes time k and F_CP at iter k = time k,
%        then F_CP_lag2 = F_CP at time k-2 = fcp(k-2) for k>=3, else 0.
%      - F_CP_cum at time k = sum(fcp(1:k))
%      - F_DI_lag1 = F_DI at time k-1 = fdi(k-1) for k>=2, else 0.
%      - S at time k = S(k)
% =========================================================================
function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
    N_ = P.N;
    xs = zeros(P.nx, N_+1);   % State (y, b)

    % CP-Stock mit geometrischer Abschreibung delta_cp
    % fcp_cum_eff(t) = sum_{j=0}^{t-1} fcp(t-j) * delta^j
    fcp_cum_eff = zeros(1, N_);
    delta_pow = P.delta_cp .^ (0:N_-1);
    for t = 1:N_
        fcp_cum_eff(t) = sum(fcp(1:t) .* fliplr(delta_pow(1:t)));
    end

    for k = 1:N_
        % Aktueller State
        y = xs(1,k); b = xs(2,k);

        % Inputs am Zeitpunkt k
        Sk = 0; if k <= length(S), Sk = S(k); end

        % F_CP-Komponenten am Zeitpunkt k (fuer Debt-Equation)
        fa_k = 0; fl_k = 0; fg_k = 0;
        if k <= length(fcp_above), fa_k = fcp_above(k); end
        if k <= length(fcp_loans), fl_k = fcp_loans(k); end
        if k <= length(fcp_guar),  fg_k = fcp_guar(k);  end
        hk = 0; if k <= length(fh), hk = fh(k); end

        % Lags
        fcp_l2 = 0; if k >= 3, fcp_l2 = fcp(k-2); end
        fcp_c  = 0; if k <= length(fcp_cum_eff), fcp_c = fcp_cum_eff(k); end
        fdi_l1 = 0; if k >= 2, fdi_l1 = fdi(k-1); end
        fdi_lag = fdi_l1;

        % eps und year_idx
        ey = 0;     if k+1 <= length(P.eps_y_vec),    ey = P.eps_y_vec(k+1); end
        yr_idx = 0; if k <= length(P.year_idx_vec),   yr_idx = P.year_idx_vec(k); end

        % Output transition (V11)
        xs(1,k+1) = mu_y + P.rho_y * y + P.alpha_S * Sk ...
                  + P.alpha_CP_cum  * fcp_c ...
                  + P.alpha_CP_lag2 * fcp_l2 ...
                  + P.alpha_DI_lag1 * fdi_l1 ...
                  + P.alpha_S_DI    * Sk * fdi_l1 ...
                  + ey;

        % Debt transition (unchanged from V4)
        xs(2,k+1) = mu_b + (1+P.r_int) * b - P.gamma_y * y ...
                  + P.kappa_above * fa_k + P.kappa_loans * fl_k ...
                  + P.kappa_guar  * fg_k ...
                  + P.kappa_F_DI  * fdi_lag ...
                  + P.kappa_H     * hk ...
                  + P.phi_t       * yr_idx;
    end
end

% =========================================================================
%  fill_iqr
% =========================================================================
function fill_iqr(x, data, col, alpha)
    sd = sort(data);  n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end