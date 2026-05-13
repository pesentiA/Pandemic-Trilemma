%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V14 (Above-Flow + Below-Stock spec)
%
%  OUTPUT EQUATION (V14 frozen, ohne F_H):
%    y_t = mu_country + rho_y * y_{t-1} + alpha_S * S_t
%        + alpha_above * F_CP_above_{t-2}
%        + alpha_below * K_below_t              (Stock!)
%        + alpha_DI    * F_DI_{t-1}
%        + alpha_S_DI  * S_t * F_DI_{t-1}
%        + eps_t
%
%    K_below_t = sum_{s<=t} (0.6 * F_loans_s + 0.35 * F_guar_s)
%
%  DEBT EQUATION:
%    b_t = mu_b_country + (1+r) * b_{t-1} - gamma_y * y_t
%        + kappa_above   * F_CP_above_t
%        + kappa_below   * F_CP_below_flow_t
%        + kappa_DI      * F_DI_{t-1}
%        + phi_t * year_idx_t
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V14 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  PARAMETERS - V14 FROZEN
% =========================================================================

% --- Output equation (V14, N=418, Country-FE, ohne F_H) ---
rho_y          =  0.231;
alpha_S        = -0.0952;
alpha_above    =  0.544;
alpha_below    =  0.2611; 
alpha_DI_lag1  =  1.470;
alpha_S_DI     = -0.0406;

% --- Take-up adjustments ---
takeup_loans   =  0.4;
takeup_guar    =  0.25;

% --- Debt equation (disaggregated below) ---
r_int        =  0.001;
gamma_y      =  0.194;
kappa_above  =  0.442;
kappa_loans  =  0.601;   % applied to F_CP_loans_adj = takeup_loans*F_loans
kappa_guar   =  0.116;   % applied to F_CP_guar_adj (already at 35%)
kappa_DI     =  0.405;
phi_t        =  -0.159;       % no time trend in new debt spec

% --- Country ISO list ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Country FE: Output (V14 ohne F_H) ---
cfe_y_val = [+1.1057, -1.0400, +0.3009, -0.0979, +1.2987, +1.3246, +1.9894, +0.1466, ...
             -3.5623, -1.7381, -0.0832, -4.8958, -1.8327, -1.4833, -1.8693, -3.3268, ...
             +0.2908, -2.0146, +8.3187, -4.8488, +2.3672, -0.5014, -1.9561, +0.6141, ...
             +0.7002, +2.3568, -0.6830, -3.2057, +1.0604, +1.2218, -1.0616, -0.5966, ...
             -2.7751, -0.2578, -1.8284, +1.0349, +4.0658, +1.0987];

% --- Country FE: Debt ---
cfe_b_val = [-0.6499, -0.2429, -0.4368, -0.1899, -0.5455, -0.9649, +0.1537, +0.3904, ...
             -1.0153, -1.0384, -1.3218, -1.1961, -0.5657, -0.5214, -0.8029, -0.8503, ...
             -0.6150, -0.6489, +0.9077, -0.9999, +0.2391, -0.6142, -0.2601, -0.3154, ...
             -0.4946, +0.3033, -0.0065, -1.6026, -0.4003, -0.4386, +0.3718, -0.6048, ...
             -1.4040, -0.0569, -0.3848, -0.3742, +0.2760, +0.6712];

year_idx_raw = [0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3];
year_idx_vec = year_idx_raw - mean(year_idx_raw);

N = 13;  K_act = 13;  nx = 2;  nu = 2;
K_y = 10;  K_b = 13;
u_lo = [0; 0];  u_hi = [20.0; 10.0];

eps_y_vec = zeros(1, N+1);
eps_y_vec(4) = -5.40;  % fallback median

% Per-country Q2.20 V14 residuals
eps_v14_iso = cfe_iso;
eps_v14_val = [-3.62, -8.55, -6.46, -7.79, -4.73, -10.10, -11.20, -3.75, ...
               -5.10, -5.98, -1.95, -9.57, -2.43, -1.97, -8.49, -12.50, ...
               -9.94, -8.99, -1.10, -4.70, -0.66, -10.90, -6.82, -1.83, ...
               -0.03, -5.31, -7.43, -7.97, -5.17, -2.45, -5.49, -1.80, ...
               -9.75, -4.87, -6.78, -4.70, -10.40, -4.79];
eps_v14_map = containers.Map(eps_v14_iso, eps_v14_val);

beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

cfe_y_map = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map = containers.Map(cfe_iso, cfe_b_val);

P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, ...
    'alpha_above',alpha_above, 'alpha_below',alpha_below, ...
    'alpha_DI_lag1',alpha_DI_lag1, 'alpha_S_DI',alpha_S_DI, ...
    'takeup_loans',takeup_loans, 'takeup_guar',takeup_guar, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, ...
    'kappa_DI',kappa_DI, 'phi_t',phi_t, ...
    'eps_y_vec',eps_y_vec, 'year_idx_vec',year_idx_vec, ...
    'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'K_y',K_y, 'K_b',K_b, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);


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
    cdata(i).FCP_above  = zeros(1,N);
    cdata(i).FCP_loans  = zeros(1,N);
    cdata(i).FCP_guar   = zeros(1,N);
    cdata(i).FDI        = zeros(1,N);
    cdata(i).y          = zeros(1,N);
    cdata(i).b          = zeros(1,N);
    cdata(i).mu_y = 0;  cdata(i).mu_b = 0;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end
    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k)         = row.S_mean_tw;
        cdata(i).FCP_above(k) = row.F_CP_above_3;
        cdata(i).FCP_loans(k) = row.F_CP_loans;
        cdata(i).FCP_guar(k)  = row.F_CP_guar_adj;   % CSV value at 35% baseline
        cdata(i).FDI(k)       = row.F_DI;
        cdata(i).y(k)         = row.y_t_pct;
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b(k)     = row.debt_dR;
        end
    end
    cdata(i).FCP_loans_adj    = takeup_loans * cdata(i).FCP_loans;
    % FCP_guar from CSV is at 35%; rescale to actual takeup_guar
    cdata(i).FCP_guar_adj     = (takeup_guar / 0.35) * cdata(i).FCP_guar;
    cdata(i).FCP_below_flow   = cdata(i).FCP_loans_adj + cdata(i).FCP_guar_adj;
    cdata(i).FCP_below_stock  = cumsum(cdata(i).FCP_below_flow);
    % Per-country eps Q2.20 (V14 residuals)
    cdata(i).eps_vec    = zeros(1, N+1);
    eps_q220 = -5.40;  % fallback
    if isKey(eps_v14_map, iso), eps_q220 = eps_v14_map(iso); end
    cdata(i).eps_vec(4) = eps_q220;
end
fprintf('  %d countries x %d quarters\n', n_c, N);
fprintf('  Loans take-up: %.0f%%, Guarantees take-up: %.0f%%\n\n', ...
    takeup_loans*100, takeup_guar*100);

%%% ========================================================================
%  STEP 1: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation V14\n');
fprintf('========================================\n');

for i = 1:n_c
    xs = forward_roll(cdata(i).FCP_above, cdata(i).FCP_loans_adj, cdata(i).FCP_guar_adj, ...
        cdata(i).FCP_below_stock, cdata(i).FDI, ...
        cdata(i).S, cdata(i).mu_y, cdata(i).mu_b, P, cdata(i).eps_vec);
    cdata(i).sim_y     = xs(1, 2:end);
    cdata(i).sim_b     = xs(2, 2:end);
    cdata(i).obs_b_cum = cumsum(cdata(i).b);
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y(1:K_y) - cdata(i).y(1:K_y)).^2));
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b(1:K_b) - cdata(i).obs_b_cum(1:K_b)).^2));
end

fprintf('  Output RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n', ...
    K_y, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d) -- Median: %.2f pp   Mean: %.2f pp\n\n', ...
    K_b, median([cdata.rmse_b]), mean([cdata.rmse_b]));

fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s\n', 'Quarter', 'Observed', 'Model');
for k = 1:N
    obs_k = arrayfun(@(c) c.y(k), cdata);
    sim_k = arrayfun(@(c) c.sim_y(k), cdata);
    marker = ' '; if k > K_y, marker = '*'; end
    fprintf('  %8s %+9.2f %+9.2f %s\n', qlbl{k}, median(obs_k), median(sim_k), marker);
end

% Fiscal contribution
fprintf('\n  Fiscal contribution (Full - NoFiscal, cum %dQ):\n', K_y);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), zeros(1,N), zeros(1,N), zeros(1,N), zeros(1,N), ...
        cdata(i).S, cdata(i).mu_y, cdata(i).mu_b, P, cdata(i).eps_vec);
    diffs(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nof(1, 2:K_y+1));
end
fprintf('    Median: %+.2f pp,  Mean: %+.2f pp,  (F > 0: %d/%d)\n', ...
    median(diffs), mean(diffs), sum(diffs > 0), n_c);

% Channel decomposition
fprintf('\n  Channel decomposition (cum %dQ):\n', K_y);
above_contrib = zeros(n_c, 1);
below_contrib = zeros(n_c, 1);
di_contrib    = zeros(n_c, 1);
for i = 1:n_c
    xs_noab = forward_roll(zeros(1,N), cdata(i).FCP_loans_adj, cdata(i).FCP_guar_adj, ...
        cdata(i).FCP_below_stock, cdata(i).FDI, cdata(i).S, cdata(i).mu_y, cdata(i).mu_b, P, cdata(i).eps_vec);
    xs_nobe = forward_roll(cdata(i).FCP_above, zeros(1,N), zeros(1,N), zeros(1,N), ...
        cdata(i).FDI, cdata(i).S, cdata(i).mu_y, cdata(i).mu_b, P, cdata(i).eps_vec);
    xs_nodi = forward_roll(cdata(i).FCP_above, cdata(i).FCP_loans_adj, cdata(i).FCP_guar_adj, ...
        cdata(i).FCP_below_stock, zeros(1,N), cdata(i).S, cdata(i).mu_y, cdata(i).mu_b, P, cdata(i).eps_vec);
    above_contrib(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_noab(1, 2:K_y+1));
    below_contrib(i) = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nobe(1, 2:K_y+1));
    di_contrib(i)    = sum(cdata(i).sim_y(1:K_y)) - sum(xs_nodi(1, 2:K_y+1));
end
fprintf('    Above-Flow:  median %+.2f pp  (>0: %d/%d)\n', ...
    median(above_contrib), sum(above_contrib > 0), n_c);
fprintf('    Below-Stock: median %+.2f pp  (>0: %d/%d)\n', ...
    median(below_contrib), sum(below_contrib > 0), n_c);
fprintf('    DI:          median %+.2f pp  (>0: %d/%d)\n', ...
    median(di_contrib), sum(di_contrib > 0), n_c);


%% ========================================================================
%  STEP 1b: NON-TARGETED MOMENTS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1b: Non-Targeted Moments\n');
fprintf('========================================\n');

fprintf('\n  Cross-Country SD:\n');
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
fprintf('\n  AC(1):  obs %.3f / sim %.3f\n', mean(ac1_obs), mean(ac1_sim));

y_obs_p = reshape([cdata.y], N, n_c)';
y_sim_p = reshape([cdata.sim_y], N, n_c)';
icc_y_obs = var(mean(y_obs_p(:,1:K_y),2)) / var(y_obs_p(:,1:K_y),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_y),2)) / var(y_sim_p(:,1:K_y),0,'all');
b_obs_p = zeros(n_c, K_b);
b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_b); end
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_b),2)) / var(b_sim_p(:,1:K_b),0,'all');
fprintf('  ICC y:  obs %.3f / sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('  ICC b:  obs %.3f / sim %.3f\n', icc_b_obs, icc_b_sim);


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
for i = 1:n_c
    sim_b_cum(i) = cdata(i).sim_b(K_b);
    obs_b_cum(i) = cdata(i).obs_b_cum(K_b);
    resid_b(i)   = obs_b_cum(i) - sim_b_cum(i);
end

[~, sort_idx] = sort(resid_b);
fprintf('\n  Largest negative residuals (model OVER-estimates):\n');
fprintf('  %5s %10s %10s %10s\n', 'ISO', 'Obs', 'Sim', 'Resid');
for j = 1:5
    i = sort_idx(j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f\n', iso_list{i}, ...
        obs_b_cum(i), sim_b_cum(i), resid_b(i));
end
fprintf('\n  Largest positive residuals (model UNDER-estimates):\n');
for j = 0:4
    i = sort_idx(n_c - j);
    fprintf('  %5s %+10.2f %+10.2f %+10.2f\n', iso_list{i}, ...
        obs_b_cum(i), sim_b_cum(i), resid_b(i));
end

fprintf('\n  Residual summary:\n');
fprintf('    Mean: %+.2f, Median: %+.2f, SD: %.2f, Range: [%+.2f, %+.2f]\n', ...
    mean(resid_b), median(resid_b), std(resid_b), min(resid_b), max(resid_b));


%% ========================================================================
%  POLICY LEVER INTENSITY: OECD Median
% =========================================================================
fprintf('\n========================================\n');
fprintf('  POLICY LEVER INTENSITY (OECD Median)\n');
fprintf('========================================\n');

S_med    = zeros(1, N);
FCPab_med = zeros(1, N);
FCPbe_med = zeros(1, N);
FCPbe_stock_med = zeros(1, N);
FDI_med  = zeros(1, N);

for k = 1:N
    S_med(k)        = median(arrayfun(@(c) c.S(k),              cdata));
    FCPab_med(k)    = median(arrayfun(@(c) c.FCP_above(k),      cdata));
    FCPbe_med(k)    = median(arrayfun(@(c) c.FCP_below_flow(k), cdata));
    FCPbe_stock_med(k) = median(arrayfun(@(c) c.FCP_below_stock(k), cdata));
    FDI_med(k)      = median(arrayfun(@(c) c.FDI(k),            cdata));
end

fprintf('\n  %8s %7s %8s %8s %9s %7s\n', ...
    'Quarter', 'S', 'F^above', 'F^below', 'K^below', 'F^DI');
for k = 1:N
    fprintf('  %8s %7.2f %8.3f %8.3f %9.3f %7.3f\n', ...
        qlbl{k}, S_med(k), FCPab_med(k), FCPbe_med(k), FCPbe_stock_med(k), FDI_med(k));
end

fprintf('\n  Cumulative (pp of 2019 GDP):\n');
fprintf('    F^above:  %.2f\n', sum(FCPab_med));
fprintf('    F^below:  %.2f\n', sum(FCPbe_med));
fprintf('    K^below (final):  %.2f\n', FCPbe_stock_med(end));
fprintf('    F^DI:  %.2f\n', sum(FDI_med));
fprintf('    Mean S: %.2f,  Peak S: %.2f\n', mean(S_med), max(S_med));


%% ========================================================================
%  VISUALIZATION
% =========================================================================
sim_y_all = reshape([cdata.sim_y], N, n_c)';
obs_y_all = reshape([cdata.y],     N, n_c)';
sim_b_all = reshape([cdata.sim_b], N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum; end

figure('Name','Calibration V14','Color','w','Position',[50 50 1100 400]);

subplot(1,2,1); hold on;
fill_iqr(1:K_y, sim_y_all(:,1:K_y), [0 .4 .8], .15);
fill_iqr(1:K_y, obs_y_all(:,1:K_y), [.5 .5 .5], .12);
plot(1:K_y, median(sim_y_all(:,1:K_y)), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:K_y, median(obs_y_all(:,1:K_y)), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:K_y, 'XTickLabel', qlbl(1:K_y), 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title(sprintf('Output Gap (K_y=%d)', K_y));
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.05, sprintf('RMSE = %.2f pp', median([cdata.rmse_y])), ...
    'Units','normalized','FontSize',8,'BackgroundColor','w');

subplot(1,2,2); hold on;
fill_iqr(1:K_b, sim_b_all(:,1:K_b), [0 .4 .8], .15);
fill_iqr(1:K_b, obs_b_all(:,1:K_b), [.5 .5 .5], .12);
plot(1:K_b, median(sim_b_all(:,1:K_b)), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:K_b, median(obs_b_all(:,1:K_b)), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
grid on;
set(gca, 'XTick', 1:K_b, 'XTickLabel', qlbl(1:K_b), 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title(sprintf('Cumulative Debt (K_b=%d)', K_b));
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp', median([cdata.rmse_b])), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

sgtitle('Calibration V14 - Above-Flow + Below-Stock','FontWeight','bold');

figure('Name','Policy Lever Intensity V14','Color','w','Position',[100 100 1100 600]);

subplot(2,2,1);
plot(1:N, S_med, 'r-o', 'LineWidth', 1.8); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('Stringency Index'); title('Containment (S)');

subplot(2,2,2); hold on;
plot(1:N, FCPab_med, 'b-o', 'LineWidth', 1.8);
plot(1:N, FCPbe_med, 'g-s', 'LineWidth', 1.8);
plot(1:N, FDI_med,  'm-^', 'LineWidth', 1.8); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Fiscal Flows');
legend('F^{above}','F^{below}','F^{DI}', 'Location','NE');

subplot(2,2,3);
plot(1:N, FCPbe_stock_med, 'g-o', 'LineWidth', 2); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Below Stock K^{below}');

subplot(2,2,4);
bar(1:N, [FCPab_med', FCPbe_med', FDI_med'], 'stacked');
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Total Fiscal Stack');
legend('Above','Below','DI', 'Location','NE');

sgtitle('OECD Median Policy Levers V14','FontWeight','bold');


%% ========================================================================
%  CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V14                                     #\n');
fprintf('################################################################\n\n');

y_obs_sd = std(reshape([cdata.y], 1, []));
b_obs_sd = std(arrayfun(@(c) c.obs_b_cum(K_b), cdata));
rmse_y_med = median([cdata.rmse_y]);
rmse_b_med = median([cdata.rmse_b]);

fprintf('1. TARGETED MOMENTS\n');
fprintf('   Output RMSE (median):   %.2f pp   (ratio: %.2f)\n', ...
    rmse_y_med, rmse_y_med/y_obs_sd);
fprintf('   Debt   RMSE (median):   %.2f pp   (ratio: %.2f)\n', ...
    rmse_b_med, rmse_b_med/b_obs_sd);
fprintf('   Mean Debt Resid:        %+.2f pp\n', mean(resid_b));

fprintf('\n2. NON-TARGETED MOMENTS\n');
fprintf('   SD ratio (mean):    %.3f\n', mean(sd_ratios));
fprintf('   AC(1):  obs %.3f / sim %.3f\n', mean(ac1_obs), mean(ac1_sim));
fprintf('   ICC y:  obs %.3f / sim %.3f\n', icc_y_obs, icc_y_sim);
fprintf('   ICC b:  obs %.3f / sim %.3f\n', icc_b_obs, icc_b_sim);

fprintf('\n3. FISCAL CONTRIBUTION\n');
fprintf('   Median y-gain:    %+.2f pp\n', median(diffs));
fprintf('   Total fiscal > 0: %d / %d\n', sum(diffs>0), n_c);
cp_contrib = above_contrib + below_contrib;
fprintf('   CP-only      > 0: %d / %d\n', sum(cp_contrib>0), n_c);
neg_idx = find(diffs < 0);
if ~isempty(neg_idx)
    fprintf('   Countries with negative total (push-on-string):\n');
    for j = 1:length(neg_idx)
        i = neg_idx(j);
        fprintf('     %s: total=%+.2f, CP=%+.2f, DI=%+.2f\n', ...
            cdata(i).iso, diffs(i), cp_contrib(i), di_contrib(i));
    end
end

fprintf('\n4. CHANNEL DECOMPOSITION (median pp-Q)\n');
fprintf('   Above-Flow:  %+.2f\n', median(above_contrib));
fprintf('   Below-Stock: %+.2f\n', median(below_contrib));
fprintf('   DI:          %+.2f\n', median(di_contrib));

fprintf('\n5. CHECKLIST\n');
checks = {
    'Output RMSE / Obs SD < 0.7',         rmse_y_med/y_obs_sd < 0.7;
    'Debt RMSE / Obs SD < 0.7',           rmse_b_med/b_obs_sd < 0.7;
    'Mean Debt Resid < 1 pp',             abs(mean(resid_b)) < 1;
    'SD ratio in [0.7, 1.3]',             mean(sd_ratios) > 0.7 && mean(sd_ratios) < 1.3;
    'AC(1) gap < 0.1',                    abs(mean(ac1_obs)-mean(ac1_sim)) < 0.1;
    'Fiscal contribution > 0 (median)',   median(diffs) > 0;
    'CP-only positive (38/38)',           sum(cp_contrib > 0) == n_c;
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
function xs = forward_roll(fcp_above, fcp_loans_adj, fcp_guar_adj, ...
                           fcp_below_stock, fdi, S, mu_y, mu_b, P, eps_vec_override)
    if nargin < 10, eps_vec_override = []; end
    eps_vec = P.eps_y_vec;
    if ~isempty(eps_vec_override), eps_vec = eps_vec_override; end
    N_ = P.N;
    xs = zeros(P.nx, N_+1);

    for k = 1:N_
        y = xs(1,k); b = xs(2,k);
        Sk = 0; if k <= length(S), Sk = S(k); end

        fab_k  = 0; if k <= length(fcp_above),     fab_k = fcp_above(k);     end
        floa_k = 0; if k <= length(fcp_loans_adj), floa_k = fcp_loans_adj(k); end
        fgua_k = 0; if k <= length(fcp_guar_adj),  fgua_k = fcp_guar_adj(k);  end

        fab_l2 = 0; if k >= 3, fab_l2 = fcp_above(k-2); end
        fdi_l1 = 0; if k >= 2, fdi_l1 = fdi(k-1);       end

        kbe_k  = 0; if k <= length(fcp_below_stock), kbe_k = fcp_below_stock(k); end

        ey = 0;     if k+1 <= length(eps_vec),        ey = eps_vec(k+1);         end
        yr_idx = 0; if k <= length(P.year_idx_vec),   yr_idx = P.year_idx_vec(k); end

        xs(1,k+1) = mu_y + P.rho_y * y + P.alpha_S * Sk ...
                  + P.alpha_above * fab_l2 ...
                  + P.alpha_below * kbe_k ...
                  + P.alpha_DI_lag1 * fdi_l1 ...
                  + P.alpha_S_DI    * Sk * fdi_l1 ...
                  + ey;

        xs(2,k+1) = mu_b + (1+P.r_int) * b - P.gamma_y * y ...
                  + P.kappa_above * fab_k ...
                  + P.kappa_loans * floa_k ...
                  + P.kappa_guar  * fgua_k ...
                  + P.kappa_DI    * fdi_l1 ...
                  + P.phi_t       * yr_idx;
    end
end

function fill_iqr(x, data, col, alpha)
    sd = sort(data);  n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end