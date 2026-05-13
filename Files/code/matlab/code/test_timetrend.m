%% ========================================================================
%  PANDEMIC TRILEMMA - CALIBRATION V14 + WAVE2 ADAPTIVE S
%
%  OUTPUT EQUATION (V14 + Wave2 indicator):
%    y_t = mu_country + rho_y * y_{t-1}
%        + alpha_S * S_t
%        + alpha_S_Wave2 * S_t * 1{Wave2_t}      (adaptive elasticity)
%        + alpha_above * F_CP_above_{t-2}
%        + alpha_below * K_below_t              (Stock!)
%        + alpha_DI    * F_DI_{t-1}
%        + alpha_S_DI  * S_t * F_DI_{t-1}
%        + eps_t
%
%    K_below_t = sum_{s<=t} (takeup_loans * F_loans_s + takeup_guar * F_guar_s)
%    Wave2 = {Q4.20, Q1.21, Q2.21}   -> reduces S-drag in 2nd wave
%                                       (behavioral adaptation, remote-
%                                       work, sector-specific protocols)
%
%  DEBT EQUATION:
%    b_t = mu_b_country + (1+r) * b_{t-1} - gamma_y * y_t
%        + kappa_above * F_CP_above_t
%        + kappa_loans * F_CP_loans_adj_t
%        + kappa_guar  * F_CP_guar_adj_t
%        + kappa_DI    * F_DI_{t-1}
%        + phi_t * t_idx_t                       (linear quarter trend)
%
%  ASYMMETRY OUTPUT vs DEBT EQUATION:
%    Output equation: NO time trend (stationary, AR(1) absorbs dynamics);
%    Wave2 indicator captures structural break in S-elasticity.
%    Debt equation:   linear quarter trend captures near-unit-root drift.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V14 + Wave2 ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  PARAMETERS - V14 + WAVE2 SPEC
% =========================================================================

% --- Output equation (V14 + Wave2, N=418, Country-FE) ---
% Source: feols(y_t_pct ~ y_lag1 + S_mean_tw + S_mean_tw:Wave2
%               + F_CP_above_flow_lag2 + F_CP_belowstock
%               + F_DI_lag1*S_mean_tw | Country)
rho_y          =  0.229277;
alpha_S        = -0.130404;
alpha_S_Wave2  =  0.050243;   % S-drag reduction in 2nd wave (Q4.20-Q2.21)
alpha_above    =  0.473037;
alpha_below    =  0.275508;
alpha_DI_lag1  =  1.077531;
alpha_S_DI     = -0.030930;

% --- Take-up adjustments ---
takeup_loans   =  0.40;
takeup_guar    =  0.25;

% --- Debt equation (unchanged from V14) ---
r_int        =  0.001;
gamma_y      =  0.176;
kappa_above  =  0.392;
kappa_loans  =  0.891;
kappa_guar   =  0.111;
kappa_DI     =  0.396;
phi_t        = -0.076;       % linear quarter trend coefficient

% --- Country ISO list ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};

% --- Country FE: Output ---
% NOTE: These are still the V14 (no-Wave2) FE values as approximation.
% For exact consistency with V14+Wave2 spec, re-extract via:
%   fixef(v14_wave)$Country in R.
cfe_y_val = [+1.8162, -0.3865, +0.8165, +0.5258, +1.8633, +1.7296, +2.7655, +0.8320, ...
             -3.1576, -1.2928, +0.2844, -4.4559, -1.3739, -1.0416, -1.2649, -2.8963, ...
             +1.1049, -1.6721, +8.7060, -4.4724, +2.9042, +0.3026, -1.3732, +1.1785, ...
             +1.0974, +2.9604, -0.1678, -2.6372, +1.5945, +1.6229, -0.2112, -0.1387, ...
             -2.2489, +0.2104, -1.2749, +1.3868, +4.5646, +1.6683];

% --- Country FE: Debt (unchanged from V14) ---
cfe_b_val = [+0.2296, +0.3707, +0.3387, +0.7393, +0.5104, -0.0655, +1.4904, +0.9267, ...
             -0.0006, -0.0780, -0.5517, -0.2551, +0.3079, +0.2583, +0.0484, -0.0142, ...
             +0.1481, -0.0351, +1.3739, -0.1310, +0.7809, +0.1886, +0.0381, +0.3889, ...
             +0.3229, +1.0169, +0.8753, -0.5943, +0.4736, +0.7201, +1.2750, +0.0755, ...
             -0.9275, +0.6452, +0.0694, +0.3347, +0.9441, +1.3417];

t_idx_raw = 4:16;
year_idx_vec = t_idx_raw;

% --- Wave2 indicator (k = 5, 6, 7 = Q4.20, Q1.21, Q2.21) ---
% Q4.19 -> k=1, Q1.20 -> k=2, ..., Q2.21 -> k=7
wave2_vec = zeros(1, 13);
wave2_vec(5:7) = 1;          % Q4.2020, Q1.2021, Q2.2021

N = 13;  K_act = 13;  nx = 2;  nu = 2;
K_y = 10;  K_b = 13;
u_lo = [0; 0];  u_hi = [20.0; 10.0];

eps_y_vec = zeros(1, N+1);
eps_y_vec(4) = -5.40;  % fallback median

% Per-country Q2.20 V14 residuals
% NOTE: from old V14 (no-Wave2) regression; re-extract from
% v14_wave residuals if exact match required.
eps_v14_iso = cfe_iso;
eps_v14_val = [+2.5082, -1.6947, +3.0183, -1.9950, +0.2742, -3.1304, -0.3829, -0.0322, ...
               +3.9271, +2.3078, +1.3133, -3.6054, +2.7086, +4.0962, -3.9676, +0.3301, ...
               -0.0307, +0.8499, -0.5288, +0.3090, +0.5154, -0.3122, -3.9698, +0.2148, ...
               +0.5013, -4.6440, +1.7618, +0.0044, +0.5709, +2.2634, +1.9210, -0.2300, ...
               +0.5131, -2.8767, -0.7936, +2.5167, -0.5797, +1.2297];
eps_v14_map = containers.Map(eps_v14_iso, eps_v14_val);

beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

cfe_y_map = containers.Map(cfe_iso, cfe_y_val);
cfe_b_map = containers.Map(cfe_iso, cfe_b_val);

P = struct( ...
    'rho_y',rho_y, 'alpha_S',alpha_S, 'alpha_S_Wave2',alpha_S_Wave2, ...
    'alpha_above',alpha_above, 'alpha_below',alpha_below, ...
    'alpha_DI_lag1',alpha_DI_lag1, 'alpha_S_DI',alpha_S_DI, ...
    'takeup_loans',takeup_loans, 'takeup_guar',takeup_guar, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_above',kappa_above, 'kappa_loans',kappa_loans, ...
    'kappa_guar',kappa_guar, ...
    'kappa_DI',kappa_DI, 'phi_t',phi_t, ...
    'eps_y_vec',eps_y_vec, 'year_idx_vec',year_idx_vec, ...
    'wave2_vec',wave2_vec, ...
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
        cdata(i).FCP_guar(k)  = row.F_CP_guar_adj;
        cdata(i).FDI(k)       = row.F_DI;
        cdata(i).y(k)         = row.y_t_pct;
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            cdata(i).b(k)     = row.debt_dR;
        end
    end
    cdata(i).FCP_loans_adj    = takeup_loans * cdata(i).FCP_loans;
    cdata(i).FCP_guar_adj     = (takeup_guar / 0.35) * cdata(i).FCP_guar;
    cdata(i).FCP_below_flow   = cdata(i).FCP_loans_adj + cdata(i).FCP_guar_adj;
    cdata(i).FCP_below_stock  = cumsum(cdata(i).FCP_below_flow);
    cdata(i).eps_vec    = zeros(1, N+1);
    % Q2.20 exact-fit: compute eps such that simulated Q2.20 = observed Q2.20.
    % Rationale: Q2.20 is the exogenous global pandemic shock (unobserved
    % drivers: novelty of virus, panic, supply-chain disruption). The model
    % is asked to explain RECOVERY (Q3.20+), not the initial impact.
    y_Q120_obs   = cdata(i).y(2);         % observed Q1.20 -> y_lag1 for Q2.20
    S_Q220       = cdata(i).S(3);
    FCPab_l2_Q220 = cdata(i).FCP_above(1); % lag-2 from Q4.19
    Kbelow_Q220  = cdata(i).FCP_below_stock(3);
    FDI_l1_Q220  = cdata(i).FDI(2);
    w2_Q220      = wave2_vec(3);          % Q2.20 not in wave2 -> 0
    aS_eff_Q220  = alpha_S + alpha_S_Wave2 * w2_Q220;

    y_pred_Q220 = cdata(i).mu_y + rho_y * y_Q120_obs ...
                + aS_eff_Q220   * S_Q220 ...
                + alpha_above   * FCPab_l2_Q220 ...
                + alpha_below   * Kbelow_Q220 ...
                + alpha_DI_lag1 * FDI_l1_Q220 ...
                + alpha_S_DI    * S_Q220 * FDI_l1_Q220;

    cdata(i).eps_vec(4) = cdata(i).y(3) - y_pred_Q220;  % exact Q2.20 fit
end
fprintf('  %d countries x %d quarters\n', n_c, N);
fprintf('  Loans take-up: %.0f%%, Guarantees take-up: %.0f%%\n', ...
    takeup_loans*100, takeup_guar*100);
fprintf('  Wave2 indicator active in: Q4.20, Q1.21, Q2.21\n\n');


%% ========================================================================
%  STEP 1: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation V14 + Wave2\n');
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
%  VISUALIZATION
% =========================================================================
sim_y_all = reshape([cdata.sim_y], N, n_c)';
obs_y_all = reshape([cdata.y],     N, n_c)';
sim_b_all = reshape([cdata.sim_b], N, n_c)';
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum; end

figure('Name','Calibration V14 + Wave2','Color','w','Position',[50 50 1100 400]);

subplot(1,2,1); hold on;
fill_iqr(1:K_y, sim_y_all(:,1:K_y), [0 .4 .8], .15);
fill_iqr(1:K_y, obs_y_all(:,1:K_y), [.5 .5 .5], .12);
plot(1:K_y, median(sim_y_all(:,1:K_y)), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:K_y, median(obs_y_all(:,1:K_y)), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
% Shade Wave2 period
yl = ylim;
fill([5 7 7 5], [yl(1) yl(1) yl(2) yl(2)], [1 .8 .4], ...
    'FaceAlpha', 0.10, 'EdgeColor', 'none');
set(gca, 'XTick', 1:K_y, 'XTickLabel', qlbl(1:K_y), 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title(sprintf('Output Gap (K_y=%d)', K_y));
legend('','','Simulated','Observed','Wave2', 'Location', 'SE', 'FontSize', 7);
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

sgtitle('Calibration V14 + Wave2 Adaptive S','FontWeight','bold');


%% ========================================================================
%  CALIBRATION REPORT
% =========================================================================
fprintf('\n################################################################\n');
fprintf('#  CALIBRATION REPORT - V14 + WAVE2                             #\n');
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

        % Wave2 adaptive S
        w2_k = 0; if k <= length(P.wave2_vec), w2_k = P.wave2_vec(k); end
        alpha_S_eff = P.alpha_S + P.alpha_S_Wave2 * w2_k;

        xs(1,k+1) = mu_y + P.rho_y * y + alpha_S_eff * Sk ...
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