%% ========================================================================
%  PANDEMIC TRILEMMA - PER-COUNTRY iLQR V14
%
%  V14 Spec: Above-Flow (Lag-2) + Below-Stock + DI*S (Push-on-string)
%
%  State (nx=6):
%    x(1) = y           output gap
%    x(2) = b           cumulative debt
%    x(3) = F_above_l1  above-the-line CP, lag 1
%    x(4) = F_above_l2  above-the-line CP, lag 2
%    x(5) = F_DI_l1     demand injection, lag 1
%    x(6) = K_below     accumulated below-the-line stock
%
%  Controls (nu=4):
%    u(1) = F_above        above-the-line flow
%    u(2) = F_loans_adj    take-up-adjusted loans (0.6 * authorized)
%    u(3) = F_guar_adj     take-up-adjusted guarantees (0.35 * authorized)
%    u(4) = F_DI           demand injection
% =========================================================================
clear; clc; close all;
fprintf('=== PER-COUNTRY iLQR V14 ===\n  %s\n\n', datestr(now));

%% Parameters (V14 frozen)
P.rho_y         =  0.231;   P.alpha_S       = -0.0952;
P.alpha_above   =  0.544;   P.alpha_below   =  0.131;
P.alpha_DI_lag1 =  1.470;   P.alpha_S_DI    = -0.0406;

P.takeup_loans  =  0.60;    P.takeup_guar   =  0.35;

P.r_int       =  0.001;     P.gamma_y     =  0.194;
P.kappa_above =  0.442;     P.kappa_loans =  0.601;   % applied to F_loans_adj
P.kappa_guar  =  0.116;     P.kappa_DI    =  0.405;
P.phi_t       =  0;

% Cost regularization (kalibriert für V14-Output-Stärke + realistische Limits)
P.r_above = 10;   % Above 5x höher alpha als V11 → höherer Penalty
P.r_loans = 15;   % Default-Risiko der Loans
P.r_guar  = 80;   % Contingent liability + moral hazard
P.r_di    = 5;
P.beta    = 0.99;
P.w_y     = 100;  P.w_b = 30;   P.W_b = 150;

P.N  = 13;   P.nx = 6;   P.nu = 4;
P.u_lo = [0; 0; 0; 0];
P.u_hi = [8; 4; 2; 6];   % realistische Quartalsobergrenzen

year_idx_raw = [0,1,1,1,1,2,2,2,2,3,3,3,3];
P.year_idx_vec = year_idx_raw - mean(year_idx_raw);
P.eps_y_vec = zeros(1, P.N+1);
P.eps_y_vec(4) = -5.40;     % V14 median Q2.20 residual

%% Country FE values from V14 estimation
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
cfe_b_val = [-0.6499, -0.2429, -0.4368, -0.1899, -0.5455, -0.9649, +0.1537, +0.3904, ...
             -1.0153, -1.0384, -1.3218, -1.1961, -0.5657, -0.5214, -0.8029, -0.8503, ...
             -0.6150, -0.6489, +0.9077, -0.9999, +0.2391, -0.6142, -0.2601, -0.3154, ...
             -0.4946, +0.3033, -0.0065, -1.6026, -0.4003, -0.4386, +0.3718, -0.6048, ...
             -1.4040, -0.0569, -0.3848, -0.3742, +0.2760, +0.6712];
mu_y_map = containers.Map(cfe_iso, cfe_y_val);
mu_b_map = containers.Map(cfe_iso, cfe_b_val);

%% Load CSV
T = readtable('country_data_for_matlab.csv');
qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

%% Per-country Q2.20 residuals (V11; ggf. mit V14-Residuen ueberschreiben)
% NOTE: Solange V14-eps-per-country nicht verfuegbar sind, nutzen wir V11-Werte
% als Approximation (gleiches Muster, leicht andere Niveaus).
eps_Q2_20_iso = cfe_iso;
eps_Q2_20_val = [-3.93, -8.67, -6.37, -7.83, -4.81, -9.99, -11.00, -3.57, ...
                 -5.01, -5.92, -1.93, -9.30, -2.58, -1.89, -8.35, -12.50, ...
                 -9.88, -8.85, -1.06, -4.65, -0.71, -10.90, -6.72, -1.69, ...
                  0.00, -5.30, -7.35, -7.81, -5.25, -2.38, -5.78, -1.84, ...
                 -9.61, -4.91, -6.85, -4.69, -10.30, -4.79];
eps_Q2_20_map = containers.Map(eps_Q2_20_iso, eps_Q2_20_val);

%% Loop: per-country observed + optimum
fprintf('--- Running V14 iLQR for %d countries ---\n', n_c);
res_obs = struct();
res_opt = struct();
eps_per_country = zeros(n_c, 1);

for i = 1:n_c
    iso = countries{i};
    sub_c = T(strcmp(T.Country, iso), :);

    S_c     = zeros(1,P.N);
    FCPab_c = zeros(1,P.N);
    Floa_c  = zeros(1,P.N);   % loans_adj (post take-up)
    Fgua_c  = zeros(1,P.N);   % guar_adj  (already 35% in CSV)
    FDI_c   = zeros(1,P.N);
    y_c     = zeros(1,P.N);
    b_c     = zeros(1,P.N);

    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        S_c(k)     = row.S_mean_tw;
        FCPab_c(k) = row.F_CP_above_3;
        Floa_c(k)  = P.takeup_loans * row.F_CP_loans;
        Fgua_c(k)  = row.F_CP_guar_adj;
        FDI_c(k)   = row.F_DI;
        y_c(k)     = row.y_t_pct;
        if ismember('debt_dR', T.Properties.VariableNames), b_c(k) = row.debt_dR; end
    end

    P.S    = S_c;
    P.mu_y = 0; if isKey(mu_y_map, iso), P.mu_y = mu_y_map(iso); end
    P.mu_b = 0; if isKey(mu_b_map, iso), P.mu_b = mu_b_map(iso); end

    P.eps_y_vec    = zeros(1, P.N+1);
    eps_Q2_20      = -5.40;
    if isKey(eps_Q2_20_map, iso), eps_Q2_20 = eps_Q2_20_map(iso); end
    P.eps_y_vec(4) = eps_Q2_20;
    eps_per_country(i) = eps_Q2_20;

    % Initial state: y_c(1), b=0, lags=0, K_below=0
    x0 = [y_c(1); 0; 0; 0; 0; 0];

    % Warm start with observed F-paths (disaggregated)
    u_init = max(P.u_lo, min(P.u_hi, [FCPab_c; Floa_c; Fgua_c; FDI_c]));
    [u_opt, x_opt, ~] = ilqr_solve(x0, u_init, P);

    res_obs(i).iso       = iso;
    res_obs(i).cum_y     = sum(y_c);
    res_obs(i).fin_b     = sum(b_c);
    res_obs(i).cum_above = sum(FCPab_c);
    res_obs(i).cum_loans = sum(Floa_c);
    res_obs(i).cum_guar  = sum(Fgua_c);
    res_obs(i).cum_DI    = sum(FDI_c);

    res_opt(i).iso       = iso;
    res_opt(i).cum_y     = sum(x_opt(1, 2:end));
    res_opt(i).fin_b     = x_opt(2, end);
    res_opt(i).cum_above = sum(u_opt(1,:));
    res_opt(i).cum_loans = sum(u_opt(2,:));
    res_opt(i).cum_guar  = sum(u_opt(3,:));
    res_opt(i).cum_DI    = sum(u_opt(4,:));
end

fprintf('\n  Per-country Q2.20 eps (V11-Residuen als Proxy):\n');
fprintf('    Median: %+.2f pp, Mean: %+.2f pp, Range: [%+.2f, %+.2f]\n', ...
    median(eps_per_country), mean(eps_per_country), ...
    min(eps_per_country), max(eps_per_country));

% --- Exclude problematic countries from aggregate stats and plots ---
exclude_iso = {'TUR', 'IRL'};
keep_idx = ~ismember({res_obs.iso}, exclude_iso);
res_obs = res_obs(keep_idx);
res_opt = res_opt(keep_idx);
eps_per_country = eps_per_country(keep_idx);
countries = countries(keep_idx);
n_c = length(countries);
fprintf('\n  Excluded: %s  ->  N = %d remaining\n', strjoin(exclude_iso, ', '), n_c);

gain_y = [res_opt.cum_y] - [res_obs.cum_y];
diff_b = [res_opt.fin_b] - [res_obs.fin_b];

fprintf('\n  Output gain (Optimal - Observed, sum 13Q):\n');
fprintf('    Median: %+.2f pp-Q,  Mean: %+.2f pp-Q\n', median(gain_y), mean(gain_y));
fprintf('    Improved: %d / %d countries\n', sum(gain_y > 0.5), n_c);
fprintf('  Debt diff (Optimal - Observed):\n');
fprintf('    Median: %+.2f pp,  Mean: %+.2f pp\n', median(diff_b), mean(diff_b));

% Channel diffs
fprintf('\n  Median F diffs (Optimal - Observed):\n');
fprintf('    F^above:  %+.2f pp\n', median([res_opt.cum_above] - [res_obs.cum_above]));
fprintf('    F^loans:  %+.2f pp\n', median([res_opt.cum_loans] - [res_obs.cum_loans]));
fprintf('    F^guar :  %+.2f pp\n', median([res_opt.cum_guar]  - [res_obs.cum_guar]));
fprintf('    F^DI   :  %+.2f pp\n', median([res_opt.cum_DI]    - [res_obs.cum_DI]));

% Top 5 gainers
[~, sort_g] = sort(gain_y, 'descend');
fprintf('\n  Top 5 Output-Gewinner:\n');
fprintf('    %5s %8s %8s %8s %8s %8s\n', 'ISO','obs_y','opt_y','gain_y','obs_b','opt_b');
for j = 1:5
    i = sort_g(j);
    fprintf('    %5s %+8.2f %+8.2f %+8.2f %+8.2f %+8.2f\n', ...
        res_obs(i).iso, res_obs(i).cum_y, res_opt(i).cum_y, gain_y(i), ...
        res_obs(i).fin_b, res_opt(i).fin_b);
end


%% =========================================================================
%  PLOT: Per-Country Pareto
% =========================================================================
figure('Name','Per-Country Pareto V14','Color','w','Position',[80 80 1100 700]);
hold on;

obs_b = [res_obs.fin_b];  obs_y = [res_obs.cum_y];
opt_b = [res_opt.fin_b];  opt_y = [res_opt.cum_y];

for i = 1:n_c
    plot([obs_b(i), opt_b(i)], [obs_y(i), opt_y(i)], '-', ...
         'Color', [.7 .7 .7 .5], 'LineWidth', 0.7, 'HandleVisibility','off');
end

scatter(obs_b, obs_y, 80, [.4 .4 .4], 'filled', 'MarkerFaceAlpha', 0.6, ...
        'DisplayName','Beobachtet (CSV)');
scatter(opt_b, opt_y, 80, [0.1 0.4 0.8], 'filled', 'MarkerFaceAlpha', 0.7, ...
        'DisplayName','Optimal (V14 iLQR)');

for i = 1:n_c
    text(opt_b(i)+0.2, opt_y(i), res_opt(i).iso, 'FontSize', 7, 'Color', [.2 .2 .5]);
end

scatter(median(obs_b), median(obs_y), 250, 'k', 's', 'filled', ...
        'DisplayName', 'OECD-Median beobachtet');
scatter(median(opt_b), median(opt_y), 250, 'r', 'd', 'filled', ...
        'DisplayName', 'OECD-Median optimal');

xlabel('Final Debt (pp of 2019 GDP, kumulativ Q4.19-Q4.22)');
ylabel('Cumulative Output Gap (pp-Q, sum 13Q)');
title('Per-Country: Beobachtet vs. iLQR-Optimum (V14)');
legend('Location','SW','FontSize',9); grid on;


%% =========================================================================
%  HISTOGRAMS
% =========================================================================
figure('Name','Per-Country Distributions V14','Color','w','Position',[100 100 1100 380]);

subplot(1,3,1);
histogram(gain_y, 15, 'FaceColor', [0.1 0.4 0.8], 'EdgeColor', 'k');
xlabel('Output-Gewinn (pp-Q)'); ylabel('# Laender');
title(sprintf('Output-Gewinn (median %+.2f)', median(gain_y))); grid on;
xline(0, 'k:', 'LineWidth', 1);

subplot(1,3,2);
histogram(diff_b, 15, 'FaceColor', [0.8 0.2 0.2], 'EdgeColor', 'k');
xlabel('Debt-Differenz (pp)'); ylabel('# Laender');
title(sprintf('Debt-Differenz (median %+.2f)', median(diff_b))); grid on;
xline(0, 'k:', 'LineWidth', 1);

subplot(1,3,3);
scatter(diff_b, gain_y, 60, [.4 .4 .4], 'filled');
for i = 1:n_c
    text(diff_b(i)+0.05, gain_y(i), res_obs(i).iso, 'FontSize', 7);
end
xline(0, 'k:'); yline(0, 'k:');
xlabel('Debt-Differenz (pp)'); ylabel('Output-Gewinn (pp-Q)');
title('Trade-off pro Land'); grid on;

fprintf('\n=== PER-COUNTRY iLQR V14 COMPLETE ===\n\n');


%% =========================================================================
%  GAIN vs PANDEMIEDRUCK
% =========================================================================
pressure = zeros(n_c,1);
for i = 1:n_c
    sub_c = T(strcmp(T.Country, countries{i}), :);
    S_c = zeros(1,P.N);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if ~isempty(row), S_c(k) = row.S_mean_tw; end
    end
    pressure(i) = sum(S_c) + abs(eps_per_country(i)) * 10;
end

[gain_sorted, sort_idx] = sort(gain_y, 'descend');
iso_sorted = {res_obs(sort_idx).iso};

figure('Name','Gain vs Druck V14','Color','w','Position',[80 80 1200 500]);

subplot(1,2,1);
bar_colors = repmat([0.1 0.4 0.8], n_c, 1);
bar_colors(gain_sorted < 0, :) = repmat([0.7 0.2 0.2], sum(gain_sorted<0), 1);
b = bar(gain_sorted, 'FaceColor','flat'); b.CData = bar_colors;
set(gca, 'XTick', 1:n_c, 'XTickLabel', iso_sorted, 'XTickLabelRotation', 90, 'FontSize', 8);
ylabel('Output-Gewinn (pp-Q)');
title(sprintf('Verpasste Output-Chance (median %+.2f, %d/%d positiv)', ...
    median(gain_y), sum(gain_y > 0.5), n_c));
yline(0, 'k:'); grid on;

subplot(1,2,2); hold on;
scatter(pressure, gain_y, 80, [.4 .4 .4], 'filled');
for i = 1:n_c
    text(pressure(i)+5, gain_y(i), countries{i}, 'FontSize', 7);
end
valid = ~isnan(pressure) & ~isnan(gain_y(:));
if sum(valid) >= 3
    p_fit = polyfit(pressure(valid), gain_y(valid)', 1);
    p_x = linspace(min(pressure(valid)), max(pressure(valid)), 50);
    plot(p_x, polyval(p_fit, p_x), 'r--', 'LineWidth', 1.5);
    c = corr(pressure(valid), gain_y(valid)');
else
    c = NaN;
end
yline(0, 'k:');
xlabel('Pandemiedruck-Index (sum S + 10|eps_{Q2.20}|)');
ylabel('Output-Gewinn durch Optimum (pp-Q)');
title(sprintf('Gewinn vs Pandemiedruck (corr = %+.2f)', c));
grid on;


%% ########################################################################
%  FUNCTIONS (V14)
%  ########################################################################
function [u_opt, x_opt, J_hist] = ilqr_solve(x0, u_init, P)
    nx = P.nx; nu = P.nu; N = P.N;
    u = u_init;
    [x, J_curr] = forward_cost(x0, u, P);
    J_hist = J_curr;
    max_iter = 50; tol = 1e-4; reg = 1e-6;

    for iter = 1:max_iter
        % Terminal cost gradient (only on b = x(2))
        Vx  = 2 * P.W_b * [0; x(2,end); 0; 0; 0; 0];
        Vxx = zeros(nx); Vxx(2,2) = 2 * P.W_b;
        K_arr = zeros(nu, nx, N); d_arr = zeros(nu, N);

        for k = N:-1:1
            [A, B] = linearize_dyn(x(:,k), u(:,k), k, P);
            disc = P.beta^(k-1);
            xn = x(:,k+1);
            lx_n  = disc * [2*P.w_y*xn(1); 2*P.w_b*xn(2); 0; 0; 0; 0];
            lxx_n = disc * diag([2*P.w_y, 2*P.w_b, 0, 0, 0, 0]);
            lu    = disc * [2*P.r_above*u(1,k); 2*P.r_loans*u(2,k); ...
                            2*P.r_guar*u(3,k);  2*P.r_di*u(4,k)];
            luu   = disc * diag([2*P.r_above, 2*P.r_loans, 2*P.r_guar, 2*P.r_di]);
            Qx  = A' * (Vx + lx_n);
            Qu  = lu + B' * (Vx + lx_n);
            Qxx = A' * (Vxx + lxx_n) * A;
            Quu = luu + B' * (Vxx + lxx_n) * B + reg*eye(nu);
            Qux = B' * (Vxx + lxx_n) * A;
            K = -Quu \ Qux; d = -Quu \ Qu;
            K_arr(:,:,k) = K; d_arr(:,k) = d;
            Vx  = Qx + K' * Quu * d + K' * Qu + Qux' * d;
            Vxx = Qxx + K' * Quu * K + K' * Qux + Qux' * K;
            Vxx = 0.5 * (Vxx + Vxx');
        end

        improved = false;
        for alpha = [1, 0.5, 0.25, 0.1, 0.01]
            x_new = zeros(nx, N+1); u_new = zeros(nu, N);
            x_new(:,1) = x0;
            for k = 1:N
                du = alpha * d_arr(:,k) + K_arr(:,:,k) * (x_new(:,k) - x(:,k));
                u_new(:,k) = max(P.u_lo, min(P.u_hi, u(:,k) + du));
                x_new(:,k+1) = dyn_step(x_new(:,k), u_new(:,k), k, P);
            end
            J_new = compute_cost(x_new, u_new, P);
            if J_new < J_curr - 1e-8
                x = x_new; u = u_new; J_curr = J_new;
                J_hist(end+1) = J_curr;
                improved = true; break;
            end
        end
        if ~improved || (length(J_hist)>=2 && abs(J_hist(end-1)-J_hist(end)) < tol), break; end
    end
    u_opt = u; x_opt = x;
end

% =========================================================================
%  DYNAMICS V14
%   State: x = [y, b, F_above_l1, F_above_l2, F_DI_l1, K_below]
%   Control: u = [F_above, F_loans_adj, F_guar_adj, F_DI]
% =========================================================================
function xn = dyn_step(x, u, k, P)
    y    = x(1);  b    = x(2);
    Fab1 = x(3);  Fab2 = x(4);
    Fdi1 = x(5);  Kbel = x(6);

    Fab_k  = u(1);  Floa_k = u(2);  Fgua_k = u(3);  Fdi_k = u(4);

    Sk     = P.S(k);
    yr_idx = P.year_idx_vec(k);
    eps_k  = 0; if k+1 <= length(P.eps_y_vec), eps_k = P.eps_y_vec(k+1); end

    % K_below grows with current below-flows
    Kbel_new = Kbel + Floa_k + Fgua_k;

    % Output transition (V14)
    y_new = P.mu_y + P.rho_y*y + P.alpha_S*Sk ...
          + P.alpha_above   * Fab2 ...
          + P.alpha_below   * Kbel_new ...
          + P.alpha_DI_lag1 * Fdi1 ...
          + P.alpha_S_DI    * Sk * Fdi1 ...
          + eps_k;

    % Debt transition (disaggregated below)
    b_new = P.mu_b + (1+P.r_int)*b - P.gamma_y*y ...
          + P.kappa_above * Fab_k ...
          + P.kappa_loans * Floa_k ...
          + P.kappa_guar  * Fgua_k ...
          + P.kappa_DI    * Fdi1 ...
          + P.phi_t       * yr_idx;

    % State carry-over
    xn = [y_new; b_new; Fab_k; Fab1; Fdi_k; Kbel_new];
end

function [A, B] = linearize_dyn(x, u, k, P)
    nx = P.nx; nu = P.nu;
    Sk = P.S(k);
    A = zeros(nx); B = zeros(nx, nu);

    % --- Output row (y_new) ---
    A(1,1) = P.rho_y;
    A(1,4) = P.alpha_above;                        % wrt F_above_l2
    A(1,5) = P.alpha_DI_lag1 + P.alpha_S_DI * Sk;  % wrt F_DI_l1
    A(1,6) = P.alpha_below;                        % wrt K_below (entered via Kbel_new = Kbel + ...)
    B(1,2) = P.alpha_below;                        % via Kbel_new
    B(1,3) = P.alpha_below;

    % --- Debt row (b_new) ---
    A(2,1) = -P.gamma_y;
    A(2,2) = (1 + P.r_int);
    A(2,5) = P.kappa_DI;
    B(2,1) = P.kappa_above;
    B(2,2) = P.kappa_loans;
    B(2,3) = P.kappa_guar;

    % --- F_above lag carry-over ---
    B(3,1) = 1;            % x(3)_new = u(1)
    A(4,3) = 1;            % x(4)_new = x(3)

    % --- F_DI lag carry-over ---
    B(5,4) = 1;            % x(5)_new = u(4)

    % --- K_below accumulation ---
    A(6,6) = 1;            % stock persists
    B(6,2) = 1;            % + loans
    B(6,3) = 1;            % + guar
end

function [x, J] = forward_cost(x0, u, P)
    x = zeros(P.nx, P.N+1); x(:,1) = x0;
    for k = 1:P.N, x(:,k+1) = dyn_step(x(:,k), u(:,k), k, P); end
    J = compute_cost(x, u, P);
end

function J = compute_cost(x, u, P)
    J = 0;
    for k = 1:P.N
        disc = P.beta^(k-1);
        J = J + disc * (P.w_y * x(1,k+1)^2 + P.w_b * x(2,k+1)^2 ...
                       + P.r_above * u(1,k)^2 + P.r_loans * u(2,k)^2 ...
                       + P.r_guar  * u(3,k)^2 + P.r_di    * u(4,k)^2);
    end
    J = J + P.W_b * x(2, P.N+1)^2;
end