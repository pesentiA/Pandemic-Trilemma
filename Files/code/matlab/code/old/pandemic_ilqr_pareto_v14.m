%% ========================================================================
%  PANDEMIC TRILEMMA - PARETO FRONTIER V14
%
%  Sweep über (w_y, w_b)-Gewichte; pro Punkt iLQR-Lösung.
%  Ergebnis: Trade-off-Kurve (Cumulative Output Gap, Final Debt).
%
%  Zwei Varianten:
%   (A) OECD-Median: pooled-FE (mu_y, mu_b = 0), Median-S-Pfad
%   (B) Per-Country Frontiers (Anhang)
% =========================================================================
clear; clc; close all;
fprintf('=== PARETO FRONTIER V14 ===\n  %s\n\n', datestr(now));

%% Parameters (V14 frozen)
P.rho_y         =  0.231;   P.alpha_S       = -0.0952;
P.alpha_above   =  0.544;   P.alpha_below   =  0.131;
P.alpha_DI_lag1 =  1.470;   P.alpha_S_DI    = -0.0406;
P.takeup_loans  =  0.60;    P.takeup_guar   =  0.35;

P.r_int       =  0.001;     P.gamma_y     =  0.194;
P.kappa_above =  0.442;     P.kappa_loans =  0.601;
P.kappa_guar  =  0.116;     P.kappa_DI    =  0.405;
P.phi_t       =  0;

% Technische Cost-Penalties (fixed, klein)
P.r_above = 1;  P.r_loans = 1;  P.r_guar = 5;  P.r_di = 1;
P.beta    = 0.99;

P.N  = 13;   P.nx = 6;   P.nu = 4;
P.u_lo = [0; 0; 0; 0];
P.u_hi = [8; 4; 2; 6];

year_idx_raw = [0,1,1,1,1,2,2,2,2,3,3,3,3];
P.year_idx_vec = year_idx_raw - mean(year_idx_raw);
P.eps_y_vec = zeros(1, P.N+1);
P.eps_y_vec(4) = -5.40;

%% Load CSV: OECD-Median-Pfade
T = readtable('country_data_for_matlab.csv');
qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

% Median-S-Pfad
S_all = nan(n_c, P.N);
y_all = nan(n_c, P.N);
for i = 1:n_c
    sub_c = T(strcmp(T.Country, countries{i}), :);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        S_all(i,k) = row.S_mean_tw;
        y_all(i,k) = row.y_t_pct;
    end
end
S_med = median(S_all, 1, 'omitnan');
y0    = median(y_all(:,1), 'omitnan');

P.S    = S_med;
P.mu_y = 0;  % Median-Land, FE = 0
P.mu_b = 0;

%% Variante A: Pareto-Sweep auf OECD-Median
fprintf('--- Variante A: Pareto-Sweep OECD-Median ---\n');

% Gewichtungs-Grid: w_y fix, w_b variabel (log-spaced)
w_b_grid = [1, 2, 5, 10, 20, 30, 50, 80, 120, 200, 300, 500, 1000, 2000];
n_pts = length(w_b_grid);
P.w_y = 100;
P.W_b = 0;   % kein extra Terminalstrafterm, damit Frontier sauber

results_A = zeros(n_pts, 6);  % [w_b, cum_y, fin_b, cum_above, cum_loans+guar, cum_DI]

x0 = [y0; 0; 0; 0; 0; 0];

for p = 1:n_pts
    P.w_b = w_b_grid(p);
    
    % Warm start: minimal F-Pfad
    u_init = 0.5 * (P.u_lo + P.u_hi);
    u_init = repmat(u_init, 1, P.N);
    
    [u_opt, x_opt, ~] = ilqr_solve(x0, u_init, P);
    
    results_A(p, 1) = P.w_b;
    results_A(p, 2) = sum(x_opt(1, 2:end));        % cum_y
    results_A(p, 3) = x_opt(2, end);                % fin_b
    results_A(p, 4) = sum(u_opt(1,:));              % above
    results_A(p, 5) = sum(u_opt(2,:) + u_opt(3,:)); % below (loans+guar)
    results_A(p, 6) = sum(u_opt(4,:));              % DI
    
    fprintf('  w_b=%5.0f:  cum_y=%+7.2f  fin_b=%+6.2f  | above=%5.2f  below=%5.2f  DI=%4.2f\n', ...
        w_b_grid(p), results_A(p,2), results_A(p,3), ...
        results_A(p,4), results_A(p,5), results_A(p,6));
end

%% Variante B: Per-Country Frontiers (subset für Visualisierung)
fprintf('\n--- Variante B: Per-Country Frontiers ---\n');

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

% Subset für Per-Country Frontiers
subset_iso = {'DEU','ESP','GBR','USA','SWE','JPN'};
w_b_grid_pc = [5, 20, 50, 150, 500];
n_pc = length(w_b_grid_pc);
results_B = struct();

for s = 1:length(subset_iso)
    iso = subset_iso{s};
    sub_c = T(strcmp(T.Country, iso), :);
    S_c = zeros(1,P.N);  y_c = zeros(1,P.N);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        S_c(k) = row.S_mean_tw;
        y_c(k) = row.y_t_pct;
    end
    
    P.S    = S_c;
    P.mu_y = mu_y_map(iso);
    P.mu_b = mu_b_map(iso);
    x0_c   = [y_c(1); 0; 0; 0; 0; 0];
    
    pts = zeros(n_pc, 2);
    for p = 1:n_pc
        P.w_b = w_b_grid_pc(p);
        u_init = 0.5 * (P.u_lo + P.u_hi);
        u_init = repmat(u_init, 1, P.N);
        [u_opt, x_opt, ~] = ilqr_solve(x0_c, u_init, P);
        pts(p, 1) = x_opt(2, end);
        pts(p, 2) = sum(x_opt(1, 2:end));
    end
    results_B.(iso) = pts;
    fprintf('  %s: frontier computed (%d points)\n', iso, n_pc);
end


%% =========================================================================
%  PLOT: Pareto Frontier (OECD-Median)
% =========================================================================
figure('Name','Pareto Frontier V14','Color','w','Position',[80 80 1100 700]);
subplot(1,2,1); hold on;

plot(results_A(:,3), results_A(:,2), 'b-o', 'LineWidth', 2, 'MarkerSize', 8, ...
     'MarkerFaceColor', 'b');
for p = 1:n_pts
    text(results_A(p,3)+0.2, results_A(p,2), sprintf('w_b=%.0f', w_b_grid(p)), ...
        'FontSize', 7, 'Color', [.2 .2 .5]);
end

% OECD-Median observed point
obs_b_all = nan(n_c, 1);
obs_y_all = nan(n_c, 1);
for i = 1:n_c
    sub_c = T(strcmp(T.Country, countries{i}), :);
    bv = 0; yv = 0;
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            bv = bv + row.debt_dR;
        end
        yv = yv + row.y_t_pct;
    end
    obs_b_all(i) = bv;  obs_y_all(i) = yv;
end
plot(median(obs_b_all,'omitnan'), median(obs_y_all,'omitnan'), 'kp', ...
     'MarkerSize', 18, 'MarkerFaceColor', 'k', 'DisplayName','OECD-Median beobachtet');

xlabel('Final Debt (pp of 2019 GDP)');
ylabel('Cumulative Output Gap (pp-Q, 13Q)');
title('Pareto Frontier V14 - OECD-Median');
grid on;
legend({'Frontier (Weight-Sweep)','OECD-Median beobachtet'},'Location','SE');

% Fiscal mix entlang Frontier
subplot(1,2,2); hold on;
plot(results_A(:,1), results_A(:,4), 'b-o', 'LineWidth', 1.5, 'DisplayName','F^{above}');
plot(results_A(:,1), results_A(:,5), 'g-s', 'LineWidth', 1.5, 'DisplayName','F^{below} (loans+guar)');
plot(results_A(:,1), results_A(:,6), 'm-^', 'LineWidth', 1.5, 'DisplayName','F^{DI}');
set(gca, 'XScale', 'log');
xlabel('w_b (Debt-Aversion)');
ylabel('Cumulative Fiscal Deployment (pp GDP)');
title('Fiscal Mix entlang Frontier');
legend('Location','NE'); grid on;

sgtitle('V14 Pareto Frontier: Output-Debt Trade-off','FontWeight','bold');


%% =========================================================================
%  PLOT: Per-Country Frontiers
% =========================================================================
figure('Name','Per-Country Frontiers V14','Color','w','Position',[100 100 900 600]);
hold on;
colors = lines(length(subset_iso));
for s = 1:length(subset_iso)
    pts = results_B.(subset_iso{s});
    plot(pts(:,1), pts(:,2), '-o', 'Color', colors(s,:), 'LineWidth', 1.8, ...
         'MarkerFaceColor', colors(s,:), 'MarkerSize', 7, ...
         'DisplayName', subset_iso{s});
    
    % Observed point
    iso = subset_iso{s};
    obs_b_c = 0; obs_y_c = 0;
    sub_c = T(strcmp(T.Country, iso), :);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        if ismember('debt_dR', T.Properties.VariableNames) && ~ismissing(row.debt_dR)
            obs_b_c = obs_b_c + row.debt_dR;
        end
        obs_y_c = obs_y_c + row.y_t_pct;
    end
    scatter(obs_b_c, obs_y_c, 120, colors(s,:), 'd', 'filled', ...
            'MarkerEdgeColor', 'k', 'LineWidth', 1, 'HandleVisibility','off');
    text(obs_b_c+0.3, obs_y_c, [iso ' (obs)'], 'FontSize', 8, 'Color', colors(s,:));
end
xlabel('Final Debt (pp of 2019 GDP)');
ylabel('Cumulative Output Gap (pp-Q, 13Q)');
title('Per-Country Pareto Frontiers V14 (Diamond = Observed)');
legend('Location','SW'); grid on;

fprintf('\n=== PARETO FRONTIER V14 COMPLETE ===\n');


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

function xn = dyn_step(x, u, k, P)
    y    = x(1);  b    = x(2);
    Fab1 = x(3);  Fab2 = x(4);
    Fdi1 = x(5);  Kbel = x(6);

    Fab_k  = u(1);  Floa_k = u(2);  Fgua_k = u(3);  Fdi_k = u(4);

    Sk     = P.S(k);
    yr_idx = P.year_idx_vec(k);
    eps_k  = 0; if k+1 <= length(P.eps_y_vec), eps_k = P.eps_y_vec(k+1); end

    Kbel_new = Kbel + Floa_k + Fgua_k;

    y_new = P.mu_y + P.rho_y*y + P.alpha_S*Sk ...
          + P.alpha_above   * Fab2 ...
          + P.alpha_below   * Kbel_new ...
          + P.alpha_DI_lag1 * Fdi1 ...
          + P.alpha_S_DI    * Sk * Fdi1 ...
          + eps_k;

    b_new = P.mu_b + (1+P.r_int)*b - P.gamma_y*y ...
          + P.kappa_above * Fab_k ...
          + P.kappa_loans * Floa_k ...
          + P.kappa_guar  * Fgua_k ...
          + P.kappa_DI    * Fdi1 ...
          + P.phi_t       * yr_idx;

    xn = [y_new; b_new; Fab_k; Fab1; Fdi_k; Kbel_new];
end

function [A, B] = linearize_dyn(x, u, k, P)
    nx = P.nx; nu = P.nu;
    Sk = P.S(k);
    A = zeros(nx); B = zeros(nx, nu);

    A(1,1) = P.rho_y;
    A(1,4) = P.alpha_above;
    A(1,5) = P.alpha_DI_lag1 + P.alpha_S_DI * Sk;
    A(1,6) = P.alpha_below;
    B(1,2) = P.alpha_below;
    B(1,3) = P.alpha_below;

    A(2,1) = -P.gamma_y;
    A(2,2) = (1 + P.r_int);
    A(2,5) = P.kappa_DI;
    B(2,1) = P.kappa_above;
    B(2,2) = P.kappa_loans;
    B(2,3) = P.kappa_guar;

    B(3,1) = 1;
    A(4,3) = 1;
    B(5,4) = 1;
    A(6,6) = 1;
    B(6,2) = 1;
    B(6,3) = 1;
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

%%
