%% ========================================================================
%  PANDEMIC TRILEMMA - PER-COUNTRY iLQR vs OBSERVED
%
%  Pro Land: iLQR mit countryspezifischer Country-FE, beobachteter S-Pfad,
%  und V11-Default-Weights. Vergleich Optimum vs. tatsaechlich beobachtete
%  Trajektorie aus CSV.
% =========================================================================
clear; clc; close all;
fprintf('=== PER-COUNTRY iLQR V11 ===\n  %s\n\n', datestr(now));

%% Parameters (V11 frozen, identisch)
P.rho_y         =  0.217;  P.alpha_S       = -0.098;
P.alpha_CP_cum  =  0.103;  P.alpha_CP_lag2 =  0.108;
P.alpha_DI_lag1 =  1.417;  P.alpha_S_DI    = -0.0396;
P.r_int       =  0.001;    P.gamma_y     =  0.188;
P.kappa_above =  0.434;    P.kappa_loans =  0.365;
P.kappa_guar  =  0.107;    P.kappa_F_DI  =  0.427;
P.phi_t       = -0.216;
P.cp_above_share = 0.60;   P.cp_loans_share = 0.30;   P.cp_guar_share = 0.10;
P.r_cp = 3;  P.r_di = 5;  P.beta = 0.99;
P.w_y = 100; P.w_b = 30;  P.W_b = 150;        % V11-Default
P.N  = 13;   P.nx = 6;   P.nu = 2;
P.u_lo = [0; 0];  P.u_hi = [20; 10];

year_idx_raw = [0,1,1,1,1,2,2,2,2,3,3,3,3];
P.year_idx_vec = year_idx_raw - mean(year_idx_raw);
P.eps_y_vec = zeros(1, P.N+1); P.eps_y_vec(4) = -5.54;

%% Country FE values from V11 estimation
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};
cfe_y_val = [+0.6427,-1.3381,-0.2827,-0.4194,+0.9259,+0.9304,+1.9562,+0.0634, ...
             -4.3329,-2.7234,-0.5551,-5.5692,-2.3823,-1.7928,-2.6562,-4.3291, ...
             -0.0135,-2.2699,+8.3140,-5.1074,+2.2691,-1.4562,-2.5599,+0.4606, ...
             +0.5079,+2.1832,-1.1012,-3.2366,+0.7695,+1.0932,-1.5983,-0.9368, ...
             -3.2343,-0.3393,-2.4286,+0.8199,+3.7357,+0.8253];
cfe_b_val = [-0.6462,-0.5190,-0.5147,-0.1395,-0.3089,-0.9431,+0.6842,+0.0991, ...
             -0.9315,-0.9908,-1.4190,-1.1902,-0.5864,-0.5923,-0.8557,-0.9631, ...
             -0.7381,-0.9150,+0.6430,-1.0453,-0.0485,-0.7098,-0.8563,-0.4437, ...
             -0.5257,+0.2124,+0.0169,-1.4596,-0.3687,-0.0728,+0.3738,-0.8068, ...
             -1.8202,-0.2072,-0.8612,-0.4815,+0.1780,+0.4707];
mu_y_map = containers.Map(cfe_iso, cfe_y_val);
mu_b_map = containers.Map(cfe_iso, cfe_b_val);

%% Load CSV
T = readtable('country_data_for_matlab.csv');
qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

%% Per-country Q2.20 residuals from V11 estimation
% Aus residuals(main_test) bei Quarter == "Q2.2020", regression-konsistent
% mit V11 Country-FE only spec
eps_Q2_20_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
                 'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
                 'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
                 'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
                 'PRT','SVK','SVN','SWE','TUR','USA'};
eps_Q2_20_val = [-3.93, -8.67, -6.37, -7.83, -4.81, -9.99, -11.00, -3.57, ...
                 -5.01, -5.92, -1.93, -9.30, -2.58, -1.89, -8.35, -12.50, ...
                 -9.88, -8.85, -1.06, -4.65, -0.71, -10.90, -6.72, -1.69, ...
                  0.00, -5.30, -7.35, -7.81, -5.25, -2.38, -5.78, -1.84, ...
                 -9.61, -4.91, -6.85, -4.69, -10.30, -4.79];
eps_Q2_20_map = containers.Map(eps_Q2_20_iso, eps_Q2_20_val);

%% Loop: per-country observed + optimum
fprintf('--- Running iLQR for %d countries ---\n', n_c);
res_obs = struct();
res_opt = struct();
eps_per_country = zeros(n_c, 1);

for i = 1:n_c
    iso = countries{i};
    sub_c = T(strcmp(T.Country, iso), :);

    S_c   = zeros(1,P.N);  FCP_c = zeros(1,P.N);
    FDI_c = zeros(1,P.N);  y_c   = zeros(1,P.N);  b_c = zeros(1,P.N);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if isempty(row), continue; end
        S_c(k)   = row.S_mean_tw;
        FCP_c(k) = row.F_CP;
        FDI_c(k) = row.F_DI;
        y_c(k)   = row.y_t_pct;
        if ismember('debt_dR', T.Properties.VariableNames), b_c(k) = row.debt_dR; end
    end

    % Per-country params
    P.S = S_c;
    P.mu_y = 0; if isKey(mu_y_map, iso), P.mu_y = mu_y_map(iso); end
    P.mu_b = 0; if isKey(mu_b_map, iso), P.mu_b = mu_b_map(iso); end

    % Per-country Q2.20 Schock aus V11-Regression-Residuen
    P.eps_y_vec = zeros(1, P.N+1);
    eps_Q2_20 = -5.54;   % Fallback: Median falls Land nicht in Map
    if isKey(eps_Q2_20_map, iso), eps_Q2_20 = eps_Q2_20_map(iso); end
    P.eps_y_vec(4) = eps_Q2_20;
    eps_per_country(i) = eps_Q2_20;

    x0 = [y_c(1); 0; 0; 0; 0; 0];

    % iLQR mit warm start = observed F UND country-specific eps
    u_init = max(P.u_lo, min(P.u_hi, [FCP_c; FDI_c]));
    [u_opt, x_opt, ~] = ilqr_solve(x0, u_init, P);

    % Observed (REAL CSV) endpoints
    res_obs(i).iso  = iso;
    res_obs(i).cum_y = sum(y_c);
    res_obs(i).fin_b = sum(b_c);
    res_obs(i).cum_FCP = sum(FCP_c);
    res_obs(i).cum_FDI = sum(FDI_c);

    % Optimal (model)
    res_opt(i).iso  = iso;
    res_opt(i).cum_y = sum(x_opt(1, 2:end));
    res_opt(i).fin_b = x_opt(2, end);
    res_opt(i).cum_FCP = sum(u_opt(1,:));
    res_opt(i).cum_FDI = sum(u_opt(2,:));
end

% Print eps statistics
fprintf('\n  Per-country Q2.20 eps (V11-Regression-Residuen):\n');
fprintf('    Median: %+.2f pp,  Mean: %+.2f pp,  Range: [%+.2f, %+.2f]\n', ...
    median(eps_per_country), mean(eps_per_country), ...
    min(eps_per_country), max(eps_per_country));

% Aggregate stats
gain_y = [res_opt.cum_y] - [res_obs.cum_y];
diff_b = [res_opt.fin_b] - [res_obs.fin_b];
diff_FCP = [res_opt.cum_FCP] - [res_obs.cum_FCP];
diff_FDI = [res_opt.cum_FDI] - [res_obs.cum_FDI];

fprintf('\n  Output gain (Optimal - Observed, sum 13Q):\n');
fprintf('    Median: %+.2f pp-Q,  Mean: %+.2f pp-Q\n', median(gain_y), mean(gain_y));
fprintf('    Improved: %d / %d countries\n', sum(gain_y > 0.5), n_c);
fprintf('  Debt diff (Optimal - Observed):\n');
fprintf('    Median: %+.2f pp,  Mean: %+.2f pp\n', median(diff_b), mean(diff_b));
fprintf('  F_CP diff (Optimal - Observed): median %+.2f pp\n', median(diff_FCP));
fprintf('  F_DI diff (Optimal - Observed): median %+.2f pp\n', median(diff_FDI));
fprintf('  Per-country Q2.20 shocks:\n');
fprintf('    Median: %+.2f,  Mean: %+.2f,  Range: [%+.2f, %+.2f]\n\n', ...
    median(eps_per_country), mean(eps_per_country), ...
    min(eps_per_country), max(eps_per_country));

% Top 5 gainers
[~, sort_g] = sort(gain_y, 'descend');
fprintf('  Top 5 Output-Gewinner:\n');
fprintf('    %5s %8s %8s %8s %8s %8s\n', 'ISO','obs_y','opt_y','gain_y','obs_b','opt_b');
for j = 1:5
    i = sort_g(j);
    fprintf('    %5s %+8.2f %+8.2f %+8.2f %+8.2f %+8.2f\n', ...
        res_obs(i).iso, res_obs(i).cum_y, res_opt(i).cum_y, gain_y(i), ...
        res_obs(i).fin_b, res_opt(i).fin_b);
end


%% =========================================================================
%  PLOT: Observed vs Optimal pro Land
% =========================================================================
figure('Name','Per-Country Pareto','Color','w','Position',[80 80 1100 700]);
hold on;

obs_b = [res_obs.fin_b];  obs_y = [res_obs.cum_y];
opt_b = [res_opt.fin_b];  opt_y = [res_opt.cum_y];

% Connection lines (observed -> optimal)
for i = 1:n_c
    plot([obs_b(i), opt_b(i)], [obs_y(i), opt_y(i)], '-', ...
         'Color', [.7 .7 .7 .5], 'LineWidth', 0.7, 'HandleVisibility','off');
end

% Observed cloud
scatter(obs_b, obs_y, 80, [.4 .4 .4], 'filled', 'MarkerFaceAlpha', 0.6, ...
        'DisplayName','Beobachtet (CSV)');

% Optimal cloud
scatter(opt_b, opt_y, 80, [0.1 0.4 0.8], 'filled', 'MarkerFaceAlpha', 0.7, ...
        'DisplayName','Optimal (V11 iLQR)');

% Country labels (am optimalen Punkt)
for i = 1:n_c
    text(opt_b(i)+0.2, opt_y(i), res_opt(i).iso, 'FontSize', 7, 'Color', [.2 .2 .5]);
end

% Median markers
scatter(median(obs_b), median(obs_y), 250, 'k', 's', 'filled', ...
        'DisplayName', 'OECD-Median beobachtet');
scatter(median(opt_b), median(opt_y), 250, 'r', 'd', 'filled', ...
        'DisplayName', 'OECD-Median optimal');

xlabel('Final Debt (pp of 2019 GDP, kumulativ Q4.19-Q4.22)');
ylabel('Cumulative Output Gap (pp-Q, sum 13Q)');
title('Per-Country: Beobachtet vs. iLQR-Optimum (V11-Default Weights)');
legend('Location','SW','FontSize',9); grid on;


%% =========================================================================
%  HISTOGRAMS: Output-Gewinn und Debt-Differenz
% =========================================================================
figure('Name','Per-Country Distributions','Color','w','Position',[100 100 1100 380]);

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

fprintf('\n=== PER-COUNTRY iLQR COMPLETE ===\n\n');

%% =========================================================================
%  GAIN vs PANDEMIEDRUCK
% =========================================================================
% Druck-Index: cum |S| + |eps_Q2.20| (kombiniert Lockdown + Schock)
pressure = zeros(n_c,1);
for i = 1:n_c
    sub_c = T(strcmp(T.Country, countries{i}), :);
    S_c = zeros(1,P.N);
    for k = 1:P.N
        row = sub_c(strcmp(sub_c.Quarter, qord{k}), :);
        if ~isempty(row), S_c(k) = row.S_mean_tw; end
    end
    pressure(i) = sum(S_c) + abs(eps_per_country(i)) * 10;  % skaliert
end

[gain_sorted, sort_idx] = sort(gain_y, 'descend');
iso_sorted = {res_obs(sort_idx).iso};
press_sorted = pressure(sort_idx);

figure('Name','Gain vs Druck','Color','w','Position',[80 80 1200 500]);

% --- Panel 1: Bar chart sorted ---
subplot(1,2,1);
bar_colors = repmat([0.1 0.4 0.8], n_c, 1);
bar_colors(gain_sorted < 0, :) = repmat([0.7 0.2 0.2], sum(gain_sorted<0), 1);
b = bar(gain_sorted, 'FaceColor','flat'); b.CData = bar_colors;
set(gca, 'XTick', 1:n_c, 'XTickLabel', iso_sorted, 'XTickLabelRotation', 90, 'FontSize', 8);
ylabel('Output-Gewinn (pp-Q)');
title(sprintf('Verpasste Output-Chance pro Land (median %+.2f, %d/%d positiv)', ...
    median(gain_y), sum(gain_y > 0.5), n_c));
yline(0, 'k:'); grid on;

% --- Panel 2: Gain vs Pandemiedruck ---
subplot(1,2,2); hold on;
scatter(pressure, gain_y, 80, [.4 .4 .4], 'filled');
for i = 1:n_c
    text(pressure(i)+5, gain_y(i), countries{i}, 'FontSize', 7);
end
% Trendlinie
p_fit = polyfit(pressure, gain_y, 1);
p_x = linspace(min(pressure), max(pressure), 50);
plot(p_x, polyval(p_fit, p_x), 'r--', 'LineWidth', 1.5);
yline(0, 'k:');
xlabel('Pandemiedruck-Index (sum S + 10|eps_{Q2.20}|)');
ylabel('Output-Gewinn durch Optimum (pp-Q)');
title(sprintf('Gewinn vs Pandemiedruck (corr = %+.2f)', corr(pressure, gain_y)));
grid on;


%% ########################################################################
%  FUNCTIONS (identisch zu pandemic_ilqr_v11.m)
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
            lx_n = disc * [2*P.w_y*xn(1); 2*P.w_b*xn(2); 0; 0; 0; 0];
            lxx_n = disc * diag([2*P.w_y, 2*P.w_b, 0, 0, 0, 0]);
            lu = disc * [2*P.r_cp*u(1,k); 2*P.r_di*u(2,k)];
            luu = disc * diag([2*P.r_cp, 2*P.r_di]);
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
    y = x(1); b = x(2);
    FCP_l1 = x(3); FCP_l2 = x(4); FDI_l1 = x(5); FCP_cum = x(6);
    FCP_k = u(1); FDI_k = u(2);
    Sk = P.S(k); yr_idx = P.year_idx_vec(k);
    eps_k = 0; if k+1 <= length(P.eps_y_vec), eps_k = P.eps_y_vec(k+1); end
    FCP_cum_new = FCP_cum + FCP_k;
    y_new = P.mu_y + P.rho_y*y + P.alpha_S*Sk + P.alpha_CP_cum*FCP_cum_new ...
          + P.alpha_CP_lag2*FCP_l2 + P.alpha_DI_lag1*FDI_l1 ...
          + P.alpha_S_DI*Sk*FDI_l1 + eps_k;
    fa = P.cp_above_share*FCP_k; fl = P.cp_loans_share*FCP_k; fg = P.cp_guar_share*FCP_k;
    b_new = P.mu_b + (1+P.r_int)*b - P.gamma_y*y ...
          + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
          + P.kappa_F_DI*FDI_l1 + P.phi_t*yr_idx;
    xn = [y_new; b_new; FCP_k; FCP_l1; FDI_k; FCP_cum_new];
end

function [A, B] = linearize_dyn(x, u, k, P)
    nx = P.nx; Sk = P.S(k);
    A = zeros(nx); B = zeros(nx, P.nu);
    A(1,1) = P.rho_y; A(1,4) = P.alpha_CP_lag2;
    A(1,5) = P.alpha_DI_lag1 + P.alpha_S_DI*Sk; A(1,6) = P.alpha_CP_cum;
    B(1,1) = P.alpha_CP_cum; B(1,2) = 0;
    A(2,1) = -P.gamma_y; A(2,2) = (1 + P.r_int); A(2,5) = P.kappa_F_DI;
    B(2,1) = P.kappa_above*P.cp_above_share + P.kappa_loans*P.cp_loans_share + P.kappa_guar*P.cp_guar_share;
    B(2,2) = 0; B(3,1) = 1; A(4,3) = 1; B(5,2) = 1; A(6,6) = 1; B(6,1) = 1;
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
        J = J + disc * (P.w_y*x(1,k+1)^2 + P.w_b*x(2,k+1)^2 ...
                       + P.r_cp*u(1,k)^2 + P.r_di*u(2,k)^2);
    end
    J = J + P.W_b * x(2, P.N+1)^2;
end