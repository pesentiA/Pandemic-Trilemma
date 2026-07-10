%% ========================================================================
%  PANDEMIC TRILEMMA - NO-FISCAL COUNTERFACTUAL V14
%
%  Pro Land: Forward-Roll mit allen Fiscal-Hebeln = 0, beobachtetem S-Pfad
%  und beobachtetem Q2.20-Schock. Vergleich gegen tatsächliche Outcomes.
%
%  Beantwortet: "Was wäre passiert ohne fiskalische Intervention?"
% =========================================================================
clear; clc; close all;
fprintf('=== NO-FISCAL COUNTERFACTUAL V14 ===\n  %s\n\n', datestr(now));

%% Parameters (V14 frozen)
P.rho_y         =  0.231;   P.alpha_S       = -0.0952;
P.alpha_above   =  0.544;   P.alpha_below   =  0.131;
P.alpha_DI_lag1 =  1.470;   P.alpha_S_DI    = -0.0406;
P.takeup_loans  =  0.60;    P.takeup_guar   =  0.35;

P.r_int       =  0.001;     P.gamma_y     =  0.194;
P.kappa_above =  0.442;     P.kappa_loans =  0.601;
P.kappa_guar  =  0.116;     P.kappa_DI    =  0.405;
P.phi_t       =  0;

P.N  = 13;   P.nx = 6;   P.nu = 4;

year_idx_raw = [0,1,1,1,1,2,2,2,2,3,3,3,3];
P.year_idx_vec = year_idx_raw - mean(year_idx_raw);

%% Country FE
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

% Per-country Q2.20 eps
eps_Q2_20_val = [-3.93, -8.67, -6.37, -7.83, -4.81, -9.99, -11.00, -3.57, ...
                 -5.01, -5.92, -1.93, -9.30, -2.58, -1.89, -8.35, -12.50, ...
                 -9.88, -8.85, -1.06, -4.65, -0.71, -10.90, -6.72, -1.69, ...
                  0.00, -5.30, -7.35, -7.81, -5.25, -2.38, -5.78, -1.84, ...
                 -9.61, -4.91, -6.85, -4.69, -10.30, -4.79];
eps_Q2_20_map = containers.Map(cfe_iso, eps_Q2_20_val);

%% Load CSV
T = readtable('country_data_for_matlab.csv');
qord = {'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl = {'Q4.19','Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22','Q3.22','Q4.22'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

%% Loop: Counterfactual pro Land
fprintf('--- Running no-fiscal counterfactual for %d countries ---\n', n_c);
res = struct();

for i = 1:n_c
    iso = countries{i};
    sub_c = T(strcmp(T.Country, iso), :);

    S_c     = zeros(1,P.N);
    FCPab_c = zeros(1,P.N);
    Floa_c  = zeros(1,P.N);
    Fgua_c  = zeros(1,P.N);
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

    x0 = [y_c(1); 0; 0; 0; 0; 0];

    % (1) Observed forward roll (uses real F-paths)
    u_obs = [FCPab_c; Floa_c; Fgua_c; FDI_c];
    xs_obs = forward_roll_sim(x0, u_obs, P);

    % (2) No-fiscal counterfactual (u = 0)
    u_zero = zeros(P.nu, P.N);
    xs_nof = forward_roll_sim(x0, u_zero, P);

    res(i).iso        = iso;
    res(i).y_obs      = y_c;
    res(i).y_model    = xs_obs(1, 2:end);
    res(i).y_nofisc   = xs_nof(1, 2:end);
    res(i).b_obs      = cumsum(b_c);
    res(i).b_model    = xs_obs(2, 2:end);
    res(i).b_nofisc   = xs_nof(2, 2:end);

    res(i).cum_y_obs    = sum(y_c);
    res(i).cum_y_model  = sum(xs_obs(1, 2:end));
    res(i).cum_y_nofisc = sum(xs_nof(1, 2:end));

    res(i).fin_b_obs    = sum(b_c);
    res(i).fin_b_model  = xs_obs(2, end);
    res(i).fin_b_nofisc = xs_nof(2, end);

    res(i).fiscal_gain  = res(i).cum_y_model - res(i).cum_y_nofisc;
    res(i).fiscal_cost  = res(i).fin_b_model - res(i).fin_b_nofisc;
end

%% Aggregate statistics
fiscal_gain = [res.fiscal_gain];
fiscal_cost = [res.fiscal_cost];

fprintf('\n--- Aggregate statistics ---\n');
fprintf('  Fiscal Gain (Output Model - NoFiscal, sum 13Q):\n');
fprintf('    Median: %+.2f pp-Q,  Mean: %+.2f pp-Q\n', median(fiscal_gain), mean(fiscal_gain));
fprintf('    Helped: %d / %d countries (gain > 0)\n', sum(fiscal_gain > 0), n_c);

fprintf('\n  Fiscal Cost (Debt Model - NoFiscal):\n');
fprintf('    Median: %+.2f pp,  Mean: %+.2f pp\n', median(fiscal_cost), mean(fiscal_cost));

fprintf('\n  Effective Multiplier (Gain per pp Debt):\n');
mult = fiscal_gain ./ max(fiscal_cost, 0.01);
fprintf('    Median: %+.2f pp-Q output / pp debt\n', median(mult(fiscal_cost > 0)));

% OECD median trajectories
y_obs_mat    = reshape([res.y_obs],    P.N, n_c)';
y_model_mat  = reshape([res.y_model],  P.N, n_c)';
y_nofisc_mat = reshape([res.y_nofisc], P.N, n_c)';

fprintf('\n  OECD Median Trajectory:\n');
fprintf('  %8s %10s %10s %10s\n', 'Quarter', 'Observed', 'Model', 'NoFiscal');
for k = 1:P.N
    fprintf('  %8s %+10.2f %+10.2f %+10.2f\n', qlbl{k}, ...
        median(y_obs_mat(:,k),    'omitnan'), ...
        median(y_model_mat(:,k),  'omitnan'), ...
        median(y_nofisc_mat(:,k), 'omitnan'));
end

% Top counterfactual losses (countries where fiscal helped most)
[~, sort_idx] = sort(fiscal_gain, 'descend');
fprintf('\n  Top 5: Länder mit grösstem Fiscal-Gain:\n');
fprintf('    %5s %10s %10s %10s\n', 'ISO', 'Obs cum y', 'NoFisc cum y', 'Gain');
for j = 1:5
    i = sort_idx(j);
    fprintf('    %5s %+10.2f %+10.2f %+10.2f\n', ...
        res(i).iso, res(i).cum_y_obs, res(i).cum_y_nofisc, fiscal_gain(i));
end

fprintf('\n  Bottom 5: Länder wo Fiscal weniger half:\n');
for j = 0:4
    i = sort_idx(n_c-j);
    fprintf('    %5s %+10.2f %+10.2f %+10.2f\n', ...
        res(i).iso, res(i).cum_y_obs, res(i).cum_y_nofisc, fiscal_gain(i));
end


%% PLOT 1: OECD Median Trajectory Comparison
figure('Name','No-Fiscal Counterfactual V14','Color','w','Position',[80 80 1200 500]);

subplot(1,2,1); hold on;
plot(1:P.N, median(y_obs_mat,'omitnan'),    'k--s', 'LineWidth', 2, 'MarkerSize', 6, ...
     'DisplayName','Observed');
plot(1:P.N, median(y_model_mat,'omitnan'),  'b-o',  'LineWidth', 2, 'MarkerSize', 6, ...
     'DisplayName','Model (with fiscal)');
plot(1:P.N, median(y_nofisc_mat,'omitnan'), 'r-^',  'LineWidth', 2, 'MarkerSize', 6, ...
     'DisplayName','No-Fiscal Counterfactual');
yline(0, 'k:'); grid on;
set(gca, 'XTick', 1:P.N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45);
ylabel('Output Gap (pp of potential GDP)');
title('OECD-Median: Output Gap Trajectories');
legend('Location','SE');

subplot(1,2,2);
histogram(fiscal_gain, 15, 'FaceColor', [0.1 0.6 0.2], 'EdgeColor', 'k');
xline(median(fiscal_gain), 'r--', 'LineWidth', 2, 'Label', sprintf('median %+.1f', median(fiscal_gain)));
xline(0, 'k:', 'LineWidth', 1);
xlabel('Fiscal Gain (pp-Q, sum 13Q)'); ylabel('# Countries');
title(sprintf('Per-Country Fiscal Gain (%d / %d positive)', sum(fiscal_gain>0), n_c));
grid on;

sgtitle('V14 No-Fiscal Counterfactual','FontWeight','bold');


%% PLOT 2: Scatter Gain vs Cost
figure('Name','Fiscal Gain vs Cost','Color','w','Position',[100 100 800 600]);
hold on;
scatter(fiscal_cost, fiscal_gain, 100, [.3 .5 .8], 'filled', 'MarkerFaceAlpha', 0.7);
for i = 1:n_c
    text(fiscal_cost(i)+0.2, fiscal_gain(i), res(i).iso, 'FontSize', 7);
end
% Efficient frontier line
xline(0, 'k:'); yline(0, 'k:');
xlabel('Fiscal Cost (pp debt added)');
ylabel('Fiscal Gain (pp-Q output saved)');
title('Per-Country: Fiscal Effectiveness V14');
grid on;


fprintf('\n=== NO-FISCAL COUNTERFACTUAL COMPLETE ===\n\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################
function xs = forward_roll_sim(x0, u, P)
    xs = zeros(P.nx, P.N+1);
    xs(:,1) = x0;
    for k = 1:P.N
        xs(:,k+1) = dyn_step(xs(:,k), u(:,k), k, P);
    end
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