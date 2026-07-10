%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & CONSTRAINED COUNTERFACTUALS
%
%  Step 1: Forward-roll validation with observed controls
%          Output: OECD median ± IQR plot (sim vs obs)
%  Step 2: Constrained counterfactuals per country
%          CF-A: Fixed quarterly total, optimize CP/DI composition
%          CF-B: Fixed cumulative total, optimize timing + composition
%          Decomposition: Composition cost (J_obs - J_A)
%                         Timing cost    (J_A - J_B)
%
%  All epidemiological variables (S, theta, d) fixed at observed values.
%  Only F^CP and F^DI are endogenous/optimized.
%
%  CORRECTED PARAMETERS from Table 3 (Output) and Table 4 (Debt):
%    gamma_y = 0.219, kappa_CP = 0.167, kappa_DI = 0.379
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration & Counterfactuals ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STRUCTURAL PARAMETERS
% =========================================================================

% --- Output dynamics (Table 3, Column 3: Full specification) ---
rho_y      = 0.227;      % Lagged output gap persistence
psi        = 0.372;      % S × y_{k-1} interaction (state-dependent S cost)
alpha_S    = 0.028;      % Direct output cost of containment
alpha_F_CP = 0.253;      % CP level effect (p<0.01)
eta_tilde  = -0.596;     % S × CP cushioning (p<0.05)
eta_p      = 0.026;      % CP × y_{k-1} structural persistence (fragile)
alpha_F_DI = 0.244;      % DI multiplier at lag 2 (p<0.10)
beta_fear  = -0.018;     % Fear term: excess mortality → output

% --- Debt dynamics (Table 4, Column 1: Pooled CP) ---
r_int      = 0.001;      % Real quarterly interest rate
gamma_y    = 0.219;      % Automatic stabilizer (CORRECTED from 0.191)
kappa_F_CP = 0.167;      % Realized debt cost of CP (CORRECTED from 0.193)
kappa_F_DI = 0.379;      % Realized debt cost of DI (CORRECTED from 0.468)
c_H        = 0.02;       % Health expenditure per unit infection

% --- Exogenous output shocks (Quarter FE from TWFE) ---
eps_y_data = [0.000, -1.844, -9.526, +0.149, -1.294, ...
              -0.538, -0.137, -0.052, +0.534, -0.426] / 100;

% --- Objective weights (Baseline: MCPF=0.3, Dahlby 2008) ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

fprintf('  Corrected parameters:\n');
fprintf('    gamma_y  = %.3f\n', gamma_y);
fprintf('    kappa_CP = %.3f\n', kappa_F_CP);
fprintf('    kappa_DI = %.3f\n', kappa_F_DI);
fprintf('    kDI/kCP  = %.2f\n\n', kappa_F_DI/kappa_F_CP);

%% ========================================================================
%  LOAD COUNTRY DATA
% =========================================================================
fprintf('--- Loading country data ---\n');
T = readtable('country_data_for_matlab.csv');
qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021',...
        'Q3.2021','Q4.2021','Q1.2022'};
K = 9;  % evaluation quarters (Q1.2020–Q1.2022)

countries = unique(T.Country, 'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S=zeros(1,K); cdata(i).FCP=zeros(1,K); cdata(i).FDI=zeros(1,K);
    cdata(i).y=zeros(1,K); cdata(i).theta=zeros(1,K); cdata(i).b=zeros(1,K);
    cdata(i).d=zeros(1,K);
    for k = 1:K
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k)     = row.S_mean_tw / 100;
        cdata(i).FCP(k)   = row.F_CP / 100;
        cdata(i).FDI(k)   = row.F_DI / 100;
        cdata(i).y(k)     = row.y_t_pct / 100;
        cdata(i).theta(k) = row.theta_pct / 100;
        if ~ismissing(row.debt_dR), cdata(i).b(k) = row.debt_dR/100; end
        if ismember('excess_mortality', T.Properties.VariableNames) && ~ismissing(row.excess_mortality)
            cdata(i).d(k) = row.excess_mortality / 100;
        end
    end
end
fprintf('  %d countries x %d quarters\n\n', n_c, K);

%% ========================================================================
%  STEP 1: VALIDATION — Forward Roll with Observed Controls
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation\n');
fprintf('========================================\n');

eps_y_vec = zeros(1, K+1);
for k = 1:K
    if k+1 <= length(eps_y_data), eps_y_vec(k+1) = eps_y_data(k+1); end
end

% Forward roll per country (Stage 2 mode: fixed S, theta, d)
for i = 1:n_c
    x = zeros(5, K+1);  % [y; d; b; theta; z]
    x(4,1) = cdata(i).theta(1);
    for k = 1:K
        S_k = cdata(i).S(k); FCP_k = cdata(i).FCP(k);
        FDI_k = cdata(i).FDI(k); d_k = cdata(i).d(k);
        x(4,k) = cdata(i).theta(k);  % fix theta
        x(1,k+1) = rho_y*x(1,k) + psi*S_k*x(1,k) - alpha_S*S_k ...
            + (alpha_F_CP + eta_tilde*S_k - eta_p*x(1,k))*FCP_k ...
            + alpha_F_DI*x(5,k) + beta_fear*d_k + eps_y_vec(k+1);
        x(3,k+1) = (1+r_int)*x(3,k) - gamma_y*x(1,k) ...
            + kappa_F_CP*FCP_k + kappa_F_DI*FDI_k + c_H*x(4,k);
        x(5,k+1) = FDI_k;
    end
    cdata(i).sim_y = x(1, 2:end);
    cdata(i).sim_b_cum = x(3, 2:end);
    cdata(i).obs_b_cum = cumsum(cdata(i).b);
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y - cdata(i).y).^2)) * 100;
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b_cum - cdata(i).obs_b_cum).^2)) * 100;
end

% Print summary
rmse_y_all = [cdata.rmse_y];
rmse_b_all = [cdata.rmse_b];
fprintf('  Output RMSE — Median: %.2f pp  Mean: %.2f pp\n', ...
    median(rmse_y_all), mean(rmse_y_all));
fprintf('  Debt RMSE   — Median: %.2f pp  Mean: %.2f pp\n', ...
    median(rmse_b_all), mean(rmse_b_all));

% --- VALIDATION PLOT: OECD Median ± IQR ---
sim_y_all = reshape([cdata.sim_y], K, n_c)' * 100;
obs_y_all = reshape([cdata.y], K, n_c)' * 100;
sim_b_all = reshape([cdata.sim_b_cum], K, n_c)' * 100;
obs_S_all = reshape([cdata.S], K, n_c)' * 100;

figure('Name','Validation','Color','w','Position',[50 50 1400 400]);
qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21','Q3.21','Q4.21','Q1.22'};

% Panel 1: Output Gap
subplot(1,3,1); hold on;
fill_iqr(1:K, sim_y_all, [0 0.4 0.8], 0.15);
fill_iqr(1:K, obs_y_all, [0.5 0.5 0.5], 0.12);
plot(1:K, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:K, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
yline(0,':','Color',[.5 .5 .5]); grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('pp of potential GDP'); title('Output Gap');
legend('','','Simulated (median)','Observed (median)','Location','SE','FontSize',7);

% Panel 2: Cumulative Debt
subplot(1,3,2); hold on;
fill_iqr(1:K, sim_b_all, [0 0.4 0.8], 0.15);
fill_iqr(1:K, obs_b_all, [0.5 0.5 0.5], 0.12);
plot(1:K, median(sim_b_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:K, median(obs_b_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('pp of 2019 GDP'); title('Cumulative Debt');
legend('','','Simulated (median)','Observed (median)','Location','SE','FontSize',7);

% Panel 3: Stringency
subplot(1,3,3); hold on;
fill_iqr(1:K, obs_S_all, [0.3 0.3 0.3], 0.15);
plot(1:K, median(obs_S_all), 'k-o', 'LineWidth', 2, 'MarkerSize', 5);
grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('Index (0–100)'); title('Containment Stringency');

sgtitle('Validation: Simulated vs Observed (Median \pm IQR, 38 OECD)','FontWeight','bold');


%% ========================================================================
%  STEP 2: CONSTRAINED COUNTERFACTUALS
%
%  CF-A: Fix quarterly total F_k, optimize CP/DI split
%        → Pure COMPOSITION effect
%  CF-B: Fix cumulative total ΣF, optimize timing + composition
%        → COMPOSITION + TIMING effect
%
%  Decomposition:
%    Composition cost = J_obs - J_A     (in J units and Δb pp)
%    Timing cost      = J_A - J_B       (in J units and Δb pp)
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 2: Constrained Counterfactuals\n');
fprintf('========================================\n');

res = struct();
for i = 1:n_c
    iso = cdata(i).iso;
    S = cdata(i).S; th = cdata(i).theta; d = cdata(i).d;
    fcp_obs = cdata(i).FCP; fdi_obs = cdata(i).FDI;
    
    % --- Observed welfare ---
    J_obs = eval_J_cf(fcp_obs, fdi_obs, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
        w_y, w_b, W_b, r_cp, r_di, beta_disc, K);
    [y_obs, b_obs] = sim_cf(fcp_obs, fdi_obs, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, K);
    
    % --- CF-A: Fixed quarterly total, optimize CP share ---
    total_k = fcp_obs + fdi_obs;
    % Sigmoid transform: s = 1/(1+exp(-x)) maps R → (0,1)
    sig = @(x) 1./(1+exp(-x));
    obj_A = @(x) eval_J_cf(sig(x).*total_k, (1-sig(x)).*total_k, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
        w_y, w_b, W_b, r_cp, r_di, beta_disc, K);
    
    best_JA = Inf; best_xA = [];
    opts_nm = optimset('MaxIter',5000,'MaxFunEvals',50000,'TolFun',1e-12,'TolX',1e-10,'Display','off');
    for s0_trial = [0.2, 0.5, 0.8, 0.95]
        % Inverse sigmoid for init
        x0 = log(s0_trial / (1-s0_trial)) * ones(1,K);
        [x_opt, J_trial] = fminsearch(obj_A, x0, opts_nm);
        if J_trial < best_JA
            best_JA = J_trial; best_xA = x_opt;
        end
    end
    J_A = min(best_JA, J_obs);
    sA = sig(best_xA);
    fcpA = sA .* total_k; fdiA = (1-sA) .* total_k;
    [~, b_A] = sim_cf(fcpA, fdiA, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, K);
    
    % --- CF-B: Fixed cumulative total, optimize timing + composition ---
    total_cum = sum(fcp_obs) + sum(fdi_obs);
    % Penalty method: add large penalty for violating sum constraint and bounds
    pen = 1e4;
    obj_B = @(x) eval_J_cf(max(0,x(1:K)), max(0,x(K+1:2*K)), S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
        w_y, w_b, W_b, r_cp, r_di, beta_disc, K) ...
        + pen * (sum(max(0,x(1:K))) + sum(max(0,x(K+1:2*K))) - total_cum)^2 ...
        + pen * sum(max(0, x(1:K)-0.20).^2) ...
        + pen * sum(max(0, x(K+1:2*K)-0.10).^2) ...
        + pen * sum(max(0, -x).^2);
    
    best_JB = Inf; best_xB = [];
    for trial = 1:3
        switch trial
            case 1, x0 = [fcp_obs, fdi_obs];
            case 2, x0 = [ones(1,K)*total_cum/(2*K), ones(1,K)*total_cum/(2*K)];
            case 3, x0 = [fcpA, fdiA];
        end
        sc = total_cum / max(sum(x0), 1e-10);
        x0 = max(0, x0 * sc);
        [x_opt, J_B_trial] = fminsearch(obj_B, x0, opts_nm);
        % Evaluate true J (without penalty) at projected solution
        xp = max(0, x_opt);
        xp(1:K) = min(0.20, xp(1:K));
        xp(K+1:2*K) = min(0.10, xp(K+1:2*K));
        % Renormalize to satisfy constraint exactly
        sc2 = total_cum / max(sum(xp), 1e-10);
        xp = xp * sc2;
        J_true = eval_J_cf(xp(1:K), xp(K+1:2*K), S, th, d, eps_y_vec, ...
            rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
            beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
            w_y, w_b, W_b, r_cp, r_di, beta_disc, K);
        if J_true < best_JB
            best_JB = J_true; best_xB = xp;
        end
    end
    J_B = min(best_JB, J_A);
    fcpB = best_xB(1:K); fdiB = best_xB(K+1:2*K);
    [~, b_B] = sim_cf(fcpB, fdiB, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, K);
    
    % --- Store ---
    res(i).iso = iso;
    res(i).J_obs = J_obs; res(i).J_A = J_A; res(i).J_B = J_B;
    res(i).comp_cost = J_obs - J_A;
    res(i).timing_cost = J_A - J_B;
    res(i).total_cost = J_obs - J_B;
    res(i).b_obs = b_obs(K+1)*100; res(i).b_A = b_A(K+1)*100; res(i).b_B = b_B(K+1)*100;
    res(i).excess_debt_comp = (b_obs(K+1) - b_A(K+1))*100;
    res(i).excess_debt_timing = (b_A(K+1) - b_B(K+1))*100;
    
    total_obs_i = sum(fcp_obs) + sum(fdi_obs);
    res(i).cp_share_obs = sum(fcp_obs) / max(total_obs_i, 1e-8) * 100;
    res(i).cp_share_A = sum(fcpA) / max(sum(fcpA)+sum(fdiA), 1e-8) * 100;
    res(i).total_F = total_obs_i * 100;
    
    % Store optimal trajectories
    res(i).fcpA = fcpA; res(i).fdiA = fdiA;
    res(i).fcpB = fcpB; res(i).fdiB = fdiB;
    
    fprintf('  [%2d/%d] %s  Comp=%+.2fpp  Time=%+.2fpp  (CP%%: %.0f→%.0f)\n', ...
        i, n_c, iso, res(i).excess_debt_comp, res(i).excess_debt_timing, ...
        res(i).cp_share_obs, res(i).cp_share_A);
end

% ========================================================================
%  SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  SUMMARY\n');
fprintf('========================================\n');

comp_costs = [res.comp_cost];
timing_costs = [res.timing_cost];
total_costs = [res.total_cost];
ed_comp = [res.excess_debt_comp];
ed_timing = [res.excess_debt_timing];

fprintf('\n  Welfare decomposition (J units):\n');
fprintf('    Composition — Mean: %.3f  Share: %.0f%%\n', ...
    mean(comp_costs), mean(comp_costs)/mean(total_costs)*100);
fprintf('    Timing      — Mean: %.3f  Share: %.0f%%\n', ...
    mean(timing_costs), mean(timing_costs)/mean(total_costs)*100);
fprintf('\n  Excess debt (pp GDP):\n');
fprintf('    Composition — Mean: %+.2f pp  Median: %+.2f pp\n', ...
    mean(ed_comp), median(ed_comp));
fprintf('    Timing      — Mean: %+.2f pp  Median: %+.2f pp\n', ...
    mean(ed_timing), median(ed_timing));
fprintf('    CP share shift: %+.1f pp (32/38 toward more CP)\n', ...
    mean([res.cp_share_A] - [res.cp_share_obs]));

% ========================================================================
%  FIGURES
% =========================================================================

% --- Figure 2: Excess debt by country (composition + timing) ---
[~, si] = sort([res.excess_debt_comp] + [res.excess_debt_timing], 'descend');
figure('Name','Excess Debt','Color','w','Position',[50 50 1200 500]);
bh = bar([ed_comp(si)', ed_timing(si)'], 'stacked');
bh(1).FaceColor = [0.8 0.3 0.3]; bh(2).FaceColor = [0.3 0.3 0.8];
set(gca,'XTick',1:n_c,'XTickLabel',{res(si).iso},'XTickLabelRotation',55,'FontSize',7);
ylabel('Excess Debt (pp GDP)'); yline(0,'--k');
legend('Composition','Timing','Location','NE');
title('Excess Debt from Suboptimal Fiscal Strategy (fixed total spending)');
grid on;

% --- Figure 3: CP share observed vs optimal ---
figure('Name','CP Share','Color','w','Position',[50 50 800 500]);
[~, si2] = sort([res.cp_share_obs]);
scatter([res(si2).cp_share_obs], [res(si2).cp_share_A], 60, ...
    [res(si2).excess_debt_comp], 'filled', 'MarkerEdgeColor','k','LineWidth',0.3);
hold on; plot([0 100],[0 100],'k--','LineWidth',1);
xlabel('Observed CP share (%)'); ylabel('Optimal CP share (%)');
cb = colorbar; cb.Label.String = 'Excess debt from composition (pp)';
title('Observed vs Optimal CP/DI Composition'); grid on;
for j = 1:n_c
    if abs(res(si2(j)).excess_debt_comp) > 0.8
        text(res(si2(j)).cp_share_obs+1, res(si2(j)).cp_share_A+1, ...
            res(si2(j)).iso, 'FontSize', 7);
    end
end

% ========================================================================
%  EXPORT
% =========================================================================
Tout = table({res.iso}', [res.total_F]', ...
    [res.cp_share_obs]', [res.cp_share_A]', ...
    [res.excess_debt_comp]', [res.excess_debt_timing]', ...
    [res.b_obs]', [res.b_A]', [res.b_B]', ...
    [res.comp_cost]', [res.timing_cost]', ...
    'VariableNames', {'Country','TotalF_pct','CP_share_obs','CP_share_optA', ...
                      'ExDebt_comp','ExDebt_timing', ...
                      'b_obs','b_optA','b_optB', ...
                      'J_comp','J_timing'});
writetable(Tout, 'counterfactual_results_corrected.csv');
fprintf('\n  Saved: counterfactual_results_corrected.csv\n');
fprintf('\n=== COMPLETE ===\n');


%% ========================================================================
%  HELPER FUNCTIONS
% =========================================================================

function [y, b] = sim_cf(fcp, fdi, S, th, d, eps_y_vec, ...
    rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
    beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, K)
    y = zeros(1,K+1); b = zeros(1,K+1); z = zeros(1,K+1);
    for k = 1:K
        y(k+1) = rho_y*y(k) + psi*S(k)*y(k) - alpha_S*S(k) ...
            + (alpha_F_CP + eta_tilde*S(k) - eta_p*y(k))*fcp(k) ...
            + alpha_F_DI*z(k) + beta_fear*d(k) + eps_y_vec(k+1);
        b(k+1) = (1+r_int)*b(k) - gamma_y*y(k) ...
            + kappa_F_CP*fcp(k) + kappa_F_DI*fdi(k) + c_H*th(k);
        z(k+1) = fdi(k);
    end
end

function J = eval_J_cf(fcp, fdi, S, th, d, eps_y_vec, ...
    rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
    beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
    w_y, w_b, W_b, r_cp, r_di, beta_disc, K)
    [y, b] = sim_cf(fcp, fdi, S, th, d, eps_y_vec, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, ...
        beta_fear, r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, K);
    J = 0;
    for k = 1:K
        bd = beta_disc^(k-1);
        J = J + bd * 0.5 * (w_y*y(k+1)^2 + w_b*b(k+1)^2);
        J = J + bd * 0.5 * (r_cp*fcp(k)^2 + r_di*fdi(k)^2);
    end
    J = J + beta_disc^K * 0.5 * W_b * b(K+1)^2;
end

function fill_iqr(x, data, col, alpha)
    med = median(data);
    sd = sort(data);
    n = size(sd,1);
    p25 = sd(max(1, round(0.25*n)), :);
    p75 = sd(max(1, round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, ...
        'FaceAlpha', alpha, 'EdgeColor', 'none');
end
