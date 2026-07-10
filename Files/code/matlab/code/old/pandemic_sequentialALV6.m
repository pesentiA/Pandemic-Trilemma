%% ========================================================================
%  PANDEMIC SOCIAL PLANNER — V6 (Theory-Aligned)
%  Corrects V5 to match Property 3 transition system exactly:
%    1. Added eta_p term (structural preservation channel)
%    2. State space R^5 (one DI auxiliary, not two)
%    3. DI debt cost contemporaneous (not lagged)
%    4. Scaling: psi and eta_tilde both in iLQR units (×100)
%    5. Jacobians fully corrected
% =========================================================================
%  Transition system (from Property 3, eq:output and eq:debt):
%
%    y_{k+1}     = rho_y*y_k + (psi*y_k - alpha_S)*S_k
%                  + (alpha_F_CP + eta_tilde*S_k - eta_p*y_k)*F_CP
%                  + alpha_F_DI * z_k + beta_d * d_k
%    d_{k+1}     = delta_theta(k) * theta_k
%    b_{k+1}     = (1+r)*b_k - gamma_y*y_k + kappa_CP*F_CP
%                  + kappa_DI*F_DI + c_H*theta_k
%    theta_{k+1} = rho_theta(k)*(1 - phi_S*S_k)*theta_k + eps_k
%    z_{k+1}     = F_DI_k
%
%  State:   x = (y, d, b, theta, z)' in R^5
%  Control: u = (S, F_DI, F_CP)' in R^3
% =========================================================================
clear; clc; close all;

%% ========================================================================
%  STEP 0: STRUCTURAL PARAMETERS (Fixed across all weight calibrations)
% =========================================================================

% --- Time Horizon ---
N    = 12;
beta_disc = 0.99;

% --- Output Dynamics (Estimated, iLQR scale) ---
% All interaction terms scaled ×100 from empirical estimates
rho_y       = 0.00;       % Baseline persistence (insignificant, set to 0)
psi         = 0.372;      % Lockdown-induced scarring (empirical 0.00372 × 100)
alpha_S     = 0.025;      % Containment flow damage
alpha_F_CP  = 0.264;      % CP level effect
eta_tilde   = -0.607;     % CP × S interaction (empirical -0.00607 × 100)
eta_p       = 0.024;      % CP structural preservation (empirical, p=0.07)
alpha_F_DI  = 0.220;      % DI multiplier (lag 2)
beta_fear   = -7.5;       % Behavioral pandemic cost (empirical, p=0.043)

% --- Epidemiology (Calibrated, wave-specific) ---
rho_theta_base = 1.30;
phi_S          = 0.55;
delta_theta    = 0.1;

% Wave structure
wave_quarters = [3,  5,  7,  8];
wave_shocks   = [0.02, 0.015, 0.025, 0.04];
wave_rho      = [1.30, 1.25, 1.40, 1.60];
wave_ifr      = [0.02, 0.025, 0.02, 0.005];

rho_theta_t   = rho_theta_base * ones(1, N);
delta_theta_t = delta_theta * ones(1, N);
for w = 1:length(wave_quarters)
    k_wave = wave_quarters(w);
    if k_wave <= N
        rho_theta_t(k_wave) = wave_rho(w);
        if k_wave + 1 <= N
            rho_theta_t(k_wave + 1) = 0.5*(wave_rho(w) + rho_theta_base);
        end
        delta_theta_t(k_wave:end) = wave_ifr(w);
    end
end

% --- Debt Dynamics (Estimated from Panel) ---
r_int      = 0.001;
gamma_y    = 0.191;
kappa_F_CP = 0.193;       % Pooled CP budget cost
kappa_F_DI = 0.468;
c_H        = 0.02;

% --- Dimensions and Bounds ---
n_x = 5;  n_u = 3;        % R^5 state, R^3 control
S_max = 0.86; FDI_max = 0.08; FCP_max = 0.10;

% --- Wave shock vector (pre-computed) ---
wave_shock_vec = zeros(1, N+1);
for w = 1:length(wave_quarters)
    k_arrival = wave_quarters(w) + 1;
    if k_arrival <= N+1
        wave_shock_vec(k_arrival) = wave_shocks(w);
    end
end

% --- Initial Conditions ---
% x0 = (y0, d0, b0, theta0, z0)'
x0 = [0; 0; 0; 0.03; 0];

fprintf('=== V6: Theory-Aligned (R^5 State, eta_p, contemporaneous DI debt) ===\n');
fprintf('  State:   x = (y, d, b, theta, z)'' in R^5\n');
fprintf('  Control: u = (S, F_DI, F_CP)'' in R^3\n');
fprintf('  Key changes from V5:\n');
fprintf('    1. eta_p = %.3f (structural preservation)\n', eta_p);
fprintf('    2. State R^5 (one DI auxiliary, not two)\n');
fprintf('    3. DI debt cost contemporaneous\n');
fprintf('    4. psi = %.3f, eta_tilde = %.3f (iLQR scale)\n\n', psi, eta_tilde);


%% ========================================================================
%  STEP 1: VSL-BASED WEIGHT CALIBRATION
% =========================================================================

fprintf('--- VSL-Based Weight Calibration ---\n\n');

GDP_pc = 45000;
VSL_multiples = [20, 40, 60, 80, 100, 140];
VSL_baseline  = 60;

w_y_base = 1;
MCPF = 0.3;

d_scale = 0.001;
y_scale = 0.05;
b_scale = 0.05;

w_y = 100;
w_d_baseline = w_y * VSL_baseline * (y_scale / d_scale)^2;
w_b_baseline = w_y * MCPF;
W_b_baseline = w_b_baseline;

fprintf('  VSL baseline: %d x GDP p.c. = $%dk\n', VSL_baseline, VSL_baseline*GDP_pc/1000);
fprintf('  w_d/w_y = %d\n', VSL_baseline);
fprintf('  w_b/w_y = %.1f (MCPF)\n', MCPF);
fprintf('  Numerical weights: w_y=%.0f, w_d=%.0f, w_b=%.1f\n', w_y, w_d_baseline, w_b_baseline);

% Control costs
r_S  = 10;
r_DI = 5;
r_CP = 0.5;

fprintf('  Control costs: r_S=%.1f, r_DI=%.1f, r_CP=%.1f\n', r_S, r_DI, r_CP);


%% ========================================================================
%  STEP 2: SOLVE BASELINE (VSL = 60, MCPF = 0.3)
% =========================================================================

fprintf('\n\n========================================\n');
fprintf('  PART A: VSL BASELINE (VSL=%d, MCPF=%.1f)\n', VSL_baseline, MCPF);
fprintf('========================================\n');

[x_base, u_base, cost_base] = solve_pandemic_ilqr_v6( ...
    w_y, w_d_baseline, w_b_baseline, W_b_baseline, r_S, r_DI, r_CP, ...
    x0, N, n_x, n_u, beta_disc, ...
    rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, beta_fear, ...
    rho_theta_t, delta_theta_t, phi_S, ...
    r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
    S_max, FDI_max, FCP_max, wave_shock_vec);

% No-intervention benchmark
x_unc = simulate_no_intervention_v6(x0, N, rho_y, beta_fear, ...
    rho_theta_t, delta_theta_t, r_int, gamma_y, c_H, wave_shock_vec);

print_results('BASELINE', x_base, u_base, N);


%% ========================================================================
%  STEP 3: VSL SENSITIVITY (Part B)
% =========================================================================

fprintf('\n\n========================================\n');
fprintf('  PART B: VSL SENSITIVITY\n');
fprintf('========================================\n');

n_vsl = length(VSL_multiples);
vsl_results = struct();
vsl_results.VSL = VSL_multiples;
vsl_results.avg_y = zeros(1, n_vsl);
vsl_results.cum_d = zeros(1, n_vsl);
vsl_results.end_b = zeros(1, n_vsl);
vsl_results.avg_S = zeros(1, n_vsl);
vsl_results.avg_CP = zeros(1, n_vsl);
vsl_results.avg_DI = zeros(1, n_vsl);
vsl_results.welfare = zeros(1, n_vsl);

for v = 1:n_vsl
    fprintf('\n--- VSL = %d x GDP p.c. ---\n', VSL_multiples(v));
    
    w_d_v = w_y * VSL_multiples(v) * (y_scale / d_scale)^2;
    
    [x_v, u_v, J_v] = solve_pandemic_ilqr_v6( ...
        w_y, w_d_v, w_b_baseline, W_b_baseline, r_S, r_DI, r_CP, ...
        x0, N, n_x, n_u, beta_disc, ...
        rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, beta_fear, ...
        rho_theta_t, delta_theta_t, phi_S, ...
        r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
        S_max, FDI_max, FCP_max, wave_shock_vec);
    
    vsl_results.avg_y(v) = mean(x_v(1, 1:N)) * 100;
    vsl_results.cum_d(v) = sum(x_v(2, 2:end)) * 100;
    vsl_results.end_b(v) = x_v(3, end) * 100;
    vsl_results.avg_S(v) = mean(u_v(1, :));
    vsl_results.avg_CP(v) = mean(u_v(3, :)) * 100;
    vsl_results.avg_DI(v) = mean(u_v(2, :)) * 100;
    vsl_results.welfare(v) = J_v;
end

% VSL Sensitivity Table
fprintf('\n=== VSL Sensitivity Results ===\n');
fprintf('  %6s | %8s | %8s | %8s | %6s | %6s | %6s\n', ...
        'VSL', 'Avg y(%)', 'Cum d(%)', 'End b(pp)', 'Avg S', 'Avg CP', 'Avg DI');
fprintf('  %s\n', repmat('-', 1, 70));
for v = 1:n_vsl
    fprintf('  %6d | %8.2f | %8.3f | %8.1f | %6.2f | %6.2f | %6.3f\n', ...
        VSL_multiples(v), vsl_results.avg_y(v), vsl_results.cum_d(v), ...
        vsl_results.end_b(v), vsl_results.avg_S(v), ...
        vsl_results.avg_CP(v), vsl_results.avg_DI(v));
end

% Plot VSL sensitivity
figure('Name', 'VSL Sensitivity', 'Color', 'w', 'Position', [50 50 1200 400]);

subplot(1,3,1);
plot(VSL_multiples, vsl_results.cum_d, 'ro-', 'LineWidth', 2, 'MarkerSize', 8);
xlabel('VSL (x GDP p.c.)'); ylabel('Cumulative Mortality (% pop.)');
title('Mortality vs. VSL'); grid on;

subplot(1,3,2);
plot(VSL_multiples, vsl_results.avg_y, 'bo-', 'LineWidth', 2, 'MarkerSize', 8);
xlabel('VSL (x GDP p.c.)'); ylabel('Avg. Output Gap (%)');
title('Output Loss vs. VSL'); grid on;

subplot(1,3,3);
plot(VSL_multiples, vsl_results.end_b, 'mo-', 'LineWidth', 2, 'MarkerSize', 8);
xlabel('VSL (x GDP p.c.)'); ylabel('Terminal Debt (pp GDP)');
title('Debt vs. VSL'); grid on;

sgtitle('VSL Sensitivity: Higher VSL → More Lockdown → More Output Loss → Less Mortality');


%% ========================================================================
%  STEP 4: 2D PARETO FRONTIER (Part C)
% =========================================================================

fprintf('\n\n========================================\n');
fprintf('  PART C: 2D PARETO FRONTIER\n');
fprintf('========================================\n');

VSL_grid  = [20, 40, 60, 80, 100, 140];
MCPF_grid = [0.1, 0.3, 0.5, 1.0, 2.0];

n_vsl_p  = length(VSL_grid);
n_mcpf_p = length(MCPF_grid);
n_total  = n_vsl_p * n_mcpf_p;

pareto = struct();
pareto.VSL   = zeros(n_total, 1);
pareto.MCPF  = zeros(n_total, 1);
pareto.avg_y = zeros(n_total, 1);
pareto.cum_d = zeros(n_total, 1);
pareto.end_b = zeros(n_total, 1);
pareto.avg_S = zeros(n_total, 1);
pareto.avg_CP = zeros(n_total, 1);
pareto.avg_DI = zeros(n_total, 1);

idx = 0;
for i = 1:n_vsl_p
    for j = 1:n_mcpf_p
        idx = idx + 1;
        fprintf('  Pareto grid [%d/%d]: VSL=%d, MCPF=%.1f\n', ...
                idx, n_total, VSL_grid(i), MCPF_grid(j));
        
        w_d_ij = w_y * VSL_grid(i) * (y_scale / d_scale)^2;
        w_b_ij = w_y * MCPF_grid(j);
        W_b_ij = w_b_ij;
        
        [x_ij, u_ij, ~] = solve_pandemic_ilqr_v6( ...
            w_y, w_d_ij, w_b_ij, W_b_ij, r_S, r_DI, r_CP, ...
            x0, N, n_x, n_u, beta_disc, ...
            rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, beta_fear, ...
            rho_theta_t, delta_theta_t, phi_S, ...
            r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
            S_max, FDI_max, FCP_max, wave_shock_vec);
        
        pareto.VSL(idx)   = VSL_grid(i);
        pareto.MCPF(idx)  = MCPF_grid(j);
        pareto.avg_y(idx) = mean(x_ij(1, 1:N)) * 100;
        pareto.cum_d(idx) = sum(x_ij(2, 2:end)) * 100;
        pareto.end_b(idx) = x_ij(3, end) * 100;
        pareto.avg_S(idx) = mean(u_ij(1, :));
        pareto.avg_CP(idx) = mean(u_ij(3, :)) * 100;
        pareto.avg_DI(idx) = mean(u_ij(2, :)) * 100;
    end
end

% --- Pareto Frontier Visualization ---
figure('Name', 'Pareto Frontier — 3D', 'Color', 'w', 'Position', [50 50 800 600]);
scatter3(pareto.cum_d, pareto.avg_y, pareto.end_b, ...
         80, pareto.VSL, 'filled');
xlabel('Cumulative Mortality (% pop.)');
ylabel('Avg. Output Gap (%)');
zlabel('Terminal Debt (pp GDP)');
title('Pareto Frontier of the Pandemic Trilemma');
cb = colorbar; cb.Label.String = 'VSL (x GDP p.c.)';
grid on; view(-30, 25);

hold on;
cum_d_unc = sum(x_unc(2, 2:end)) * 100;
avg_y_unc = mean(x_unc(1, 1:N)) * 100;
end_b_unc = x_unc(3, end) * 100;
scatter3(cum_d_unc, avg_y_unc, end_b_unc, 200, 'rx', 'LineWidth', 3);
text(cum_d_unc, avg_y_unc, end_b_unc, '  No intervention', 'FontSize', 10);

% 2D projections
figure('Name', 'Pareto Frontier — 2D Projections', 'Color', 'w', ...
       'Position', [50 50 1400 400]);

subplot(1,3,1);
scatter(pareto.cum_d, pareto.avg_y, 60, pareto.MCPF, 'filled');
hold on; plot(cum_d_unc, avg_y_unc, 'rx', 'MarkerSize', 15, 'LineWidth', 3);
xlabel('Cumulative Mortality (%)'); ylabel('Avg. Output Gap (%)');
title('Mortality–Output Tradeoff');
cb = colorbar; cb.Label.String = 'MCPF'; grid on;

subplot(1,3,2);
scatter(pareto.cum_d, pareto.end_b, 60, pareto.MCPF, 'filled');
hold on; plot(cum_d_unc, end_b_unc, 'rx', 'MarkerSize', 15, 'LineWidth', 3);
xlabel('Cumulative Mortality (%)'); ylabel('Terminal Debt (pp GDP)');
title('Mortality–Debt Tradeoff');
cb = colorbar; cb.Label.String = 'MCPF'; grid on;

subplot(1,3,3);
scatter(pareto.avg_y, pareto.end_b, 60, pareto.VSL, 'filled');
hold on; plot(avg_y_unc, end_b_unc, 'rx', 'MarkerSize', 15, 'LineWidth', 3);
xlabel('Avg. Output Gap (%)'); ylabel('Terminal Debt (pp GDP)');
title('Output–Debt Tradeoff');
cb = colorbar; cb.Label.String = 'VSL'; grid on;

sgtitle('Pareto Frontier: 2D Projections of the Pandemic Trilemma');

% Fiscal composition across frontier
figure('Name', 'Fiscal Composition on Frontier', 'Color', 'w', ...
       'Position', [50 50 800 400]);

subplot(1,2,1);
scatter(pareto.VSL, pareto.avg_CP, 60, pareto.MCPF, 'filled');
xlabel('VSL (x GDP p.c.)'); ylabel('Avg. CP (% GDP)');
title('CP Deployment across Frontier');
cb = colorbar; cb.Label.String = 'MCPF'; grid on;

subplot(1,2,2);
scatter(pareto.VSL, pareto.avg_DI, 60, pareto.MCPF, 'filled');
xlabel('VSL (x GDP p.c.)'); ylabel('Avg. DI (% GDP)');
title('DI Deployment across Frontier');
cb = colorbar; cb.Label.String = 'MCPF'; grid on;

sgtitle('Fiscal Composition across Pareto Frontier');

% Print Pareto Table
fprintf('\n=== Pareto Frontier Results ===\n');
fprintf('  %4s | %4s | %7s | %7s | %7s | %5s | %5s | %5s\n', ...
        'VSL', 'MCPF', 'y(%)', 'd(%)', 'b(pp)', 'S', 'CP(%)', 'DI(%)');
fprintf('  %s\n', repmat('-', 1, 65));
for idx = 1:n_total
    fprintf('  %4d | %4.1f | %7.2f | %7.3f | %7.1f | %5.2f | %5.2f | %5.3f\n', ...
        pareto.VSL(idx), pareto.MCPF(idx), ...
        pareto.avg_y(idx), pareto.cum_d(idx), pareto.end_b(idx), ...
        pareto.avg_S(idx), pareto.avg_CP(idx), pareto.avg_DI(idx));
end


%% ========================================================================
%  STEP 5: BASELINE VISUALIZATION
% =========================================================================

quarter_labels = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21',...
                  'Q3.21','Q4.21','Q1.22','Q2.22','Q3.22','Q4.22'};
time = 0:N;

figure('Name', 'V6: Baseline Optimal Policy', 'Color', 'w', ...
       'Position', [50 50 1400 900]);

% Output Gap
subplot(3, 2, 1);
plot(time, x_unc(1,:)*100, 'r--', 'LineWidth', 1.2); hold on;
plot(time, x_base(1,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
legend('No intervention', 'Optimal (VSL=60)', 'Location', 'SouthEast');
title('Output Gap (y_k)'); ylabel('% deviation'); xlabel('Quarter'); grid on;

% Infection
subplot(3, 2, 2);
yyaxis left
plot(time, x_base(4,:)*100, 'Color', [0.6 0 0], 'LineWidth', 2);
ylabel('Optimal (% pop.)');
for w = 1:length(wave_quarters)
    xline(wave_quarters(w), ':', sprintf('W%d', w+1), 'Color', [0.5 0 0]);
end
yyaxis right
plot(time, x_unc(4,:)*100, 'r--', 'LineWidth', 1.2);
ylabel('No intervention (% pop.)');
ax = gca;
ax.YAxis(1).Color = [0.6 0 0];
ax.YAxis(2).Color = [1 0.4 0.4];
legend('Optimal', 'No intervention', 'Location', 'NorthWest');
title('Infection Rate (\theta_k)'); xlabel('Quarter'); grid on;

% Debt
subplot(3, 2, 3);
plot(time, x_unc(3,:)*100, 'r--', 'LineWidth', 1.2); hold on;
plot(time, x_base(3,:)*100, 'm-', 'LineWidth', 2);
yline(0, '--k');
legend('No intervention', 'Optimal');
title('Public Debt (b_k)'); ylabel('pp of GDP'); xlabel('Quarter'); grid on;

% Mortality
subplot(3, 2, 4);
d_cum_unc = [0, cumsum(x_unc(2, 2:end))]*100;
d_flow_opt = x_base(2, :)*100;
yyaxis left
plot(time, d_flow_opt, 'Color', [0.8 0 0], 'LineWidth', 2);
ylabel('Optimal flow (% pop.)');
yyaxis right
plot(time, d_cum_unc, 'r--', 'LineWidth', 1.5);
ylabel('No intervention cumulative (% pop.)');
ax = gca;
ax.YAxis(1).Color = [0.8 0 0];
ax.YAxis(2).Color = [1 0.4 0.4];
legend('Quarterly flow (optimal)', 'Cumulative (no intervention)', 'Location', 'NorthWest');
title('Excess Mortality (d_k)'); xlabel('Quarter'); grid on;

% Containment
subplot(3, 2, 5);
bar(1:N, u_base(1,:)', 'FaceColor', [0.2 0.4 0.7]);
hold on; yline(0.51, '--r', 'S^{crit}', 'LineWidth', 1.5);
title('Containment Intensity (S_k)'); ylabel('S [0, 1]');
ylim([0 1]); grid on;
set(gca, 'XTickLabel', quarter_labels, 'XTickLabelRotation', 45);

% Fiscal
subplot(3, 2, 6);
bar(1:N, [u_base(2,:)', u_base(3,:)'] * 100, 'grouped');
legend('F^{DI}', 'F^{CP}', 'Location', 'NorthEast');
title('Fiscal Composition'); ylabel('% of GDP'); grid on;
set(gca, 'XTickLabel', quarter_labels, 'XTickLabelRotation', 45);

sgtitle(sprintf('V6 Optimal Policy: VSL=%d, MCPF=%.1f', VSL_baseline, MCPF), ...
        'FontWeight', 'bold', 'FontSize', 14);


%% ========================================================================
%  HELPER FUNCTIONS
% =========================================================================

function [x_opt, u_opt, J_opt] = solve_pandemic_ilqr_v6( ...
    w_y, w_d, w_b, W_b, r_S, r_DI, r_CP, ...
    x0, N, n_x, n_u, beta_disc, ...
    rho_y, psi, alpha_S, alpha_F_CP, eta_tilde, eta_p, alpha_F_DI, beta_fear, ...
    rho_theta_t, delta_theta_t, phi_S, ...
    r_int, gamma_y, kappa_F_CP, kappa_F_DI, c_H, ...
    S_max, FDI_max, FCP_max, wave_shock_vec)

    % Cost matrices (R^5 state)
    Q_base = diag([w_y, w_d, w_b, 0, 0]);
    Q_N    = diag([0, 0, W_b, 0, 0]);
    R      = diag([r_S, r_DI, r_CP]);

    % =================================================================
    %  Transition function (matches Property 3 exactly)
    %  x = [y; d; b; theta; z]   u = [S; F_DI; F_CP]
    % =================================================================
    f_tv = @(x, u, k) [ ...
        rho_y*x(1) + psi*u(1)*x(1) - alpha_S*u(1) ...
            + (alpha_F_CP + eta_tilde*u(1) - eta_p*x(1))*u(3) ...
            + alpha_F_DI*x(5) ...
            + beta_fear*x(2); ...                           % y_{k+1}
        delta_theta_t(k) * x(4); ...                        % d_{k+1}
        (1+r_int)*x(3) - gamma_y*x(1) ...
            + kappa_F_CP*u(3) + kappa_F_DI*u(2) ...
            + c_H*x(4); ...                                 % b_{k+1}
        rho_theta_t(k) * (1 - phi_S*u(1)) * x(4); ...      % theta_{k+1}
        u(2) ...                                             % z_{k+1} = F_DI_k
    ];

    f_wave = @(x, u, k) f_tv(x, u, k) + [0; 0; 0; wave_shock_vec(k+1); 0];

    % =================================================================
    %  Jacobians A_k = df/dx, B_k = df/du
    %
    %  Output row (y_{k+1}):
    %    dy/dy = rho_y + psi*S - eta_p*F_CP
    %    dy/dd = beta_fear
    %    dy/db = 0
    %    dy/dtheta = 0
    %    dy/dz = alpha_F_DI
    %    dy/dS = psi*y - alpha_S + eta_tilde*F_CP
    %    dy/dF_DI = 0
    %    dy/dF_CP = alpha_F_CP + eta_tilde*S - eta_p*y
    % =================================================================
    Ak_fun = @(x, u, k) [ ...
        rho_y + psi*u(1) - eta_p*u(3),  beta_fear,  0,       0,                              alpha_F_DI; ...
        0,                                0,          0,       delta_theta_t(k),               0; ...
        -gamma_y,                         0,          1+r_int, c_H,                            0; ...
        0,                                0,          0,       rho_theta_t(k)*(1-phi_S*u(1)),  0; ...
        0,                                0,          0,       0,                              0  ...
    ];

    Bk_fun = @(x, u, k) [ ...
        psi*x(1) - alpha_S + eta_tilde*u(3),   0,           alpha_F_CP + eta_tilde*u(1) - eta_p*x(1); ...
        0,                                      0,           0; ...
        0,                                      kappa_F_DI,  kappa_F_CP; ...
        -rho_theta_t(k)*phi_S*x(4),            0,           0; ...
        0,                                      1,           0  ...
    ];

    % =================================================================
    %  Heuristic initial trajectory
    % =================================================================
    x_bar = zeros(n_x, N+1);
    u_bar = zeros(n_u, N);
    x_bar(:, 1) = x0;
    for k = 1:N
        base_S = max(0.1, 0.7 - 0.05*(k-1));
        if any(abs(k - [3 5 7 8]) <= 1)
            base_S = min(S_max, base_S + 0.2);
        end
        base_CP = max(0, 0.06 - 0.005*(k-1));
        u_bar(:, k) = [base_S; 0.002; base_CP];
        x_bar(:, k+1) = f_wave(x_bar(:, k), u_bar(:, k), k);
    end

    % =================================================================
    %  AL-iLQR
    % =================================================================
    max_outer = 30;
    max_inner = 60;
    tol_inner = 1e-4;
    tol_outer = 1e-4;
    phi_pen   = 2.5;
    rho_base  = 1e-4;

    % Constraints: 6 inequality constraints on 3 controls
    n_c = 6;
    lambda = zeros(n_c, N);
    mu     = 10.0 * ones(n_c, N);

    C_u = [-1 0 0; 1 0 0; 0 -1 0; 0 1 0; 0 0 -1; 0 0 1];
    c_bounds = [0; S_max; 0; FDI_max; 0; FCP_max];

    for outer = 1:max_outer
        for inner = 1:max_inner
            Ak = cell(N,1); Bk = cell(N,1);
            for k = 1:N
                Ak{k} = Ak_fun(x_bar(:,k), u_bar(:,k), k);
                Bk{k} = Bk_fun(x_bar(:,k), u_bar(:,k), k);
            end

            P = cell(N+1,1); p = cell(N+1,1);
            K = cell(N,1); k_ff = cell(N,1);
            P{N+1} = Q_N * beta_disc^N;
            p{N+1} = Q_N * beta_disc^N * x_bar(:,N+1);

            rho_reg = rho_base;
            bw_ok = false;
            while ~bw_ok && rho_reg < 1e6
                bw_ok = true;
                for k = N:-1:1
                    Q_k = Q_base * beta_disc^(k-1);
                    R_k = R      * beta_disc^(k-1);   % symmetric discounting (fix)
                    A = Ak{k}; B = Bk{k};
                    c_val = C_u * u_bar(:,k) - c_bounds;
                    act = (c_val > 0) | (lambda(:,k) > 0);
                    I_mu = diag(mu(:,k) .* act);

                    Qx  = Q_k*x_bar(:,k) + A'*p{k+1};
                    Qxx = Q_k + A'*P{k+1}*A;
                    Qux = B'*P{k+1}*A;
                    Qu  = R_k*u_bar(:,k) + B'*p{k+1} + C_u'*(lambda(:,k) + I_mu*c_val);
                    Quu = R_k + B'*P{k+1}*B + C_u'*I_mu*C_u + rho_reg*eye(n_u);

                    [~, pc] = chol(Quu);
                    if pc > 0
                        bw_ok = false; rho_reg = rho_reg*10; break;
                    end

                    K{k} = Quu\Qux;
                    k_ff{k} = -Quu\Qu;
                    p{k} = Qx + K{k}'*Quu*k_ff{k};
                    P{k} = Qxx - K{k}'*Quu*K{k};
                end
            end
            if ~bw_ok, break; end

            al = 1.0;
            cost_old = al_cost_v6(x_bar, u_bar, Q_base, Q_N, R, beta_disc, ...
                                   N, lambda, mu, C_u, c_bounds);
            ls_ok = false;
            while al > 1e-8
                xn = zeros(n_x, N+1); un = zeros(n_u, N);
                xn(:,1) = x0;
                for k = 1:N
                    un(:,k) = u_bar(:,k) + al*k_ff{k} - K{k}*(xn(:,k)-x_bar(:,k));
                    un(1,k) = max(0, min(S_max, un(1,k)));
                    un(2,k) = max(0, min(FDI_max, un(2,k)));
                    un(3,k) = max(0, min(FCP_max, un(3,k)));
                    xn(:,k+1) = f_wave(xn(:,k), un(:,k), k);
                end
                cn = al_cost_v6(xn, un, Q_base, Q_N, R, beta_disc, ...
                                 N, lambda, mu, C_u, c_bounds);
                if cn < cost_old, ls_ok = true; break; else, al = al*0.5; end
            end
            if ~ls_ok, break; end

            dx = norm(xn-x_bar,'fro')/(norm(x_bar,'fro')+1e-12);
            x_bar = xn; u_bar = un;
            if dx < tol_inner, break; end
        end

        mv = 0;
        for k = 1:N
            c_val = C_u*u_bar(:,k) - c_bounds;
            mv = max(mv, max(c_val));
            lambda(:,k) = max(0, lambda(:,k) + mu(:,k).*c_val);
            mu(:,k) = mu(:,k)*phi_pen;
        end
        if mv < tol_outer, break; end
    end

    x_opt = x_bar; u_opt = u_bar;
    J_opt = al_cost_v6(x_opt, u_opt, Q_base, Q_N, R, beta_disc, N, ...
                        zeros(n_c,N), zeros(n_c,N), C_u, c_bounds);
end


function x_unc = simulate_no_intervention_v6(x0, N, rho_y, beta_fear, ...
    rho_theta_t, delta_theta_t, r_int, gamma_y, c_H, wave_shock_vec)
    % No intervention: S=0, F_DI=0, F_CP=0
    % Only persistence, fear, automatic stabilizers, and epi dynamics
    n_x = 5;
    x_unc = zeros(n_x, N+1);
    x_unc(:, 1) = x0;
    for k = 1:N
        x_unc(1, k+1) = rho_y*x_unc(1,k) + beta_fear*x_unc(2,k);
        x_unc(2, k+1) = delta_theta_t(k) * x_unc(4,k);
        x_unc(3, k+1) = (1+r_int)*x_unc(3,k) - gamma_y*x_unc(1,k) + c_H*x_unc(4,k);
        x_unc(4, k+1) = rho_theta_t(k) * x_unc(4,k) + wave_shock_vec(k+1);
        x_unc(5, k+1) = 0;
    end
end


function print_results(label, x, u, N)
    fprintf('\n=== %s Results ===\n', label);
    fprintf('  Controls:\n');
    fprintf('    S:    '); fprintf('%.3f ', u(1,:)); fprintf('\n');
    fprintf('    F_DI: '); fprintf('%.4f ', u(2,:)); fprintf('\n');
    fprintf('    F_CP: '); fprintf('%.4f ', u(3,:)); fprintf('\n');
    fprintf('  Outcomes:\n');
    fprintf('    Avg output gap:       %.2f%%\n', mean(x(1,1:N))*100);
    fprintf('    Cumulative mortality: %.3f%%\n', sum(x(2,2:end))*100);
    fprintf('    Terminal debt:        %.1f pp GDP\n', x(3,end)*100);
    fprintf('    Avg containment:      %.2f\n', mean(u(1,:)));
    fprintf('    Avg CP:               %.2f%% GDP\n', mean(u(3,:))*100);
    fprintf('    Avg DI:               %.3f%% GDP\n', mean(u(2,:))*100);
end


function J = al_cost_v6(x, u, Q_base, Q_N, R, beta, N, lambda, mu, C_u, c_bounds)
    J = 0;
    for k = 1:N
        sc = beta^(k-1) * (0.5*x(:,k)'*Q_base*x(:,k) + 0.5*u(:,k)'*R*u(:,k));
        cv = C_u*u(:,k) - c_bounds;
        act = (cv > 0) | (lambda(:,k) > 0);
        Im = diag(mu(:,k).*act);
        ac = lambda(:,k)'*cv + 0.5*cv'*Im*cv;
        J = J + sc + ac;
    end
    J = J + beta^N * 0.5*x(:,N+1)'*Q_N*x(:,N+1);
end


function c = parma_colors()
    c = [0.2 0.2 0.8; 0.2 0.6 0.8; 0.2 0.8 0.4; 0.8 0.8 0.2; 0.8 0.4 0.2; 0.8 0.2 0.2];
    c = interp1(linspace(0,1,size(c,1)), c, linspace(0,1,256));
end
