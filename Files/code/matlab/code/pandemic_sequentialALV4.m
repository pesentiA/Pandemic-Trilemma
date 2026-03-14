%% ========================================================================
%  PANDEMIC SOCIAL PLANNER — V4 (Waves + Welfare Analysis)
%  Empirically calibrated, exogenous wave shocks, welfare decomposition
% =========================================================================
clear; clc; close all;

%% ========================================================================
%  STEP 0: PARAMETERS
% =========================================================================

% --- Time Horizon ---
N    = 12;          % Q1.2020 to Q4.2022
beta = 0.99;

% --- Output Dynamics (Estimated) ---
rho_y      = 0.00;
psi        = 0.00424;
alpha_S    = 0.028;
alpha_F_CP = 0.253;
gamma_int  = -0.00596;
alpha_F_DI = 0.244;

% --- Epidemiology (Calibrated, wave-specific) ---
% Base transmission — will be SHOCKED at wave onsets
rho_theta_base = 1.30;    % Between waves: slow growth
phi_S          = 0.45;
delta_theta    = 0.5;

% --- Wave Shocks (exogenous theta injections) ---
% Each wave injects additional infections on top of endogenous dynamics
% Wave 1 (Q1.2020): Initial outbreak — captured by x0
% Wave 2 (Q3.2020): Second wave (Alpha)
% Wave 3 (Q1.2021): Third wave (Alpha/Beta)
% Wave 4 (Q3.2021): Delta variant
% Wave 5 (Q4.2021): Omicron (higher transmission, lower severity)

wave_quarters = [3,  5,  7,  8];    % k-index of wave onset
wave_shocks   = [0.03, 0.015, 0.025, 0.04]; % Additional theta injection (fraction)
wave_rho      = [1.30, 1.25, 1.40, 1.60];   % Elevated rho_theta during wave quarter
wave_ifr      = [0.03, 0.025, 0.02, 0.001]; % Declining IFR (vaccination + Omicron)

% Time-varying rho_theta and delta_theta
rho_theta_t   = rho_theta_base * ones(1, N);
delta_theta_t = delta_theta * ones(1, N);

for w = 1:length(wave_quarters)
    k_wave = wave_quarters(w);
    if k_wave <= N
        rho_theta_t(k_wave) = wave_rho(w);
        % Elevated transmission persists for 2 quarters
        if k_wave + 1 <= N
            rho_theta_t(k_wave + 1) = 0.5*(wave_rho(w) + rho_theta_base);
        end
        % Wave-specific IFR from this quarter onward
        delta_theta_t(k_wave:end) = wave_ifr(w);
    end
end

% --- Debt Dynamics (Estimated) ---
r          = 0.001;
gamma_y    = 0.191;
kappa_F_CP = 0.193;
kappa_F_DI = 0.268;
c_H        = 0.02;

% --- Dimensions ---
n_x = 6;  n_u = 3;

% --- Print ---
fprintf('=== V4: Pandemic with Waves ===\n');
fprintf('  Waves at quarters: '); fprintf('%d ', wave_quarters); fprintf('\n');
fprintf('  Theta shocks:      '); fprintf('%.3f ', wave_shocks); fprintf('\n');
fprintf('  rho_theta_t:       '); fprintf('%.2f ', rho_theta_t); fprintf('\n');
fprintf('  delta_theta_t:     '); fprintf('%.3f ', delta_theta_t); fprintf('\n');


%% ========================================================================
%  STEP 1: TIME-VARYING TRANSITION FUNCTION
% =========================================================================
% f is now time-varying because rho_theta and delta_theta change with waves

  beta_fear = -10.0;% Fear effect (calibrated, Goolsbee & Syverson 2021)% Kalibrieren
  

f_tv = @(x, u, k) [
    rho_y*x(1) + psi*u(1)*x(1) - alpha_S*u(1) ...
        + alpha_F_CP*u(3) + gamma_int*u(1)*u(3) + alpha_F_DI*x(6) ...
        + beta_fear * delta_theta_t(k) * x(4);
    delta_theta_t(k) * x(4);
    (1+r)*x(3) - gamma_y*x(1) + kappa_F_CP*u(3) + kappa_F_DI*x(5) + c_H*x(4);
    rho_theta_t(k) * (1 - phi_S*u(1)) * x(4);
    u(2);
    x(5)
];
% Pre-compute wave shock vector (length N+1, shock arrives at k+1)
wave_shock_vec = zeros(1, N+1);
for w = 1:length(wave_quarters)
    k_arrival = wave_quarters(w) + 1;  % Shock arrives after transition
    if k_arrival <= N+1
        wave_shock_vec(k_arrival) = wave_shocks(w);
    end
end

fprintf('  Wave shock vector: '); fprintf('%.3f ', wave_shock_vec); fprintf('\n');

% Combined: transition + pre-computed shock
f_wave = @(x, u, k) f_tv(x, u, k) + [0; 0; 0; wave_shock_vec(k+1); 0; 0];
%% ========================================================================
%  STEP 2: VALIDATE WAVE DYNAMICS (no control)
% =========================================================================
fprintf('\n--- Uncontrolled pandemic with waves ---\n');

x_unc = zeros(n_x, N+1);
x_unc(:, 1) = [0; 0; 0; 0.01; 0; 0];  % 1% initial infection

for k = 1:N
    x_unc(:, k+1) = f_wave(x_unc(:, k), [0; 0; 0], k);
end

fprintf('  theta (%%): '); fprintf('%.2f ', x_unc(4,:)*100); fprintf('\n');
fprintf('  d (%%):     '); fprintf('%.4f ', x_unc(2,:)*100); fprintf('\n');

figure('Name', 'Step 2: Uncontrolled Waves', 'Color', 'w');
subplot(2,2,1);
plot(0:N, x_unc(4,:)*100, 'r-o', 'LineWidth', 2); hold on;
for w = 1:length(wave_quarters)
    xline(wave_quarters(w), '--', sprintf('W%d', w+1), 'Color', [0.5 0 0]);
end
title('Infection Rate (\theta_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2,2,2);
plot(0:N, x_unc(2,:)*100, 'r-o', 'LineWidth', 2);
title('Excess Mortality (d_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2,2,3);
plot(0:N, x_unc(1,:)*100, 'b-o', 'LineWidth', 2);
yline(0, '--k'); title('Output Gap (no lockdown)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2,2,4);
stem(1:N, rho_theta_t, 'filled', 'MarkerSize', 6);
title('Effective \rho_\theta (time-varying)');
ylabel('\rho_\theta'); xlabel('Quarter'); grid on;

sgtitle('Uncontrolled Pandemic with Wave Shocks');
fprintf('  Verify: theta should show multiple peaks at wave onsets.\n');


%% ========================================================================
%  STEP 3: COST FUNCTION
% =========================================================================

% --- Weights ---
w_y = 25;
w_d = 1e8;          % High: mortality matters
w_b = 2.5;
W_b = 2.5;          % Terminal debt
r_S  = 30;          % Containment cost
r_DI = 15;          % DI usage cost
r_CP = 1;           % CP almost free to use (consistent with empirics)

Q_base = diag([w_y, w_d, w_b, 0, 0, 0]);
Q_N    = diag([0, 0, W_b, 0, 0, 0]);
R      = diag([r_S, r_DI, r_CP]);

% Calibration check
fprintf('\n--- Cost calibration ---\n');
y_typ = 0.05; d_typ = 0.001; b_typ = 0.05;
fprintf('  w_y*y^2 = %.3f,  w_d*d^2 = %.3f,  w_b*b^2 = %.3f\n', ...
        w_y*y_typ^2, w_d*d_typ^2, w_b*b_typ^2);


%% ========================================================================
%  STEP 4: AL-iLQR WITH WAVES
% =========================================================================

% --- Initial Conditions ---
x0 = [0; 0; 0; 0.01; 0; 0];

% --- Time-varying Jacobians ---
compute_Ak_tv = @(x_bar, u_bar, k) [
    rho_y + psi*u_bar(1), 0, 0, beta_fear * delta_theta_t(k) ,     0, alpha_F_DI;
    0,                     0, 0, delta_theta_t(k),                 0, 0;
    -gamma_y,              0, 1+r, c_H,                           kappa_F_DI, 0;
    0,                     0, 0, rho_theta_t(k)*(1-phi_S*u_bar(1)), 0, 0;
    0,                     0, 0, 0,                               0, 0;
    0,                     0, 0, 0,                               1, 0
];

compute_Bk_tv = @(x_bar, u_bar, k) [
    psi*x_bar(1) - alpha_S + gamma_int*u_bar(3), 0, alpha_F_CP + gamma_int*u_bar(1);
    0,                                            0, 0;
    0,                                            0, kappa_F_CP;
    -rho_theta_t(k)*phi_S*x_bar(4),             0, 0;
    0,                                            1, 0;
    0,                                            0, 0
];

% --- Heuristic Initial Trajectory ---
x_bar = zeros(n_x, N+1);
u_bar = zeros(n_u, N);
x_bar(:, 1) = x0;

for k = 1:N
    % Reactive heuristic: tighten after wave shocks
    base_S = max(0.1, 0.7 - 0.05*(k-1));
    % Extra tightening if wave hits
    if ismember(k, wave_quarters) || ismember(k-1, wave_quarters)
        base_S = min(0.86, base_S + 0.2);
    end
    base_CP = max(0, 0.08 - 0.006*(k-1));  % Front-loaded CP
    
    u_bar(:, k) = [base_S; 0.005; base_CP];
    x_bar(:, k+1) = f_wave(x_bar(:, k), u_bar(:, k), k);
end

fprintf('\n--- Initial heuristic trajectory ---\n');
fprintf('  S:     '); fprintf('%.2f ', u_bar(1,:)); fprintf('\n');
fprintf('  F_CP:  '); fprintf('%.3f ', u_bar(3,:)); fprintf('\n');
fprintf('  theta: '); fprintf('%.3f ', x_bar(4,:)*100); fprintf('\n');
fprintf('  y:     '); fprintf('%.2f ', x_bar(1,:)*100); fprintf('\n');

% --- AL-iLQR ---
max_outer_iter = 50;
max_inner_iter = 80;
tol_inner      = 1e-4;
tol_outer      = 1e-4;
phi_penalty    = 2.5;
rho_reg_base   = 1e-4;

% Bounds
S_max = 0.86; FDI_max = 0.30; FCP_max = 0.30;
lambda = zeros(6, N);
mu     = 10.0 * ones(6, N);

C_u = [-1  0  0;
        1  0  0;
        0 -1  0;
        0  1  0;
        0  0 -1;
        0  0  1];
c_bounds = [0; S_max; 0; FDI_max; 0; FCP_max];

fprintf('\n================================================================\n');
fprintf('  AL-iLQR V4 — Pandemic with Waves\n');
fprintf('================================================================\n\n');

for outer_iter = 1:max_outer_iter
    fprintf('--- Outer %d ---\n', outer_iter);

    for inner_iter = 1:max_inner_iter
        % Jacobians
        Ak = cell(N, 1); Bk = cell(N, 1);
        for k = 1:N
            Ak{k} = compute_Ak_tv(x_bar(:, k), u_bar(:, k), k);
            Bk{k} = compute_Bk_tv(x_bar(:, k), u_bar(:, k), k);
        end

        % Backward Riccati
        P = cell(N+1, 1); p = cell(N+1, 1);
        K = cell(N, 1);   k_ff = cell(N, 1);
        P{N+1} = Q_N * beta^N;
        p{N+1} = Q_N * beta^N * x_bar(:, N+1);

        rho_reg = rho_reg_base;
        backward_success = false;

        while ~backward_success && rho_reg < 1e6
            backward_success = true;
            for k = N:-1:1
                Q_k = Q_base * beta^(k-1);
                A = Ak{k}; B = Bk{k};

                c_val = C_u * u_bar(:, k) - c_bounds;
                active_idx = (c_val > 0) | (lambda(:, k) > 0);
                I_mu = diag(mu(:, k) .* active_idx);

                Qx  = Q_k * x_bar(:, k) + A' * p{k+1};
                Qxx = Q_k + A' * P{k+1} * A;
                Qux = B' * P{k+1} * A;
                Qu  = R * u_bar(:, k) + B' * p{k+1} + C_u' * (lambda(:, k) + I_mu * c_val);
                Quu = R + B' * P{k+1} * B + C_u' * I_mu * C_u;
                Quu = Quu + rho_reg * eye(n_u);

                [~, p_chol] = chol(Quu);
                if p_chol > 0
                    backward_success = false;
                    rho_reg = rho_reg * 10;
                    break;
                end

                K{k}    = Quu \ Qux;
                k_ff{k} = -Quu \ Qu;
                p{k} = Qx + K{k}' * Quu * k_ff{k};
                P{k} = Qxx - K{k}' * Quu * K{k};
            end
        end

        if ~backward_success
            fprintf('      Backward pass failed.\n'); break;
        end

        % Forward pass with line search + hard clipping
        alpha_ls = 1.0;
        cost_old = compute_al_cost_tv(x_bar, u_bar, Q_base, Q_N, R, beta, N, ...
                                       lambda, mu, C_u, c_bounds);
        line_search_success = false;

        while alpha_ls > 1e-8
            x_new = zeros(n_x, N+1);
            u_new = zeros(n_u, N);
            x_new(:, 1) = x0;

            for k = 1:N
                u_new(:, k) = u_bar(:, k) + alpha_ls * k_ff{k} ...
                              - K{k} * (x_new(:, k) - x_bar(:, k));
                % Hard clipping
                u_new(1, k) = max(0, min(S_max,   u_new(1, k)));
                u_new(2, k) = max(0, min(FDI_max, u_new(2, k)));
                u_new(3, k) = max(0, min(FCP_max, u_new(3, k)));
                
                x_new(:, k+1) = f_wave(x_new(:, k), u_new(:, k), k);
            end

            cost_new = compute_al_cost_tv(x_new, u_new, Q_base, Q_N, R, beta, N, ...
                                           lambda, mu, C_u, c_bounds);

            if cost_new < cost_old
                line_search_success = true; break;
            else
                alpha_ls = alpha_ls * 0.5;
            end
        end

        if ~line_search_success
            fprintf('      Line search failed at inner iter %d.\n', inner_iter);
            break;
        end

        delta_x = norm(x_new - x_bar, 'fro') / (norm(x_bar, 'fro') + 1e-12);
        x_bar = x_new; u_bar = u_new;

        if delta_x < tol_inner
            fprintf('    Inner converged in %d iters (delta=%.2e).\n', inner_iter, delta_x);
            break;
        end
    end

    % Outer AL updates
    max_viol = 0;
    for k = 1:N
        c_val = C_u * u_bar(:, k) - c_bounds;
        max_viol = max(max_viol, max(c_val));
        lambda(:, k) = max(0, lambda(:, k) + mu(:, k) .* c_val);
        mu(:, k) = mu(:, k) * phi_penalty;
    end

    fprintf('    Max violation: %.6f\n', max_viol);
    if max_viol < tol_outer
        fprintf('\n*** CONVERGED: Optimal policy found ***\n');
        break;
    end
end

x_opt = x_bar; u_opt = u_bar;

% Print optimal controls
fprintf('\n=== Optimal Controls ===\n');
fprintf('  S:    '); fprintf('%.3f ', u_opt(1,:)); fprintf('\n');
fprintf('  F_DI: '); fprintf('%.4f ', u_opt(2,:)); fprintf('\n');
fprintf('  F_CP: '); fprintf('%.4f ', u_opt(3,:)); fprintf('\n');


%% ========================================================================
%  STEP 5: WELFARE COST DECOMPOSITION
% =========================================================================

fprintf('\n=== Welfare Cost Decomposition ===\n');

% Total discounted cost per component
cost_output    = 0;
cost_mortality = 0;
cost_debt      = 0;
cost_S         = 0;
cost_DI        = 0;
cost_CP        = 0;

for k = 1:N
    disc = beta^(k-1);
    cost_output    = cost_output    + disc * w_y * x_opt(1,k)^2;
    cost_mortality = cost_mortality + disc * w_d * x_opt(2,k)^2;
    cost_debt      = cost_debt      + disc * w_b * x_opt(3,k)^2;
    cost_S         = cost_S         + disc * r_S  * u_opt(1,k)^2;
    cost_DI        = cost_DI        + disc * r_DI * u_opt(2,k)^2;
    cost_CP        = cost_CP        + disc * r_CP * u_opt(3,k)^2;
end

% Terminal cost
cost_debt_terminal = beta^N * W_b * x_opt(3,N+1)^2;
cost_debt_total    = cost_debt + cost_debt_terminal;

total_cost = cost_output + cost_mortality + cost_debt_total + cost_S + cost_DI + cost_CP;

fprintf('\n  --- State Costs (Welfare Losses) ---\n');
fprintf('    Output gap:      %.4f  (%.1f%%)\n', cost_output, cost_output/total_cost*100);
fprintf('    Mortality:       %.4f  (%.1f%%)\n', cost_mortality, cost_mortality/total_cost*100);
fprintf('    Debt (running):  %.4f  (%.1f%%)\n', cost_debt, cost_debt/total_cost*100);
fprintf('    Debt (terminal): %.4f  (%.1f%%)\n', cost_debt_terminal, cost_debt_terminal/total_cost*100);
fprintf('    Debt (total):    %.4f  (%.1f%%)\n', cost_debt_total, cost_debt_total/total_cost*100);

fprintf('\n  --- Control Costs (Policy Costs) ---\n');
fprintf('    Containment (S): %.4f  (%.1f%%)\n', cost_S, cost_S/total_cost*100);
fprintf('    DI usage:        %.4f  (%.1f%%)\n', cost_DI, cost_DI/total_cost*100);
fprintf('    CP usage:        %.4f  (%.1f%%)\n', cost_CP, cost_CP/total_cost*100);

fprintf('\n  --- Total Welfare Cost: %.4f ---\n', total_cost);

% Compare with no-intervention cost
cost_unc = 0;
for k = 1:N
    disc = beta^(k-1);
    cost_unc = cost_unc + disc * (w_y*x_unc(1,k)^2 + w_d*x_unc(2,k)^2 + w_b*x_unc(3,k)^2);
end
cost_unc = cost_unc + beta^N * W_b * x_unc(3,N+1)^2;

fprintf('\n  No-intervention cost: %.4f\n', cost_unc);
fprintf('  Optimal cost:         %.4f\n', total_cost);
fprintf('  Welfare gain:         %.1f%%\n', (1 - total_cost/cost_unc)*100);


%% ========================================================================
%  STEP 6: VISUALIZATION
% =========================================================================

quarter_labels = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21',...
                  'Q3.21','Q4.21','Q1.22','Q2.22','Q3.22','Q4.22'};
time = 0:N;

figure('Name', 'V4: Optimal Policy with Waves', 'Color', 'w', ...
       'Position', [50 50 1400 900]);

% Output Gap
subplot(3, 2, 1);
plot(time, x_unc(1,:)*100, 'r--', 'LineWidth', 1.2); hold on;
plot(time, x_opt(1,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
legend('No intervention', 'Optimal', 'Location', 'SouthEast');
title('Output Gap (y_k)'); ylabel('% deviation'); xlabel('Quarter'); grid on;

% Infection
subplot(3, 2, 2);
plot(time, x_unc(4,:)*100, 'r--', 'LineWidth', 1.2); hold on;
plot(time, x_opt(4,:)*100, 'Color', [0.6 0 0], 'LineWidth', 2);
for w = 1:length(wave_quarters)
    xline(wave_quarters(w), ':', sprintf('W%d', w+1), 'Color', [0.5 0 0], 'LineWidth', 1);
end
legend('No intervention', 'Optimal');
title('Infection Rate (\theta_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

% Debt
subplot(3, 2, 3);
plot(time, x_unc(3,:)*100, 'r--', 'LineWidth', 1.2); hold on;
plot(time, x_opt(3,:)*100, 'm-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
legend('No intervention', 'Optimal');
title('Public Debt (b_k)'); ylabel('pp of GDP'); xlabel('Quarter'); grid on;

% Mortality
subplot(3, 2, 4);
d_cum_unc = [0, cumsum(x_unc(2, 2:end))]*100;
d_flow_opt = x_opt(2, :)*100;
plot(0:N, d_cum_unc, 'r--', 'LineWidth', 1.5); hold on;
plot(0:N, d_flow_opt, 'Color', [0.8 0 0], 'LineWidth', 2);
legend('Cumulative (no intervention)', 'Quarterly flow (optimal)');
title('Excess Mortality (d_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

% Containment
subplot(3, 2, 5);
bar(1:N, u_opt(1,:)', 'FaceColor', [0.2 0.4 0.7]);
hold on;
yline(0.51, '--r', 'S^{crit}', 'LineWidth', 1.5);
title('Containment Intensity (S_k)'); ylabel('S [0, 1]');
ylim([0 1]); grid on;
set(gca, 'XTickLabel', quarter_labels, 'XTickLabelRotation', 45);

% Fiscal
subplot(3, 2, 6);
bar(1:N, [u_opt(2,:)', u_opt(3,:)'] * 100, 'grouped');
legend('F^{DI}', 'F^{CP}', 'Location', 'NorthEast');
title('Fiscal Composition'); ylabel('% of GDP'); grid on;
set(gca, 'XTickLabel', quarter_labels, 'XTickLabelRotation', 45);

sgtitle('Optimal Pandemic Policy with Wave Shocks (V4)', ...
        'FontWeight', 'bold', 'FontSize', 14);

% --- Welfare pie chart ---
figure('Name', 'Welfare Decomposition', 'Color', 'w');
labels = {'Output', 'Mortality', 'Debt', 'Containment', 'DI cost', 'CP cost'};
values = [cost_output, cost_mortality, cost_debt_total, cost_S, cost_DI, cost_CP];
% Remove near-zero components for cleaner pie
mask = values > 0.001 * total_cost;
pie(values(mask), labels(mask));
title('Welfare Cost Decomposition (Optimal Policy)');


%% === HELPER: Time-varying AL cost ===
function J = compute_al_cost_tv(x_traj, u_traj, Q_base, Q_N, R, beta, N, ...
                                 lambda, mu, C_u, c_bounds)
    J = 0;
    for k = 1:N
        step_cost = beta^(k-1) * ( ...
            0.5 * x_traj(:,k)' * Q_base * x_traj(:,k) + ...
            0.5 * u_traj(:,k)' * R * u_traj(:,k));
        c_val = C_u * u_traj(:, k) - c_bounds;
        active_idx = (c_val > 0) | (lambda(:, k) > 0);
        I_mu = diag(mu(:, k) .* active_idx);
        al_cost = lambda(:, k)' * c_val + 0.5 * c_val' * I_mu * c_val;
        J = J + step_cost + al_cost;
    end
    J = J + beta^N * (0.5 * x_traj(:,N+1)' * Q_N * x_traj(:,N+1));
end

%%Checks vs real Data

% OECD-Durchschnittsdaten (aus deinem Panel)
% Trage die empirischen Quartalsmittel manuell ein
S_observed   = [0.52 0.65 0.45 0.55 0.50 0.45 0.40 0.35 0.25 0.15 0.10 0.05];
CP_observed  = [0.04 0.03 0.01 0.01 0.01 0.005 0.003 0.002 0 0 0 0];
DI_observed  = [0.005 0.01 0.003 0.002 0.002 0.001 0 0 0 0 0 0];

figure('Name', 'Optimal vs Observed', 'Color', 'w');
subplot(1,3,1);
bar(1:N, [u_opt(1,:)', S_observed'], 'grouped');
legend('Optimal', 'OECD Average'); title('Containment'); ylabel('S');

subplot(1,3,2);
bar(1:N, [u_opt(3,:)'*100, CP_observed'*100], 'grouped');
legend('Optimal', 'OECD Average'); title('CP (% GDP)');

subplot(1,3,3);
bar(1:N, [u_opt(2,:)'*100, DI_observed'*100], 'grouped');
legend('Optimal', 'OECD Average'); title('DI (% GDP)');

%%Pareto Frontier
w_b_grid = [0.1, 0.5, 1, 2.5, 5, 10, 25, 50];
pareto_y = zeros(length(w_b_grid), 1);
pareto_d = zeros(length(w_b_grid), 1);
pareto_b = zeros(length(w_b_grid), 1);

for i = 1:length(w_b_grid)
    % Lauf iLQR mit w_b = w_b_grid(i)
    % Speichere finale x_opt(1,end), x_opt(2,end), x_opt(3,end)
end