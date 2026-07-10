%% ========================================================================
%  PANDEMIC SOCIAL PLANNER — SEQUENTIAL BUILD V3
%  Empirically calibrated from Pesenti (2026) panel estimation
%  Extended state space: x = [y; d; b; theta; z1; z2], u = [S; F_DI; F_CP]
% =========================================================================
%  Run each section (%%) individually with Ctrl+Enter.
%  Do NOT proceed to the next section until the current one validates.
% =========================================================================

clear; clc; close all;

%% ========================================================================
%  STEP 0: PARAMETER DEFINITION (Empirically Calibrated)
% =========================================================================
% All parameters in iLQR scale: y as fraction, S in [0,1], F as fraction
% of GDP. Interaction terms scaled by x100 from empirical estimates.

% --- Time Horizon ---
N    = 12;          % Quarters (Q1.2020 to Q4.2022)
beta = 0.99;        % Quarterly discount factor

% --- Output Dynamics (Estimated, Table 3 Col 3) ---
rho_y      = 0.00;      % Baseline persistence (insignificant, set to 0)
psi        = 0.00424;     % Lockdown-induced scarring (0.00424 * 100)
alpha_S    = 0.028;     % Containment flow damage
alpha_F_CP = 0.253;     % CP level effect (NEW — not in V2)
gamma_int  = -0.00596;    % S x CP interaction (-0.00596 * 100)
alpha_F_DI = 0.244;     % DI multiplier (lag 2, via z2)

% --- Epidemiology (Calibrated) ---
rho_theta   = 1.30;     % Quarterly transmission rate (wave-specific)
delta_theta = 0.03;     % Infection fatality rate
phi_S       = 0.45;     % Containment effectiveness on infections

% --- Debt Dynamics (Estimated, Table 4 Col 3) ---
r          = 0.001;     % Quarterly real interest rate
gamma_y    = 0.191;     % Automatic stabilizers
kappa_F_CP = 0.193;     % CP budget cost (pooled, estimated)
kappa_F_DI = 0.468;     % DI budget cost (lag 1, via z1)
c_H        = 0.02;      % Health cost per unit infection (calibrated)

% --- State and Control Dimensions ---
n_x = 6;   % [y; d; b; theta; z1; z2]
n_u = 3;   % [S; F_DI; F_CP]

% --- Print summary ---
fprintf('=== Parameters loaded (V3 — Empirically Calibrated) ===\n');
fprintf('  Output:  rho_y=%.3f, psi=%.3f, alpha_S=%.3f\n', rho_y, psi, alpha_S);
fprintf('           alpha_F_CP=%.3f, gamma=%.3f, alpha_F_DI=%.3f\n', ...
        alpha_F_CP, gamma_int, alpha_F_DI);
fprintf('  Debt:    gamma_y=%.3f, kappa_CP=%.3f, kappa_DI=%.3f\n', ...
        gamma_y, kappa_F_CP, kappa_F_DI);
fprintf('  Epi:     rho_theta=%.2f, phi_S=%.2f, delta_theta=%.3f\n', ...
        rho_theta, phi_S, delta_theta);
fprintf('  Thresholds:\n');
fprintf('    CP break-even: S* = %.2f\n', alpha_F_CP / abs(gamma_int));
fprintf('    Infection suppression: S_crit = %.2f\n', (1 - 1/rho_theta)/phi_S);


%% ========================================================================
%  STEP 1: DEFINE NON-LINEAR TRANSITION FUNCTION
% =========================================================================
%  Extended state: x = [y; d; b; theta; z1; z2],  u = [S; F_DI; F_CP]
%
%  y_{k+1}     = rho_y*y + psi*S*y - alpha_S*S + alpha_F_CP*F_CP
%                + gamma_int*S*F_CP + alpha_F_DI*z2
%  d_{k+1}     = delta_theta * theta
%  b_{k+1}     = (1+r)*b - gamma_y*y + kappa_CP*F_CP + kappa_DI*z1 + c_H*theta
%  theta_{k+1} = rho_theta*(1 - phi_S*S)*theta
%  z1_{k+1}    = F_DI           (stores current DI for debt at k+1)
%  z2_{k+1}    = z1             (stores lagged DI for output at k+2)

f = @(x, u) [
    rho_y*x(1) + psi*u(1)*x(1) - alpha_S*u(1) ...
        + alpha_F_CP*u(3) + gamma_int*u(1)*u(3) + alpha_F_DI*x(6);
    delta_theta * x(4);
    (1+r)*x(3) - gamma_y*x(1) + kappa_F_CP*u(3) + kappa_F_DI*x(5) + c_H*x(4);
    rho_theta * (1 - phi_S*u(1)) * x(4);
    u(2);       % z1_{k+1} = F_DI_k
    x(5)        % z2_{k+1} = z1_k
];

% Quick sanity: f at steady state must return zero
x_ss = zeros(n_x, 1);
u_ss = zeros(n_u, 1);
assert(norm(f(x_ss, u_ss)) < 1e-15, 'FAIL: f(0,0) must be zero.');
fprintf('\n=== Step 1 PASS: f(0,0) = 0 (steady state confirmed) ===\n');


%% ========================================================================
%  STEP 2: VALIDATE INFECTION DYNAMICS (theta only)
% =========================================================================
fprintf('\n--- Test: Infection dynamics (S=0) ---\n');
theta_0 = 0.005;  % 0.5% initial infection
theta_test = zeros(1, N+1);
theta_test(1) = theta_0;

for k = 1:N
    x_k = [0; 0; 0; theta_test(k); 0; 0];
    x_next = f(x_k, [0; 0; 0]);
    theta_test(k+1) = x_next(4);
end

theta_exact = theta_0 * rho_theta.^(0:N);

fprintf('  Simulated:  ');  fprintf('%.4f ', theta_test(1:6)); fprintf('...\n');
fprintf('  Analytical: ');  fprintf('%.4f ', theta_exact(1:6)); fprintf('...\n');
fprintf('  Max error: %.2e\n', max(abs(theta_test - theta_exact)));

figure('Name', 'Step 2: Infection Dynamics', 'Color', 'w');
plot(0:N, theta_test*100, 'ro-', 'LineWidth', 2, 'MarkerSize', 6); hold on;
plot(0:N, theta_exact*100, 'b--', 'LineWidth', 1.5);
legend('Simulated', 'Analytical: \theta_0 \cdot \rho_\theta^k');
title('Step 2: Uncontrolled Infection Growth');
ylabel('% population'); xlabel('Quarter'); grid on;

if max(abs(theta_test - theta_exact)) < 1e-10
    fprintf('  PASS: Infection dynamics correct.\n');
else
    fprintf('  *** FAIL: Infection dynamics incorrect! ***\n');
    return;
end


%% ========================================================================
%  STEP 3: VALIDATE OUTPUT DYNAMICS
% =========================================================================
%  Test 3a: S=0, no F -> y_k = rho_y^k * y_0 (immediate recovery since rho_y=0)
%  Test 3b: S=0.5 constant -> recession from containment
%  Test 3c: S=0.5 + F_CP=0.05 -> CP mitigates damage

fprintf('\n--- Test 3a: Output gap recovery WITHOUT S (rho_y=0) ---\n');
y_0 = -0.10;
y_test_a = zeros(1, N+1);
y_test_a(1) = y_0;

for k = 1:N
    x_k = [y_test_a(k); 0; 0; 0; 0; 0];
    x_next = f(x_k, [0; 0; 0]);
    y_test_a(k+1) = x_next(1);
end

fprintf('  y trajectory (%%): '); fprintf('%.1f  ', y_test_a*100); fprintf('\n');

% With rho_y=0, output should jump to 0 in one period
if abs(y_test_a(2)) < 1e-10
    fprintf('  PASS: Immediate recovery with rho_y=0 and S=0.\n');
else
    fprintf('  NOTE: rho_y=%.3f -> partial persistence (expected).\n', rho_y);
end

fprintf('\n--- Test 3b: Output gap WITH S=0.5 ---\n');
S_const = 0.5;
rho_eff = rho_y + psi * S_const;
y_ss = -alpha_S * S_const / (1 - rho_eff);

fprintf('  rho_y_eff = %.3f  (< 1 required for stability)\n', rho_eff);
fprintf('  Steady state y* = %.4f (%.1f%%)\n', y_ss, y_ss*100);

y_test_b = zeros(1, N+1);
y_test_b(1) = y_0;

for k = 1:N
    x_k = [y_test_b(k); 0; 0; 0; 0; 0];
    x_next = f(x_k, [S_const; 0; 0]);
    y_test_b(k+1) = x_next(1);
end

fprintf('  y trajectory (%%): '); fprintf('%.1f  ', y_test_b*100); fprintf('\n');

fprintf('\n--- Test 3c: Output gap WITH S=0.5 AND F_CP=0.05 ---\n');
F_CP_const = 0.05;
marginal_CP = alpha_F_CP + gamma_int * S_const;
fprintf('  Marginal CP effect at S=%.1f: %.3f (should be > 0)\n', ...
        S_const, marginal_CP);

y_test_c = zeros(1, N+1);
y_test_c(1) = y_0;

for k = 1:N
    x_k = [y_test_c(k); 0; 0; 0; 0; 0];
    x_next = f(x_k, [S_const; 0; F_CP_const]);
    y_test_c(k+1) = x_next(1);
end

fprintf('  y trajectory (%%): '); fprintf('%.1f  ', y_test_c*100); fprintf('\n');

% CP should improve output relative to no-CP
if all(y_test_c(2:end) >= y_test_b(2:end) - 1e-10)
    fprintf('  PASS: CP mitigates output loss.\n');
else
    fprintf('  *** CHECK: CP effect may not be working correctly ***\n');
end

figure('Name', 'Step 3: Output Dynamics', 'Color', 'w');
plot(0:N, y_test_a*100, 'b-o', 'LineWidth', 2); hold on;
plot(0:N, y_test_b*100, 'r-s', 'LineWidth', 2);
plot(0:N, y_test_c*100, 'g-d', 'LineWidth', 2);
yline(0, '--k');
yline(y_ss*100, ':r', sprintf('SS(S=0.5) = %.1f%%', y_ss*100));
legend('S=0 (recovery)', 'S=0.5 (lockdown)', 'S=0.5 + CP=5%', ...
       'Location', 'SouthEast');
title('Step 3: Output Gap — Containment and Capacity Preservation');
ylabel('% deviation'); xlabel('Quarter'); grid on;


%% ========================================================================
%  STEP 4: VALIDATE DEBT DYNAMICS
% =========================================================================
fprintf('\n--- Test 4a: Debt from automatic stabilizers ---\n');

b_test = zeros(1, N+1);
% Use S=0.5 trajectory to generate a recession
for k = 1:N
    x_k = [y_test_b(k); 0; b_test(k); 0; 0; 0];
    x_next = f(x_k, [S_const; 0; 0]);
    b_test(k+1) = x_next(3);
end

fprintf('  b trajectory (pp GDP): '); fprintf('%.2f  ', b_test*100); fprintf('\n');

% Debt should accumulate when y < 0
if all(b_test(2:end) > 0)
    fprintf('  PASS: Automatic stabilizers generate debt during recession.\n');
else
    fprintf('  *** FAIL: Debt should be positive! ***\n'); return;
end

fprintf('\n--- Test 4b: Debt from DI (lag structure) ---\n');
% Deploy DI in period 1 only, check when debt impact appears
b_test_di = zeros(1, N+1);
z1_test = zeros(1, N+1);
z2_test = zeros(1, N+1);
y_test_di = zeros(1, N+1);

for k = 1:N
    if k == 1
        u_k = [0; 0.05; 0];  % Deploy 5% GDP DI in period 1
    else
        u_k = [0; 0; 0];
    end
    x_k = [y_test_di(k); 0; b_test_di(k); 0; z1_test(k); z2_test(k)];
    x_next = f(x_k, u_k);
    y_test_di(k+1) = x_next(1);
    b_test_di(k+1) = x_next(3);
    z1_test(k+1) = x_next(5);
    z2_test(k+1) = x_next(6);
end

fprintf('  DI deployed in k=1: F_DI=0.05\n');
fprintf('  z1 (lag 1): '); fprintf('%.3f ', z1_test); fprintf('\n');
fprintf('  z2 (lag 2): '); fprintf('%.3f ', z2_test); fprintf('\n');
fprintf('  y  (%%):     '); fprintf('%.3f ', y_test_di*100); fprintf('\n');
fprintf('  b  (pp):    '); fprintf('%.3f ', b_test_di*100); fprintf('\n');

% Check: debt impact at k=2 (via z1), output impact at k=3 (via z2)
if abs(b_test_di(3)) > 0 && abs(y_test_di(4)) > 0
    fprintf('  PASS: DI lag structure working (debt at k+1, output at k+2).\n');
else
    fprintf('  *** CHECK: DI lag timing may be incorrect ***\n');
end

figure('Name', 'Step 4: Debt & DI Lag Dynamics', 'Color', 'w');
subplot(2,2,1);
plot(0:N, y_test_b*100, 'r-o', 'LineWidth', 2);
yline(0, '--k'); title('Output (S=0.5, input for debt)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2,2,2);
plot(0:N, b_test*100, 'm-o', 'LineWidth', 2);
yline(0, '--k'); title('Debt from auto-stabilizers');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

subplot(2,2,3);
plot(0:N, y_test_di*100, 'b-o', 'LineWidth', 2);
yline(0, '--k'); title('Output from DI impulse (lag 2)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2,2,4);
plot(0:N, b_test_di*100, 'm-o', 'LineWidth', 2);
yline(0, '--k'); title('Debt from DI impulse (lag 1)');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

sgtitle('Step 4: Debt Dynamics & DI Lag Structure');


%% ========================================================================
%  STEP 5: FULL UNCONTROLLED BENCHMARK
% =========================================================================
fprintf('\n--- Test 5: Full uncontrolled system ---\n');

x_unc = zeros(n_x, N+1);
x_unc(:, 1) = [0; 0; 0; 0.005; 0; 0];  % 0.5% initial infection

for k = 1:N
    x_unc(:, k+1) = f(x_unc(:, k), [0; 0; 0]);
end

fprintf('  y (%%):     '); fprintf('%7.2f', x_unc(1,:)*100); fprintf('\n');
fprintf('  d (%%):     '); fprintf('%7.4f', x_unc(2,:)*100); fprintf('\n');
fprintf('  b (pp):    '); fprintf('%7.2f', x_unc(3,:)*100); fprintf('\n');
fprintf('  theta (%%): '); fprintf('%7.2f', x_unc(4,:)*100); fprintf('\n');
fprintf('  z1:        '); fprintf('%7.4f', x_unc(5,:)); fprintf('\n');
fprintf('  z2:        '); fprintf('%7.4f', x_unc(6,:)); fprintf('\n');

figure('Name', 'Step 5: Uncontrolled Benchmark', 'Color', 'w', ...
       'Position', [50 50 1200 700]);

subplot(2,3,1);
plot(0:N, x_unc(1,:)*100, 'b-o', 'LineWidth', 2);
yline(0, '--k'); title('Output Gap (y_k)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2,3,2);
plot(0:N, x_unc(4,:)*100, 'r-o', 'LineWidth', 2);
title('Infection Rate (\theta_k)');
ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2,3,3);
plot(0:N, x_unc(3,:)*100, 'm-o', 'LineWidth', 2);
yline(0, '--k'); title('Public Debt (b_k)');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

subplot(2,3,4);
plot(0:N, x_unc(2,:)*100, 'r-o', 'LineWidth', 2);
title('Excess Mortality (d_k)');
ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2,3,5);
plot(0:N, x_unc(5,:), 'k-o', 'LineWidth', 2);
title('z_1 (DI lag 1)'); ylabel('Fraction'); xlabel('Quarter'); grid on;

subplot(2,3,6);
plot(0:N, x_unc(6,:), 'k-o', 'LineWidth', 2);
title('z_2 (DI lag 2)'); ylabel('Fraction'); xlabel('Quarter'); grid on;

sgtitle('Step 5: Catastrophe Benchmark (u = 0)', 'FontWeight', 'bold');

fprintf('\n  No-intervention: theta explodes, y=0 (no lockdown = no recession),\n');
fprintf('  debt from health costs only. Proceed to Step 6.\n');


%% ========================================================================
%  STEP 6: COST FUNCTION CALIBRATION
% =========================================================================
fprintf('\n--- Step 6: Cost function calibration ---\n');

% Weights (to be refined via Pareto frontier)
w_y = 25;
w_d = 1e9;
w_b = 2.5;
W_b = 2.5;        % Terminal debt weight
r_S = 50;
r_DI = 20;
r_CP = 11;

% State cost: penalize y, d, b (not theta, z1, z2 directly)
Q_base = diag([w_y, w_d, w_b, 0, 0, 0]);
Q_N    = diag([0, 0, W_b, 0, 0, 0]);
R      = diag([r_S, r_DI, r_CP]);

% Peak magnitudes from uncontrolled scenario
y_typ     = max(abs(x_unc(1, :)));
d_typ     = max(abs(x_unc(2, :)));
b_typ     = max(abs(x_unc(3, :)));
theta_typ = max(abs(x_unc(4, :)));

fprintf('\n  Peak magnitudes (uncontrolled):\n');
fprintf('    |y|_max     = %.4f  (%.1f%%)\n', y_typ, y_typ*100);
fprintf('    |d|_max     = %.6f  (%.4f%%)\n', d_typ, d_typ*100);
fprintf('    |b|_max     = %.4f  (%.1f pp)\n', b_typ, b_typ*100);
fprintf('    |theta|_max = %.4f  (%.1f%%)\n', theta_typ, theta_typ*100);

cost_y = w_y * y_typ^2;
cost_d = w_d * d_typ^2;
cost_b = w_b * b_typ^2;

fprintf('\n  Effective cost contributions (w * x_typ^2):\n');
fprintf('    Output:    %8.1f * %.6f^2 = %.6f\n', w_y, y_typ, cost_y);
fprintf('    Mortality: %8.1f * %.6f^2 = %.6f\n', w_d, d_typ, cost_d);
fprintf('    Debt:      %8.1f * %.6f^2 = %.6f\n', w_b, b_typ, cost_b);

total = cost_y + cost_d + cost_b;
fprintf('\n  Relative shares:\n');
fprintf('    Output:    %.1f%%\n', cost_y/total*100);
fprintf('    Mortality: %.1f%%\n', cost_d/total*100);
fprintf('    Debt:      %.1f%%\n', cost_b/total*100);

fprintf('\n  Desired priority: Mortality >> Output > Debt\n');
fprintf('  Adjust weights above if shares do not reflect this ordering.\n');
fprintf('\n  If satisfied, proceed to Step 7 (iLQR).\n');

%% ========================================================================
%  STEP 7: AL-iLQR OPTIMIZATION
% =========================================================================

% --- 1. Initial Conditions ---
x0 = [0; 0; 0; 0.005; 0; 0];  % 0.5% initial infection, at potential

% --- 2. Dynamics Functions ---
f_nonlin = @(x, u, k) [
    rho_y*x(1) + psi*u(1)*x(1) - alpha_S*u(1) ...
        + alpha_F_CP*u(3) + gamma_int*u(1)*u(3) + alpha_F_DI*x(6);
    delta_theta * x(4);
    (1+r)*x(3) - gamma_y*x(1) + kappa_F_CP*u(3) + kappa_F_DI*x(5) + c_H*x(4);
    rho_theta * (1 - phi_S*u(1)) * x(4);
    u(2);
    x(5)
];

% --- Jacobian A = df/dx (6x6) ---
compute_Ak = @(x_bar, u_bar, k) [
    rho_y + psi*u_bar(1), 0, 0, 0,                            0, alpha_F_DI;
    0,                     0, 0, delta_theta,                   0, 0;
    -gamma_y,              0, 1+r, c_H,                        kappa_F_DI, 0;
    0,                     0, 0, rho_theta*(1-phi_S*u_bar(1)),  0, 0;
    0,                     0, 0, 0,                            0, 0;
    0,                     0, 0, 0,                            1, 0
];

% --- Jacobian B = df/du (6x3) ---
%   Columns: S, F_DI, F_CP
compute_Bk = @(x_bar, u_bar, k) [
    psi*x_bar(1) - alpha_S + gamma_int*u_bar(3), 0, alpha_F_CP + gamma_int*u_bar(1);
    0,                                            0, 0;
    0,                                            0, kappa_F_CP;
    -rho_theta*phi_S*x_bar(4),                   0, 0;
    0,                                            1, 0;
    0,                                            0, 0
];


% --- 3. Initial Trajectory (heuristic policy) ---
x_bar = zeros(n_x, N+1);
u_bar = zeros(n_u, N);
x_bar(:, 1) = x0;

for k = 1:N
    % Heuristic: moderate lockdown, some CP, no DI
    if k <= 4
        u_bar(:, k) = [0.50; 0.00; 0.05];   % First year: S=0.5, CP=5%
    elseif k <= 8
        u_bar(:, k) = [0.30; 0.00; 0.02];   % Second year: relax
    else
        u_bar(:, k) = [0.10; 0.00; 0.00];   % Third year: minimal
    end
    x_bar(:, k+1) = f_nonlin(x_bar(:, k), u_bar(:, k), k);
end

% Verify: theta should be controlled, not exploding
fprintf('  Initial trajectory theta: ');
fprintf('%.4f ', x_bar(4,:)*100); fprintf('\n');
fprintf('  Initial trajectory y:     ');
fprintf('%.2f ', x_bar(1,:)*100); fprintf('\n');

% --- 4. AL-iLQR Hyperparameters ---
max_outer_iter = 20;
max_inner_iter = 50;
tol_inner      = 1e-4;
tol_outer      = 1e-4;
phi_penalty    = 2.0;
rho_reg_base   = 1e-4;

% --- 5. Constraint Setup ---
% Bounds (iLQR scale): S in [0,0.86], F_DI in [0,0.08], F_CP in [0,0.30]
lambda = zeros(6, N);
mu     = 1.0 * ones(6, N);

% Constraint Jacobian: [-S; S-S_max; -F_DI; F_DI-F_DI_max; -F_CP; F_CP-F_CP_max]
C_u = [-1  0  0;
        1  0  0;
        0 -1  0;
        0  1  0;
        0  0 -1;
        0  0  1];

S_max = 0.86; FDI_max = 0.08; FCP_max = 0.30;
c_bounds = [0; S_max; 0; FDI_max; 0; FCP_max];

fprintf('================================================================\n');
fprintf('  AL-iLQR — Pandemic Social Planner (V3, Empirically Calibrated)\n');
fprintf('  State dim: %d, Control dim: %d, Horizon: %d quarters\n', n_x, n_u, N);
fprintf('  Bounds: S=[0,%.2f], F_DI=[0,%.2f], F_CP=[0,%.2f]\n', S_max, FDI_max, FCP_max);
fprintf('================================================================\n\n');

% --- 6. Outer AL Loop ---
for outer_iter = 1:max_outer_iter
    fprintf('--- Outer Iteration %d ---\n', outer_iter);

    % --- Inner iLQR Loop ---
    for inner_iter = 1:max_inner_iter
        % 1. Compute time-varying Jacobians
        Ak = cell(N, 1); Bk = cell(N, 1);
        for k = 1:N
            Ak{k} = compute_Ak(x_bar(:, k), u_bar(:, k), k);
            Bk{k} = compute_Bk(x_bar(:, k), u_bar(:, k), k);
        end

        % 2. Backward Riccati recursion
        P = cell(N+1, 1); p = cell(N+1, 1);
        K = cell(N, 1);   k_ff = cell(N, 1);

        P{N+1} = Q_N * beta^N;
        p{N+1} = Q_N * beta^N * x_bar(:, N+1);

        rho_reg = rho_reg_base;
        backward_success = false;

        while ~backward_success && rho_reg < 1e4
            backward_success = true;
            for k = N:-1:1
                Q_k = Q_base * beta^(k-1);
                A = Ak{k}; B = Bk{k};

                % Constraint violations
                c_val = C_u * u_bar(:, k) - c_bounds;
                active_idx = (c_val > 0) | (lambda(:, k) > 0);
                I_mu = diag(mu(:, k) .* active_idx);

                % Q-function expansions
                Qx  = Q_k * x_bar(:, k) + A' * p{k+1};
                Qxx = Q_k + A' * P{k+1} * A;
                Qux = B' * P{k+1} * A;

                Qu  = R * u_bar(:, k) + B' * p{k+1} ...
                      + C_u' * (lambda(:, k) + I_mu * c_val);
                Quu = R + B' * P{k+1} * B + C_u' * I_mu * C_u;

                % Regularization
                Quu = Quu + rho_reg * eye(n_u);

                % Check positive definiteness
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
            fprintf('      Backward pass failed.\n');
            break;
        end

        % 3. Forward simulation with line search
        alpha_ls = 1.0;
        cost_old = compute_al_cost(x_bar, u_bar, Q_base, Q_N, R, beta, N, ...
                                    lambda, mu, C_u, c_bounds, n_x);
        line_search_success = false;

        while alpha_ls > 1e-8
            x_new = zeros(n_x, N+1);
            u_new = zeros(n_u, N);
            x_new(:, 1) = x0;

            for k = 1:N
                u_new(:, k) = u_bar(:, k) + alpha_ls * k_ff{k} ...
                              - K{k} * (x_new(:, k) - x_bar(:, k));
                x_new(:, k+1) = f_nonlin(x_new(:, k), u_new(:, k), k);
            end

            cost_new = compute_al_cost(x_new, u_new, Q_base, Q_N, R, beta, N, ...
                                        lambda, mu, C_u, c_bounds, n_x);

            if cost_new < cost_old
                line_search_success = true;
                break;
            else
                alpha_ls = alpha_ls * 0.5;
            end
        end

        if ~line_search_success
            fprintf('      Line search failed.\n');
            break;
        end

        % Convergence check
        delta_x = norm(x_new - x_bar, 'fro') / (norm(x_bar, 'fro') + 1e-12);
        x_bar = x_new; u_bar = u_new;

        if delta_x < tol_inner
            fprintf('    Inner loop converged in %d iterations.\n', inner_iter);
            break;
        end
    end

    % --- 4. Outer AL Updates ---
    max_viol = 0;
    for k = 1:N
        c_val = C_u * u_bar(:, k) - c_bounds;
        max_viol = max(max_viol, max(c_val));
        lambda(:, k) = max(0, lambda(:, k) + mu(:, k) .* c_val);
        mu(:, k) = mu(:, k) * phi_penalty;
    end

    fprintf('    Max Constraint Violation: %.6f\n', max_viol);
    if max_viol < tol_outer
        fprintf('\n*** Optimal Policy Found: Constraints Satisfied ***\n');
        break;
    end
end

x_opt = x_bar; u_opt = u_bar;


%% ========================================================================
%  STEP 8: VISUALIZATION
% =========================================================================

time = 0:N;
quarter_labels = arrayfun(@(k) sprintf('Q%d.%d', mod(k-1,4)+1, 2020+floor((k-1)/4)), ...
                          1:N, 'UniformOutput', false);

figure('Name', 'Optimal Pandemic Policy — V3', 'Color', 'w', ...
       'Position', [50 50 1400 900]);

% --- State Variables ---
subplot(3, 2, 1);
plot(time, x_opt(1,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
title('Output Gap (y_k)'); ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(3, 2, 2);
plot(time, x_opt(4,:)*100, 'Color', [0.6 0 0], 'LineWidth', 2);
title('Infection Rate (\theta_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

subplot(3, 2, 3);
plot(time, x_opt(3,:)*100, 'm-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
title('Public Debt (b_k)'); ylabel('pp of pre-pandemic GDP'); xlabel('Quarter'); grid on;

subplot(3, 2, 4);
plot(time, x_opt(2,:)*100, 'r-', 'LineWidth', 2);
title('Excess Mortality (d_k)'); ylabel('% population'); xlabel('Quarter'); grid on;

% --- Control Variables ---
subplot(3, 2, 5);
bar(1:N, u_opt(1,:)', 'FaceColor', [0.2 0.4 0.7]);
title('Containment Intensity (S_k)'); ylabel('Intensity [0, 1]');
ylim([0 1.1]); xlabel('Quarter'); grid on;
set(gca, 'XTickLabel', quarter_labels);

subplot(3, 2, 6);
bar(1:N, [u_opt(2,:)', u_opt(3,:)'] * 100, 'grouped');
legend('Demand Injection (F^{DI})', 'Capacity Preservation (F^{CP})', ...
       'Location', 'NorthEast');
title('Fiscal Composition'); ylabel('% of GDP'); xlabel('Quarter'); grid on;
set(gca, 'XTickLabel', quarter_labels);

sgtitle('Optimal Pandemic Policy (V3 — Empirically Calibrated)', ...
        'FontWeight', 'bold', 'FontSize', 14);


%% ========================================================================
%  STEP 9: COMPARE OPTIMAL vs. UNCONTROLLED vs. OBSERVED
% =========================================================================

figure('Name', 'Policy Comparison', 'Color', 'w', ...
       'Position', [50 50 1200 600]);

subplot(1,3,1);
plot(time, x_unc(1,:)*100, 'r--', 'LineWidth', 1.5); hold on;
plot(time, x_opt(1,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k'); legend('No intervention', 'Optimal');
title('Output Gap'); ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(1,3,2);
plot(time, x_unc(2,:)*100, 'r--', 'LineWidth', 1.5); hold on;
plot(time, x_opt(2,:)*100, 'b-', 'LineWidth', 2);
legend('No intervention', 'Optimal');
title('Excess Mortality'); ylabel('% population'); xlabel('Quarter'); grid on;

subplot(1,3,3);
plot(time, x_unc(3,:)*100, 'r--', 'LineWidth', 1.5); hold on;
plot(time, x_opt(3,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k'); legend('No intervention', 'Optimal');
title('Public Debt'); ylabel('pp of GDP'); xlabel('Quarter'); grid on;

sgtitle('Optimal vs. No-Intervention Trajectory', 'FontWeight', 'bold');


%% === HELPER FUNCTION: Augmented Lagrangian Cost ===
function J = compute_al_cost(x_traj, u_traj, Q_base, Q_N, R, beta, N, ...
                              lambda, mu, C_u, c_bounds, n_x)
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