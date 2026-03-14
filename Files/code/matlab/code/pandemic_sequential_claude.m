%% ========================================================================
%  PANDEMIC SOCIAL PLANNER — SEQUENTIAL BUILD
%  Step-by-step validation before optimization
% =========================================================================
%  Run each section (%%) individually with Ctrl+Enter.
%  Do NOT proceed to the next section until the current one validates.
% =========================================================================

clear; clc; close all;

%% ========================================================================
%  STEP 0: PARAMETER DEFINITION
% =========================================================================

% --- Time Horizon ---
N    = 12;          % Quarters
beta = 0.99;        % Quarterly discount factor

% --- Output Dynamics ---
rho_y   = 0.85;     % Baseline persistence (Cerra et al., 2023)
psi     = 0.10;     % Hysteresis sensitivity to lockdown

% --- Epidemiology ---
rho_theta   = 1.30;  % Transmission rate (>1: explosive)
delta_theta = 0.01;  % Infection fatality rate

% --- Containment (S) ---
alpha_S = 0.15;      % Contractionary effect on output
phi_S   = 0.60;      % Suppression effectiveness on infections

% --- Demand Injection (F_DI) ---
alpha_F_DI = 0.80;   % Fiscal multiplier
kappa_F_DI = 0.95;   % Budgetary cost per unit

% --- Capacity Preservation (F_CP) ---
eta        = 0.15;   % Effectiveness on persistence reduction
kappa_F_CP = 0.40;   % Budgetary cost per unit

% --- Debt Dynamics ---
r       = 0.001;     % Quarterly real interest rate
gamma_y = 0.12;      % Budget semi-elasticity (auto-stabilizers)
c_H     = 0.20;      % Pandemic health cost per unit infection

% --- Print summary ---
fprintf('=== Parameters loaded ===\n');
fprintf('  rho_y=%.2f, psi=%.2f, rho_theta=%.2f\n', rho_y, psi, rho_theta);
fprintf('  alpha_S=%.2f, phi_S=%.2f, alpha_F_DI=%.2f, eta=%.2f\n', ...
        alpha_S, phi_S, alpha_F_DI, eta);
fprintf('  gamma_y=%.2f, c_H=%.2f, r=%.3f\n', gamma_y, c_H, r);


%% ========================================================================
%  STEP 1: DEFINE NON-LINEAR TRANSITION FUNCTION
% =========================================================================
%
%  x = [y; d; b; theta],  u = [S; F_DI; F_CP]
%
%  y_{k+1}     = [rho_y + psi*S - eta*F_CP] * y  -  alpha_S*S  +  alpha_F_DI*F_DI
%  d_{k+1}     = delta_theta * theta
%  b_{k+1}     = (1+r)*b  -  gamma_y*y  +  kappa_DI*F_DI  +  kappa_CP*F_CP  +  c_H*theta
%  theta_{k+1} = rho_theta*(1 - phi_S*S)*theta

f = @(x, u) [
    (rho_y + psi*u(1) - eta*u(3)) * x(1)  -  alpha_S*u(1)  +  alpha_F_DI*u(2);
    delta_theta * x(4);
    (1+r)*x(3)  -  gamma_y*x(1)  +  kappa_F_DI*u(2)  +  kappa_F_CP*u(3)  +  c_H*x(4);
    rho_theta * (1 - phi_S*u(1)) * x(4)
];

% Quick sanity: f at steady state must return zero
assert(norm(f([0;0;0;0], [0;0;0])) < 1e-15, 'FAIL: f(0,0) must be zero.');
fprintf('\n=== Step 1 PASS: f(0,0) = 0 (steady state confirmed) ===\n');


%% ========================================================================
%  STEP 2: VALIDATE INFECTION DYNAMICS (theta only)
% =========================================================================
%  Expect: theta_k = theta_0 * rho_theta^k  (explosive growth)

fprintf('\n--- Test: Infection dynamics (S=0) ---\n');
theta_0 = 0.05;
theta_test = zeros(1, N+1);
theta_test(1) = theta_0;

for k = 1:N
    x_k = [0; 0; 0; theta_test(k)];
    x_next = f(x_k, [0; 0; 0]);
    theta_test(k+1) = x_next(4);
end

% Analytical prediction
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
    return;  % Stop here
end


%% ========================================================================
%  STEP 3: VALIDATE OUTPUT DYNAMICS (y only, no pandemic)
% =========================================================================
%  Test 3a: S=0 -> y_k = rho_y^k * y_0 (exponential recovery)
%  Test 3b: S=0.5 constant -> deeper recession, slower recovery

fprintf('\n--- Test 3a: Output gap recovery WITHOUT S ---\n');
y_0 = -0.10;
y_test_a = zeros(1, N+1);
y_test_a(1) = y_0;

for k = 1:N
    x_k = [y_test_a(k); 0; 0; 0];
    x_next = f(x_k, [0; 0; 0]);
    y_test_a(k+1) = x_next(1);
end

y_exact_a = y_0 * rho_y.^(0:N);

fprintf('  Simulated:  ');  fprintf('%.4f ', y_test_a(1:6)); fprintf('...\n');
fprintf('  Analytical: ');  fprintf('%.4f ', y_exact_a(1:6)); fprintf('...\n');
fprintf('  Max error: %.2e\n', max(abs(y_test_a - y_exact_a)));

if max(abs(y_test_a - y_exact_a)) < 1e-10
    fprintf('  PASS: Output dynamics (S=0) correct.\n');
else
    fprintf('  *** FAIL ***\n'); return;
end

fprintf('\n--- Test 3b: Output gap WITH S=0.5 ---\n');
S_const = 0.5;
rho_eff = rho_y + psi * S_const;  % No F_CP -> eta term is 0
y_ss    = -alpha_S * S_const / (1 - rho_eff);  % Steady state

fprintf('  rho_y_eff = %.3f  (< 1, so system converges)\n', rho_eff);
fprintf('  Steady state y* = %.4f (%.1f%%)\n', y_ss, y_ss*100);

y_test_b = zeros(1, N+1);
y_test_b(1) = y_0;

for k = 1:N
    x_k = [y_test_b(k); 0; 0; 0];
    x_next = f(x_k, [S_const; 0; 0]);
    y_test_b(k+1) = x_next(1);
end

fprintf('  y trajectory (%%): ');
fprintf('%.1f  ', y_test_b*100);
fprintf('\n');

% Verify: must be BELOW y_test_a at all k > 0
if all(y_test_b(2:end) < y_test_a(2:end))
    fprintf('  PASS: S=0.5 produces deeper recession than S=0.\n');
else
    fprintf('  *** FAIL: S should deepen the recession! ***\n'); return;
end

figure('Name', 'Step 3: Output Dynamics', 'Color', 'w');
plot(0:N, y_test_a*100, 'b-o', 'LineWidth', 2); hold on;
plot(0:N, y_test_b*100, 'r-s', 'LineWidth', 2);
yline(0, '--k');
yline(y_ss*100, ':r', sprintf('SS = %.1f%%', y_ss*100));
legend('S = 0', 'S = 0.5', 'Location', 'SouthEast');
title('Step 3: Output Gap — Effect of Containment');
ylabel('% deviation'); xlabel('Quarter'); grid on;


%% ========================================================================
%  STEP 4: VALIDATE DEBT DYNAMICS (automatic stabilizers)
% =========================================================================
%  With y < 0 and no fiscal instruments: debt must accumulate
%  because -gamma_y * y_k > 0 when y_k < 0

fprintf('\n--- Test 4: Debt from automatic stabilizers ---\n');

b_test = zeros(1, N+1);
y_for_debt = y_test_a;  % Use the S=0 output trajectory from Step 3

for k = 1:N
    x_k = [y_for_debt(k); 0; b_test(k); 0];  % No pandemic (theta=0)
    x_next = f(x_k, [0; 0; 0]);
    b_test(k+1) = x_next(3);
end

fprintf('  b trajectory (pp GDP): ');
fprintf('%.2f  ', b_test*100);
fprintf('\n');

% Verify: debt must be positive and growing (from stabilizers)
if all(b_test(2:end) > 0) && all(diff(b_test(2:end)) >= 0)
    fprintf('  PASS: Automatic stabilizers generate debt during recession.\n');
else
    fprintf('  *** FAIL: Debt should be positive and accumulating! ***\n');
    return;
end

figure('Name', 'Step 4: Debt Dynamics', 'Color', 'w');
subplot(1,2,1);
plot(0:N, y_for_debt*100, 'b-o', 'LineWidth', 2);
yline(0, '--k'); title('Output gap (input)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(1,2,2);
plot(0:N, b_test*100, 'm-o', 'LineWidth', 2);
yline(0, '--k'); title('Debt from auto-stabilizers');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

sgtitle('Step 4: Automatic Stabilizer Channel');


%% ========================================================================
%  STEP 5: VALIDATE FULL SYSTEM — UNCONTROLLED BENCHMARK
% =========================================================================
%  All states interact, u = 0. This is the "catastrophe trajectory".
%  Expect: theta explodes, d follows theta, b grows from c_H*theta,
%          y stays at 0 (no containment -> no recession).

fprintf('\n--- Test 5: Full uncontrolled system ---\n');

x_unc = zeros(4, N+1);
x_unc(:, 1) = [0; 0; 0; 0.05];  % Pandemic shock only

for k = 1:N
    x_unc(:, k+1) = f(x_unc(:, k), [0; 0; 0]);
end

fprintf('  y (%%):     '); fprintf('%7.2f', x_unc(1,:)*100); fprintf('\n');
fprintf('  d (%%):     '); fprintf('%7.4f', x_unc(2,:)*100); fprintf('\n');
fprintf('  b (pp):    '); fprintf('%7.2f', x_unc(3,:)*100); fprintf('\n');
fprintf('  theta (%%): '); fprintf('%7.2f', x_unc(4,:)*100); fprintf('\n');

figure('Name', 'Step 5: Uncontrolled Benchmark', 'Color', 'w', ...
       'Position', [50 50 1200 700]);

subplot(2,2,1);
plot(0:N, x_unc(1,:)*100, 'b-o', 'LineWidth', 2);
yline(0, '--k'); title('Output Gap (y_k)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2,2,2);
plot(0:N, x_unc(4,:)*100, 'r-o', 'LineWidth', 2);
title('Infection Rate (\theta_k)');
ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2,2,3);
plot(0:N, x_unc(3,:)*100, 'm-o', 'LineWidth', 2);
yline(0, '--k'); title('Public Debt (b_k)');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

subplot(2,2,4);
plot(0:N, x_unc(2,:)*100, 'r-o', 'LineWidth', 2);
title('Excess Mortality (d_k)');
ylabel('% population'); xlabel('Quarter'); grid on;

sgtitle('Step 5: Catastrophe Benchmark (u = 0)', 'FontWeight', 'bold');

fprintf('\n  This is the do-nothing trajectory. All states should diverge.\n');
fprintf('  If this looks correct, proceed to Step 6 (cost function calibration).\n');


%% ========================================================================
%  STEP 6: COST FUNCTION CALIBRATION CHECK
% =========================================================================
%  Compute the effective penalty contribution of each state variable
%  using the uncontrolled benchmark magnitudes from Step 5.
%  Goal: verify that the relative costs reflect the desired priority.

fprintf('\n--- Step 6: Cost function calibration ---\n');

% Weights
w_y = 10;    w_d = 500000;    w_b = 0.5;    W_b = 10;
r_S = 0.01; r_DI = 0.05;   r_CP = 0.03;

Q_base = diag([w_y, w_d, w_b, 0]);
Q_N    = diag([0, 0, W_b, 0]);
R      = diag([r_S, r_DI, r_CP]);

% Typical magnitudes from uncontrolled scenario (peak values)
y_typ     = max(abs(x_unc(1, :)));
d_typ     = max(abs(x_unc(2, :)));
b_typ     = max(abs(x_unc(3, :)));
theta_typ = max(abs(x_unc(4, :)));

fprintf('\n  Peak magnitudes (uncontrolled):\n');
fprintf('    |y|_max     = %.4f  (%.1f%%)\n', y_typ, y_typ*100);
fprintf('    |d|_max     = %.4f  (%.2f%%)\n', d_typ, d_typ*100);
fprintf('    |b|_max     = %.4f  (%.1f pp)\n', b_typ, b_typ*100);
fprintf('    |theta|_max = %.4f  (%.1f%%)\n', theta_typ, theta_typ*100);

% Effective cost contribution: w * x_typ^2
cost_y = w_y * y_typ^2;
cost_d = w_d * d_typ^2;
cost_b = w_b * b_typ^2;

fprintf('\n  Effective cost contributions (w * x_typ^2):\n');
fprintf('    Output:    w_y * y^2     = %8.1f * %.4f^2 = %.4f\n', w_y, y_typ, cost_y);
fprintf('    Mortality: w_d * d^2     = %8.1f * %.4f^2 = %.4f\n', w_d, d_typ, cost_d);
fprintf('    Debt:      w_b * b^2     = %8.1f * %.4f^2 = %.4f\n', w_b, b_typ, cost_b);

total = cost_y + cost_d + cost_b;
fprintf('\n  Relative shares:\n');
fprintf('    Output:    %.1f%%\n', cost_y/total*100);
fprintf('    Mortality: %.1f%%\n', cost_d/total*100);
fprintf('    Debt:      %.1f%%\n', cost_b/total*100);

fprintf('\n  Desired priority: Mortality >> Output > Debt\n');
fprintf('  Adjust weights if shares do not reflect this ordering.\n');
fprintf('\n  If satisfied, proceed to Step 7 (iLQR).\n');

%WAS GENAU BEDEUTET DAS?
%% ========================================================================
%  STEP 7: iLQR OPTIMIZATION
% =========================================================================
%  Now that all equations and weights are validated, run the optimizer.
%  PLACEHOLDER — add iLQR code here only after Steps 1-6 all pass.

fprintf('\n=== READY FOR iLQR ===\n');
fprintf('  All equation tests passed.\n');
fprintf('  Cost calibration reviewed.\n');
fprintf('  Paste iLQR code (Section 5 from iLQR_pandemic.m) below.\n');

%% STEP 7: iLQR
% --- Initial condition and noise ---
x0 = [0; 0; 0; 0.001];

sigma_noise = 0.005;
rng(42);
noise_vec = zeros(4, N);
noise_vec(4, :) = randn(1, N) * sigma_noise;

%% ========================================================================
%  HELPER FUNCTIONS: NON-LINEAR DYNAMICS AND JACOBIANS
% =========================================================================

% --- Non-linear transition function f(x, u) ---
%   Returns x_{k+1} given x_k and u_k (deterministic part)
f_nonlin = @(x, u) [
    (rho_y + psi*u(1) - eta*u(3)) * x(1) - alpha_S*u(1) + alpha_F_DI*u(2);
    delta_theta * x(4);
    (1+r)*x(3) - gamma_y*x(1) + kappa_F_DI*u(2) + kappa_F_CP*u(3) + c_H*x(4);
    rho_theta * (1 - phi_S*u(1)) * x(4)
];

% --- Jacobian A_k = df/dx evaluated at (x_bar, u_bar) ---
%   All three bilinear terms produce time-varying entries:
%     A(1,1): rho_y + psi*S_bar - eta*F_CP_bar   (from psi*S*y and eta*F_CP*y)
%     A(4,4): rho_theta*(1 - phi_S*S_bar)          (from rho_theta*phi_S*S*theta)
%   Other entries are time-invariant.
compute_Ak = @(x_bar, u_bar) [
    rho_y + psi*u_bar(1) - eta*u_bar(3),  0,  0,  0;
    0,                                      0,  0,  delta_theta;
    -gamma_y,                               0,  (1+r),  c_H;
    0,                                      0,  0,  rho_theta*(1 - phi_S*u_bar(1))
];

% --- Jacobian B_k = df/du evaluated at (x_bar, u_bar) ---
%   Time-varying entries from bilinear terms:
%     B(1,1): psi*y_bar - alpha_S               (from psi*S*y)
%     B(1,3): -eta*y_bar                          (from eta*F_CP*y)
%     B(4,1): -rho_theta*phi_S*theta_bar          (from rho_theta*phi_S*S*theta)
%   Time-invariant entries:
%     B(1,2): alpha_F_DI
%     B(3,2): kappa_F_DI,  B(3,3): kappa_F_CP
compute_Bk = @(x_bar, u_bar) [
    psi*x_bar(1) - alpha_S,    alpha_F_DI,    -eta*x_bar(1);
    0,                          0,              0;
    0,                          kappa_F_DI,     kappa_F_CP;
    -rho_theta*phi_S*x_bar(4), 0,              0
];


%% ========================================================================
%  ITERATIVE LQR (iLQR)
% =========================================================================
%
%  Algorithm (Li & Todorov, 2004):
%    1. Initialize reference trajectory (e.g., zero controls)
%    2. Linearize non-linear system around reference -> A_k, B_k
%    3. Solve time-varying LQR backward (Riccati) -> K_k
%    4. Forward simulate with FULL NON-LINEAR dynamics -> new trajectory
%    5. Check convergence. If not: damped update of reference -> go to 2.

max_iter  = 100;       % Maximum iterations
tol       = 1e-7;      % Convergence tolerance (relative trajectory change)
step_size = 0.5;       % Damping factor for reference update (0 < alpha <= 1)
                       % Lower values = more stable, slower convergence

fprintf('================================================================\n');
fprintf('  iLQR — Pandemic Social Planner Optimization\n');
fprintf('  States: [y, d, b, theta]   Controls: [S, F_DI, F_CP]\n');
fprintf('  Bilinear terms: psi*S*y, eta*F_CP*y, rho_theta*phi_S*S*theta\n');
fprintf('  Max iterations: %d,  Tolerance: %.1e\n', max_iter, tol);
fprintf('================================================================\n\n');

% --- Step 1: Initialize reference trajectory (zero controls) ---
x_bar = zeros(4, N+1);
u_bar = zeros(3, N);
x_bar(:, 1) = x0;

% Uncontrolled forward simulation as initial reference
for k = 1:N
    x_bar(:, k+1) = f_nonlin(x_bar(:, k), u_bar(:, k));
end

% Storage for convergence diagnostics
conv_history = zeros(max_iter, 1);
cost_history = zeros(max_iter, 1);

% --- Main iLQR loop ---
for iter = 1:max_iter

    % --- Step 2: Compute time-varying Jacobians along reference ---
    Ak = cell(N, 1);
    Bk = cell(N, 1);
    for k = 1:N
        Ak{k} = compute_Ak(x_bar(:, k), u_bar(:, k));
        Bk{k} = compute_Bk(x_bar(:, k), u_bar(:, k));
    end

    % --- Step 3: Backward Riccati recursion (time-varying) ---
    P = cell(N+1, 1);
    K = cell(N, 1);
    P{N+1} = Q_N * beta^N;

    for k = N:-1:1
        Q_k = Q_base * beta^(k-1);
        A = Ak{k};
        B = Bk{k};
        P_next = P{k+1};

        % Gain matrix: K_k = (B'P_{k+1}B + R)^{-1} B'P_{k+1}A
        K{k} = (B' * P_next * B + R) \ (B' * P_next * A);

        % Riccati update: P_k = A'P_{k+1}A - A'P_{k+1}B*K_k + Q_k
        P{k} = A' * P_next * A - A' * P_next * B * K{k} + Q_k;
    end

    % --- Step 4: Forward simulation with FULL NON-LINEAR dynamics ---
    x_new = zeros(4, N+1);
    u_new = zeros(3, N);
    x_new(:, 1) = x0;

    for k = 1:N
        % iLQR feedback law: u = u_bar - K*(x - x_bar)
        u_new(:, k) = u_bar(:, k) - K{k} * (x_new(:, k) - x_bar(:, k));

        % Enforce constraints
        u_new(1, k) = max(0, min(1, u_new(1, k)));    % S in [0, 1]
        u_new(2, k) = max(0, u_new(2, k));             % F_DI >= 0
        u_new(3, k) = max(0, u_new(3, k));             % F_CP >= 0

        % Non-linear state transition + noise
        x_new(:, k+1) = f_nonlin(x_new(:, k), u_new(:, k)) + noise_vec(:, k);
    end

    % --- Step 5: Convergence check ---
    % Relative change in full state trajectory
    delta_x = norm(x_new - x_bar, 'fro') / (norm(x_bar, 'fro') + 1e-12);
    conv_history(iter) = delta_x;
    cost_history(iter) = compute_welfare(x_new, u_new, Q_base, Q_N, R, beta, N);

    if iter <= 5 || mod(iter, 10) == 0 || delta_x < tol
        fprintf('  Iter %3d:  ||dx||/||x|| = %.2e,  J = %.6f\n', ...
                iter, delta_x, cost_history(iter));
    end

    if delta_x < tol
        fprintf('\n  *** Converged after %d iterations ***\n', iter);
        break;
    end

    % --- Step 6: Damped reference update ---
    x_bar = (1 - step_size) * x_bar + step_size * x_new;
    u_bar = (1 - step_size) * u_bar + step_size * u_new;
    % Keep initial condition fixed
    x_bar(:, 1) = x0;
end

n_iter = min(iter, max_iter);
if conv_history(n_iter) >= tol
    fprintf('\n  WARNING: No convergence after %d iterations (delta = %.2e)\n', ...
            max_iter, conv_history(n_iter));
end

% Store optimal solution
x_opt = x_new;
u_opt = u_new;
K_opt = K;
J_opt = cost_history(n_iter);

fprintf('\n  Optimal cost J* = %.6f\n', J_opt);
fprintf('================================================================\n\n');
%% === HELPER FUNCTION ===
function J = compute_welfare(x_traj, u_traj, Q_base, Q_N, R, beta, N)
    J = 0;
    for k = 1:N
        J = J + beta^(k-1) * (x_traj(:,k)' * Q_base * x_traj(:,k) + ...
                               u_traj(:,k)' * R * u_traj(:,k));
    end
    J = J + beta^N * (x_traj(:,N+1)' * Q_N * x_traj(:,N+1));
end

%%Ploten optimal Trajectorys

%% ========================================================================
%  7. VISUALIZATION: OPTIMAL TRAJECTORY
% =========================================================================

time = 0:N;
quarter_labels = arrayfun(@(k) sprintf('Q%d', k), 1:N, 'UniformOutput', false);

figure('Name', 'Optimal Pandemic Policy — iLQR', 'Color', 'w', ...
       'Position', [50 50 1400 900]);

% --- State Variables ---
subplot(3, 2, 1);
plot(time, x_opt(1,:)*100, 'b-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
title('Output Gap (y_k)');
ylabel('% deviation from potential');
xlabel('Quarter'); grid on;

subplot(3, 2, 2);
plot(time, x_opt(4,:)*100, 'Color', [0.6 0 0], 'LineWidth', 2);
title('Infection Rate (\theta_k)');
ylabel('% population infected');
xlabel('Quarter'); grid on;

subplot(3, 2, 3);
plot(time, x_opt(3,:)*100, 'm-', 'LineWidth', 2);
yline(0, '--k', 'LineWidth', 0.5);
title('Public Debt Deviation (b_k)');
ylabel('pp of pre-pandemic GDP');
xlabel('Quarter'); grid on;

subplot(3, 2, 4);
plot(time, x_opt(2,:)*100, 'r-', 'LineWidth', 2);
title('Excess Mortality (d_k)');
ylabel('% population');
xlabel('Quarter'); grid on;

% --- Control Variables ---
subplot(3, 2, 5);
bar(1:N, u_opt(1,:)', 'FaceColor', [0.2 0.4 0.7]);
title('Containment Intensity (S_k)');
ylabel('Intensity [0, 1]'); ylim([0 1.1]);
xlabel('Quarter'); grid on;

subplot(3, 2, 6);
bar(1:N, [u_opt(2,:)', u_opt(3,:)'] * 100, 'grouped');
legend('Demand Injection (F^{DI})', 'Capacity Preservation (F^{CP})', ...
       'Location', 'NorthEast');
title('Fiscal Composition');
ylabel('% of GDP');
xlabel('Quarter'); grid on;

sgtitle('Optimal Pandemic Policy Trajectory (iLQR)', 'FontWeight', 'bold', ...
        'FontSize', 14);

