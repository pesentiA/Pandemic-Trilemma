%% ========================================================================
%  Optimal Social Planning in a Pandemic — iLQR Implementation
%  Pesenti (2026), based on Bertsekas (2017) and Li & Todorov (2004)
% =========================================================================
%


%GANZER CODE


%  NON-LINEAR SYSTEM (Equations 5, 6, 10 from Section 2):
%
%    y_{k+1}     = [rho_y + psi*S_k - eta*F_CP_k] * y_k
%                  - alpha_S * S_k + alpha_F_DI * F_DI_k
%
%    d_{k+1}     = delta_theta * theta_k
%
%    b_{k+1}     = (1+r)*b_k - gamma_y*y_k
%                  + kappa_DI*F_DI_k + kappa_CP*F_CP_k + c_H*theta_k
%
%    theta_{k+1} = rho_theta * (1 - phi_S*S_k) * theta_k + eps_k
%
%  THREE BILINEAR TERMS requiring iLQR:
%    (1) psi * S_k * y_k          — lockdown-induced hysteresis
%    (2) eta * F_CP_k * y_k       — capacity preservation on persistence
%    (3) rho_theta*phi_S*S_k*theta_k — multiplicative suppression
%
%  State:   x_k = [y_k; d_k; b_k; theta_k]   (4x1)
%  Control: u_k = [S_k; F_DI_k; F_CP_k]       (3x1)
%
% =========================================================================

clear; clc; close all;

%% ========================================================================
%  1. PARAMETER CALIBRATION (Literature-Based)
% =========================================================================
%
%  All parameters are calibrated from the existing literature as a first
%  pass. The final estimation in Section 5 replaces these with panel LP
%  estimates across 38 OECD economies.

% --- Time Horizon ---
N    = 12;          % Periods (quarters: Q1 2020 — Q4 2022)
beta = 0.99;        % Quarterly discount factor

% --- Output Gap Dynamics ---
% rho_y: Baseline persistence of the output gap.
%   Cerra, Fatas, Saxena (2023): Post-crisis persistence ~0.85 quarterly.
%   Kozlowski, Veldkamp, Venkateswaran (2020): Scarring effects increase
%   persistence toward 0.9. We use 0.85 as a moderate baseline.
rho_y = 0.85;

% psi: Sensitivity of output persistence to lockdown intensity.
%   This parameter captures lockdown-induced hysteresis — the mechanism
%   by which containment destroys productive structure (firm closures,
%   broken supply chains, severed employment relationships).
%   Cerra et al. (2023) document persistence increases of 0.05-0.15 in
%   deep recessions. We set psi = 0.10, implying a full lockdown raises
%   effective persistence from 0.85 to 0.95.
psi = 0.8;

% --- Epidemiological Parameters ---
% rho_theta: Uncontrolled transmission rate (effective R0 per quarter).
%   Flaxman et al. (2020): R0 ~ 3.0-3.5 for original SARS-CoV-2.
%   Over a quarterly frequency with continuous contact: rho_theta > 1
%   captures explosive growth. We set 1.3, implying 30% infection growth
%   per quarter absent intervention. This is conservative; weekly models
%   would use higher values.
rho_theta = 0;

% delta_theta: Infection-fatality rate (lagged one period).
%   Mathieu et al. (2020), Msemburi et al. (2023): Global IFR estimates
%   range from 0.5-1.5% depending on age structure and healthcare.
%   For OECD average with healthcare access: delta_theta = 0.01.
delta_theta = 0.0;

% --- Social Distancing (S_k) ---
% alpha_S: Direct contractionary effect of NPIs on the output gap.
%   Deb et al. (2021): GDP contraction of 15% (0.15) at full lockdown.
%   Goolsbee and Syverson (2021): Voluntary behavioral adjustment accounts
%   for ~60% of activity decline, policy-mandated for ~40%.
%   alpha_S captures only the policy-mandated component: 0.15.
alpha_S = 0.5;

% phi_S: Effectiveness of NPIs in reducing the reproduction rate.
%   Flaxman et al. (2020): Lockdowns reduced R_t by 40-80%.
%   Brauner et al. (2021): Business closures + stay-at-home orders
%   reduce R_t by ~35-55%.
%   phi_S = 0.60 implies that a full lockdown (S=1) reduces
%   rho_theta^eff from 1.30 to 1.30*(1-0.60) = 0.52 (well below 1).
phi_S = 0;

% --- Demand Injection (F_DI_k) ---
% alpha_F_DI: Fiscal multiplier for demand injection instruments.
%   Pre-pandemic: Blanchard & Perotti (2002), Ramey & Zubairy (2018)
%   estimate expenditure multipliers of 0.5-1.5.
%   During pandemic: Guerrieri et al. (2022) show Keynesian supply shocks
%   reduce DI effectiveness. Chetty et al. (2020) find stimulus checks
%   had limited spending impact during lockdowns.
%   We use 0.8 — below normal times, reflecting supply-constrained demand.
alpha_F_DI = 0;

% kappa_F_DI: Budgetary cost of DI per unit deployed.
%   Close to 1:1 — one unit of transfer = one unit of debt. Slightly
%   below 1 due to income tax recovery on transfers (Autor et al., 2022).
kappa_F_DI = 0;

% --- Capacity Preservation (F_CP_k) ---
% eta: Effectiveness of CP in reducing output persistence.
%   CP counteracts the hysteresis parameter psi. Fujita et al. (2024):
%   Job retention schemes preserved ~60% of threatened employment
%   relationships. Deb et al. (2021): Liquidity-based measures outperform
%   income support during strict containment.
%   eta = 0.15 implies that F_CP = psi*S/eta = 0.67 units of CP fully
%   offset the hysteresis from a full lockdown.
eta = 0;

% kappa_F_CP: Budgetary cost of CP per unit deployed.
%   Structurally lower than DI: loans create financial assets (net cost
%   depends on default rate), guarantees have zero initial outflow.
%   OECD average default rate on pandemic guarantee schemes: ~3-8%.
%   Weighted average across direct CP (kappa~1) and contingent CP
%   (kappa~0.05-0.10): we use 0.40 as a composite.
kappa_F_CP = 0;

% --- Debt Dynamics ---
% r: Quarterly real interest rate on sovereign debt.
%   Average OECD 10Y real yield in 2020-2021: ~0% to -0.5% annually.
%   However, post-pandemic rate increases (Jorda et al., 2023) raised
%   real rates to ~1-2% by 2023. For the pandemic period: r = 0.001
%   (approximately 0.4% annualized).
r = 0.001;

% gamma_y: Budget semi-elasticity (automatic stabilizers).
%   OECD (2020): Average budget semi-elasticity ~0.55 across OECD,
%   meaning a 1pp output gap widens the deficit by 0.55pp.
%   Quarterly: gamma_y = 0.55/4 ~ 0.14. We round to 0.12 to account
%   for delayed activation of some stabilizer components.
gamma_y = 0.0;

% c_H: Pandemic-specific health expenditure rate per unit infection.
%   OECD Health Statistics: Average COVID-related health spending ~2-4%
%   GDP in 2020. With peak infection rates ~10-20% population:
%   c_H = health_spend / theta_avg ~ 0.03 / 0.15 ~ 0.20.
%   This captures ICU, testing, vaccination, PPE — costs above the
%   cyclical health spending already in gamma_y.
c_H = 0.0;


%% ========================================================================
%  2. COST FUNCTION WEIGHTS
% =========================================================================
%
%  Q_k penalizes state deviations, R penalizes control usage.
%  All weights are on squared deviations: a weight of 1 on y means
%  a 10% output gap costs 1 * 0.10^2 = 0.01 per period.

% --- Running State Cost Q_k = beta^k * diag(w_y, w_d, w_b, 0) ---
w_y = 1;            % Output gap: baseline normalization
w_d = 5000;         % Excess mortality: high weight reflects social cost
                    %   of life. At delta_theta = 0.01, a 10% infection
                    %   rate produces d = 0.001 (0.1% mortality). Cost:
                    %   5000 * 0.001^2 = 0.005.
w_b = 0.5;          % Debt: moderate — reflects that debt is penalized
                    %   both here AND at the terminal condition.

% --- Terminal Cost Q_N = beta^N * diag(0, 0, W_b, 0) ---
W_b = 10;           % Terminal debt: high weight captures post-pandemic
                    %   fiscal pressure (Jorda et al., 2023). Only debt
                    %   persists beyond the horizon — output gaps close,
                    %   deaths are irreversible and already penalized.

% --- Control Cost R = diag(r_S, r_DI, r_CP) ---
r_S  = 0.05;        % Political/social cost of lockdown per se
r_DI = 0.10;        % Administrative cost of DI deployment
r_CP = 0.08;        % Administrative cost of CP deployment

% Assemble matrices
Q_base = diag([w_y, w_d, w_b, 0]);
Q_N    = diag([0, 0, W_b, 0]);
R      = diag([r_S, r_DI, r_CP]);


%% ========================================================================
%  3. INITIAL CONDITION
% =========================================================================

% Pre-pandemic steady state: x = 0. Pandemic shock: 5% initial infection.
x0 = [-0.1; 0; 0; 0];

% Pre-generate noise for reproducibility
sigma_noise = 0.005;
rng(42);
noise_vec = zeros(4, N);
noise_vec(4, :) = randn(1, N) * sigma_noise;


%% ========================================================================
%  4. HELPER FUNCTIONS: NON-LINEAR DYNAMICS AND JACOBIANS
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
%  5. ITERATIVE LQR (iLQR)
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


%% ========================================================================
%  6. EIGENVALUE ANALYSIS OF A_k ALONG OPTIMAL TRAJECTORY
% =========================================================================
%  The eigenvalues of A_k reveal system stability at each point:
%    lambda_1 = rho_y^eff = rho_y + psi*S_k - eta*F_CP_k
%    lambda_2 = 0                    (mortality is memoryless)
%    lambda_3 = (1+r)                (debt is always explosive)
%    lambda_4 = rho_theta^eff = rho_theta*(1 - phi_S*S_k)

fprintf('--- Eigenvalues of A_k along optimal trajectory ---\n');
fprintf('%4s  %10s  %10s  %10s  %10s\n', 'k', 'rho_y_eff', 'lambda_2', '(1+r)', 'rho_th_eff');

eig_trajectory = zeros(4, N);
for k = 1:N
    Ak_opt = compute_Ak(x_opt(:, k), u_opt(:, k));
    eig_trajectory(:, k) = eig(Ak_opt);
    fprintf('%4d  %10.4f  %10.4f  %10.4f  %10.4f\n', k, ...
            eig_trajectory(1,k), eig_trajectory(2,k), ...
            eig_trajectory(3,k), eig_trajectory(4,k));
end
fprintf('\n');


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


%% ========================================================================
%  8. EFFECTIVE PERSISTENCE AND COMPOSITION ANALYSIS
% =========================================================================
%  Track rho_y^eff and the DI/CP mix over time to verify the theoretical
%  prediction: CP dominates early (during containment), DI dominates later.

figure('Name', 'Composition and Persistence Analysis', 'Color', 'w', ...
       'Position', [50 50 1200 500]);

% --- Effective Output Persistence ---
subplot(1, 2, 1);
rho_y_eff = zeros(1, N);
for k = 1:N
    rho_y_eff(k) = rho_y + psi * u_opt(1, k) - eta * u_opt(3, k);
end
plot(1:N, rho_y_eff, 'b-o', 'LineWidth', 2, 'MarkerSize', 6);
hold on;
yline(rho_y, '--k', '\rho_y (baseline)', 'LineWidth', 1);
yline(1, '--r', 'Explosive threshold', 'LineWidth', 1);
title('Effective Output Persistence (\rho_y^{eff})');
ylabel('\rho_y + \psi S_k - \eta F_k^{CP}');
xlabel('Quarter'); grid on;
ylim([0.5 1.2]);

% --- DI vs CP share ---
subplot(1, 2, 2);
total_fiscal = u_opt(2,:) + u_opt(3,:) + 1e-12; % Avoid division by zero
cp_share = u_opt(3,:) ./ total_fiscal * 100;
di_share = u_opt(2,:) ./ total_fiscal * 100;
area(1:N, [di_share', cp_share']);
legend('DI share (%)', 'CP share (%)', 'Location', 'East');
title('Fiscal Composition Over Time');
ylabel('Share of total fiscal stimulus');
xlabel('Quarter'); grid on;
ylim([0 100]);

sgtitle('Composition Dynamics', 'FontWeight', 'bold');


%% ========================================================================
%  9. COUNTERFACTUAL: PREMATURE OPENING
% =========================================================================
%  The planner follows the optimal policy until Q5, then S is forced to 0.
%  Fiscal instruments continue to optimize (planner adjusts F given S=0).

x_cf = zeros(4, N+1);
u_cf = zeros(3, N);
x_cf(:, 1) = x0;

for k = 1:N
    % iLQR feedback law
    u_cf(:, k) = u_bar(:, k) - K_opt{k} * (x_cf(:, k) - x_bar(:, k));

    % Constraints
    u_cf(1, k) = max(0, min(1, u_cf(1, k)));
    u_cf(2, k) = max(0, u_cf(2, k));
    u_cf(3, k) = max(0, u_cf(3, k));

    % INTERVENTION: S = 0 from Q5 onward
    if k >= 5
        u_cf(1, k) = 0;
    end

    % Non-linear dynamics
    x_cf(:, k+1) = f_nonlin(x_cf(:, k), u_cf(:, k)) + noise_vec(:, k);
end

J_cf = compute_welfare(x_cf, u_cf, Q_base, Q_N, R, beta, N);

fprintf('--- Counterfactual: Premature Opening ---\n');
fprintf('  J_optimal    = %.6f\n', J_opt);
fprintf('  J_premature  = %.6f\n', J_cf);
fprintf('  Welfare cost  = %.6f  (%+.1f%%)\n\n', ...
        J_cf - J_opt, (J_cf - J_opt)/J_opt * 100);

% Plot
figure('Name', 'Counterfactual: Premature Opening', 'Color', 'w', ...
       'Position', [50 50 1200 700]);

subplot(2, 2, 1);
plot(time, x_opt(4,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_cf(4,:)*100, 'r--', 'LineWidth', 2);
legend('Optimal', 'Premature opening (Q5)', 'Location', 'NorthEast');
title('Infection Rate (\theta_k)');
ylabel('% population'); xlabel('Quarter'); grid on;

subplot(2, 2, 2);
plot(time, x_opt(1,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_cf(1,:)*100, 'r--', 'LineWidth', 2);
title('Output Gap (y_k)');
ylabel('% deviation'); xlabel('Quarter'); grid on;

subplot(2, 2, 3);
plot(time, x_opt(3,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_cf(3,:)*100, 'r--', 'LineWidth', 2);
title('Public Debt (b_k)');
ylabel('pp of GDP'); xlabel('Quarter'); grid on;

subplot(2, 2, 4);
stairs(1:N, u_opt(1,:), 'b-', 'LineWidth', 2); hold on;
stairs(1:N, u_cf(1,:), 'r--', 'LineWidth', 2);
title('Containment (S_k)');
ylabel('Intensity'); xlabel('Quarter'); grid on;
ylim([-0.1 1.1]);

sgtitle('Counterfactual: Premature Opening After Q5', 'FontWeight', 'bold');


%% ========================================================================
%  10. COUNTRY DEVIATION COSTS
% =========================================================================
%
%  For each country, we compute:
%    1. J_country: The cost of the OBSERVED trajectory (S_obs, F_DI_obs, F_CP_obs)
%       evaluated through the cost function
%    2. J_conditional: The OPTIMAL fiscal composition given the country's
%       observed containment path S_obs (re-optimize F_DI, F_CP only)
%    3. J_full_optimal: The fully optimal trajectory (from Section 5)
%
%  Deviation costs:
%    Delta_J_composition = J_country - J_conditional
%      = excess cost due to suboptimal fiscal composition
%    Delta_J_total = J_country - J_full_optimal
%      = total excess cost relative to unconstrained optimum
%
%  PLACEHOLDER: In the final version, observed trajectories come from
%  the database (Section 4). Here we illustrate with two stylized countries.

fprintf('================================================================\n');
fprintf('  Country Deviation Cost Analysis\n');
fprintf('================================================================\n\n');

% --- Stylized Country A: Heavy DI, Low CP ("Transfer-Heavy") ---
%   Emulates US-style response: large stimulus checks, limited job retention
country_A.name = 'Country A (DI-Heavy)';
country_A.S_obs    = [0.7 0.8 0.6 0.4 0.2 0.1 0.05 0 0 0 0 0];
country_A.F_DI_obs = [0.02 0.05 0.08 0.06 0.04 0.03 0.02 0.01 0 0 0 0];
country_A.F_CP_obs = [0.01 0.01 0.02 0.01 0.01 0 0 0 0 0 0 0];

% --- Stylized Country B: Balanced DI/CP ("Mixed Package") ---
%   Emulates German-style response: Kurzarbeitergeld + KfW loans + transfers
country_B.name = 'Country B (Balanced)';
country_B.S_obs    = [0.7 0.8 0.6 0.4 0.2 0.1 0.05 0 0 0 0 0];
country_B.F_DI_obs = [0.01 0.02 0.03 0.03 0.03 0.02 0.01 0.01 0 0 0 0];
country_B.F_CP_obs = [0.02 0.04 0.05 0.04 0.02 0.01 0.01 0 0 0 0 0];

countries = {country_A, country_B};

for c = 1:length(countries)
    ctry = countries{c};
    fprintf('--- %s ---\n', ctry.name);

    % ----- Cost of observed trajectory -----
    x_obs = zeros(4, N+1);
    u_obs = zeros(3, N);
    x_obs(:, 1) = x0;
    u_obs(1, :) = ctry.S_obs;
    u_obs(2, :) = ctry.F_DI_obs;
    u_obs(3, :) = ctry.F_CP_obs;

    for k = 1:N
        x_obs(:, k+1) = f_nonlin(x_obs(:, k), u_obs(:, k)) + noise_vec(:, k);
    end
    J_obs = compute_welfare(x_obs, u_obs, Q_base, Q_N, R, beta, N);

    % ----- Conditional optimum: optimize F_DI, F_CP given S_obs -----
    % Re-run iLQR but fix S_k = S_obs at each period.
    % This requires modifying the forward pass to override S.
    x_bar_c = x_obs;           % Initialize at observed
    u_bar_c = u_obs;

    for iter_c = 1:50
        % Jacobians along current reference
        Ak_c = cell(N, 1);
        Bk_c = cell(N, 1);
        for k = 1:N
            Ak_c{k} = compute_Ak(x_bar_c(:, k), u_bar_c(:, k));
            Bk_c{k} = compute_Bk(x_bar_c(:, k), u_bar_c(:, k));
        end

        % Backward pass
        P_c = cell(N+1, 1);
        K_c = cell(N, 1);
        P_c{N+1} = Q_N * beta^N;
        for k = N:-1:1
            Q_k = Q_base * beta^(k-1);
            A = Ak_c{k}; B = Bk_c{k};
            K_c{k} = (B' * P_c{k+1} * B + R) \ (B' * P_c{k+1} * A);
            P_c{k} = A' * P_c{k+1} * A - A' * P_c{k+1} * B * K_c{k} + Q_k;
        end

        % Forward pass with S FIXED to observed
        x_new_c = zeros(4, N+1);
        u_new_c = zeros(3, N);
        x_new_c(:, 1) = x0;

        for k = 1:N
            u_new_c(:, k) = u_bar_c(:, k) - K_c{k} * (x_new_c(:, k) - x_bar_c(:, k));

            % Override S with observed containment
            u_new_c(1, k) = ctry.S_obs(k);

            % Constraints on fiscal instruments only
            u_new_c(2, k) = max(0, u_new_c(2, k));
            u_new_c(3, k) = max(0, u_new_c(3, k));

            x_new_c(:, k+1) = f_nonlin(x_new_c(:, k), u_new_c(:, k)) + noise_vec(:, k);
        end

        % Convergence check
        delta_c = norm(x_new_c - x_bar_c, 'fro') / (norm(x_bar_c, 'fro') + 1e-12);
        if delta_c < 1e-6
            break;
        end

        % Damped update
        x_bar_c = 0.5 * x_bar_c + 0.5 * x_new_c;
        u_bar_c = 0.5 * u_bar_c + 0.5 * u_new_c;
        u_bar_c(1, :) = ctry.S_obs;  % Keep S fixed
        x_bar_c(:, 1) = x0;
    end

    J_cond = compute_welfare(x_new_c, u_new_c, Q_base, Q_N, R, beta, N);

    % ----- Deviation costs -----
    Delta_J_comp  = J_obs - J_cond;     % Cost of suboptimal composition
    Delta_J_total = J_obs - J_opt;      % Total deviation from full optimum

    fprintf('  J_observed          = %.6f\n', J_obs);
    fprintf('  J_conditional_opt   = %.6f\n', J_cond);
    fprintf('  J_full_optimal      = %.6f\n', J_opt);
    fprintf('  Composition cost    = %.6f  (%+.1f%% of J_obs)\n', ...
            Delta_J_comp, Delta_J_comp/J_obs * 100);
    fprintf('  Total deviation     = %.6f  (%+.1f%% of J_obs)\n', ...
            Delta_J_total, Delta_J_total/J_obs * 100);

    % Decompose: how much of total deviation is composition vs containment?
    Delta_J_containment = J_cond - J_opt;
    fprintf('  --- Decomposition ---\n');
    fprintf('  Due to containment path: %.6f  (%.1f%% of total)\n', ...
            Delta_J_containment, Delta_J_containment / max(Delta_J_total, 1e-12) * 100);
    fprintf('  Due to fiscal comp:     %.6f  (%.1f%% of total)\n\n', ...
            Delta_J_comp, Delta_J_comp / max(Delta_J_total, 1e-12) * 100);

    % Store for plotting
    countries{c}.x_obs  = x_obs;
    countries{c}.x_cond = x_new_c;
    countries{c}.u_cond = u_new_c;
    countries{c}.J_obs  = J_obs;
    countries{c}.J_cond = J_cond;
end

% --- Deviation Cost Comparison Plot ---
figure('Name', 'Country Deviation Costs', 'Color', 'w', ...
       'Position', [50 50 1400 600]);

for c = 1:length(countries)
    ctry = countries{c};

    subplot(2, length(countries), c);
    plot(time, x_opt(3,:)*100, 'k-', 'LineWidth', 2); hold on;
    plot(time, ctry.x_cond(3,:)*100, 'b--', 'LineWidth', 2);
    plot(time, ctry.x_obs(3,:)*100, 'r-.', 'LineWidth', 2);
    legend('Full optimum', 'Conditional optimum', 'Observed', ...
           'Location', 'NorthWest');
    title(sprintf('%s: Debt', ctry.name));
    ylabel('pp of GDP'); xlabel('Quarter'); grid on;

    subplot(2, length(countries), c + length(countries));
    bar(1:N, [ctry.x_obs(1,1:N)', ctry.x_cond(1,1:N)', x_opt(1,1:N)'] * 100);
    legend('Observed', 'Conditional opt.', 'Full optimum', 'Location', 'SouthEast');
    title(sprintf('%s: Output Gap', ctry.name));
    ylabel('% deviation'); xlabel('Quarter'); grid on;
end

sgtitle('Deviation Costs: Observed vs. Optimal Policy', 'FontWeight', 'bold');


%% ========================================================================
%  11. CONVERGENCE DIAGNOSTICS
% =========================================================================

figure('Name', 'Convergence Diagnostics', 'Color', 'w', ...
       'Position', [100 100 1000 400]);

subplot(1, 2, 1);
semilogy(1:n_iter, conv_history(1:n_iter), 'b-o', 'LineWidth', 1.5, ...
         'MarkerSize', 4);
hold on;
yline(tol, '--r', sprintf('tol = %.0e', tol), 'LineWidth', 1);
title('Trajectory Convergence');
xlabel('Iteration'); ylabel('||dx|| / ||x|| (relative)');
grid on;

subplot(1, 2, 2);
plot(1:n_iter, cost_history(1:n_iter), 'b-o', 'LineWidth', 1.5, ...
     'MarkerSize', 4);
title('Cost Function J per Iteration');
xlabel('Iteration'); ylabel('J');
grid on;

sgtitle('iLQR Convergence Diagnostics', 'FontWeight', 'bold');


%% ========================================================================
%  12. SUMMARY TABLE
% =========================================================================

fprintf('================================================================\n');
fprintf('  SUMMARY\n');
fprintf('================================================================\n');
fprintf('  %-35s  %10s  %10s  %10s\n', 'Scenario', 'J', 'Delta b_N', 'Delta d_N');
fprintf('  %-35s  %10.4f  %10.2f%%  %10.4f%%\n', ...
        'Full optimum (iLQR)', J_opt, x_opt(3,end)*100, x_opt(2,end)*100);
fprintf('  %-35s  %10.4f  %10.2f%%  %10.4f%%\n', ...
        'Premature opening (Q5)', J_cf, x_cf(3,end)*100, x_cf(2,end)*100);

for c = 1:length(countries)
    ctry = countries{c};
    fprintf('  %-35s  %10.4f  %10.2f%%  %10.4f%%\n', ...
            [ctry.name ' (observed)'], ctry.J_obs, ...
            ctry.x_obs(3,end)*100, ctry.x_obs(2,end)*100);
    fprintf('  %-35s  %10.4f  %10.2f%%  %10.4f%%\n', ...
            [ctry.name ' (cond. opt.)'], ctry.J_cond, ...
            ctry.x_cond(3,end)*100, ctry.x_cond(2,end)*100);
end
fprintf('================================================================\n');


%% ========================================================================
%  HELPER FUNCTION: Welfare Cost Computation
% =========================================================================

function J = compute_welfare(x_traj, u_traj, Q_base, Q_N, R, beta, N)
% Compute the total cost J for a given trajectory (x, u).
%   J = sum_{k=0}^{N-1} beta^k (x'Qx + u'Ru) + beta^N x_N'Q_N x_N
    J = 0;
    for k = 1:N
        x_k = x_traj(:, k);
        u_k = u_traj(:, k);
        J = J + beta^(k-1) * (x_k' * Q_base * x_k + u_k' * R * u_k);
    end
    J = J + beta^N * (x_traj(:, N+1)' * Q_N * x_traj(:, N+1));
end
