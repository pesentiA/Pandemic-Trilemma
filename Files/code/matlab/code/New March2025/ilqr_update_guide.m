% ================================================================
% iLQR UPDATE GUIDE: Old Specification → New (Empirically Calibrated)
% ================================================================

%% CHANGES TO PARAMETER DEFINITION (Step 0)
% ---------------------------------------------------------------
% OLD                          NEW (Empirical)
% ---------------------------------------------------------------
% rho_y   = 0.80              rho_y   = 0.064   (≈ 0, set to 0)
% psi     = 0.10              psi     = 0.424   (scaled: 0.00424 × 100)
% alpha_S = 0.10              alpha_S = 0.028
% alpha_F_DI = 0.30           alpha_F_DI = 0.244
% eta     = 0.40              --- REMOVED (replaced by gamma and alpha_F_CP)
% omega   = 0.30              --- REMOVED
% kappa_F_DI = 0.30           kappa_F_DI = 0.468
% kappa_F_CP = 0.20           kappa_F_CP = 0.193  (pooled)
% gamma_y = 0.45              gamma_y = 0.191

% NEW PARAMETERS:
% alpha_F_CP = 0.253          CP level effect (NOT in old spec)
% gamma_int  = -0.596         S×CP interaction (scaled: -0.00596 × 100)

%% CHANGES TO STATE SPACE (Step 1)
% ---------------------------------------------------------------
% OLD: x = [y; d; b; theta]           (n_x = 4)
%      u = [S; F_DI; F_CP]            (n_u = 3)
%
% NEW: x = [y; d; b; theta; z1; z2]   (n_x = 6)
%      u = [S; F_DI; F_CP]            (n_u = 3)
%
% z1 = F_DI from previous period (for debt equation: kappa_DI * z1)
% z2 = F_DI from two periods ago (for output equation: alpha_DI * z2)

n_x = 6;
n_u = 3;

%% NEW TRANSITION FUNCTION
% ---------------------------------------------------------------
% OLD:
% y_{k+1}     = [rho_y + psi*S - eta*F_CP] * y  -  alpha_S*S  +  alpha_F_DI*F_DI
% d_{k+1}     = delta_theta * theta
% b_{k+1}     = (1+r)*b  -  gamma_y*y  +  kappa_DI*F_DI  +  kappa_CP*F_CP  +  c_H*theta
% theta_{k+1} = rho_theta*(1 - phi_S*S)*theta
%
% NEW:
% y_{k+1}     = rho_y*y + psi*S*y - alpha_S*S + alpha_F_CP*F_CP + gamma_int*S*F_CP + alpha_F_DI*z2
% d_{k+1}     = delta_theta * theta
% b_{k+1}     = (1+r)*b - gamma_y*y + kappa_CP*F_CP + kappa_DI*z1 + c_H*theta
% theta_{k+1} = rho_theta*(1 - phi_S*S)*theta
% z1_{k+1}    = F_DI                    (stores current DI for next period's debt)
% z2_{k+1}    = z1                       (stores lagged DI for output two periods later)

f = @(x, u) [
    rho_y*x(1) + psi*u(1)*x(1) - alpha_S*u(1) + alpha_F_CP*u(3) + gamma_int*u(1)*u(3) + alpha_F_DI*x(6);
    delta_theta * x(4);
    (1+r)*x(3) - gamma_y*x(1) + kappa_F_CP*u(3) + kappa_F_DI*x(5) + c_H*x(4);
    rho_theta * (1 - phi_S*u(1)) * x(4);
    u(2);      % z1_{k+1} = F_DI_k
    x(5)       % z2_{k+1} = z1_k
];

%% NEW JACOBIANS
% ---------------------------------------------------------------
% A = df/dx (6×6)
% Note: A depends on the current state and control (bilinear system)

A = @(x, u) [
    rho_y + psi*u(1),   0, 0, 0,                       0, alpha_F_DI;
    0,                   0, 0, delta_theta,              0, 0;
    -gamma_y,            0, 1+r, c_H,                   kappa_F_DI, 0;
    0,                   0, 0, rho_theta*(1-phi_S*u(1)), 0, 0;
    0,                   0, 0, 0,                       0, 0;
    0,                   0, 0, 0,                       1, 0
];

% B = df/du (6×3)
%       S                                  F_DI   F_CP
B = @(x, u) [
    psi*x(1) - alpha_S + gamma_int*u(3),   0,     alpha_F_CP + gamma_int*u(1);
    0,                                      0,     0;
    0,                                      0,     kappa_F_CP;
    -rho_theta*phi_S*x(4),                 0,     0;
    0,                                      1,     0;
    0,                                      0,     0
];

%% CONTROL BOUNDS
% ---------------------------------------------------------------
% Keep within empirically observed support

S_min  = 0.00;   S_max  = 0.86;    % Observed: [0, 85.66] / 100
FDI_min = 0.00;  FDI_max = 0.08;   % Observed: [0, 8.32] / 100
FCP_min = 0.00;  FCP_max = 0.30;   % Observed: [0, 30.12] / 100

u_lb = [S_min; FDI_min; FCP_min];
u_ub = [S_max; FDI_max; FCP_max];

%% INITIAL CONDITIONS
% ---------------------------------------------------------------
% Representative OECD country at pandemic onset (Q1.2020)

y_0     = 0;        % Pre-pandemic: at potential
d_0     = 0;        % Cumulative excess mortality: zero
b_0     = 0.60;     % OECD median debt-to-GDP ≈ 60%
theta_0 = 0.005;    % Initial infection: 0.5% of population
z1_0    = 0;        % No prior DI
z2_0    = 0;        % No prior DI

x0 = [y_0; d_0; b_0; theta_0; z1_0; z2_0];

%% PARAMETER VALUES (FINAL)
% ---------------------------------------------------------------

% Output dynamics (estimated)
rho_y      = 0.00;      % Set to 0 (empirically insignificant)
psi        = 0.424;     % Scaled: 0.00424 × 100
alpha_S    = 0.028;     % Direct containment damage
alpha_F_CP = 0.253;     % CP level effect
gamma_int  = -0.596;    % S×CP interaction (scaled: -0.00596 × 100)
alpha_F_DI = 0.244;     % DI multiplier (lag 2, via z2)
beta_theta = -0.05;     % Fear effect (calibrated, optional)

% Epidemiology (calibrated)
rho_theta   = 1.30;     % Base reproduction (wave-specific)
delta_theta = 0.03;     % Infection fatality rate
phi_S       = 0.45;     % Containment effectiveness

% Debt dynamics (estimated + calibrated)
r           = 0.001;    % Quarterly real interest rate
gamma_y     = 0.191;    % Automatic stabilizers (estimated)
kappa_F_CP  = 0.193;    % CP budget cost, pooled (estimated)
kappa_F_DI  = 0.468;    % DI budget cost, lag 1 (estimated)
c_H         = 0.02;     % Health cost (calibrated)

% Discount factor
beta_disc = 0.99;       % Quarterly

% Print
fprintf('=== Updated Parameters (Empirically Calibrated) ===\n');
fprintf('  Output:  rho_y=%.3f, psi=%.3f, alpha_S=%.3f\n', rho_y, psi, alpha_S);
fprintf('           alpha_F_CP=%.3f, gamma=%.3f, alpha_F_DI=%.3f\n', alpha_F_CP, gamma_int, alpha_F_DI);
fprintf('  Debt:    gamma_y=%.3f, kappa_CP=%.3f, kappa_DI=%.3f\n', gamma_y, kappa_F_CP, kappa_F_DI);
fprintf('  Epi:     rho_theta=%.2f, phi_S=%.2f, delta_theta=%.3f\n', rho_theta, phi_S, delta_theta);
fprintf('  CP break-even: S* = %.2f\n', alpha_F_CP / abs(gamma_int));
fprintf('  Infection threshold: S_crit = %.2f\n', (1-1/rho_theta)/phi_S);
