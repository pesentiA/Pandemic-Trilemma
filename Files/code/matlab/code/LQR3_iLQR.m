%% Optimal Social Planning in a Pandemic (Iterative LQR Approach)
% Basierend auf Pesenti (2026) und Bertsekas (2017)
% UPDATE: Bilineare Epidemiologie mit iterativer Linearisierung
%         um eine Referenztrajektorie (iLQR / Sequential LQ)
%
% KERNÄNDERUNG gegenüber LQR3.m:
% Alt (additiv):       theta_{k+1} = rho_theta * theta_k - phi_S * S_k
% Neu (bilinear):      theta_{k+1} = rho_theta * (1 - phi_S * S_k) * theta_k
%
% Die bilineare Dynamik ist epidemiologisch korrekt: NPIs senken die
% effektive Reproduktionsrate, nicht das Infektionsniveau direkt.
% Da der Term S_k * theta_k nicht LQR-kompatibel ist, linearisieren wir
% um eine zeitabhängige Referenztrajektorie und iterieren bis zur Konvergenz.

clear; clc; close all;

%% 1. PARAMETER KALIBRIERUNG
% Zeithorizont
N = 12;             
beta = 0.99;        

% Ökonomische Basis-Parameter
rho_y   = 0.8;      % Persistenz Output Gap
gamma_y = 0.05;     % Automatic Stabilizers (OECD Standard)
r       = 0.001;    % Realzins

% Epidemiologische Parameter
rho_theta   = 1.35;    % Infektionswachstum (>1: explosiv ohne Intervention)
delta_theta = 0.02;  % Sterberate (IFR)

% --- POLICY PARAMETER ---

% 1. Social Distancing (S)
alpha_S = 0.2;      % Kontraktionärer Effekt auf GDP pro Einheit S
phi_S   = 0.8;       % Effektivität: Reduktion der Reproduktionsrate

% 2. Fiskalpolitik - Disaggregiert
alpha_F_exp  = 1;   kappa_F_exp  = 1;   % Expenditure
alpha_F_rev  = 0.1;    kappa_F_rev  = 0.8;  % Revenue (Tax Cuts)
alpha_F_loan = 0.05;   kappa_F_loan = 0.2;   % Loans & Guarantees

%% 2. ZEITINVARIANTE SYSTEMMATRIZEN (Teile von A und B, die sich nicht ändern)
% Zustandsvektor x = [y; d; b; theta] (4x1)
% Controlvektor  u = [S; F_exp; F_rev; F_loan] (4x1)

% Die ersten 3 Zeilen von A sind zeitinvariant
% Nur Zeile 4 (Infektionsdynamik) wird zeitvariant durch die Linearisierung
A_base = [rho_y,      0,      0,      0; 
          0,          0,      0,      delta_theta;
          -gamma_y,   0,      (1+r),  0;
          0,          0,      0,      0];  % <-- Zeile 4 wird in der Schleife gesetzt

% Die ersten 3 Zeilen von B sind zeitinvariant
% Nur B(4,1) wird zeitvariant (Lockdown-Effekt hängt von theta_bar ab)
B_base = [  -alpha_S,   alpha_F_exp,   alpha_F_rev,   alpha_F_loan;
             0,          0,             0,             0;
             0,          kappa_F_exp,   kappa_F_rev,   kappa_F_loan;
             0,          0,             0,             0];  % <-- B(4,1) wird gesetzt

%% 3. GEWICHTUNGSMATRIZEN (COST FUNCTION) - unverändert

% Running Cost
w_y = 1;       
w_d = 10000;   
w_b = 1;       
Q_base = diag([w_y, w_d, w_b, 0]); 

% Control Cost
r_S    = 0.05;  
r_exp  = 0.5;  
r_rev  = 0.3;  
r_loan = 0.2;  
R = diag([r_S, r_exp, r_rev, r_loan]);

% Terminal Cost
W_b = 10;   
Q_N = diag([0, 0, W_b, 0]);

%% 4. INITIAL STATE
x0 = [0; 0; 0; 0.1];   % 10% Infektionen, Rest am Steady State

% Noise (vorgeneriert für Reproduzierbarkeit)
sigma_noise = 0.01; 
rng(42); 
noise_vec = zeros(4, N);
for t = 1:N
    noise_vec(4, t) = randn() * sigma_noise;
end

%% 5. ITERATIVE LQR (iLQR)
% =========================================================================
% Algorithmus:
%   1. Initialisiere Referenztrajektorie (z.B. ohne Lockdown)
%   2. Linearisiere bilineares System um Referenz → A_k, B_k
%   3. Löse LQR rückwärts (Riccati) → K_k
%   4. Simuliere vorwärts mit NICHTLINEARER Dynamik → neue Trajektorie
%   5. Prüfe Konvergenz. Falls nicht: Referenz updaten → zurück zu 2.
% =========================================================================

max_iter = 50;        % Maximale Iterationen
tol      = 1e-6;      % Konvergenztoleranz (auf theta-Trajektorie)
step_size = 0.7;      % Dämpfung für Referenz-Update (0<alpha<=1)
                       % alpha < 1 verhindert Oszillationen

fprintf('=== Iterative LQR (Bilineare Epidemiologie) ===\n');
fprintf('Maximale Iterationen: %d, Toleranz: %.1e\n\n', max_iter, tol);

% --- Schritt 1: Initiale Referenztrajektorie ---
% Ohne Lockdown: theta wächst mit rho_theta, kein Fiscal Stimulus
theta_bar = zeros(1, N+1);
S_bar     = zeros(1, N);
theta_bar(1) = x0(4);
for k = 1:N
    theta_bar(k+1) = rho_theta * theta_bar(k);  % Unkontrolliertes Wachstum
end

% Speicher für die finale Lösung
K_final = cell(N, 1);
x_final = [];
u_final = [];

for iter = 1:max_iter
    
    % --- Schritt 2: Zeitvariante A_k, B_k aus Referenztrajektorie ---
    A_k = cell(N, 1);
    B_k = cell(N, 1);
    
    for k = 1:N
        % A_k: Zeile 4 wird zeitvariant
        % Partielle Ableitung df/d(theta) = rho_theta * (1 - phi_S * S_bar_k)
        A_temp = A_base;
        A_temp(4,4) = rho_theta * (1 - phi_S * S_bar(k));
        A_k{k} = A_temp;
        
        % B_k: Eintrag (4,1) wird zeitvariant
        % Partielle Ableitung df/d(S) = -rho_theta * phi_S * theta_bar_k
        B_temp = B_base;
        B_temp(4,1) = -rho_theta * phi_S * theta_bar(k);
        B_k{k} = B_temp;
    end
    
    % --- Schritt 3: Backward Riccati Recursion (zeitvariant) ---
    P = cell(N+1, 1);
    K = cell(N, 1);
    P{N+1} = Q_N;
    
    for k = N:-1:1
        P_disc = P{k+1} * beta;
        
        % Zeitvariante Matrizen verwenden!
        Ak = A_k{k};
        Bk = B_k{k};
        
        inv_term = inv(R + Bk' * P_disc * Bk);
        K{k} = inv_term * Bk' * P_disc * Ak;
        P{k} = Q_base + Ak' * P_disc * Ak - Ak' * P_disc * Bk * K{k};
    end
    
    % --- Schritt 4: Forward Simulation mit NICHTLINEARER Dynamik ---
    x = zeros(4, N+1);
    u = zeros(4, N);
    x(:, 1) = x0;
    
    for k = 1:N
        % Optimale Policy aus linearisiertem System
        desired_u = -K{k} * x(:, k);
        
        % Constraints
        desired_u(1) = max(0, min(1, desired_u(1)));  % S in [0, 1]
        desired_u(2:4) = max(0, desired_u(2:4));      % F >= 0
        
        u(:, k) = desired_u;
        
        % NICHTLINEARE Dynamik für theta!
        % Zeilen 1-3: Linear (wie gehabt)
        % Zeile 4:    Bilinear theta_{k+1} = rho_theta*(1 - phi_S*S_k)*theta_k
        S_k = u(1, k);
        
        x(1, k+1) = rho_y * x(1,k) - alpha_S * S_k ...
                     + alpha_F_exp * u(2,k) + alpha_F_rev * u(3,k) ...
                     + alpha_F_loan * u(4,k);
        
        x(2, k+1) = delta_theta * x(4, k);
        
        x(3, k+1) = (1+r) * x(3,k) - gamma_y * x(1,k) ...
                     + kappa_F_exp * u(2,k) + kappa_F_rev * u(3,k) ...
                     + kappa_F_loan * u(4,k);
        
        % *** BILINEARE EPIDEMIOLOGIE ***
        x(4, k+1) = rho_theta * (1 - phi_S * S_k) * x(4,k) + noise_vec(4,k);
    end
    
    % --- Schritt 5: Konvergenzcheck ---
    theta_new = x(4, :);
    S_new     = u(1, :);
    
    delta_traj = norm(theta_new - theta_bar) / (norm(theta_bar) + 1e-12);
    
    if iter <= 5 || mod(iter, 10) == 0
        fprintf('  Iteration %3d: ||delta_theta|| = %.2e\n', iter, delta_traj);
    end
    
    if delta_traj < tol
        fprintf('\n  *** Konvergenz erreicht nach %d Iterationen (tol = %.1e) ***\n', iter, tol);
        K_final = K;
        x_final = x;
        u_final = u;
        break;
    end
    
    % --- Schritt 6: Referenztrajektorie updaten (gedämpft) ---
    % Dämpfung verhindert Oszillationen: 
    %   theta_bar_new = (1-alpha)*theta_bar_old + alpha*theta_simuliert
    theta_bar = (1 - step_size) * theta_bar + step_size * theta_new;
    S_bar     = (1 - step_size) * S_bar     + step_size * S_new;
    
    K_final = K;
    x_final = x;
    u_final = u;
end

if delta_traj >= tol
    fprintf('\n  WARNUNG: Keine Konvergenz nach %d Iterationen (delta = %.2e)\n', max_iter, delta_traj);
end

% Ergebnis übernehmen
x = x_final;
u = u_final;
K = K_final;

fprintf('\n=== Simulation abgeschlossen ===\n');

%% 6. VISUALISIERUNG DER ERGEBNISSE

time = 0:N;
figure('Name', 'iLQR: Bilinear Epidemiology', 'Color', 'w', 'Position', [100 100 1200 800]);

% --- Linke Spalte: Zustände ---

subplot(3, 2, 1);
plot(time, x(1,:)*100, 'LineWidth', 2, 'Color', 'b');
yline(0, '--k'); title('Output Gap (y)'); ylabel('% Deviation'); grid on;

subplot(3, 2, 3);
plot(time, x(2,:)*100, 'LineWidth', 2, 'Color', 'r');
title('Excess Mortality (d)'); ylabel('% Population'); grid on;

subplot(3, 2, 5);
plot(time, x(3,:)*100, 'LineWidth', 2, 'Color', 'm'); 
title('Public Debt Increase (b)'); ylabel('% GDP'); grid on;

% --- Rechte Spalte: Controls & Epidemie ---

subplot(3, 2, 2);
plot(time, x(4,:)*100, 'LineWidth', 2, 'Color', 'k');
title('Infection Rate (\theta) — Bilinear Dynamics'); ylabel('% Infected'); grid on;

subplot(3, 2, 4);
bar(time(1:end-1), u(1, :), 'FaceColor', [0 0.4470 0.7410]);
title('Policy: Social Distancing (S)'); ylim([0 1]); ylabel('Index (0-1)'); grid on;

% Stacked Bar Chart für Fiscal Mix
subplot(3, 2, 6);
fiscal_data = u(2:4, :)' * 100;
b_plot = bar(time(1:end-1), fiscal_data, 'stacked');
b_plot(1).FaceColor = [0.8500 0.3250 0.0980];
b_plot(2).FaceColor = [0.9290 0.6940 0.1250];
b_plot(3).FaceColor = [0.4660 0.6740 0.1880];
title('Policy: Fiscal Stimulus Decomposition'); 
ylabel('% GDP'); 
legend({'Expenditure', 'Tax Cuts', 'Guarantees'}, 'Location', 'NorthEast');
grid on;

%% 7. VERGLEICH: ADDITIVES LQR vs. iLQR (Bilinear)
% =========================================================================
% Wir lösen dasselbe Problem auch mit dem alten additiven Modell, um den
% Unterschied zu demonstrieren. Dies dient als Robustness Check.
% =========================================================================

fprintf('\n=== Vergleich: Additives LQR (alt) vs. iLQR (neu) ===\n');

% Altes additives Modell (konstante A, B)
A_additive = [rho_y,      0,      0,      0; 
              0,          0,      0,      delta_theta;
              -gamma_y,   0,      (1+r),  0;
              0,          0,      0,      rho_theta];

B_additive = [-alpha_S,   alpha_F_exp,   alpha_F_rev,   alpha_F_loan;
               0,          0,             0,             0;
               0,          kappa_F_exp,   kappa_F_rev,   kappa_F_loan;
              -phi_S,      0,             0,             0];

% Standard-LQR Backward Recursion (zeitinvariant)
P_add = cell(N+1, 1);
K_add = cell(N, 1);
P_add{N+1} = Q_N;

for k = N:-1:1
    P_disc = P_add{k+1} * beta;
    inv_term = inv(R + B_additive' * P_disc * B_additive);
    K_add{k} = inv_term * B_additive' * P_disc * A_additive;
    P_add{k} = Q_base + A_additive' * P_disc * A_additive ...
               - A_additive' * P_disc * B_additive * K_add{k};
end

% Forward Simulation mit ADDITIVER Dynamik (alt)
x_add = zeros(4, N+1);
u_add = zeros(4, N);
x_add(:, 1) = x0;

for k = 1:N
    desired_u = -K_add{k} * x_add(:, k);
    desired_u(1) = max(0, min(1, desired_u(1)));
    desired_u(2:4) = max(0, desired_u(2:4));
    u_add(:, k) = desired_u;
    x_add(:, k+1) = A_additive * x_add(:, k) + B_additive * u_add(:, k) + noise_vec(:, k);
end

% Forward Simulation: Additive Policy angewendet auf BILINEARE Realität
% (Was passiert, wenn der Planner das falsche Modell verwendet?)
x_add_real = zeros(4, N+1);
x_add_real(:, 1) = x0;

for k = 1:N
    % Policy kommt aus dem additiven Modell
    desired_u = -K_add{k} * x_add_real(:, k);
    desired_u(1) = max(0, min(1, desired_u(1)));
    desired_u(2:4) = max(0, desired_u(2:4));
    
    S_k = desired_u(1);
    
    % Aber die REALITÄT ist bilinear!
    x_add_real(1, k+1) = rho_y * x_add_real(1,k) - alpha_S * S_k ...
                         + alpha_F_exp * desired_u(2) + alpha_F_rev * desired_u(3) ...
                         + alpha_F_loan * desired_u(4);
    x_add_real(2, k+1) = delta_theta * x_add_real(4, k);
    x_add_real(3, k+1) = (1+r) * x_add_real(3,k) - gamma_y * x_add_real(1,k) ...
                         + kappa_F_exp * desired_u(2) + kappa_F_rev * desired_u(3) ...
                         + kappa_F_loan * desired_u(4);
    x_add_real(4, k+1) = rho_theta * (1 - phi_S * S_k) * x_add_real(4,k) + noise_vec(4,k);
end

% Vergleichsplot
figure('Name', 'Model Comparison: Additive vs. Bilinear', 'Color', 'w', ...
       'Position', [100 100 1200 600]);

subplot(2, 3, 1);
plot(time, x(4,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_add(4,:)*100, 'r--', 'LineWidth', 2);
plot(time, x_add_real(4,:)*100, 'k:', 'LineWidth', 2);
legend('iLQR (bilinear)', 'Additive LQR (eigene Welt)', ...
       'Additive Policy in bilinearer Realität', 'Location', 'NorthEast');
title('Infection Rate (\theta)'); ylabel('% Infected'); grid on;

subplot(2, 3, 2);
plot(time, x(1,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_add(1,:)*100, 'r--', 'LineWidth', 2);
plot(time, x_add_real(1,:)*100, 'k:', 'LineWidth', 2);
title('Output Gap (y)'); ylabel('% Deviation'); grid on;

subplot(2, 3, 3);
plot(time, x(2,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_add(2,:)*100, 'r--', 'LineWidth', 2);
plot(time, x_add_real(2,:)*100, 'k:', 'LineWidth', 2);
title('Excess Mortality (d)'); ylabel('% Population'); grid on;

subplot(2, 3, 4);
plot(time, x(3,:)*100, 'b-', 'LineWidth', 2); hold on;
plot(time, x_add(3,:)*100, 'r--', 'LineWidth', 2);
plot(time, x_add_real(3,:)*100, 'k:', 'LineWidth', 2);
title('Public Debt (b)'); ylabel('% GDP'); grid on;

subplot(2, 3, 5);
bar_data = [u(1,:)', u_add(1,:)'];
bar(time(1:end-1), bar_data);
legend('iLQR', 'Additive LQR'); 
title('Social Distancing (S)'); ylabel('Intensity'); grid on;

subplot(2, 3, 6);
% Gesamter Fiskal-Stimulus Vergleich
total_fiscal_ilqr = sum(u(2:4,:), 1) * 100;
total_fiscal_add  = sum(u_add(2:4,:), 1) * 100;
bar_data = [total_fiscal_ilqr', total_fiscal_add'];
bar(time(1:end-1), bar_data);
legend('iLQR', 'Additive LQR');
title('Total Fiscal Stimulus'); ylabel('% GDP'); grid on;

sgtitle('Robustness Check: Additive vs. Bilinear Epidemiology', 'FontWeight', 'bold');

%% 8. COUNTERFACTUAL: ZU FRÜHE ÖFFNUNG (jetzt mit bilinearer Dynamik)

x_cf = zeros(4, N+1);
x_cf(:, 1) = x0; 
u_cf = zeros(4, N);   

for k = 1:N
    desired_u = -K{k} * x_cf(:, k);
    desired_u(1) = max(0, min(1, desired_u(1)));
    desired_u(2:4) = max(0, desired_u(2:4));
    
    % INTERVENTION: Ab Q5 kein Lockdown mehr
    if k >= 5
        desired_u(1) = 0; 
    end
    
    u_cf(:, k) = desired_u;
    S_k = u_cf(1, k);
    
    % Bilineare Dynamik
    x_cf(1, k+1) = rho_y * x_cf(1,k) - alpha_S * S_k ...
                   + alpha_F_exp * u_cf(2,k) + alpha_F_rev * u_cf(3,k) ...
                   + alpha_F_loan * u_cf(4,k);
    x_cf(2, k+1) = delta_theta * x_cf(4, k);
    x_cf(3, k+1) = (1+r) * x_cf(3,k) - gamma_y * x_cf(1,k) ...
                   + kappa_F_exp * u_cf(2,k) + kappa_F_rev * u_cf(3,k) ...
                   + kappa_F_loan * u_cf(4,k);
    x_cf(4, k+1) = rho_theta * (1 - phi_S * S_k) * x_cf(4,k) + noise_vec(4,k);
end

% Vergleichs-Plot
figure('Name', 'Counterfactual: Premature Opening (Bilinear)', 'Color', 'w');

subplot(2, 1, 1);
plot(time, x(4,:)*100, 'b', 'LineWidth', 2); hold on;
plot(time, x_cf(4,:)*100, '--r', 'LineWidth', 2);
legend('Optimal Policy (iLQR)', 'Premature Opening (after Q5)');
title('Infection Rate (\theta): The "Bounce Back" Effect');
ylabel('% Infected'); grid on; axis tight;

subplot(2, 1, 2);
stairs(time(1:end-1), u(1, :), 'b', 'LineWidth', 2); hold on;
stairs(time(1:end-1), u_cf(1, :), '--r', 'LineWidth', 2);
title('Lockdown Intensity (S)');
ylabel('Intensity (0-1)'); ylim([-0.1 1.1]);
legend('Optimal', 'Premature Opening'); grid on;


%% 9. WELFARE COST BERECHNUNG
% Vergleiche die Gesamtkosten der drei Ansätze

fprintf('\n=== Welfare Cost Vergleich ===\n');

% Kostenfunktion berechnen
compute_cost = @(x_traj, u_traj) compute_welfare(x_traj, u_traj, Q_base, Q_N, R, beta, N);

J_ilqr     = compute_cost(x, u);
J_additive = compute_cost(x_add_real, u_add);  % Additive Policy in bilinearer Welt
J_cf       = compute_cost(x_cf, u_cf);

fprintf('  iLQR (bilinear, optimal):          J = %.4f\n', J_ilqr);
fprintf('  Additives LQR in bilinearer Welt:  J = %.4f  (Gap: %+.1f%%)\n', ...
        J_additive, (J_additive - J_ilqr)/J_ilqr * 100);
fprintf('  Premature Opening:                 J = %.4f  (Gap: %+.1f%%)\n', ...
        J_cf, (J_cf - J_ilqr)/J_ilqr * 100);

%% 10. KONVERGENZDIAGNOSTIK
% Zeige, wie sich die Trajektorie über die Iterationen verändert hat.
% (Dafür müssen wir den iLQR nochmal laufen lassen und Zwischenstände speichern)

fprintf('\n=== Konvergenzdiagnostik ===\n');

% Erneut iterieren, aber theta-Trajektorie jeder Iteration speichern
theta_history = zeros(max_iter, N+1);
theta_bar_diag = zeros(1, N+1);
S_bar_diag = zeros(1, N);
theta_bar_diag(1) = x0(4);
for k = 1:N
    theta_bar_diag(k+1) = rho_theta * theta_bar_diag(k);
end

conv_history = zeros(max_iter, 1);

for iter = 1:max_iter
    % Linearisieren
    A_k_d = cell(N, 1);
    B_k_d = cell(N, 1);
    for k = 1:N
        A_temp = A_base;
        A_temp(4,4) = rho_theta * (1 - phi_S * S_bar_diag(k));
        A_k_d{k} = A_temp;
        B_temp = B_base;
        B_temp(4,1) = -rho_theta * phi_S * theta_bar_diag(k);
        B_k_d{k} = B_temp;
    end
    
    % Backward
    P_d = cell(N+1, 1);
    K_d = cell(N, 1);
    P_d{N+1} = Q_N;
    for k = N:-1:1
        P_disc = P_d{k+1} * beta;
        Ak = A_k_d{k}; Bk = B_k_d{k};
        inv_term = inv(R + Bk' * P_disc * Bk);
        K_d{k} = inv_term * Bk' * P_disc * Ak;
        P_d{k} = Q_base + Ak' * P_disc * Ak - Ak' * P_disc * Bk * K_d{k};
    end
    
    % Forward (nichtlinear)
    x_d = zeros(4, N+1);
    u_d = zeros(4, N);
    x_d(:, 1) = x0;
    for k = 1:N
        desired_u = -K_d{k} * x_d(:, k);
        desired_u(1) = max(0, min(1, desired_u(1)));
        desired_u(2:4) = max(0, desired_u(2:4));
        u_d(:, k) = desired_u;
        S_k = desired_u(1);
        x_d(1, k+1) = rho_y * x_d(1,k) - alpha_S * S_k ...
                     + alpha_F_exp * u_d(2,k) + alpha_F_rev * u_d(3,k) ...
                     + alpha_F_loan * u_d(4,k);
        x_d(2, k+1) = delta_theta * x_d(4, k);
        x_d(3, k+1) = (1+r) * x_d(3,k) - gamma_y * x_d(1,k) ...
                     + kappa_F_exp * u_d(2,k) + kappa_F_rev * u_d(3,k) ...
                     + kappa_F_loan * u_d(4,k);
        x_d(4, k+1) = rho_theta * (1 - phi_S * S_k) * x_d(4,k) + noise_vec(4,k);
    end
    
    theta_history(iter, :) = x_d(4, :);
    conv_history(iter) = norm(x_d(4,:) - theta_bar_diag) / (norm(theta_bar_diag) + 1e-12);
    
    if conv_history(iter) < tol
        theta_history = theta_history(1:iter, :);
        conv_history = conv_history(1:iter);
        break;
    end
    
    theta_bar_diag = (1-step_size)*theta_bar_diag + step_size*x_d(4,:);
    S_bar_diag     = (1-step_size)*S_bar_diag     + step_size*u_d(1,:);
end

n_converged = length(conv_history);

figure('Name', 'Convergence Diagnostics', 'Color', 'w', 'Position', [100 100 1000 400]);

subplot(1, 2, 1);
for j = 1:min(n_converged, 10)
    alpha_val = 0.2 + 0.8 * (j / min(n_converged, 10));
    plot(time, theta_history(j,:)*100, 'Color', [0 0 1 alpha_val], 'LineWidth', 1.5);
    hold on;
end
plot(time, theta_history(end,:)*100, 'r-', 'LineWidth', 2.5);
title('Convergence of \theta Trajectory');
ylabel('% Infected'); xlabel('Period');
legend('Early iterations', '', '', '', '', '', '', '', '', 'Converged', 'Location', 'NorthEast');
grid on;

subplot(1, 2, 2);
semilogy(1:n_converged, conv_history(1:n_converged), 'b-o', 'LineWidth', 1.5, 'MarkerSize', 4);
hold on;
yline(tol, '--r', 'Tolerance', 'LineWidth', 1);
title('Convergence Rate');
xlabel('Iteration'); ylabel('||delta \theta|| (relative)');
grid on;

fprintf('  Konvergiert nach %d Iterationen.\n', n_converged);


%% === HILFSFUNKTION ===

function J = compute_welfare(x_traj, u_traj, Q_base, Q_N, R, beta, N)
    J = 0;
    for k = 1:N
        step_cost = x_traj(:,k)' * Q_base * x_traj(:,k) + ...
                    u_traj(:,k)' * R * u_traj(:,k);
        J = J + (beta^(k-1)) * step_cost;
    end
    J = J + (beta^N) * (x_traj(:,N+1)' * Q_N * x_traj(:,N+1));
end
