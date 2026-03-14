%% Optimal Social Planning in a Pandemic (LQR Approach)
% Basierend auf Pesenti (2026) und Bertsekas (2017)
% Implementierung des "Finite Horizon Discrete-Time LQR"

clear; clc; close all;

%% 1. PARAMETER KALIBRIERUNG (Illustrativ)
% HINWEIS: Diese Werte dienen der Demonstration. Für das finale Paper 
% müssen diese basierend auf den Daten in Abschnitt 5 geschätzt werden.

% Zeithorizont
N = 12;             % Anzahl der Perioden (z.B. Quartale)
beta = 0.99;        % Diskontfaktor (Zeitpräferenz)

% Ökonomische Parameter
rho_y   = 0.9;      % Persistenz des Output Gap (Mean Reversion) (Smet & Wouters New Keynesian Modell)
gamma_y = 0.05;      % Automatic Stabilizators: Elastizität: Wie stark sinken Steuern bei schlechtem GDP? OECD average budget semi-elasticity (Standartwert (Girouard & André 2005)
r       = 0.005;     % Realzins (für Schulden-Dynamik)

% Epidemiologische Parameter
rho_theta = 1.5;    % Infektionswachstum (R0 > 1 ohne Intervention -> explosiv)
delta_theta = 0.001;  % Sterberate (Verhältnis Infektionen zu Toten, verzögert)

% Policy Transmission (Wirkung der Instrumente)
alpha_S = 0.15;      % Wie stark schadet Lockdown (S) dem GDP? (Goolsbee & Syverson 2021 & IMF World Economic Outlook) 10-25%
alpha_F = 0.5;      % Fiskalmultiplikator: Wie stark hilft Stimulus (F) dem GDP? (Auerbach 2021) 0.5-0-8
phi_S   = 0.4;      % Wie effektiv senkt Lockdown (S) die Infektionen? (Flaxman e al. 2020 & Hsiang 2020)
kappa_F = 0.5;      % Kosten des Stimulus für die Schulden-> HIER ANSETZEN

%% 2. SYSTEM MATRIZEN DEFINIEREN
% Zustandsvektor x = [y; d; b; theta] (Output, Death, Debt, Infection)
% Gleichung (3) im Paper 

% Matrix A (Systemdynamik ohne Eingriff)
% y_k+1     = rho_y * y_k
% d_k+1     = delta_theta * theta_k
% b_k+1     = -gamma_y * y_k + (1+r) * b_k
% theta_k+1 = rho_theta * theta_k
A = [rho_y,      0,      0,      0; 
     0,          0,      0,      delta_theta;
     -gamma_y,   0,      (1+r),  0;
     0,          0,      0,      rho_theta];

% Matrix B (Einfluss der Controls u = [S; F])
% Gleichung (3) im Paper 
B = [-alpha_S,   alpha_F;   % Wirkung auf y
     0,          0;         % Wirkung auf d (keine direkte)
     0,          kappa_F;   % Wirkung auf b
     -phi_S,     0];        % Wirkung auf theta

%% 3. GEWICHTUNGSMATRIZEN (COST FUNCTION)
% Hier definieren wir, was dem Social Planner "weh tut".

% Running Cost Gewichte (w_y, w_d, w_b) siehe Gleichung (10) [cite: 170]
w_y = 1;   % Hohe Strafe für GDP Verlust
w_d = 10000;  % Sehr hohe Strafe für Tote
w_b = 1;    % Moderate Strafe für laufende Schulden
% theta wird nicht direkt bestraft, nur indirekt über d

Q_base = diag([w_y, w_d, w_b, 0]); 

% Control Cost (R) siehe Gleichung (12) [cite: 183]
r_S = 0.1;    % Politische Kosten für Lockdown
r_F = 0.1;    % Politische Kosten für Geld ausgeben
R = diag([r_S, r_F]);

% Terminal Cost (Q_N) - Am Ende zählen nur die Schulden
% Gleichung (11) 
W_b = 11;   % Hohe Strafe für Restschulden am Ende der Zeit
Q_N = diag([0, 0, W_b, 0]);

%% 4. BACKWARD RECURSION (Riccati Equation)
% Wir lösen das Problem rückwärts von N nach 0, um die optimale Policy K zu finden.
% Siehe Gleichung (14) und (15) im Paper 

% Initialisierung (ohne beta^N, wir betrachten alles in "Current Value" Einheiten)
P = cell(N+1, 1);
K = cell(N, 1);
P{N+1} = Q_N; % Keine Diskontierung hier

for k = N:-1:1
    % P_next ist die Value Function von morgen. 
    % Aus Sicht von heute (k) ist sie beta * P_next wert.
    P_discounted = P{k+1} * beta; 
    
    % Jetzt die Standard LQR Formel mit dem diskontierten P nutzen
    % K = (R + B' * P_disc * B)^-1 * B' * P_disc * A
    inv_term = inv(R + B' * P_discounted * B);
    K{k} = inv_term * B' * P_discounted * A;
    
    % Riccati Update
    % P_k = Q + A' * P_disc * (A - B * K)
    % (Oder die algebraisch äquivalente Form, die Sie genutzt haben)
    P{k} = Q_base + A' * P_discounted * A - A' * P_discounted * B * K{k};
end
disp('Optimale Policy K berechnet.')

 
%% 5. FORWARD SIMULATION (Mit Constraints und Korrektur)
%Noise simulieren-> wie richtig machen??

% Wir wenden die Policy an, verbieten aber negatives F.

x = zeros(4, N+1);
u = zeros(2, N);

% Initial State: Pandemie-Schock (z.B. 10% Infektionen)
x(:, 1) = [0; 0; 0; 0.1]; 

% Noise settings (Hier war der Fehler: Definition fehlte)
sigma_noise = 0.01; % Standardabweichung der Schocks
rng(42);            % Reproduzierbarkeit

disp('Starte Forward Simulation mit Constraints...');

%% 5. FORWARD SIMULATION (Mit 0-1 Constraint)
% ... (Initialisierung wie gehabt) ...

for k = 1:N
    % 1. Unbeschränkter Wunsch des Planners
    desired_u = -K{k} * x(:, k);
    
    % --- Constraint 2: Fiskalpolitik (F) nur positiv ---
    if desired_u(2) < 0
        desired_u(2) = 0; 
    end
    % F hat theoretisch kein Limit nach oben (man kann unendlich Geld drucken),
    % aber man könnte hier z.B. 10% GDP (0.1) als Limit setzen, wenn man will.
    
    u(:, k) = desired_u;
    
    % 2. System evolviert
    noise = [0; 0; 0; randn() * sigma_noise]; 
    x(:, k+1) = A * x(:, k) + B * u(:, k) + noise;
end

disp('Simulation abgeschlossen.');

%% 6. VISUALISIERUNG DER ERGEBNISSE
time = 0:N;

figure('Name', 'Social Planner Simulation', 'Color', 'w');

% Plot 1: Output Gap (y)
subplot(3, 2, 1);
plot(time, x(1,:), 'LineWidth', 2, 'Color', 'b');
yline(0, '--k');
title('Output Gap (y)');
grid on; ylabel('% Abweichung');

% Plot 2: Excess Mortality (d)
subplot(3, 2, 2);
plot(time, x(2,:), 'LineWidth', 2, 'Color', 'r');
title('Excess Mortality (d)');
grid on;

% Plot 3: Public Debt (b)
subplot(3, 2, 3);
plot(time, x(3,:), 'LineWidth', 2, 'Color', 'm');
title('Public Debt (b)');
grid on; ylabel('% GDP');

% Plot 4: Infection Rate (theta)
subplot(3, 2, 4);
plot(time, x(4,:), 'LineWidth', 2, 'Color', 'k');
title('Infection Rate (\theta)');
grid on;

% Plot 5: Controls (Lockdown & Stimulus)
subplot(3, 2, [5 6]);
bar(time(1:end-1), u');
legend('Social Distancing (S)', 'Fiscal Stimulus (F)');
title('Optimale Policy (Controls)');
grid on;

%% 6. VISUALISIERUNG DER ERGEBNISSE
time = 0:N;

figure('Name', 'Social Planner Simulation', 'Color', 'w');

% Plot 1: Output Gap (y)
subplot(3, 2, 1);
plot(time, x(1,:), 'LineWidth', 2, 'Color', 'b');
yline(0, '--k');
title('Output Gap (y)');
grid on; ylabel('% Abweichung');

% Plot 2: Excess Mortality (d)
subplot(3, 2, 2);
plot(time, x(2,:), 'LineWidth', 2, 'Color', 'r');
title('Excess Mortality (d)');
grid on;

% Plot 3: Public Debt (b)
subplot(3, 2, 3);
plot(time, x(3,:), 'LineWidth', 2, 'Color', 'm');
title('Public Debt (b)');
grid on; ylabel('% GDP');

% Plot 4: Infection Rate (theta)
subplot(3, 2, 4);
plot(time, x(4,:), 'LineWidth', 2, 'Color', 'k');
title('Infection Rate (\theta)');
grid on;

% --- HIER IST DIE ÄNDERUNG: GETRENNTE PLOTS ---

% Plot 5: Social Distancing (S)
subplot(3, 2, 5);
bar(time(1:end-1), u(1, :), 'FaceColor', [0 0.4470 0.7410]); % Blau
title('Policy: Social Distancing (S)');
grid on; 
ylabel('Intensität (0-1)');
ylim([0 1]); % Fixiert die Skala auf 0 bis 1

% Plot 6: Fiscal Stimulus (F)
subplot(3, 2, 6);
% HINWEIS: Wir rechnen *100 für die Anzeige in Prozent!
bar(time(1:end-1), u(2, :)*100, 'FaceColor', [0.8500 0.3250 0.0980]); % Orange
title('Policy: Fiscal Stimulus (F)');
grid on; 
ylabel('% of GDP');

%% 7. COUNTERFACTUAL: Was passiert, wenn wir zu früh öffnen?
% Wir simulieren ein Szenario, in dem der Lockdown (S) nach 5 Quartalen
% abrupt beendet wird ("Political Fatigue"), egal was der Virus macht.

x_cf = zeros(4, N+1);
x_cf(:, 1) = x(:, 1); % Gleicher Startpunkt

u_cf = zeros(2, N);   % Counterfactual Policy

disp('Starte Counterfactual Simulation (Premature Opening)...');

for k = 1:N
   
    % --- DAS SZENARIO: AB QUARTAL 5 IST SCHLUSS MIT LOCKDOWN ---
    if k >= 5
        desired_u(1) = 0; % Zwangseröffnung! S wird auf 0 gesetzt.
    end
    
    u_cf(:, k) = desired_u;
    
    % 2. System evolviert (Gleicher Noise wie im Haupt-Szenario für Fairness)
    % Wir nehmen hier keinen neuen Random-Noise, sondern idealerweise 0 
    % oder den gleichen Seed, um den Effekt rein auf die Policy zu isolieren.
    x_cf(:, k+1) = A * x_cf(:, k) + B * u_cf(:, k); % + noise weggelassen für Klarheit
end

%% 8. VERGLEICHS-PLOT (Optimal vs. Zu früh geöffnet)
figure('Name', 'Counterfactual: The Second Wave', 'Color', 'w');

% Plot: Infektionsrate
subplot(2, 1, 1);
plot(time, x(4,:), 'k', 'LineWidth', 2); hold on;
plot(time, x_cf(4,:), '--r', 'LineWidth', 2);
legend('Optimale Policy', 'Zu frühe Öffnung (nach Q5)', 'Location', 'NorthWest');
title('Infektionsrate (\theta): Der "Bounce Back" Effekt');
grid on; ylabel('Infizierte (Share)');
axis tight;

% Plot: Lockdown Policy
subplot(2, 1, 2);
stairs(time(1:end-1), u(1, :), 'Color', [0 0.4470 0.7410], 'LineWidth', 2); hold on;
stairs(time(1:end-1), u_cf(1, :), '--r', 'LineWidth', 2);
title('Lockdown Intensität (S)');
grid on; ylabel('Intensität (0-1)');
legend('Optimal', 'Abbruch nach Q5');
ylim([-0.1 1.1]);