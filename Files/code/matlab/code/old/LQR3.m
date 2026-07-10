%% Optimal Social Planning in a Pandemic (LQR Approach)
% Basierend auf Pesenti (2026) und Bertsekas (2017)
% Implementierung des "Finite Horizon Discrete-Time LQR"
% UPDATE: Mit disaggregierter Fiskalpolitik (Expenditure, Revenue, Loans)

clear; clc; close all;

%% 1. PARAMETER KALIBRIERUNG
% Zeithorizont
N = 12;             
beta = 0.99;        

% Ökonomische Basis-Parameter
rho_y   = 0.9;      % Persistenz Output Gap
gamma_y = 0.05;       % Automatic Stabilizers (OECD Standard)
r       = 0.005;     % Realzins

% Epidemiologische Parameter
rho_theta = 1.5;    % Infektionswachstum (Kalibriert für Quartale, >1)
delta_theta = 0.001; % Sterberate (0.2% IFR auf Bevölkerung gerechnet)

% --- POLICY PARAMETER (Die Erweiterung) ---

% 1. Social Distancing (S)
alpha_S = 0.10;      % Kosten: 15% GDP Verlust bei vollem Lockdown
phi_S   = 0.8;       % Nutzen: Reduziert R0 massiv

% 2. Fiskalpolitik (F) - Disaggregiert
% Wir unterscheiden nach Multiplikator (alpha) und Schuldenkosten (kappa)

% F_exp: Direkte Ausgaben (Transfers, Investitionen)
alpha_F_exp = 0.25;   % Hoher Multiplikator (Schätzung)
kappa_F_exp = 0.4;   % Teuer (1:1 schuldenwirksam)

% F_rev: Steuersenkungen
alpha_F_rev = 0.1;   % Mittlerer Multiplikator (Sparquote dämpft Effekt)
kappa_F_rev = 0.15;   % Teuer (1:1 Defizit)

% F_loan: Kredite & Garantien
alpha_F_loan = 0.05;  % Geringer direkter Demand-Effekt (Liquidity support)
kappa_F_loan = 0.1;  % Sehr billig! (Nur Ausfallrisiko erhöht Schulden)

%% 2. SYSTEM MATRIZEN DEFINIEREN
% Zustandsvektor x = [y; d; b; theta] (4x1)
% Controlvektor  u = [S; F_exp; F_rev; F_loan] (4x1)

% Matrix A (Systemdynamik ohne Eingriff) - unverändert
A = [rho_y,      0,      0,      0; 
     0,          0,      0,      delta_theta;
     -gamma_y,   0,      (1+r),  0;
     0,          0,      0,      rho_theta];

% Matrix B (Einfluss der Controls) - JETZT 4 SPALTEN
% Spalten: [S, F_exp, F_rev, F_loan]
B = [-alpha_S,   alpha_F_exp,   alpha_F_rev,   alpha_F_loan;  % Wirkung auf y
     0,          0,             0,             0;             % Wirkung auf d
     0,          kappa_F_exp,   kappa_F_rev,   kappa_F_loan;  % Wirkung auf b
     -phi_S,     0,             0,             0];            % Wirkung auf theta

%% 3. GEWICHTUNGSMATRIZEN (COST FUNCTION)

% Running Cost (Zustände)
w_y = 1;       
w_d = 10000;   % Hohe Moral: Leben retten hat Priorität
w_b = 1;       % Mittlere Schuldenaversion

Q_base = diag([w_y, w_d, w_b, 0]); 

% Control Cost (R) - Strategische Auswahl
% Quadratische Kosten sorgen für "Diminishing Returns" und Mix
r_S    = 0.5;  % Lockdown tut politisch weh
r_exp  = 0.5;  % Ausgaben sind Standard
r_rev  = 0.8;  % Steuersenkungen politisch "teurer" (schwer zurückzunehmen)
r_loan = 0.2;  % Garantien sind politisch "billig" (versteckte Kosten)

R = diag([r_S, r_exp, r_rev, r_loan]);

% Terminal Cost (Schulden am Ende bestrafen)
W_b = 10;   
Q_N = diag([0, 0, W_b, 0]);

%% 4. BACKWARD RECURSION (LQR Solver)

P = cell(N+1, 1);
K = cell(N, 1);
P{N+1} = Q_N; 

for k = N:-1:1
    P_discounted = P{k+1} * beta; 
    
    % LQR Feedback Gain Matrix (jetzt 4x4 Inversion)
    inv_term = inv(R + B' * P_discounted * B);
    K{k} = inv_term * B' * P_discounted * A;
    
    % Riccati Update
    P{k} = Q_base + A' * P_discounted * A - A' * P_discounted * B * K{k};
end
disp('Optimale Policy K berechnet.');

%% 5. FORWARD SIMULATION (Mit Constraints & Noise Fix)

x = zeros(4, N+1);
u = zeros(4, N); % 4 Controls!

% Initial State: 10% Infektionen
x(:, 1) = [0; 0; 0; 0.1]; 

% Noise VORHER generieren für Fairness im Vergleich
sigma_noise = 0.01; 
rng(42); 
noise_vec = zeros(4, N);
for t = 1:N
    noise_vec(4, t) = randn() * sigma_noise; % Schock nur auf Theta
end

disp('Starte Simulation...');

for k = 1:N
    % 1. Unbeschränkter Wunsch
    desired_u = -K{k} * x(:, k);
    
    % 2. Constraints anwenden (Clipping)
    
    % S: Zwischen 0 und 1
    desired_u(1) = max(0, min(1, desired_u(1)));
    
    % F_exp, F_rev, F_loan: Nicht negativ (keine Steuererhöhung in Krise)
    desired_u(2) = max(0, desired_u(2));
    desired_u(3) = max(0, desired_u(3));
    desired_u(4) = max(0, desired_u(4));
    
    u(:, k) = desired_u;
    
    % 3. System Update
    x(:, k+1) = A * x(:, k) + B * u(:, k) + noise_vec(:, k);
end

%% 6. VISUALISIERUNG DER ERGEBNISSE

time = 0:N;
figure('Name', 'Disaggregated Fiscal Policy Simulation', 'Color', 'w', 'Position', [100 100 1200 800]);

% --- Linke Spalte: Zustände ---

subplot(3, 2, 1);
plot(time, x(1,:)*100, 'LineWidth', 2, 'Color', 'b'); % *100 für Prozent
yline(0, '--k'); title('Output Gap (y)'); ylabel('% Deviation'); grid on;

subplot(3, 2, 3);
plot(time, x(2,:)*100, 'LineWidth', 2, 'Color', 'r'); % *100 für Prozent
title('Excess Mortality (d)'); ylabel('% Population'); grid on;

subplot(3, 2, 5);
plot(time, x(3,:)*100, 'LineWidth', 2, 'Color', 'm'); 
title('Public Debt Increase (b)'); ylabel('% GDP'); grid on;

% --- Rechte Spalte: Controls & Epidemie ---

subplot(3, 2, 2);
plot(time, x(4,:)*100, 'LineWidth', 2, 'Color', 'k');
title('Infection Rate (\theta)'); ylabel('% Infected'); grid on;

subplot(3, 2, 4);
bar(time(1:end-1), u(1, :), 'FaceColor', [0 0.4470 0.7410]);
title('Policy: Social Distancing (S)'); ylim([0 1]); ylabel('Index (0-1)'); grid on;

% --- DAS HIGHLIGHT: STACKED BAR CHART FÜR FISCAL MIX ---
subplot(3, 2, 6);
% Wir stapeln F_exp, F_rev, F_loan übereinander
fiscal_data = u(2:4, :)' * 100; % Transponieren für bar(), *100 für %GDP
b = bar(time(1:end-1), fiscal_data, 'stacked');

% Farben und Legende
b(1).FaceColor = [0.8500 0.3250 0.0980]; % Orange (Exp)
b(2).FaceColor = [0.9290 0.6940 0.1250]; % Gelb (Rev)
b(3).FaceColor = [0.4660 0.6740 0.1880]; % Grün (Loan)

title('Policy: Fiscal Stimulus Decomposition'); 
ylabel('% GDP'); 
legend({'Expenditure', 'Tax Cuts', 'Guarantees'}, 'Location', 'NorthEast');
grid on;

%% 7. COUNTERFACTUAL (Szenario: Zu frühe Öffnung)
% Funktioniert jetzt auch mit dem neuen u-Vektor

x_cf = zeros(4, N+1);
x_cf(:, 1) = x(:, 1); 
u_cf = zeros(4, N);   

for k = 1:N
    % Normale Regel
    desired_u = -K{k} * x_cf(:, k);
    
    % Constraints
    desired_u(1) = max(0, min(1, desired_u(1)));
    desired_u(2:4) = max(0, desired_u(2:4));
    
    % INTERVENTION: Politisches Ende von S nach Q5
    if k >= 5
        desired_u(1) = 0; 
    end
    
    u_cf(:, k) = desired_u;
    
    % Gleicher Noise wie oben!
    x_cf(:, k+1) = A * x_cf(:, k) + B * u_cf(:, k) + noise_vec(:, k);
end

% Vergleichs-Plot (Nur Infektionen)
figure('Name', 'Scenario Comparison', 'Color', 'w');
plot(time, x(4,:)*100, 'k', 'LineWidth', 2); hold on;
plot(time, x_cf(4,:)*100, '--r', 'LineWidth', 2);
legend('Optimal Policy', 'Premature Opening (after Q5)');
title('Effect of Premature Opening on Infection Rate');
ylabel('% Infected'); grid on;



%% 9. BENCHMARKING: ACTUAL VS. OPTIMAL POLICY (The Efficiency Gap)
% Dieser Abschnitt vergleicht reale Länderdaten mit dem Modell-Optimum.

% A. DATEN VORBEREITUNG (Hier später Ihre echten Daten laden!)
% Wir simulieren hier 3 "Typen" von Ländern als Platzhalter:
% 1. "Hesitant": Zu spät reagiert, dann Panik-Lockdown
% 2. "Strict": Früh und hart reagiert (z.B. Neuseeland)
% 3. "Laissez-Faire": Kaum Maßnahmen (z.B. Schweden am Anfang)

countries = {'Hesitant Country', 'Strict Country', 'Laissez-Faire'};
n_countries = length(countries);

% Platzhalter für reale Pfade (u_real) und Zustände (x_real)
% In der Praxis: x_real kommt aus Daten (GDP, Excess Mortality), u_real aus Oxford Stringency Index & IMF Data
real_data = struct();

% Beispiel-Daten generieren (Zufällig + Bias je nach Typ)
for i = 1:n_countries
    % Wir simulieren, dass alle mit dem gleichen Schock starten
    real_data(i).x0 = [0; 0; 0; 0.1]; 
    
    % Erzeuge künstliche "Reale Politik" Pfade (u_real)
    u_temp = zeros(4, N);
    if i == 1 % Hesitant: Erst nichts, dann Panik
        u_temp(1, 1:3) = 0.1; u_temp(1, 4:8) = 0.8; % Später Lockdown
        u_temp(2, 4:8) = 2.0; % Später Stimulus
    elseif i == 2 % Strict: Sofort hart
        u_temp(1, 1:6) = 0.6; % Konstanter Lockdown
        u_temp(2, 1:6) = 1.0; % Konstanter Stimulus
    else % Laissez-Faire
        u_temp(1, :) = 0.1; % Kaum Lockdown
        u_temp(2, :) = 0.5; % Wenig Stimulus
    end
    real_data(i).u_actual = u_temp;
    
    % Wir simulieren die Konsequenzen (x_actual) mit unserem Modell A/B
    % (In der Realität laden Sie hier die echten x-Werte wie GDP/Tote!)
    x_temp = zeros(4, N+1);
    x_temp(:,1) = real_data(i).x0;
    for k = 1:N
        noise = [0;0;0; randn()*0.005]; % Kleiner Zufall
        x_temp(:,k+1) = A * x_temp(:,k) + B * u_temp(:,k) + noise;
    end
    real_data(i).x_actual = x_temp;
end

% B. BERECHNUNG DES "WELFARE LOSS"
results = table('Size', [n_countries, 4], ...
    'VariableTypes', {'string', 'double', 'double', 'double'}, ...
    'VariableNames', {'Country', 'Cost_Actual', 'Cost_Optimal', 'Efficiency_Gap_Percent'});

figure('Name', 'Efficiency Frontier Analysis', 'Color', 'w');

for i = 1:n_countries
    % 1. Hole Daten
    x_act = real_data(i).x_actual;
    u_act = real_data(i).u_actual;
    x0    = real_data(i).x0;
    
    % 2. Berechne Kosten der REALEN Politik (J_actual)
    J_actual = 0;
    for k = 1:N
        % Kosten: Zustand + Politik (diskontiert)
        step_cost = (x_act(:,k)' * Q_base * x_act(:,k)) + ...
                    (u_act(:,k)' * R * u_act(:,k));
        J_actual = J_actual + (beta^(k-1)) * step_cost;
    end
    % Terminal Cost dazu
    J_actual = J_actual + (beta^N) * (x_act(:,N+1)' * Q_N * x_act(:,N+1));
    
    % 3. Berechne OPTIMALE Politik (J_optimal)
    % Wir nutzen unseren LQR Solver (K Matrix) für diesen Startwert
    x_opt = zeros(4, N+1); x_opt(:,1) = x0;
    u_opt = zeros(4, N);
    J_optimal = 0;
    
    for k = 1:N
        % Optimale Regel anwenden
        u_k = -K{k} * x_opt(:,k);
        % Constraints (wichtig für fairen Vergleich!)
        u_k(1) = max(0, min(1, u_k(1))); 
        u_k(2:4) = max(0, u_k(2:4));
        
        u_opt(:,k) = u_k;
        
        % Kosten summieren
        step_cost = (x_opt(:,k)' * Q_base * x_opt(:,k)) + ...
                    (u_opt(:,k)' * R * u_opt(:,k));
        J_optimal = J_optimal + (beta^(k-1)) * step_cost;
        
        % System update (ohne Noise für das ideale Referenzszenario)
        x_opt(:,k+1) = A * x_opt(:,k) + B * u_k; 
    end
    J_optimal = J_optimal + (beta^N) * (x_opt(:,N+1)' * Q_N * x_opt(:,N+1));
    
    % 4. Ergebnisse speichern
    efficiency_gap = (J_actual - J_optimal) / J_optimal * 100; % Wie viel % schlechter?
    
    results.Country(i) = countries{i};
    results.Cost_Actual(i) = J_actual;
    results.Cost_Optimal(i) = J_optimal;
    results.Efficiency_Gap_Percent(i) = efficiency_gap;
    
    % 5. Plotten (Scatter Plot: Kosten vs. Tote)
    total_deaths = sum(x_act(2,:)); % Summe Excess Mortality
    total_gdp_loss = sum(abs(min(0, x_act(1,:)))); % Summe GDP Verlust
    
    subplot(1,2,1);
    scatter(total_gdp_loss*100, total_deaths*100, 100, 'filled'); hold on;
    text(total_gdp_loss*100+0.1, total_deaths*100, countries{i});
end

% C. VISUALISIERUNG DER "EFFICIENCY FRONTIER"
subplot(1,2,1);
xlabel('Cumulative GDP Loss (%)');
ylabel('Cumulative Excess Mortality (%)');
title('Health-Economy Trade-Off (Actual Data)');
grid on;

% Bar Chart der Ineffizienz
subplot(1,2,2);
bar(categorical(results.Country), results.Efficiency_Gap_Percent);
ylabel('Efficiency Gap (%)');
title('Welfare Loss compared to Optimal Planner');
grid on;

disp('Benchmarking abgeschlossen. Ergebnisse:');
disp(results);

%% Mit allen 38 OECD Länder

%% 9. BENCHMARKING: OECD LÄNDER VERGLEICH
% Voraussetzung: Sie haben eine Tabelle 'data' geladen (z.B. aus Excel)
% mit Spalten: Country, Time, GDP_Gap, Mortality, Debt, Stringency, F_exp, F_rev, F_loan

% Beispiel für Daten-Import (müssen Sie an Ihre Datei anpassen)
% T = readtable('OECD_Pandemic_Data.xlsx'); 
% countries = unique(T.Country); 

% HINWEIS: Da ich Ihre Datei nicht habe, simuliere ich hier kurz die Struktur.
% In Ihrem echten Code: Löschen Sie diesen Block und nutzen Sie readtable.
countries = {'USA', 'DEU', 'ITA', 'SWE', 'JPN', 'FRA'}; % ... bis 38
n_countries = length(countries);

% Ergebnistabelle vorbereiten
results = table('Size', [n_countries, 5], ...
    'VariableTypes', {'string', 'double', 'double', 'double', 'string'}, ...
    'VariableNames', {'Country', 'Cost_Actual', 'Cost_Optimal', 'Efficiency_Gap_Pct', 'Verdict'});

fprintf('Starte Analyse für %d Länder...\n', n_countries);

for i = 1:n_countries
    country_name = countries{i};
    
    % ---------------------------------------------------------
    % A. DATEN FÜR DIESES LAND LADEN
    % ---------------------------------------------------------
    % Hier filtern Sie Ihre Tabelle nach dem Land "i"
    % country_rows = T(strcmp(T.Country, country_name), :);
    
    % WICHTIG: Mappen Sie hier Ihre echten Spalten auf die Modell-Vektoren!
    % Achten Sie auf Einheiten! (Dezimal vs. Prozent)
    
    % Dummy-Werte (HIER IHRE ECHTEN DATEN EINSETZEN):
    % x_real = [y; d; b; theta]
    x_real = zeros(4, N+1); 
    x_real(:,1) = [0; 0; 0; 0.1]; % Startwert 2020Q1 aus Daten nehmen!
    
    % u_real = [S; F_exp; F_rev; F_loan]
    u_real = zeros(4, N);
    
    % Simuliertes Beispiel: Wir tun so, als hätten wir Daten geladen
    % (Das hier löschen und durch echte Datenzuweisung ersetzen)
    x_real(1,:) = -0.02 + randn(1, N+1)*0.01; % Echtes GDP Gap
    x_real(2,:) = 0.001 + randn(1, N+1)*0.0001; % Echte Tote
    x_real(3,:) = linspace(0, 0.1, N+1); % Echte Schulden
    u_real(1,:) = 0.5; % Echter Stringency Index (0-1)
    u_real(2,:) = 0.02; % Echte Ausgaben (% GDP)
    
    % Startwert für die Optimierung (Wichtig!)
    x0 = x_real(:, 1); 
    
    % ---------------------------------------------------------
    % B. KOSTEN DER REALEN POLITIK BERECHNEN (J_actual)
    % ---------------------------------------------------------
    J_actual = 0;
    for k = 1:N
        % Kostenfunktion anwenden auf reale Daten
        step_cost = (x_real(:,k)' * Q_base * x_real(:,k)) + ...
                    (u_real(:,k)' * R * u_real(:,k));
        J_actual = J_actual + (beta^(k-1)) * step_cost;
    end
    % Terminal Cost (Endzustand der realen Daten)
    J_actual = J_actual + (beta^N) * (x_real(:,N+1)' * Q_N * x_real(:,N+1));
    
    % ---------------------------------------------------------
    % C. OPTIMALE POLITIK BERECHNEN (J_optimal)
    % ---------------------------------------------------------
    % Wir fragen den LQR Solver: "Was hättest du mit Startwert x0 getan?"
    
    x_opt = zeros(4, N+1); 
    x_opt(:,1) = x0; % Gleicher Startpunkt wie Realität!
    u_opt = zeros(4, N);
    J_optimal = 0;
    
    for k = 1:N
        % LQR Regel
        u_k = -K{k} * x_opt(:,k);
        
        % Constraints auch hier anwenden (Fairness)
        u_k(1) = max(0, min(1, u_k(1))); 
        u_k(2:4) = max(0, u_k(2:4));
        
        u_opt(:,k) = u_k;
        
        % Kosten summieren
        step_cost = (x_opt(:,k)' * Q_base * x_opt(:,k)) + ...
                    (u_opt(:,k)' * R * u_opt(:,k));
        J_optimal = J_optimal + (beta^(k-1)) * step_cost;
        
        % System update (Modell-Welt)
        x_opt(:,k+1) = A * x_opt(:,k) + B * u_k; 
    end
    J_optimal = J_optimal + (beta^N) * (x_opt(:,N+1)' * Q_N * x_opt(:,N+1));
    
    % ---------------------------------------------------------
    % D. ERGEBNIS SPEICHERN
    % ---------------------------------------------------------
    gap = (J_actual - J_optimal) / J_optimal * 100;
    
    results.Country(i) = country_name;
    results.Cost_Actual(i) = J_actual;
    results.Cost_Optimal(i) = J_optimal;
    results.Efficiency_Gap_Pct(i) = gap;
    
    if gap < 20
        results.Verdict(i) = "High Efficiency";
    elseif gap < 100
        results.Verdict(i) = "Moderate Efficiency";
    else
        results.Verdict(i) = "Inefficient / Bad Luck";
    end
end

% Sortieren nach Effizienz (Beste zuerst)
results = sortrows(results, 'Efficiency_Gap_Pct');

disp('Ranking der OECD Länder nach Policy Effizienz:');
disp(results);

% Plotten
figure('Name', 'OECD Efficiency Ranking', 'Color', 'w');
bar(categorical(results.Country), results.Efficiency_Gap_Pct);
ylabel('Welfare Loss vs. Optimal Policy (%)');
title('Pandemic Policy Efficiency (Lower is Better)');
grid on;