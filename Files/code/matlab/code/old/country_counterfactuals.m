%% ========================================================================
%  COUNTRY-SPECIFIC COUNTERFACTUALS
%  Fix S and theta at observed values, optimize only F_CP and F_DI
% =========================================================================
clear; clc; close all;

%% ========================================================================
%  STEP 0: LOAD DATA AND PARAMETERS
% =========================================================================

% --- Load CSV ---
T = readtable('country_data_for_matlab.csv');

% Fix Quarter ordering
quarter_order = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
                 'Q1.2021','Q2.2021','Q3.2021','Q4.2021','Q1.2022'};
T.Quarter = categorical(T.Quarter, quarter_order, 'Ordinal', true);
T = sortrows(T, {'Country', 'Quarter'});

countries = unique(T.Country);
n_countries = length(countries);
N = 9;  % Quarters

fprintf('=== Country Counterfactuals ===\n');
fprintf('  Countries: %d\n', n_countries);
fprintf('  Quarters:  %d\n', N);

% --- Structural Parameters (from panel estimation) ---
params = struct();
params.rho_y      = 0.00;
params.psi        = 0.424;
params.alpha_S    = 0.028;
params.alpha_F_CP = 0.253;
params.gamma_int  = -0.596;
params.alpha_F_DI = 0.244;
params.beta_fear  = -50;
params.r_int      = 0.001;
params.gamma_y    = 0.191;
params.kappa_F_CP = 0.193;
params.kappa_F_DI = 0.468;
params.c_H        = 0.02;
params.beta_disc  = 0.99;
params.FDI_max    = 0.08;
params.FCP_max    = 0.20;

% IFR schedule (wave-specific, from V5 calibration)
% Approximate: declining over time due to vaccination + Omicron
ifr_by_quarter = [0.030, 0.030, 0.025, 0.025, 0.020, 0.015, 0.010, 0.005, 0.003];

% --- Weights (VSL=60 baseline) ---
w_y  = 100;
w_b  = 30;   % = w_y * MCPF
W_b  = 30;
r_DI = 5;
r_CP = 0.5;


%% ========================================================================
%  STEP 1: EXTRACT COUNTRY DATA
% =========================================================================

% Pre-allocate country data structure
cdata = struct();

for i = 1:n_countries
    c = countries{i};
    idx = strcmp(T.Country, c);
    ct = T(idx, :);
    ct = sortrows(ct, 'Quarter');
    
    % Scale to fractions (MATLAB convention: S in [0,1], F in fractions)
    cdata(i).name       = c;
    cdata(i).S_obs      = ct.S_mean_tw' / 100;          % [0,1]
    cdata(i).theta_obs  = ct.theta_pct' / 100;           % fraction
    cdata(i).F_CP_obs   = ct.F_CP' / 100;                % fraction of GDP
    cdata(i).F_DI_obs   = ct.F_DI' / 100;                % fraction of GDP
    cdata(i).F_H_obs    = ct.F_H' / 100;                 % fraction of GDP
    cdata(i).y_obs      = ct.y_t_pct' / 100;             % fraction
    cdata(i).debt_obs   = ct.debt_dR' / 100;             % fraction of GDP
    cdata(i).vax_rate   = ct.vax_rate';
    
    % Replace NaNs with 0 (debt_dR in Q1.2020)
    cdata(i).debt_obs(isnan(cdata(i).debt_obs)) = 0;
    cdata(i).F_H_obs(isnan(cdata(i).F_H_obs)) = 0;
    
    % Pre-compute exogenous mortality flow
    cdata(i).d_exo = zeros(1, N);
    for k = 1:N
        cdata(i).d_exo(k) = ifr_by_quarter(k) * cdata(i).theta_obs(k);
    end
end

fprintf('  Data loaded for %d countries.\n', n_countries);


%% ========================================================================
%  STEP 2: RUN COUNTERFACTUALS FOR EACH COUNTRY
% =========================================================================

% Results storage
results = struct();
results.country   = cell(n_countries, 1);
results.F_CP_obs_avg  = zeros(n_countries, 1);
results.F_DI_obs_avg  = zeros(n_countries, 1);
results.F_CP_opt_avg  = zeros(n_countries, 1);
results.F_DI_opt_avg  = zeros(n_countries, 1);
results.y_obs_avg     = zeros(n_countries, 1);
results.y_opt_avg     = zeros(n_countries, 1);
results.y_nofiscal_avg = zeros(n_countries, 1);
results.b_obs_end     = zeros(n_countries, 1);
results.b_opt_end     = zeros(n_countries, 1);
results.b_nofiscal_end = zeros(n_countries, 1);
results.welfare_gain  = zeros(n_countries, 1);
results.fiscal_value  = zeros(n_countries, 1);

for i = 1:n_countries
    fprintf('\n--- Country %d/%d: %s ---\n', i, n_countries, cdata(i).name);
    
    cd_i = cdata(i);
    S_obs = cd_i.S_obs;
    theta_obs = cd_i.theta_obs;
    d_exo = cd_i.d_exo;
    F_H_obs = cd_i.F_H_obs;
    
    % ========================================
    % Scenario A: Observed policy (forward sim)
    % ========================================
    x_A = zeros(4, N+1);  % [y; b; z1; z2]
    
    for k = 1:N
        y_k = x_A(1,k); b_k = x_A(2,k);
        z1_k = x_A(3,k); z2_k = x_A(4,k);
        FCP = cd_i.F_CP_obs(k); FDI = cd_i.F_DI_obs(k);
        
        x_A(1,k+1) = (params.rho_y + params.psi*S_obs(k)) * y_k ...
                    - params.alpha_S*S_obs(k) ...
                    + (params.alpha_F_CP + params.gamma_int*S_obs(k)) * FCP ...
                    + params.alpha_F_DI * z2_k ...
                    + params.beta_fear * d_exo(k);
        
        x_A(2,k+1) = (1+params.r_int)*b_k - params.gamma_y*y_k ...
                    + params.kappa_F_CP*FCP + params.kappa_F_DI*z1_k ...
                    + params.c_H*theta_obs(k) + F_H_obs(k);
        
        x_A(3,k+1) = FDI;
        x_A(4,k+1) = z1_k;
    end
    
    % ========================================
    % Scenario C: No fiscal (forward sim)
    % ========================================
    x_C = zeros(4, N+1);
    
    for k = 1:N
        y_k = x_C(1,k); b_k = x_C(2,k);
        z1_k = x_C(3,k); z2_k = x_C(4,k);
        
        x_C(1,k+1) = (params.rho_y + params.psi*S_obs(k)) * y_k ...
                    - params.alpha_S*S_obs(k) ...
                    + params.alpha_F_DI * z2_k ...
                    + params.beta_fear * d_exo(k);
        
        x_C(2,k+1) = (1+params.r_int)*b_k - params.gamma_y*y_k ...
                    + params.kappa_F_DI*z1_k ...
                    + params.c_H*theta_obs(k) + F_H_obs(k);
        
        x_C(3,k+1) = 0;
        x_C(4,k+1) = z1_k;
    end
    
    % ========================================
    % Scenario B: Optimal fiscal (iLQR)
    % ========================================
    [x_B, u_B] = solve_country_ilqr(cd_i, params, w_y, w_b, W_b, r_DI, r_CP, ...
                                     ifr_by_quarter, N);
    
    % ========================================
    % Welfare comparison
    % ========================================
    J_A = welfare_cost(x_A, w_y, w_b, W_b, params.beta_disc, N);
    J_B = welfare_cost(x_B, w_y, w_b, W_b, params.beta_disc, N);
    J_C = welfare_cost(x_C, w_y, w_b, W_b, params.beta_disc, N);
    
    % Store results
    results.country{i}       = cdata(i).name;
    results.F_CP_obs_avg(i)  = mean(cd_i.F_CP_obs) * 100;
    results.F_DI_obs_avg(i)  = mean(cd_i.F_DI_obs) * 100;
    results.F_CP_opt_avg(i)  = mean(u_B(2,:)) * 100;
    results.F_DI_opt_avg(i)  = mean(u_B(1,:)) * 100;
    results.y_obs_avg(i)     = mean(x_A(1,1:N)) * 100;
    results.y_opt_avg(i)     = mean(x_B(1,1:N)) * 100;
    results.y_nofiscal_avg(i) = mean(x_C(1,1:N)) * 100;
    results.b_obs_end(i)     = x_A(2,end) * 100;
    results.b_opt_end(i)     = x_B(2,end) * 100;
    results.b_nofiscal_end(i) = x_C(2,end) * 100;
    results.welfare_gain(i)  = (J_A - J_B) / abs(J_A) * 100;
    results.fiscal_value(i)  = (J_C - J_A) / abs(J_C) * 100;
    
    fprintf('    Observed: y=%.1f%%, b=%.1f pp, CP=%.1f%%, DI=%.2f%%\n', ...
            results.y_obs_avg(i), results.b_obs_end(i), ...
            results.F_CP_obs_avg(i), results.F_DI_obs_avg(i));
    fprintf('    Optimal:  y=%.1f%%, b=%.1f pp, CP=%.1f%%, DI=%.2f%%\n', ...
            results.y_opt_avg(i), results.b_opt_end(i), ...
            results.F_CP_opt_avg(i), results.F_DI_opt_avg(i));
    fprintf('    Welfare gain: %.1f%%\n', results.welfare_gain(i));
end


%% ========================================================================
%  STEP 3: RESULTS TABLE
% =========================================================================

fprintf('\n\n========================================\n');
fprintf('  COUNTRY COUNTERFACTUAL RESULTS\n');
fprintf('========================================\n\n');

fprintf('  %3s | %6s %6s | %6s %6s | %6s %6s | %6s %6s | %6s %6s\n', ...
        '', 'CP_obs', 'CP_opt', 'DI_obs', 'DI_opt', ...
        'y_obs', 'y_opt', 'b_obs', 'b_opt', 'W_gain', 'F_val');
fprintf('  %s\n', repmat('-', 1, 85));

for i = 1:n_countries
    fprintf('  %3s | %6.2f %6.2f | %6.2f %6.2f | %6.1f %6.1f | %6.1f %6.1f | %6.1f %6.1f\n', ...
        results.country{i}, ...
        results.F_CP_obs_avg(i), results.F_CP_opt_avg(i), ...
        results.F_DI_obs_avg(i), results.F_DI_opt_avg(i), ...
        results.y_obs_avg(i), results.y_opt_avg(i), ...
        results.b_obs_end(i), results.b_opt_end(i), ...
        results.welfare_gain(i), results.fiscal_value(i));
end


%% ========================================================================
%  STEP 4: VISUALIZATION
% =========================================================================

% --- Plot 1: Fiscal Efficiency Gap (CP) ---
figure('Name', 'Fiscal Efficiency Gap', 'Color', 'w', ...
       'Position', [50 50 700 600]);

max_val = max([results.F_CP_obs_avg; results.F_CP_opt_avg]) * 1.1;
plot([0 max_val], [0 max_val], 'k--', 'LineWidth', 1); hold on;
scatter(results.F_CP_obs_avg, results.F_CP_opt_avg, 50, 'filled', ...
        'MarkerFaceColor', [0.2 0.5 0.8]);

for i = 1:n_countries
    text(results.F_CP_obs_avg(i)+0.05, results.F_CP_opt_avg(i), ...
         results.country{i}, 'FontSize', 7);
end

xlabel('Observed CP (avg. % GDP)'); ylabel('Optimal CP (avg. % GDP)');
title('Fiscal Efficiency Gap: Observed vs. Optimal CP');
subtitle('Below 45° line: country deployed too much CP. Above: too little.');
grid on; axis equal;
xlim([0 max_val]); ylim([0 max_val]);


% --- Plot 2: Output Gain vs. Debt Saving ---
figure('Name', 'Welfare Decomposition', 'Color', 'w', ...
       'Position', [50 50 700 600]);

delta_y = results.y_opt_avg - results.y_obs_avg;        % Positive = improvement
delta_b = results.b_obs_end - results.b_opt_end;        % Positive = saving

scatter(delta_b, delta_y, 50, 'filled', 'MarkerFaceColor', [0.2 0.7 0.3]);
hold on;
for i = 1:n_countries
    text(delta_b(i)+0.05, delta_y(i), results.country{i}, 'FontSize', 7);
end

xline(0, '--k'); yline(0, '--k');
xlabel('Debt Saving (pp GDP): Observed - Optimal');
ylabel('Output Gain (pp): Optimal - Observed');
title('Welfare Decomposition by Country');
subtitle('Top-right: optimal policy improves both output AND reduces debt');
grid on;


% --- Plot 3: Case Study (CAN vs CHE) ---
can_idx = find(strcmp(results.country, 'CAN'));
che_idx = find(strcmp(results.country, 'CHE'));

if ~isempty(can_idx) && ~isempty(che_idx)
    figure('Name', 'Case Study: CAN vs CHE', 'Color', 'w', ...
           'Position', [50 50 1000 400]);
    
    case_countries = [can_idx, che_idx];
    case_labels = {'Canada', 'Switzerland'};
    case_colors = {[0.8 0.2 0.2], [0.2 0.4 0.8]};
    
    for j = 1:2
        ci = case_countries(j);
        
        % Recompute trajectories for plotting
        cd_j = cdata(ci);
        x_A_j = recompute_observed(cd_j, params, ifr_by_quarter, N);
        [x_B_j, u_B_j] = solve_country_ilqr(cd_j, params, w_y, w_b, W_b, ...
                                              r_DI, r_CP, ifr_by_quarter, N);
        
        subplot(1,3,1);
        plot(0:N, x_A_j(1,:)*100, '--', 'Color', case_colors{j}, 'LineWidth', 1.5); hold on;
        plot(0:N, x_B_j(1,:)*100, '-', 'Color', case_colors{j}, 'LineWidth', 2);
        
        subplot(1,3,2);
        plot(0:N, x_A_j(2,:)*100, '--', 'Color', case_colors{j}, 'LineWidth', 1.5); hold on;
        plot(0:N, x_B_j(2,:)*100, '-', 'Color', case_colors{j}, 'LineWidth', 2);
        
        subplot(1,3,3);
        bar_data_obs = [cd_j.F_CP_obs; cd_j.F_DI_obs]' * 100;
        bar_data_opt = [u_B_j(2,:); u_B_j(1,:)]' * 100;
    end
    
    subplot(1,3,1);
    yline(0, '--k'); title('Output Gap');
    ylabel('% deviation'); xlabel('Quarter'); grid on;
    legend('CAN obs', 'CAN opt', 'CHE obs', 'CHE opt', 'Location', 'SouthEast');
    
    subplot(1,3,2);
    yline(0, '--k'); title('Cumulative Debt');
    ylabel('pp GDP'); xlabel('Quarter'); grid on;
    
    subplot(1,3,3);
    title('Fiscal Composition');
    ylabel('% GDP'); xlabel('Quarter'); grid on;
    
    sgtitle('Case Study: Canada (DI-heavy) vs Switzerland (CP-heavy)');
end


% --- Plot 4: Welfare Gain Ranking ---
figure('Name', 'Welfare Gain Ranking', 'Color', 'w', ...
       'Position', [50 50 800 600]);

[sorted_gain, sort_idx] = sort(results.welfare_gain, 'descend');
sorted_names = results.country(sort_idx);

barh(1:n_countries, sorted_gain, 'FaceColor', [0.3 0.6 0.8]);
set(gca, 'YTick', 1:n_countries, 'YTickLabel', sorted_names, 'FontSize', 7);
xlabel('Welfare Gain from Optimal Fiscal Composition (%)');
title('Which Countries Would Have Benefited Most from Optimal Fiscal Policy?');
xline(0, '--k');
grid on;


%% ========================================================================
%  HELPER FUNCTIONS
% =========================================================================

function [x_opt, u_opt] = solve_country_ilqr(cd, params, w_y, w_b, W_b, ...
                                              r_DI, r_CP, ifr_by_quarter, N)
    % Reduced model: x = [y; b; z1; z2], u = [F_DI; F_CP]
    n_x = 4; n_u = 2;
    
    S_obs = cd.S_obs;
    theta_obs = cd.theta_obs;
    d_exo = cd.d_exo;
    F_H_obs = cd.F_H_obs;
    
    % Transition
    f_r = @(x, u, k) [
        (params.rho_y + params.psi*S_obs(k)) * x(1) ...
            - params.alpha_S*S_obs(k) ...
            + (params.alpha_F_CP + params.gamma_int*S_obs(k)) * u(2) ...
            + params.alpha_F_DI * x(4) ...
            + params.beta_fear * d_exo(k);
        (1+params.r_int)*x(2) - params.gamma_y*x(1) ...
            + params.kappa_F_CP*u(2) + params.kappa_F_DI*x(3) ...
            + params.c_H*theta_obs(k) + F_H_obs(k);
        u(1);
        x(3)
    ];
    
    % Jacobians
    Ak_r = @(x, u, k) [
        params.rho_y + params.psi*S_obs(k), 0,              0,              params.alpha_F_DI;
        -params.gamma_y,                     1+params.r_int, params.kappa_F_DI, 0;
        0,                                   0,              0,              0;
        0,                                   0,              1,              0
    ];
    
    Bk_r = @(x, u, k) [
        0,  params.alpha_F_CP + params.gamma_int*S_obs(k);
        0,  params.kappa_F_CP;
        1,  0;
        0,  0
    ];
    
    % Cost matrices
    Q_base = diag([w_y, w_b, 0, 0]);
    Q_N    = diag([0, W_b, 0, 0]);
    R      = diag([r_DI, r_CP]);
    
    % Constraints: F_DI >= 0, F_DI <= FDI_max, F_CP >= 0, F_CP <= FCP_max
    C_u = [-1 0; 1 0; 0 -1; 0 1];
    c_bounds = [0; params.FDI_max; 0; params.FCP_max];
    n_c = 4;
    
    % Initial trajectory: start with observed policy
    x_bar = zeros(n_x, N+1);
    u_bar = zeros(n_u, N);
    x_bar(:, 1) = [0; 0; 0; 0];
    
    for k = 1:N
        u_bar(:, k) = [cd.F_DI_obs(k); cd.F_CP_obs(k)];
        % Clip to bounds
        u_bar(1, k) = max(0, min(params.FDI_max, u_bar(1, k)));
        u_bar(2, k) = max(0, min(params.FCP_max, u_bar(2, k)));
        x_bar(:, k+1) = f_r(x_bar(:, k), u_bar(:, k), k);
    end
    
    % AL-iLQR
    max_outer = 25;
    max_inner = 50;
    tol_inner = 1e-4;
    tol_outer = 1e-4;
    phi_pen   = 2.5;
    rho_base  = 1e-4;
    
    lambda = zeros(n_c, N);
    mu     = 10.0 * ones(n_c, N);
    
    for outer = 1:max_outer
        for inner = 1:max_inner
            % Jacobians
            Ak = cell(N,1); Bk = cell(N,1);
            for k = 1:N
                Ak{k} = Ak_r(x_bar(:,k), u_bar(:,k), k);
                Bk{k} = Bk_r(x_bar(:,k), u_bar(:,k), k);
            end
            
            % Backward Riccati
            P = cell(N+1,1); p = cell(N+1,1);
            K = cell(N,1); k_ff = cell(N,1);
            P{N+1} = Q_N * params.beta_disc^N;
            p{N+1} = Q_N * params.beta_disc^N * x_bar(:,N+1);
            
            rho_reg = rho_base;
            bw_ok = false;
            while ~bw_ok && rho_reg < 1e6
                bw_ok = true;
                for k = N:-1:1
                    Q_k = Q_base * params.beta_disc^(k-1);
                    A = Ak{k}; B = Bk{k};
                    c_val = C_u * u_bar(:,k) - c_bounds;
                    act = (c_val > 0) | (lambda(:,k) > 0);
                    I_mu = diag(mu(:,k) .* act);
                    
                    Qx  = Q_k*x_bar(:,k) + A'*p{k+1};
                    Qxx = Q_k + A'*P{k+1}*A;
                    Qux = B'*P{k+1}*A;
                    Qu  = R*u_bar(:,k) + B'*p{k+1} + C_u'*(lambda(:,k) + I_mu*c_val);
                    Quu = R + B'*P{k+1}*B + C_u'*I_mu*C_u + rho_reg*eye(n_u);
                    
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
            
            % Forward pass with line search + clipping
            al = 1.0;
            cost_old = al_cost_r(x_bar, u_bar, Q_base, Q_N, R, params.beta_disc, ...
                                  N, lambda, mu, C_u, c_bounds);
            ls_ok = false;
            while al > 1e-8
                xn = zeros(n_x, N+1); un = zeros(n_u, N);
                xn(:,1) = [0; 0; 0; 0];
                for k = 1:N
                    un(:,k) = u_bar(:,k) + al*k_ff{k} - K{k}*(xn(:,k)-x_bar(:,k));
                    un(1,k) = max(0, min(params.FDI_max, un(1,k)));
                    un(2,k) = max(0, min(params.FCP_max, un(2,k)));
                    xn(:,k+1) = f_r(xn(:,k), un(:,k), k);
                end
                cn = al_cost_r(xn, un, Q_base, Q_N, R, params.beta_disc, ...
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
end


function x_A = recompute_observed(cd, params, ifr_by_quarter, N)
    x_A = zeros(4, N+1);
    for k = 1:N
        y_k = x_A(1,k); b_k = x_A(2,k);
        z1_k = x_A(3,k); z2_k = x_A(4,k);
        FCP = cd.F_CP_obs(k); FDI = cd.F_DI_obs(k);
        S = cd.S_obs(k);
        
        x_A(1,k+1) = (params.rho_y + params.psi*S) * y_k ...
                    - params.alpha_S*S ...
                    + (params.alpha_F_CP + params.gamma_int*S) * FCP ...
                    + params.alpha_F_DI * z2_k ...
                    + params.beta_fear * cd.d_exo(k);
        
        x_A(2,k+1) = (1+params.r_int)*b_k - params.gamma_y*y_k ...
                    + params.kappa_F_CP*FCP + params.kappa_F_DI*z1_k ...
                    + params.c_H*cd.theta_obs(k) + cd.F_H_obs(k);
        
        x_A(3,k+1) = FDI;
        x_A(4,k+1) = z1_k;
    end
end


function J = welfare_cost(x, w_y, w_b, W_b, beta, N)
    J = 0;
    for k = 1:N
        J = J + beta^(k-1) * (w_y*x(1,k)^2 + w_b*x(2,k)^2);
    end
    J = J + beta^N * W_b * x(2,N+1)^2;
end


function J = al_cost_r(x, u, Q, QN, R, beta, N, lam, mu, Cu, cb)
    J = 0;
    for k = 1:N
        sc = beta^(k-1) * (0.5*x(:,k)'*Q*x(:,k) + 0.5*u(:,k)'*R*u(:,k));
        cv = Cu*u(:,k) - cb;
        act = (cv > 0) | (lam(:,k) > 0);
        Im = diag(mu(:,k).*act);
        ac = lam(:,k)'*cv + 0.5*cv'*Im*cv;
        J = J + sc + ac;
    end
    J = J + beta^N * 0.5*x(:,N+1)'*QN*x(:,N+1);
end
