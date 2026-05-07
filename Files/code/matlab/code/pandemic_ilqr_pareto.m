%% ========================================================================
%  PANDEMIC TRILEMMA — STAGE 2: iLQR SOLVER + PARETO FRONTIER
%
%  Iterative LQR mit Pareto-Frontier-Sweep über Output-Debt-Trade-off.
%  Loadable: erwartet cdata, P aus V5b workspace.
%
%  STATE:   x = (y, b, w, z) wobei w = F^CP_{k-1}, z = F^DI_{k-1}
%  CONTROL: u = (F^CP_k, F^DI_k)
%
%  PLANNER PROBLEM:
%    min sum_k beta^k * [w_y * y_k^2 + w_b * b_k^2 + r_cp*F^CP_k^2 + r_di*F^DI_k^2]
%        + beta^N * W_b * b_N^2
%    s.t.: x_{k+1} = f(x_k, u_k, exog_k)
%          u_lo <= u_k <= u_hi
%
%  PARETO SWEEP: variiert lambda in [0, 1]:
%    w_y_eff = lambda * w_y_base
%    w_b_eff = (1-lambda) * w_b_base
%  bei festen Control-Costs r_cp, r_di.
% =========================================================================

%% ========================================================================
%  WELFARE GAIN: Observed vs Optimal (per country)
% =========================================================================
fprintf('\n========================================\n');
fprintf('  WELFARE GAIN: Observed vs Pareto-Optimal\n');
fprintf('========================================\n');

for ii = 1:length(demo_idx)
    i = demo_idx(ii);
    iso = cdata(i).iso;
    
    % Observed outcome (model with observed levers)
    y_obs_loss = sum(cdata(i).sim_y(1:K_act).^2);
    b_obs_term = cdata(i).sim_b(K_act);
    
    % Find lambda where optimal b_N matches observed b_N (debt-matched)
    [~, j_match] = min(abs(results(ii).b_term - b_obs_term));
    y_opt_match = results(ii).y_loss(j_match);
    
    % Welfare gain = output gain at same debt level
    y_gain = y_obs_loss - y_opt_match;
    pct_gain = 100 * y_gain / y_obs_loss;
    
    fprintf('\n  %s:\n', iso);
    fprintf('    Observed:  sumY^2=%.1f, b_N=%.2f\n', y_obs_loss, b_obs_term);
    fprintf('    Optimal at same debt (lam=%.2f): sumY^2=%.1f, b_N=%.2f\n', ...
        lambda_grid(j_match), y_opt_match, results(ii).b_term(j_match));
    fprintf('    Welfare gain: %.1f units (%.1f%% of observed loss)\n', ...
        y_gain, pct_gain);
    fprintf('    Optimal F^CP=%.2f vs observed=%.2f\n', ...
        results(ii).F_cp(j_match), sum(cdata(i).FCP));
    fprintf('    Optimal F^DI=%.2f vs observed=%.2f\n', ...
        results(ii).F_di(j_match), sum(cdata(i).FDI));
end

%% ========================================================================
%  OPTIMAL COMPOSITION PLOT: Observed vs Optimal Lever Trajectories
% =========================================================================
figure('Name','Optimal Composition','Color','w','Position',[100 100 1200 700]);

for ii = 1:length(demo_idx)
    i = demo_idx(ii);
    iso = cdata(i).iso;
    
    % Find debt-matched optimal lambda
    b_obs_term = cdata(i).sim_b(K_act);
    [~, j_match] = min(abs(results(ii).b_term - b_obs_term));
    lam_opt = lambda_grid(j_match);
    
    % Re-solve at matched lambda to get full trajectory
    Pj = P;
    Pj.w_y = lam_opt * w_y_base;
    Pj.w_b = (1 - lam_opt) * w_b_base;
    Pj.W_b = (1 - lam_opt) * W_b_base;
    Pj.r_cp = r_cp; Pj.r_di = r_di;
    u_init = [cdata(i).FCP; cdata(i).FDI];
    [u_opt, ~, ~] = run_ilqr(u_init, cdata(i), Pj, ilqr_opts);
    
    % --- F^CP plot ---
    subplot(2, length(demo_idx), ii); hold on; grid on;
    bar(1:N, [cdata(i).FCP; u_opt(1,:)]', 'grouped');
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45, 'FontSize', 7);
    ylabel('F^{CP} (pp)');
    title(sprintf('%s: F^{CP} (\\lambda^*=%.2f)', iso, lam_opt));
    legend('Observed','Optimal','Location','NE','FontSize',7);
    
    % --- F^DI plot ---
    subplot(2, length(demo_idx), ii+length(demo_idx)); hold on; grid on;
    bar(1:N, [cdata(i).FDI; u_opt(2,:)]', 'grouped');
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45, 'FontSize', 7);
    ylabel('F^{DI} (pp)');
    title(sprintf('%s: F^{DI}', iso));
    legend('Observed','Optimal','Location','NE','FontSize',7);
end

sgtitle('Observed vs Pareto-Optimal Fiscal Composition (debt-matched)','FontWeight','bold');


%% ========================================================================
%  iLQR HYPER-PARAMETERS
% =========================================================================
ilqr_opts = struct(...
    'max_iter',      50, ...
    'tol_du',        1e-4, ...     % Konvergenz: max |du|
    'tol_J',         1e-6, ...     % Konvergenz: relative cost change
    'reg_init',      1e-3, ...     % Levenberg-Marquardt Regularisierung
    'reg_max',       1e10, ...
    'reg_factor',    10, ...
    'line_search',   [1, 0.5, 0.25, 0.125, 0.0625], ... % step sizes
    'verbose',       false);

% Pareto-Sweep: lambda steuert relative Gewichtung y vs b
lambda_grid = [0.05, 0.10, 0.20, 0.35, 0.50, 0.65, 0.80, 0.90, 0.95];
n_lam = length(lambda_grid);

% Base-Weights (werden via lambda skaliert)
w_y_base = 100;
w_b_base = 30;
W_b_base = 150;
r_cp     = 3;
r_di     = 5;

%% ========================================================================
%  REFERENCE: Country selection für Pareto demo
% =========================================================================
% Wähle: AUT (median EU), USA (DI-heavy), DEU (CP-heavy), ITA (high CP)
demo_iso = {'AUT','USA','DEU','ITA'};
demo_idx = find(ismember({cdata.iso}, demo_iso));

%% ========================================================================
%  RUN PARETO SWEEP per country
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STAGE 2: iLQR + PARETO FRONTIER\n');
fprintf('========================================\n');

results = struct();
for ii = 1:length(demo_idx)
    i = demo_idx(ii);
    iso = cdata(i).iso;
    fprintf('\n--- Country: %s ---\n', iso);
    
    pareto_y = zeros(n_lam, 1);
    pareto_b = zeros(n_lam, 1);
    pareto_F_cp = zeros(n_lam, 1);
    pareto_F_di = zeros(n_lam, 1);
    
    for j = 1:n_lam
        lam = lambda_grid(j);
        Pj = P;
        Pj.w_y = lam * w_y_base;
        Pj.w_b = (1 - lam) * w_b_base;
        Pj.W_b = (1 - lam) * W_b_base;
        Pj.r_cp = r_cp;
        Pj.r_di = r_di;
        
        % Initial guess: observed controls
        u_init = [cdata(i).FCP; cdata(i).FDI];   % 2 x N (CP aggregate, DI)
        
        [u_opt, x_opt, J_hist] = run_ilqr(u_init, cdata(i), Pj, ilqr_opts);
        
        pareto_y(j)    = sum(x_opt(1, 2:end).^2);     % squared output gap
        pareto_b(j)    = x_opt(2, end);                % terminal cum debt
        pareto_F_cp(j) = sum(u_opt(1,:));
        pareto_F_di(j) = sum(u_opt(2,:));
        
        fprintf('  lam=%.2f: sumY^2=%.1f, b_N=%.2f, F^CP=%.2f, F^DI=%.2f\n', ...
            lam, pareto_y(j), pareto_b(j), pareto_F_cp(j), pareto_F_di(j));
    end
    
    results(ii).iso = iso;
    results(ii).lambda = lambda_grid;
    results(ii).y_loss = pareto_y;
    results(ii).b_term = pareto_b;
    results(ii).F_cp   = pareto_F_cp;
    results(ii).F_di   = pareto_F_di;
end

%% ========================================================================
%  VISUALIZATION: Pareto Frontier
% =========================================================================
figure('Name','Pareto Frontier','Color','w','Position',[100 100 1100 700]);

% Frontier output-debt
subplot(2,2,1); hold on; grid on;
colors = lines(length(demo_idx));
for ii = 1:length(demo_idx)
    plot(results(ii).y_loss, results(ii).b_term, '-o', ...
        'Color', colors(ii,:), 'LineWidth', 1.8, 'MarkerSize', 6, ...
        'DisplayName', results(ii).iso);
end
xlabel('Cumulative Output Loss (sum y_k^2)');
ylabel('Terminal Debt b_N (pp)');
title('Pareto Frontier: Output vs Debt');
legend('Location','NE');

% Lambda vs F^CP
subplot(2,2,2); hold on; grid on;
for ii = 1:length(demo_idx)
    plot(lambda_grid, results(ii).F_cp, '-o', ...
        'Color', colors(ii,:), 'LineWidth', 1.8, 'MarkerSize', 6, ...
        'DisplayName', results(ii).iso);
end
xlabel('\lambda (output weight share)');
ylabel('Cum F^{CP} deployment');
title('CP intensity along frontier');
legend('Location','NW');

% Lambda vs F^DI
subplot(2,2,3); hold on; grid on;
for ii = 1:length(demo_idx)
    plot(lambda_grid, results(ii).F_di, '-o', ...
        'Color', colors(ii,:), 'LineWidth', 1.8, 'MarkerSize', 6, ...
        'DisplayName', results(ii).iso);
end
xlabel('\lambda (output weight share)');
ylabel('Cum F^{DI} deployment');
title('DI intensity along frontier');
legend('Location','NW');

% Composition shift along frontier
subplot(2,2,4); hold on; grid on;
for ii = 1:length(demo_idx)
    cp_share = results(ii).F_cp ./ (results(ii).F_cp + results(ii).F_di + 1e-6);
    plot(lambda_grid, cp_share, '-o', ...
        'Color', colors(ii,:), 'LineWidth', 1.8, 'MarkerSize', 6, ...
        'DisplayName', results(ii).iso);
end
xlabel('\lambda (output weight share)');
ylabel('CP share = F^{CP} / (F^{CP}+F^{DI})');
title('Composition shift');
legend('Location','SW');

sgtitle('Pareto Frontier: Optimal Fiscal Composition','FontWeight','bold');

% Frontier output-debt + observed point
subplot(2,2,1); hold on; grid on;
colors = lines(length(demo_idx));
for ii = 1:length(demo_idx)
    i = demo_idx(ii);
    
    % Plot Pareto frontier
    plot(results(ii).y_loss, results(ii).b_term, '-o', ...
        'Color', colors(ii,:), 'LineWidth', 1.8, 'MarkerSize', 6, ...
        'DisplayName', results(ii).iso);
    
    % Compute observed point (with calibrated model)
    y_obs_loss = sum(cdata(i).sim_y(1:K_act).^2);
    b_obs_term = cdata(i).sim_b(K_act);
    plot(y_obs_loss, b_obs_term, 'p', ...
        'MarkerFaceColor', colors(ii,:), 'MarkerEdgeColor', 'k', ...
        'MarkerSize', 14, 'LineWidth', 1.2, ...
        'DisplayName', sprintf('%s observed', results(ii).iso));
end
xlabel('Cumulative Output Loss (sum y_k^2)');
ylabel('Terminal Debt b_N (pp)');
title('Pareto Frontier: Output vs Debt (\star = observed)');
legend('Location','NE','FontSize',7);
%% ########################################################################
%  iLQR CORE FUNCTIONS
%  ########################################################################

function [u_opt, x_opt, J_hist] = run_ilqr(u_init, cd, P, opts)
%  Iterative LQR with Levenberg-Marquardt regularization and line search.
%  Inputs:
%    u_init : 2 x N initial control sequence
%    cd     : cdata(i) — Country-specific exogenous inputs
%    P      : parameters struct (with weights)
%    opts   : iLQR hyper-parameters
%  Outputs:
%    u_opt, x_opt : optimal control + state trajectory
%    J_hist       : cost history per iteration

    N = P.N;
    nx = P.nx; nu = P.nu;
    u = u_init;
    
    % Forward simulate initial trajectory
    x = simulate_trajectory(u, cd, P);
    J = cost(x, u, P);
    J_hist = J;
    
    reg = opts.reg_init;
    
    for iter = 1:opts.max_iter
        % --- Backward pass: compute Jacobians + value function ---
        [A, B] = compute_jacobians(x, u, cd, P);
        [K, k_ff, success] = backward_pass(A, B, x, u, P, reg);
        
        if ~success
            reg = min(reg * opts.reg_factor, opts.reg_max);
            if reg >= opts.reg_max, break; end
            continue;
        end
        
        % --- Forward pass with line search ---
        J_new = inf; u_new = u; x_new = x;
        for alpha = opts.line_search
            [u_try, x_try] = forward_pass(u, x, K, k_ff, alpha, cd, P);
            J_try = cost(x_try, u_try, P);
            if J_try < J_new
                J_new = J_try;
                u_new = u_try;
                x_new = x_try;
            end
            if J_try < J, break; end
        end
        
        % --- Convergence check ---
        du_max = max(abs(u_new(:) - u(:)));
        rel_change = (J - J_new) / max(abs(J), 1e-10);
        
        if J_new < J
            u = u_new; x = x_new;
            J_hist(end+1) = J_new;
            reg = max(reg / opts.reg_factor, opts.reg_init);
            if du_max < opts.tol_du || rel_change < opts.tol_J
                break;
            end
            J = J_new;
        else
            reg = min(reg * opts.reg_factor, opts.reg_max);
            if reg >= opts.reg_max, break; end
        end
    end
    
    u_opt = u; x_opt = x;
end

function x = simulate_trajectory(u, cd, P)
%  Forward-simulate state given controls u (2 x N) and exogenous inputs.
    N = P.N;
    x = zeros(P.nx, N+1);
    for k = 1:N
        x(:, k+1) = dynamics_step(x(:,k), u(:,k), k, cd, P);
    end
end

function x_next = dynamics_step(x, u, k, cd, P)
%  Single-step transition.
%  x = (y, b, w, z), u = (F^CP_k, F^DI_k)
%  CP-Decomposition: in der ilQR-Optimierung verwenden wir aggregate kappa_CP_pooled
%  (pooled spec) statt three-way decomposition. Robustness via lambda-Sweep.
    y = x(1); b = x(2); w = x(3); z = x(4);
    fcp_k = u(1); fdi_k = u(2);
    
    Sk     = 0; if k <= length(cd.S),     Sk     = cd.S(k);     end
    dk     = 0; if k <= length(cd.d),     dk     = cd.d(k);     end
    hk     = 0; if k <= length(cd.FH),    hk     = cd.FH(k);    end
    ey     = 0; if k+1 <= length(P.eps_y_vec),  ey     = P.eps_y_vec(k+1);     end
    yr_idx = 0; if k <= length(P.year_idx_vec), yr_idx = P.year_idx_vec(k);    end
    
    spline_active = (y < 0);
    
    y_next = cd.mu_y + P.rho_y*y + P.alpha_S*Sk ...
           + spline_active * P.eta_p * w * y ...
           + P.alpha_F_DI*z + P.beta_fear*dk + ey;
    
    b_next = cd.mu_b + (1+P.r_int)*b - P.gamma_y*y ...
           + P.kappa_CP_pooled*fcp_k ...
           + P.kappa_F_DI*z ...
           + P.kappa_H*hk + P.phi_t*yr_idx;
    
    w_next = fcp_k;
    z_next = fdi_k;
    
    x_next = [y_next; b_next; w_next; z_next];
end

function [A, B] = compute_jacobians(x, u, cd, P)
%  Numerical Jacobians df/dx, df/du via finite differences.
    N = P.N; nx = P.nx; nu = P.nu;
    A = zeros(nx, nx, N);
    B = zeros(nx, nu, N);
    eps = 1e-6;
    
    for k = 1:N
        x_k = x(:,k); u_k = u(:,k);
        f0 = dynamics_step(x_k, u_k, k, cd, P);
        
        % df/dx
        for i = 1:nx
            x_p = x_k; x_p(i) = x_p(i) + eps;
            f_p = dynamics_step(x_p, u_k, k, cd, P);
            A(:, i, k) = (f_p - f0) / eps;
        end
        % df/du
        for j = 1:nu
            u_p = u_k; u_p(j) = u_p(j) + eps;
            f_p = dynamics_step(x_k, u_p, k, cd, P);
            B(:, j, k) = (f_p - f0) / eps;
        end
    end
end

function J = cost(x, u, P)
%  Quadratic cost: state penalty on (y, b) only; control penalty on (F^CP, F^DI).
    N = P.N;
    Q = diag([P.w_y, P.w_b, 0, 0]);
    QN = diag([P.w_y, P.W_b, 0, 0]);
    R = diag([P.r_cp, P.r_di]);
    
    J = 0;
    for k = 1:N
        J = J + P.beta_disc^(k-1) * (x(:,k)' * Q * x(:,k) + u(:,k)' * R * u(:,k));
    end
    J = J + P.beta_disc^N * x(:,N+1)' * QN * x(:,N+1);
end

function [K, k_ff, success] = backward_pass(A, B, x, u, P, reg)
%  Riccati recursion with regularization.
    N = P.N; nx = P.nx; nu = P.nu;
    Q = diag([P.w_y, P.w_b, 0, 0]);
    QN = diag([P.w_y, P.W_b, 0, 0]);
    R = diag([P.r_cp, P.r_di]);
    
    K = zeros(nu, nx, N);
    k_ff = zeros(nu, N);
    
    Vx = QN * x(:, N+1);
    Vxx = QN;
    
    success = true;
    for k = N:-1:1
        Ak = A(:,:,k); Bk = B(:,:,k);
        
        Qx  = Q * x(:,k) + Ak' * Vx;
        Qu  = R * u(:,k) + Bk' * Vx;
        Qxx = Q + Ak' * Vxx * Ak;
        Quu = R + Bk' * Vxx * Bk + reg * eye(nu);
        Qux = Bk' * Vxx * Ak;
        
        % Check positive definiteness
        [~, p] = chol(Quu);
        if p > 0
            success = false;
            return;
        end
        
        K(:,:,k) = -Quu \ Qux;
        k_ff(:,k) = -Quu \ Qu;
        
        Vx  = Qx + K(:,:,k)' * Quu * k_ff(:,k) + K(:,:,k)' * Qu + Qux' * k_ff(:,k);
        Vxx = Qxx + K(:,:,k)' * Quu * K(:,:,k) + K(:,:,k)' * Qux + Qux' * K(:,:,k);
        Vxx = 0.5 * (Vxx + Vxx');   % enforce symmetry
    end
end

function [u_new, x_new] = forward_pass(u, x, K, k_ff, alpha, cd, P)
%  Forward roll with feedback + feedforward updates, clipped to bounds.
    N = P.N;
    u_new = zeros(size(u));
    x_new = zeros(size(x));
    x_new(:,1) = x(:,1);
    
    for k = 1:N
        du = alpha * k_ff(:,k) + K(:,:,k) * (x_new(:,k) - x(:,k));
        u_new(:,k) = u(:,k) + du;
        % Apply control bounds
        u_new(:,k) = max(P.u_lo, min(P.u_hi, u_new(:,k)));
        x_new(:,k+1) = dynamics_step(x_new(:,k), u_new(:,k), k, cd, P);
    end
end
