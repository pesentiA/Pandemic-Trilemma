%% ========================================================================
%  PANDEMIC TRILEMMA — PARETO FRONTIER ANALYSIS (V4)
%
%  Requires: pandemic_calibration_v4.m (must have been run first).
%
%  OBJECTIVE
%  ---------
%  Construct Pareto-efficient trade-off between cumulative output gap
%  loss and terminal debt accumulation, for each country, under both:
%    (a) Baseline coefficients (alpha_DI = 0.185, point estimate)
%    (b) Conservative coefficients (alpha_DI = 0, supply-constraint case)
%
%  The gap between the two frontiers quantifies the empirical
%  uncertainty driven by the insignificant DI coefficient.
%
%  METHOD
%  ------
%  Weighted-sum scalarization: for lambda in [0, 1], solve
%      min  lambda * sum(y_k^2) + (1-lambda) * b_N^2
%           + r_control * sum(F^CP_k^2 + F^DI_k^2)     (regularization)
%  subject to forward dynamics and u_lo <= u <= u_hi.
%
%  No budget constraint. Control bounds are the physical maximum
%  deployment rates (u_hi = [20, 10] pp/quarter).
%
%  Grid: 25 lambda values on sigmoidal spacing (more points near
%  lambda=0 and lambda=1 where the frontier is steepest).
%
%  OUTPUT
%  ------
%  For each country and each lambda:
%    (cum_y, terminal_b, F^CP_opt, F^DI_opt)
%
%  Plus:
%    - Location of observed policy relative to frontier
%    - Welfare dominance check (is observed inside or on frontier?)
%    - Frontier shape: convexity, range, dominance ordering across alpha_DI
% =========================================================================

if ~exist('P', 'var') || ~exist('cdata', 'var')
    error(['Run pandemic_calibration_v4.m first to set up P and cdata.']);
end

if ~exist('qlbl', 'var')
    qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21', ...
            'Q1.21','Q2.21','Q1.22','Q2.22'};
end

fprintf('\n\n');
fprintf('========================================\n');
fprintf('  PARETO FRONTIER ANALYSIS (V4)\n');
fprintf('========================================\n\n');

N_  = P.N;
Ka  = P.K_act;
n_c = length(cdata);


%% ========================================================================
%  STEP 1: SETUP
% =========================================================================

% Lambda grid: sigmoidal spacing, 25 points
n_lambda = 25;
t_grid = linspace(0, 1, n_lambda);
lambda_grid = 1 ./ (1 + exp(-8*(t_grid - 0.5)));   % sigmoidal
lambda_grid(1) = 0.001;   % avoid exact 0 (numerical)
lambda_grid(end) = 0.999; % avoid exact 1

fprintf('  Countries: %d\n',      n_c);
fprintf('  Lambda grid points: %d\n',  n_lambda);
fprintf('  Sensitivity variants: 2 (alpha_DI = %.3f, 0)\n', P.alpha_F_DI);
fprintf('  Total iLQR runs: %d\n\n', 2 * n_c * n_lambda);

% Regularization (small, constant — only for numerical stability)
r_control_reg = 0.01;

% Control bounds (from calibration)
u_lo = P.u_lo;
u_hi = P.u_hi;

% Prepare sensitivity P-struct
P_base = P;
P_sens = P; P_sens.alpha_F_DI = 0;


%% ========================================================================
%  STEP 2: COMPUTE FRONTIERS PER COUNTRY
% =========================================================================
fprintf('--- Step 2: Computing frontiers (this takes 10-30 minutes) ---\n');

pf = struct();
tic_total = tic;

for i = 1:n_c
    iso     = cdata(i).iso;
    S_i     = cdata(i).S;       th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i    = cdata(i).mu_y;    mb_i = cdata(i).mu_b;
    fcp_o   = cdata(i).FCP;     fdi_o = cdata(i).FDI;
    fcp_a_o = cdata(i).FCP_above;
    fcp_l_o = cdata(i).FCP_loans;
    fcp_g_o = cdata(i).FCP_guar;
    fh_i    = cdata(i).FH;

    % Country composition (constant across quarters, from observed cumulative)
    above_tot = sum(fcp_a_o(1:Ka));
    below_tot = sum(fcp_l_o(1:Ka)) + sum(fcp_g_o(1:Ka));
    cp_tot    = above_tot + below_tot;
    if cp_tot > 0.01
        fa_i = above_tot / cp_tot;
    else
        fa_i = 0.55;   % OECD average fallback
    end
    fb_i = 1 - fa_i;

    % Compute observed point
    xs_obs = forward_roll(fcp_o, fcp_a_o, fcp_l_o, fcp_g_o, ...
                          fdi_o, fh_i, S_i, th_i, d_i, my_i, mb_i, P_base);
    xs_obs_sens = forward_roll(fcp_o, fcp_a_o, fcp_l_o, fcp_g_o, ...
                               fdi_o, fh_i, S_i, th_i, d_i, my_i, mb_i, P_sens);

    pf(i).iso = iso;
    pf(i).fa_i = fa_i;
    pf(i).fb_i = fb_i;
    pf(i).cum_y_obs      = sum(xs_obs(1, 2:Ka+1));
    pf(i).bN_obs         = xs_obs(2, N_+1);
    pf(i).cum_y_obs_sens = sum(xs_obs_sens(1, 2:Ka+1));
    pf(i).bN_obs_sens    = xs_obs_sens(2, N_+1);
    pf(i).fcp_obs = fcp_o;
    pf(i).fdi_obs = fdi_o;

    % Pre-allocate frontier arrays
    pf(i).cum_y_base    = zeros(1, n_lambda);
    pf(i).bN_base       = zeros(1, n_lambda);
    pf(i).fcp_base      = zeros(n_lambda, Ka);
    pf(i).fdi_base      = zeros(n_lambda, Ka);
    pf(i).cum_y_sens    = zeros(1, n_lambda);
    pf(i).bN_sens       = zeros(1, n_lambda);
    pf(i).fcp_sens      = zeros(n_lambda, Ka);
    pf(i).fdi_sens      = zeros(n_lambda, Ka);

    tic_country = tic;
    for j = 1:n_lambda
        lam = lambda_grid(j);

        % --- Baseline coefficients ---
        [us_b, ~] = solve_ilqr_unconstrained(fcp_o, fdi_o, fh_i, ...
            S_i, th_i, d_i, my_i, mb_i, fa_i, fb_i, ...
            lam, r_control_reg, u_lo, u_hi, P_base);

        fcp_vec_b = [us_b(1,:), zeros(1, N_-Ka)];
        fdi_vec_b = [us_b(2,:), zeros(1, N_-Ka)];
        fcp_a_vec_b = fa_i * fcp_vec_b;
        fcp_b_vec_b = fb_i * fcp_vec_b;
        xs_b = forward_roll(fcp_vec_b, fcp_a_vec_b, 0.5*fcp_b_vec_b, 0.5*fcp_b_vec_b, ...
            fdi_vec_b, fh_i, S_i, th_i, d_i, my_i, mb_i, P_base);

        pf(i).cum_y_base(j) = sum(xs_b(1, 2:Ka+1));
        pf(i).bN_base(j)    = xs_b(2, N_+1);
        pf(i).fcp_base(j,:) = us_b(1,:);
        pf(i).fdi_base(j,:) = us_b(2,:);

        % --- Sensitivity: alpha_DI = 0 ---
        [us_s, ~] = solve_ilqr_unconstrained(fcp_o, fdi_o, fh_i, ...
            S_i, th_i, d_i, my_i, mb_i, fa_i, fb_i, ...
            lam, r_control_reg, u_lo, u_hi, P_sens);

        fcp_vec_s = [us_s(1,:), zeros(1, N_-Ka)];
        fdi_vec_s = [us_s(2,:), zeros(1, N_-Ka)];
        fcp_a_vec_s = fa_i * fcp_vec_s;
        fcp_b_vec_s = fb_i * fcp_vec_s;
        xs_s = forward_roll(fcp_vec_s, fcp_a_vec_s, 0.5*fcp_b_vec_s, 0.5*fcp_b_vec_s, ...
            fdi_vec_s, fh_i, S_i, th_i, d_i, my_i, mb_i, P_sens);

        pf(i).cum_y_sens(j) = sum(xs_s(1, 2:Ka+1));
        pf(i).bN_sens(j)    = xs_s(2, N_+1);
        pf(i).fcp_sens(j,:) = us_s(1,:);
        pf(i).fdi_sens(j,:) = us_s(2,:);
    end
    ctime = toc(tic_country);

    % Check dominance: is observed point strictly dominated by the frontier?
    % Dominated means: exists a point on the frontier with both better
    % output AND less debt.
    pf(i).dominated_base = check_dominance(pf(i).cum_y_obs, pf(i).bN_obs, ...
        pf(i).cum_y_base, pf(i).bN_base);
    pf(i).dominated_sens = check_dominance(pf(i).cum_y_obs_sens, pf(i).bN_obs_sens, ...
        pf(i).cum_y_sens, pf(i).bN_sens);

    fprintf('  %3d/%d  %s   %5.1fs   dom_base=%d   dom_sens=%d\n', ...
        i, n_c, iso, ctime, pf(i).dominated_base, pf(i).dominated_sens);
end

total_time = toc(tic_total);
fprintf('\n  Total time: %.1f min\n', total_time/60);


%% ========================================================================
%  STEP 3: DOMINANCE SUMMARY
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  DOMINANCE SUMMARY\n');
fprintf('========================================\n\n');

n_dom_base = sum([pf.dominated_base]);
n_dom_sens = sum([pf.dominated_sens]);

fprintf('  Observed policy dominated by frontier:\n');
fprintf('    Under alpha_DI = %.3f (baseline):   %d / %d countries\n', ...
    P_base.alpha_F_DI, n_dom_base, n_c);
fprintf('    Under alpha_DI = 0 (conservative):  %d / %d countries\n\n', ...
    n_dom_sens, n_c);

if n_dom_base > 0 || n_dom_sens > 0
    fprintf('  Countries dominated under BOTH specifications:\n');
    fprintf('  (i.e., observed policy is strictly suboptimal regardless\n');
    fprintf('   of which alpha_DI value is correct)\n\n');
    fprintf('  %5s %10s %10s %10s %10s\n', ...
        'ISO', 'Y_obs', 'B_obs', 'dom_base', 'dom_sens');
    for i = 1:n_c
        if pf(i).dominated_base && pf(i).dominated_sens
            fprintf('  %5s %+10.2f %+10.2f %10d %10d\n', ...
                pf(i).iso, pf(i).cum_y_obs, pf(i).bN_obs, ...
                pf(i).dominated_base, pf(i).dominated_sens);
        end
    end
end


%% ========================================================================
%  STEP 4: FRONTIER RANGE SUMMARY
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  FRONTIER RANGE (per country)\n');
fprintf('========================================\n\n');

fprintf('  Baseline (alpha_DI = %.3f):\n', P_base.alpha_F_DI);
fprintf('  %5s %12s %12s %12s %12s %12s\n', ...
    'ISO', 'Y_min_base', 'Y_max_base', 'B_min_base', 'B_max_base', 'Y_obs');
for i = 1:n_c
    fprintf('  %5s %+12.2f %+12.2f %+12.2f %+12.2f %+12.2f\n', ...
        pf(i).iso, ...
        min(pf(i).cum_y_base), max(pf(i).cum_y_base), ...
        min(pf(i).bN_base),    max(pf(i).bN_base), ...
        pf(i).cum_y_obs);
end

fprintf('\n  Conservative (alpha_DI = 0):\n');
fprintf('  %5s %12s %12s %12s %12s %12s\n', ...
    'ISO', 'Y_min_sens', 'Y_max_sens', 'B_min_sens', 'B_max_sens', 'Y_obs');
for i = 1:n_c
    fprintf('  %5s %+12.2f %+12.2f %+12.2f %+12.2f %+12.2f\n', ...
        pf(i).iso, ...
        min(pf(i).cum_y_sens), max(pf(i).cum_y_sens), ...
        min(pf(i).bN_sens),    max(pf(i).bN_sens), ...
        pf(i).cum_y_obs_sens);
end


%% ========================================================================
%  STEP 5: VISUALIZATION
% =========================================================================

% --- Fig 1: OECD-average frontier (mean across countries) ---
figure('Name','Pareto OECD','Color','w','Position',[50 50 900 600]);
hold on;

% Compute OECD-average frontier
cum_y_oecd_base = zeros(1, n_lambda);
bN_oecd_base    = zeros(1, n_lambda);
cum_y_oecd_sens = zeros(1, n_lambda);
bN_oecd_sens    = zeros(1, n_lambda);
for j = 1:n_lambda
    cum_y_oecd_base(j) = mean(arrayfun(@(p) p.cum_y_base(j), pf));
    bN_oecd_base(j)    = mean(arrayfun(@(p) p.bN_base(j),    pf));
    cum_y_oecd_sens(j) = mean(arrayfun(@(p) p.cum_y_sens(j), pf));
    bN_oecd_sens(j)    = mean(arrayfun(@(p) p.bN_sens(j),    pf));
end

% Mean observed point
y_obs_mean = mean([pf.cum_y_obs]);
b_obs_mean = mean([pf.bN_obs]);
y_obs_mean_sens = mean([pf.cum_y_obs_sens]);
b_obs_mean_sens = mean([pf.bN_obs_sens]);

plot(bN_oecd_base, cum_y_oecd_base, 'b-', 'LineWidth', 2.5);
plot(bN_oecd_sens, cum_y_oecd_sens, 'r--', 'LineWidth', 2.5);
plot(b_obs_mean,      y_obs_mean,      'ks', ...
    'MarkerSize', 14, 'MarkerFaceColor', 'k');
plot(b_obs_mean_sens, y_obs_mean_sens, 'kd', ...
    'MarkerSize', 14, 'MarkerFaceColor', 'none');
xlabel('Terminal debt (pp of 2019 GDP)', 'FontSize', 11);
ylabel('Cumulative output gap (pp, sum 10Q)', 'FontSize', 11);
title('Pareto Frontier: OECD-Average', 'FontSize', 12, 'FontWeight', 'bold');
legend('Baseline: \alpha_{DI} = 0.185', 'Conservative: \alpha_{DI} = 0', ...
    'Observed (baseline)', 'Observed (conservative)', ...
    'Location', 'SE', 'FontSize', 10);
grid on;

% --- Fig 2: All 38 country frontiers (baseline) ---
figure('Name','Pareto All Countries — Baseline','Color','w','Position',[50 50 900 600]);
hold on;
col_map = lines(n_c);
for i = 1:n_c
    plot(pf(i).bN_base, pf(i).cum_y_base, '-', ...
        'Color', [col_map(i,:), 0.4], 'LineWidth', 0.8);
    if pf(i).dominated_base
        plot(pf(i).bN_obs, pf(i).cum_y_obs, 'o', ...
            'MarkerSize', 6, 'MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k');
    else
        plot(pf(i).bN_obs, pf(i).cum_y_obs, 'o', ...
            'MarkerSize', 6, 'MarkerFaceColor', 'g', 'MarkerEdgeColor', 'k');
    end
end
xlabel('Terminal debt (pp of 2019 GDP)', 'FontSize', 11);
ylabel('Cumulative output gap (pp)', 'FontSize', 11);
title('Pareto Frontiers, All 38 Countries (Baseline)', ...
    'FontSize', 12, 'FontWeight', 'bold');
text(0.02, 0.98, ...
    sprintf('Dominated: %d / %d (red)', n_dom_base, n_c), ...
    'Units', 'normalized', 'VerticalAlignment', 'top', ...
    'BackgroundColor', 'w', 'FontSize', 9);
grid on;

% --- Fig 3: All 38 country frontiers (conservative) ---
figure('Name','Pareto All Countries — Conservative','Color','w','Position',[950 50 900 600]);
hold on;
for i = 1:n_c
    plot(pf(i).bN_sens, pf(i).cum_y_sens, '-', ...
        'Color', [col_map(i,:), 0.4], 'LineWidth', 0.8);
    if pf(i).dominated_sens
        plot(pf(i).bN_obs_sens, pf(i).cum_y_obs_sens, 'o', ...
            'MarkerSize', 6, 'MarkerFaceColor', 'r', 'MarkerEdgeColor', 'k');
    else
        plot(pf(i).bN_obs_sens, pf(i).cum_y_obs_sens, 'o', ...
            'MarkerSize', 6, 'MarkerFaceColor', 'g', 'MarkerEdgeColor', 'k');
    end
end
xlabel('Terminal debt (pp of 2019 GDP)', 'FontSize', 11);
ylabel('Cumulative output gap (pp)', 'FontSize', 11);
title('Pareto Frontiers, All 38 Countries (Conservative: \alpha_{DI} = 0)', ...
    'FontSize', 12, 'FontWeight', 'bold');
text(0.02, 0.98, ...
    sprintf('Dominated: %d / %d (red)', n_dom_sens, n_c), ...
    'Units', 'normalized', 'VerticalAlignment', 'top', ...
    'BackgroundColor', 'w', 'FontSize', 9);
grid on;

% --- Fig 4: Selected countries (DEU, USA, ESP, JPN) ---
selected = {'DEU', 'USA', 'ESP', 'JPN', 'GBR', 'ITA'};
figure('Name','Pareto Selected','Color','w','Position',[50 650 1400 700]);
for s = 1:length(selected)
    iso = selected{s};
    idx = find(strcmp({pf.iso}, iso), 1);
    if isempty(idx), continue; end

    subplot(2, 3, s); hold on;
    plot(pf(idx).bN_base, pf(idx).cum_y_base, 'b-', 'LineWidth', 2);
    plot(pf(idx).bN_sens, pf(idx).cum_y_sens, 'r--', 'LineWidth', 2);
    plot(pf(idx).bN_obs, pf(idx).cum_y_obs, 'ks', ...
        'MarkerSize', 12, 'MarkerFaceColor', 'k');
    plot(pf(idx).bN_obs_sens, pf(idx).cum_y_obs_sens, 'kd', ...
        'MarkerSize', 12, 'MarkerFaceColor', 'none');
    xlabel('Terminal debt (pp)', 'FontSize', 9);
    ylabel('Cum output gap (pp)', 'FontSize', 9);
    title(iso, 'FontSize', 11, 'FontWeight', 'bold');
    if s == 1
        legend('Baseline frontier', 'Conservative frontier', ...
            'Obs (base)', 'Obs (cons)', 'Location', 'SE', 'FontSize', 7);
    end
    grid on;
end
sgtitle('Pareto Frontiers — Selected Economies', ...
    'FontSize', 13, 'FontWeight', 'bold');

fprintf('\n\n=== PARETO FRONTIER ANALYSIS COMPLETE ===\n\n');


%% ########################################################################
%  LOCAL FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    for k = 1:N_
        y = xs(1,k); b = xs(2,k); w = xs(3,k); z = xs(4,k);
        fk = 0; fa = 0; fl = 0; fg = 0; gk = 0; hk = 0;
        Sk = 0; thk = 0; dk = 0; ey = 0;
        if k <= length(fcp),       fk  = fcp(k);       end
        if k <= length(fcp_above), fa  = fcp_above(k); end
        if k <= length(fcp_loans), fl  = fcp_loans(k); end
        if k <= length(fcp_guar),  fg  = fcp_guar(k);  end
        if k <= length(fdi),       gk  = fdi(k);       end
        if k <= length(fh),        hk  = fh(k);        end
        if k <= length(S),         Sk  = S(k);         end
        if k <= length(theta),     thk = theta(k);     end
        if k <= length(d),         dk  = d(k);         end
        if k+1 <= length(P.eps_y_vec), ey = P.eps_y_vec(k+1); end

        rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;
        xs(1,k+1) = mu_y + rho_eff*y + P.alpha_S*Sk ...
                  + P.alpha_F_DI*z + P.beta_fear*dk + ey;
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
                  + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
                  + P.kappa_F_DI*gk + P.kappa_H*hk;
        xs(3,k+1) = fk;
        xs(4,k+1) = gk;
    end
end


function dominated = check_dominance(y_obs, b_obs, y_front, b_front)
% Observed point (y_obs, b_obs) is dominated if there exists at least one
% frontier point (y_f, b_f) with:
%   b_f <= b_obs  (not worse debt) AND
%   y_f >= y_obs  (not worse output; "better" means less negative)
% AND at least one strict inequality.
%
% Note: y is cumulative output gap (negative = loss; higher is better)
% Note: b is terminal debt (positive = more debt; lower is better)
    dominated = 0;
    for j = 1:length(y_front)
        if b_front(j) <= b_obs + 1e-6 && y_front(j) >= y_obs - 1e-6
            if b_front(j) < b_obs - 1e-6 || y_front(j) > y_obs + 1e-6
                dominated = 1;
                return;
            end
        end
    end
end


function [us_opt, J_opt] = solve_ilqr_unconstrained(fcp_init, fdi_init, fh_vec, ...
    S, theta, d, mu_y, mu_b, frac_above, frac_below, ...
    lambda, r_reg, u_lo, u_hi, P)
% Solve iLQR without budget constraint, with weighted-sum objective:
%   J = lambda*sum(y_k^2) + (1-lambda)*b_N^2
%       + r_reg * sum(F^CP_k^2 + F^DI_k^2)
%
% Within-CP composition follows (frac_above, frac_below) constant split.
% Below split 50/50 into loans and guarantees.

    N_ = P.N;  Ka = P.K_act;  nx_ = P.nx;  nu_ = P.nu;

    % Objective weights implied by lambda
    wy_eff = lambda;
    wbN_eff = 1 - lambda;
    r_eff = r_reg;

    % Initialize: start from observed controls (clipped)
    us_f = zeros(nu_, N_);
    us_f(:, 1:Ka) = [max(u_lo(1)+0.001, min(u_hi(1)-0.001, fcp_init(1:Ka))); ...
                     max(u_lo(2)+0.001, min(u_hi(2)-0.001, fdi_init(1:Ka)))];

    % Forward roll with composition-split CP
    xs = zeros(nx_, N_+1);
    for k = 1:N_
        fcp_k = us_f(1, k);
        fcp_a_k = frac_above * fcp_k;
        fcp_b_k = frac_below * fcp_k;
        xs(:, k+1) = dynamics_step(xs(:,k), us_f(:,k), k, S, theta, d, ...
            mu_y, mu_b, fcp_a_k, 0.5*fcp_b_k, 0.5*fcp_b_k, fh_vec, P);
    end

    % Q matrices
    Qm = diag([wy_eff, 0, 0, 0]);       % output gap only in stage cost
    Rm = diag([r_eff, r_eff]);
    Qf = diag([0, wbN_eff, 0, 0]);      % terminal: only debt

    reg = 1e-6;
    max_iter = 80;

    for iter = 1:max_iter
        % Backward pass
        Vxx = Qf;
        Vx  = Qf * xs(:, N_+1);
        Ks = zeros(nu_, nx_, N_);
        ds = zeros(nu_, N_);
        bw_ok = true;

        for k = N_:-1:1
            [Ak, Bk] = get_jacobians(xs(:,k), us_f(:,k), k, S, frac_above, P);
            Qxx_k = Qm + Ak'*Vxx*Ak;
            qx_k = Qm*xs(:,k) + Ak'*Vx;

            if k <= Ka
                Quu_k = Rm + Bk'*Vxx*Bk + reg*eye(nu_);
                Qux_k = Bk'*Vxx*Ak;
                qu_k = Rm*us_f(:,k) + Bk'*Vx;

                [~, pd] = chol(Quu_k);
                if pd > 0, bw_ok = false; break; end

                Ki = Quu_k \ Qux_k;
                di = -Quu_k \ qu_k;
                Ks(:,:,k) = Ki;
                ds(:,k) = di;
                Vxx = Qxx_k - Ki'*Quu_k*Ki;
                Vx = qx_k - Ki'*Quu_k*di;
            else
                Vxx = Qxx_k;
                Vx = qx_k;
            end
        end

        if ~bw_ok
            reg = reg * 10;
            if reg > 1e8, break; end
            continue;
        end

        % Line search
        J_old = eval_J(us_f, xs, Ka, N_, wy_eff, wbN_eff, r_eff);

        al = 1.0;
        acc = false;
        while al > 1e-10
            xn = zeros(nx_, N_+1);
            un = zeros(nu_, N_);
            for k = 1:N_
                if k <= Ka
                    dx = xn(:,k) - xs(:,k);
                    un(:,k) = us_f(:,k) + al*ds(:,k) - Ks(:,:,k)*dx;
                    un(:,k) = max(u_lo, min(u_hi, un(:,k)));
                end
                fcp_k = un(1, k);
                fcp_a_k = frac_above * fcp_k;
                fcp_b_k = frac_below * fcp_k;
                xn(:, k+1) = dynamics_step(xn(:,k), un(:,k), k, S, theta, d, ...
                    mu_y, mu_b, fcp_a_k, 0.5*fcp_b_k, 0.5*fcp_b_k, fh_vec, P);
            end
            Jn = eval_J(un, xn, Ka, N_, wy_eff, wbN_eff, r_eff);
            if Jn < J_old - 1e-12
                acc = true;  break;
            end
            al = al * 0.5;
        end

        if ~acc
            reg = reg * 10;
            if reg > 1e8, break; end
            continue;
        end

        dn = norm(un(:, 1:Ka) - us_f(:, 1:Ka), 'fro');
        xs = xn;
        us_f = un;
        reg = max(reg * 0.5, 1e-8);
        if dn < 1e-8, break; end
    end

    us_opt = us_f(:, 1:Ka);
    J_opt = eval_J(us_f, xs, Ka, N_, wy_eff, wbN_eff, r_eff);
end


function J = eval_J(us, xs, Ka, N_, wy, wbN, r)
    J = 0;
    for k = 1:Ka
        y = xs(1, k+1);
        J = J + 0.5 * wy * y^2;
        J = J + 0.5 * r * (us(1,k)^2 + us(2,k)^2);
    end
    J = J + 0.5 * wbN * xs(2, N_+1)^2;
end


function xn = dynamics_step(x, u, k, S, theta, d, mu_y, mu_b, ...
                            fcp_a, fcp_l, fcp_g, fh_vec, P)
    y = x(1);  b = x(2);  w = x(3);  z = x(4);
    fcp = u(1);  fdi = u(2);

    Sk = 0;  thk = 0;  dk = 0;  ey = 0;  hk = 0;
    if k <= length(S),      Sk  = S(k);      end
    if k <= length(theta),  thk = theta(k);  end
    if k <= length(d),      dk  = d(k);      end
    if k <= length(fh_vec), hk  = fh_vec(k); end
    if k+1 <= length(P.eps_y_vec), ey = P.eps_y_vec(k+1); end

    rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;
    xn = [mu_y + rho_eff*y + P.alpha_S*Sk + P.alpha_F_DI*z + P.beta_fear*dk + ey;
          mu_b + (1+P.r_int)*b - P.gamma_y*y + P.kappa_above*fcp_a ...
              + P.kappa_loans*fcp_l + P.kappa_guar*fcp_g ...
              + P.kappa_F_DI*fdi + P.kappa_H*hk;
          fcp;
          fdi];
end


function [A, B] = get_jacobians(x, u, k, S, frac_above, P)
    y = x(1);  w = x(3);
    Sk = 0;  if k <= length(S), Sk = S(k); end
    frac_below = 1 - frac_above;
    kappa_eff_cp = frac_above * P.kappa_above ...
                 + frac_below * 0.5 * P.kappa_loans ...
                 + frac_below * 0.5 * P.kappa_guar;

    A = [P.rho_y + P.psi*Sk + P.eta_p*w,  0,         P.eta_p*y,  P.alpha_F_DI;
         -P.gamma_y,                       1+P.r_int, 0,          0;
         0,                                 0,         0,          0;
         0,                                 0,         0,          0];
    B = [0,             0;
         kappa_eff_cp,  P.kappa_F_DI;
         1,             0;
         0,             1];
end