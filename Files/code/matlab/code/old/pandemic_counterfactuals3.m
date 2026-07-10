%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & SOLVER VERIFICATION
%
%  This script validates the transition system and verifies that two
%  independent solution methods — Direct Shooting and iLQR — converge
%  to the same optimum for the constrained counterfactual (CF-B).
%
%  STEP 1: Validation — Forward roll with observed controls
%  STEP 2: CF-B solved by Direct Shooting (fminsearch + penalty)
%  STEP 3: CF-B solved by iLQR (Riccati backward pass + Augmented Lagrangian)
%  STEP 4: Verification — Compare the two solutions
%
%  State:   x = (y, b, z)'    where z = F^DI_{k-1} (lagged DI)
%  Control: u = (F^CP, F^DI)'
%  Exogenous (fixed): S_obs, theta_obs, d_obs
%
%  Parameters from Table 3 (Output) and Table 4 (Debt).
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration & Verification ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  PARAMETERS
% =========================================================================

% Output equation (Table 3, Col 3)
rho_y      = 0.227;
psi        = 0.372;
alpha_S    = 0.028;
alpha_F_CP = 0.253;
eta_tilde  = -0.596;
eta_p      = 0.026;
alpha_F_DI = 0.244;
beta_fear  = -0.018;

% Debt equation (Table 4, Col 1)
r_int      = 0.001;
gamma_y    = 0.219;
kappa_F_CP = 0.167;
kappa_F_DI = 0.379;
c_H        = 0.02;

% Quarter FE (output shocks, fractions)
eps_y_raw = [0, -1.844, -9.526, 0.149, -1.294, -0.538, -0.137, -0.052, 0.534, -0.426] / 100;

% Objective weights (MCPF = 0.3, Dahlby 2008)
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% Dimensions
K = 9;  nx = 3;  nu = 2;

% Build eps_y vector
eps_y_vec = zeros(1, K+1);
for k = 1:K
    if k+1 <= length(eps_y_raw), eps_y_vec(k+1) = eps_y_raw(k+1); end
end

% Bounds
u_lo = [0; 0];
u_hi = [0.20; 0.10];

% Pack
P = struct('rho_y',rho_y,'psi',psi,'alpha_S',alpha_S,...
    'alpha_F_CP',alpha_F_CP,'eta_tilde',eta_tilde,'eta_p',eta_p,...
    'alpha_F_DI',alpha_F_DI,'beta_fear',beta_fear,...
    'r_int',r_int,'gamma_y',gamma_y,'kappa_F_CP',kappa_F_CP,...
    'kappa_F_DI',kappa_F_DI,'c_H',c_H,...
    'eps_y_vec',eps_y_vec,'beta_disc',beta_disc,...
    'w_y',w_y,'w_b',w_b,'W_b',W_b,'r_cp',r_cp,'r_di',r_di,...
    'K',K,'nx',nx,'nu',nu,'u_lo',u_lo,'u_hi',u_hi);

fprintf('  gamma_y=%.3f  kappa_CP=%.3f  kappa_DI=%.3f  kDI/kCP=%.2f\n\n',...
    gamma_y, kappa_F_CP, kappa_F_DI, kappa_F_DI/kappa_F_CP);

%% ========================================================================
%  LOAD DATA
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');
qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021',...
        'Q3.2021','Q4.2021','Q1.2022'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S = zeros(1,K); cdata(i).FCP = zeros(1,K);
    cdata(i).FDI = zeros(1,K); cdata(i).y = zeros(1,K);
    cdata(i).theta = zeros(1,K); cdata(i).b = zeros(1,K);
    cdata(i).d = zeros(1,K);
    for k = 1:K
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k) = row.S_mean_tw / 100;
        cdata(i).FCP(k) = row.F_CP / 100;
        cdata(i).FDI(k) = row.F_DI / 100;
        cdata(i).y(k) = row.y_t_pct / 100;
        cdata(i).theta(k) = row.theta_pct / 100;
        if ~ismissing(row.debt_dR), cdata(i).b(k) = row.debt_dR/100; end
        if ismember('excess_mortality',T.Properties.VariableNames) && ~ismissing(row.excess_mortality)
            cdata(i).d(k) = row.excess_mortality/100;
        end
    end
end
fprintf('  %d countries x %d quarters\n\n', n_c, K);

%% ========================================================================
%  STEP 1: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation (forward roll)\n');
fprintf('========================================\n');

for i = 1:n_c
    xs = forward_roll(cdata(i).FCP, cdata(i).FDI, ...
        cdata(i).S, cdata(i).theta, cdata(i).d, P);
    cdata(i).sim_y = xs(1, 2:end);
    cdata(i).sim_b = xs(2, 2:end);
    cdata(i).obs_b_cum = cumsum(cdata(i).b);
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y - cdata(i).y).^2)) * 100;
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b - cdata(i).obs_b_cum).^2)) * 100;
end

rmse_y_all = [cdata.rmse_y];
rmse_b_all = [cdata.rmse_b];
fprintf('  Output RMSE — Median: %.2f pp  Mean: %.2f pp\n', median(rmse_y_all), mean(rmse_y_all));
fprintf('  Debt RMSE   — Median: %.2f pp  Mean: %.2f pp\n\n', median(rmse_b_all), mean(rmse_b_all));

%% ========================================================================
%  STEP 2-3: CF-B — DIRECT SHOOTING vs iLQR
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 2-3: CF-B — Direct Shooting vs iLQR\n');
fprintf('========================================\n\n');

res = struct();
for i = 1:n_c
    iso = cdata(i).iso;
    S_i = cdata(i).S; th_i = cdata(i).theta; d_i = cdata(i).d;
    fcp_obs = cdata(i).FCP; fdi_obs = cdata(i).FDI;
    
    [us_ds, J_ds] = solve_direct_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P);
    [us_il, J_il] = solve_ilqr_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P);
    
    xs_ds = forward_roll(us_ds(1,:), us_ds(2,:), S_i, th_i, d_i, P);
    xs_il = forward_roll(us_il(1,:), us_il(2,:), S_i, th_i, d_i, P);
    
    res(i).iso = iso;
    res(i).J_ds = J_ds; res(i).J_il = J_il;
    res(i).dJ = abs(J_ds - J_il);
    res(i).b_ds = xs_ds(2,K+1)*100;
    res(i).b_il = xs_il(2,K+1)*100;
    res(i).db = abs(xs_ds(2,K+1) - xs_il(2,K+1))*100;
    res(i).du_max = max(abs(us_ds(:) - us_il(:)))*100;
    
    if res(i).dJ < 0.05 && res(i).db < 0.5, flag='OK'; else, flag='WARN'; end
    fprintf('  [%2d/%d] %s  |dJ|=%.2e  |db|=%.4fpp  |du|=%.3f%%  %s\n', ...
        i, n_c, iso, res(i).dJ, res(i).db, res(i).du_max, flag);
end

%% ========================================================================
%  STEP 4: SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 4: Verification Summary\n');
fprintf('========================================\n');

dJs = [res.dJ]; dbs = [res.db]; du_maxs = [res.du_max];
n_conv = sum(dJs < 0.05 & dbs < 0.5);

fprintf('\n  |dJ| — Mean: %.2e  Max: %.2e\n', mean(dJs), max(dJs));
fprintf('  |db| — Mean: %.4f pp  Max: %.4f pp\n', mean(dbs), max(dbs));
fprintf('  |du| — Mean: %.3f%%  Max: %.3f%%\n', mean(du_maxs), max(du_maxs));
fprintf('  Converged: %d / %d\n', n_conv, n_c);

Tout = table({res.iso}', [res.J_ds]', [res.J_il]', [res.dJ]', ...
    [res.b_ds]', [res.b_il]', [res.db]', [res.du_max]', ...
    'VariableNames', {'Country','J_direct','J_ilqr','abs_dJ',...
                      'b_direct','b_ilqr','abs_db_pp','abs_du_max_pct'});
writetable(Tout, 'verification_results.csv');
fprintf('\n  Saved: verification_results.csv\n');
fprintf('\n=== COMPLETE ===\n');


%% ########################################################################
%  HELPER FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fdi, S, theta, d, P)
    K = P.K; xs = zeros(P.nx, K+1);
    for k = 1:K
        y = xs(1,k); b = xs(2,k); z = xs(3,k);
        xs(1,k+1) = P.rho_y*y + P.psi*S(k)*y - P.alpha_S*S(k) + (P.alpha_F_CP + P.eta_tilde*S(k) - P.eta_p*y)*fcp(k) + P.alpha_F_DI*z + P.beta_fear*d(k) + P.eps_y_vec(k+1);
        xs(2,k+1) = (1+P.r_int)*b - P.gamma_y*y + P.kappa_F_CP*fcp(k) + P.kappa_F_DI*fdi(k) + P.c_H*theta(k);
        xs(3,k+1) = fdi(k);
    end
end

function J = eval_J(fcp, fdi, S, theta, d, P)
    xs = forward_roll(fcp, fdi, S, theta, d, P);
    K = P.K; J = 0;
    for k = 1:K
        bd = P.beta_disc^(k-1);
        J = J + bd*0.5*(P.w_y*xs(1,k+1)^2 + P.w_b*xs(2,k+1)^2);
        J = J + bd*0.5*(P.r_cp*fcp(k)^2 + P.r_di*fdi(k)^2);
    end
    J = J + P.beta_disc^K * 0.5 * P.W_b * xs(2,K+1)^2;
end

function [A, B] = get_jacobians(x, u, k, S, P)
    y = x(1); fcp = u(1);
    A = [P.rho_y+P.psi*S(k)-P.eta_p*fcp, 0, P.alpha_F_DI; -P.gamma_y, 1+P.r_int, 0; 0, 0, 0];
    B = [P.alpha_F_CP+P.eta_tilde*S(k)-P.eta_p*y, 0; P.kappa_F_CP, P.kappa_F_DI; 0, 1];
end

function x_next = dynamics_step(x, u, k, S, theta, d, P)
    y=x(1); b=x(2); z=x(3); fcp=u(1); fdi=u(2);
    x_next = [P.rho_y*y + P.psi*S(k)*y - P.alpha_S*S(k) + (P.alpha_F_CP + P.eta_tilde*S(k) - P.eta_p*y)*fcp + P.alpha_F_DI*z + P.beta_fear*d(k) + P.eps_y_vec(k+1); (1+P.r_int)*b - P.gamma_y*y + P.kappa_F_CP*fcp + P.kappa_F_DI*fdi + P.c_H*theta(k); fdi];
end

%% ========================================================================
%  DIRECT SHOOTING (fminsearch + penalty)
% =========================================================================
function [us_opt, J_opt] = solve_direct_cfb(fcp_obs, fdi_obs, S, theta, d, P)
    K = P.K; total_cum = sum(fcp_obs) + sum(fdi_obs);
    pen = 1e4;
    
    obj = @(x) eval_J(max(0,x(1:K)), max(0,x(K+1:2*K)), S, theta, d, P) ...
        + pen*(sum(max(0,x(1:K)))+sum(max(0,x(K+1:2*K)))-total_cum)^2 ...
        + pen*sum(max(0, x(1:K)-P.u_hi(1)).^2) ...
        + pen*sum(max(0, x(K+1:2*K)-P.u_hi(2)).^2) ...
        + pen*sum(max(0, -x).^2);
    
    opts = optimset('MaxIter',5000,'MaxFunEvals',80000,'TolFun',1e-13,'TolX',1e-11,'Display','off');
    
    best_J = Inf; best_x = zeros(1,2*K);
    inits = {[fcp_obs, fdi_obs], ones(1,2*K)*total_cum/(2*K)};
    w_back = exp(0.3*(0:K-1)); w_back = w_back/sum(w_back);
    inits{end+1} = [w_back*total_cum*0.9, w_back*total_cum*0.1];
    w_front = exp(-0.3*(0:K-1)); w_front = w_front/sum(w_front);
    inits{end+1} = [w_front*total_cum*0.9, w_front*total_cum*0.1];
    
    for t = 1:length(inits)
        x0 = max(0.0001, inits{t});
        sc = total_cum / sum(x0); x0 = x0*sc;
        [xo, Jo] = fminsearch(obj, x0, opts);
        if Jo < best_J, best_J=Jo; best_x=xo; end
    end
    
    xp = max(0, best_x);
    xp(1:K) = min(P.u_hi(1), xp(1:K));
    xp(K+1:2*K) = min(P.u_hi(2), xp(K+1:2*K));
    sc = total_cum / max(sum(xp), 1e-12); xp = xp*sc;
    
    us_opt = [xp(1:K); xp(K+1:2*K)];
    J_opt = eval_J(us_opt(1,:), us_opt(2,:), S, theta, d, P);
end

%% ========================================================================
%  iLQR (Riccati + Augmented Lagrangian for sum constraint)
% =========================================================================
function [us_opt, J_opt] = solve_ilqr_cfb(fcp_obs, fdi_obs, S, theta, d, P)
    K = P.K; nx = P.nx; nu = P.nu;
    total_cum = sum(fcp_obs) + sum(fdi_obs);
    gc = [1; 1];
    
    Qmat = diag([P.w_y, P.w_b, 0]);
    Rmat = diag([P.r_cp, P.r_di]);
    Qf_term = diag([0, P.W_b, 0]);
    
    us = [max(0.001,min(0.19,fcp_obs)); max(0.001,min(0.09,fdi_obs))];
    xs = forward_roll(us(1,:), us(2,:), S, theta, d, P);
    
    lam_sum = 0; mu_sum = 50;
    max_outer = 30; max_inner = 100; reg = 1e-6;
    
    for outer = 1:max_outer
        for inner = 1:max_inner
            sum_viol = sum(us(:)) - total_cum;
            
            % --- Backward pass ---
            Vxx = P.beta_disc^(K-1)*Qmat + P.beta_disc^K*Qf_term;
            Vx = Vxx * xs(:,K+1);
            Ks_all = zeros(nu, nx, K);
            ds_all = zeros(nu, K);
            bw_ok = true;
            
            for k = K:-1:1
                bk = P.beta_disc^(k-1);
                [Ak, Bk] = get_jacobians(xs(:,k), us(:,k), k, S, P);
                
                Qxx_k = Ak'*Vxx*Ak;
                Quu_k = bk*Rmat + Bk'*Vxx*Bk + mu_sum*(gc*gc') + reg*eye(nu);
                Qux_k = Bk'*Vxx*Ak;
                qx_k = Ak'*Vx;
                qu_k = bk*Rmat*us(:,k) + Bk'*Vx + (lam_sum + mu_sum*sum_viol)*gc;
                
                [~,pd] = chol(Quu_k);
                if pd > 0, bw_ok = false; break; end
                
                Ki = Quu_k \ Qux_k;
                di = -Quu_k \ qu_k;
                Ks_all(:,:,k) = Ki;
                ds_all(:,k) = di;
                
                Vxx_new = Qxx_k - Ki'*Quu_k*Ki;
                Vx_new = qx_k - Ki'*Quu_k*di;
                
                if k > 1
                    Vxx = P.beta_disc^(k-2)*Qmat + Vxx_new;
                    Vx = P.beta_disc^(k-2)*Qmat*xs(:,k) + Vx_new;
                else
                    Vxx = Vxx_new; Vx = Vx_new;
                end
            end
            
            if ~bw_ok
                reg = reg*10;
                if reg > 1e8, break; end
                continue;
            end
            
            % --- Forward pass (line search) ---
            sv_old = sum(us(:)) - total_cum;
            J_old = eval_J(us(1,:),us(2,:),S,theta,d,P) + lam_sum*sv_old + 0.5*mu_sum*sv_old^2;
            
            alpha_ls = 1.0; accepted = false;
            while alpha_ls > 1e-10
                xs_n = zeros(nx, K+1); us_n = zeros(nu, K);
                for k = 1:K
                    dx = xs_n(:,k) - xs(:,k);
                    us_n(:,k) = us(:,k) + alpha_ls*ds_all(:,k) - Ks_all(:,:,k)*dx;
                    us_n(:,k) = max(P.u_lo, min(P.u_hi, us_n(:,k)));
                    xs_n(:,k+1) = dynamics_step(xs_n(:,k), us_n(:,k), k, S, theta, d, P);
                end
                sv_n = sum(us_n(:)) - total_cum;
                J_new = eval_J(us_n(1,:),us_n(2,:),S,theta,d,P) + lam_sum*sv_n + 0.5*mu_sum*sv_n^2;
                if J_new < J_old - 1e-12, accepted = true; break; end
                alpha_ls = alpha_ls * 0.5;
            end
            
            if ~accepted
                reg = reg*10;
                if reg > 1e8, break; end
                continue;
            end
            
            du_norm = norm(us_n(:) - us(:));
            xs = xs_n; us = us_n;
            reg = max(reg*0.5, 1e-8);
            if du_norm < 1e-9, break; end
        end
        
        % --- Outer: update multiplier ---
        sum_viol = sum(us(:)) - total_cum;
        lam_sum = lam_sum + mu_sum*sum_viol;
        mu_sum = mu_sum * 2;
        if abs(sum_viol) < 1e-8, break; end
    end
    
    % Enforce constraint exactly
    sc = total_cum / max(sum(us(:)), 1e-12);
    us = us*sc;
    us = max(P.u_lo, min(P.u_hi, us));
    
    us_opt = us;
    J_opt = eval_J(us_opt(1,:), us_opt(2,:), S, theta, d, P);
end
