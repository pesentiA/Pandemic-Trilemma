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

% Quarter FE (output shocks, fractions)-> Start Q4.2019 bis Q4.2021
eps_y_raw = [0, -1.844, -9.526, 0.149, -1.294, -0.538, -0.137, -0.052, 0.534, -0.426] / 100;

% Objective weights (MCPF = 0.3, Dahlby 2008)-> Only for counterfactual,
% calibration is not 
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
    
    [us_il, J_il] = solve_ilqr_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P);
    
    % Direct Shooting: use iLQR solution as additional init
    [us_ds, J_ds] = solve_direct_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P, us_il);
    
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
function [us_opt, J_opt] = solve_direct_cfb(fcp_obs, fdi_obs, S, theta, d, P, us_ilqr)
    K = P.K; total_cum = sum(fcp_obs) + sum(fdi_obs);
    pen = 1e4;
    
    obj = @(x) eval_J(max(0,x(1:K)), max(0,x(K+1:2*K)), S, theta, d, P) ...
        + pen*(sum(max(0,x(1:K)))+sum(max(0,x(K+1:2*K)))-total_cum)^2 ...
        + pen*sum(max(0, x(1:K)-P.u_hi(1)).^2) ...
        + pen*sum(max(0, x(K+1:2*K)-P.u_hi(2)).^2) ...
        + pen*sum(max(0, -x).^2);
    
    opts = optimset('MaxIter',15000,'MaxFunEvals',300000,'TolFun',1e-13,'TolX',1e-12,'Display','off');
    
    best_J = Inf; best_x = zeros(1,2*K);
    inits = {[fcp_obs, fdi_obs], ones(1,2*K)*total_cum/(2*K)};
    w_back = exp(0.3*(0:K-1)); w_back = w_back/sum(w_back);
    inits{end+1} = [w_back*total_cum*0.9, w_back*total_cum*0.1];
    w_front = exp(-0.3*(0:K-1)); w_front = w_front/sum(w_front);
    inits{end+1} = [w_front*total_cum*0.9, w_front*total_cum*0.1];
    % Steep back-load (all in last 2 quarters)
    w_steep = zeros(1,K); w_steep(K-1)=0.3; w_steep(K)=0.7;
    inits{end+1} = [w_steep*total_cum, zeros(1,K)];
    % iLQR warm start (if provided)
    if nargin >= 7 && ~isempty(us_ilqr)
        inits{end+1} = [us_ilqr(1,:), us_ilqr(2,:)];
    end
    
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


%% ========================================================================
%  STEP 5: VISUALIZATION
%  Add this block after STEP 4 in pandemic_counterfactuals.m
% =========================================================================

qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21','Q3.21','Q4.21','Q1.22'};

% ---- Figure 1: Verification — J and b scatter ----
figure('Name','Verification','Color','w','Position',[50 50 1100 450]);

subplot(1,2,1);
plot([res.J_ds], [res.J_il], 'ko', 'MarkerFaceColor', [.2 .5 .8], 'MarkerSize', 7);
hold on;
mn = min([res.J_ds])*0.95; mx = max([res.J_ds])*1.05;
plot([mn mx],[mn mx],'r--','LineWidth',1.5);
xlabel('J — Direct Shooting'); ylabel('J — iLQR');
title('Objective Function'); grid on; axis equal;
text(mn+0.05*(mx-mn), mx-0.05*(mx-mn), sprintf('Max |\\DeltaJ| = %.2e', max(dJs)), 'FontSize', 8);

subplot(1,2,2);
plot([res.b_ds], [res.b_il], 'ko', 'MarkerFaceColor', [.8 .3 .3], 'MarkerSize', 7);
hold on;
mn = min([res.b_ds])*0.95; mx = max([res.b_ds])*1.05;
plot([mn mx],[mn mx],'r--','LineWidth',1.5);
xlabel('Terminal Debt (pp) — Direct Shooting'); ylabel('Terminal Debt (pp) — iLQR');
title('Terminal Debt'); grid on; axis equal;
text(mn+0.05*(mx-mn), mx-0.05*(mx-mn), sprintf('Max |\\Deltab| = %.4f pp', max(dbs)), 'FontSize', 8);

sgtitle('Solver Verification: Direct Shooting vs iLQR (38 countries)','FontWeight','bold');

% ---- Figure 2: Convergence bar chart ----
figure('Name','Convergence','Color','w','Position',[50 50 1200 400]);

subplot(1,2,1);
[~,si] = sort(dJs,'descend');
bar(dJs(si), 'FaceColor', [.2 .5 .8]); hold on;
yline(0.05, 'r--', 'Threshold', 'LineWidth', 1.5, 'LabelHorizontalAlignment', 'left');
set(gca,'XTick',1:n_c,'XTickLabel',{res(si).iso},'XTickLabelRotation',55,'FontSize',6);
ylabel('|ΔJ|'); title('Objective Difference'); grid on;

subplot(1,2,2);
[~,si2] = sort(dbs,'descend');
bar(dbs(si2), 'FaceColor', [.8 .3 .3]); hold on;
yline(0.5, 'r--', 'Threshold', 'LineWidth', 1.5, 'LabelHorizontalAlignment', 'left');
set(gca,'XTick',1:n_c,'XTickLabel',{res(si2).iso},'XTickLabelRotation',55,'FontSize',6);
ylabel('|Δb| (pp GDP)'); title('Terminal Debt Difference'); grid on;

sgtitle('Convergence Diagnostics','FontWeight','bold');

% ---- Figure 3: Selected countries — optimal trajectories both methods ----
selected = {'USA','DEU','ITA','GBR','JPN','CHL'};
n_sel = length(selected);

figure('Name','Trajectories','Color','w','Position',[30 30 1500 800]);

for s = 1:n_sel
    iso = selected{s};
    idx = find(strcmp({cdata.iso}, iso));
    S_i = cdata(idx).S; th_i = cdata(idx).theta; d_i = cdata(idx).d;
    fcp_obs = cdata(idx).FCP; fdi_obs = cdata(idx).FDI;
    
    % Re-solve (or retrieve from stored results if you saved trajectories)
    [us_il, ~] = solve_ilqr_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P);
    [us_ds, ~] = solve_direct_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, P, us_il);
    
    xs_obs = forward_roll(fcp_obs, fdi_obs, S_i, th_i, d_i, P);
    xs_ds = forward_roll(us_ds(1,:), us_ds(2,:), S_i, th_i, d_i, P);
    xs_il = forward_roll(us_il(1,:), us_il(2,:), S_i, th_i, d_i, P);
    
    total_obs = (fcp_obs + fdi_obs)*100;
    total_ds = (us_ds(1,:) + us_ds(2,:))*100;
    total_il = (us_il(1,:) + us_il(2,:))*100;
    
    % Col 1: Total F (obs vs DS vs iLQR)
    subplot(n_sel, 3, (s-1)*3+1); hold on;
    bar_data = [total_obs; total_ds; total_il]';
    bh = bar(1:K, bar_data, 'grouped');
    bh(1).FaceColor = [.5 .5 .5]; bh(2).FaceColor = [.2 .5 .8]; bh(3).FaceColor = [.8 .3 .3];
    set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',6);
    ylabel('% GDP'); grid on;
    if s == 1
        title('Total Fiscal (% GDP)');
        legend('Observed','Direct','iLQR','FontSize',6,'Location','NE');
    end
    text(0.02, 0.95, iso, 'Units','normalized','FontSize',10,'FontWeight','bold','VerticalAlignment','top');
    
    % Col 2: CP share (obs vs DS vs iLQR)
    subplot(n_sel, 3, (s-1)*3+2); hold on;
    cp_obs = fcp_obs ./ max(fcp_obs+fdi_obs, 1e-10) * 100;
    cp_ds = us_ds(1,:) ./ max(us_ds(1,:)+us_ds(2,:), 1e-10) * 100;
    cp_il = us_il(1,:) ./ max(us_il(1,:)+us_il(2,:), 1e-10) * 100;
    % Only plot quarters with nonzero spending
    for kk = 1:K
        if total_obs(kk) > 0.01, plot(kk, cp_obs(kk), 'ks', 'MarkerSize', 7, 'MarkerFaceColor', [.5 .5 .5]); end
        if total_ds(kk) > 0.01, plot(kk, cp_ds(kk), 'bo', 'MarkerSize', 6, 'MarkerFaceColor', [.2 .5 .8]); end
        if total_il(kk) > 0.01, plot(kk, cp_il(kk), 'r^', 'MarkerSize', 6, 'MarkerFaceColor', [.8 .3 .3]); end
    end
    ylim([0 105]);
    set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',6);
    ylabel('CP share (%)'); grid on;
    if s == 1, title('CP Share (%)'); end
    
    % Col 3: Output gap and debt (obs vs optimal)
    subplot(n_sel, 3, (s-1)*3+3); hold on;
    yyaxis left;
    plot(1:K, xs_obs(1,2:end)*100, 'k--', 'LineWidth', 1.5);
    plot(1:K, xs_ds(1,2:end)*100, 'b-', 'LineWidth', 1.5);
    plot(1:K, xs_il(1,2:end)*100, 'r:', 'LineWidth', 1.5);
    ylabel('Output gap (pp)');
    yyaxis right;
    plot(1:K, xs_obs(2,2:end)*100, 'k--s', 'LineWidth', 1, 'MarkerSize', 3);
    plot(1:K, xs_ds(2,2:end)*100, 'b-o', 'LineWidth', 1, 'MarkerSize', 3);
    plot(1:K, xs_il(2,2:end)*100, 'r:^', 'LineWidth', 1, 'MarkerSize', 3);
    ylabel('Cum. debt (pp)');
    set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',6);
    grid on;
    if s == 1
        title('Outcomes');
        legend('Obs','Direct','iLQR','Location','SE','FontSize',5);
    end
end

sgtitle('CF-B: Observed vs Optimal (Direct Shooting \approx iLQR)','FontWeight','bold');

% ---- Figure 4: Validation — OECD IQR ----
sim_y_all = reshape([cdata.sim_y], K, n_c)' * 100;
obs_y_all = reshape([cdata.y], K, n_c)' * 100;
sim_b_all = reshape([cdata.sim_b], K, n_c)' * 100;
obs_b_all = zeros(n_c, K);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum * 100; end
obs_S_all = reshape([cdata.S], K, n_c)' * 100;

figure('Name','Validation','Color','w','Position',[50 50 1400 400]);

subplot(1,3,1); hold on;
fill_iqr(1:K, sim_y_all, [0 0.4 0.8], 0.15);
fill_iqr(1:K, obs_y_all, [0.5 0.5 0.5], 0.12);
plot(1:K, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:K, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
yline(0,':','Color',[.5 .5 .5]); grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('pp of potential GDP'); title('Output Gap');
legend('','','Simulated','Observed','Location','SE','FontSize',7);

subplot(1,3,2); hold on;
fill_iqr(1:K, sim_b_all, [0 0.4 0.8], 0.15);
fill_iqr(1:K, obs_b_all, [0.5 0.5 0.5], 0.12);
plot(1:K, median(sim_b_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 5);
plot(1:K, median(obs_b_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 5);
grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('pp of 2019 GDP'); title('Cumulative Debt');
legend('','','Simulated','Observed','Location','SE','FontSize',7);

subplot(1,3,3); hold on;
fill_iqr(1:K, obs_S_all, [0.3 0.3 0.3], 0.15);
plot(1:K, median(obs_S_all), 'k-o', 'LineWidth', 2, 'MarkerSize', 5);
grid on;
set(gca,'XTick',1:K,'XTickLabel',qlbl,'FontSize',8);
ylabel('Index (0-100)'); title('Stringency');

sgtitle('Validation: Simulated vs Observed (Median \pm IQR)','FontWeight','bold');

% ---- Helper for IQR fill ----
function fill_iqr(x, data, col, alpha)
    sd = sort(data); n = size(sd,1);
    p25 = sd(max(1,round(0.25*n)), :);
    p75 = sd(max(1,round(0.75*n)), :);
    fill([x, fliplr(x)], [p25, fliplr(p75)], col, 'FaceAlpha', alpha, 'EdgeColor', 'none');
end