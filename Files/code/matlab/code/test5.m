%% ========================================================================
%  PANDEMIC TRILEMMA - iLQR SOLVER V17, AVERAGE OECD ECONOMY (MATLAB)
%  ------------------------------------------------------------------------
%  V17 = V16 + TERMINAL VALUE Phi(x_N) on the end-of-horizon state.
%
%  WHY. Finite horizon without terminal valuation prices everything beyond
%  N at zero: debt left at N is punished only once although it must be
%  serviced/consolidated for decades, and the infection stock theta_N
%  keeps producing deaths after N. Both cause end-of-horizon artifacts.
%
%  FIX. J = sum_{q=1..N} beta^q L(x_q,u_q) + Phi(x_N), with Phi the
%  CONTINUATION VALUE of the post-pandemic problem. Two variants
%  (switch via terminal_mode below):
%
%  VARIANT 3 ('riccati', baseline): model-consistent continuation value.
%    Stationary post-pandemic problem in deviation form (b - b0; theta, d,
%    stocks -> 0), post-vax regime (rho_theta_post, decay_K), omicron-era
%    delta_theta, no shocks/trend, same running loss, discount beta.
%    Linearized at the steady state, where the bilinear terms vanish, the
%    problem is exactly LQ; its discounted algebraic Riccati equation
%    yields P_inf and  Phi(x_N) = beta^N (x_N - xbar)' P_inf (x_N - xbar).
%    Controls are unconstrained in the continuation problem (negative
%    fiscal = consolidation): P_inf prices the optimal future
%    consolidation path AND the post-horizon death stream of theta_N.
%
%  VARIANT 2 ('debt_M', robustness): reduced-form uplift on terminal debt,
%    Phi(x_N) = beta^N * M * w_b * (b_N - b0)^2,  M = 1/(1-beta*lam_cons^2)
%    (PV of running debt costs if deviations decay geometrically at
%    lam_cons under a consolidation rule; lam_cons=0.98 -> M ~ 20.5).
%    Nests the hand-derived w_b_tilde = w_b + k_b/2  (M = 1 + k_b/2w_b).
%
%  'none' reproduces the V16 solver exactly.
%  (V16 header below)
%  ------------------------------------------------------------------------
%  V16 changes vs V15:
%   (i)  time-varying rho_theta: explosive (1.035) until Q2.2021, then
%        rho_theta_post < 1 from Q3.2021 (vaccination/immunity). The
%        trilemma is resolved technologically: post-vax, infections are
%        mean-reverting even at S = 0, so no permanent stringency floor.
%   (ii) the below-the-line stock decays at decay_K per quarter:
%        stock' = (1-decay_K)*stock + current flows. Reduces the
%        perpetuity value of front-loaded guarantees.
%  Both knobs sit in the parameter block below. decay_K = 0 and
%  rho_theta_post = 1.035 reproduce the V15 solver exactly.
%  ------------------------------------------------------------------------
%  Solves the finite-horizon planner problem on the calibrated V15 dynamics
%  with iterative LQR (iLQR; Li & Todorov 2004, Tassa et al. 2014).
%
%  ALGORITHM (the 6 steps, marked below and in the local functions):
%   STEP 0  State augmentation. iLQR needs Markovian dynamics x' = f_q(x,u).
%           Lagged controls (F_above lag 2, F_DI lag 1) and the cumulated
%           below-the-line stock become extra states:
%             x = [ y; b; theta; d; fab_l1; fab_l2; fdi_l1; stock ]  (n = 8)
%             u = [ S; F_above; F_loans; F_guar; F_DI ]              (m = 5)
%   STEP 1  Nominal rollout with an initial control guess.
%   STEP 2  Linearize/quadratize along the trajectory: analytic Jacobians
%           A_q = df/dx, B_q = df/du (trajectory-dependent because of the
%           bilinear terms S*theta and S*F_DI_lag1 - the reason plain LQR
%           does not apply), plus cost derivatives l_x, l_u, l_xx, l_uu.
%   STEP 3  Backward (Riccati-like) pass:
%             Q_x = l_x + A'V_x,   Q_u  = l_u + B'V_x,
%             Q_xx= l_xx+ A'V_xx A, Q_uu= l_uu+ B'V_xx B + mu*I,
%             Q_ux= B'V_xx A,
%           feedforward k = -Q_uu \ Q_u, feedback K = -Q_uu \ Q_ux.
%           (Gauss-Newton iLQR: V_x * f_xx tensor terms dropped.)
%           mu = Levenberg-Marquardt regularizer, adapted automatically.
%   STEP 4  Forward pass with backtracking line search,
%             u = clamp( u_nom + alpha*k + K (x - x_nom), lb, ub ),
%           box constraints enforced by clamping (simple variant of
%           control-limited DDP, Tassa et al. 2014).
%   STEP 5  Accept if cost decreased (shrink mu), else raise mu and retry.
%           Iterate STEPs 2-5 until relative improvement < tol.
%
%  PLANNER OBJECTIVE:
%    J = sum_{q=1..N} beta^q [ w_y y_q^2 + lam_d w_d d_q^2
%        + w_b (b_q - b0)^2 + p_S S_q^2 + p_F sum F_q^2 ]
%  Weights = inverse pooled variance of observed OECD paths; lam_d scales
%  the health weight; p_S, p_F are small control-effort penalties.
%
%  Requires: country_data_for_matlab.csv, weekly_mortality_matlab.csv
% =========================================================================
clear; clc; close all;
fprintf('=== iLQR PLANNER V17 (V16 + terminal value): AVERAGE OECD ===\n  %s\n\n', datestr(now));

%% ------------------------------------------------------------------------
%  Calibrated V15 parameters
% -------------------------------------------------------------------------
P.rho_y=0.231;  
P.alpha_S=-0.095; 
P.alpha_above=0.544; 
P.alpha_below=0.261;
P.alpha_DI=1.470; 
P.alpha_SDI=-0.041;
P.beta_d=0.0;
P.c_lo=0.40; 
P.c_gu=0.25;
P.r=0.001; 
P.gamma_y=0.117;
P.k_ab=0.664; 
P.k_lo=0.836; 
P.k_gu=0.536; 
P.k_di=0.536; 
P.phi_t=0;

% V16: time-varying persistence + stock decay
rho_theta_pre  = 1.035;   rho_theta_post = 0.95;   q_vax = 8;  % Q3.2021
P.rho_th_q = [repmat(rho_theta_pre,1,q_vax-1), ...
              repmat(rho_theta_post,1,13-q_vax+1)];
P.decay_K  = 0.1;        % below-the-line stock depreciation per quarter-> robustness ∈ {0, 0.05, 0.10, 0.15}
P.phi_S=0.5;   % V17b: calibrated from NPI literature (Brauner et al. 2021 Science;
               % Flaxman et al. 2020 Nature): full containment (S=1) -> ~50% reduction
               % in transmission. (1 - phi_S*S) at S=1 gives 0.5. Was 0.314 (own
               % circular theta-estimate); replaced since phi_S only parameterizes
               % the exogenous theta path and carries no normative weight.
ifr=[0.009 0.007 0.006 0.004 0.003 0.0004];
P.delta_q=ifr([1 1 1 2 3 3 4 5 5 5 6 6 6])*1e6;     % wave-indexed deltas
P.N=13; P.yr=4:16;
P.n=8; P.m=4;                                       % state / control dims
% V17b: S removed from control vector. The planner optimizes ONLY fiscal
% composition GIVEN the containment trajectory (RQ: "given the containment
% trajectory and epidemiological state"). S enters as an EXOGENOUS quarterly
% input C.S_exo(q). Control vector is now u = [F_above; F_loans; F_guar; F_DI].
% state indices: 1 y | 2 b | 3 theta | 4 d | 5 fab_l1 | 6 fab_l2 | 7 fdi_l1 | 8 stock
% control indices: 1 F_above | 2 F_loans | 3 F_guar | 4 F_DI

qord={'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021', ...
      'Q3.2021','Q4.2021','Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl={'Q4.19','Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21','Q3.21', ...
      'Q4.21','Q1.22','Q2.22','Q3.22','Q4.22'};

cfe_y_val=[+1.1057 -1.0400 +0.3009 -0.0979 +1.2987 +1.3246 +1.9894 +0.1466 ...
           -3.5623 -1.7381 -0.0832 -4.8958 -1.8327 -1.4833 -1.8693 -3.3268 ...
           +0.2908 -2.0146 +8.3187 -4.8488 +2.3672 -0.5014 -1.9561 +0.6141 ...
           +0.7002 +2.3568 -0.6830 -3.2057 +1.0604 +1.2218 -1.0616 -0.5966 ...
           -2.7751 -0.2578 -1.8284 +1.0349 +4.0658 +1.0987];

cfe_b_val=[-0.8525, -0.6001, -0.8731, -0.2648, -0.4315, -1.0291, +0.5492, +0.1213, ...
             -0.6570, -1.0947, -1.6607, -0.9624, -0.4810, -0.6315, -0.9667, -1.3722, ...
             -1.3041, -0.9670, -0.2702, -0.8165, -0.3418, -1.1930, -1.6567, -0.5136, ...
             -0.4824, +0.1193, +0.0475, -0.9082, -0.4272, -0.1091, +0.0389, -0.6929, ...
             -1.7242, -0.1307, -0.9910, -0.5114, -0.3150, -0.2251];

b0_val=[37.6 69.7 77.9 44.2 16.3 29.6 47.7 53.2 30.2 34.0 34.1 86.0 11.8 54.5 ...
        85.4 107.0 205.0 52.1 54.3 69.9 56.9 122.0 199.0 35.1 29.3 26.6 39.7 ...
        7.25 43.9 16.6 73.0 45.6 111.0 48.4 72.1 27.7 54.0 97.3];

eps_v14=[-3.62 -8.55 -6.46 -7.79 -4.73 -10.10 -11.20 -3.75 -5.10 -5.98 -1.95 ...
         -9.57 -2.43 -1.97 -8.49 -12.50 -9.94 -8.99 -1.10 -4.70 -0.66 -10.90 ...
         -6.82 -1.83 -0.03 -5.31 -7.43 -7.97 -5.17 -2.45 -5.49 -1.80 -9.75 ...
         -4.87 -6.78 -4.70 -10.40 -4.79];

%% ------------------------------------------------------------------------
%  DATA: observed OECD paths -> representative agent, weights, bounds
% -------------------------------------------------------------------------
T = readtable('country_data_for_matlab.csv');
M = readtable('weekly_mortality_matlab.csv');
M.date = datetime(M.date);
M.qstr = strings(height(M),1);
for r0 = 1:height(M)
    M.qstr(r0) = sprintf('Q%d.%d', quarter(M.date(r0)), year(M.date(r0)));
end
M.d_pmw = M.deaths_w ./ M.pop * 1e6;
[gid,gC,gQ] = findgroups(M.Country, M.qstr);
th_q = splitapply(@nanmean, M.theta_hat, gid);
d_q  = splitapply(@nanmean, M.d_pmw, gid);
th_map = containers.Map(); d_map = containers.Map();
for r0 = 1:length(gC)
    key = sprintf('%s_%s', gC{r0}, gQ{r0});
    th_map(key)=th_q(r0); d_map(key)=d_q(r0);
end

countries = unique(T.Country,'stable'); n_c = numel(countries); N = P.N;
[S_o,Fa,Fl,Fg,Fd,y_o,bd,th_o,d_o] = deal(zeros(n_c,N));
for i = 1:n_c
    iso = countries{i};
    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}),:);
        if isempty(row), continue; end
        S_o(i,k)=row.S_mean_tw;  Fa(i,k)=row.F_CP_above_3;
        Fl(i,k)=row.F_CP_loans;  Fg(i,k)=row.F_CP_guar_adj; Fd(i,k)=row.F_DI;
        y_o(i,k)=row.y_t_pct;
        if ~ismissing(row.debt_dR), bd(i,k)=row.debt_dR; end
        key = sprintf('%s_%s', iso, qord{k});
        if isKey(th_map,key) && ~isnan(th_map(key)), th_o(i,k)=th_map(key); end
        if isKey(d_map,key)  && ~isnan(d_map(key)),  d_o(i,k)=d_map(key);  end
    end
end
b_lvl = b0_val' + cumsum(bd,2);

% exogenous epidemic impulses: theta innovations under observed policy
eps_th = zeros(n_c,N+1);
for i = 1:n_c
    for k = 2:N
        expct = P.rho_th_q(k)*(1-P.phi_S*S_o(i,k)/100)*th_o(i,k-1);
        eps_th(i,k+1) = th_o(i,k) - expct;
    end
end
C.eps_th = mean(eps_th,1);                          % OECD-mean impulse path
C.b0 = mean(b0_val); C.mu_y = mean(cfe_y_val); C.mu_b = mean(cfe_b_val);
C.eps_y = zeros(1,N+1); C.eps_y(4) = mean(eps_v14); % Q2.20 shock
C.S_exo = mean(S_o,1);                              % V17b: exogenous containment
                                                    % path (observed OECD avg), q=1..N

% planner weights (inverse pooled variances) and control bounds (obs. p95)
W.beta=0.99; W.lam_d=1.0;
W.w_y = 1/var(y_o(:),1);  W.w_d = 1/var(d_o(:),1);
dev = b_lvl - b0_val';    W.w_b = 1/var(dev(:),1);
W.p_S = 0.05/var(S_o(S_o>0),1);                     % kept for reference (S exo)
allF = [Fa(Fa>0); Fl(Fl>0); Fg(Fg>0); Fd(Fd>0)];
W.p_F = 0.05/var(allF,1);
% ---- V17: terminal value configuration ---------------------------------
W.terminal_mode = 'riccati';   % 'riccati' (Var.3) | 'debt_M' (Var.2) | 'none'
W.lam_cons = 0.98;             % Var.2: consolidation persistence
W.M_term   = 1/(1 - W.beta*W.lam_cons^2);

% V17b: control bounds now 4-dim (S dropped; it is exogenous)
lb = zeros(P.m,1);
ub = [pctile(Fa(Fa>0),95); pctile(Fl(Fl>0),95); ...
      pctile(Fg(Fg>0),95); pctile(Fd(Fd>0),95)];

fprintf('  b0 = %.1f%% GDP | mu_y %+.3f | mu_b %+.3f | eps_y(Q2.20) %+.2f\n', ...
        C.b0, C.mu_y, C.mu_b, C.eps_y(4));
fprintf('  bounds: F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f\n\n', ub);

%% ------------------------------------------------------------------------
%  RUN iLQR (multistart: observed average policy, zeros)
% -------------------------------------------------------------------------
x0 = zeros(P.n,1); x0(2) = C.b0;
W.xbar = x0;                                   % steady state: only b0 nonzero
switch W.terminal_mode
    case 'riccati'
        [A_s,B_s,Q_s,R_s] = stationary_matrices(P, W);
        W.P_inf = solve_dare(A_s, B_s, Q_s, R_s, W.beta);
        fprintf('  [terminal] Riccati: P_inf(b,b)=%.3f (uplift M=%.1f x w_b)\n', ...
                W.P_inf(2,2), W.P_inf(2,2)/W.w_b);
    case 'debt_M'
        W.P_inf = zeros(P.n); W.P_inf(2,2) = W.M_term*W.w_b;
        fprintf('  [terminal] reduced-form debt uplift M = %.1f\n', W.M_term);
    otherwise
        W.P_inf = zeros(P.n);
end
U_obs = [mean(Fa,1); mean(Fl,1); mean(Fg,1); mean(Fd,1)]; % m x N (S exogenous)

fprintf('Start 1: observed average policy\n');
[X1,U1,J1] = ilqr(U_obs, x0, P, C, W, lb, ub);
fprintf('Start 2: zero controls\n');
[X2,U2,J2] = ilqr(zeros(P.m,P.N), x0, P, C, W, lb, ub);
if J1 <= J2, X=X1; U=U1; J=J1; else, X=X2; U=U2; J=J2; end
fprintf('\nBest of multistart: J* = %.4f\n', J);

Xo = rollout(U_obs, x0, P, C);
fprintf('Observed average policy (same closure): J = %.4f\n\n', ...
        total_cost(Xo, U_obs, P, C, W));

fprintf('Optimal policy and trajectory:\n');
fprintf('  %6s %6s %5s %5s %5s %5s | %7s %7s %8s %6s\n', ...
        'Q','S(exo)','F_ab','F_lo','F_gu','F_DI','y','b','theta','d');
for k = 1:P.N
    fprintf('  %6s %6.1f %5.2f %5.2f %5.2f %5.2f | %+7.2f %7.2f %8.5f %6.1f\n', ...
        qlbl{k}, C.S_exo(k), U(1,k),U(2,k),U(3,k),U(4,k), ...
        X(1,k+1),X(2,k+1),X(3,k+1),X(4,k+1));
end
fprintf('\n  Cum. output gap: %+.2f ppQ | cum. deaths: %.0f/million | terminal debt %.2f%% (db %+.2f)\n', ...
    sum(X(1,2:end)), sum(X(4,2:end))*13, X(2,end), X(2,end)-C.b0);

figure('Color','w','Position',[60 60 1200 600]);
ttl={'Stringency S','Output gap y (pp)','Debt b (% GDP)','Deaths d (/10^6/wk)'};
subplot(2,2,1); stairs(1:N,C.S_exo,'k-','LineWidth',2);
title('Stringency S (exogenous, given)'); legend('observed avg (given)');
subplot(2,2,2); plot(1:N,X(1,2:end),'b-o','LineWidth',2); hold on;
plot(1:N,Xo(1,2:end),'k--s'); yline(0,':');
subplot(2,2,3); plot(1:N,X(2,2:end),'b-o','LineWidth',2); hold on;
plot(1:N,Xo(2,2:end),'k--s'); yline(C.b0,':');
subplot(2,2,4); plot(1:N,X(4,2:end),'b-o','LineWidth',2); hold on;
plot(1:N,Xo(4,2:end),'k--s');
for sp = 1:4
    subplot(2,2,sp); grid on; title(ttl{sp});
    set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45);
end
sgtitle('iLQR planner V17 vs observed average OECD policy','FontWeight','bold');

%% ========================================================================
%  LOCAL FUNCTIONS
% =========================================================================

function xp = f_step(x, u, q, P, C)
% STEP 0: one-step Markovian transition; q = quarter index (1..N).
% V17b: S is EXOGENOUS (C.S_exo); control u = [F_above;F_loans;F_guar;F_DI].
    S=C.S_exo(q); fab=u(1); flo=u(2); fgu=u(3); fdi=u(4);
    y=x(1); b=x(2); th=x(3); d=x(4); a1=x(5); a2=x(6); di1=x(7); st=x(8);
    st_used = (1-P.decay_K)*st + P.c_lo*flo + P.c_gu*fgu;  % V16: decaying stock
    eth = 0; if q+1 <= P.N, eth = C.eps_th(q+2); end  % shock convention of V15
    xp = zeros(P.n,1);
    xp(3) = P.rho_th_q(q)*(1-P.phi_S*S/100)*th + eth;              % theta
    xp(4) = P.delta_q(q)*th;                                       % deaths
    xp(1) = C.mu_y + P.rho_y*y + P.alpha_S*S + P.alpha_above*a2 ...
          + P.alpha_below*st_used + P.alpha_DI*di1 ...
          + P.alpha_SDI*S*di1 - P.beta_d*d + C.eps_y(q+1);         % output
    xp(2) = C.mu_b + (1+P.r)*b - P.gamma_y*y + P.k_ab*fab ...
          + P.k_lo*P.c_lo*flo + P.k_gu*P.c_gu*fgu + P.k_di*di1 ...
          + P.phi_t*P.yr(q);                                       % debt
    xp(5) = fab; xp(6) = a1; xp(7) = fdi; xp(8) = st_used;         % lags
end

function X = rollout(U, x0, P, C)
% STEP 1: nominal forward simulation. X is n x (N+1), U is m x N.
    X = zeros(P.n, P.N+1); X(:,1) = x0;
    for q = 1:P.N, X(:,q+1) = f_step(X(:,q), U(:,q), q, P, C); end
end

function J = total_cost(X, U, P, C, W)
% Discounted quadratic trilemma loss + V17 terminal value Phi(x_N).
% V17b: S is exogenous; its effort penalty is a constant. We include it for
% comparability of J across runs (same closure) but it does not affect the
% optimum (constant in U).
    J = 0;
    for q = 1:P.N
        x = X(:,q+1); u = U(:,q);
        sc = W.w_y*x(1)^2 + W.lam_d*W.w_d*x(4)^2 + W.w_b*(x(2)-C.b0)^2;
        cc = W.p_S*C.S_exo(q)^2 + W.p_F*sum(u.^2);   % S exogenous constant
        J = J + W.beta^q * (sc + cc);
    end
    dx = X(:,end) - W.xbar;
    J = J + W.beta^P.N * (dx' * W.P_inf * dx);     % Phi(x_N)
end

function [A,B] = jacobians(x, u, q, P, C)
% STEP 2: analytic Jacobians A = df/dx, B = df/du at (x,u,q).
% V17b: S is exogenous (C.S_exo(q)), so there is NO B column for S.
% Bilinear terms S*theta, S*di1 now treat S as a constant.
% B columns: 1 F_above | 2 F_loans | 3 F_guar | 4 F_DI.
    S=C.S_exo(q); th=x(3); di1=x(7);
    A = zeros(P.n); B = zeros(P.n,P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI + P.alpha_SDI*S; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,2)=P.alpha_below*P.c_lo; B(1,3)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,1)=P.k_ab; B(2,2)=P.k_lo*P.c_lo; B(2,3)=P.k_gu*P.c_gu;
    A(3,3)=P.rho_th_q(q)*(1-P.phi_S*S/100);
    A(4,3)=P.delta_q(q);
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,1)=1; B(7,4)=1; B(8,2)=P.c_lo; B(8,3)=P.c_gu;
end

function [lx,lu,lxx,luu] = cost_derivs(xp, u, q, P, C, W)
% STEP 2: cost gradients/Hessians; xp = arrival state x_{q}, u = control u_q.
% V17b: control is fiscal only (u = F's). S is exogenous -> no S penalty here
% (a constant in the objective; zero gradient/Hessian).
    dq = W.beta^q;
    lx = zeros(P.n,1); lxx = zeros(P.n);
    lx(1)=2*dq*W.w_y*xp(1); lx(4)=2*dq*W.lam_d*W.w_d*xp(4);
    lx(2)=2*dq*W.w_b*(xp(2)-C.b0);
    lxx(1,1)=2*dq*W.w_y; lxx(4,4)=2*dq*W.lam_d*W.w_d; lxx(2,2)=2*dq*W.w_b;
    lu  = 2*dq*W.p_F*u;                 % all four controls are fiscal
    luu = 2*dq*W.p_F*eye(P.m);
end

function [X,U,J] = ilqr(U0, x0, P, C, W, lb, ub)
% STEPs 3-5: iLQR main loop.
    U = U0; X = rollout(U, x0, P, C); J = total_cost(X, U, P, C, W);
    mu = 1e-6; max_iter = 400; tol = 1e-9;
    for it = 1:max_iter
        % ---- STEP 3: backward pass -----------------------------------
        % V17: seed the recursion with the terminal value
        %   Phi(x_N) = beta^N (x_N-xbar)' P_inf (x_N-xbar)
        %   -> V_x = 2 beta^N P_inf (x_N-xbar), V_xx = 2 beta^N P_inf.
        Vx  = 2*W.beta^P.N * W.P_inf * (X(:,end) - W.xbar);
        Vxx = 2*W.beta^P.N * W.P_inf;
        ks = zeros(P.m,P.N); Ks = zeros(P.m,P.n,P.N); fail = false;
        for q = P.N:-1:1
            [A,B] = jacobians(X(:,q), U(:,q), q, P, C);
            [lxp,lu,lxxp,luu] = cost_derivs(X(:,q+1), U(:,q), q, P, C, W);
            Vx_  = lxp + Vx;                 % arrival-state cost folded in
            Vxx_ = lxxp + Vxx;
            Qx  = A'*Vx_;            Qu  = lu + B'*Vx_;
            Qxx = A'*Vxx_*A;         Quu = luu + B'*Vxx_*B + mu*eye(P.m);
            Qux = B'*Vxx_*A;
            [~,pflag] = chol(Quu);
            if pflag > 0, fail = true; break; end
            kq = -Quu\Qu;  Kq = -Quu\Qux;
            ks(:,q) = kq;  Ks(:,:,q) = Kq;
            Vx  = Qx + Kq'*Quu*kq + Kq'*Qu + Qux'*kq;
            Vxx = Qxx + Kq'*Quu*Kq + Kq'*Qux + Qux'*Kq;
            Vxx = (Vxx+Vxx')/2;
        end
        if fail, mu = max(mu*10,1e-6); continue; end
        % ---- STEP 4: forward pass with line search --------------------
        improved = false;
        for alpha = 2.^(0:-1:-9)
            Xn = zeros(size(X)); Xn(:,1) = x0; Un = zeros(size(U));
            for q = 1:P.N
                du = alpha*ks(:,q) + Ks(:,:,q)*(Xn(:,q)-X(:,q));
                Un(:,q) = min(max(U(:,q)+du, lb), ub);   % clamp to box
                Xn(:,q+1) = f_step(Xn(:,q), Un(:,q), q, P, C);
            end
            Jn = total_cost(Xn, Un, P, C, W);
            if Jn < J - 1e-12, improved = true; break; end
        end
        % ---- STEP 5: accept/reject, adapt regularizer, check converged -
        if improved
            rel = (J-Jn)/max(J,1e-12);
            X = Xn; U = Un; J = Jn; mu = max(mu/2,1e-8);
            if mod(it,25)==0
                fprintf('  iter %3d: J = %10.4f (rel %.1e, mu %.1e)\n',it,J,rel,mu);
            end
            if rel < tol
                fprintf('  CONVERGED at iter %d: J = %.4f\n', it, J); break;
            end
        else
            mu = mu*10;
            if mu > 1e8
                fprintf('  stopped (regularizer maxed) at iter %d: J = %.4f\n',it,J);
                break;
            end
        end
    end
end

function [A,B,Q,R] = stationary_matrices(P, W)
% V17 STEP T: post-pandemic stationary LQ problem (deviation form).
% Linearized at the steady state (theta=0, u=0): bilinear terms vanish.
% V17b: S is exogenous and post-pandemic S=0, so the continuation control
% is fiscal only (m=4). B has no S column; R is 4-dim.
% B columns: 1 F_above | 2 F_loans | 3 F_guar | 4 F_DI.
    A = zeros(P.n); B = zeros(P.n, P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,2)=P.alpha_below*P.c_lo; B(1,3)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,1)=P.k_ab; B(2,2)=P.k_lo*P.c_lo; B(2,3)=P.k_gu*P.c_gu;
    A(3,3)=P.rho_th_q(end);                 % theta self-stable post-vax
    A(4,3)=P.delta_q(end);                  % omicron IFR mapping
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,1)=1; B(7,4)=1; B(8,2)=P.c_lo; B(8,3)=P.c_gu;
    Q = zeros(P.n); Q(1,1)=W.w_y; Q(4,4)=W.lam_d*W.w_d; Q(2,2)=W.w_b;
    R = W.p_F*eye(P.m);
end

function Pinf = solve_dare(A, B, Q, R, beta_)
% Discounted algebraic Riccati equation by fixed-point iteration
% (discount absorbed by scaling A, B with sqrt(beta)). V(x) = x'Pinf x.
    Ad = sqrt(beta_)*A; Bd = sqrt(beta_)*B;
    Pinf = Q;
    for it = 1:20000
        K  = (R + Bd'*Pinf*Bd) \ (Bd'*Pinf*Ad);
        Pn = Q + Ad'*Pinf*(Ad - Bd*K);
        Pn = (Pn+Pn')/2;
        if max(abs(Pn(:)-Pinf(:))) < 1e-12, Pinf = Pn; return; end
        Pinf = Pn;
    end
end

function v = pctile(x, p)
% Toolbox-free percentile (linear interpolation between order statistics).
    x = sort(x(:)); n = numel(x);
    idx = 1 + (n-1)*p/100;
    lo = floor(idx); hi = ceil(idx);
    v = x(lo) + (idx-lo)*(x(hi)-x(lo));
end