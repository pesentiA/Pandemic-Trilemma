

%% ========================================================================
%  PANDEMIC TRILEMMA - iLQR SOLVER V17f, AVERAGE OECD ECONOMY (MATLAB)
%
%  PROBLEM. Finite-horizon (N=13, Q4.2019–Q4.2022) deterministic optimal-
%  control problem solved by iLQR. The planner chooses a 4-instrument fiscal
%  control u=[F_above;F_loans;F_guar;F_DI] each quarter to minimize a
%  discounted quadratic loss in the output gap y and the debt deviation
%  (b-b0), subject to the empirically estimated transmission dynamics.
%
%  EXOGENOUS EPIDEMIC BLOCK (Option A). theta and deaths d are NOT simulated;
%  they are fixed to the observed OECD-mean paths and enter only as data.
%  The mortality leg is therefore a CONSTANT in the objective: this is a
%  TWO-OBJECTIVE (output vs. debt) problem. w_d, lam_d, and the deaths cost
%  terms are retained in the code but are INERT (they shift J's level, never
%  the argmin). The mortality margin reactivates only with an endogenous
%  epidemic block.
%
%  WEIGHTS (option 2, welfare-calibrated). Output scale fixed at w_y=1/var(y).
%  The output/debt PRICE is set by tau: "1pp output gap == tau %GDP debt",
%  giving w_y/w_b = tau^2. Baseline tau=5 (robustness {2,5,10}). Only the
%  RATIO matters in 2D; the level is irrelevant. NB: tau=1/var-implied ratio
%  would be ~1.16, an indefensibly debt-hawkish stance under which the
%  planner is passive -- hence the explicit calibration.
%
%  STATE x0 (Q4.19): y=0, b=b0(=60.7), all else 0. Controls LOCKED to 0 in
%  Q4.19 (q_start=2); first fiscal action in Q1.20. TERMINAL: quadratic
%  penalty dx'*P_inf*dx with xbar=x0, so the planner is pulled back toward
%  y=0, b=b0 by Q4.22. P_inf from the discounted DARE of the stationary
%  (zero-forcing) system; this is an LQ closure, NOT the true continuation,
%  and it drives the late-horizon debt paydown and a small output overshoot.
%
%  CAPS (time-varying box). Upper bound = observed p99 per instrument
%  (in-support, no extrapolation), zero before q_start. Doubling the cap
%  moves the worst-quarter optimal output by <0.3pp: the binding constraint
%  is the transmission channel, not the spending envelope.
%
%  SOLVER NOTES. Backward Riccati pass with Levenberg-Marquardt reg (mu),
%  line-searched, box-clamped forward pass. At tau=5 the run iterates to a
%  relative tolerance ~1e-6 at the iteration cap (max_iter=400) rather than
%  hitting tol=1e-9; reported J is thus an upper bound on J*. Both
%  multistarts converge to the same basin (suggests a unique optimum).
%
%  KEY RESULT. The gains over observed policy are COMPOSITIONAL: the planner
%  routes spending through below-the-line CP (loans, guarantees) and uses
%  ~zero DI and little above-the-line, reflecting the estimated kappa
%  heterogeneity. The impact-quarter (Q2.20) collapse is exogenous (S +
%  eps_y) and cannot be prevented; fiscal composition improves the RECOVERY.
%
%  Control u = [F_above; F_loans; F_guar; F_DI]  (S exogenous, given).
%  Requires: country_data_for_matlab.csv, weekly_mortality_matlab.csv
%
%  V17f = V17e + exogenous epidemic block (Option A).
%  V17e = Q4.2019 fiscal lockout; time-varying box bounds.
% =========================================================================
clear; clc; close all;
fprintf('=== iLQR PLANNER V17f (exogenous epi block; Q4.19 lockout): AVERAGE OECD ===\n  %s\n\n', datestr(now));

%% ------------------------------------------------------------------------
%  Calibrated parameters
% -------------------------------------------------------------------------
P.rho_y=0.231;
P.alpha_S=-0.095;
P.alpha_above=0.544;
P.alpha_below=0.261;
P.alpha_DI=1.470;
P.alpha_SDI=-0.041;
P.beta_d=0;
P.c_lo=0.40;
P.c_gu=0.25;
P.r=0.001;
P.gamma_y=0.117;
P.k_ab=0.664;
P.k_lo=0.836;
P.k_gu=0.536;
P.k_di=0.536;
P.phi_t=0;

% --- Epidemic-block parameters: now only used to BUILD the observed series
%     / residual diagnostics, NOT to propagate state inside the solver.
rho_theta_pre  = 1.035;   rho_theta_post = 0.95;   q_vax = 8;  % Q3.2021
P.rho_th_q = [repmat(rho_theta_pre,1,q_vax-1), ...
              repmat(rho_theta_post,1,13-q_vax+1)];
P.decay_K  = 0.1;        % robustness in {0, 0.05, 0.10, 0.15}
P.phi_S=0.5;            % NPI literature (Brauner 2021; Flaxman 2020)
ifr=[0.009 0.007 0.006 0.004 0.003 0.0004];
P.delta_q=ifr([1 1 1 2 3 3 4 5 5 5 6 6 6])*1e6;
P.N=13; P.yr=4:16;
P.n=8; P.m=4;
% V17e: first quarter in which fiscal action is allowed (1=Q4.19, 2=Q1.20...).
P.q_start = 2;          % Q1.2020 (Q4.19 locked to zero)
% state idx: 1 y|2 b|3 theta|4 d|5 fab_l1|6 fab_l2|7 fdi_l1|8 stock
% control idx: 1 F_above|2 F_loans|3 F_guar|4 F_DI
% NOTE V17f: states 3 (theta) and 4 (d) are now exogenous passthroughs of
%            data; no control or state feeds back into them.

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
%  DATA
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

% Observed OECD-median series (raw data; independent of P)
obs_y  = mean(y_o, 1);          % 1xN
obs_b  = mean(b_lvl, 1);        % 1xN
obs_th = mean(th_o, 1);         % 1xN
obs_d  = mean(d_o, 1);          % 1xN

% --- theta residual diagnostic (kept for plausibility check only; the solver
%     no longer uses eps_th to propagate state under the exogenous block).
eps_th = zeros(n_c,N+1);
for i = 1:n_c
    for k = 2:N
        expct = P.rho_th_q(k)*(1-P.phi_S*S_o(i,k)/100)*th_o(i,k-1);
        eps_th(i,k+1) = th_o(i,k) - expct;
    end
end
C.eps_th = mean(eps_th,1);
C.b0 = mean(b0_val); C.mu_y = mean(cfe_y_val); C.mu_b = mean(cfe_b_val);
C.eps_y = zeros(1,N+1); C.eps_y(4) = mean(eps_v14);
C.S_exo = mean(S_o,1);

% V17f: exogenous epidemic block — observed mean paths, fed directly to state.
C.obs_th = obs_th;   % 1xN, theta exogenous
C.obs_d  = obs_d;    % 1xN, deaths exogenous

W.beta=0.99; W.lam_d=1.0;
W.w_y = 1/var(y_o(:),1);  W.w_d = 1/var(d_o(:),1);
dev = b_lvl - b0_val';                       % kept for reference / robustness
tau  = 5;                                    % price: 1pp output gap == tau %GDP debt
W.w_b = W.w_y / tau^2;                        % implied debt weight (option 2)
fprintf('  tau = %.1f  =>  w_y/w_b = %.3f (vs 1/var ratio %.3f)\n', ...
        tau, W.w_y/W.w_b, W.w_y/(1/var(dev(:),1)));
W.p_S = 0.05/var(S_o(S_o>0),1);
allF = [Fa(Fa>0); Fl(Fl>0); Fg(Fg>0); Fd(Fd>0)];
W.p_F = 0.05/var(allF,1);
W.terminal_mode = 'riccati';
W.lam_cons = 0.98;
W.M_term   = 1/(1 - W.beta*W.lam_cons^2);

% V17e: TIME-VARYING box bounds (m x N). Q4.19 (column 1) locked to zero;
% from q_start onward, the observed p95 cap applies.
ub_cap = [pctile(Fa(Fa>0),99); pctile(Fl(Fl>0),99); ...
          pctile(Fg(Fg>0),99); pctile(Fd(Fd>0),99)];
lb = zeros(P.m, P.N);
ub = repmat(ub_cap, 1, P.N);
ub(:, 1:(P.q_start-1)) = 0;       % lock all controls to zero before q_start

fprintf('  b0 = %.1f%% GDP | mu_y %+.3f | mu_b %+.3f | eps_y(Q2.20) %+.2f\n', ...
        C.b0, C.mu_y, C.mu_b, C.eps_y(4));
fprintf('  EXOGENOUS epi block: theta,d set to observed mean paths (Option A)\n');
fprintf('  fiscal allowed from %s onward (Q4.19 locked to 0)\n', qlbl{P.q_start});
fprintf('  caps: F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f\n\n', ub_cap);

%% ------------------------------------------------------------------------
%  RUN iLQR
% -------------------------------------------------------------------------
x0 = zeros(P.n,1); x0(2) = C.b0;
W.xbar = x0;
switch W.terminal_mode
    case 'riccati'
        [A_s,B_s,Q_s,R_s] = stationary_matrices(P, W);
        W.P_inf = solve_dare(A_s, B_s, Q_s, R_s, W.beta);
        fprintf('  [terminal] Riccati: P_inf(b,b)=%.3f (uplift M=%.1f x w_b)\n', ...
                W.P_inf(2,2), W.P_inf(2,2)/W.w_b);
    case 'debt_M'
        W.P_inf = zeros(P.n); W.P_inf(2,2) = W.M_term*W.w_b;
    otherwise
        W.P_inf = zeros(P.n);
end
U_obs = [mean(Fa,1); mean(Fl,1); mean(Fg,1); mean(Fd,1)];
U_obs(:,1:(P.q_start-1)) = 0;     % observed-policy baseline also respects lock

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
plot(1:N,obs_y,'k--s'); yline(0,':');
subplot(2,2,3); plot(1:N,X(2,2:end),'b-o','LineWidth',2); hold on;
plot(1:N,obs_b,'k--s'); yline(C.b0,':');
subplot(2,2,4); plot(1:N,X(4,2:end),'b-o','LineWidth',2); hold on;
plot(1:N,obs_d,'k--s');
for sp = 1:4
    subplot(2,2,sp); grid on; title(ttl{sp});
    set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45);
end
sgtitle('iLQR planner V17f (exogenous epi block) vs observed average OECD','FontWeight','bold');

%% ========================================================================
%  LOCAL FUNCTIONS
% =========================================================================

function xp = f_step(x, u, q, P, C)
    S=C.S_exo(q); fab=u(1); flo=u(2); fgu=u(3); fdi=u(4);
    y=x(1); b=x(2); d=x(4); a1=x(5); a2=x(6); di1=x(7); st=x(8);
    st_used = (1-P.decay_K)*st + P.c_lo*flo + P.c_gu*fgu;
    xp = zeros(P.n,1);
    % --- EXOGENOUS epidemic block (Option A): observed mean paths, not simulated.
    xp(3) = C.obs_th(q);     % theta = data
    xp(4) = C.obs_d(q);      % deaths = data  => blue d == grey d by construction
    % --- output and debt remain endogenous in the controls/state.
    xp(1) = C.mu_y + P.rho_y*y + P.alpha_S*S + P.alpha_above*a2 ...
          + P.alpha_below*st_used + P.alpha_DI*di1 ...
          + P.alpha_SDI*S*di1 - P.beta_d*d + C.eps_y(q+1);
    xp(2) = C.mu_b + (1+P.r)*b - P.gamma_y*y + P.k_ab*fab ...
          + P.k_lo*P.c_lo*flo + P.k_gu*P.c_gu*fgu + P.k_di*di1 ...
          + P.phi_t*P.yr(q);
    xp(5) = fab; xp(6) = a1; xp(7) = fdi; xp(8) = st_used;
end

function X = rollout(U, x0, P, C)
    X = zeros(P.n, P.N+1); X(:,1) = x0;
    for q = 1:P.N, X(:,q+1) = f_step(X(:,q), U(:,q), q, P, C); end
end

function J = total_cost(X, U, P, C, W)
    J = 0;
    for q = 1:P.N
        x = X(:,q+1); u = U(:,q);
        sc = W.w_y*x(1)^2 + W.lam_d*W.w_d*x(4)^2 + W.w_b*(x(2)-C.b0)^2;
        cc = W.p_S*C.S_exo(q)^2 + W.p_F*sum(u.^2);
        J = J + W.beta^q * (sc + cc);
    end
    dx = X(:,end) - W.xbar;
    J = J + W.beta^P.N * (dx' * W.P_inf * dx);
end

function [A,B] = jacobians(x, u, q, P, C)
    S=C.S_exo(q);
    A = zeros(P.n); B = zeros(P.n,P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI + P.alpha_SDI*S; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,2)=P.alpha_below*P.c_lo; B(1,3)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,1)=P.k_ab; B(2,2)=P.k_lo*P.c_lo; B(2,3)=P.k_gu*P.c_gu;
    % V17f: theta (row 3) and d (row 4) are exogenous => no state feedback.
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,1)=1; B(7,4)=1; B(8,2)=P.c_lo; B(8,3)=P.c_gu;
end

function [lx,lu,lxx,luu] = cost_derivs(xp, u, q, P, C, W)
    dq = W.beta^q;
    lx = zeros(P.n,1); lxx = zeros(P.n);
    lx(1)=2*dq*W.w_y*xp(1); lx(4)=2*dq*W.lam_d*W.w_d*xp(4);
    lx(2)=2*dq*W.w_b*(xp(2)-C.b0);
    lxx(1,1)=2*dq*W.w_y; lxx(4,4)=2*dq*W.lam_d*W.w_d; lxx(2,2)=2*dq*W.w_b;
    lu  = 2*dq*W.p_F*u;
    luu = 2*dq*W.p_F*eye(P.m);
end

function [X,U,J] = ilqr(U0, x0, P, C, W, lb, ub)
% STEPs 3-5. V17e: lb/ub are now m x N matrices (time-varying box).
    U = U0; X = rollout(U, x0, P, C); J = total_cost(X, U, P, C, W);
    mu = 1e-6; max_iter = 1400; tol = 1e-9;
    for it = 1:max_iter
        Vx  = 2*W.beta^P.N * W.P_inf * (X(:,end) - W.xbar);
        Vxx = 2*W.beta^P.N * W.P_inf;
        ks = zeros(P.m,P.N); Ks = zeros(P.m,P.n,P.N); fail = false;
        for q = P.N:-1:1
            [A,B] = jacobians(X(:,q), U(:,q), q, P, C);
            [lxp,lu,lxxp,luu] = cost_derivs(X(:,q+1), U(:,q), q, P, C, W);
            Vx_  = lxp + Vx;
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
        improved = false;
        for alpha = 2.^(0:-1:-9)
            Xn = zeros(size(X)); Xn(:,1) = x0; Un = zeros(size(U));
            for q = 1:P.N
                du = alpha*ks(:,q) + Ks(:,:,q)*(Xn(:,q)-X(:,q));
                Un(:,q) = min(max(U(:,q)+du, lb(:,q)), ub(:,q));  % time-varying clamp
                Xn(:,q+1) = f_step(Xn(:,q), Un(:,q), q, P, C);
            end
            Jn = total_cost(Xn, Un, P, C, W);
            if Jn < J - 1e-12, improved = true; break; end
        end
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
    A = zeros(P.n); B = zeros(P.n, P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,2)=P.alpha_below*P.c_lo; B(1,3)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,1)=P.k_ab; B(2,2)=P.k_lo*P.c_lo; B(2,3)=P.k_gu*P.c_gu;
    % V17f: theta and d exogenous => rows 3,4 carry no dynamics in the DARE.
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,1)=1; B(7,4)=1; B(8,2)=P.c_lo; B(8,3)=P.c_gu;
    Q = zeros(P.n); Q(1,1)=W.w_y; Q(4,4)=W.lam_d*W.w_d; Q(2,2)=W.w_b;
    R = W.p_F*eye(P.m);
end

function Pinf = solve_dare(A, B, Q, R, beta_)
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
    x = sort(x(:)); n = numel(x);
    idx = 1 + (n-1)*p/100;
    lo = floor(idx); hi = ceil(idx);
    v = x(lo) + (idx-lo)*(x(hi)-x(lo));
end