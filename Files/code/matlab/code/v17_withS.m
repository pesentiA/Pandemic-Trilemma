%% ========================================================================
%  PANDEMIC TRILEMMA - iLQR SOLVER V17 (dS-box), AVERAGE OECD ECONOMY
%  ------------------------------------------------------------------------
%  STRINGENCY ADJUSTMENT handled as a CONSTRAINT, not a penalty.
%
%  Earlier we penalized Delta S via a quadratic cost p_dS*(S_q-S_{q-1})^2.
%  That required augmenting the state with S_{t-1} and introduced a cost
%  CROSS TERM l_ux in the backward pass. Combined with the bilinear dynamics
%  (S*theta, S*F_DI), the cross term ill-conditions Quu (mu saturates) and
%  the problem becomes multimodal/solver-fragile. Diagnosis (terminal_mode
%  'none' + robust solver) showed the multimodality traces to the penalty's
%  cross term, NOT to the model economics or the terminal value.
%
%  FIX (cleaner): impose a hard per-quarter LIMIT on how fast stringency may
%  change, |S_q - S_{q-1}| <= dS_max, enforced by clamping in the forward
%  pass. A constraint has NO cost cross term, so the Hessian stays as benign
%  as the dS-free case, AND it is economically transparent: containment
%  cannot ramp faster than governments actually managed. dS_max is set to
%  the 95th percentile of observed quarter-on-quarter |Delta S| (in-support).
%
%  This removes the S_lag state entirely (back to n=8) and the p_dS penalty.
%
%  Control u = [S; F_above; F_loans; F_guar; F_DI];  state n=8 as in V16/V17.
%  Requires: country_data_for_matlab.csv, weekly_mortality_matlab.csv
% =========================================================================
clear; clc; close all;
fprintf('=== iLQR PLANNER V17 (dS as box constraint): AVERAGE OECD ===\n  %s\n\n', datestr(now));

%% ------------------------------------------------------------------------
%  Calibrated V15 parameters
% -------------------------------------------------------------------------
P.rho_y=0.231;
P.alpha_S=-0.095;%are there upper bounds?
P.alpha_above=0.544;
P.alpha_below=0.261;
P.alpha_DI=1.470;
P.alpha_SDI=-0.041;
P.beta_d=0;
P.c_lo=0.40; %Check this value
P.c_gu=0.25; 
P.r=0.001;
P.gamma_y=0.117;
P.k_ab=0.664;
P.k_lo=0.836;
P.k_gu=0.536;
P.k_di=0.526;
P.phi_t= 0;

rho_theta_pre  = 1.5;   rho_theta_post = 0.95;   q_vax = 8; %halbierung der rate durch impfung-> de Gier VET + Impfquote # Central estimate (Brauner et al. 2021; Flaxman et al. 2020) phi_S_range   <- c(0.60, 0.82)  # Range from Calibration Report
P.rho_th_q = [repmat(rho_theta_pre,1,q_vax-1), ...
              repmat(rho_theta_post,1,13-q_vax+1)];
P.th_max = 0.9;   % Sättigungsdeckel: max. gleichzeitige Prävalenz (10%); Robustness 0.05-0.15-> wenn ausstellen auf "Inf" setzen
P.decay_K  = 0.1;
P.phi_S=0.5;
ifr=[0.005 0.012 0.0045 0.002 0.002 0.0002];  %[0.005 0.005 0.004 0.003 0.002 0.0002]-> from data with lo %FLAG
P.delta_q=ifr([1 1 1 2 3 3 4 5 5 6 6 6 6])*1e6;
P.N=13; P.yr=4:16;
P.n=8; P.m=5;                       % <-- back to n = 8 (no S_lag state)
P.q_start = 2; %where to start and how
%state indices: 1 y | 2 b | 3 theta | 4 d | 5 fab_l1 | 6 fab_l2 | 7 fdi_l1 | 8 stock

qord={'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021', ...
      'Q3.2021','Q4.2021','Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
qlbl={'Q4.19','Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21','Q3.21', ...
      'Q4.21','Q1.22','Q2.22','Q3.22','Q4.22'};
%Q
cfe_y_val=[+1.1057 -1.0400 +0.3009 -0.0979 +1.2987 +1.3246 +1.9894 +0.1466 ...
           -3.5623 -1.7381 -0.0832 -4.8958 -1.8327 -1.4833 -1.8693 -3.3268 ...
           +0.2908 -2.0146 +8.3187 -4.8488 +2.3672 -0.5014 -1.9561 +0.6141 ...
           +0.7002 +2.3568 -0.6830 -3.2057 +1.0604 +1.2218 -1.0616 -0.5966 ...
           -2.7751 -0.2578 -1.8284 +1.0349 +4.0658 +1.0987];
%
cfe_b_val=[-0.8525, -0.6001, -0.8731, -0.2648, -0.4315, -1.0291, +0.5492, +0.1213, ...
             -0.6570, -1.0947, -1.6607, -0.9624, -0.4810, -0.6315, -0.9667, -1.3722, ...
             -1.3041, -0.9670, -0.2702, -0.8165, -0.3418, -1.1930, -1.6567, -0.5136, ...
             -0.4824, +0.1193, +0.0475, -0.9082, -0.4272, -0.1091, +0.0389, -0.6929, ...
             -1.7242, -0.1307, -0.9910, -0.5114, -0.3150, -0.2251];

%Startingvalues of Debt
b0_val=[37.6 69.7 77.9 44.2 16.3 29.6 47.7 53.2 30.2 34.0 34.1 86.0 11.8 54.5 ...
        85.4 107.0 205.0 52.1 54.3 69.9 56.9 122.0 199.0 35.1 29.3 26.6 39.7 ...
        7.25 43.9 16.6 73.0 45.6 111.0 48.4 72.1 27.7 54.0 97.3];

% Wave Effect
eps_v14=[-3.62 -8.55 -6.46 -7.79 -4.73 -10.10 -11.20 -3.75 -5.10 -5.98 -1.95 ...
         -9.57 -2.43 -1.97 -8.49 -12.50 -9.94 -8.99 -1.10 -4.70 -0.66 -10.90 ...
         -6.82 -1.83 -0.03 -5.31 -7.43 -7.97 -5.17 -2.45 -5.49 -1.80 -9.75 ...
         -4.87 -6.78 -4.70 -10.40 -4.79]; %FLAG

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
[gid,gC,gQ] = findgroups(M.Country, M.qstr); %FLAG-> wie rechnet er das hoch?
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
        Fl(i,k)=row.F_CP_loans;  Fg(i,k)=row.F_CP_guar; Fd(i,k)=row.F_DI; %FLAG-> adj or unadjusted
        y_o(i,k)=row.y_t_pct;
        if ~ismissing(row.debt_dR), bd(i,k)=row.debt_dR; end
        key = sprintf('%s_%s', iso, qord{k});
        if isKey(th_map,key) && ~isnan(th_map(key)), th_o(i,k)=th_map(key); end
        if isKey(d_map,key)  && ~isnan(d_map(key)),  d_o(i,k)=d_map(key);  end
    end
end
b_lvl = b0_val' + cumsum(bd,2);

eps_th = zeros(n_c,N+1);
for i = 1:n_c
    for k = 2:N
        expct = P.rho_th_q(k)*(1-P.phi_S*S_o(i,k)/100)*th_o(i,k-1)*(1 - th_o(i,k-1)/P.th_max);
        eps_th(i,k+1) = th_o(i,k) - expct;
    end
end
C.eps_th = mean(eps_th,1);
C.b0 = mean(b0_val); C.mu_y = 0; C.mu_b = 0; %mean(cfe_b_val) %auf 0 für die average economy
C.eps_y = zeros(1,N+1);
C.eps_y(3) = 0.08 * mean(eps_v14);   % Q1.20 onset
C.eps_y(4) = 0.92 * mean(eps_v14);   % Q2.20 through (FROM DATA BUT DISTRIBUTED ACROSS Q1 AND Q1 SO THAT IT FITS Q1.2020)-> alternativ auf 1 setten in dieser Zeile  %INITIAL SHOCk-> TAKE IT OUT FOR ROBUSTNESS TRAJECTORY STAYS

dev  = b_lvl - b0_val';
allF = [Fa(Fa>0); Fl(Fl>0); Fg(Fg>0); Fd(Fd>0)];

% ---- planner weights: welfare-calibrated (option 2)
% ---------------------- %FLAG
W.beta = 0.99;  %fixiert
W.w_y = 1/var(y_o(:),1);              % Varianz-normiert-> fixiert
tau   = 1; %Parameter um zu drehen
W.w_b = 1/var(b_lvl(:),1);                % Schulden: relativ zu Output via tau
W.lam_d = 2;          % lam_d = freier Relativgewicht-Parameter (Sweep)
W.w_d = 1/var(d_o(:),1);             % Varianz-normiert, analog zu w_y-> fixiert
fprintf('  tau=%.1f => w_y/w_b=%.2f | lam_d=%.2f, w_d/w_y=%.3f\n', ...
        tau, W.w_y/W.w_b, W.lam_d, W.w_d/W.w_y);

% Stringency LEVEL: small numerical regularizer only (output cost of S is
% already endogenous via alpha_S). No dS PENALTY here - dS is a constraint.
W.p_S = 0.02 * W.w_y; %0.005-0.02
W.p_F = 0.05/var(allF,1); %FLAG: was sind das für werte?

W.terminal_mode = 'debt_M'; %mit 'riccati' wird auch Theta als Endzustand gewichtet
W.lam_cons = 0.98;
W.M_term   = 1/(1 - W.beta*W.lam_cons^2); %FLAG

% V17e: box bounds (m x N). Q4.19 locked to zero.
ub_cap = [pctile(S_o(S_o>0),99); pctile(Fa(Fa>0),99); pctile(Fl(Fl>0),99); ...
          pctile(Fg(Fg>0),99);   pctile(Fd(Fd>0),99)];
lb = zeros(P.m, P.N);
ub = repmat(ub_cap, 1, P.N); %FLAG
ub(:, 1:(P.q_start-1)) = 0; 
lb(1,3) = mean(S_o(:,3));   % Q2.20 S-Untergrenze = observed-> wenn er bleibt passt das
%ub(1,3) = mean(S_o(:,3));   % Q2.20 S-Obergrenze = observed  -> fixiert

% ---- dS CONSTRAINT: max per-quarter change in S, from observed data ------
% 95th percentile of |S_{i,k} - S_{i,k-1}| over the panel (in-support cap on
% how fast any country actually changed stringency quarter to quarter).
dS_obs = abs(diff(S_o,1,2));              % n_c x (N-1) quarter-on-quarter |dS|
dS_obs = dS_obs(dS_obs > 0);              % ignore exact-zero (missing) gaps
P.dS_max = pctile(dS_obs, 95); %FLAG: MAX VALUE??
fprintf('  dS_max (obs p95 |Delta S|) = %.1f S-points/quarter\n', P.dS_max);

fprintf('  b0 = %.1f%% GDP | mu_y %+.3f | mu_b %+.3f | eps_y(Q2.20) %+.2f\n', ...
        C.b0, C.mu_y, C.mu_b, C.eps_y(4));
fprintf('  policy allowed from %s onward (Q4.19 locked: S and fiscal = 0)\n', qlbl{P.q_start});
fprintf('  caps: S<=%.1f, F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f\n\n', ub_cap);


y_obs_avg = mean(y_o,1);                 % 1xN, raw output gap
b_obs_avg = mean(b_lvl,1);               % 1xN, raw debt level
d_obs_avg = mean(d_o,1);                 % 1xN, raw deaths /10^6/wk



%% ------------------------------------------------------------------------
%  RUN iLQR
% -------------------------------------------------------------------------
x0 = zeros(P.n,1); x0(2) = C.b0; %FLAG: ist der Solver korrekt?
W.xbar = x0;
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
U_obs = [mean(S_o,1); mean(Fa,1); mean(Fl,1); mean(Fg,1); mean(Fd,1)];
U_obs(:,1:(P.q_start-1)) = 0;

fprintf('Q1.20 obs policy: S=%.1f Fab=%.2f Flo=%.2f Fgu=%.2f Fdi=%.2f\n', U_obs(:,2));   % <-- add here

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
        'Q','S','F_ab','F_lo','F_gu','F_DI','y','b','theta','d');
for k = 1:P.N
    fprintf('  %6s %6.1f %5.2f %5.2f %5.2f %5.2f | %+7.2f %7.2f %8.5f %6.1f\n', ...
        qlbl{k}, U(1,k),U(2,k),U(3,k),U(4,k),U(5,k), ...
        X(1,k+1),X(2,k+1),X(3,k+1),X(4,k+1));
end
fprintf('\n  Cum. output gap: %+.2f ppQ | cum. deaths: %.0f/million | terminal debt %.2f%% (db %+.2f)\n', ...
    sum(X(1,2:end)), sum(X(4,2:end))*13, X(2,end), X(2,end)-C.b0);
fprintf('\n  CUM comparison    | optimal | observed\n');
fprintf('  cum output gap    | %+7.2f | %+7.2f\n', sum(X(1,2:end)),    sum(Xo(1,2:end)));
fprintf('  cum deaths /M     | %7.0f | %7.0f\n',   sum(X(4,2:end))*13, sum(Xo(4,2:end))*13);
fprintf('  terminal debt %%   | %7.2f | %7.2f\n',   X(2,end),          Xo(2,end));
qs = 1:N;                                  % states and controls share ticks 1:N
figure('Color','w','Position',[60 60 1200 600]);
ttl={'Stringency S','Output gap y (pp)','Debt b (% GDP)','Deaths d (/10^6/wk)'};

subplot(2,2,1);                            % control S, ticks 1:N
stairs(1:N, U(1,:),    'b-', 'LineWidth',2); hold on;
stairs(1:N, U_obs(1,:),'k--','LineWidth',1.5);
legend('optimal','observed (raw)','Location','best');
set(gca,'XTick',1:N,'XTickLabel',qlbl,'XTickLabelRotation',45);

subplot(2,2,2);                            % output gap
plot(qs, X(1,2:N+1), 'b-o','LineWidth',2); hold on;
plot(qs, Xo(1,2:N+1),'r-^','LineWidth',1.5);
plot(qs, y_obs_avg,'k--s'); yline(0,':');
legend('optimal','model @ obs policy','observed (raw)','Location','best');
set(gca,'XTick',qs,'XTickLabel',qlbl,'XTickLabelRotation',45);

subplot(2,2,3);                            % debt
plot(qs, X(2,2:N+1), 'b-o','LineWidth',2); hold on;
plot(qs, Xo(2,2:N+1),'r-^','LineWidth',1.5);
plot(qs, b_obs_avg,'k--s'); yline(C.b0,':');
legend('optimal','model @ obs policy','observed (raw)','Location','best');
set(gca,'XTick',qs,'XTickLabel',qlbl,'XTickLabelRotation',45);

subplot(2,2,4);                            % deaths
plot(qs, X(4,1:N), 'b-o','LineWidth',2); hold on;
plot(qs, Xo(4,1:N),'r-^','LineWidth',1.5);
plot(qs, d_obs_avg,'k--s');
legend('optimal','model @ obs policy','observed (raw)','Location','best');
set(gca,'XTick',qs,'XTickLabel',qlbl,'XTickLabelRotation',45);

for sp=1:4, subplot(2,2,sp); xlim([1 N]); grid on; title(ttl{sp}); end
sgtitle('iLQR planner V17 (dS box constraint) vs observed average OECD','FontWeight','bold');

%plot(qs, P.delta_q.*mean(th_o,1), 'r:','LineWidth',1.5);   % model deaths
%at observed theta-> switch back on
%% ========================================================================
%  LOCAL FUNCTIONS
% =========================================================================

function xp = f_step(x, u, q, P, C)
    S=u(1); fab=u(2); flo=u(3); fgu=u(4); fdi=u(5);
    y=x(1); b=x(2); th=x(3); d=x(4); a1=x(5); a2=x(6); di1=x(7); st=x(8);
    st_used = (1-P.decay_K)*st + P.c_lo*flo + P.c_gu*fgu;
    eth = 0; if q+1 <= P.N, eth = C.eps_th(q+2); end
    xp = zeros(P.n,1);
    xp(3) = P.rho_th_q(q)*(1-P.phi_S*S/100)*th*(1 - th/P.th_max) + eth;
    xp(4) = P.delta_q(q)*th;
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
% Running loss (NO dS penalty - dS is a constraint) + terminal Phi(x_N).
    J = 0;
    for q = 1:P.N
        x = X(:,q+1); u = U(:,q);
        sc = W.w_y*x(1)^2 + W.lam_d*W.w_d*x(4)^2 + W.w_b*(x(2)-C.b0)^2;
        cc = W.p_S*u(1)^2 + W.p_F*sum(u(2:5).^2);
        J = J + W.beta^q * (sc + cc);
    end
    dx = X(:,end) - W.xbar;
    J = J + W.beta^P.N * (dx' * W.P_inf * dx);
end

function [A,B] = jacobians(x, u, q, P)
    S=u(1); th=x(3); di1=x(7);
    A = zeros(P.n); B = zeros(P.n,P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI + P.alpha_SDI*S; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,1)=P.alpha_S + P.alpha_SDI*di1;
    B(1,3)=P.alpha_below*P.c_lo; B(1,4)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,2)=P.k_ab; B(2,3)=P.k_lo*P.c_lo; B(2,4)=P.k_gu*P.c_gu;
    g = P.rho_th_q(q)*(1-P.phi_S*S/100);
    A(3,3)= g*(1 - 2*th/P.th_max);
    B(3,1)=-P.rho_th_q(q)*P.phi_S/100*th*(1 - th/P.th_max);
    A(4,3)=P.delta_q(q);
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,2)=1; B(7,5)=1; B(8,3)=P.c_lo; B(8,4)=P.c_gu;
end

function [lx,lu,lxx,luu] = cost_derivs(xp, u, q, P, C, W)
% NO dS cross term: cost is separable in (x,u) again, so lux = 0 (omitted).
    dq = W.beta^q;
    lx = zeros(P.n,1); lxx = zeros(P.n);
    lx(1)=2*dq*W.w_y*xp(1); lx(4)=2*dq*W.lam_d*W.w_d*xp(4);
    lx(2)=2*dq*W.w_b*(xp(2)-C.b0);
    lxx(1,1)=2*dq*W.w_y; lxx(4,4)=2*dq*W.lam_d*W.w_d; lxx(2,2)=2*dq*W.w_b;
    lu  = 2*dq*[W.p_S*u(1); W.p_F*u(2:5)];
    luu = 2*dq*diag([W.p_S, W.p_F*ones(1,4)]);
end

function [X,U,J] = ilqr(U0, x0, P, C, W, lb, ub) %FLAG: Control Costs hier?-> braucht es sonst ist die Matrix singular
% STEPs 3-5: iLQR main loop. NO cost cross term (separable cost) => the
% backward pass is as benign as the dS-free case. The dS limit is enforced
% only in the FORWARD pass by clamping S relative to the previous quarter.
    U = U0; X = rollout(U, x0, P, C); J = total_cost(X, U, P, C, W);
    mu = 1e-6; mu_max = 1e12; max_iter = 1400; tol = 1e-9;
    reject_run = 0; reject_cap = 40;
    for it = 1:max_iter
        Vx  = 2*W.beta^P.N * W.P_inf * (X(:,end) - W.xbar);
        Vxx = 2*W.beta^P.N * W.P_inf;
        ks = zeros(P.m,P.N); Ks = zeros(P.m,P.n,P.N); fail = false;
        for q = P.N:-1:1
            [A,B] = jacobians(X(:,q), U(:,q), q, P);
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
        if fail, mu = min(max(mu*10,1e-6), mu_max); continue; end
        improved = false;
        for alpha = 2.^(0:-1:-12)
            Xn = zeros(size(X)); Xn(:,1) = x0; Un = zeros(size(U));
            for q = 1:P.N
                du = alpha*ks(:,q) + Ks(:,:,q)*(Xn(:,q)-X(:,q));
                uq = min(max(U(:,q)+du, lb(:,q)), ub(:,q));   % static box
                % --- dS CONSTRAINT: limit S relative to previous quarter ---
                if q == 1
                    Sprev = 0;                 % Q4.19 locked at S=0
                else
                    Sprev = Un(1,q-1);
                end
                uq(1) = min(max(uq(1), Sprev - P.dS_max), Sprev + P.dS_max);
                uq(1) = min(max(uq(1), lb(1,q)), ub(1,q));    % re-apply static box
                Un(:,q) = uq;
                Xn(:,q+1) = f_step(Xn(:,q), Un(:,q), q, P, C);
            end
            Jn = total_cost(Xn, Un, P, C, W);
            if Jn < J - 1e-12, improved = true; break; end
        end
        if improved
            rel = (J-Jn)/max(J,1e-12);
            X = Xn; U = Un; J = Jn;
            mu = max(mu/2,1e-8); reject_run = 0;
            if mod(it,25)==0
                fprintf('  iter %3d: J = %10.4f (rel %.1e, mu %.1e)\n',it,J,rel,mu);
            end
            if rel < tol
                fprintf('  CONVERGED at iter %d: J = %.4f\n', it, J); break;
            end
        else
            mu = min(mu*10, mu_max); reject_run = reject_run + 1;
            if reject_run >= reject_cap
                fprintf('  stalled (no progress in %d steps) at iter %d: J = %.4f\n', ...
                        reject_cap, it, J); break;
            end
            if mu >= mu_max && reject_run >= 5
                fprintf('  converged (mu saturated) at iter %d: J = %.4f\n', it, J); break;
            end
        end
    end
end

function [A,B,Q,R] = stationary_matrices(P, W)
% Post-pandemic stationary LQ problem (deviation form), n=8.
    A = zeros(P.n); B = zeros(P.n, P.m);
    A(1,1)=P.rho_y; A(1,4)=-P.beta_d; A(1,6)=P.alpha_above;
    A(1,7)=P.alpha_DI; A(1,8)=P.alpha_below*(1-P.decay_K);
    B(1,1)=P.alpha_S; B(1,3)=P.alpha_below*P.c_lo; B(1,4)=P.alpha_below*P.c_gu;
    A(2,1)=-P.gamma_y; A(2,2)=1+P.r; A(2,7)=P.k_di;
    B(2,2)=P.k_ab; B(2,3)=P.k_lo*P.c_lo; B(2,4)=P.k_gu*P.c_gu;
    A(3,3)=P.rho_th_q(end);
    A(4,3)=P.delta_q(end);
    A(6,5)=1; A(8,8)=1-P.decay_K;
    B(5,2)=1; B(7,5)=1; B(8,3)=P.c_lo; B(8,4)=P.c_gu;
    Q = zeros(P.n); Q(1,1)=W.w_y; Q(4,4)=W.lam_d*W.w_d; Q(2,2)=W.w_b;
    R = diag([W.p_S, W.p_F*ones(1,4)]);
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

