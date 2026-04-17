%% ========================================================================
%  PANDEMIC TRILEMMA — COUNTERFACTUAL ANALYSIS
%
%  Prerequisite: Run pandemic_counterfactuals.m first (calibration OK).
%  This script performs the counterfactual decomposition:
%
%  CF-A: Fix quarterly total F_k, optimize CP/DI split
%        → pure COMPOSITION effect
%  CF-B: Fix cumulative total ΣF, optimize timing + composition
%        → COMPOSITION + TIMING effect
%  Decomposition: Composition cost = J_obs - J_A
%                 Timing cost      = J_A - J_B
%  Pareto dominance: weight-free (exists budget-neutral improvement?)
%
%  All counterfactuals condition on observed S, theta, d.
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Counterfactual Analysis ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS (identical to calibration file)
% =========================================================================
rho_y=0.372; psi=0.200; alpha_S=0.016; alpha_F_CP=0.249;
eta_tilde=-0.400; eta_p=2.600; alpha_F_DI=0.224; beta_fear=-0.022;
r_int=0.001; gamma_y=0.219; kappa_F_DI=0.379; c_H=0.02;

qfe_pp = [-1.6945,-9.2225,1.1002,-0.8301,-0.1618,...
            0.2676,0.3364,0.8448,-0.2020,-0.3219];

cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI',...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR',...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR',...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL',...
           'PRT','SVK','SVN','SWE','TUR','USA'};
cfe_y_val = [0.1530,-1.3482,0.1786,-0.9951,0.7648,0.2349,0.9292,-0.5516,...
            -2.7814,-1.2624,0.3376,-3.9030,-1.2179,-1.1787,-1.3129,-2.4354,...
            -0.5643,-1.7510,5.1692,-3.8459,0.5602,-0.0380,-1.1328,0.0174,...
             0.2433,0.9685,-0.6834,-2.4163,0.1424,0.4993,-0.9626,-0.8523,...
            -2.4815,-0.4481,-1.5356,0.3285,2.8974,0.3696];
cfe_b_val = [-0.6746,-0.2777,-0.4649,-0.2143,-0.4844,-0.9781,0.1462,0.4048,...
             -1.0619,-1.0287,-1.2539,-1.2272,-0.5830,-0.5348,-0.8409,-0.8974,...
             -0.6096,-0.6704,0.9444,-1.0335,0.2331,-0.6479,-0.2022,-0.2991,...
             -0.5080,0.2937,-0.0212,-1.6298,-0.4222,-0.4402,0.3454,-0.6348,...
             -1.4319,-0.0641,-0.4026,-0.3780,0.3130,0.6681];
kappa_cp_i = [0.3908,0.3548,0.2079,0.2952,0.2425,0.1903,0.2290,0.2723,...
              0.1757,0.2085,0.3273,0.1973,0.2982,0.2182,0.2081,0.2412,...
              0.3514,0.2611,0.3521,0.3465,0.3243,0.1970,0.2602,0.2864,...
              0.3250,0.2506,0.2764,0.3195,0.2994,0.2328,0.3647,0.3157,...
              0.2030,0.3968,0.3290,0.2735,0.1727,0.3257];

beta_disc=0.99; w_y=100; w_b=30; W_b=150; r_cp=3; r_di=5;
N=10; K_act=8; nx=3; nu=2;
eps_y_vec=zeros(1,N+1);
for k=1:N, if k<=length(qfe_pp), eps_y_vec(k+1)=qfe_pp(k)/100; end; end
u_lo=[0;0]; u_hi=[0.20;0.10];

P = struct('rho_y',rho_y,'psi',psi,'alpha_S',alpha_S,...
    'alpha_F_CP',alpha_F_CP,'eta_tilde',eta_tilde,'eta_p',eta_p,...
    'alpha_F_DI',alpha_F_DI,'beta_fear',beta_fear,...
    'r_int',r_int,'gamma_y',gamma_y,'kappa_F_DI',kappa_F_DI,'c_H',c_H,...
    'eps_y_vec',eps_y_vec,'beta_disc',beta_disc,...
    'w_y',w_y,'w_b',w_b,'W_b',W_b,'r_cp',r_cp,'r_di',r_di,...
    'N',N,'K_act',K_act,'nx',nx,'nu',nu,'u_lo',u_lo,'u_hi',u_hi);

cfe_y_map = containers.Map(cfe_iso, cfe_y_val/100);
cfe_b_map = containers.Map(cfe_iso, cfe_b_val/100);
kappa_map = containers.Map(cfe_iso, kappa_cp_i);


%% ========================================================================
%  LOAD DATA
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');
qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020',...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021',...
        'Q1.2022','Q2.2022'};
qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20',...
        'Q1.21','Q2.21','Q3.21','Q4.21',...
        'Q1.22','Q2.22'};
countries = unique(T.Country,'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S=zeros(1,N); cdata(i).FCP=zeros(1,N); cdata(i).FDI=zeros(1,N);
    cdata(i).y=zeros(1,N); cdata(i).theta=zeros(1,N); cdata(i).b=zeros(1,N);
    cdata(i).d=zeros(1,N);
    cdata(i).mu_y=0; cdata(i).mu_b=0; cdata(i).kappa_cp=0.277;
    if isKey(cfe_y_map,iso), cdata(i).mu_y=cfe_y_map(iso); end
    if isKey(cfe_b_map,iso), cdata(i).mu_b=cfe_b_map(iso); end
    if isKey(kappa_map,iso), cdata(i).kappa_cp=kappa_map(iso); end
    for k = 1:N
        if k>length(qord), break; end
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}),:);
        if isempty(row), continue; end
        cdata(i).S(k)=row.S_mean_tw/100; cdata(i).FCP(k)=row.F_CP/100;
        cdata(i).FDI(k)=row.F_DI/100; cdata(i).y(k)=row.y_t_pct/100;
        cdata(i).theta(k)=row.theta_pct/100;
        if ~ismissing(row.debt_dR), cdata(i).b(k)=row.debt_dR/100; end
        if ismember('excess_mortality',T.Properties.VariableNames) ...
                && ~ismissing(row.excess_mortality)
            cdata(i).d(k)=row.excess_mortality/100;
        end
    end
end
fprintf('  %d countries loaded\n\n', n_c);


%% ========================================================================
%  CF-A: PURE COMPOSITION (fix quarterly total, optimize CP/DI split)
%
%  For each country and quarter k:
%    F_k^total = F_k^{CP,obs} + F_k^{DI,obs}   (fixed)
%    Choose omega_k in [0,1] to set F_k^CP = omega_k * F_k^total
%                                     F_k^DI = (1-omega_k) * F_k^total
%    Subject to box constraints on F^CP and F^DI.
%
%  Solved via grid search over omega (1000 points per quarter).
%  Then refined via fminsearch starting from grid optimum.
% =========================================================================
fprintf('========================================\n');
fprintf('  CF-A: Pure Composition\n');
fprintf('========================================\n\n');

n_grid = 1000;

for i = 1:n_c
    iso  = cdata(i).iso;
    S_i  = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i = cdata(i).mu_y;   mb_i = cdata(i).mu_b;    kc_i = cdata(i).kappa_cp;
    fcp_obs = cdata(i).FCP;  fdi_obs = cdata(i).FDI;

    % Observed total per quarter
    F_total = fcp_obs(1:K_act) + fdi_obs(1:K_act);

    % Grid search: for each quarter, find optimal omega
    best_omega = zeros(1, K_act);
    for kk = 1:K_act
        if F_total(kk) < 1e-8
            % No spending this quarter — nothing to optimize
            best_omega(kk) = 0;
            continue;
        end

        % Feasible omega range given box constraints
        omega_lo = max(0, 1 - u_hi(2)/F_total(kk));  % F_DI <= u_hi(2)
        omega_hi = min(1, u_hi(1)/F_total(kk));       % F_CP <= u_hi(1)
        if omega_lo > omega_hi
            % Constraints conflict — use observed
            best_omega(kk) = fcp_obs(kk) / F_total(kk);
            continue;
        end

        omegas = linspace(omega_lo, omega_hi, n_grid);
        J_vals = zeros(1, n_grid);
        for g = 1:n_grid
            fcp_test = fcp_obs(1:K_act);
            fdi_test = fdi_obs(1:K_act);
            fcp_test(kk) = omegas(g) * F_total(kk);
            fdi_test(kk) = (1-omegas(g)) * F_total(kk);
            J_vals(g) = eval_J(fcp_test, fdi_test, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
        end
        [~, idx] = min(J_vals);
        best_omega(kk) = omegas(idx);
    end

    % Joint optimization: optimize all omega simultaneously via fminsearch
    obj_A = @(w) eval_omega(w, F_total, fcp_obs, fdi_obs, S_i, th_i, d_i, ...
                            my_i, mb_i, kc_i, P);
    opts = optimset('MaxIter',5000,'MaxFunEvals',50000,...
                    'TolFun',1e-13,'TolX',1e-12,'Display','off');
    [omega_opt, J_A] = fminsearch(obj_A, best_omega, opts);

    % Enforce bounds
    for kk = 1:K_act
        if F_total(kk) < 1e-8, omega_opt(kk)=0; continue; end
        omega_opt(kk) = max(0, min(1, omega_opt(kk)));
        omega_opt(kk) = max(omega_opt(kk), 1 - u_hi(2)/F_total(kk));
        omega_opt(kk) = min(omega_opt(kk), u_hi(1)/F_total(kk));
    end

    % Build CF-A controls
    fcp_A = omega_opt .* F_total;
    fdi_A = (1-omega_opt) .* F_total;
    fcp_A_full = [fcp_A, zeros(1, N-K_act)];
    fdi_A_full = [fdi_A, zeros(1, N-K_act)];

    % Evaluate
    J_A = eval_J(fcp_A, fdi_A, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    J_obs = eval_J(fcp_obs(1:K_act), fdi_obs(1:K_act), S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    xs_obs = forward_roll(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    xs_A   = forward_roll(fcp_A_full, fdi_A_full, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % CP share: observed vs optimal
    cp_share_obs = sum(fcp_obs(1:K_act)) / max(sum(F_total), 1e-10);
    cp_share_A   = sum(fcp_A) / max(sum(F_total), 1e-10);

    % Store results
    cdata(i).J_obs      = J_obs;
    cdata(i).J_A        = J_A;
    cdata(i).fcp_A      = fcp_A;
    cdata(i).fdi_A      = fdi_A;
    cdata(i).xs_obs     = xs_obs;
    cdata(i).xs_A       = xs_A;
    cdata(i).cp_sh_obs  = cp_share_obs;
    cdata(i).cp_sh_A    = cp_share_A;
    cdata(i).dy_A       = (xs_A(1,N+1) - xs_obs(1,N+1)) * 100;     % output gain (pp)
    cdata(i).db_A       = (xs_A(2,N+1) - xs_obs(2,N+1)) * 100;     % debt change (pp)
    cdata(i).dJ_A       = J_obs - J_A;                               % welfare gain

    fprintf('  [%2d/%d] %s  dJ=%+.4f  dy=%+.2fpp  db=%+.2fpp  CP: %.0f%%->%.0f%%\n', ...
        i, n_c, iso, cdata(i).dJ_A, cdata(i).dy_A, cdata(i).db_A, ...
        cp_share_obs*100, cp_share_A*100);
end


%% ========================================================================
%  CF-A: SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  CF-A Summary: Pure Composition\n');
fprintf('========================================\n');

dJ_A_all = [cdata.dJ_A];
dy_A_all = [cdata.dy_A];
db_A_all = [cdata.db_A];
cp_obs_all = [cdata.cp_sh_obs] * 100;
cp_A_all   = [cdata.cp_sh_A] * 100;
cp_shift   = cp_A_all - cp_obs_all;

fprintf('\n  Welfare gain (dJ):  Mean: %.4f   Median: %.4f\n', mean(dJ_A_all), median(dJ_A_all));
fprintf('  Output gain (dy):   Mean: %+.2f pp  Median: %+.2f pp\n', mean(dy_A_all), median(dy_A_all));
fprintf('  Debt change (db):   Mean: %+.2f pp  Median: %+.2f pp\n', mean(db_A_all), median(db_A_all));
fprintf('  CP share shift:     Mean: %+.1f pp  Median: %+.1f pp\n', mean(cp_shift), median(cp_shift));
fprintf('  Countries improved: %d / %d\n', sum(dJ_A_all > 0), n_c);
fprintf('  Pareto-dominant:    %d / %d  (dy>=0 AND db<=0)\n', ...
    sum(dy_A_all >= -0.01 & db_A_all <= 0.01), n_c);

% Top 5 and bottom 5
[~, si] = sort(dJ_A_all, 'descend');
fprintf('\n  Top 5 by welfare gain:\n');
fprintf('  %5s %8s %8s %8s %10s\n', 'ISO', 'dJ', 'dy(pp)', 'db(pp)', 'CP shift');
for j = 1:5
    ii = si(j);
    fprintf('  %5s %8.4f %+8.2f %+8.2f %+8.1f pp\n', ...
        cdata(ii).iso, dJ_A_all(ii), dy_A_all(ii), db_A_all(ii), cp_shift(ii));
end
fprintf('\n  Bottom 5:\n');
for j = n_c-4:n_c
    ii = si(j);
    fprintf('  %5s %8.4f %+8.2f %+8.2f %+8.1f pp\n', ...
        cdata(ii).iso, dJ_A_all(ii), dy_A_all(ii), db_A_all(ii), cp_shift(ii));
end


fprintf('\n=== CF-A COMPLETE ===\n');


%% ########################################################################
%  FUNCTIONS (same as calibration file)
%  ########################################################################

function J = eval_omega(omega, F_total, fcp_obs, fdi_obs, S, theta, d, mu_y, mu_b, kcp, P)
    Ka = P.K_act;
    fcp = fcp_obs(1:Ka);  fdi = fdi_obs(1:Ka);
    for kk = 1:Ka
        if F_total(kk) < 1e-8, continue; end
        w = max(0, min(1, omega(kk)));
        fcp(kk) = w * F_total(kk);
        fdi(kk) = (1-w) * F_total(kk);
        fcp(kk) = min(P.u_hi(1), max(0, fcp(kk)));
        fdi(kk) = min(P.u_hi(2), max(0, fdi(kk)));
    end
    J = eval_J(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P);
end

function xs = forward_roll(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
    N_=P.N; xs=zeros(P.nx, N_+1);
    for k=1:N_
        y=xs(1,k); b=xs(2,k); z=xs(3,k);
        fk=0; gk=0; Sk=0; thk=0; dk=0; ey=0;
        if k<=length(fcp), fk=fcp(k); end
        if k<=length(fdi), gk=fdi(k); end
        if k<=length(S), Sk=S(k); end
        if k<=length(theta), thk=theta(k); end
        if k<=length(d), dk=d(k); end
        if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
        xs(1,k+1) = mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
            + (P.alpha_F_CP + P.eta_tilde*Sk - P.eta_p*y)*fk ...
            + P.alpha_F_DI*z + P.beta_fear*dk + ey;
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
            + kcp*fk + P.kappa_F_DI*gk + P.c_H*thk;
        xs(3,k+1) = gk;
    end
end

function J = eval_J(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
    fcp_f=[fcp, zeros(1, P.N-length(fcp))];
    fdi_f=[fdi, zeros(1, P.N-length(fdi))];
    xs=forward_roll(fcp_f, fdi_f, S, theta, d, mu_y, mu_b, kcp, P);
    N_=P.N; J=0;
    for k=1:N_
        bd=P.beta_disc^(k-1);
        J=J+bd*0.5*(P.w_y*xs(1,k+1)^2 + P.w_b*xs(2,k+1)^2);
        fk=0; gk=0;
        if k<=length(fcp), fk=fcp(k); gk=fdi(k); end
        J=J+bd*0.5*(P.r_cp*fk^2 + P.r_di*gk^2);
    end
    J=J+P.beta_disc^N_*0.5*P.W_b*xs(2,N_+1)^2;
end


%% ========================================================================
%  CF-B: COMPOSITION + TIMING
%  Fix cumulative total: sum_{k=1}^{K_act} (FCP+FDI) = sum observed
%  Optimize timing AND composition freely.
%  Then decompose: Composition = J_obs - J_A,  Timing = J_A - J_B
% =========================================================================
fprintf('\n========================================\n');
fprintf('  CF-B: Composition + Timing\n');
fprintf('========================================\n\n');

for i = 1:n_c
    iso  = cdata(i).iso;
    S_i  = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i = cdata(i).mu_y;   mb_i = cdata(i).mu_b;    kc_i = cdata(i).kappa_cp;
    fcp_obs = cdata(i).FCP;  fdi_obs = cdata(i).FDI;

    % Solve CF-B via iLQR (primary) + Direct Shooting (verification)
    [us_il, J_il] = solve_ilqr_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    [us_ds, J_ds] = solve_direct_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, P, us_il);

    % Take best
    if J_ds < J_il
        us_B = us_ds;  J_B = J_ds;
    else
        us_B = us_il;  J_B = J_il;
    end

    % Full forward roll
    fcp_B_full = [us_B(1,:), zeros(1, N-K_act)];
    fdi_B_full = [us_B(2,:), zeros(1, N-K_act)];
    xs_B = forward_roll(fcp_B_full, fdi_B_full, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % CP share
    cp_share_B = sum(us_B(1,:)) / max(sum(us_B(:)), 1e-10);

    % Store
    cdata(i).J_B       = J_B;
    cdata(i).us_B      = us_B;
    cdata(i).xs_B      = xs_B;
    cdata(i).cp_sh_B   = cp_share_B;
    cdata(i).dy_B      = (xs_B(1,N+1) - cdata(i).xs_obs(1,N+1)) * 100;
    cdata(i).db_B      = (xs_B(2,N+1) - cdata(i).xs_obs(2,N+1)) * 100;
    cdata(i).dJ_B      = cdata(i).J_obs - J_B;

    % Decomposition
    cdata(i).cost_comp   = cdata(i).J_obs - cdata(i).J_A;    % composition
    cdata(i).cost_timing = cdata(i).J_A   - cdata(i).J_B;    % timing
    cdata(i).cost_total  = cdata(i).J_obs - cdata(i).J_B;    % total

    fprintf('  [%2d/%d] %s  dJ_B=%+.4f  dy=%+.2fpp  db=%+.2fpp  CP:%.0f%%  [comp:%.0f%% tim:%.0f%%]\n', ...
        i, n_c, iso, cdata(i).dJ_B, cdata(i).dy_B, cdata(i).db_B, ...
        cp_share_B*100, ...
        cdata(i).cost_comp/max(cdata(i).cost_total,1e-10)*100, ...
        cdata(i).cost_timing/max(cdata(i).cost_total,1e-10)*100);
end


%% ========================================================================
%  CF-B SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  CF-B Summary\n');
fprintf('========================================\n');

dJ_B_all = [cdata.dJ_B];
dy_B_all = [cdata.dy_B];
db_B_all = [cdata.db_B];
cp_B_all = [cdata.cp_sh_B] * 100;

fprintf('\n  Welfare gain (dJ):  Mean: %.4f   Median: %.4f\n', mean(dJ_B_all), median(dJ_B_all));
fprintf('  Output gain (dy):   Mean: %+.2f pp  Median: %+.2f pp\n', mean(dy_B_all), median(dy_B_all));
fprintf('  Debt change (db):   Mean: %+.2f pp  Median: %+.2f pp\n', mean(db_B_all), median(db_B_all));
fprintf('  CP share (opt):     Mean: %.0f%%    Median: %.0f%%\n', mean(cp_B_all), median(cp_B_all));
fprintf('  Countries improved: %d / %d\n', sum(dJ_B_all > 0), n_c);
fprintf('  Pareto-dominant:    %d / %d  (dy>=0 AND db<=0)\n', ...
    sum(dy_B_all >= -0.01 & db_B_all <= 0.01), n_c);


%% ========================================================================
%  DECOMPOSITION: Composition vs Timing
% =========================================================================
fprintf('\n========================================\n');
fprintf('  Decomposition: Composition vs Timing\n');
fprintf('========================================\n');

cost_comp_all   = [cdata.cost_comp];
cost_timing_all = [cdata.cost_timing];
cost_total_all  = [cdata.cost_total];

% Share of total
sh_comp   = cost_comp_all ./ max(cost_total_all, 1e-10) * 100;
sh_timing = cost_timing_all ./ max(cost_total_all, 1e-10) * 100;

fprintf('\n  %30s %10s %10s %10s\n', '', 'Mean', 'Median', 'SD');
fprintf('  %30s %10.4f %10.4f %10.4f\n', 'Composition cost (J_obs-J_A)', mean(cost_comp_all), median(cost_comp_all), std(cost_comp_all));
fprintf('  %30s %10.4f %10.4f %10.4f\n', 'Timing cost (J_A-J_B)',        mean(cost_timing_all), median(cost_timing_all), std(cost_timing_all));
fprintf('  %30s %10.4f %10.4f %10.4f\n', 'Total cost (J_obs-J_B)',       mean(cost_total_all), median(cost_total_all), std(cost_total_all));
fprintf('\n  Share of total welfare cost:\n');
fprintf('    Composition: %.0f%%   Timing: %.0f%%  (mean)\n', mean(sh_comp), mean(sh_timing));
fprintf('    Composition: %.0f%%   Timing: %.0f%%  (median)\n', median(sh_comp), median(sh_timing));

% Per-country table
fprintf('\n  Per-country decomposition (sorted by total cost):\n');
fprintf('  %5s %8s %8s %8s %6s %6s %8s %8s\n', ...
    'ISO', 'J_comp', 'J_time', 'J_total', '%comp', '%time', 'dy_B', 'db_B');
[~, si] = sort(cost_total_all, 'descend');
for j = 1:n_c
    ii = si(j);
    fprintf('  %5s %8.4f %8.4f %8.4f %5.0f%% %5.0f%% %+8.2f %+8.2f\n', ...
        cdata(ii).iso, cost_comp_all(ii), cost_timing_all(ii), cost_total_all(ii), ...
        sh_comp(ii), sh_timing(ii), dy_B_all(ii), db_B_all(ii));
end


%% ========================================================================
%  PARETO DOMINANCE (weight-free)
% =========================================================================
fprintf('\n========================================\n');
fprintf('  Pareto Dominance Analysis\n');
fprintf('========================================\n');

pareto_A = (dy_B_all >= -0.01) & (db_B_all <= 0.01);  % CF-B improves both
n_pareto = sum(pareto_A);
fprintf('\n  Pareto-dominated countries (CF-B): %d / %d\n', n_pareto, n_c);
fprintf('  (Budget-neutral reallocation that improves output AND reduces debt)\n');

if n_pareto > 0
    fprintf('\n  %5s %8s %8s %8s %8s\n', 'ISO', 'dy(pp)', 'db(pp)', 'CP_obs%', 'CP_opt%');
    for ii = 1:n_c
        if pareto_A(ii)
            fprintf('  %5s %+8.2f %+8.2f %8.0f %8.0f\n', ...
                cdata(ii).iso, dy_B_all(ii), db_B_all(ii), ...
                cdata(ii).cp_sh_obs*100, cdata(ii).cp_sh_B*100);
        end
    end
end

fprintf('\n  Mean Pareto gain (dominated countries only):\n');
fprintf('    Output: %+.2f pp   Debt: %.2f pp\n', ...
    mean(dy_B_all(pareto_A)), mean(db_B_all(pareto_A)));


%% ========================================================================
%  RESULTS TABLE (CSV export)
% =========================================================================
res_table = table({cdata.iso}', ...
    [cdata.J_obs]', [cdata.J_A]', [cdata.J_B]', ...
    [cdata.cost_comp]', [cdata.cost_timing]', [cdata.cost_total]', ...
    sh_comp', sh_timing', ...
    [cdata.dy_A]', [cdata.db_A]', ...
    [cdata.dy_B]', [cdata.db_B]', ...
    [cdata.cp_sh_obs]'*100, [cdata.cp_sh_A]'*100, [cdata.cp_sh_B]'*100, ...
    pareto_A', ...
    'VariableNames', {'Country', ...
        'J_obs','J_A','J_B', ...
        'cost_comp','cost_timing','cost_total', ...
        'pct_comp','pct_timing', ...
        'dy_A','db_A','dy_B','db_B', ...
        'cp_obs','cp_cfA','cp_cfB', ...
        'pareto_dominated'});
writetable(res_table, 'counterfactual_results.csv');
fprintf('\n  Saved: counterfactual_results.csv\n');


%% ========================================================================
%  VISUALIZATION
% =========================================================================

% --- Fig 1: Pareto Plot (dy vs db, CF-B) ---
figure('Name','Pareto','Color','w','Position',[50 50 600 500]);
hold on;
scatter(db_B_all(pareto_A), dy_B_all(pareto_A), 60, [.2 .6 .2], 'filled');
scatter(db_B_all(~pareto_A), dy_B_all(~pareto_A), 60, [.8 .3 .3], 'filled');
xline(0, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]);
% Label points
for ii = 1:n_c
    text(db_B_all(ii)+0.02, dy_B_all(ii)+0.01, cdata(ii).iso, 'FontSize', 6);
end
xlabel('\Delta Debt (pp GDP)');  ylabel('\Delta Output (pp)');
title('CF-B: Budget-Neutral Reallocation');
legend('Pareto-dominant', 'Not Pareto-dominant', 'Location', 'NW');
% Shade Pareto-dominant quadrant
xl = xlim;  yl = ylim;
fill([xl(1) 0 0 xl(1)], [0 0 yl(2) yl(2)], [.2 .8 .2], ...
    'FaceAlpha', 0.05, 'EdgeColor', 'none');
grid on;

% --- Fig 2: Decomposition bar chart ---
figure('Name','Decomposition','Color','w','Position',[50 50 1200 400]);
[~, si] = sort(cost_total_all, 'descend');
bar_data = [cost_comp_all(si); cost_timing_all(si)]';
bh = bar(1:n_c, bar_data, 'stacked');
bh(1).FaceColor = [.2 .5 .8];
bh(2).FaceColor = [.8 .5 .2];
set(gca, 'XTick', 1:n_c, 'XTickLabel', {cdata(si).iso}, ...
    'XTickLabelRotation', 55, 'FontSize', 6);
ylabel('Welfare cost (J_{obs} - J_{opt})');
title('Decomposition: Composition vs Timing');
legend('Composition', 'Timing', 'Location', 'NE');
grid on;

% --- Fig 3: CP share comparison (observed vs CF-A vs CF-B) ---
figure('Name','CP Share','Color','w','Position',[50 50 1200 400]);
[~, si] = sort([cdata.cp_sh_obs], 'descend');
bar_data = [cp_obs_all(si); cp_A_all(si); cp_B_all(si)]';
bh = bar(1:n_c, bar_data, 'grouped');
bh(1).FaceColor = [.5 .5 .5];
bh(2).FaceColor = [.2 .5 .8];
bh(3).FaceColor = [.8 .3 .3];
set(gca, 'XTick', 1:n_c, 'XTickLabel', {cdata(si).iso}, ...
    'XTickLabelRotation', 55, 'FontSize', 6);
ylabel('CP share (%)');
title('CP Share: Observed vs CF-A vs CF-B');
legend('Observed', 'CF-A (composition)', 'CF-B (comp+timing)', ...
    'Location', 'NE', 'FontSize', 7);
grid on;

% --- Fig 4: Selected countries — observed vs CF-A vs CF-B trajectories ---
selected = {'USA','DEU','ITA','GBR','JPN','CHL'};
n_sel = length(selected);
figure('Name','CF Trajectories','Color','w','Position',[30 30 1200 700]);
for s = 1:n_sel
    iso = selected{s};
    idx = find(strcmp({cdata.iso}, iso));

    % Debt trajectories
    subplot(n_sel, 2, (s-1)*2 + 1); hold on;
    plot(1:N, cdata(idx).xs_obs(2,2:end)*100, 'k-s', 'LineWidth', 1.5, 'MarkerSize', 3);
    plot(1:N, cdata(idx).xs_A(2,2:end)*100, 'b--o', 'LineWidth', 1.5, 'MarkerSize', 3);
    plot(1:N, cdata(idx).xs_B(2,2:end)*100, 'r-^', 'LineWidth', 1.5, 'MarkerSize', 3);
    xline(K_act+0.5, ':', 'Color', [.5 .5 .5]);
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 5);
    ylabel('Cum. debt (pp)');  grid on;
    text(0.02, 0.95, iso, 'Units', 'normalized', 'FontSize', 10, ...
        'FontWeight', 'bold', 'VerticalAlignment', 'top');
    if s == 1
        title('Cumulative Debt');
        legend('Observed','CF-A','CF-B', 'FontSize', 5, 'Location', 'SE');
    end

    % Fiscal composition (stacked: CP + DI)
    subplot(n_sel, 2, (s-1)*2 + 2); hold on;
    fcp_o = cdata(idx).FCP(1:K_act)*100;
    fdi_o = cdata(idx).FDI(1:K_act)*100;
    fcp_b = cdata(idx).us_B(1,:)*100;
    fdi_b = cdata(idx).us_B(2,:)*100;
    bh1 = bar(1:K_act, [fcp_o; fdi_o]', 'stacked');
    bh1(1).FaceColor = [.6 .6 .6];  bh1(2).FaceColor = [.8 .8 .8];
    bh2 = bar((1:K_act)+0.35, [fcp_b; fdi_b]', 'stacked', 'BarWidth', 0.3);
    bh2(1).FaceColor = [.8 .2 .2];  bh2(2).FaceColor = [1 .6 .6];
    set(gca, 'XTick', 1:K_act, 'XTickLabel', qlbl(1:K_act), 'FontSize', 5);
    ylabel('% GDP');  grid on;
    if s == 1
        title('Fiscal Composition');
        legend('CP obs','DI obs','CP opt','DI opt', 'FontSize', 5, 'Location', 'NE');
    end
end
sgtitle('Counterfactual: Observed vs CF-A vs CF-B', 'FontWeight', 'bold');

fprintf('\n=== ANALYSIS COMPLETE ===\n');


%% ########################################################################
%  ADDITIONAL FUNCTIONS (CF-B solvers — same as calibration file)
%  ########################################################################

function [A,B] = get_jacobians(x, u, k, S, kcp, P)
    y=x(1); fcp=u(1); Sk=0;
    if k<=length(S), Sk=S(k); end
    A = [P.rho_y+P.psi*Sk-P.eta_p*fcp, 0, P.alpha_F_DI;
         -P.gamma_y, 1+P.r_int, 0;
         0, 0, 0];
    B = [P.alpha_F_CP+P.eta_tilde*Sk-P.eta_p*y, 0;
         kcp, P.kappa_F_DI;
         0, 1];
end

function xn = dynamics_step(x, u, k, S, theta, d, mu_y, mu_b, kcp, P)
    y=x(1); b=x(2); z=x(3); fcp=u(1); fdi=u(2);
    Sk=0; thk=0; dk=0; ey=0;
    if k<=length(S), Sk=S(k); end
    if k<=length(theta), thk=theta(k); end
    if k<=length(d), dk=d(k); end
    if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
    xn = [mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
              + (P.alpha_F_CP+P.eta_tilde*Sk-P.eta_p*y)*fcp ...
              + P.alpha_F_DI*z + P.beta_fear*dk + ey;
          mu_b + (1+P.r_int)*b - P.gamma_y*y ...
              + kcp*fcp + P.kappa_F_DI*fdi + P.c_H*thk;
          fdi];
end

function [us_opt,J_opt] = solve_direct_cfb(fcp_obs,fdi_obs,S,theta,d,mu_y,mu_b,kcp,P,us_ilqr)
    Ka=P.K_act; tot=sum(fcp_obs(1:Ka))+sum(fdi_obs(1:Ka)); pen=1e4;
    obj=@(x) eval_J(max(0,x(1:Ka)),max(0,x(Ka+1:2*Ka)),S,theta,d,mu_y,mu_b,kcp,P) ...
        +pen*(sum(max(0,x(1:Ka)))+sum(max(0,x(Ka+1:2*Ka)))-tot)^2 ...
        +pen*sum(max(0,x(1:Ka)-P.u_hi(1)).^2) ...
        +pen*sum(max(0,x(Ka+1:2*Ka)-P.u_hi(2)).^2) ...
        +pen*sum(max(0,-x).^2);
    opts=optimset('MaxIter',15000,'MaxFunEvals',300000,...
        'TolFun',1e-13,'TolX',1e-12,'Display','off');
    best_J=Inf; best_x=zeros(1,2*Ka);
    inits={[fcp_obs(1:Ka),fdi_obs(1:Ka)], ones(1,2*Ka)*tot/(2*Ka)};
    wb=exp(0.3*(0:Ka-1)); wb=wb/sum(wb); inits{end+1}=[wb*tot*0.9,wb*tot*0.1];
    wf=exp(-0.3*(0:Ka-1)); wf=wf/sum(wf); inits{end+1}=[wf*tot*0.9,wf*tot*0.1];
    ws=zeros(1,Ka); ws(Ka-1)=0.3; ws(Ka)=0.7; inits{end+1}=[ws*tot,zeros(1,Ka)];
    if nargin>=10 && ~isempty(us_ilqr)
        inits{end+1}=[us_ilqr(1,:),us_ilqr(2,:)];
    end
    for t=1:length(inits)
        x0=max(0.0001,inits{t}); sc=tot/sum(x0); x0=x0*sc;
        [xo,Jo]=fminsearch(obj,x0,opts);
        if Jo<best_J, best_J=Jo; best_x=xo; end
    end
    xp=max(0,best_x);
    xp(1:Ka)=min(P.u_hi(1),xp(1:Ka));
    xp(Ka+1:2*Ka)=min(P.u_hi(2),xp(Ka+1:2*Ka));
    sc=tot/max(sum(xp),1e-12); xp=xp*sc;
    us_opt=[xp(1:Ka); xp(Ka+1:2*Ka)];
    J_opt=eval_J(us_opt(1,:),us_opt(2,:),S,theta,d,mu_y,mu_b,kcp,P);
end

function [us_opt,J_opt] = solve_ilqr_cfb(fcp_obs,fdi_obs,S,theta,d,mu_y,mu_b,kcp,P)
    N_=P.N; Ka=P.K_act; nx_=P.nx; nu_=P.nu;
    tot=sum(fcp_obs(1:Ka))+sum(fdi_obs(1:Ka)); gc=[1;1];
    Qm=diag([P.w_y,P.w_b,0]); Rm=diag([P.r_cp,P.r_di]); Qf=diag([0,P.W_b,0]);
    us_f=zeros(nu_,N_);
    us_f(:,1:Ka)=[max(0.001,min(0.19,fcp_obs(1:Ka)));...
                  max(0.001,min(0.09,fdi_obs(1:Ka)))];
    xs=zeros(nx_,N_+1);
    for k=1:N_
        xs(:,k+1)=dynamics_step(xs(:,k),us_f(:,k),k,S,theta,d,mu_y,mu_b,kcp,P);
    end
    lam_sum=0; mu_sum=50; max_outer=30; max_inner=100; reg=1e-6;
    for outer=1:max_outer
        for inner=1:max_inner
            sv=sum(us_f(1,1:Ka))+sum(us_f(2,1:Ka))-tot;
            Vxx=P.beta_disc^(N_-1)*Qm+P.beta_disc^N_*Qf;
            Vx=Vxx*xs(:,N_+1);
            Ks=zeros(nu_,nx_,N_); ds=zeros(nu_,N_); bw_ok=true;
            for k=N_:-1:1
                bk=P.beta_disc^(k-1);
                [Ak,Bk]=get_jacobians(xs(:,k),us_f(:,k),k,S,kcp,P);
                Qxx_k=Ak'*Vxx*Ak; qx_k=Ak'*Vx;
                if k<=Ka
                    Quu_k=bk*Rm+Bk'*Vxx*Bk+mu_sum*(gc*gc')+reg*eye(nu_);
                    Qux_k=Bk'*Vxx*Ak;
                    qu_k=bk*Rm*us_f(:,k)+Bk'*Vx+(lam_sum+mu_sum*sv)*gc;
                    [~,pd]=chol(Quu_k);
                    if pd>0, bw_ok=false; break; end
                    Ki=Quu_k\Qux_k; di=-Quu_k\qu_k;
                    Ks(:,:,k)=Ki; ds(:,k)=di;
                    Vn=Qxx_k-Ki'*Quu_k*Ki;
                    vn=qx_k-Ki'*Quu_k*di;
                else
                    Vn=Qxx_k; vn=qx_k;
                end
                if k>1
                    Vxx=P.beta_disc^(k-2)*Qm+Vn;
                    Vx=P.beta_disc^(k-2)*Qm*xs(:,k)+vn;
                else
                    Vxx=Vn; Vx=vn;
                end
            end
            if ~bw_ok, reg=reg*10; if reg>1e8, break; end; continue; end
            J_old=eval_J(us_f(1,1:Ka),us_f(2,1:Ka),S,theta,d,mu_y,mu_b,kcp,P)...
                +lam_sum*sv+0.5*mu_sum*sv^2;
            al=1.0; acc=false;
            while al>1e-10
                xn=zeros(nx_,N_+1); un=zeros(nu_,N_);
                for k=1:N_
                    if k<=Ka
                        dx=xn(:,k)-xs(:,k);
                        un(:,k)=us_f(:,k)+al*ds(:,k)-Ks(:,:,k)*dx;
                        un(:,k)=max(P.u_lo,min(P.u_hi,un(:,k)));
                    end
                    xn(:,k+1)=dynamics_step(xn(:,k),un(:,k),k,S,theta,d,mu_y,mu_b,kcp,P);
                end
                svn=sum(un(1,1:Ka))+sum(un(2,1:Ka))-tot;
                Jn=eval_J(un(1,1:Ka),un(2,1:Ka),S,theta,d,mu_y,mu_b,kcp,P)...
                    +lam_sum*svn+0.5*mu_sum*svn^2;
                if Jn<J_old-1e-12, acc=true; break; end
                al=al*0.5;
            end
            if ~acc, reg=reg*10; if reg>1e8, break; end; continue; end
            dn=norm(un(:,1:Ka)-us_f(:,1:Ka),'fro');
            xs=xn; us_f=un; reg=max(reg*0.5,1e-8);
            if dn<1e-9, break; end
        end
        sv=sum(us_f(1,1:Ka))+sum(us_f(2,1:Ka))-tot;
        lam_sum=lam_sum+mu_sum*sv; mu_sum=mu_sum*2;
        if abs(sv)<1e-8, break; end
    end
    act=us_f(:,1:Ka);
    sc=tot/max(sum(act(:)),1e-12);
    act=act*sc; act=max(P.u_lo,min(P.u_hi,act));
    us_opt=act;
    J_opt=eval_J(us_opt(1,:),us_opt(2,:),S,theta,d,mu_y,mu_b,kcp,P);
end

%% ========================================================================
%  SENSITIVITY ANALYSIS
%
%  Tests robustness of counterfactual results to:
%  (1) Asymmetric tail penalty: w_tail * max(0, -y - y_bar)^2
%  (2) eta_p at upper CI bound (3.5 vs 2.6)
%  (3) Both combined
%
%  All four specs use the same solvers, data, and constraints.
% =========================================================================
fprintf('\n========================================\n');
fprintf('  SENSITIVITY ANALYSIS\n');
fprintf('========================================\n\n');

% --- Define specifications ---
% P_sens(s) = modified P struct for each spec
sens_names = {'Baseline', 'Tail (w=500)', 'eta_p=3.5', 'Both'};
n_sens = length(sens_names);

P_sens = cell(1, n_sens);
for s = 1:n_sens
    Ps = P;
    Ps.w_tail = 0;
    Ps.y_tail = -0.05;    % -5pp threshold
    if s == 2 || s == 4
        Ps.w_tail = 500;  % tail penalty active
    end
    if s == 3 || s == 4
        Ps.eta_p = 3.5;   % upper CI
    end
    P_sens{s} = Ps;
end

% --- Results storage ---
sens_res = struct();
for s = 1:n_sens
    sens_res(s).name = sens_names{s};
    sens_res(s).cpA = zeros(n_c, 1);
    sens_res(s).dbA = zeros(n_c, 1);
    sens_res(s).dyA = zeros(n_c, 1);
    sens_res(s).cpB = zeros(n_c, 1);
    sens_res(s).dbB = zeros(n_c, 1);
    sens_res(s).dyB = zeros(n_c, 1);
    sens_res(s).dJA = zeros(n_c, 1);
    sens_res(s).dJB = zeros(n_c, 1);
end

% --- Run all specs ---
for s = 1:n_sens
    Ps = P_sens{s};
    fprintf('  [%d/%d] %s ...', s, n_sens, sens_names{s});

    for i = 1:n_c
        S_i  = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
        my_i = cdata(i).mu_y;   mb_i = cdata(i).mu_b;    kc_i = cdata(i).kappa_cp;
        fcp_obs = cdata(i).FCP;  fdi_obs = cdata(i).FDI;
        F_total = fcp_obs(1:K_act) + fdi_obs(1:K_act);

        % J_obs
        J_obs = eval_J_sens(fcp_obs(1:K_act), fdi_obs(1:K_act), ...
            S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);

        % --- CF-A ---
        obj_A = @(w) eval_omega_sens(w, F_total, fcp_obs, fdi_obs, ...
            S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);
        w0 = fcp_obs(1:K_act) ./ max(F_total, 1e-10);
        opts_a = optimset('MaxIter',5000,'MaxFunEvals',50000,...
            'TolFun',1e-13,'TolX',1e-12,'Display','off');
        [omega_opt, ~] = fminsearch(obj_A, w0, opts_a);

        omega_opt = max(0, min(1, omega_opt));
        for kk = 1:K_act
            if F_total(kk) < 1e-8, omega_opt(kk) = 0; continue; end
            omega_opt(kk) = max(omega_opt(kk), 1 - P.u_hi(2)/F_total(kk));
            omega_opt(kk) = min(omega_opt(kk), P.u_hi(1)/F_total(kk));
        end
        fcp_A = omega_opt .* F_total;
        fdi_A = (1 - omega_opt) .* F_total;
        J_A = eval_J_sens(fcp_A, fdi_A, S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);

        fcp_Af = [fcp_A, zeros(1, N-K_act)];
        fdi_Af = [fdi_A, zeros(1, N-K_act)];
        xs_obs = forward_roll(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);
        xs_A   = forward_roll(fcp_Af, fdi_Af, S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);

        sens_res(s).cpA(i) = sum(fcp_A) / max(sum(F_total), 1e-10) * 100;
        sens_res(s).dbA(i) = (xs_A(2,N+1) - xs_obs(2,N+1)) * 100;
        sens_res(s).dyA(i) = (xs_A(1,N+1) - xs_obs(1,N+1)) * 100;
        sens_res(s).dJA(i) = J_obs - J_A;

        % --- CF-B ---
        [us_B, J_B] = solve_direct_cfb_sens(fcp_obs, fdi_obs, ...
            S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);

        fcp_Bf = [us_B(1,:), zeros(1, N-K_act)];
        fdi_Bf = [us_B(2,:), zeros(1, N-K_act)];
        xs_B = forward_roll(fcp_Bf, fdi_Bf, S_i, th_i, d_i, my_i, mb_i, kc_i, Ps);

        sens_res(s).cpB(i) = sum(us_B(1,:)) / max(sum(us_B(:)), 1e-10) * 100;
        sens_res(s).dbB(i) = (xs_B(2,N+1) - xs_obs(2,N+1)) * 100;
        sens_res(s).dyB(i) = (xs_B(1,N+1) - xs_obs(1,N+1)) * 100;
        sens_res(s).dJB(i) = J_obs - J_B;
    end

    n_pareto = sum(sens_res(s).dyB >= -0.01 & sens_res(s).dbB <= 0.01);
    fprintf(' Pareto: %d/%d  CP_B: %.0f%%  db_B: %+.2f pp\n', ...
        n_pareto, n_c, mean(sens_res(s).cpB), mean(sens_res(s).dbB));
end


%% --- Comparison Table ---
fprintf('\n  %-28s', '');
for s = 1:n_sens, fprintf(' %14s', sens_names{s}); end
fprintf('\n  %s', repmat('-', 1, 28));
for s = 1:n_sens, fprintf(' %s', repmat('-', 1, 14)); end
fprintf('\n');

metrics = {'CF-A: CP opt (mean %%)',   @(s) mean(sens_res(s).cpA);
           'CF-A: db (mean pp)',       @(s) mean(sens_res(s).dbA);
           'CF-B: CP opt (mean %%)',   @(s) mean(sens_res(s).cpB);
           'CF-B: dy (mean pp)',       @(s) mean(sens_res(s).dyB);
           'CF-B: db (mean pp)',       @(s) mean(sens_res(s).dbB);
           'CF-B: Pareto',             @(s) sum(sens_res(s).dyB >= -0.01 & sens_res(s).dbB <= 0.01)};

for m = 1:size(metrics, 1)
    fprintf('  %-28s', metrics{m,1});
    for s = 1:n_sens
        val = metrics{m,2}(s);
        if contains(metrics{m,1}, 'Pareto')
            fprintf(' %14.0f', val);
        elseif contains(metrics{m,1}, '%%')
            fprintf(' %13.0f%%', val);
        else
            fprintf(' %+14.2f', val);
        end
    end
    fprintf('\n');
end

% --- Per-country CP share ---
fprintf('\n  CF-B optimal CP share (%%):\n');
fprintf('  %5s %5s', 'ISO', 'Obs');
for s = 1:n_sens, fprintf(' %12s', sens_names{s}); end
fprintf('\n');
sel_iso = {'USA','DEU','ITA','GBR','JPN','GRC','CHL','AUS','FRA','ESP','CZE','SVN'};
for j = 1:length(sel_iso)
    idx = find(strcmp({cdata.iso}, sel_iso{j}));
    if isempty(idx), continue; end
    fprintf('  %5s %4.0f%%', sel_iso{j}, cdata(idx).cp_sh_obs*100);
    for s = 1:n_sens
        fprintf(' %11.0f%%', sens_res(s).cpB(idx));
    end
    fprintf('\n');
end

% --- Pareto status changes ---
fprintf('\n  Pareto status changes:\n');
fprintf('  %5s', 'ISO');
for s = 1:n_sens, fprintf(' %12s', sens_names{s}); end
fprintf('\n');
for i = 1:n_c
    flags = zeros(1, n_sens);
    for s = 1:n_sens
        flags(s) = (sens_res(s).dyB(i) >= -0.01) & (sens_res(s).dbB(i) <= 0.01);
    end
    if min(flags) ~= max(flags)  % status changes
        fprintf('  %5s', cdata(i).iso);
        for s = 1:n_sens
            if flags(s), fprintf(' %12s', 'YES'); else, fprintf(' %12s', 'no'); end
        end
        fprintf('\n');
    end
end

% --- Save ---
T_sens = table();
T_sens.Country = {cdata.iso}';
for s = 1:n_sens
    sn = strrep(sens_names{s}, ' ', '_');
    sn = strrep(sn, '(', ''); sn = strrep(sn, ')', '');
    sn = strrep(sn, '=', '');
    T_sens.(['cpB_' sn]) = sens_res(s).cpB;
    T_sens.(['dbB_' sn]) = sens_res(s).dbB;
    T_sens.(['dyB_' sn]) = sens_res(s).dyB;
end
writetable(T_sens, 'sensitivity_results.csv');
fprintf('\n  Saved: sensitivity_results.csv\n');

fprintf('\n=== SENSITIVITY COMPLETE ===\n');


%% ########################################################################
%  SENSITIVITY-SPECIFIC FUNCTIONS
%  (eval_J with tail penalty, solve_direct with tail penalty)
%  ########################################################################

function J = eval_J_sens(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
    fcp_f = [fcp, zeros(1, P.N-length(fcp))];
    fdi_f = [fdi, zeros(1, P.N-length(fdi))];
    xs = forward_roll(fcp_f, fdi_f, S, theta, d, mu_y, mu_b, kcp, P);
    N_ = P.N; J = 0;
    for k = 1:N_
        bd = P.beta_disc^(k-1);
        J = J + bd*0.5*(P.w_y*xs(1,k+1)^2 + P.w_b*xs(2,k+1)^2);
        fk = 0; gk = 0;
        if k <= length(fcp), fk = fcp(k); gk = fdi(k); end
        J = J + bd*0.5*(P.r_cp*fk^2 + P.r_di*gk^2);
        % Tail penalty
        if isfield(P,'w_tail') && P.w_tail > 0 && xs(1,k+1) < P.y_tail
            J = J + bd*0.5*P.w_tail*(xs(1,k+1) - P.y_tail)^2;
        end
    end
    J = J + P.beta_disc^N_ * 0.5 * P.W_b * xs(2,N_+1)^2;
end

function J = eval_omega_sens(omega, F_total, fcp_obs, fdi_obs, S, theta, d, mu_y, mu_b, kcp, P)
    Ka = P.K_act;
    fcp = fcp_obs(1:Ka);  fdi = fdi_obs(1:Ka);
    for kk = 1:Ka
        if F_total(kk) < 1e-8, continue; end
        w = max(0, min(1, omega(kk)));
        fcp(kk) = w * F_total(kk);
        fdi(kk) = (1-w) * F_total(kk);
        fcp(kk) = min(P.u_hi(1), max(0, fcp(kk)));
        fdi(kk) = min(P.u_hi(2), max(0, fdi(kk)));
    end
    J = eval_J_sens(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P);
end

function [us_opt, J_opt] = solve_direct_cfb_sens(fcp_obs, fdi_obs, S, theta, d, mu_y, mu_b, kcp, P)
    Ka = P.K_act;
    tot = sum(fcp_obs(1:Ka)) + sum(fdi_obs(1:Ka));
    pen = 1e4;

    obj = @(x) eval_J_sens(max(0,x(1:Ka)), max(0,x(Ka+1:2*Ka)), ...
        S, theta, d, mu_y, mu_b, kcp, P) ...
        + pen*(sum(max(0,x(1:Ka)))+sum(max(0,x(Ka+1:2*Ka)))-tot)^2 ...
        + pen*sum(max(0, x(1:Ka)-P.u_hi(1)).^2) ...
        + pen*sum(max(0, x(Ka+1:2*Ka)-P.u_hi(2)).^2) ...
        + pen*sum(max(0, -x).^2);

    opts = optimset('MaxIter',15000,'MaxFunEvals',300000,...
        'TolFun',1e-13,'TolX',1e-12,'Display','off');

    best_J = Inf; best_x = zeros(1, 2*Ka);
    inits = {[fcp_obs(1:Ka), fdi_obs(1:Ka)], ...
             ones(1, 2*Ka)*tot/(2*Ka)};
    wb = exp(0.3*(0:Ka-1)); wb = wb/sum(wb);
    inits{end+1} = [wb*tot*0.9, wb*tot*0.1];
    wf = exp(-0.3*(0:Ka-1)); wf = wf/sum(wf);
    inits{end+1} = [wf*tot*0.9, wf*tot*0.1];

    for t = 1:length(inits)
        x0 = max(0.0001, inits{t});
        sc = tot/sum(x0); x0 = x0*sc;
        [xo, Jo] = fminsearch(obj, x0, opts);
        if Jo < best_J, best_J = Jo; best_x = xo; end
    end

    xp = max(0, best_x);
    xp(1:Ka) = min(P.u_hi(1), xp(1:Ka));
    xp(Ka+1:2*Ka) = min(P.u_hi(2), xp(Ka+1:2*Ka));
    sc = tot / max(sum(xp), 1e-12); xp = xp*sc;

    us_opt = [xp(1:Ka); xp(Ka+1:2*Ka)];
    J_opt = eval_J_sens(us_opt(1,:), us_opt(2,:), S, theta, d, mu_y, mu_b, kcp, P);
end