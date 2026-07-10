%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & SOLVER VERIFICATION (V2)
%
%  SPECIFICATION CHANGE (V2):
%    CP and DI both operate at lag 2 — no contemporaneous output effect,
%    no S×F or y×F interactions. CP preserves capacity; the output effect
%    materializes two quarters after deployment when lockdowns ease.
%
%  Horizon: N = 10 quarters (Q1.2020 – Q2.2022)
%    k = 1..K_act  (Q1.2020 – Q4.2021)  active pandemic, F available
%    k = K_act+1..N (Q1.2022 – Q2.2022) post-trilemma tail, F = 0
%
%  State:   x = (y, b, w, z)',  w = F^CP_{k-1},  z = F^DI_{k-1}
%  Control: u = (F^CP, F^DI)'  for k <= K_act;  u = 0  for k > K_act
%
%  Output transition (V2, simplified):
%    y_{k+1} = mu_y + rho_y*y_k + (psi*y_k - alpha_S)*S_k
%              + alpha_CP*w_k + alpha_DI*z_k + beta_d*d_k + eps_k
%
%    Key: Controls do NOT enter y contemporaneously.
%         u_k → w_{k+1}/z_{k+1} → y_{k+2}  (lag-2 transmission)
%
%  Debt transition (unchanged):
%    b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%              + kappa_cp*F^CP_k + kappa_di*F^DI_k + c_H*theta_k
%
%  Country-specific parameters:
%    kappa_i  — realized debt cost of CP (from CP subcomponent weights)
%
%  Regression: feols, Q4.2019–Q2.2022 (N=418), Country + Quarter FE
%  SE: Clustered by Country (AER standard)
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V2 (lag-2 CP) ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS
% =========================================================================

% --- Output equation (V2: lag-2 CP/DI, no interactions) ---
rho_y      = 0.4139;       % beta(y_lag1)
psi        = 0.1357;       % beta(S*y_lag1) in fraction units
alpha_S    = 0.0302;       % -beta(S), stored positive
alpha_F_CP = 0.0712;       % beta(FCP_lag2) — lag-2 effect
alpha_F_DI = 0.2067;       % beta(FDI_lag2) — lag-2 effect
beta_fear  = -0.0233;      % beta(d)

% --- Debt equation (unchanged from V1) ---
r_int      = 0.001;
gamma_y    = 0.200;        % -beta(y) in debt eq, updated for new sample
kappa_F_DI = 0.464;        % beta(FDI_lag1) in debt eq

% No c_H in V2 (H reclassified into DI)
c_H        = 0.00;

% --- Country-specific kappa_CP (from debt eq, unchanged) ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};
kappa_cp_i = [0.3908, 0.3548, 0.2079, 0.2952, 0.2425, 0.1903, 0.2290, 0.2723, ...
              0.1757, 0.2085, 0.3273, 0.1973, 0.2982, 0.2182, 0.2081, 0.2412, ...
              0.3514, 0.2611, 0.3521, 0.3465, 0.3243, 0.1970, 0.2602, 0.2864, ...
              0.3250, 0.2506, 0.2764, 0.3195, 0.2994, 0.2328, 0.3647, 0.3157, ...
              0.2030, 0.3968, 0.3290, 0.2735, 0.1727, 0.3257];

% --- Quarter FE (V2, Q1.2020–Q2.2022, units: pp -> /100) ---
% Estimated jointly with Q4.2019 in sample; Q4.2019 FE absorbed into IC
qfe_pp = [-1.4084, -9.1511, 1.2090, -0.7543, 0.2312, ...
            0.5744,  0.5501, 1.0630, -0.0763, -0.4338];

% --- Country FE: Output (V2, pp -> /100) ---
cfe_y_val = [ 0.5433, -0.9880,  0.3353, -0.5562,  1.0078,  0.4200,  1.3329, -0.2332, ...
             -2.4543, -0.9374,  0.3127, -3.3921, -0.9471, -0.9378, -1.1469, -2.0486, ...
             -0.1647, -1.4731,  5.4165, -3.5552,  0.8822, -0.0273, -0.6526,  0.3073, ...
              0.5006,  1.2701, -0.2714, -2.0624,  0.4749,  0.7898, -0.7133, -0.5545, ...
             -2.1535, -0.1310, -1.2752,  0.6456,  3.1395,  0.6931];

% --- Country FE: Debt (unchanged from V1) ---
cfe_b_val = [-0.6746, -0.2777, -0.4649, -0.2143, -0.4844, -0.9781,  0.1462,  0.4048, ...
             -1.0619, -1.0287, -1.2539, -1.2272, -0.5830, -0.5348, -0.8409, -0.8974, ...
             -0.6096, -0.6704,  0.9444, -1.0335,  0.2331, -0.6479, -0.2022, -0.2991, ...
             -0.5080,  0.2937, -0.0212, -1.6298, -0.4222, -0.4402,  0.3454, -0.6348, ...
             -1.4319, -0.0641, -0.4026, -0.3780,  0.3130,  0.6681];

% --- Objective weights ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions (V2: nx=4) ---
N = 10;  K_act = 8;  nx = 4;  nu = 2;

% --- Build eps_y vector (fractions) ---
eps_y_vec = zeros(1, N+1);
for k = 1:N
    if k <= length(qfe_pp), eps_y_vec(k+1) = qfe_pp(k) / 100; end
end

% --- Build lookup maps (fractions) ---
cfe_y_map  = containers.Map(cfe_iso, cfe_y_val / 100);
cfe_b_map  = containers.Map(cfe_iso, cfe_b_val / 100);
kappa_map  = containers.Map(cfe_iso, kappa_cp_i);

% --- Control bounds ---
u_lo = [0; 0];  u_hi = [0.20; 0.10];

% --- Pack parameters ---
P = struct( ...
    'rho_y',rho_y, 'psi',psi, 'alpha_S',alpha_S, ...
    'alpha_F_CP',alpha_F_CP, 'alpha_F_DI',alpha_F_DI, ...
    'beta_fear',beta_fear, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_F_DI',kappa_F_DI, 'c_H',c_H, ...
    'eps_y_vec',eps_y_vec, 'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   nx = %d (V2: y,b,w,z)\n', N, K_act, nx);
fprintf('  rho_y = %.3f   psi = %.3f   alpha_S = %.3f\n', rho_y, psi, alpha_S);
fprintf('  alpha_CP = %.4f (lag-2)   alpha_DI = %.4f (lag-2)\n', alpha_F_CP, alpha_F_DI);
fprintf('  beta_d = %.4f\n', beta_fear);
fprintf('  gamma_y = %.3f   kappa_DI = %.3f\n', gamma_y, kappa_F_DI);
fprintf('  kappa_CP: mean=%.3f  range=[%.3f, %.3f]\n\n', ...
    mean(kappa_cp_i), min(kappa_cp_i), max(kappa_cp_i));


%% ========================================================================
%  LOAD DATA
% =========================================================================
fprintf('--- Loading data ---\n');
T = readtable('country_data_for_matlab.csv');
qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020', ...
        'Q1.2021','Q2.2021','Q3.2021','Q4.2021', ...
        'Q1.2022','Q2.2022'};
qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20', ...
        'Q1.21','Q2.21','Q3.21','Q4.21', ...
        'Q1.22','Q2.22'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S=zeros(1,N); cdata(i).FCP=zeros(1,N); cdata(i).FDI=zeros(1,N);
    cdata(i).y=zeros(1,N); cdata(i).theta=zeros(1,N); cdata(i).b=zeros(1,N);
    cdata(i).d=zeros(1,N);
    cdata(i).mu_y = 0;  cdata(i).mu_b = 0;  cdata(i).kappa_cp = 0.277;
    if isKey(cfe_y_map, iso), cdata(i).mu_y = cfe_y_map(iso); end
    if isKey(cfe_b_map, iso), cdata(i).mu_b = cfe_b_map(iso); end
    if isKey(kappa_map,  iso), cdata(i).kappa_cp = kappa_map(iso); end

    for k = 1:N
        if k > length(qord), break; end
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k) = row.S_mean_tw / 100;
        cdata(i).FCP(k) = row.F_CP / 100;
        cdata(i).FDI(k) = row.F_DI / 100;
        cdata(i).y(k) = row.y_t_pct / 100;
        cdata(i).theta(k) = row.theta_pct / 100;
        if ~ismissing(row.debt_dR), cdata(i).b(k) = row.debt_dR/100; end
        if ismember('excess_mortality',T.Properties.VariableNames) ...
                && ~ismissing(row.excess_mortality)
            cdata(i).d(k) = row.excess_mortality / 100;
        end
    end
end
fprintf('  %d countries x %d quarters (K_act = %d)\n\n', n_c, N, K_act);


%% ========================================================================
%  STEP 1: VALIDATION (forward roll with observed controls)
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation (N = %d, nx = %d)\n', N, nx);
fprintf('========================================\n');

for i = 1:n_c
    xs = forward_roll(cdata(i).FCP, cdata(i).FDI, ...
        cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, cdata(i).kappa_cp, P);
    cdata(i).sim_y     = xs(1, 2:end);
    cdata(i).sim_b     = xs(2, 2:end);
    cdata(i).obs_b_cum = cumsum(cdata(i).b);
    cdata(i).rmse_y = sqrt(mean((cdata(i).sim_y(1:K_act) - cdata(i).y(1:K_act)).^2))*100;
    cdata(i).rmse_b = sqrt(mean((cdata(i).sim_b(1:K_act) - cdata(i).obs_b_cum(1:K_act)).^2))*100;
end

fprintf('  Output RMSE (k=1:%d) — Median: %.2f pp   Mean: %.2f pp\n', ...
    K_act, median([cdata.rmse_y]), mean([cdata.rmse_y]));
fprintf('  Debt   RMSE (k=1:%d) — Median: %.2f pp   Mean: %.2f pp\n\n', ...
    K_act, median([cdata.rmse_b]), mean([cdata.rmse_b]));

% --- Trajectory comparison ---
fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s\n', 'Quarter', 'Observed', 'Model');
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata) * 100;
    sim_k = arrayfun(@(c) c.sim_y(k), cdata) * 100;
    fprintf('  %8s %+9.2f %+9.2f\n', qlbl{k}, median(obs_k), median(sim_k));
end

% --- Fiscal contribution: observed vs no-fiscal ---
fprintf('\n  Fiscal contribution (Full - NoF, cum %dQ):\n', K_act);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), zeros(1,N), ...
        cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, cdata(i).kappa_cp, P);
    diffs(i) = (sum(cdata(i).sim_y(1:K_act)) - sum(xs_nof(1,2:K_act+1))) * 100;
end
fprintf('    Mean:   %+.2f pp\n', mean(diffs));
fprintf('    Median: %+.2f pp\n', median(diffs));
fprintf('    F > 0:  %d / %d\n', sum(diffs > 0), n_c);


%% ========================================================================
%  STEP 1b: NON-TARGETED MOMENTS
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1b: Non-Targeted Moments\n');
fprintf('========================================\n');

% --- (1) Cross-Country SD ---
fprintf('\n  (1) Cross-Country SD of Output Gap:\n');
fprintf('  %10s %8s %8s %8s\n', 'Quarter', 'SD_obs', 'SD_sim', 'Ratio');
sd_ratios = zeros(1, K_act);
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata) * 100;
    sim_k = arrayfun(@(c) c.sim_y(k), cdata) * 100;
    sd_obs = std(obs_k);  sd_sim = std(sim_k);
    sd_ratios(k) = sd_sim / max(sd_obs, 1e-10);
    fprintf('  %10s %8.2f %8.2f %8.3f\n', qlbl{k}, sd_obs, sd_sim, sd_ratios(k));
end
fprintf('  %10s %8s %8s %8.3f\n', 'Mean', '', '', mean(sd_ratios));

% --- (2) Within-Country Autocorrelation ---
fprintf('\n  (2) Within-Country Autocorrelation:\n');
ac1_obs = zeros(n_c,1);  ac1_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_act);  ys = cdata(i).sim_y(1:K_act);
    co1 = corrcoef(yo(1:end-1), yo(2:end));  ac1_obs(i) = co1(1,2);
    cs1 = corrcoef(ys(1:end-1), ys(2:end));  ac1_sim(i) = cs1(1,2);
end
fprintf('  %20s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %20s %10.3f %10.3f\n', 'AC(1) mean', mean(ac1_obs), mean(ac1_sim));

% --- (3) ICC ---
y_obs_p = reshape([cdata.y], N, n_c)' * 100;
y_sim_p = reshape([cdata.sim_y], N, n_c)' * 100;
icc_y_obs = var(mean(y_obs_p(:,1:K_act),2)) / var(y_obs_p(:,1:K_act),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_act),2)) / var(y_sim_p(:,1:K_act),0,'all');
b_obs_p = zeros(n_c, K_act);
b_sim_p = reshape([cdata.sim_b], N, n_c)';
for i = 1:n_c, b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_act)*100; end
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p(:,1:K_act)*100,2)) / var(b_sim_p(:,1:K_act)*100,0,'all');
fprintf('\n  (3) ICC:\n');
fprintf('  %20s %10.3f %10.3f\n', 'ICC output', icc_y_obs, icc_y_sim);
fprintf('  %20s %10.3f %10.3f\n', 'ICC debt', icc_b_obs, icc_b_sim);


%% ========================================================================
%  STEP 2-3: CF-B — DIRECT SHOOTING vs iLQR
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 2-3: CF-B  (K_act = %d, N = %d, nx = %d)\n', K_act, N, nx);
fprintf('========================================\n\n');

res = struct();
for i = 1:n_c
    iso  = cdata(i).iso;
    S_i  = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i = cdata(i).mu_y;   mb_i = cdata(i).mu_b;    kc_i = cdata(i).kappa_cp;
    fcp_obs = cdata(i).FCP;  fdi_obs = cdata(i).FDI;

    [us_il, J_il] = solve_ilqr_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    [us_ds, J_ds] = solve_direct_cfb(fcp_obs, fdi_obs, S_i, th_i, d_i, my_i, mb_i, kc_i, P, us_il);

    fcp_ds_f = [us_ds(1,:), zeros(1, N-K_act)];
    fdi_ds_f = [us_ds(2,:), zeros(1, N-K_act)];
    xs_ds = forward_roll(fcp_ds_f, fdi_ds_f, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    res(i).iso    = iso;
    res(i).J_ds   = J_ds;       res(i).J_il   = J_il;
    res(i).dJ     = abs(J_ds - J_il);
    res(i).b_ds   = xs_ds(2, N+1)*100;
    res(i).db     = abs(xs_ds(2,N+1))*100;
    res(i).us_ds  = us_ds;      res(i).us_il  = us_il;
    res(i).xs_ds  = xs_ds;

    if res(i).dJ < 0.05, flag='OK'; else, flag='WARN'; end
    fprintf('  [%2d/%d] %s  |dJ|=%.2e  b_N=%.2fpp  k=%.3f  %s\n', ...
        i, n_c, iso, res(i).dJ, res(i).b_ds, kc_i, flag);
end

%% ========================================================================
%  STEP 4: VERIFICATION SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 4: Verification Summary\n');
fprintf('========================================\n');

dJs = [res.dJ];
n_conv = sum(dJs < 0.05);
fprintf('\n  |dJ| — Mean: %.2e   Max: %.2e\n', mean(dJs), max(dJs));
fprintf('  Converged: %d / %d\n', n_conv, n_c);

Tout = table({res.iso}', [res.J_ds]', [res.J_il]', [res.dJ]', [res.b_ds]', ...
    'VariableNames', {'Country','J_direct','J_ilqr','abs_dJ','b_N_pp'});
writetable(Tout, 'verification_results_v2.csv');
fprintf('\n  Saved: verification_results_v2.csv\n');


%% ========================================================================
%  STEP 5: VISUALIZATION
% =========================================================================

% --- Fig 1: Validation — Output Gap and Cumulative Debt ---
sim_y_all = reshape([cdata.sim_y], N, n_c)' * 100;
obs_y_all = reshape([cdata.y],    N, n_c)' * 100;
sim_b_all = reshape([cdata.sim_b], N, n_c)' * 100;
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum * 100; end

figure('Name','Validation V2','Color','w','Position',[50 50 1000 400]);

subplot(1,2,1); hold on;
fill_iqr(1:N, sim_y_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_y_all, [.5 .5 .5], .12);
plot(1:N, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act+.5, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title('Output Gap (V2: lag-2 CP)');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.05, sprintf('RMSE = %.2f pp', median([cdata.rmse_y])), ...
    'Units','normalized','FontSize',8,'BackgroundColor','w');

subplot(1,2,2); hold on;
fill_iqr(1:N, sim_b_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_b_all, [.5 .5 .5], .12);
plot(1:N, median(sim_b_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_b_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act+.5, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Cumulative Debt');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);
text(0.02, 0.95, sprintf('RMSE = %.2f pp', median([cdata.rmse_b])), ...
    'Units','normalized','FontSize',8,'VerticalAlignment','top','BackgroundColor','w');

sgtitle('V2 Validation: Simulated vs Observed','FontWeight','bold');

fprintf('\n=== COMPLETE ===\n');


%% ########################################################################
%  FUNCTIONS (V2: nx=4)
%  ########################################################################

function out = iff(cond, a, b)
    if cond, out = a; else, out = b; end
end

function xs = forward_roll(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
% V2: x = (y, b, w, z),  w = FCP_{k-1},  z = FDI_{k-1}
% Controls affect y only through lagged states w and z.
    N_=P.N; xs=zeros(P.nx, N_+1);
    for k=1:N_
        y=xs(1,k); b=xs(2,k); w=xs(3,k); z=xs(4,k);
        fk=0; gk=0; Sk=0; thk=0; dk=0; ey=0;
        if k<=length(fcp), fk=fcp(k); end
        if k<=length(fdi), gk=fdi(k); end
        if k<=length(S), Sk=S(k); end
        if k<=length(theta), thk=theta(k); end
        if k<=length(d), dk=d(k); end
        if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
        % Output: lag-2 transmission via w and z
        xs(1,k+1) = mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
            + P.alpha_F_CP*w + P.alpha_F_DI*z + P.beta_fear*dk + ey;
        % Debt: contemporaneous fiscal cost
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
            + kcp*fk + P.kappa_F_DI*gk + P.c_H*thk;
        % Lagged states
        xs(3,k+1) = fk;   % w_{k+1} = FCP_k
        xs(4,k+1) = gk;   % z_{k+1} = FDI_k
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

function [A,B] = get_jacobians(x, u, k, S, kcp, P)
% V2 Jacobians: x=(y,b,w,z), u=(fcp,fdi)
%   dy/dy = rho_y + psi*S_k    dy/db = 0    dy/dw = alpha_CP    dy/dz = alpha_DI
%   db/dy = -gamma_y            db/db = 1+r  db/dw = 0           db/dz = 0
%   dw/d* = 0                   dz/d* = 0
%
%   dy/dfcp = 0 (!)   dy/dfdi = 0 (!)   — controls don't affect y contemporaneously
%   db/dfcp = kcp     db/dfdi = kappa_DI
%   dw/dfcp = 1       dw/dfdi = 0
%   dz/dfcp = 0       dz/dfdi = 1
    Sk=0;
    if k<=length(S), Sk=S(k); end
    A = [P.rho_y+P.psi*Sk,  0,         P.alpha_F_CP,  P.alpha_F_DI;
         -P.gamma_y,         1+P.r_int, 0,             0;
         0,                  0,         0,             0;
         0,                  0,         0,             0];
    B = [0,    0;
         kcp,  P.kappa_F_DI;
         1,    0;
         0,    1];
end

function xn = dynamics_step(x, u, k, S, theta, d, mu_y, mu_b, kcp, P)
% V2: x=(y,b,w,z), u=(fcp,fdi)
    y=x(1); b=x(2); w=x(3); z=x(4); fcp=u(1); fdi=u(2);
    Sk=0; thk=0; dk=0; ey=0;
    if k<=length(S), Sk=S(k); end
    if k<=length(theta), thk=theta(k); end
    if k<=length(d), dk=d(k); end
    if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
    xn = [mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
              + P.alpha_F_CP*w + P.alpha_F_DI*z + P.beta_fear*dk + ey;
          mu_b + (1+P.r_int)*b - P.gamma_y*y ...
              + kcp*fcp + P.kappa_F_DI*fdi + P.c_H*thk;
          fcp;
          fdi];
end

function [us_opt,J_opt] = solve_direct_cfb(fcp_obs,fdi_obs,S,theta,d,mu_y,mu_b,kcp,P,us_ilqr)
    Ka=P.K_act; tot=sum(fcp_obs(1:Ka))+sum(fdi_obs(1:Ka)); pen=1e4;
    obj=@(x) eval_J(max(0,x(1:Ka)),max(0,x(Ka+1:2*Ka)),S,theta,d,mu_y,mu_b,kcp,P) ...
        +pen*(sum(max(0,x(1:Ka)))+sum(max(0,x(Ka+1:2*Ka)))-tot)^2 ...
        +pen*sum(max(0,x(1:Ka)-P.u_hi(1)).^2) ...
        +pen*sum(max(0,x(Ka+1:2*Ka)-P.u_hi(2)).^2) ...
        +pen*sum(max(0,-x).^2);
    opts=optimset('MaxIter',15000,'MaxFunEvals',300000, ...
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
    % V2: 4x4 Q matrices (w and z states unpenalized)
    Qm=diag([P.w_y, P.w_b, 0, 0]);
    Rm=diag([P.r_cp, P.r_di]);
    Qf=diag([0, P.W_b, 0, 0]);
    us_f=zeros(nu_,N_);
    us_f(:,1:Ka)=[max(0.001,min(0.19,fcp_obs(1:Ka))); ...
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
            J_old=eval_J(us_f(1,1:Ka),us_f(2,1:Ka),S,theta,d,mu_y,mu_b,kcp,P) ...
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
                Jn=eval_J(un(1,1:Ka),un(2,1:Ka),S,theta,d,mu_y,mu_b,kcp,P) ...
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

function fill_iqr(x,data,col,alpha)
    sd=sort(data); n=size(sd,1);
    p25=sd(max(1,round(0.25*n)),:);
    p75=sd(max(1,round(0.75*n)),:);
    fill([x,fliplr(x)],[p25,fliplr(p75)],col,...
        'FaceAlpha',alpha,'EdgeColor','none');
end
