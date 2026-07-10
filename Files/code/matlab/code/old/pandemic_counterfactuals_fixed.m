%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & SOLVER VERIFICATION
%
%  Horizon: N = 10 quarters (Q1.2020 – Q2.2022)
%    k = 1..K_act  (Q1.2020 – Q4.2021)  active pandemic, F available
%    k = K_act+1..N (Q1.2022 – Q2.2022) post-trilemma tail, F = 0
%
%  State:   x = (y, b, z1, z2)',  z1 = F^DI_{k-1}, z2 = F^DI_{k-2}
%  Control: u = (F^CP, F^DI)'  for k <= K_act;  u = 0  for k > K_act
%
%  DI acts with lag 2 on output: y_{k+1} depends on F^DI_{k-2} = z2_k
%
%  Fixed effects:
%    mu_y_i   — country FE, output equation (raw, N=418 spec)
%    mu_b_i   — country FE, debt equation (raw)
%    eps_y_k  — quarter FE, output equation (Q4.2019 = 0 reference)
%
%  Country-specific parameters:
%    kappa_i  — realized debt cost of CP, from CP subcomponent weights
%               kappa_i = sh_above*0.423 + sh_loans*0.352 + sh_guar*0.095
%
%  Parameters: N=418 specification, unit-converted from regression to model
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration & Verification ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS
% =========================================================================

% --- Output equation (Table 3, Col 3, N=418) ---
rho_y      = 0.372;       % beta(y_lag)   x1
psi        = 0.200;       % beta(S*y)     x100
alpha_S    = 0.016;       % -beta(S)      x1
alpha_F_CP = 0.249;       % beta(FCP)     x1
eta_tilde  = -0.700;      % beta(S*FCP)   x100
eta_p      = 2.600;       % -beta(y*FCP)  x100
alpha_F_DI = 0.224;       % beta(FDI_l2)  x1
beta_fear  = -0.022;      % beta(d)       x1

% --- Debt equation (Table 4) ---
r_int      = 0.001;
gamma_y    = 0.219;
kappa_F_DI = 0.379;       % pooled (DI is homogeneous across countries)
c_H        = 0.02;

% --- Country-specific kappa_CP (Table 4 Col 3, weighted by CP composition) ---
%  kappa_i = sh_above * 0.423 + sh_loans * 0.352 + sh_guar * 0.095
%  Mean = 0.277, Range [0.173, 0.397]
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

% --- Quarter FE (Q4.2019 = 0, units: pp -> /100) ---
qfe_pp = [-1.6945, -9.2225,  1.1002, -0.8301, -0.1618, ...
            0.2676,  0.3364,  0.8448, -0.2020, -0.3219];

% --- Country FE: Output (units: pp -> /100) ---
cfe_y_val = [ 0.1530, -1.3482,  0.1786, -0.9951,  0.7648,  0.2349,  0.9292, -0.5516, ...
             -2.7814, -1.2624,  0.3376, -3.9030, -1.2179, -1.1787, -1.3129, -2.4354, ...
             -0.5643, -1.7510,  5.1692, -3.8459,  0.5602, -0.0380, -1.1328,  0.0174, ...
              0.2433,  0.9685, -0.6834, -2.4163,  0.1424,  0.4993, -0.9626, -0.8523, ...
             -2.4815, -0.4481, -1.5356,  0.3285,  2.8974,  0.3696];

% --- Country FE: Debt (raw, units: pp -> /100) ---
cfe_b_val = [-0.6746, -0.2777, -0.4649, -0.2143, -0.4844, -0.9781,  0.1462,  0.4048, ...
             -1.0619, -1.0287, -1.2539, -1.2272, -0.5830, -0.5348, -0.8409, -0.8974, ...
             -0.6096, -0.6704,  0.9444, -1.0335,  0.2331, -0.6479, -0.2022, -0.2991, ...
             -0.5080,  0.2937, -0.0212, -1.6298, -0.4222, -0.4402,  0.3454, -0.6348, ...
             -1.4319, -0.0641, -0.4026, -0.3780,  0.3130,  0.6681];

% --- Objective weights (for solver verification only) ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions ---
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

% --- Pack (kappa_F_CP omitted — country-specific, passed separately) ---
P = struct( ...
    'rho_y',rho_y, 'psi',psi, 'alpha_S',alpha_S, ...
    'alpha_F_CP',alpha_F_CP, 'eta_tilde',eta_tilde, 'eta_p',eta_p, ...
    'alpha_F_DI',alpha_F_DI, 'beta_fear',beta_fear, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_F_DI',kappa_F_DI, 'c_H',c_H, ...
    'eps_y_vec',eps_y_vec, 'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   tail = %d\n', N, K_act, N-K_act);
fprintf('  rho_y = %.3f   psi = %.3f   alpha_S = %.3f\n', rho_y, psi, alpha_S);
fprintf('  alpha_CP = %.3f   eta_tilde = %.3f   eta_p = %.3f\n', alpha_F_CP, eta_tilde, eta_p);
fprintf('  Net CP at S=0.44: %.3f\n', alpha_F_CP + eta_tilde*0.44);
fprintf('  gamma_y = %.3f   kappa_DI = %.3f\n', gamma_y, kappa_F_DI);
fprintf('  kappa_CP: mean=%.3f  range=[%.3f, %.3f] (country-specific)\n\n', ...
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
%  STEP 1: VALIDATION
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 1: Validation (N = %d)\n', N);
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

%% ========================================================================
%  STEP 1b: NON-TARGETED MOMENTS
%  These moments were NOT used in estimation. The model produces them
%  from the interaction of estimated parameters with country-specific
%  policy trajectories. A good match validates the model structure.
%
%  Includes: (1) Cross-Country SD, (2) Autocorrelation, (3) Output-Debt
%  Nexus, (4) ICC, (5) Fiscal Multiplier, (6) Theta Validation
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 1b: Non-Targeted Moments\n');
fprintf('========================================\n');

% --- (1) Cross-Country Standard Deviation per Quarter ---
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
ac2_obs = zeros(n_c,1);  ac2_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_act);  ys = cdata(i).sim_y(1:K_act);
    co1 = corrcoef(yo(1:end-1), yo(2:end));  ac1_obs(i) = co1(1,2);
    cs1 = corrcoef(ys(1:end-1), ys(2:end));  ac1_sim(i) = cs1(1,2);
    co2 = corrcoef(yo(1:end-2), yo(3:end));  ac2_obs(i) = co2(1,2);
    cs2 = corrcoef(ys(1:end-2), ys(3:end));  ac2_sim(i) = cs2(1,2);
end
fprintf('  %20s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %20s %10.3f %10.3f\n', 'AC(1) mean', mean(ac1_obs), mean(ac1_sim));
fprintf('  %20s %10.3f %10.3f\n', 'AC(2) mean', mean(ac2_obs), mean(ac2_sim));

% --- (3) Output-Debt Nexus ---
fprintf('\n  (3) Output-Debt Nexus:\n');
corr_yd_obs = zeros(n_c,1);  corr_yd_sim = zeros(n_c,1);
for i = 1:n_c
    yo = cdata(i).y(1:K_act);
    dbo = cdata(i).b(1:K_act);
    ys = cdata(i).sim_y(1:K_act);
    dbs_i = diff([0, cdata(i).sim_b(1:K_act)]);
    co = corrcoef(yo, dbo);    corr_yd_obs(i) = co(1,2);
    cs = corrcoef(ys, dbs_i);  corr_yd_sim(i) = cs(1,2);
end
cum_y_obs = arrayfun(@(c) sum(c.y(1:K_act)), cdata);
cum_b_obs = arrayfun(@(c) sum(c.b(1:K_act)), cdata);
cum_y_sim = arrayfun(@(c) sum(c.sim_y(1:K_act)), cdata);
cum_b_sim = arrayfun(@(c) c.sim_b(K_act), cdata);
cc_obs = corrcoef(cum_y_obs, cum_b_obs);
cc_sim = corrcoef(cum_y_sim, cum_b_sim);
fprintf('  %25s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %25s %10.3f %10.3f\n', 'Corr(y,db) within', mean(corr_yd_obs), mean(corr_yd_sim));
fprintf('  %25s %10.3f %10.3f\n', 'Corr(Sy,Sb) cross', cc_obs(1,2), cc_sim(1,2));

% --- (4) ICC ---
fprintf('\n  (4) ICC (between / total variance):\n');
y_obs_p = reshape([cdata.y], N, n_c)' * 100;
y_sim_p = reshape([cdata.sim_y], N, n_c)' * 100;
b_obs_p = zeros(n_c, K_act);  b_sim_p = zeros(n_c, K_act);
for i = 1:n_c
    b_obs_p(i,:) = cdata(i).obs_b_cum(1:K_act) * 100;
    b_sim_p(i,:) = cdata(i).sim_b(1:K_act) * 100;
end
icc_y_obs = var(mean(y_obs_p(:,1:K_act),2)) / var(y_obs_p(:,1:K_act),0,'all');
icc_y_sim = var(mean(y_sim_p(:,1:K_act),2)) / var(y_sim_p(:,1:K_act),0,'all');
icc_b_obs = var(mean(b_obs_p,2)) / var(b_obs_p,0,'all');
icc_b_sim = var(mean(b_sim_p,2)) / var(b_sim_p,0,'all');
fprintf('  %20s %10s %10s\n', '', 'Observed', 'Model');
fprintf('  %20s %10.3f %10.3f\n', 'Output gap', icc_y_obs, icc_y_sim);
fprintf('  %20s %10.3f %10.3f\n', 'Cum. debt', icc_b_obs, icc_b_sim);

% --- (5) Fiscal Multiplier ---
fprintf('\n  (5) Cumulative Fiscal Multiplier:\n');
oecd_S  = mean(reshape([cdata.S], N, n_c), 2)';
oecd_th = mean(reshape([cdata.theta], N, n_c), 2)';
oecd_d  = mean(reshape([cdata.d], N, n_c), 2)';
oecd_FCP = mean(reshape([cdata.FCP], N, n_c), 2)';
oecd_FDI = mean(reshape([cdata.FDI], N, n_c), 2)';
mu_y_avg = mean([cdata.mu_y]);
mu_b_avg = mean([cdata.mu_b]);
kcp_avg  = mean([cdata.kappa_cp]);

xs_base = forward_roll(oecd_FCP, oecd_FDI, oecd_S, oecd_th, oecd_d, mu_y_avg, mu_b_avg, kcp_avg, P);
fcp_sh = oecd_FCP;  fcp_sh(1) = fcp_sh(1) + 0.01;
fdi_sh = oecd_FDI;  fdi_sh(1) = fdi_sh(1) + 0.01;
xs_cp = forward_roll(fcp_sh, oecd_FDI, oecd_S, oecd_th, oecd_d, mu_y_avg, mu_b_avg, kcp_avg, P);
xs_di = forward_roll(oecd_FCP, fdi_sh, oecd_S, oecd_th, oecd_d, mu_y_avg, mu_b_avg, kcp_avg, P);

cum_cp = sum(xs_cp(1,2:K_act+1) - xs_base(1,2:K_act+1));
cum_di = sum(xs_di(1,2:K_act+1) - xs_base(1,2:K_act+1));
fprintf('  %25s %10s %10s\n', '', 'CP', 'DI');
fprintf('  %25s %10.3f %10.3f\n', 'Impact (Q1)', ...
    (xs_cp(1,2)-xs_base(1,2))/0.01, (xs_di(1,2)-xs_base(1,2))/0.01);
fprintf('  %25s %10.3f %10.3f\n', 'Cumulative (8Q)', cum_cp/0.01, cum_di/0.01);
fprintf('  %25s %10.3f %10.3f\n', 'Debt cost (pp/pp)', ...
    (xs_cp(2,N+1)-xs_base(2,N+1))/0.01, (xs_di(2,N+1)-xs_base(2,N+1))/0.01);

fprintf('\n  Net CP at different S:\n');
for S_test = [0.20, 0.35, 0.44, 0.55]
    net = P.alpha_F_CP + P.eta_tilde * S_test;
    if net > 0, lbl = 'effective'; else, lbl = 'counterproductive'; end
    fprintf('    S = %.2f: %+.3f  (%s)\n', S_test, net, lbl);
end


%% ========================================================================
%  (6) THETA VALIDATION — Epidemiological Module (Non-Targeted)
%
%  theta_{k+1} = rho_theta * (1 - phi_S * S_k) * theta_k + eps_k
%
%  rho_theta: wave-specific, from Liu & Rocklov (2021)
%  phi_S:     0.55, from Brauner (2021), Haug et al. (2020)
%  eps_k:     common wave shocks, extracted from OECD average
%  theta_0:   country-specific initial conditions from data
%
%  Cross-country variation is NON-TARGETED: arises only from different
%  S paths interacting with common parameters.
% =========================================================================
fprintf('\n  (6) Theta Validation (epidemiological module):\n');

% Wave parameters: [start_quarter(1-indexed), rho_theta]
wave_starts = [1, 3, 5, 7, 8];
wave_rho    = [1.30, 1.25, 1.45, 1.55, 1.70];
phi_S_epi   = 0.55;

% Theta initial values (fractions) — stored per country
theta0_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
              'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
              'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
              'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
              'PRT','SVK','SVN','SWE','TUR','USA'};
theta0_val = [5.829e-4, 2.234e-3, 1.345e-2, 1.502e-3, 4.668e-3, 4.373e-4, ...
              0,        1.409e-4, 6.061e-4, 1.474e-3, 5.242e-4, 2.033e-2, ...
              3.783e-4, 7.043e-4, 7.717e-3, 9.820e-3, 5.036e-4, 8.265e-4, ...
              4.598e-3, 1.457e-3, 7.389e-4, 1.500e-2, 0,        4.398e-5, ...
              2.063e-3, 2.276e-3, 0,        2.769e-4, 9.381e-3, 8.856e-4, ...
              1.453e-4, 1.673e-4, 3.649e-3, 6.611e-4, 7.681e-4, 5.533e-3, ...
              2.525e-4, 4.364e-3];
theta0_map = containers.Map(theta0_iso, theta0_val);

% Build rho_theta vector for each quarter (precomputed from wave table)
rho_by_k = zeros(1, N);
for k = 1:N
    rho_by_k(k) = wave_rho(1);
    for wi = length(wave_starts):-1:1
        if k >= wave_starts(wi), rho_by_k(k) = wave_rho(wi); break; end
    end
end

% Extract common wave shocks (eps_k) from OECD average theta
oecd_theta = mean(reshape([cdata.theta], N, n_c), 2)';
oecd_S_vec = mean(reshape([cdata.S], N, n_c), 2)';
oecd_th0   = mean(theta0_val);

eps_theta = zeros(1, N);
th_prev = oecd_th0;
for k = 1:N
    rho_k = rho_by_k(k);
    predicted = rho_k * (1 - phi_S_epi * oecd_S_vec(k)) * th_prev;
    eps_theta(k) = oecd_theta(k) - predicted;
    th_prev = oecd_theta(k);
end

% Simulate theta per country
for i = 1:n_c
    iso = cdata(i).iso;
    th0_i = 0;
    if isKey(theta0_map, iso), th0_i = theta0_map(iso); end
    th = zeros(1, N);
    th_prev = th0_i;
    for k = 1:N
        rho_k = rho_by_k(k);
        th(k) = rho_k * (1 - phi_S_epi * cdata(i).S(k)) * th_prev + eps_theta(k);
        th(k) = max(th(k), 0);
        th_prev = th(k);
    end
    cdata(i).sim_theta = th;
end

% Compute fit statistics
rmse_th = zeros(n_c,1);  corr_th = zeros(n_c,1);
for i = 1:n_c
    th_o = cdata(i).theta(1:K_act);
    th_s = cdata(i).sim_theta(1:K_act);
    rmse_th(i) = sqrt(mean((th_s - th_o).^2)) * 100;
    cc = corrcoef(th_o, th_s);
    corr_th(i) = cc(1,2);
end

fprintf('    RMSE (k=1:%d):  Median: %.3f pp   Mean: %.3f pp\n', ...
    K_act, median(rmse_th), mean(rmse_th));
fprintf('    Temporal corr:  Mean: %.3f\n', mean(corr_th(~isnan(corr_th))));

% Cross-country SD
fprintf('\n    Cross-Country SD of theta:\n');
fprintf('    %10s %8s %8s %8s\n', 'Quarter', 'SD_obs', 'SD_sim', 'Ratio');
sd_theta = zeros(1, K_act);
for k = 1:K_act
    obs_k = arrayfun(@(c) c.theta(k), cdata) * 100;
    sim_k = arrayfun(@(c) c.sim_theta(k), cdata) * 100;
    sd_o = std(obs_k);  sd_s = std(sim_k);
    sd_theta(k) = sd_s / max(sd_o, 1e-10);
    fprintf('    %10s %8.3f %8.3f %8.3f\n', qlbl{k}, sd_o, sd_s, sd_theta(k));
end
fprintf('    %10s %8s %8s %8.3f\n', 'Mean', '', '', mean(sd_theta));

% ICC theta
th_obs_p = reshape([cdata.theta], N, n_c)' * 100;
th_sim_p = reshape([cdata.sim_theta], N, n_c)' * 100;
icc_th_obs = var(mean(th_obs_p(:,1:K_act),2)) / var(th_obs_p(:,1:K_act),0,'all');
icc_th_sim = var(mean(th_sim_p(:,1:K_act),2)) / var(th_sim_p(:,1:K_act),0,'all');
fprintf('\n    ICC theta:  obs=%.3f   sim=%.3f\n', icc_th_obs, icc_th_sim);


%% ========================================================================
%  SUMMARY TABLE
% =========================================================================
fprintf('\n  %-40s %10s %10s %6s\n', 'Moment', 'Observed', 'Model', '');
fprintf('  %s\n', repmat('-', 1, 68));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'SD ratio output (mean)',   1.0,               mean(sd_ratios),       iff(abs(1-mean(sd_ratios))<0.3, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'AC(1) output',             mean(ac1_obs),     mean(ac1_sim),         iff(abs(mean(ac1_obs)-mean(ac1_sim))<0.15, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'AC(2) output',             mean(ac2_obs),     mean(ac2_sim),         iff(abs(mean(ac2_obs)-mean(ac2_sim))<0.15, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'Corr(y,db) within',        mean(corr_yd_obs), mean(corr_yd_sim),     iff(abs(mean(corr_yd_obs)-mean(corr_yd_sim))<0.20, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'Corr(Sy,Sb) cross',        cc_obs(1,2),       cc_sim(1,2),           iff(abs(cc_obs(1,2)-cc_sim(1,2))<0.30, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'ICC output',                icc_y_obs,         icc_y_sim,             iff(abs(icc_y_obs-icc_y_sim)<0.10, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'ICC debt',                  icc_b_obs,         icc_b_sim,             iff(abs(icc_b_obs-icc_b_sim)<0.20, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'SD ratio theta (mean)',     1.0,               mean(sd_theta),        iff(abs(1-mean(sd_theta))<0.30, 'OK','WEAK'));
fprintf('  %-40s %10.3f %10.3f %6s\n', 'Theta temporal corr',       1.0,               mean(corr_th(~isnan(corr_th))), iff(mean(corr_th(~isnan(corr_th)))>0.50, 'OK','WEAK'));


%% ========================================================================
%  FIG 5: Theta Validation (OECD Median +/- IQR)
% =========================================================================
th_obs_all = reshape([cdata.theta], N, n_c)' * 100;
th_sim_all = reshape([cdata.sim_theta], N, n_c)' * 100;

figure('Name','Theta Validation','Color','w','Position',[50 50 700 400]);
hold on;
fill_iqr(1:N, th_sim_all, [0.8 0.2 0.2], 0.15);
fill_iqr(1:N, th_obs_all, [0.5 0.5 0.5], 0.12);
plot(1:N, median(th_sim_all), 'r-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(th_obs_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act + 0.5, ':', 'Color', [.5 .5 .5]);
grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('Pandemic pressure (\theta, pp)');
title('Epidemiological Module: Non-Targeted Validation');
legend('', '', 'Simulated', 'Observed', 'Location', 'NW', 'FontSize', 8);
text(0.98, 0.95, sprintf('RMSE = %.2f pp\nCorr = %.2f\nSD ratio = %.2f', ...
    median(rmse_th), mean(corr_th(~isnan(corr_th))), mean(sd_theta)), ...
    'Units', 'normalized', 'HorizontalAlignment', 'right', ...
    'VerticalAlignment', 'top', 'FontSize', 8, 'BackgroundColor', 'w');


% --- Helper ---
function out = iff(cond, a, b)
    if cond, out = a; else, out = b; end
end

%% ========================================================================
%  STEP 2-3: CF-B — DIRECT SHOOTING vs iLQR
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 2-3: CF-B  (K_act = %d, N = %d)\n', K_act, N);
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
    fcp_il_f = [us_il(1,:), zeros(1, N-K_act)];
    fdi_il_f = [us_il(2,:), zeros(1, N-K_act)];
    xs_ds = forward_roll(fcp_ds_f, fdi_ds_f, S_i, th_i, d_i, my_i, mb_i, kc_i, P);
    xs_il = forward_roll(fcp_il_f, fdi_il_f, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    res(i).iso    = iso;
    res(i).J_ds   = J_ds;       res(i).J_il   = J_il;
    res(i).dJ     = abs(J_ds - J_il);
    res(i).b_ds   = xs_ds(2, N+1)*100;
    res(i).b_il   = xs_il(2, N+1)*100;
    res(i).db     = abs(xs_ds(2,N+1) - xs_il(2,N+1))*100;
    res(i).du_max = max(abs(us_ds(:) - us_il(:)))*100;
    res(i).us_ds  = us_ds;      res(i).us_il  = us_il;
    res(i).xs_ds  = xs_ds;      res(i).xs_il  = xs_il;

    if res(i).dJ < 0.05 && res(i).db < 0.5, flag='OK'; else, flag='WARN'; end
    fprintf('  [%2d/%d] %s  |dJ|=%.2e  |db|=%.4fpp  |du|=%.3f%%  k=%.3f  %s\n', ...
        i, n_c, iso, res(i).dJ, res(i).db, res(i).du_max, kc_i, flag);
end


%% ========================================================================
%  STEP 4: VERIFICATION SUMMARY
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 4: Verification Summary\n');
fprintf('========================================\n');

dJs = [res.dJ]; dbs = [res.db]; du_maxs = [res.du_max];
n_conv = sum(dJs < 0.05 & dbs < 0.5);

fprintf('\n  |dJ| — Mean: %.2e   Max: %.2e\n', mean(dJs), max(dJs));
fprintf('  |db| — Mean: %.4f pp  Max: %.4f pp\n', mean(dbs), max(dbs));
fprintf('  |du| — Mean: %.3f%%   Max: %.3f%%\n', mean(du_maxs), max(du_maxs));
fprintf('  Converged: %d / %d\n', n_conv, n_c);

Tout = table({res.iso}', [res.J_ds]', [res.J_il]', [res.dJ]', ...
    [res.b_ds]', [res.b_il]', [res.db]', [res.du_max]', ...
    'VariableNames', {'Country','J_direct','J_ilqr','abs_dJ', ...
                      'b_direct_N','b_ilqr_N','abs_db_pp','abs_du_max_pct'});
writetable(Tout, 'verification_results.csv');
fprintf('\n  Saved: verification_results.csv\n');


%% ========================================================================
%  STEP 5: VISUALIZATION
% =========================================================================

% --- Fig 1: Verification scatter ---
figure('Name','Verification','Color','w','Position',[50 50 1100 450]);
subplot(1,2,1);
plot([res.J_ds],[res.J_il],'ko','MarkerFaceColor',[.2 .5 .8],'MarkerSize',7); hold on;
mn=min([res.J_ds])*0.95; mx=max([res.J_ds])*1.05;
plot([mn mx],[mn mx],'r--','LineWidth',1.5);
xlabel('J — Direct'); ylabel('J — iLQR'); title('Objective'); grid on; axis equal;
subplot(1,2,2);
plot([res.b_ds],[res.b_il],'ko','MarkerFaceColor',[.8 .3 .3],'MarkerSize',7); hold on;
mn=min([res.b_ds])*0.95; mx=max([res.b_ds])*1.05;
plot([mn mx],[mn mx],'r--','LineWidth',1.5);
xlabel('b_N — Direct'); ylabel('b_N — iLQR');
title(sprintf('Terminal Debt (N=%d)',N)); grid on; axis equal;
sgtitle('Solver Verification','FontWeight','bold');

% --- Fig 2: Convergence bars ---
figure('Name','Convergence','Color','w','Position',[50 50 1200 400]);
subplot(1,2,1); [~,si]=sort(dJs,'descend');
bar(dJs(si),'FaceColor',[.2 .5 .8]); hold on; yline(0.05,'r--','LineWidth',1.5);
set(gca,'XTick',1:n_c,'XTickLabel',{res(si).iso},'XTickLabelRotation',55,'FontSize',6);
ylabel('|dJ|'); title('Objective Diff'); grid on;
subplot(1,2,2); [~,si2]=sort(dbs,'descend');
bar(dbs(si2),'FaceColor',[.8 .3 .3]); hold on; yline(0.5,'r--','LineWidth',1.5);
set(gca,'XTick',1:n_c,'XTickLabel',{res(si2).iso},'XTickLabelRotation',55,'FontSize',6);
ylabel('|db| pp'); title('Debt Diff'); grid on;
sgtitle('Convergence Diagnostics','FontWeight','bold');

% --- Fig 3: Selected countries ---
selected = {'USA','DEU','ITA','GBR','JPN','CHL'}; n_sel = length(selected);
figure('Name','Trajectories','Color','w','Position',[30 30 1500 800]);
for s = 1:n_sel
    iso = selected{s}; idx = find(strcmp({cdata.iso}, iso));
    xs_obs = forward_roll(cdata(idx).FCP, cdata(idx).FDI, ...
        cdata(idx).S, cdata(idx).theta, cdata(idx).d, ...
        cdata(idx).mu_y, cdata(idx).mu_b, cdata(idx).kappa_cp, P);
    t_obs = (cdata(idx).FCP(1:K_act)+cdata(idx).FDI(1:K_act))*100;
    t_ds  = (res(idx).us_ds(1,:)+res(idx).us_ds(2,:))*100;
    t_il  = (res(idx).us_il(1,:)+res(idx).us_il(2,:))*100;

    subplot(n_sel,3,(s-1)*3+1); hold on;
    bh=bar(1:K_act,[t_obs;t_ds;t_il]','grouped');
    bh(1).FaceColor=[.5 .5 .5]; bh(2).FaceColor=[.2 .5 .8]; bh(3).FaceColor=[.8 .3 .3];
    set(gca,'XTick',1:K_act,'XTickLabel',qlbl(1:K_act),'FontSize',6);
    ylabel('% GDP'); grid on;
    if s==1, title('Total Fiscal'); legend('Obs','DS','iLQR','FontSize',5,'Location','NE'); end
    text(0.02,0.95,iso,'Units','normalized','FontSize',10,'FontWeight','bold','VerticalAlignment','top');

    subplot(n_sel,3,(s-1)*3+2); hold on;
    for kk=1:K_act
        tot_o=cdata(idx).FCP(kk)+cdata(idx).FDI(kk);
        tot_d=res(idx).us_ds(1,kk)+res(idx).us_ds(2,kk);
        if tot_o>1e-5, plot(kk,cdata(idx).FCP(kk)/tot_o*100,'ks','MarkerSize',7,'MarkerFaceColor',[.5 .5 .5]); end
        if tot_d>1e-5, plot(kk,res(idx).us_ds(1,kk)/tot_d*100,'bo','MarkerSize',6,'MarkerFaceColor',[.2 .5 .8]); end
    end
    ylim([0 105]); set(gca,'XTick',1:K_act,'XTickLabel',qlbl(1:K_act),'FontSize',6);
    ylabel('CP%'); grid on;
    if s==1, title('CP Share'); end

    subplot(n_sel,3,(s-1)*3+3); hold on;
    yyaxis left;
    plot(1:N,xs_obs(1,2:end)*100,'k--','LineWidth',1.5);
    plot(1:N,res(idx).xs_ds(1,2:end)*100,'b-','LineWidth',1.5);
    xline(K_act+0.5,':','Color',[.5 .5 .5]); ylabel('y (pp)');
    yyaxis right;
    plot(1:N,xs_obs(2,2:end)*100,'k--s','LineWidth',1,'MarkerSize',2);
    plot(1:N,res(idx).xs_ds(2,2:end)*100,'b-o','LineWidth',1,'MarkerSize',2);
    ylabel('b (pp)'); set(gca,'XTick',1:N,'XTickLabel',qlbl,'FontSize',5); grid on;
    if s==1, title('Outcomes'); legend('Obs','Opt','Location','SE','FontSize',5); end
end
sgtitle(sprintf('CF-B: N=%d, K_{act}=%d',N,K_act),'FontWeight','bold');

% --- Fig 4: Validation — Output Gap and Cumulative Debt (2 panels) ---
sim_y_all = reshape([cdata.sim_y], N, n_c)' * 100;
obs_y_all = reshape([cdata.y],    N, n_c)' * 100;
sim_b_all = reshape([cdata.sim_b], N, n_c)' * 100;
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum * 100; end

figure('Name','Validation','Color','w','Position',[50 50 1000 400]);

subplot(1,2,1); hold on;
fill_iqr(1:N, sim_y_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_y_all, [.5 .5 .5], .12);
plot(1:N, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act+.5, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title('Output Gap');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);

subplot(1,2,2); hold on;
fill_iqr(1:N, sim_b_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_b_all, [.5 .5 .5], .12);
plot(1:N, median(sim_b_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_b_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act+.5, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of 2019 GDP'); title('Cumulative Debt');
legend('','','Simulated','Observed', 'Location', 'SE', 'FontSize', 7);

sgtitle(sprintf('Validation: Simulated vs Observed (N=%d, Median \\pm IQR)', N), ...
    'FontWeight', 'bold');

fprintf('\n=== COMPLETE ===\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

function xs = forward_roll(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
    N_=P.N; xs=zeros(P.nx, N_+1);
    for k=1:N_
        y=xs(1,k); b=xs(2,k); z1=xs(3,k); z2=xs(4,k);
        fk=0; gk=0; Sk=0; thk=0; dk=0; ey=0;
        if k<=length(fcp), fk=fcp(k); end
        if k<=length(fdi), gk=fdi(k); end
        if k<=length(S), Sk=S(k); end
        if k<=length(theta), thk=theta(k); end
        if k<=length(d), dk=d(k); end
        if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
        xs(1,k+1) = mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
            + (P.alpha_F_CP + P.eta_tilde*Sk - P.eta_p*y)*fk ...
            + P.alpha_F_DI*z2 + P.beta_fear*dk + ey;
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
            + kcp*fk + P.kappa_F_DI*gk + P.c_H*thk;
        xs(3,k+1) = gk;     % z1 = current F_DI
        xs(4,k+1) = z1;     % z2 = lagged z1 = F_DI_{k-1}
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
    y=x(1); fcp=u(1); Sk=0;
    if k<=length(S), Sk=S(k); end
    A = [P.rho_y+P.psi*Sk-P.eta_p*fcp, 0,          0, P.alpha_F_DI;
         -P.gamma_y,                     1+P.r_int,  0, 0;
         0,                              0,          0, 0;
         0,                              0,          1, 0];
    B = [P.alpha_F_CP+P.eta_tilde*Sk-P.eta_p*y, 0;
         kcp,                                     P.kappa_F_DI;
         0,                                       1;
         0,                                       0];
end

function xn = dynamics_step(x, u, k, S, theta, d, mu_y, mu_b, kcp, P)
    y=x(1); b=x(2); z1=x(3); z2=x(4); fcp=u(1); fdi=u(2);
    Sk=0; thk=0; dk=0; ey=0;
    if k<=length(S), Sk=S(k); end
    if k<=length(theta), thk=theta(k); end
    if k<=length(d), dk=d(k); end
    if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
    xn = [mu_y + P.rho_y*y + P.psi*Sk*y - P.alpha_S*Sk ...
              + (P.alpha_F_CP+P.eta_tilde*Sk-P.eta_p*y)*fcp ...
              + P.alpha_F_DI*z2 + P.beta_fear*dk + ey;
          mu_b + (1+P.r_int)*b - P.gamma_y*y ...
              + kcp*fcp + P.kappa_F_DI*fdi + P.c_H*thk;
          fdi;
          z1];
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
    Qm=diag([P.w_y,P.w_b,0,0]); Rm=diag([P.r_cp,P.r_di]); Qf=diag([0,P.W_b,0,0]);
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
