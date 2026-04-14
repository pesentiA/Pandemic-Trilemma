%% ========================================================================
%  PANDEMIC SOCIAL PLANNER — CALIBRATED VERSION
%
%  Based on ALV10, with three key improvements:
%    1. OECD-calibrated theta wave shocks (Stage 1)
%    2. Fixed observed theta + d in Stage 2 (not endogenous)
%    3. Validation forward-roll with fit diagnostics
%
%  Architecture:
%    VALIDATION: Forward-roll with observed controls → fit diagnostics
%    STAGE 1 (Normative): Pareto frontier — what is achievable?
%       - Sweep VSL/MCPF weights, optimize all instruments
%       - Theta endogenous with OECD-calibrated wave shocks
%    STAGE 2 (Positive): Country counterfactuals — what went wrong?
%       - Fix S_obs, theta_obs, d_obs (all exogenous)
%       - Optimize only F^CP and F^DI
%       - Welfare decomposition: Composition Gap + Containment Gap
%
%  Horizon: N=12 quarters (Q1.2020–Q4.2022)
%    k=1:9  — active pandemic + recovery, data available
%    k=10:12 — post-trilemma tail, F=0, debt dynamics continue
%
%  UNIT CONVENTION (all model variables in fractions):
%    S in [0,1], y/b/theta/F in fractions of GDP or population
%
%  TRANSITION SYSTEM (main.tex Section 3):
%    y_{k+1} = rho_y*y + (psi*y - alpha_S)*S
%              + (alpha_CP + eta_tilde*S - eta_p*y)*F^CP
%              + alpha_DI*z + beta_d*d + eps_y(k)
%    d_{k+1} = delta_theta(k)*theta
%    b_{k+1} = (1+r)*b - gamma_y*y + kappa_CP*F^CP + kappa_DI*F^DI + c_H*theta
%    theta_{k+1} = rho_theta(k)*(1 - phi_S*S)*theta + eps_theta(k)
%    z_{k+1} = F^DI   (auxiliary state for DI lag)
%
%  State: x=(y,d,b,theta,z)' in R^5    Control: u=(S,F^DI,F^CP)' in R^3
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibrated Model ===\n');
fprintf('  %s\n\n', datestr(now));

%% ========================================================================
%  STEP 0: STRUCTURAL PARAMETERS
% =========================================================================
N = 12;  beta_disc = 0.99;  n_x = 5;  n_u = 3;

% --- Output dynamics (TWFE estimates, main.tex Section 5) ---
%  Estimation sample: Q1.2020-Q2.2022, 38 OECD countries, N=380
%  feols with country + quarter FE, clustered SE at country level
rho_y      = 0.227;      % Baseline persistence (pre-pandemic norm: 0.8-0.9)
psi        = 0.372;      % State-dependent containment cost (S x y_{k-1})
alpha_S    = 0.028;      % Direct output cost of containment
alpha_F_CP = 0.253;      % CP level effect (p<0.01)
eta_tilde  = -0.596;     % CP x S cushioning interaction (p<0.05)
eta_p      = 0.026;      % CP x y_{k-1} structural persistence (fragile, p~0.10)
alpha_F_DI = 0.244;      % DI multiplier at lag 2 (p<0.10, sensitive to spec)
beta_fear  = -0.018;     % Fear term: output cost of excess mortality (P-score)

% --- Debt dynamics (Country FE estimates, main.tex Section 5) ---
r_int      = 0.001;      % Real quarterly interest rate (OECD 2022)
gamma_y    = 0.191;      % Automatic stabilizer semi-elasticity
kappa_F_CP = 0.193;      % Realized debt cost of CP (pooled, baseline 35% adj.)
kappa_F_DI = 0.468;      % Realized debt cost of DI
c_H        = 0.02;       % Health expenditure per unit infection (IMF/OECD)

% --- Epidemiological parameters (calibrated from literature) ---
phi_S = 0.55;            % NPI suppression effectiveness (Brauner 2021, Haug 2020)
rho_theta_base = 1.30;   % Baseline effective reproduction (quarterly)
baseline_qmort = 0.0025; % Baseline quarterly all-cause mortality rate (~1%/yr)

%  Wave-specific IFRs (IHME/COVID-19 Forecasting Team, Lancet 2022)
%  delta_theta = IFR / baseline_qmort converts theta→d in P-score units
wave_ifr = [0.020, 0.015, 0.012, 0.008, 0.003];
delta_theta_base = wave_ifr(1) / baseline_qmort;  % = 8.0 (Ancestral)

%  Wave table: [quarter(1-indexed), rho_theta, delta_theta, wave_shock]
%  Quarters: k=1→Q1.20, k=3→Q3.20, k=5→Q1.21, k=7→Q3.21, k=8→Q4.21
wave_table = [1 1.30 wave_ifr(1)/baseline_qmort;
              3 1.25 wave_ifr(2)/baseline_qmort;
              5 1.45 wave_ifr(3)/baseline_qmort;
              7 1.55 wave_ifr(4)/baseline_qmort;
              8 1.70 wave_ifr(5)/baseline_qmort];

% Build time-varying rho_theta and delta_theta vectors
rho_theta_t = rho_theta_base * ones(1, N);
delta_theta_t = delta_theta_base * ones(1, N);
for w = 1:size(wave_table,1)
    kw = wave_table(w,1);
    if kw <= N
        rho_theta_t(kw) = wave_table(w,2);
        if kw+1 <= N
            rho_theta_t(kw+1) = 0.5*(wave_table(w,2) + rho_theta_base);
        end
        delta_theta_t(kw:end) = wave_table(w,3);
    end
end

% --- OECD-calibrated wave shocks for theta (Stage 1) ---
%  Calibrated to reproduce the OECD-average theta trajectory exactly
%  when combined with OECD-average S. Derived by solving:
%    eps(k) = theta_OECD(k+1) - rho_theta(k)*(1-phi_S*S_OECD(k))*theta_OECD(k)
%  4 of 9 shocks are negative (inter-wave recovery/immunity effects)
eps_theta_calibrated = [-0.0020864, 0.0007164, 0.0059961, -0.0024487, ...
                        -0.0005719, 0.0022462, 0.0042843, 0.0278499, ...
                        -0.0231259, 0, 0, 0];  % k=1:12, zeros for post-pandemic
wave_shock_vec = zeros(1, N+1);
for k = 1:N
    wave_shock_vec(k+1) = eps_theta_calibrated(k);
end

% --- Exogenous output shocks (Quarter FE from TWFE estimation) ---
%  These capture the common pandemic effect absorbed by Quarter FE.
%  Without them, alpha_S only measures the cross-country partial effect.
%  Units: pp → fractions (/100).
eps_y_data = [0.000, -1.844, -9.526, +0.149, -1.294, ...
              -0.538, -0.137, -0.052, +0.534, -0.426] / 100;
eps_y_vec = zeros(1, N+1);
for k = 1:min(9, N)
    if k+1 <= length(eps_y_data)
        eps_y_vec(k+1) = eps_y_data(k+1);
    end
end

% --- Control bounds ---
S_max = 0.86;   FDI_max = 0.04;   FCP_max = 0.12;

% --- Pack all parameters into struct ---
P = struct('rho_y',rho_y, 'psi',psi, 'alpha_S',alpha_S, ...
    'alpha_F_CP',alpha_F_CP, 'eta_tilde',eta_tilde, 'eta_p',eta_p, ...
    'alpha_F_DI',alpha_F_DI, 'beta_fear',beta_fear, ...
    'rho_theta_t',rho_theta_t, 'delta_theta_t',delta_theta_t, ...
    'phi_S',phi_S, 'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_F_CP',kappa_F_CP, 'kappa_F_DI',kappa_F_DI, 'c_H',c_H, ...
    'S_max',S_max, 'FDI_max',FDI_max, 'FCP_max',FCP_max, ...
    'wave_shock_vec',wave_shock_vec, 'eps_y_vec',eps_y_vec, ...
    'N',N, 'n_x',n_x, 'n_u',n_u, 'beta_disc',beta_disc, ...
    'baseline_qmort',baseline_qmort);

fprintf('  Parameters loaded.\n');
fprintf('  N = %d quarters | phi_S = %.2f | beta_fear = %.3f\n', N, phi_S, beta_fear);
fprintf('  Wave shocks: OECD-calibrated (4 negative / 9 total)\n\n');


%% ========================================================================
%  STEP 1: LOAD COUNTRY DATA
% =========================================================================
fprintf('--- Loading country data ---\n');
T = readtable('country_data_for_matlab.csv');
qord = {'Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021',...
        'Q3.2021','Q4.2021','Q1.2022','Q2.2022','Q3.2022','Q4.2022'};
countries = unique(T.Country, 'stable');
n_c = length(countries);

cdata = struct();
for i = 1:n_c
    iso = countries{i};
    cdata(i).iso = iso;
    cdata(i).S = zeros(1,N);     cdata(i).FCP = zeros(1,N);
    cdata(i).FDI = zeros(1,N);   cdata(i).y = zeros(1,N);
    cdata(i).theta = zeros(1,N); cdata(i).b = zeros(1,N);
    cdata(i).d = zeros(1,N);
    for k = 1:N
        if k > length(qord), break; end
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}), :);
        if isempty(row), continue; end
        cdata(i).S(k)     = row.S_mean_tw / 100;
        cdata(i).FCP(k)   = row.F_CP / 100;
        cdata(i).FDI(k)   = row.F_DI / 100;
        cdata(i).y(k)     = row.y_t_pct / 100;
        cdata(i).theta(k) = row.theta_pct / 100;
        if ~ismissing(row.debt_dR)
            cdata(i).b(k) = row.debt_dR / 100;
        end
        if ismember('excess_mortality', T.Properties.VariableNames) && ~ismissing(row.excess_mortality)
            cdata(i).d(k) = row.excess_mortality / 100;
        end
    end
end

% OECD averages (for Stage 1 reference and plots)
oecd = struct();
oecd.S     = mean(reshape([cdata.S], N, n_c), 2)';
oecd.FCP   = mean(reshape([cdata.FCP], N, n_c), 2)';
oecd.FDI   = mean(reshape([cdata.FDI], N, n_c), 2)';
oecd.y     = mean(reshape([cdata.y], N, n_c), 2)';
oecd.theta = mean(reshape([cdata.theta], N, n_c), 2)';
oecd.b     = mean(reshape([cdata.b], N, n_c), 2)';
oecd.d     = mean(reshape([cdata.d], N, n_c), 2)';
fprintf('  %d countries x %d quarters loaded.\n\n', n_c, N);


%% ========================================================================
%  STEP 2: VALIDATION — FORWARD ROLL WITH OBSERVED CONTROLS
%  Purpose: verify that the transition system with estimated + calibrated
%  parameters reproduces observed trajectories when fed actual policy.
%  Stage 2 mode: theta and d are fixed at observed values.
% =========================================================================
fprintf('========================================\n');
fprintf('  VALIDATION: Forward Roll (Stage 2 mode)\n');
fprintf('========================================\n');

K_eval = 9;  % evaluate first 9 quarters (Q1.2020–Q1.2022)
val = struct();

for i = 1:n_c
    % Initial conditions: steady state except country-specific theta_0
    x0_i = [0; 0; 0; cdata(i).theta(1); 0];
    
    % Observed controls
    u_obs = [cdata(i).S; cdata(i).FDI; cdata(i).FCP];
    
    % Forward roll with FIXED theta_obs and d_obs
    x_sim = sim_fixed_epi(x0_i, u_obs, cdata(i).theta, cdata(i).d, P);
    
    val(i).iso = cdata(i).iso;
    val(i).sim_y = x_sim(1, 2:end);    % simulated output gap (fractions)
    val(i).obs_y = cdata(i).y;          % observed output gap
    val(i).sim_b_cum = x_sim(3, 2:end); % simulated cumulative debt
    val(i).obs_b_cum = cumsum(cdata(i).b); % observed cumulative debt
    
    % RMSE for output (pp)
    val(i).rmse_y = sqrt(mean((val(i).sim_y(1:K_eval) - val(i).obs_y(1:K_eval)).^2)) * 100;
    
    % RMSE for cumulative debt (pp)
    val(i).rmse_b = sqrt(mean((val(i).sim_b_cum(1:K_eval) - val(i).obs_b_cum(1:K_eval)).^2)) * 100;
    
    % Temporal correlation
    if std(val(i).sim_y(1:K_eval)) > 1e-10 && std(val(i).obs_y(1:K_eval)) > 1e-10
        val(i).corr_y = corr(val(i).sim_y(1:K_eval)', val(i).obs_y(1:K_eval)');
    else
        val(i).corr_y = NaN;
    end
end

% Summary statistics
rmse_y_all = [val.rmse_y];
rmse_b_all = [val.rmse_b];
corr_y_all = [val(~isnan([val.corr_y])).corr_y];

fprintf('\n  Output Gap (pp):\n');
fprintf('    RMSE  — Median: %.2f  Mean: %.2f  P25: %.2f  P75: %.2f\n', ...
    median(rmse_y_all), mean(rmse_y_all), prctile(rmse_y_all,25), prctile(rmse_y_all,75));
fprintf('    Corr  — Median: %.3f  Mean: %.3f\n', median(corr_y_all), mean(corr_y_all));
fprintf('  Cumulative Debt (pp):\n');
fprintf('    RMSE  — Median: %.2f  Mean: %.2f\n', median(rmse_b_all), mean(rmse_b_all));

% Cross-country ranking
sim_cum_y = arrayfun(@(v) sum(v.sim_y(1:K_eval)), val) * 100;
obs_cum_y = arrayfun(@(v) sum(v.obs_y(1:K_eval)), val) * 100;
sim_cum_b = arrayfun(@(v) v.sim_b_cum(K_eval), val) * 100;
obs_cum_b = arrayfun(@(v) v.obs_b_cum(K_eval), val) * 100;
[rho_rank_y, p_rank_y] = corr(sim_cum_y', obs_cum_y', 'Type', 'Spearman');
[rho_rank_b, p_rank_b] = corr(sim_cum_b', obs_cum_b', 'Type', 'Spearman');
fprintf('  Cross-country ranking (Spearman):\n');
fprintf('    Output:  rho=%.3f (p=%.4f)\n', rho_rank_y, p_rank_y);
fprintf('    Debt:    rho=%.3f (p=%.4f)\n', rho_rank_b, p_rank_b);

% OECD average trajectory comparison
fprintf('\n  OECD Average: sim vs obs\n');
fprintf('  %10s %7s %7s | %7s %7s\n', 'Quarter', 'sim_y', 'obs_y', 'sim_bC', 'obs_bC');
for k = 1:K_eval
    sy = mean(arrayfun(@(v) v.sim_y(k), val)) * 100;
    oy = mean(arrayfun(@(v) v.obs_y(k), val)) * 100;
    sb = mean(arrayfun(@(v) v.sim_b_cum(k), val)) * 100;
    ob = mean(arrayfun(@(v) v.obs_b_cum(k), val)) * 100;
    fprintf('  %10s %7.2f %7.2f | %7.2f %7.2f\n', qord{k}, sy, oy, sb, ob);
end
fprintf('\n');


%% ========================================================================
%  STEP 3: WEIGHT GRID FOR PARETO FRONTIER
% =========================================================================
w_y = 100;   r_S = 10;   r_DI = 5;   r_CP = 0.5;

% Variance normalization for mortality weight
var_y = var(oecd.y(1:9));
var_d = var(delta_theta_t(1:9) .* oecd.theta(1:9));
if var_d < 1e-10, var_d = 1e-6; end
vn = var_y / var_d;

% Weight grids: VSL scales mortality weight, MCPF scales debt weight
VSL_grid  = [10 20 40 60 80 100 140];
MCPF_grid = [0.1 0.3 0.5 1.0 2.0];


%% ========================================================================
%  STAGE 1: PARETO FRONTIER
%  Endogenous theta with OECD-calibrated wave shocks.
%  Planner controls all three instruments (S, F^DI, F^CP).
% =========================================================================
fprintf('========================================\n');
fprintf('  STAGE 1: Pareto Frontier\n');
fprintf('========================================\n');

nv = length(VSL_grid); nm = length(MCPF_grid); np = nv * nm;
x0 = [0; 0; 0; 0.003; 0];  % common start: small theta seed

par = struct('VSL',zeros(np,1), 'MCPF',zeros(np,1), ...
    'cum_y',zeros(np,1), 'cum_d',zeros(np,1), 'end_b',zeros(np,1), ...
    'avg_S',zeros(np,1), 'avg_CP',zeros(np,1), 'avg_DI',zeros(np,1), ...
    'CP_share',zeros(np,1), 'J',zeros(np,1));
par.x = cell(np,1);  par.u = cell(np,1);

idx = 0;
for iv = 1:nv
    for im = 1:nm
        idx = idx + 1;
        w_d = w_y * VSL_grid(iv) * vn;
        w_b = w_y * MCPF_grid(im);
        W_b = w_b * 5;  % terminal debt penalty
        fprintf('  [%2d/%d] VSL=%3d MCPF=%.1f ', idx, np, VSL_grid(iv), MCPF_grid(im));
        
        % Solve: all instruments free, theta endogenous
        [xp, up, Jp] = solve_ilqr(w_y, w_d, w_b, W_b, r_S, r_DI, r_CP, ...
                                   x0, P, [], [], []);
        
        par.VSL(idx) = VSL_grid(iv);
        par.MCPF(idx) = MCPF_grid(im);
        par.cum_y(idx) = mean(xp(1, 2:end)) * 100;
        par.cum_d(idx) = sum(xp(2, 2:end)) * baseline_qmort * 100;
        par.end_b(idx) = xp(3, end) * 100;
        par.avg_S(idx) = mean(up(1, 1:9));
        par.avg_CP(idx) = mean(up(3, 1:9)) * 100;
        par.avg_DI(idx) = mean(up(2, 1:9)) * 100;
        tf = par.avg_CP(idx) + par.avg_DI(idx);
        par.CP_share(idx) = par.avg_CP(idx) / max(tf, 1e-6);
        par.J(idx) = Jp;
        par.x{idx} = xp;
        par.u{idx} = up;
        
        fprintf('-> y=%.1f%% d=%.4f%% b=%.0fpp\n', ...
            par.cum_y(idx), par.cum_d(idx), par.end_b(idx));
    end
end

% No-intervention benchmark
x_noint = sim_noint(x0, P);


%% ========================================================================
%  STAGE 1 FIGURES
% =========================================================================
qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21',...
        'Q3.21','Q4.21','Q1.22','Q2.22','Q3.22','Q4.22'};

% --- Figure 1: Pareto Frontier (3 projections) ---
figure('Name','Pareto Frontier','Color','w','Position',[50 50 1400 450]);
subplot(1,3,1);
scatter(par.cum_d, par.cum_y, 70, par.VSL, 'filled', 'MarkerEdgeColor','k','LineWidth',.3);
xlabel('Cum. Excess Deaths (% pop.)'); ylabel('Avg Output Gap (%)');
title('Mortality–Output'); cb=colorbar; cb.Label.String='VSL'; grid on;
subplot(1,3,2);
scatter(par.cum_d, par.end_b, 70, par.MCPF, 'filled', 'MarkerEdgeColor','k','LineWidth',.3);
xlabel('Cum. Excess Deaths (% pop.)'); ylabel('Terminal Debt (pp GDP)');
title('Mortality–Debt'); cb=colorbar; cb.Label.String='MCPF'; grid on;
subplot(1,3,3);
scatter(par.cum_y, par.end_b, 70, par.CP_share*100, 'filled', 'MarkerEdgeColor','k','LineWidth',.3);
xlabel('Avg Output Gap (%)'); ylabel('Terminal Debt (pp GDP)');
title('Output–Debt'); cb=colorbar; cb.Label.String='CP share (%)'; grid on;
sgtitle('Stage 1: Pareto Frontier (OECD-calibrated)','FontWeight','bold');

% --- Figure 2: Time profiles for selected frontier points ---
sel_lab = {'Pro-Health','Balanced','Pro-Fiscal','High-VSL Fiscal','Low-VSL Loose'};
sel_v = [140 60 20 100 10]; sel_m = [0.1 0.3 2.0 1.0 0.1]; ns = length(sel_v);
si = zeros(ns,1);
for s = 1:ns
    [~, si(s)] = min(abs(par.VSL-sel_v(s)) + abs(par.MCPF-sel_m(s)));
end
cols = [.8 .1 .1; .2 .5 .8; .1 .7 .2; .7 .4 .1; .5 .1 .7];

figure('Name','Time Profiles','Color','w','Position',[30 30 1500 700]);
titles_sub = {'Containment (S)','CP (% GDP)','DI (% GDP)',...
              'Output Gap (pp)','Excess Mortality (P-score %)','Debt (pp GDP)'};
bar_CP = zeros(N, ns); bar_DI = zeros(N, ns);
for s = 1:ns
    bar_CP(:,s) = par.u{si(s)}(3,:)' * 100;
    bar_DI(:,s) = par.u{si(s)}(2,:)' * 100;
end

for p = 1:6
    subplot(2,3,p); hold on;
    switch p
        case 1  % Containment
            for s=1:ns, plot(1:N, par.u{si(s)}(1,:), '-', 'Color', cols(s,:), 'LineWidth', 1.8); end
            plot(1:N, oecd.S, 'k:', 'LineWidth', 2); ylabel('S [0,1]'); ylim([0 1]);
        case 2  % CP
            bh = bar(1:N, bar_CP, 'grouped');
            for s=1:ns, bh(s).FaceColor = cols(s,:); bh(s).FaceAlpha = 0.7; end
            plot(1:N, oecd.FCP*100, 'k:o', 'LineWidth', 2, 'MarkerSize', 4); ylabel('% GDP');
        case 3  % DI
            bh = bar(1:N, bar_DI, 'grouped');
            for s=1:ns, bh(s).FaceColor = cols(s,:); bh(s).FaceAlpha = 0.7; end
            plot(1:N, oecd.FDI*100, 'k:o', 'LineWidth', 2, 'MarkerSize', 4); ylabel('% GDP');
        case 4  % Output gap
            for s=1:ns, plot(0:N, par.x{si(s)}(1,:)*100, '-', 'Color', cols(s,:), 'LineWidth', 1.8); end
            plot(1:N, oecd.y*100, 'k:', 'LineWidth', 2); yline(0,'--k'); ylabel('pp GDP');
        case 5  % Excess mortality
            for s=1:ns, plot(1:N, par.x{si(s)}(2,2:end)*100, '-', 'Color', cols(s,:), 'LineWidth', 1.8); end
            plot(1:N, oecd.d*100, 'k:', 'LineWidth', 2); ylabel('P-score (%)');
        case 6  % Debt
            for s=1:ns, plot(0:N, par.x{si(s)}(3,:)*100, '-', 'Color', cols(s,:), 'LineWidth', 1.8); end
            plot(0:N, [0, cumsum(oecd.b)]*100, 'k:', 'LineWidth', 2); ylabel('pp GDP');
    end
    title(titles_sub{p}); grid on;
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'XTickLabelRotation', 45, 'FontSize', 6);
end
legend([sel_lab {'OECD observed'}], 'Location', 'best', 'FontSize', 6);
sgtitle('Stage 1: Optimal Strategies along the Pareto Frontier','FontWeight','bold');


%% ========================================================================
%  STAGE 2: COUNTRY COUNTERFACTUALS
%  Fix S_obs, theta_obs, d_obs. Optimize only F^CP and F^DI.
%  This is the PRIMARY COUNTERFACTUAL of the paper.
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STAGE 2: Country Counterfactuals\n');
fprintf('  (Fix S_obs + theta_obs + d_obs)\n');
fprintf('========================================\n');

% Reference weights for counterfactual (can be varied for sensitivity)
VSL_cf = 60;  MCPF_cf = 0.3;
w_d_cf = w_y * VSL_cf * vn;
w_b_cf = w_y * MCPF_cf;
W_b_cf = w_b_cf * 5;

res = struct();
for i = 1:n_c
    iso = cdata(i).iso;
    fprintf('  [%2d/%d] %s ', i, n_c, iso);
    
    % Country-specific initial condition
    x0_i = [0; 0; 0; max(cdata(i).theta(1), 1e-6); 0];
    
    % Observed forcing variables
    fS  = cdata(i).S;      % containment trajectory (fixed)
    fTh = [cdata(i).theta(1), cdata(i).theta];  % theta trajectory (fixed)
    fD  = [0, cdata(i).d];                       % d trajectory (fixed, for fear term)
    % Extend post-data quarters with decay
    for kk = 10:N
        fTh(kk+1) = fTh(kk) * 0.7;
        fD(kk+1)  = fD(kk) * 0.5;
    end
    
    % (a) Observed cost: actual policy with fixed epidemiology
    u_obs = [cdata(i).S; cdata(i).FDI; cdata(i).FCP];
    x_obs = sim_fixed_epi(x0_i, u_obs, cdata(i).theta, cdata(i).d, P);
    J_obs = eval_J(x_obs, u_obs, w_y, w_d_cf, w_b_cf, W_b_cf, r_S, r_DI, r_CP, P);
    
    % (b) Conditional optimum: fix S + theta + d, optimize fiscal
    [xcA, ucA, JcA] = solve_ilqr(w_y, w_d_cf, w_b_cf, W_b_cf, ...
                                  r_S, r_DI, r_CP, x0_i, P, fS, fTh, fD);
    
    % (c) Full optimum: optimize all instruments (theta endogenous)
    [xf, uf, Jf] = solve_ilqr(w_y, w_d_cf, w_b_cf, W_b_cf, ...
                                r_S, r_DI, r_CP, x0_i, P, [], [], []);
    
    % Store results
    res(i).iso = iso;
    res(i).J_obs = J_obs;  res(i).J_cond = JcA;  res(i).J_full = Jf;
    res(i).x_obs = x_obs;  res(i).x_cond = xcA;  res(i).x_full = xf;
    res(i).u_obs = u_obs;  res(i).u_cond = ucA;  res(i).u_full = uf;
    
    % Welfare decomposition
    res(i).comp_gap = J_obs - JcA;     % cost of suboptimal fiscal composition
    res(i).cont_gap = JcA - Jf;        % cost of suboptimal containment
    res(i).total_gap = J_obs - Jf;
    
    % Debt decomposition
    res(i).obs_end_b  = x_obs(3, end) * 100;
    res(i).cond_end_b = xcA(3, end) * 100;
    res(i).excess_debt = (x_obs(3,end) - xcA(3,end)) * 100;
    
    % Output gap
    res(i).obs_cum_y  = mean(x_obs(1, 2:10)) * 100;
    res(i).cond_cum_y = mean(xcA(1, 2:10)) * 100;
    
    fprintf('-> ExDebt=%+.1fpp  CompGap=%.1f  ContGap=%.1f\n', ...
        res(i).excess_debt, res(i).comp_gap, res(i).cont_gap);
end


%% ========================================================================
%  STAGE 2 FIGURES
% =========================================================================

% --- Figure 3: Excess debt from suboptimal composition ---
figure('Name','Excess Debt','Color','w','Position',[50 50 1200 500]);
ed = [res.excess_debt]; [~, si3] = sort(ed, 'descend');
bar(ed(si3), 'FaceColor', [.7 .2 .2]); hold on; yline(0, '--k');
set(gca, 'XTick', 1:n_c, 'XTickLabel', {res(si3).iso}, ...
    'XTickLabelRotation', 55, 'FontSize', 7);
ylabel('Excess Debt (pp GDP)');
title('Stage 2: Excess Debt from Suboptimal Fiscal Composition'); grid on;

% --- Figure 4: Welfare decomposition ---
figure('Name','Welfare Decomp','Color','w','Position',[50 50 1200 500]);
cg = [res.comp_gap]; kg = [res.cont_gap];
[~, si4] = sort([res.total_gap], 'descend');
b2 = bar([cg(si4)', kg(si4)'], 'stacked');
b2(1).FaceColor = [.8 .3 .3]; b2(2).FaceColor = [.3 .3 .8];
set(gca, 'XTick', 1:n_c, 'XTickLabel', {res(si4).iso}, ...
    'XTickLabelRotation', 55, 'FontSize', 7);
legend('Composition Gap', 'Containment Gap', 'Location', 'NE');
title('Welfare Decomposition'); ylabel('J units'); grid on;

% --- Figure 5: Countries on Pareto frontier ---
figure('Name','Countries on Frontier','Color','w','Position',[50 50 1200 500]);
subplot(1,2,1);
scatter(par.cum_d, par.end_b, 50, par.VSL, 'filled', 'MarkerEdgeColor', [.5 .5 .5]);
hold on;
for i = 1:n_c
    od = sum(res(i).x_obs(2, 2:end)) * baseline_qmort * 100;
    plot(od, res(i).obs_end_b, 'kx', 'MarkerSize', 6, 'LineWidth', 1);
end
xlabel('Cum. Excess Deaths (% pop.)'); ylabel('Terminal Debt (pp GDP)');
title('Mortality–Debt'); cb=colorbar; cb.Label.String='VSL'; grid on;

subplot(1,2,2);
scatter(par.cum_y, par.end_b, 50, par.CP_share*100, 'filled', 'MarkerEdgeColor', [.5 .5 .5]);
hold on;
for i = 1:n_c
    plot(res(i).obs_cum_y, res(i).obs_end_b, 'kx', 'MarkerSize', 6, 'LineWidth', 1);
end
xlabel('Avg Output Gap (%)'); ylabel('Terminal Debt (pp GDP)');
title('Output–Debt'); cb=colorbar; cb.Label.String='CP%'; grid on;
sgtitle('Countries (x) on Pareto Frontier','FontWeight','bold');


%% ========================================================================
%  EXPORT RESULTS
% =========================================================================
fprintf('\n--- Exporting results ---\n');

% Country results
Tout = table({res.iso}', [res.obs_cum_y]', [res.cond_cum_y]', ...
    [res.obs_end_b]', [res.cond_end_b]', [res.excess_debt]', ...
    [res.comp_gap]', [res.cont_gap]', ...
    'VariableNames', {'Country','obs_y','cond_y','obs_b','cond_b', ...
                      'excess_debt','comp_gap','cont_gap'});
writetable(Tout, 'calibrated_country_results.csv');

% Pareto frontier
Tpar = table(par.VSL, par.MCPF, par.cum_y, par.cum_d, par.end_b, ...
    par.avg_S, par.avg_CP, par.avg_DI, par.CP_share, ...
    'VariableNames', {'VSL','MCPF','cum_y','cum_d','end_b', ...
                      'avg_S','avg_CP','avg_DI','CP_share'});
writetable(Tpar, 'calibrated_pareto_frontier.csv');

% Validation
Tval = table({val.iso}', [val.rmse_y]', [val.rmse_b]', [val.corr_y]', ...
    'VariableNames', {'Country','rmse_y_pp','rmse_b_pp','corr_y'});
writetable(Tval, 'calibrated_validation.csv');

fprintf('  Saved: calibrated_country_results.csv\n');
fprintf('  Saved: calibrated_pareto_frontier.csv\n');
fprintf('  Saved: calibrated_validation.csv\n');
fprintf('\n=== COMPLETE ===\n');


%% ========================================================================
%  HELPER FUNCTIONS
% =========================================================================

% --- Forward roll with FIXED theta_obs and d_obs (Stage 2 validation) ---
function x = sim_fixed_epi(x0, u, theta_obs, d_obs, P)
    N = P.N; x = zeros(P.n_x, N+1); x(:,1) = x0;
    for k = 1:N
        % Fix theta and d at observed values
        if k <= length(theta_obs), x(4,k) = theta_obs(k); end
        d_fear = 0;
        if k <= length(d_obs), d_fear = d_obs(k); end
        
        % Output (endogenous, uses d_obs for fear term)
        x(1,k+1) = P.rho_y*x(1,k) + P.psi*u(1,k)*x(1,k) - P.alpha_S*u(1,k) ...
            + (P.alpha_F_CP + P.eta_tilde*u(1,k) - P.eta_p*x(1,k))*u(3,k) ...
            + P.alpha_F_DI*x(5,k) + P.beta_fear*d_fear + P.eps_y_vec(k+1);
        % Mortality (fixed)
        x(2,k+1) = d_fear;  % store d_obs for next period's fear term
        % Debt (endogenous)
        x(3,k+1) = (1+P.r_int)*x(3,k) - P.gamma_y*x(1,k) ...
            + P.kappa_F_CP*u(3,k) + P.kappa_F_DI*u(2,k) + P.c_H*x(4,k);
        % Theta (fixed)
        if k < length(theta_obs)
            x(4,k+1) = theta_obs(min(k+1, length(theta_obs)));
        else
            x(4,k+1) = x(4,k) * 0.7;  % decay post-data
        end
        % Auxiliary (lagged DI)
        x(5,k+1) = u(2,k);
    end
end

% --- Forward roll with endogenous theta (Stage 1 / no-intervention) ---
function x = sim_noint(x0, P)
    N = P.N; x = zeros(P.n_x, N+1); x(:,1) = x0;
    for k = 1:N
        x(1,k+1) = P.rho_y*x(1,k) + P.beta_fear*x(2,k) + P.eps_y_vec(k+1);
        x(2,k+1) = P.delta_theta_t(k)*x(4,k);
        x(3,k+1) = (1+P.r_int)*x(3,k) - P.gamma_y*x(1,k) + P.c_H*x(4,k);
        x(4,k+1) = P.rho_theta_t(k)*x(4,k) + P.wave_shock_vec(k+1);
        x(5,k+1) = 0;
    end
end

% --- Evaluate objective J ---
function J = eval_J(x, u, wy, wd, wb, Wb, rS, rD, rC, P)
    N = P.N; Q = diag([wy,wd,wb,0,0]); QN = diag([0,0,Wb,0,0]); R = diag([rS,rD,rC]);
    J = 0;
    for k = 1:N
        J = J + P.beta_disc^(k-1) * (0.5*x(:,k)'*Q*x(:,k) + 0.5*u(:,k)'*R*u(:,k));
    end
    J = J + P.beta_disc^N * 0.5 * x(:,N+1)' * QN * x(:,N+1);
end

% --- Augmented Lagrangian cost ---
function J = alc(x, u, Qb, QN, R, b, N, lm, mu, Cu, cb)
    J = 0;
    for k = 1:N
        sc = b^(k-1) * (0.5*x(:,k)'*Qb*x(:,k) + 0.5*u(:,k)'*R*u(:,k));
        cv = Cu*u(:,k) - cb;
        act = (cv > 0) | (lm(:,k) > 0);
        Im = diag(mu(:,k) .* act);
        J = J + sc + lm(:,k)'*cv + 0.5*cv'*Im*cv;
    end
    J = J + b^N * 0.5 * x(:,N+1)' * QN * x(:,N+1);
end


%% ========================================================================
%  iLQR SOLVER (Augmented Lagrangian)
%
%  Inputs:
%    w_y, w_d, w_b, W_b  — state weights
%    r_S, r_DI, r_CP     — control weights
%    x0                   — initial state [5x1]
%    P                    — parameter struct
%    fS                   — fixed S trajectory (empty = optimize)
%    fTh                  — fixed theta trajectory (empty = endogenous)
%    fD                   — fixed d trajectory (empty = endogenous)
%
%  Outputs:
%    x_opt [5 x N+1]     — optimal state trajectory
%    u_opt [3 x N]        — optimal control trajectory
%    J_opt                — optimal cost
% =========================================================================
function [x_opt, u_opt, J_opt] = solve_ilqr(w_y, w_d, w_b, W_b, ...
                                             r_S, r_DI, r_CP, x0, P, fS, fTh, fD)
    N = P.N; nx = P.n_x; nu = P.n_u;
    Qb = diag([w_y, w_d, w_b, 0, 0]);
    QN = diag([0, 0, W_b, 0, 0]);
    R  = diag([r_S, r_DI, r_CP]);
    
    doS  = ~isempty(fS);
    doTh = ~isempty(fTh);
    doD  = ~isempty(fD);
    
    % --- Dynamics: full nonlinear transition ---
    ftv = @(x, u, k) [ ...
        P.rho_y*x(1) + P.psi*u(1)*x(1) - P.alpha_S*u(1) ...
        + (P.alpha_F_CP + P.eta_tilde*u(1) - P.eta_p*x(1))*u(3) ...
        + P.alpha_F_DI*x(5) + P.beta_fear*x(2); ...
        P.delta_theta_t(k)*x(4); ...
        (1+P.r_int)*x(3) - P.gamma_y*x(1) + P.kappa_F_CP*u(3) ...
        + P.kappa_F_DI*u(2) + P.c_H*x(4); ...
        P.rho_theta_t(k)*(1 - P.phi_S*u(1))*x(4); ...
        u(2)];
    
    fw = @(x, u, k) ftv(x, u, k) + [P.eps_y_vec(k+1); 0; 0; P.wave_shock_vec(k+1); 0];
    
    % --- Jacobians ---
    Af = @(x, u, k) [ ...
        P.rho_y + P.psi*u(1) - P.eta_p*u(3), P.beta_fear, 0, 0, P.alpha_F_DI; ...
        0, 0, 0, P.delta_theta_t(k), 0; ...
        -P.gamma_y, 0, 1+P.r_int, P.c_H, 0; ...
        0, 0, 0, P.rho_theta_t(k)*(1-P.phi_S*u(1)), 0; ...
        0, 0, 0, 0, 0];
    
    Bf = @(x, u, k) [ ...
        P.psi*x(1) - P.alpha_S + P.eta_tilde*u(3), 0, ...
            P.alpha_F_CP + P.eta_tilde*u(1) - P.eta_p*x(1); ...
        0, 0, 0; ...
        0, P.kappa_F_DI, P.kappa_F_CP; ...
        -P.rho_theta_t(k)*P.phi_S*x(4), 0, 0; ...
        0, 1, 0];
    
    % --- Initialize trajectory ---
    xb = zeros(nx, N+1); ub = zeros(nu, N); xb(:,1) = x0;
    for k = 1:N
        if doS
            ub(1,k) = fS(k);
        else
            ub(1,k) = max(0.05, min(P.S_max, 0.6 - 0.04*(k-1)));
        end
        ub(2,k) = 0.002;
        ub(3,k) = max(0, 0.04 - 0.003*(k-1));
        xb(:,k+1) = fw(xb(:,k), ub(:,k), k);
        if doTh, xb(4,k+1) = fTh(k+1); end
        if doD,  xb(2,k+1) = fD(k+1); end
    end
    
    % --- Augmented Lagrangian iLQR ---
    max_outer = 25; max_inner = 50;
    tol_inner = 1e-4; tol_outer = 1e-4;
    penalty_growth = 2.5; reg_base = 1e-4;
    
    nc_con = 6;  % number of box constraints
    lm = zeros(nc_con, N);
    mu = 10 * ones(nc_con, N);
    Cu = [-1 0 0; 1 0 0; 0 -1 0; 0 1 0; 0 0 -1; 0 0 1];
    cb = [0; P.S_max; 0; P.FDI_max; 0; P.FCP_max];
    
    for oo = 1:max_outer
        for ii = 1:max_inner
            % --- Linearize ---
            Ak = cell(N,1); Bk = cell(N,1);
            for k = 1:N
                Ak{k} = Af(xb(:,k), ub(:,k), k);
                Bk{k} = Bf(xb(:,k), ub(:,k), k);
                % Zero out rows for fixed states
                if doTh, Ak{k}(4,:) = 0; Bk{k}(4,:) = 0; end
                if doD,  Ak{k}(2,:) = 0; Bk{k}(2,:) = 0; end
            end
            
            % --- Backward pass (Riccati) ---
            Pm = cell(N+1,1); pm = cell(N+1,1);
            K = cell(N,1); kf = cell(N,1);
            Pm{N+1} = QN * P.beta_disc^N;
            pm{N+1} = QN * P.beta_disc^N * xb(:,N+1);
            
            rr = reg_base; bw_ok = false;
            while ~bw_ok && rr < 1e6
                bw_ok = true;
                for k = N:-1:1
                    Qk = Qb * P.beta_disc^(k-1);
                    Rk = R * P.beta_disc^(k-1);
                    A = Ak{k}; B = Bk{k};
                    
                    cv = Cu * ub(:,k) - cb;
                    act = (cv > 0) | (lm(:,k) > 0);
                    Im = diag(mu(:,k) .* act);
                    
                    Qx  = Qk * xb(:,k) + A' * pm{k+1};
                    Qxx = Qk + A' * Pm{k+1} * A;
                    Qux = B' * Pm{k+1} * A;
                    Qu  = Rk * ub(:,k) + B' * pm{k+1} + Cu' * (lm(:,k) + Im*cv);
                    Quu = Rk + B' * Pm{k+1} * B + Cu' * Im * Cu + rr * eye(nu);
                    
                    [~, pc] = chol(Quu);
                    if pc > 0, bw_ok = false; rr = rr * 10; break; end
                    
                    K{k}  = Quu \ Qux;
                    kf{k} = -Quu \ Qu;
                    
                    % Zero out gains for fixed controls
                    if doS, K{k}(1,:) = 0; kf{k}(1) = 0; end
                    
                    pm{k} = Qx + K{k}' * Quu * kf{k};
                    Pm{k} = Qxx - K{k}' * Quu * K{k};
                end
            end
            if ~bw_ok, break; end
            
            % --- Forward pass with line search ---
            al = 1;
            co = alc(xb, ub, Qb, QN, R, P.beta_disc, N, lm, mu, Cu, cb);
            ls_ok = false;
            
            while al > 1e-8
                xn = zeros(nx, N+1); un = zeros(nu, N);
                xn(:,1) = x0;
                for k = 1:N
                    un(:,k) = ub(:,k) + al*kf{k} - K{k}*(xn(:,k) - xb(:,k));
                    
                    % Enforce fixed S
                    if doS, un(1,k) = fS(k);
                    else,   un(1,k) = max(0, min(P.S_max, un(1,k))); end
                    % Enforce bounds on fiscal instruments
                    un(2,k) = max(0, min(P.FDI_max, un(2,k)));
                    un(3,k) = max(0, min(P.FCP_max, un(3,k)));
                    
                    xn(:,k+1) = fw(xn(:,k), un(:,k), k);
                    
                    % Override fixed states
                    if doTh, xn(4,k+1) = fTh(k+1); end
                    if doD,  xn(2,k+1) = fD(k+1); end
                end
                
                cn = alc(xn, un, Qb, QN, R, P.beta_disc, N, lm, mu, Cu, cb);
                if cn < co, ls_ok = true; break;
                else, al = al * 0.5; end
            end
            if ~ls_ok, break; end
            
            dx = norm(xn - xb, 'fro') / (norm(xb, 'fro') + 1e-12);
            xb = xn; ub = un;
            if dx < tol_inner, break; end
        end
        
        % --- Outer loop: update multipliers ---
        mv = 0;
        for k = 1:N
            cv = Cu * ub(:,k) - cb;
            mv = max(mv, max(cv));
            lm(:,k) = max(0, lm(:,k) + mu(:,k) .* cv);
            mu(:,k) = mu(:,k) * penalty_growth;
        end
        if mv < tol_outer, break; end
    end
    
    x_opt = xb; u_opt = ub;
    J_opt = alc(x_opt, u_opt, Qb, QN, R, P.beta_disc, N, ...
                zeros(nc_con,N), zeros(nc_con,N), Cu, cb);
end
