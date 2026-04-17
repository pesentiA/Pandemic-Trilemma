%% ========================================================================
%  PANDEMIC TRILEMMA — CALIBRATION & VALIDATION (V3)
%
%  SPECIFICATION:
%    CP operates exclusively through the persistence channel.
%    No contemporaneous output effect of CP. No S×F or level F interactions.
%    CP deployed under lockdown preserves firm-worker matches; the output
%    effect materializes as reduced persistence when containment eases.
%
%  Output transition:
%    y_{k+1} = mu_y + (rho_y + psi*S_k + eta_p*w_k)*y_k
%              - alpha_S*S_k + alpha_DI*z_k + beta_d*d_k + eps_k
%
%  Debt transition:
%    b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%              + kappa_cp*F^CP_k + kappa_di*F^DI_k
%
%  State:   x = (y, b, w, z)'   w = F^CP_{k-1},  z = F^DI_{k-1}
%  Control: u = (F^CP, F^DI)'
%
%  Regression: plm TWFE, Q4.2019–Q2.2022 (N=418), HC1 clustered SE
%  Reference: Q4.2019 = 0 for Quarter FE
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Calibration V3 (persistence CP) ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS
% =========================================================================

% --- Output equation (V3: CP through persistence only) ---
rho_y      = 0.4202;       % beta(y_lag1)
psi        = 0.2043;       % beta(S*y_lag1), fraction units
alpha_S    = 0.0265;       % -beta(S), stored positive
eta_p      = -1.2260;      % beta(FCP_lag2 * y_lag1), fraction units
alpha_F_DI = 0.2126;       % beta(FDI_lag2), lag-2 level effect
beta_fear  = -0.0225;      % beta(d)

% --- Debt equation (Country FE only, no Quarter FE) ---
r_int      = 0.001;
gamma_y    = 0.200;
kappa_F_DI = 0.464;
c_H        = 0.00;

% --- Country-specific kappa_CP ---
cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};
kappa_cp_i = [0.391, 0.355, 0.208, 0.295, 0.243, 0.190, 0.229, 0.272, ...
              0.176, 0.209, 0.327, 0.197, 0.298, 0.218, 0.208, 0.241, ...
              0.351, 0.261, 0.352, 0.347, 0.324, 0.197, 0.260, 0.286, ...
              0.325, 0.251, 0.276, 0.320, 0.299, 0.233, 0.365, 0.316, ...
              0.203, 0.397, 0.329, 0.274, 0.173, 0.326];

% --- Quarter FE (pp, normalized: Q4.2019 = 0) ---
%  Raw plm output shifted by -qfe(Q4.2019) = -0.236176
qfe_pp = [-1.2260, -8.9610,  1.2572, -0.4427,  0.5402, ...
            0.8549,  0.8096,  1.2985,  0.1524, -0.1484];

% --- Country FE: Output (pp, shifted by +0.236176 from raw plm) ---
cfe_y_val = [ 0.4724, -0.9709,  0.2991, -0.6141,  0.9424,  0.3107,  1.2922, -0.2214, ...
             -2.4363, -1.0706,  0.2942, -3.4472, -0.9680, -0.9503, -1.3205, -2.2762, ...
             -0.2184, -1.4786,  5.2005, -3.3472,  0.8369, -0.0990, -0.6646,  0.2799, ...
              0.4441,  1.1671, -0.3224, -1.9092,  0.4297,  0.7366, -0.7778, -0.5831, ...
             -2.1751, -0.1458, -1.2736,  0.5762,  2.9973,  0.6029];

% --- Country FE: Debt (pp, unchanged) ---
cfe_b_val = [-0.6746, -0.2777, -0.4649, -0.2143, -0.4844, -0.9781,  0.1462,  0.4048, ...
             -1.0619, -1.0287, -1.2539, -1.2272, -0.5830, -0.5348, -0.8409, -0.8974, ...
             -0.6096, -0.6704,  0.9444, -1.0335,  0.2331, -0.6479, -0.2022, -0.2991, ...
             -0.5080,  0.2937, -0.0212, -1.6298, -0.4222, -0.4402,  0.3454, -0.6348, ...
             -1.4319, -0.0641, -0.4026, -0.3780,  0.3130,  0.6681];

% --- Objective weights ---
beta_disc = 0.99;
w_y = 100;  w_b = 30;  W_b = 150;  r_cp = 3;  r_di = 5;

% --- Dimensions ---
N = 10;  K_act = 8;  nx = 4;  nu = 2;

% --- Build eps_y vector (fractions) ---
eps_y_vec = zeros(1, N+1);
for k = 1:N
    if k <= length(qfe_pp), eps_y_vec(k+1) = qfe_pp(k) / 100; end
end

% --- Build lookup maps ---
cfe_y_map  = containers.Map(cfe_iso, cfe_y_val / 100);
cfe_b_map  = containers.Map(cfe_iso, cfe_b_val / 100);
kappa_map  = containers.Map(cfe_iso, kappa_cp_i);

% --- Control bounds ---
u_lo = [0; 0];  u_hi = [0.20; 0.10];

% --- Pack parameters ---
P = struct( ...
    'rho_y',rho_y, 'psi',psi, 'alpha_S',alpha_S, ...
    'eta_p',eta_p, 'alpha_F_DI',alpha_F_DI, 'beta_fear',beta_fear, ...
    'r_int',r_int, 'gamma_y',gamma_y, ...
    'kappa_F_DI',kappa_F_DI, 'c_H',c_H, ...
    'eps_y_vec',eps_y_vec, 'beta_disc',beta_disc, ...
    'w_y',w_y, 'w_b',w_b, 'W_b',W_b, 'r_cp',r_cp, 'r_di',r_di, ...
    'N',N, 'K_act',K_act, 'nx',nx, 'nu',nu, 'u_lo',u_lo, 'u_hi',u_hi);

fprintf('  N = %d   K_act = %d   nx = %d (y, b, w, z)\n', N, K_act, nx);
fprintf('  rho_y = %.4f   psi = %.4f   alpha_S = %.4f\n', rho_y, psi, alpha_S);
fprintf('  eta_p = %.4f (CP persistence channel)\n', eta_p);
fprintf('  alpha_DI = %.4f (DI level, lag-2)\n', alpha_F_DI);
fprintf('  beta_d = %.4f\n', beta_fear);
fprintf('  Effective persistence at OECD avg (S=0.44, FCP_lag2=0.05):\n');
fprintf('    rho_eff = %.4f + %.4f*0.44 + (%.4f)*0.05 = %.4f\n', ...
    rho_y, psi, eta_p, rho_y + psi*0.44 + eta_p*0.05);
fprintf('    Without CP: rho = %.4f + %.4f*0.44 = %.4f\n', ...
    rho_y, psi, rho_y + psi*0.44);
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
%  STEP 1: VALIDATION
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

% --- Trajectory ---
fprintf('  OECD Median Trajectory (pp):\n');
fprintf('  %8s %9s %9s %9s\n', 'Quarter', 'Observed', 'Model', 'rho_eff');
for k = 1:K_act
    obs_k = arrayfun(@(c) c.y(k), cdata) * 100;
    sim_k = arrayfun(@(c) c.sim_y(k), cdata) * 100;
    % median effective persistence
    rho_effs = zeros(n_c, 1);
    for i = 1:n_c
        wk = 0; if k >= 3, wk = cdata(i).FCP(k-2); end
        rho_effs(i) = P.rho_y + P.psi * cdata(i).S(k) + P.eta_p * wk;
    end
    fprintf('  %8s %+9.2f %+9.2f %9.3f\n', qlbl{k}, median(obs_k), median(sim_k), median(rho_effs));
end

% --- Fiscal contribution ---
fprintf('\n  Fiscal contribution (Full - NoF, cum %dQ):\n', K_act);
diffs = zeros(n_c, 1);
for i = 1:n_c
    xs_nof = forward_roll(zeros(1,N), zeros(1,N), ...
        cdata(i).S, cdata(i).theta, cdata(i).d, ...
        cdata(i).mu_y, cdata(i).mu_b, cdata(i).kappa_cp, P);
    diffs(i) = (sum(cdata(i).sim_y(1:K_act)) - sum(xs_nof(1, 2:K_act+1))) * 100;
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

% --- (2) AC(1) ---
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
%  VISUALIZATION
% =========================================================================

% --- Fig 1: Validation ---
sim_y_all = reshape([cdata.sim_y], N, n_c)' * 100;
obs_y_all = reshape([cdata.y],    N, n_c)' * 100;
sim_b_all = reshape([cdata.sim_b], N, n_c)' * 100;
obs_b_all = zeros(n_c, N);
for i = 1:n_c, obs_b_all(i,:) = cdata(i).obs_b_cum * 100; end

figure('Name','V3 Validation','Color','w','Position',[50 50 1000 400]);

subplot(1,2,1); hold on;
fill_iqr(1:N, sim_y_all, [0 .4 .8], .15);
fill_iqr(1:N, obs_y_all, [.5 .5 .5], .12);
plot(1:N, median(sim_y_all), 'b-o', 'LineWidth', 2, 'MarkerSize', 4);
plot(1:N, median(obs_y_all), 'k--s', 'LineWidth', 2, 'MarkerSize', 4);
xline(K_act+.5, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]); grid on;
set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, 'XTickLabelRotation', 45);
ylabel('pp of potential GDP'); title('Output Gap (V3: persistence CP)');
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

sgtitle('V3 Validation: Persistence-Channel CP','FontWeight','bold');

fprintf('\n=== CALIBRATION COMPLETE ===\n');


%% ########################################################################
%  FUNCTIONS (V3: nx=4, persistence-channel CP)
%  ########################################################################

function xs = forward_roll(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
% V3: x = (y, b, w, z)
% y_{k+1} = mu_y + (rho + psi*S + eta_p*w)*y - alpha_S*S + alpha_DI*z + beta_d*d + eps
% CP enters ONLY through effective persistence (eta_p * w * y)
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
        % Effective persistence
        rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;
        % Output: CP only through persistence
        xs(1,k+1) = mu_y + rho_eff*y - P.alpha_S*Sk ...
            + P.alpha_F_DI*z + P.beta_fear*dk + ey;
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
% V3 Jacobians: x=(y,b,w,z), u=(fcp,fdi)
%   dy/dy = rho_y + psi*S_k + eta_p*w_k     (state-dependent!)
%   dy/dw = eta_p * y_k                      (CP effect on y through w)
%   dy/dz = alpha_DI
%   dy/dfcp = 0   dy/dfdi = 0                (no contemporaneous output effect)
    y=x(1); w=x(3); Sk=0;
    if k<=length(S), Sk=S(k); end
    A = [P.rho_y+P.psi*Sk+P.eta_p*w,  0,         P.eta_p*y,  P.alpha_F_DI;
         -P.gamma_y,                    1+P.r_int, 0,          0;
         0,                             0,         0,          0;
         0,                             0,         0,          0];
    B = [0,    0;
         kcp,  P.kappa_F_DI;
         1,    0;
         0,    1];
end

function xn = dynamics_step(x, u, k, S, theta, d, mu_y, mu_b, kcp, P)
    y=x(1); b=x(2); w=x(3); z=x(4); fcp=u(1); fdi=u(2);
    Sk=0; thk=0; dk=0; ey=0;
    if k<=length(S), Sk=S(k); end
    if k<=length(theta), thk=theta(k); end
    if k<=length(d), dk=d(k); end
    if k+1<=length(P.eps_y_vec), ey=P.eps_y_vec(k+1); end
    rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;
    xn = [mu_y + rho_eff*y - P.alpha_S*Sk ...
              + P.alpha_F_DI*z + P.beta_fear*dk + ey;
          mu_b + (1+P.r_int)*b - P.gamma_y*y ...
              + kcp*fcp + P.kappa_F_DI*fdi + P.c_H*thk;
          fcp;
          fdi];
end

function fill_iqr(x,data,col,alpha)
    sd=sort(data); n=size(sd,1);
    p25=sd(max(1,round(0.25*n)),:);
    p75=sd(max(1,round(0.75*n)),:);
    fill([x,fliplr(x)],[p25,fliplr(p75)],col,...
        'FaceAlpha',alpha,'EdgeColor','none');
end

%% ========================================================================
%  INSERT THIS BLOCK BEFORE the line "% FUNCTIONS (V3: nx=4..."
%  i.e., between the visualization section and the functions section.
%  Also add the two solver functions at the END of the file, after fill_iqr.
% =========================================================================


%% ========================================================================
%  STEP 2-3: CF-B — DIRECT SHOOTING vs iLQR (per country)
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
writetable(Tout, 'verification_results_v3.csv');
fprintf('\n  Saved: verification_results_v3.csv\n');


%% ========================================================================
%  STEP 5: SOLVER VISUALIZATION
% =========================================================================

% --- Fig 2: Verification scatter ---
figure('Name','V3 Verification','Color','w','Position',[50 50 1100 450]);
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
sgtitle('V3 Solver Verification','FontWeight','bold');

% --- Fig 3: Selected countries ---
selected = {'USA','DEU','ITA','GBR','JPN','CHL'}; n_sel = length(selected);
figure('Name','V3 Trajectories','Color','w','Position',[30 30 1500 800]);
for s = 1:n_sel
    iso = selected{s}; idx = find(strcmp({cdata.iso}, iso));
    xs_obs = forward_roll(cdata(idx).FCP, cdata(idx).FDI, ...
        cdata(idx).S, cdata(idx).theta, cdata(idx).d, ...
        cdata(idx).mu_y, cdata(idx).mu_b, cdata(idx).kappa_cp, P);
    t_obs = (cdata(idx).FCP(1:K_act)+cdata(idx).FDI(1:K_act))*100;
    t_ds  = (res(idx).us_ds(1,:)+res(idx).us_ds(2,:))*100;

    subplot(n_sel,3,(s-1)*3+1); hold on;
    bh=bar(1:K_act,[t_obs;t_ds]','grouped');
    bh(1).FaceColor=[.5 .5 .5]; bh(2).FaceColor=[.2 .5 .8];
    set(gca,'XTick',1:K_act,'XTickLabel',qlbl(1:K_act),'FontSize',6);
    ylabel('% GDP'); grid on;
    if s==1, title('Total Fiscal'); legend('Obs','Opt','FontSize',5,'Location','NE'); end
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
sgtitle(sprintf('V3 CF-B: N=%d, K_{act}=%d',N,K_act),'FontWeight','bold');


%% ========================================================================
%  ADD THESE TWO FUNCTIONS AT THE END OF THE FILE (after fill_iqr)
% =========================================================================

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
    % V3: 4x4 Q matrices
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


%% ========================================================================
%  PANDEMIC TRILEMMA — SCENARIO ANALYSIS (V3)
%
%  Requires: pandemic_calibration_v3.m to have been run (loads P, cdata)
%
%  Scenarios (S fixed at observed, fiscal composition varied):
%    S0: Observed fiscal deployment
%    S1: No CP  (F_CP = 0, F_DI = observed)
%    S2: No DI  (F_CP = observed, F_DI = 0)
%    S3: No Fiscal (F_CP = 0, F_DI = 0)
%    S4: CP→DI  (all fiscal as DI)
%    S5: DI→CP  (all fiscal as CP)
%
%  Sensitivity: alpha_DI = 0 (DI insignificant, conservative bound)
%
%  Key output: CP contribution through persistence channel,
%              effective persistence trajectories, Pareto frontier
% =========================================================================
fprintf('\n========================================\n');
fprintf('  SCENARIO ANALYSIS (V3: persistence CP)\n');
fprintf('========================================\n\n');

N_ = P.N;  Ka = P.K_act;


%% ========================================================================
%  STEP 1: COMPUTE ALL SCENARIOS PER COUNTRY
% =========================================================================

scen = struct();
for i = 1:n_c
    iso = cdata(i).iso;
    S_i = cdata(i).S;  th_i = cdata(i).theta;  d_i = cdata(i).d;
    my_i = cdata(i).mu_y;  mb_i = cdata(i).mu_b;  kc_i = cdata(i).kappa_cp;
    fcp = cdata(i).FCP;  fdi = cdata(i).FDI;
    ftot = fcp + fdi;  % total fiscal per quarter

    % S0: Observed
    xs0 = forward_roll(fcp, fdi, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % S1: No CP
    xs1 = forward_roll(zeros(1,N_), fdi, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % S2: No DI
    xs2 = forward_roll(fcp, zeros(1,N_), S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % S3: No Fiscal
    xs3 = forward_roll(zeros(1,N_), zeros(1,N_), S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % S4: CP→DI (all fiscal as DI, no CP)
    xs4 = forward_roll(zeros(1,N_), ftot, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % S5: DI→CP (all fiscal as CP, no DI)
    xs5 = forward_roll(ftot, zeros(1,N_), S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    scen(i).iso = iso;
    scen(i).y0 = xs0(1, 2:end)*100;  scen(i).b0 = xs0(2, 2:end)*100;
    scen(i).y1 = xs1(1, 2:end)*100;  scen(i).b1 = xs1(2, 2:end)*100;
    scen(i).y2 = xs2(1, 2:end)*100;  scen(i).b2 = xs2(2, 2:end)*100;
    scen(i).y3 = xs3(1, 2:end)*100;  scen(i).b3 = xs3(2, 2:end)*100;
    scen(i).y4 = xs4(1, 2:end)*100;  scen(i).b4 = xs4(2, 2:end)*100;
    scen(i).y5 = xs5(1, 2:end)*100;  scen(i).b5 = xs5(2, 2:end)*100;

    % Cumulative output (8Q)
    scen(i).cum0 = sum(scen(i).y0(1:Ka));
    scen(i).cum1 = sum(scen(i).y1(1:Ka));
    scen(i).cum2 = sum(scen(i).y2(1:Ka));
    scen(i).cum3 = sum(scen(i).y3(1:Ka));
    scen(i).cum4 = sum(scen(i).y4(1:Ka));
    scen(i).cum5 = sum(scen(i).y5(1:Ka));

    % Terminal debt
    scen(i).bN0 = xs0(2, N_+1)*100;
    scen(i).bN1 = xs1(2, N_+1)*100;
    scen(i).bN3 = xs3(2, N_+1)*100;
    scen(i).bN4 = xs4(2, N_+1)*100;
    scen(i).bN5 = xs5(2, N_+1)*100;

    % Decomposition
    scen(i).cp_eff  = scen(i).cum0 - scen(i).cum1;
    scen(i).di_eff  = scen(i).cum0 - scen(i).cum2;
    scen(i).tot_eff = scen(i).cum0 - scen(i).cum3;
    scen(i).interact = scen(i).tot_eff - scen(i).cp_eff - scen(i).di_eff;
end

fprintf('  Computed %d countries x 6 scenarios\n\n', n_c);


%% ========================================================================
%  STEP 2: OECD MEDIAN TRAJECTORY
% =========================================================================
fprintf('  OECD MEDIAN TRAJECTORY (pp):\n');
fprintf('  %8s %7s %7s %7s %7s %7s %7s %7s\n', ...
    'Quarter', 'Obs', 'S0', 'S1', 'S2', 'S3', 'S4', 'S5');

for k = 1:Ka
    obs_k  = arrayfun(@(c) c.y(k)*100, cdata);
    s0_k   = arrayfun(@(s) s.y0(k), scen);
    s1_k   = arrayfun(@(s) s.y1(k), scen);
    s2_k   = arrayfun(@(s) s.y2(k), scen);
    s3_k   = arrayfun(@(s) s.y3(k), scen);
    s4_k   = arrayfun(@(s) s.y4(k), scen);
    s5_k   = arrayfun(@(s) s.y5(k), scen);
    fprintf('  %8s %+7.2f %+7.2f %+7.2f %+7.2f %+7.2f %+7.2f %+7.2f\n', ...
        qlbl{k}, median(obs_k), median(s0_k), median(s1_k), ...
        median(s2_k), median(s3_k), median(s4_k), median(s5_k));
end


%% ========================================================================
%  STEP 3: CUMULATIVE OUTPUT & DECOMPOSITION
% =========================================================================
fprintf('\n\n  CUMULATIVE OUTPUT (sum %dQ, pp):\n', Ka);
fprintf('  %20s %8s %8s %8s\n', 'Scenario', 'Mean', 'Median', 'vs S0');

cum_names = {'S0: Observed','S1: No CP','S2: No DI','S3: No Fiscal','S4: CP->DI','S5: DI->CP'};
cum_fields = {'cum0','cum1','cum2','cum3','cum4','cum5'};
for j = 1:6
    vals = arrayfun(@(s) s.(cum_fields{j}), scen);
    diffs = vals - [scen.cum0];
    fprintf('  %20s %+8.2f %+8.2f %+8.2f\n', ...
        cum_names{j}, mean(vals), median(vals), mean(diffs));
end

fprintf('\n\n  FISCAL DECOMPOSITION (cum %dQ, pp):\n', Ka);
fprintf('  %8s %9s %9s %9s %9s\n', '', 'CP contr', 'DI contr', 'Interact', 'Total F');
fprintf('  %8s %+9.2f %+9.2f %+9.2f %+9.2f\n', 'Mean', ...
    mean([scen.cp_eff]), mean([scen.di_eff]), ...
    mean([scen.interact]), mean([scen.tot_eff]));
fprintf('  %8s %+9.2f %+9.2f %+9.2f %+9.2f\n', 'Median', ...
    median([scen.cp_eff]), median([scen.di_eff]), ...
    median([scen.interact]), median([scen.tot_eff]));


%% ========================================================================
%  STEP 4: EFFECTIVE PERSISTENCE
% =========================================================================
fprintf('\n\n  EFFECTIVE PERSISTENCE (OECD median):\n');
fprintf('  %8s %9s %10s %9s %8s\n', 'Quarter', 'S0(obs)', 'S1(noCP)', 'S3(noF)', 'CP gain');

for k = 1:Ka
    rho_s0 = zeros(n_c,1);  rho_s1 = zeros(n_c,1);
    for i = 1:n_c
        Sk = cdata(i).S(k);
        wk = 0; if k >= 3, wk = cdata(i).FCP(k-2); end
        rho_s0(i) = P.rho_y + P.psi*Sk + P.eta_p*wk;
        rho_s1(i) = P.rho_y + P.psi*Sk;  % w=0
    end
    gain = median(rho_s0) - median(rho_s1);
    fprintf('  %8s %9.3f %10.3f %9.3f %+8.3f\n', ...
        qlbl{k}, median(rho_s0), median(rho_s1), median(rho_s1), gain);
end


%% ========================================================================
%  STEP 5: COUNTRY-LEVEL RESULTS
% =========================================================================
fprintf('\n\n  COUNTRY-LEVEL CP CONTRIBUTION (cum %dQ):\n', Ka);
fprintf('  %5s %8s %8s %8s %8s %8s\n', ...
    'ISO', 'y_obs', 'y_noCP', 'CP_eff', 'FCP_tot', 'bN_obs');

for i = 1:n_c
    fcp_tot = sum(cdata(i).FCP(1:Ka))*100;
    fprintf('  %5s %+8.2f %+8.2f %+8.2f %7.1f%% %+8.2f\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum1, scen(i).cp_eff, ...
        fcp_tot, scen(i).bN0);
end

fprintf('\n  CP helps: %d / %d\n', sum([scen.cp_eff] > 0), n_c);
fprintf('  Mean CP effect:  %+.2f pp\n', mean([scen.cp_eff]));
fprintf('  Mean DI effect:  %+.2f pp\n', mean([scen.di_eff]));


%% ========================================================================
%  STEP 6: REALLOCATION — S4 vs S5
% =========================================================================
fprintf('\n\n  REALLOCATION: S4 (CP->DI) vs S5 (DI->CP):\n');
fprintf('  %5s %8s %9s %9s %8s\n', 'ISO', 'S0_obs', 'S4_CP2DI', 'S5_DI2CP', 'Winner');

s4_wins = 0;  s5_wins = 0;
for i = 1:n_c
    if scen(i).cum5 > scen(i).cum4
        winner = 'DI->CP'; s5_wins = s5_wins + 1;
    else
        winner = 'CP->DI'; s4_wins = s4_wins + 1;
    end
    fprintf('  %5s %+8.2f %+9.2f %+9.2f %8s\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum4, scen(i).cum5, winner);
end
fprintf('\n  CP->DI wins: %d / %d\n', s4_wins, n_c);
fprintf('  DI->CP wins: %d / %d\n', s5_wins, n_c);


%% ========================================================================
%  STEP 7: SENSITIVITY — alpha_DI = 0
%  DI is insignificant (p=0.12). Recompute under conservative assumption.
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  SENSITIVITY: alpha_DI = 0\n');
fprintf('========================================\n');

P_sens = P;
P_sens.alpha_F_DI = 0;

scen_s = struct();
for i = 1:n_c
    iso = cdata(i).iso;
    S_i = cdata(i).S;  th_i = cdata(i).theta;  d_i = cdata(i).d;
    my_i = cdata(i).mu_y;  mb_i = cdata(i).mu_b;  kc_i = cdata(i).kappa_cp;
    fcp = cdata(i).FCP;  fdi = cdata(i).FDI;
    ftot = fcp + fdi;

    xs0 = forward_roll(fcp, fdi, S_i, th_i, d_i, my_i, mb_i, kc_i, P_sens);
    xs1 = forward_roll(zeros(1,N_), fdi, S_i, th_i, d_i, my_i, mb_i, kc_i, P_sens);
    xs3 = forward_roll(zeros(1,N_), zeros(1,N_), S_i, th_i, d_i, my_i, mb_i, kc_i, P_sens);
    xs4 = forward_roll(zeros(1,N_), ftot, S_i, th_i, d_i, my_i, mb_i, kc_i, P_sens);
    xs5 = forward_roll(ftot, zeros(1,N_), S_i, th_i, d_i, my_i, mb_i, kc_i, P_sens);

    scen_s(i).iso = iso;
    scen_s(i).cum0 = sum(xs0(1, 2:Ka+1))*100;
    scen_s(i).cum1 = sum(xs1(1, 2:Ka+1))*100;
    scen_s(i).cum3 = sum(xs3(1, 2:Ka+1))*100;
    scen_s(i).cum4 = sum(xs4(1, 2:Ka+1))*100;
    scen_s(i).cum5 = sum(xs5(1, 2:Ka+1))*100;
    scen_s(i).cp_eff = scen_s(i).cum0 - scen_s(i).cum1;
    scen_s(i).tot_eff = scen_s(i).cum0 - scen_s(i).cum3;
end

fprintf('\n  With alpha_DI = 0:\n');
fprintf('    Total fiscal effect: Mean = %+.2f pp  (vs %.2f with DI)\n', ...
    mean([scen_s.tot_eff]), mean([scen.tot_eff]));
fprintf('    CP contribution:     Mean = %+.2f pp  (vs %.2f with DI)\n', ...
    mean([scen_s.cp_eff]), mean([scen.cp_eff]));
fprintf('    CP helps: %d / %d\n', sum([scen_s.cp_eff] > 0), n_c);

% Reallocation under alpha_DI = 0
s4w = 0; s5w = 0;
for i = 1:n_c
    if scen_s(i).cum5 > scen_s(i).cum4, s5w = s5w+1; else, s4w = s4w+1; end
end
fprintf('\n  Reallocation under alpha_DI = 0:\n');
fprintf('    CP->DI wins: %d / %d\n', s4w, n_c);
fprintf('    DI->CP wins: %d / %d\n', s5w, n_c);


%% ========================================================================
%  STEP 8: OUTPUT-DEBT PARETO
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  OUTPUT-DEBT PARETO\n');
fprintf('========================================\n');

fprintf('\n  %5s %10s %10s %10s %10s %10s\n', ...
    'ISO', 'y_obs', 'y_noF', 'b_obs', 'b_noF', 'Pareto?');

n_pareto = 0;
for i = 1:n_c
    dy = scen(i).cum0 - scen(i).cum3;  % positive = fiscal helped output
    db = scen(i).bN0 - scen(i).bN3;    % positive = fiscal raised debt
    % Pareto improvement if dy > 0 (better output) — debt always rises with F
    pareto = 'YES';
    if dy <= 0, pareto = 'NO'; end
    if strcmp(pareto, 'YES'), n_pareto = n_pareto + 1; end
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %+10.2f %10s\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum3, ...
        scen(i).bN0, scen(i).bN3, pareto);
end
fprintf('\n  Fiscal improved output: %d / %d\n', n_pareto, n_c);
fprintf('  Mean output gain: %+.2f pp\n', mean([scen.tot_eff]));
fprintf('  Mean debt cost:   %+.2f pp\n', mean([scen.bN0] - [scen.bN3]));
fprintf('  Output/Debt ratio: %.3f\n', ...
    mean([scen.tot_eff]) / mean([scen.bN0] - [scen.bN3]));


%% ========================================================================
%  STEP 9: VISUALIZATION
% =========================================================================

% --- Fig S1: OECD Median Trajectory (6 scenarios) ---
figure('Name','V3 Scenarios','Color','w','Position',[50 50 900 500]);
hold on;
cols = lines(6);
snames = {'S0: Observed','S1: No CP','S2: No DI','S3: No Fiscal','S4: CP\rightarrowDI','S5: DI\rightarrowCP'};
yfields = {'y0','y1','y2','y3','y4','y5'};
lstyles = {'-','--','--',':','-.','-.'};
for j = 1:6
    med_k = zeros(1, Ka);
    for k = 1:Ka
        med_k(k) = median(arrayfun(@(s) s.(yfields{j})(k), scen));
    end
    plot(1:Ka, med_k, lstyles{j}, 'Color', cols(j,:), 'LineWidth', 1.8, 'MarkerSize', 4);
end
yline(0, ':', 'Color', [.5 .5 .5]);
set(gca, 'XTick', 1:Ka, 'XTickLabel', qlbl(1:Ka), 'FontSize', 8, 'XTickLabelRotation', 45);
ylabel('Output gap (pp)'); grid on;
legend(snames, 'Location', 'SE', 'FontSize', 7);
title('V3 Scenario Analysis: OECD Median Output Gap');

% --- Fig S2: Effective persistence S0 vs S1 ---
figure('Name','V3 Persistence','Color','w','Position',[50 550 700 400]);
hold on;
rho_s0_med = zeros(1, Ka);  rho_s1_med = zeros(1, Ka);
for k = 1:Ka
    rho_s0_k = zeros(n_c,1);  rho_s1_k = zeros(n_c,1);
    for i = 1:n_c
        Sk = cdata(i).S(k);
        wk = 0; if k >= 3, wk = cdata(i).FCP(k-2); end
        rho_s0_k(i) = P.rho_y + P.psi*Sk + P.eta_p*wk;
        rho_s1_k(i) = P.rho_y + P.psi*Sk;
    end
    rho_s0_med(k) = median(rho_s0_k);
    rho_s1_med(k) = median(rho_s1_k);
end
area(1:Ka, rho_s1_med, 'FaceColor', [.9 .7 .7], 'EdgeColor', 'none', 'FaceAlpha', 0.5);
area(1:Ka, rho_s0_med, 'FaceColor', [.7 .8 .9], 'EdgeColor', 'none', 'FaceAlpha', 0.7);
plot(1:Ka, rho_s1_med, 'r--', 'LineWidth', 2);
plot(1:Ka, rho_s0_med, 'b-', 'LineWidth', 2);
yline(P.rho_y, ':', sprintf('\\rho_y = %.3f', P.rho_y), 'Color', [.5 .5 .5]);
set(gca, 'XTick', 1:Ka, 'XTickLabel', qlbl(1:Ka), 'FontSize', 8, 'XTickLabelRotation', 45);
ylabel('Effective persistence \rho_{eff}'); grid on;
legend('S destroys (no CP)', 'S destroys, CP preserves', ...
    '\rho_{eff} without CP', '\rho_{eff} with CP', 'Location', 'NE', 'FontSize', 8);
title('V3: CP Reduces Lockdown-Induced Persistence');

% --- Fig S3: Country-level CP contribution ---
figure('Name','V3 CP Contribution','Color','w','Position',[750 50 700 500]);
cp_effs = [scen.cp_eff];
[~, si] = sort(cp_effs, 'descend');
barh(1:n_c, cp_effs(si), 'FaceColor', [.2 .5 .8]);
hold on; xline(0, 'k-', 'LineWidth', 1);
set(gca, 'YTick', 1:n_c, 'YTickLabel', {scen(si).iso}, 'FontSize', 6);
xlabel('CP contribution to cumulative output (pp, 8Q)');
title('V3: Country-Level CP Effectiveness');
grid on;

% --- Fig S4: Output-Debt Pareto ---
figure('Name','V3 Pareto','Color','w','Position',[750 550 700 500]);
hold on;
for i = 1:n_c
    dy = scen(i).cum0 - scen(i).cum3;
    db = scen(i).bN0 - scen(i).bN3;
    plot(db, dy, 'ko', 'MarkerSize', 6, 'MarkerFaceColor', [.2 .5 .8]);
    text(db+0.1, dy+0.05, scen(i).iso, 'FontSize', 5);
end
xline(0, ':', 'Color', [.5 .5 .5]); yline(0, ':', 'Color', [.5 .5 .5]);
xlabel('Debt cost (terminal debt increase, pp)');
ylabel('Output gain (cumulative, pp)');
title('V3: Output-Debt Pareto (Observed vs No Fiscal)');
grid on;

fprintf('\n=== SCENARIO ANALYSIS COMPLETE ===\n');
