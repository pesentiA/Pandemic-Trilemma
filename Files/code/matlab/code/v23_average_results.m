%% ========================================================================
%  PANDEMIC TRILEMMA - iLQR SOLVER V22, AVERAGE OECD ECONOMY
%  ------------------------------------------------------------------------
%  V22 = V21 + strukturierte Outputs fuer die Results-Section:
%
%   6.1 Planner benchmark:
%       fig_planner_policy_paths_baseline_aerstyle.pdf/.png   (NEU: Export)
%       fig_planner_outcome_paths_baseline_aerstyle.pdf/.png
%       table_planner_summary_baseline.csv
%   6.2 Constrained frontier:
%       table_constrained_frontier.csv
%       table_constrained_frontier_slacks.csv
%       table_frontier_identity.csv                            (NEU)
%       fig_frontier_comparison_baseline_aerstyle.pdf/.png     (NEU)
%       table_chi_sweep.csv                                    (NEU, Appendix)
%   6.3 Weighted planner:
%       table_weighted_sweep.csv                               (NEU)
%       table_welfare_decomposition_baseline.csv
%       table_welfare_gain_decomposition_baseline.csv
%       fig_welfare_gain_decomposition_baseline_aerstyle.pdf/.png
%
%  Struktur-Aenderungen ggue. V21 (Modell/Solver UNVERAENDERT):
%   - 4-Start-Multistart in planner_starts() / planner_multistart() gekapselt
%   - Terminal-Setup in make_terminal() gekapselt (fuer Sweeps wiederverwendbar)
%   - RUN FLAGS zum An-/Abschalten der teuren Bloecke
%   - Frontier-Identitaets-Diagnostik (kollabieren A/B/C zur selben Politik?)
%   - Gewichts-Sweep (tau_b, lam_d inkl. VSL-Grid) fuer 6.3
%   - chi-Sweep (Komplementaritaet 0 / 0.25 / 0.50) fuer Frontier-Robustness
%
%  STRINGENCY ADJUSTMENT handled as a CONSTRAINT, not a penalty.
%  (Begruendung siehe V21-Header; unveraendert.)
%
%  Control u = [S; F_above; F_loans; F_guar; F_DI];  state n=9 in
%  capacity-stock specification.
%  Requires: country_data_for_matlab.csv, weekly_mortality_matlab.csv
% =========================================================================
clear; clc; close all;
fprintf('=== iLQR PLANNER V22: AVERAGE OECD (results outputs) ===\n  %s\n\n', datestr(now));

%% ------------------------------------------------------------------------
%  RUN FLAGS  (V22)
% -------------------------------------------------------------------------
RUN.baseline_figures = true;    % 6.1 Pfad-Figuren + Summary-Tabelle
RUN.frontier         = true;    % 6.2 Frontier (fmincon, 3 Szenarien x 5 Starts) - LANGSAM
RUN.frontier_figure  = true;    % 6.2 Vergleichsfigur (braucht RUN.frontier)
RUN.weight_sweep     = true;    % 6.3 (tau_b, lam_d)-Sweep (7 Szenarien x 4 iLQR-Starts)
RUN.chi_sweep        = true;   % Appendix: chi in {0, .25, .50} x Frontier - SEHR LANGSAM
                                % (fuer den finalen Lauf auf true setzen)

%% ------------------------------------------------------------------------
%  Calibrated V15 parameters
% -------------------------------------------------------------------------
%PARAMETERS-> DATA UND LITERATURE
P.rho_y=0.231;
P.alpha_S=-0.095; %are there upper bounds?
P.alpha_above=0.544;
P.alpha_below=0.261;
P.alpha_DI=1.470;
P.alpha_SDI=-0.041;
P.beta_d=0; %no FEAR term, alternativ theta in R um S bereinigen, vlt dann identifiziert
% Take-up rates used only to convert raw/headline data into effective
% below-the-line fiscal instruments. After data construction, Fl and Fg are
% effective values. Do NOT multiply by c_lo or c_gu inside the dynamics.
%Use those takeup rates to calculate back to the announced values for the
%policy interpretation
P.c_lo = 0.6;
P.c_gu = 0.25;
P.r=0.001;
P.gamma_y=0.117;
P.k_ab=0.664;
P.k_lo=0.836;
P.k_gu=0.536;
P.k_di=0.526;
P.phi_t= 0;
P.omega_ab_now = 0.0;      % kept for old timing experiments; not used in cap-stock specification

% Capacity-preservation stock from above-line fiscal support.
target_half_life_cap_q = 6;
P.decay_cap = 1 - 0.5^(1/target_half_life_cap_q);
P.alpha_cap   = 0.30 * P.alpha_above;   % ~ 0.163
P.chi_cap_liq = 0.50; %Komplementaritaetsparameter (Baseline; Sweep unten)
P.cap_scale   = NaN;      % set after ub_cap is computed from data

rho_theta_pre  = 1.5;   rho_theta_post = 0.75;   q_vax = 8;
P.q_vax = q_vax;
P.rho_th_q = [repmat(rho_theta_pre,1,q_vax-1), ...
              repmat(rho_theta_post,1,13-q_vax+1)];
P.th_max = Inf;
P.decay_K  = 0.1;
P.phi_S=0.8;
ifr=[0.005 0.002 0.007 0.004 0.002 0.002 0.0002];
P.delta_q=ifr([1 1 2 3 4 5 5 6 6 7 7 7 7])*1e6;
P.N=13; P.yr=4:16;
P.n = 9; P.m=5;
P.q_start = 2;
% state indices:
% 1 y | 2 b | 3 theta | 4 d | 5 fab_l1 | 6 fab_l2 | 7 fdi_l1 | 8 stock_liq | 9 stock_cap

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
%  DATA   (unveraendert ggue. V21)
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

T_theta_patch = readtable('theta_quarterly_CRI_JPN_TUR_frommonthly.csv');
for r0 = 1:height(T_theta_patch)
    key = sprintf('%s_%s', T_theta_patch.Country{r0}, T_theta_patch.Quarter{r0});
    th_map(key) = T_theta_patch.theta_hat(r0);
end
fprintf('  [patch] theta ueberschrieben fuer CRI/JPN/TUR (th_map)\n');

countries = unique(T.Country,'stable'); n_c = numel(countries); N = P.N;
[S_o,Fa,Fl,Fg,Fd,y_o,bd,th_o,d_o] = deal(zeros(n_c,N));
[Fl_raw,Fg_raw] = deal(zeros(n_c,N));
for i = 1:n_c
    iso = countries{i};
    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}),:);
        if isempty(row), continue; end
        S_o(i,k)=row.S_mean_tw;  Fa(i,k)=row.F_CP_above_3;
        Fl_raw(i,k) = row.F_CP_loans; Fg_raw(i,k) = row.F_CP_guar;
        Fl(i,k) = P.c_lo * row.F_CP_loans;   % effective loans
        Fg(i,k) = P.c_gu * row.F_CP_guar;    % effective guarantees
        Fd(i,k)=row.F_DI;
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
C.b0 = mean(b0_val); C.mu_y = 0; C.mu_b = 0;
C.eps_y = zeros(1,N+1);
C.eps_y(3) = 0 * mean(eps_v14); %%SHOCK HINEIN Q1.2020->DEFAULT: 0
C.eps_y(4) = 1 * mean(eps_v14); %%SHOCK HINEIN Q2.2020-> DEFAULT: 1

dev  = b_lvl - b0_val';
allF = [Fa(Fa>0); Fl(Fl>0); Fg(Fg>0); Fd(Fd>0)];

%% ------------------------------------------------------------------------
%  Planner weights (Baseline)   (unveraendert ggue. V21)
% -------------------------------------------------------------------------
W.beta = 0.99;

% Reference scales in economically interpretable units
y_scale = 5;      % pp output gap in one quarter
b_scale = 10;     % pp GDP debt increase
d_scale = 100;    % deaths per million per week

% Baseline preferences (Szenario 3: preferred average-welfare planner)
tau_b = 0.050;
lam_d = 75;

W.w_y = 1 / y_scale^2;
W.w_b = tau_b / b_scale^2;
W.w_d = 1 / d_scale^2;
W.lam_d = lam_d;

fprintf('  scales: y=%.2f, b_gap=%.2f, d=%.2f | tau_b=%.3f, lam_d=%.2f | w_y/w_b=%.2f, w_d/w_y=%.3f\n', ...
        y_scale, b_scale, d_scale, tau_b, lam_d, W.w_y/W.w_b, W.w_d/W.w_y);

% VSL anchoring: lam_d = 75 entspricht VSL ~ $6.5M (siehe V21-Kommentar).
% Sweep-Grid fuer 6.3 unten in SWEEP.scenarios.

W.p_u = 1e-8 * ones(5,1);

W.terminal_mode = 'debt_M';
W.lam_cons = 0.99;
W.M_term   = 1/(1 - W.beta*W.lam_cons^2);

% V21e: box bounds (m x N). Q4.19 locked to zero.
ub_cap = [pctile(S_o(S_o>0),99); pctile(Fa(Fa>0),99); pctile(Fl(Fl>0),99); ...
          pctile(Fg(Fg>0),99);   pctile(Fd(Fd>0),99)];
P.cap_scale = max(ub_cap(2), 1);
W.u_scale = ub_cap;
W.u_scale(W.u_scale <= 0 | isnan(W.u_scale) | isinf(W.u_scale)) = 1;
lb = zeros(P.m, P.N);
ub = repmat(ub_cap, 1, P.N);
%ub(1,:) = 100;
ub(:, 1:(P.q_start-1)) = 0;

% ---- dS CONSTRAINT ------------------------------------------------------
dS_obs = abs(diff(S_o,1,2));
dS_obs = dS_obs(dS_obs > 0);
P.dS_max = pctile(dS_obs, 95); %DEFINE
fprintf('  dS_max (obs p95 |Delta S|) = %.1f S-points/quarter\n', P.dS_max);

fprintf('  b0 = %.1f%% GDP | mu_y %+.3f | mu_b %+.3f | eps_y(Q2.20) %+.2f\n', ...
        C.b0, C.mu_y, C.mu_b, C.eps_y(4));
fprintf('  policy allowed from %s onward (Q4.19 locked: S and fiscal = 0)\n', qlbl{P.q_start});
fprintf('  caps: S<=%.1f, F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f\n\n', ub_cap);

y_obs_avg = mean(y_o,1);
b_obs_avg = mean(b_lvl,1);
d_obs_avg = mean(d_o,1);

%% ------------------------------------------------------------------------
%  RUN iLQR (Baseline, 4-Start-Multistart)          [V22: gekapselt]
% -------------------------------------------------------------------------
x0 = zeros(P.n,1); x0(2) = C.b0;
W.xbar = x0;
W = make_terminal(P, W, true);

U_obs = [mean(S_o,1); mean(Fa,1); mean(Fl,1); mean(Fg,1); mean(Fd,1)];
U_obs(:,1:(P.q_start-1)) = 0;

W.p_stock = 0.00; %deaktiviert
Xobs_scale = rollout(U_obs, x0, P, C);
W.stock_scale = max([max(abs(Xobs_scale(8,2:end))), ub_cap(3)+ub_cap(4), 1]);

fprintf('Q1.20 obs policy: S=%.1f Fab=%.2f Flo=%.2f Fgu=%.2f Fdi=%.2f\n', U_obs(:,2));

starts_planner = planner_starts(U_obs, ub, P);
[X, U, J, Js_all, best_ix] = planner_multistart(starts_planner, x0, P, C, W, lb, ub, true);
fprintf('\nBest of %d starts: start %d | J* = %.4f | all J: [%s]\n', ...
        numel(starts_planner), best_ix, J, sprintf('%.4f ', Js_all));

Xo = rollout(U_obs, x0, P, C);
J_obs = total_cost(Xo, U_obs, P, C, W);
fprintf('Observed average policy (same closure): J = %.4f\n\n', J_obs);

fprintf('Optimal policy and trajectory:\n');
fprintf('  %6s %6s %5s %5s %5s %5s | %7s %7s %8s %6s\n', ...
        'Q','S','F_ab','F_lo','F_gu','F_DI','y','b','theta','d');
for k = 1:P.N
    fprintf('  %6s %6.1f %5.2f %5.2f %5.2f %5.2f | %+7.2f %7.2f %8.5f %6.1f\n', ...
        qlbl{k}, U(1,k),U(2,k),U(3,k),U(4,k),U(5,k), ...
        X(1,k+1),X(2,k+1),X(3,k+1),X(4,k+1));
end
fprintf('\n  CUM comparison    | optimal | model@obs | raw data\n');
fprintf('  cum output gap    | %+7.2f | %+9.2f | %+8.2f\n', ...
    sum(X(1,2:end)), sum(Xo(1,2:end)), sum(y_obs_avg));
fprintf('  cum deaths /M     | %7.0f | %9.0f | %8.0f\n', ...
    sum(X(4,2:end))*13, sum(Xo(4,2:end))*13, sum(d_obs_avg)*13);
fprintf('  terminal debt %%   | %7.2f | %9.2f | %8.2f\n', ...
    X(2,end), Xo(2,end), b_obs_avg(end));

%% ------------------------------------------------------------------------
%  6.1 FIGURE: Planner policy paths (paper style)   [V22: mit Export]
% -------------------------------------------------------------------------
if RUN.baseline_figures

idx_plot = 1:P.N;
x = 1:numel(idx_plot);
xlabels = qlbl(idx_plot);

S_opt  = U(1,idx_plot);      S_obs  = U_obs(1,idx_plot);
Fab_opt = U(2,idx_plot);     Fab_obs = U_obs(2,idx_plot);
Fbelow_opt = U(3,idx_plot)+U(4,idx_plot);
Fbelow_obs = U_obs(3,idx_plot)+U_obs(4,idx_plot);
Fdi_opt = U(5,idx_plot);     Fdi_obs = U_obs(5,idx_plot);

show_ticks = 1:2:numel(x);
show_labels = xlabels(show_ticks);

fig = figure('Color','w','Position',[80 80 1050 650]);
tl = tiledlayout(2,2,'TileSpacing','compact','Padding','compact');
planner_col = [0 0 0]; obs_col = [0.45 0.45 0.45];
lw_planner = 2.0; lw_obs = 1.8; ms = 4;

panels = {
    S_opt,      S_obs,      'A. Containment stringency',            'Stringency index';
    Fab_opt,    Fab_obs,    'B. Above-the-line capacity preservation','% of 2019 GDP';
    Fbelow_opt, Fbelow_obs, 'C. Below-the-line liquidity support',   '% of 2019 GDP';
    Fdi_opt,    Fdi_obs,    'D. Demand injection',                   '% of 2019 GDP'};

for pnl = 1:4
    nexttile;
    plot(x, panels{pnl,1}, '-', 'Color', planner_col, ...
         'LineWidth', lw_planner, 'Marker', 'o', 'MarkerSize', ms); hold on;
    plot(x, panels{pnl,2}, '--', 'Color', obs_col, ...
         'LineWidth', lw_obs, 'Marker', 's', 'MarkerSize', ms);
    title(panels{pnl,3}, 'FontWeight','normal');
    ylabel(panels{pnl,4});
    xlim([1 numel(x)]);
    ylim([0 max([panels{pnl,1} panels{pnl,2}])*1.15 + 1e-6]);
    set(gca,'XTick',show_ticks,'XTickLabel',show_labels, ...
            'TickDir','out','Box','off','FontSize',9);
    grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';
end
lgd = legend({'Planner benchmark','Observed OECD average'}, ...
             'Orientation','horizontal','Box','off','FontSize',9);
lgd.Layout.Tile = 'south';

exportgraphics(fig, 'fig_planner_policy_paths_baseline_aerstyle.pdf', 'ContentType','vector');
exportgraphics(fig, 'fig_planner_policy_paths_baseline_aerstyle.png', 'Resolution', 300);

%% ------------------------------------------------------------------------
%  6.1 FIGURE: Planner outcome paths (paper style)  (wie V21)
% -------------------------------------------------------------------------
state_idx = idx_plot + 1;
y_opt = X(1,state_idx);     y_obsm = Xo(1,state_idx);
b_opt = X(2,state_idx);     b_obsm = Xo(2,state_idx);
theta_opt = X(3,state_idx); theta_obsm = Xo(3,state_idx);
d_opt = X(4,state_idx);     d_obsm = Xo(4,state_idx);

fig = figure('Color','w','Position',[80 80 1050 650]);
tl = tiledlayout(2,2,'TileSpacing','compact','Padding','compact');

nexttile;
plot(x, y_opt, '-', 'Color', planner_col,'LineWidth',lw_planner,'Marker','o','MarkerSize',ms); hold on;
plot(x, y_obsm, '--','Color', obs_col,'LineWidth',lw_obs,'Marker','s','MarkerSize',ms);
yline(0, ':', 'Color', [0.4 0.4 0.4]);
title('A. Output gap','FontWeight','normal'); ylabel('pp potential GDP');
xlim([1 numel(x)]);
set(gca,'XTick',show_ticks,'XTickLabel',show_labels,'TickDir','out','Box','off','FontSize',9);
grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';

nexttile;
plot(x, b_opt, '-', 'Color', planner_col,'LineWidth',lw_planner,'Marker','o','MarkerSize',ms); hold on;
plot(x, b_obsm, '--','Color', obs_col,'LineWidth',lw_obs,'Marker','s','MarkerSize',ms);
title('B. Public debt','FontWeight','normal'); ylabel('% of 2019 GDP');
xlim([1 numel(x)]);
set(gca,'XTick',show_ticks,'XTickLabel',show_labels,'TickDir','out','Box','off','FontSize',9);
grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';

nexttile;
plot(x, d_opt, '-', 'Color', planner_col,'LineWidth',lw_planner,'Marker','o','MarkerSize',ms); hold on;
plot(x, d_obsm, '--','Color', obs_col,'LineWidth',lw_obs,'Marker','s','MarkerSize',ms);
title('C. Excess mortality','FontWeight','normal'); ylabel('Deaths / million / week');
xlim([1 numel(x)]); ylim([0 max([d_opt d_obsm])*1.15 + 1e-6]);
set(gca,'XTick',show_ticks,'XTickLabel',show_labels,'TickDir','out','Box','off','FontSize',9);
grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';

nexttile;
plot(x, theta_opt, '-', 'Color', planner_col,'LineWidth',lw_planner,'Marker','o','MarkerSize',ms); hold on;
plot(x, theta_obsm, '--','Color', obs_col,'LineWidth',lw_obs,'Marker','s','MarkerSize',ms);
title('D. Infection prevalence','FontWeight','normal'); ylabel('Share of population');
xlim([1 numel(x)]); ylim([0 max([theta_opt theta_obsm])*1.15 + 1e-6]);
set(gca,'XTick',show_ticks,'XTickLabel',show_labels,'TickDir','out','Box','off','FontSize',9);
grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';

lgd = legend({'Planner benchmark','OECD model-implied'}, ...
             'Orientation','horizontal','Box','off','FontSize',9);
lgd.Layout.Tile = 'south';

exportgraphics(fig, 'fig_planner_outcome_paths_baseline_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_planner_outcome_paths_baseline_aerstyle.png','Resolution', 300);

%% ------------------------------------------------------------------------
%  6.1 TABLE: Aggregate planner comparison          (wie V21)
% -------------------------------------------------------------------------
idx_eval = 2:P.N;
state_idx = idx_eval + 1;

cum_y_opt = sum(X(1,state_idx));    cum_y_obs = sum(Xo(1,state_idx));
cum_d_opt = sum(X(4,state_idx))*13; cum_d_obs = sum(Xo(4,state_idx))*13;
bT_opt = X(2,state_idx(end));       bT_obs = Xo(2,state_idx(end));
thetaT_opt = X(3,state_idx(end));   thetaT_obs = Xo(3,state_idx(end));

tot_Fab_opt = sum(U(2,idx_eval));               tot_Fab_obs = sum(U_obs(2,idx_eval));
tot_Fbelow_opt = sum(U(3,idx_eval)+U(4,idx_eval));
tot_Fbelow_obs = sum(U_obs(3,idx_eval)+U_obs(4,idx_eval));
tot_Fdi_opt = sum(U(5,idx_eval));               tot_Fdi_obs = sum(U_obs(5,idx_eval));
avg_S_opt = mean(U(1,idx_eval));                avg_S_obs = mean(U_obs(1,idx_eval));

J_gain_pct = 100 * (J_obs - J) / J_obs;

T_summary = table( ...
    ["Cumulative output gap"; "Cumulative deaths per million"; "Terminal debt"; ...
     "Terminal infection prevalence"; "Average stringency"; "Total above-the-line CP"; ...
     "Total below-the-line liquidity"; "Total demand injection"; ...
     "Objective value"; "Objective gain, percent"], ...
    [cum_y_obs; cum_d_obs; bT_obs; thetaT_obs; avg_S_obs; ...
     tot_Fab_obs; tot_Fbelow_obs; tot_Fdi_obs; J_obs; NaN], ...
    [cum_y_opt; cum_d_opt; bT_opt; thetaT_opt; avg_S_opt; ...
     tot_Fab_opt; tot_Fbelow_opt; tot_Fdi_opt; J; J_gain_pct], ...
    [cum_y_opt-cum_y_obs; cum_d_opt-cum_d_obs; bT_opt-bT_obs; ...
     thetaT_opt-thetaT_obs; avg_S_opt-avg_S_obs; ...
     tot_Fab_opt-tot_Fab_obs; tot_Fbelow_opt-tot_Fbelow_obs; ...
     tot_Fdi_opt-tot_Fdi_obs; J-J_obs; NaN], ...
    'VariableNames', {'Outcome','Observed_policy','Planner_benchmark','Difference'});
disp(T_summary);
writetable(T_summary, 'table_planner_summary_baseline.csv');

end % RUN.baseline_figures

%% ------------------------------------------------------------------------
%  6.2 CONSTRAINED POLICY FRONTIER                  (wie V21)
% -------------------------------------------------------------------------
if RUN.frontier

M_obs = frontier_metrics(Xo, P, C, W);

fprintf('\n=== CONSTRAINED POLICY FRONTIER: model-observed thresholds ===\n');
fprintf('  Observed output loss Ly     = %.4f\n', M_obs.Ly);
fprintf('  Observed cum deaths /M      = %.1f\n', M_obs.Dcum);
fprintf('  Observed terminal debt      = %.2f\n', M_obs.bT);

frontier.tie_eps = 1e-8;

starts = cell(5,1);
starts{1} = U_obs;                       % observed
starts{2} = U;                           % weighted-planner solution
starts{3} = zeros(P.m, P.N);             % zero
starts{4} = starts_planner{3};           % high above-line, observed S
U_mix_start = U_obs;
U_mix_start(2,:) = ub(2,:);
U_mix_start(:,1:(P.q_start-1)) = 0;
starts{5} = U_mix_start;                 % high above + observed below

fprintf('\nScenario A: output frontier\n');
[XA, UA, MA, exitA] = solve_frontier_multistart('output', starts, x0, P, C, W, lb, ub, M_obs, frontier);
fprintf('\nScenario B: mortality frontier\n');
[XB, UB, MB, exitB] = solve_frontier_multistart('mortality', starts, x0, P, C, W, lb, ub, M_obs, frontier);
fprintf('\nScenario C: debt frontier\n');
[XC, UC, MC, exitC] = solve_frontier_multistart('debt', starts, x0, P, C, W, lb, ub, M_obs, frontier);

% ---- Summary table (wie V21) --------------------------------------------
T_frontier = table( ...
    ["Observed"; "A_Output_frontier"; "B_Mortality_frontier"; "C_Debt_frontier"], ...
    [M_obs.Ly;   MA.Ly;   MB.Ly;   MC.Ly], ...
    [M_obs.cum_y; MA.cum_y; MB.cum_y; MC.cum_y], ...
    [M_obs.Dcum; MA.Dcum; MB.Dcum; MC.Dcum], ...
    [M_obs.bT;   MA.bT;   MB.bT;   MC.bT], ...
    [sum(U_obs(1,2:P.N))/(P.N-1); sum(UA(1,2:P.N))/(P.N-1); sum(UB(1,2:P.N))/(P.N-1); sum(UC(1,2:P.N))/(P.N-1)], ...
    [sum(U_obs(2,2:P.N)); sum(UA(2,2:P.N)); sum(UB(2,2:P.N)); sum(UC(2,2:P.N))], ...
    [sum(U_obs(3,2:P.N)+U_obs(4,2:P.N)); sum(UA(3,2:P.N)+UA(4,2:P.N)); sum(UB(3,2:P.N)+UB(4,2:P.N)); sum(UC(3,2:P.N)+UC(4,2:P.N))], ...
    [sum(U_obs(5,2:P.N)); sum(UA(5,2:P.N)); sum(UB(5,2:P.N)); sum(UC(5,2:P.N))], ...
    'VariableNames', {'Scenario','Output_loss_Ly','Cum_output_gap','Cum_deaths_per_million','Terminal_debt', ...
                      'Avg_stringency','Total_above','Total_effective_below','Total_DI'});
disp(T_frontier);
writetable(T_frontier, 'table_constrained_frontier.csv');

% ---- Slack table (wie V21) ----------------------------------------------
tol_frontier = 1e-5;
Scenario = ["Observed"; "A_Output_frontier"; "B_Mortality_frontier"; "C_Debt_frontier"];
Output_loss_Ly = [M_obs.Ly; MA.Ly; MB.Ly; MC.Ly];
Cum_deaths_per_million = [M_obs.Dcum; MA.Dcum; MB.Dcum; MC.Dcum];
Terminal_debt = [M_obs.bT; MA.bT; MB.bT; MC.bT];
Output_slack = M_obs.Ly - Output_loss_Ly;
Deaths_slack = M_obs.Dcum - Cum_deaths_per_million;
Debt_slack   = M_obs.bT - Terminal_debt;
Target_improvement = [NaN; M_obs.Ly-MA.Ly; M_obs.Dcum-MB.Dcum; M_obs.bT-MC.bT];
Target_improvement_pct = [NaN; ...
    100*(M_obs.Ly-MA.Ly)/M_obs.Ly; ...
    100*(M_obs.Dcum-MB.Dcum)/M_obs.Dcum; ...
    100*(M_obs.bT-MC.bT)/abs(M_obs.bT)];
Feasible = [true; ...
    (MA.Dcum<=M_obs.Dcum+tol_frontier)&&(MA.bT<=M_obs.bT+tol_frontier); ...
    (MB.Ly<=M_obs.Ly+tol_frontier)&&(MB.bT<=M_obs.bT+tol_frontier); ...
    (MC.Ly<=M_obs.Ly+tol_frontier)&&(MC.Dcum<=M_obs.Dcum+tol_frontier)];
T_frontier_slack = table(Scenario, Output_loss_Ly, Cum_deaths_per_million, Terminal_debt, ...
    Output_slack, Deaths_slack, Debt_slack, Target_improvement, Target_improvement_pct, Feasible);
disp(T_frontier_slack);
writetable(T_frontier_slack, 'table_constrained_frontier_slacks.csv');

%% ------------------------------------------------------------------------
%  6.2 DIAGNOSTIK: Kollabieren die drei Frontier-Loesungen?   [V22 NEU]
% -------------------------------------------------------------------------
% Zweck: entscheidet die TEXT-FORMULIERUNG in 6.2.
%  - Wenn A/B/C (nahezu) identische Politiken sind: berichte EINE zulaessige
%    Politik, die die beobachtete in allen drei Dimensionen simultan schwach
%    dominiert (beobachtete Politik liegt strikt im Inneren der
%    Moeglichkeitsmenge) - NICHT drei separate Frontiers.
%  - Wenn sie sich unterscheiden: drei echte Frontier-Punkte.

pairlbl = ["A_vs_B"; "A_vs_C"; "B_vs_C"];
Upairs = {UA, UB; UA, UC; UB, UC};
inst_names = {'S','F_above','F_loans','F_guar','F_DI'};
maxdiff = zeros(3, P.m);
for pp = 1:3
    D = abs(Upairs{pp,1} - Upairs{pp,2});
    maxdiff(pp,:) = max(D, [], 2)';
end
T_identity = array2table(maxdiff, 'VariableNames', ...
    strcat('maxdiff_', inst_names));
T_identity = addvars(T_identity, pairlbl, 'Before', 1, 'NewVariableNames','Pair');
disp('Frontier solution identity check (max |dU| per instrument, over quarters):');
disp(T_identity);
writetable(T_identity, 'table_frontier_identity.csv');

% Simultane Dominanz: verbessert jede Szenario-Loesung ALLE drei Dimensionen?
tolD = 1e-4;
simdom = [ ...
    (M_obs.Ly-MA.Ly>=-tolD) && (M_obs.Dcum-MA.Dcum>=-tolD) && (M_obs.bT-MA.bT>=-tolD); ...
    (M_obs.Ly-MB.Ly>=-tolD) && (M_obs.Dcum-MB.Dcum>=-tolD) && (M_obs.bT-MB.bT>=-tolD); ...
    (M_obs.Ly-MC.Ly>=-tolD) && (M_obs.Dcum-MC.Dcum>=-tolD) && (M_obs.bT-MC.bT>=-tolD)];
fprintf('\nSimultaneous weak dominance of observed policy: A=%d, B=%d, C=%d\n', simdom);
if all(simdom) && max(maxdiff(:)) < 1.0
    fprintf('  -> A/B/C kollabieren zu (nahezu) EINER dominierenden Politik.\n');
    fprintf('     Text-Formulierung: "single feasible policy strictly inside the frontier".\n');
else
    fprintf('  -> A/B/C sind unterscheidbare Frontier-Punkte.\n');
end

%% ------------------------------------------------------------------------
%  6.2 FIGURE: Frontier comparison (paper style)              [V22 NEU]
% -------------------------------------------------------------------------
if RUN.frontier_figure
scen_lbl = {'Observed','A: min output','B: min deaths','C: min debt'};
vals_y = [M_obs.cum_y, MA.cum_y, MB.cum_y, MC.cum_y];
vals_d = [M_obs.Dcum,  MA.Dcum,  MB.Dcum,  MC.Dcum];
vals_b = [M_obs.bT,    MA.bT,    MB.bT,    MC.bT] - C.b0;   % als Delta b vs b0

fig = figure('Color','w','Position',[100 100 1050 380]);
tl = tiledlayout(1,3,'TileSpacing','compact','Padding','compact');
greys = [0.80 0.80 0.80; 0.55 0.55 0.55; 0.35 0.35 0.35; 0.15 0.15 0.15];

panel_data = {vals_y, 'A. Cumulative output gap', 'pp-quarters'; ...
              vals_d, 'B. Cumulative excess mortality', 'Deaths per million'; ...
              vals_b, 'C. Terminal debt increase', 'pp of GDP vs. 2019Q4'};
for pnl = 1:3
    nexttile;
    bh = bar(panel_data{pnl,1}, 0.65, 'FaceColor','flat','EdgeColor',[0 0 0]);
    for jj = 1:4, bh.CData(jj,:) = greys(jj,:); end
    yline(0, '-', 'Color', [0 0 0], 'LineWidth', 0.5);
    title(panel_data{pnl,2}, 'FontWeight','normal');
    ylabel(panel_data{pnl,3});
    set(gca,'XTick',1:4,'XTickLabel',scen_lbl,'XTickLabelRotation',30, ...
        'TickDir','out','Box','off','FontSize',9);
    grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';
end
exportgraphics(fig, 'fig_frontier_comparison_baseline_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_frontier_comparison_baseline_aerstyle.png','Resolution',300);
end

end % RUN.frontier

%% ------------------------------------------------------------------------
%  6.3 WELFARE DECOMPOSITION (Baseline)             (wie V21, gekuerzt)
% -------------------------------------------------------------------------
Comp_opt = welfare_decomposition(X,  U,     P, C, W);
Comp_obs = welfare_decomposition(Xo, U_obs, P, C, W);

fprintf('\nWelfare decomposition consistency check:\n');
fprintf('  Planner:  total_cost = %.6f | decomposition = %.6f | diff = %.2e\n', ...
        total_cost(X, U, P, C, W), Comp_opt.Total, total_cost(X, U, P, C, W)-Comp_opt.Total);
fprintf('  Observed: total_cost = %.6f | decomposition = %.6f | diff = %.2e\n', ...
        total_cost(Xo, U_obs, P, C, W), Comp_obs.Total, total_cost(Xo, U_obs, P, C, W)-Comp_obs.Total);
fprintf('Regularizer share of J (should be << 1%%): planner %.3f%%+%.3f%% | obs %.3f%%+%.3f%%\n', ...
        100*Comp_opt.Stringency/Comp_opt.Total, 100*Comp_opt.Fiscal/Comp_opt.Total, ...
        100*Comp_obs.Stringency/Comp_obs.Total, 100*Comp_obs.Fiscal/Comp_obs.Total);

component_names = ["Output"; "Mortality"; "Debt"; "Stringency friction"; ...
                   "Fiscal friction"; "Below-line stock risk"; "Terminal value"];
obs_vals = [Comp_obs.Output; Comp_obs.Mortality; Comp_obs.Debt; Comp_obs.Stringency; ...
            Comp_obs.Fiscal; Comp_obs.Stock; Comp_obs.Terminal];
opt_vals = [Comp_opt.Output; Comp_opt.Mortality; Comp_opt.Debt; Comp_opt.Stringency; ...
            Comp_opt.Fiscal; Comp_opt.Stock; Comp_opt.Terminal];

T_welfare = table([component_names; "Total"], [obs_vals; Comp_obs.Total], ...
    [opt_vals; Comp_opt.Total], [opt_vals-obs_vals; Comp_opt.Total-Comp_obs.Total], ...
    100*[obs_vals; Comp_obs.Total]/Comp_obs.Total, ...
    100*[opt_vals; Comp_opt.Total]/Comp_obs.Total, ...
    'VariableNames', {'Component','Observed_policy','Planner_benchmark', ...
                      'Difference','Observed_pct_of_obs_total','Planner_pct_of_obs_total'});
disp(T_welfare);
writetable(T_welfare, 'table_welfare_decomposition_baseline.csv');

% ---- Gain decomposition + Figur (wie V21) --------------------------------
gain_vals = obs_vals - opt_vals;
total_gain = Comp_obs.Total - Comp_opt.Total;
gain_pct_obs_total = 100 * gain_vals / Comp_obs.Total;

T_gain = table(component_names, obs_vals, opt_vals, gain_vals, gain_pct_obs_total, ...
    100*gain_vals/total_gain, ...
    'VariableNames', {'Component','Observed_policy_loss','Planner_benchmark_loss', ...
                      'Gain_obs_minus_planner','Gain_pct_of_observed_total_loss', ...
                      'Contribution_pct_of_total_gain'});
disp(T_gain);
writetable(T_gain, 'table_welfare_gain_decomposition_baseline.csv');
fprintf('Total welfare gain: %.6f (%.2f%% of observed total loss)\n', ...
        total_gain, 100*total_gain/Comp_obs.Total);

fig = figure('Color','w','Position',[100 100 850 480]);
plot_names = flip(component_names);
plot_vals  = flip(gain_pct_obs_total);
bh = barh(plot_vals, 0.65); hold on;
bh.FaceColor = 'flat';
for j = 1:numel(plot_vals)
    if plot_vals(j) >= 0, bh.CData(j,:) = [0.25 0.25 0.25];
    else, bh.CData(j,:) = [0.75 0.75 0.75]; end
end
bh.EdgeColor = [0 0 0];
xline(0, '-', 'Color', [0 0 0], 'LineWidth', 0.8);
set(gca,'YTick',1:numel(plot_names),'YTickLabel',plot_names, ...
    'TickDir','out','Box','off','FontSize',10);
xlabel('Contribution to welfare gain (% of observed total welfare loss)');
title('Welfare gain decomposition','FontWeight','normal');
grid on; ax = gca; ax.GridAlpha = 0.12; ax.YGrid = 'off';
xrange = max(plot_vals)-min(plot_vals); if xrange==0, xrange=1; end
for j = 1:numel(plot_vals)
    val = plot_vals(j);
    if val >= 0, xpos = val+0.015*xrange; halign='left';
    else, xpos = val-0.015*xrange; halign='right'; end
    text(xpos, j, sprintf('%.1f', val), 'VerticalAlignment','middle', ...
        'HorizontalAlignment',halign,'FontSize',9);
end
xmin=min([plot_vals;0]); xmax=max([plot_vals;0]);
margin=0.15*max(abs([xmin xmax])); if margin==0, margin=1; end
xlim([xmin-margin, xmax+margin]);
exportgraphics(fig,'fig_welfare_gain_decomposition_baseline_aerstyle.pdf','ContentType','vector');
exportgraphics(fig,'fig_welfare_gain_decomposition_baseline_aerstyle.png','Resolution',300);

%% ------------------------------------------------------------------------
%  6.3 WEIGHT SWEEP: (tau_b, lam_d)-Szenarien + VSL-Grid      [V22 NEU]
% -------------------------------------------------------------------------
% Zeigt: Hauptaussagen (Politik-Komposition, Richtung der Gains) sind
% robust ueber die Praeferenzszenarien und den VSL-Sweep.
% Fuer jedes Szenario: J_obs wird UNTER DENSELBEN GEWICHTEN neu berechnet,
% damit der Gain-Vergleich intern konsistent ist.
if RUN.weight_sweep

sweep_lbl = ["Output_oriented"; "Balanced"; "Baseline"; "Precautionary"; ...
             "VSL_5M"; "VSL_9M"; "VSL_11M"];
sweep_tau = [0.025; 0.050; 0.050; 0.100; 0.050; 0.050; 0.050];
sweep_lam = [25;    50;    75;    150;   58;    104;   127];

nS = numel(sweep_lbl);
R_sweep = nan(nS, 12);
idx_eval = 2:P.N; state_idx = idx_eval + 1;

for s = 1:nS
    fprintf('\n--- Weight sweep %d/%d: %s (tau_b=%.3f, lam_d=%.0f) ---\n', ...
            s, nS, sweep_lbl(s), sweep_tau(s), sweep_lam(s));
    W2 = W;
    W2.w_b   = sweep_tau(s) / b_scale^2;
    W2.lam_d = sweep_lam(s);
    W2 = make_terminal(P, W2, false);

    J_obs2 = total_cost(Xo, U_obs, P, C, W2);
    [Xw, Uw, Jw] = planner_multistart(planner_starts(U_obs, ub, P), ...
                                      x0, P, C, W2, lb, ub, false);

    R_sweep(s,:) = [ sweep_tau(s), sweep_lam(s), ...
        sum(Xw(1,state_idx)), sum(Xw(4,state_idx))*13, Xw(2,end), ...
        mean(Uw(1,idx_eval)), sum(Uw(2,idx_eval)), ...
        sum(Uw(3,idx_eval)+Uw(4,idx_eval)), sum(Uw(5,idx_eval)), ...
        Jw, J_obs2, 100*(J_obs2-Jw)/J_obs2 ];

    fprintf('    cum_y %+.2f | cum_d %.0f | bT %.2f | S %.1f | ab %.2f | bel %.2f | DI %.2f | gain %.1f%%\n', ...
        R_sweep(s,3:9), R_sweep(s,12));
end

T_sweep = array2table(R_sweep, 'VariableNames', ...
    {'tau_b','lam_d','Cum_output_gap','Cum_deaths_per_million','Terminal_debt', ...
     'Avg_stringency','Total_above','Total_effective_below','Total_DI', ...
     'J_planner','J_observed','Gain_pct'});
T_sweep = addvars(T_sweep, sweep_lbl, 'Before', 1, 'NewVariableNames','Scenario');
disp(T_sweep);
writetable(T_sweep, 'table_weighted_sweep.csv');

end % RUN.weight_sweep

%% ------------------------------------------------------------------------
%  APPENDIX: CHI SWEEP (Komplementaritaet) x Frontier          [V22 NEU]
% -------------------------------------------------------------------------
% Zentraler Robustness-Check: haengt die Pareto-Dominanz an chi?
% Fuer jedes chi werden die model-implied observed thresholds NEU berechnet
% (die beobachtete Politik erzeugt unter anderem chi andere Outcomes!).
% Reduziertes Multistart-Set (4 Starts) aus Laufzeitgruenden.
if RUN.chi_sweep

chi_grid = [0, 0.25, 0.50];
nChi = numel(chi_grid);
R_chi = nan(nChi, 10);
frontier2.tie_eps = 1e-8;

for cc = 1:nChi
    fprintf('\n===== CHI SWEEP %d/%d: chi = %.2f =====\n', cc, nChi, chi_grid(cc));
    P2 = P; P2.chi_cap_liq = chi_grid(cc);

    Xo2 = rollout(U_obs, x0, P2, C);
    M_obs2 = frontier_metrics(Xo2, P2, C, W);

    starts_f = planner_starts(U_obs, ub, P2);   % obs, zero, high-above, high-below

    impA = NaN; impB = NaN; impC = NaN;
    try
        [~, ~, MA2] = solve_frontier_multistart('output',    starts_f, x0, P2, C, W, lb, ub, M_obs2, frontier2);
        impA = 100*(M_obs2.Ly - MA2.Ly)/M_obs2.Ly;
    catch ME, fprintf('  output frontier failed: %s\n', ME.message); end
    try
        [~, ~, MB2] = solve_frontier_multistart('mortality', starts_f, x0, P2, C, W, lb, ub, M_obs2, frontier2);
        impB = 100*(M_obs2.Dcum - MB2.Dcum)/M_obs2.Dcum;
    catch ME, fprintf('  mortality frontier failed: %s\n', ME.message); end
    try
        [~, ~, MC2] = solve_frontier_multistart('debt',      starts_f, x0, P2, C, W, lb, ub, M_obs2, frontier2);
        impC = 100*(M_obs2.bT - MC2.bT)/abs(M_obs2.bT);
    catch ME, fprintf('  debt frontier failed: %s\n', ME.message); end

    % Weighted planner unter chi (Baseline-Gewichte)
    [Xc, Uc, Jc] = planner_multistart(starts_f, x0, P2, C, W, lb, ub, false);
    J_obs_c = total_cost(Xo2, U_obs, P2, C, W);

    R_chi(cc,:) = [ chi_grid(cc), M_obs2.Ly, M_obs2.Dcum, M_obs2.bT, ...
                    impA, impB, impC, ...
                    sum(Uc(3,2:P.N)+Uc(4,2:P.N)), sum(Uc(2,2:P.N)), ...
                    100*(J_obs_c-Jc)/J_obs_c ];

    fprintf('  chi=%.2f: frontier improvements A %.1f%% | B %.1f%% | C %.1f%% | weighted gain %.1f%%\n', ...
            chi_grid(cc), impA, impB, impC, R_chi(cc,10));
end

T_chi = array2table(R_chi, 'VariableNames', ...
    {'chi','Obs_Ly','Obs_Dcum','Obs_bT', ...
     'Frontier_A_output_impr_pct','Frontier_B_deaths_impr_pct','Frontier_C_debt_impr_pct', ...
     'Planner_total_below','Planner_total_above','Weighted_gain_pct'});
disp(T_chi);
writetable(T_chi, 'table_chi_sweep.csv');

end % RUN.chi_sweep

fprintf('\n=== V22 DONE. Outputs fuer Results-Section geschrieben. ===\n');

%% ========================================================================
%  LOCAL FUNCTIONS
% =========================================================================

function starts = planner_starts(U_obs, ub, P)                    % [V22 NEU]
% Vier oekonomisch unterscheidbare Startpolitiken fuer den iLQR-Multistart.
    starts = cell(4,1);
    starts{1} = U_obs;                          % observed average policy
    starts{2} = zeros(P.m, P.N);                % zero controls
    U_ab0 = zeros(P.m, P.N);                    % high above-line, observed S
    U_ab0(1,:) = U_obs(1,:);
    U_ab0(2,:) = ub(2,:);
    U_ab0(:,1:(P.q_start-1)) = 0;
    starts{3} = U_ab0;
    U_bl0 = zeros(P.m, P.N);                    % high below-line, observed S
    U_bl0(1,:) = U_obs(1,:);
    U_bl0(3,:) = ub(3,:);
    U_bl0(4,:) = ub(4,:);
    U_bl0(:,1:(P.q_start-1)) = 0;
    starts{4} = U_bl0;
end

function [X, U, J, Js, best_ix] = planner_multistart(starts, x0, P, C, W, lb, ub, verbose)  % [V22 NEU]
% Best-of-N iLQR ueber die Startpolitiken.
    labels = {'observed','zero','high-above','high-below'};
    nS = numel(starts);
    Js = zeros(nS,1); Xs = cell(nS,1); Us = cell(nS,1);
    for s = 1:nS
        if verbose
            fprintf('Start %d (%s)\n', s, labels{min(s,numel(labels))});
        end
        [Xs{s}, Us{s}, Js(s)] = ilqr(starts{s}, x0, P, C, W, lb, ub);
    end
    [J, best_ix] = min(Js);
    X = Xs{best_ix}; U = Us{best_ix};
end

function W = make_terminal(P, W, verbose)                          % [V22 NEU]
% Terminal-Gewichtsmatrix P_inf gemaess W.terminal_mode setzen.
    switch W.terminal_mode
        case 'riccati'
            [A_s,B_s,Q_s,R_s] = stationary_matrices(P, W);
            W.P_inf = solve_dare(A_s, B_s, Q_s, R_s, W.beta);
            if verbose
                fprintf('  [terminal] Riccati: P_inf(b,b)=%.3f (uplift M=%.1f x w_b)\n', ...
                        W.P_inf(2,2), W.P_inf(2,2)/W.w_b);
            end
        case 'debt_M'
            W.P_inf = zeros(P.n); W.P_inf(2,2) = W.M_term*W.w_b;
            if verbose
                fprintf('  [terminal] reduced-form debt uplift M = %.1f\n', W.M_term);
            end
        otherwise
            W.P_inf = zeros(P.n);
    end
end

function xp = f_step(x, u, q, P, C)
    S   = u(1);
    fab = u(2);
    flo = u(3);   % effective loans
    fgu = u(4);   % effective guarantees
    fdi = u(5);

    y      = x(1);
    b      = x(2);
    th     = x(3);
    d      = x(4);
    a1     = x(5);
    di1    = x(7);
    st_liq = x(8);
    st_cap = x(9);

    liq_used = (1-P.decay_K)*st_liq + flo + fgu;
    cap_used = (1-P.decay_cap)*st_cap + fab;
    cap_multiplier = 1 + P.chi_cap_liq * cap_used / (cap_used + P.cap_scale);

    eth = 0;
    if (q+2) <= numel(C.eps_th)
        eth = C.eps_th(q+2);
    end
    if q < P.q_vax
        eth = max(eth, 0);
    end

    xp = zeros(P.n,1);

    xp(3) = P.rho_th_q(q)*(1-P.phi_S*S/100)*th*(1 - th/P.th_max) + eth;
    xp(4) = P.delta_q(q)*th;

    xp(1) = C.mu_y + P.rho_y*y + P.alpha_S*S ...
          + P.alpha_cap*cap_used ...
          + P.alpha_below*liq_used*cap_multiplier ...
          + P.alpha_DI*di1 ...
          + P.alpha_SDI*S*di1 ...
          - P.beta_d*d + C.eps_y(q+1);

    xp(2) = C.mu_b + (1+P.r)*b - P.gamma_y*y + P.k_ab*fab ...
          + P.k_lo*flo + P.k_gu*fgu + P.k_di*di1 ...
          + P.phi_t*P.yr(q);

    xp(5) = fab;
    xp(6) = a1;
    xp(7) = fdi;
    xp(8) = liq_used;
    xp(9) = cap_used;
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
        sc = W.w_y*x(1)^2 ...
           + W.lam_d*W.w_d*x(4)^2 ...
           + W.w_b*(x(2)-C.b0)^2;
        cc = sum(W.p_u .* (u ./ W.u_scale).^2);
        stockc = W.p_stock * (x(8) / W.stock_scale)^2;
        J = J + W.beta^q * (sc + cc + stockc);
    end
    dx = X(:,end) - W.xbar;
    J = J + W.beta^P.N * (dx' * W.P_inf * dx);
end

function [A,B] = jacobians(x, u, q, P)
    S   = u(1);
    fab = u(2);
    flo = u(3);
    fgu = u(4);

    th      = x(3);
    di1     = x(7);
    st_liq  = x(8);
    st_cap  = x(9);

    liq_used = (1-P.decay_K)*st_liq + flo + fgu;
    cap_used = (1-P.decay_cap)*st_cap + fab;

    cap_den = cap_used + P.cap_scale;
    cap_multiplier = 1 + P.chi_cap_liq * cap_used / cap_den;
    dmult_dcap = P.chi_cap_liq * P.cap_scale / (cap_den^2);

    A = zeros(P.n);
    B = zeros(P.n,P.m);

    A(1,1) = P.rho_y;
    A(1,4) = -P.beta_d;
    A(1,7) = P.alpha_DI + P.alpha_SDI*S;
    A(1,8) = P.alpha_below * cap_multiplier * (1-P.decay_K);
    A(1,9) = (P.alpha_cap + P.alpha_below*liq_used*dmult_dcap) * (1-P.decay_cap);

    B(1,1) = P.alpha_S + P.alpha_SDI*di1;
    B(1,2) = P.alpha_cap + P.alpha_below*liq_used*dmult_dcap;
    B(1,3) = P.alpha_below * cap_multiplier;
    B(1,4) = P.alpha_below * cap_multiplier;

    A(2,1) = -P.gamma_y;
    A(2,2) = 1+P.r;
    A(2,7) = P.k_di;

    B(2,2) = P.k_ab;
    B(2,3) = P.k_lo;
    B(2,4) = P.k_gu;

    g = P.rho_th_q(q)*(1-P.phi_S*S/100);
    A(3,3) = g*(1 - 2*th/P.th_max);
    B(3,1) = -P.rho_th_q(q)*P.phi_S/100*th*(1 - th/P.th_max);

    A(4,3) = P.delta_q(q);

    A(6,5) = 1;
    A(8,8) = 1-P.decay_K;
    A(9,9) = 1-P.decay_cap;

    B(5,2) = 1;
    B(7,5) = 1;
    B(8,3) = 1;
    B(8,4) = 1;
    B(9,2) = 1;
end

function [lx,lu,lxx,luu] = cost_derivs(xp, u, q, P, C, W)
% Running cost derivatives with respect to next state xp and control u.
    dq = W.beta^q;

    lx  = zeros(P.n,1);
    lxx = zeros(P.n);

    lx(1) = 2*dq*W.w_y*xp(1);
    lx(4) = 2*dq*W.lam_d*W.w_d*xp(4);
    lx(2) = 2*dq*W.w_b*(xp(2)-C.b0);

    lxx(1,1) = 2*dq*W.w_y;
    lxx(4,4) = 2*dq*W.lam_d*W.w_d;
    lxx(2,2) = 2*dq*W.w_b;

    lx(8)    = lx(8)    + 2*dq*W.p_stock*xp(8)/(W.stock_scale^2);
    lxx(8,8) = lxx(8,8) + 2*dq*W.p_stock/(W.stock_scale^2);

    lu  = 2*dq * (W.p_u .* u ./ (W.u_scale.^2));
    luu = 2*dq * diag(W.p_u ./ (W.u_scale.^2));
end

function [X,U,J] = ilqr(U0, x0, P, C, W, lb, ub)
% iLQR main loop. NO cost cross term (separable cost). dS limit enforced
% in the FORWARD pass by clamping S relative to the previous quarter.
    U = U0; X = rollout(U, x0, P, C); J = total_cost(X, U, P, C, W);
    mu = 1e-6; mu_max = 1e12; max_iter = 12000; tol = 1e-9;
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
            Qxx = A'*Vxx_*A;
            Qux = B'*Vxx_*A;
            Quu0 = luu + B'*Vxx_*B;
            Quu0 = 0.5 * (Quu0 + Quu0');
            Dmu = diag(1 ./ (W.u_scale.^2));
            Quu = Quu0 + mu * Dmu;
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
% Post-pandemic stationary LQ problem (deviation form), n=9.
    A = zeros(P.n);
    B = zeros(P.n, P.m);

    A(1,1) = P.rho_y;
    A(1,4) = -P.beta_d;
    A(1,7) = P.alpha_DI;
    A(1,8) = P.alpha_below*(1-P.decay_K);
    A(1,9) = P.alpha_cap*(1-P.decay_cap);

    B(1,1) = P.alpha_S;
    B(1,2) = P.alpha_cap;
    B(1,3) = P.alpha_below;
    B(1,4) = P.alpha_below;

    A(2,1) = -P.gamma_y;
    A(2,2) = 1+P.r;
    A(2,7) = P.k_di;

    B(2,2) = P.k_ab;
    B(2,3) = P.k_lo;
    B(2,4) = P.k_gu;

    A(3,3) = P.rho_th_q(end);
    A(4,3) = P.delta_q(end);

    A(6,5) = 1;
    A(8,8) = 1-P.decay_K;
    A(9,9) = 1-P.decay_cap;

    B(5,2) = 1;
    B(7,5) = 1;
    B(8,3) = 1;
    B(8,4) = 1;
    B(9,2) = 1;

    Q = zeros(P.n);
    Q(1,1) = W.w_y;
    Q(4,4) = W.lam_d*W.w_d;
    Q(2,2) = W.w_b;

    if isfield(W,'p_stock') && isfield(W,'stock_scale') && W.stock_scale > 0
        Q(8,8) = W.p_stock/(W.stock_scale^2);
    end

    R = diag(W.p_u ./ (W.u_scale.^2));
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

function Comp = welfare_decomposition(X, U, P, C, W)
% Decompose total_cost() into interpretable welfare components.
    Jy = 0; Jd = 0; Jb = 0; JS = 0; JF = 0; JStock = 0;

    for q = 1:P.N
        xq = X(:,q+1);
        uq = U(:,q);
        dq = W.beta^q;

        Jy = Jy + dq * W.w_y * xq(1)^2;
        Jd = Jd + dq * W.lam_d * W.w_d * xq(4)^2;
        Jb = Jb + dq * W.w_b * (xq(2) - C.b0)^2;

        if isfield(W,'p_stock') && isfield(W,'stock_scale') && W.stock_scale > 0
            JStock = JStock + dq * W.p_stock * (xq(8) / W.stock_scale)^2;
        end

        JS = JS + dq * W.p_u(1) * (uq(1) / W.u_scale(1))^2;
        JF = JF + dq * sum(W.p_u(2:5) .* (uq(2:5) ./ W.u_scale(2:5)).^2);
    end

    dx = X(:,end) - W.xbar;
    JT = W.beta^P.N * (dx' * W.P_inf * dx);

    Comp.Output     = Jy;
    Comp.Mortality  = Jd;
    Comp.Debt       = Jb;
    Comp.Stringency = JS;
    Comp.Fiscal     = JF;
    Comp.Stock      = JStock;
    Comp.Terminal   = JT;
    Comp.Total      = Jy + Jd + Jb + JS + JF + JStock + JT;
end

function M = frontier_metrics(X, P, C, W)
% Metrics used for constrained policy frontier.
    idx_eval = 2:P.N;
    state_idx = idx_eval + 1;
    beta_vec = W.beta .^ idx_eval;

    y = X(1,state_idx);
    d = X(4,state_idx);
    b = X(2,state_idx);

    M.Ly    = sum(beta_vec .* (y.^2));
    M.cum_y = sum(y);
    M.Dcum  = sum(d) * 13;
    M.Ld    = sum(beta_vec .* (d.^2));
    M.bT    = b(end);
    M.dbT   = b(end) - C.b0;
end

function [Xopt, Uopt, Mopt, exitflag] = solve_frontier(mode, U_start, x0, P, C, W, lb, ub, M_obs, frontier)
% Solve one constrained-policy-frontier problem using fmincon.
    z0  = U_start(:);
    zlb = lb(:);
    zub = ub(:);

    opts = optimoptions('fmincon', ...
        'Algorithm','sqp', ...
        'Display','final', ...
        'MaxFunctionEvaluations', 2e5, ...
        'MaxIterations', 2000, ...
        'OptimalityTolerance', 1e-8, ...
        'StepTolerance', 1e-10, ...
        'ConstraintTolerance', 1e-8);

    [zopt, fval, exitflag] = fmincon(@frontier_obj_nested, z0, [], [], [], [], ...
                                     zlb, zub, @frontier_nonlcon_nested, opts);

    Uopt = reshape(zopt, P.m, P.N);
    Xopt = rollout(Uopt, x0, P, C);
    Mopt = frontier_metrics(Xopt, P, C, W);

    fprintf('  exitflag = %d | objective = %.6f\n', exitflag, fval);
    fprintf('  Ly %.4f vs obs %.4f | D %.1f vs obs %.1f | bT %.2f vs obs %.2f | cum_y %.2f\n', ...
        Mopt.Ly, M_obs.Ly, Mopt.Dcum, M_obs.Dcum, Mopt.bT, M_obs.bT, Mopt.cum_y);
    fprintf('  policy totals: avg S %.2f | above %.2f | below-eff %.2f | DI %.2f\n', ...
        mean(Uopt(1,2:P.N)), sum(Uopt(2,2:P.N)), ...
        sum(Uopt(3,2:P.N)+Uopt(4,2:P.N)), sum(Uopt(5,2:P.N)));

    function f = frontier_obj_nested(z)
        Utmp = reshape(z, P.m, P.N);
        Xtmp = rollout(Utmp, x0, P, C);
        Mtmp = frontier_metrics(Xtmp, P, C, W);

        switch mode
            case 'output',    f_main = Mtmp.Ly;
            case 'mortality', f_main = Mtmp.Dcum;
            case 'debt',      f_main = Mtmp.bT;
            otherwise, error('Unknown frontier mode: %s', mode);
        end

        tie = 0;
        for qq = 1:P.N
            uq = Utmp(:,qq);
            tie = tie + sum((uq ./ W.u_scale).^2);
        end
        f = f_main + frontier.tie_eps * tie;
    end

    function [c, ceq] = frontier_nonlcon_nested(z)
        Utmp = reshape(z, P.m, P.N);
        Xtmp = rollout(Utmp, x0, P, C);
        Mtmp = frontier_metrics(Xtmp, P, C, W);

        switch mode
            case 'output'
                c = [Mtmp.Dcum - M_obs.Dcum; Mtmp.bT - M_obs.bT];
            case 'mortality'
                c = [Mtmp.Ly - M_obs.Ly; Mtmp.bT - M_obs.bT];
            case 'debt'
                c = [Mtmp.Ly - M_obs.Ly; Mtmp.Dcum - M_obs.Dcum];
            otherwise
                error('Unknown frontier mode: %s', mode);
        end

        for qq = 1:P.N
            if qq == 1, Sprev = 0; else, Sprev = Utmp(1,qq-1); end
            Sq = Utmp(1,qq);
            c = [c; Sq - Sprev - P.dS_max; Sprev - Sq - P.dS_max];
        end
        ceq = [];
    end
end

function [Xbest, Ubest, Mbest, exitbest] = solve_frontier_multistart(mode, starts, x0, P, C, W, lb, ub, M_obs, frontier)
% Multistart wrapper around solve_frontier().
    best_obj = Inf;
    Xbest = []; Ubest = []; Mbest = []; exitbest = NaN;
    tol_feas = 1e-4;

    for s = 1:numel(starts)
        fprintf('\n  Frontier %s | start %d/%d\n', mode, s, numel(starts));
        try
            [Xs, Us, Ms, exits] = solve_frontier(mode, starts{s}, x0, P, C, W, lb, ub, M_obs, frontier);
        catch ME
            fprintf('    start %d failed: %s\n', s, ME.message);
            continue;
        end

        switch mode
            case 'output'
                feas = Ms.Dcum <= M_obs.Dcum + tol_feas && Ms.bT <= M_obs.bT + tol_feas;
                obj_s = Ms.Ly;
            case 'mortality'
                feas = Ms.Ly <= M_obs.Ly + tol_feas && Ms.bT <= M_obs.bT + tol_feas;
                obj_s = Ms.Dcum;
            case 'debt'
                feas = Ms.Ly <= M_obs.Ly + tol_feas && Ms.Dcum <= M_obs.Dcum + tol_feas;
                obj_s = Ms.bT;
            otherwise
                error('Unknown frontier mode: %s', mode);
        end

        fprintf('    result: exit %d | feasible %d | Ly %.4f | D %.1f | bT %.2f | above %.2f | below %.2f | DI %.2f\n', ...
            exits, feas, Ms.Ly, Ms.Dcum, Ms.bT, ...
            sum(Us(2,2:P.N)), sum(Us(3,2:P.N)+Us(4,2:P.N)), sum(Us(5,2:P.N)));

        if feas && obj_s < best_obj
            best_obj = obj_s;
            Xbest = Xs; Ubest = Us; Mbest = Ms; exitbest = exits;
        end
    end

    if isempty(Ubest)
        error('No feasible frontier solution found for mode %s.', mode);
    end

    fprintf('\n  BEST frontier %s:\n', mode);
    fprintf('    Ly %.4f | D %.1f | bT %.2f | above %.2f | below %.2f | DI %.2f | exit %d\n', ...
        Mbest.Ly, Mbest.Dcum, Mbest.bT, ...
        sum(Ubest(2,2:P.N)), sum(Ubest(3,2:P.N)+Ubest(4,2:P.N)), sum(Ubest(5,2:P.N)), exitbest);
end