%% ========================================================================
%  PANDEMIC TRILEMMA - CROSS-COUNTRY SOLVER V23
%  ------------------------------------------------------------------------
%  Ziel (Section 6.4): landesspezifische Frontier-Distanz als Hauptbefund.
%  Design-Entscheidungen (fixiert):
%   (1) GEMEINSAME Caps (OECD-weit gepoolte 99. Perzentile) und gemeinsames
%       dS_max: alle Laender waehlen aus demselben Politik-Menue.
%   (2) Schulden durchgehend als Delta b = b_T - b0_i (Laender-Levels sind
%       ueber b0 von 7% bis 205% nicht vergleichbar).
%   (3) GEMEINSAME Gewichte (nur fuer die sekundaere Planner-Metrik relevant;
%       die Frontier ist gewichtsfrei und bleibt der Hauptbefund).
%   (4) Volles Multistart-Set (5 Starts je Frontier-Programm inkl.
%       Weighted-Planner-Loesung); Feasibility ex post verifiziert.
%
%  Struktur je Land i (gemeinsame Strukturparameter P, gemeinsames W):
%   C_i: eps_theta aus dem laendereigenen theta-Pfad, eps_y = Onset-Schock,
%        mu_y/mu_b = Laender-FE, b0 = b0_i. U_obs_i = beobachtete Politik.
%
%  Outputs:
%   table_countries_inputs.csv        (Bedingungen)
%   table_countries_observed.csv      (Model@obs-Outcomes + Politik-Deskriptoren)
%   table_countries_frontier.csv      (HAUPTBEFUND: A/B/C-Improvements)
%   table_countries_planner.csv       (sekundaer: gewichteter Gain)
%   table_countries_counterfactual.csv(Null-Politik + realisierter y/b-Wechselkurs)
%   fig_countries_frontier_bars_aerstyle.pdf/.png
%   fig_countries_scatter_AC_aerstyle.pdf/.png      (Schlagzeilen-Figur)
%   fig_countries_correlates_aerstyle.pdf/.png
%
%  Laufzeit-Hinweis: 38 Laender x 3 Szenarien x 5 Starts ~ 570 fmincon-Laeufe.
%  Fuer Tests RUN.countries_subset setzen (z.B. {'JPN','DEU','USA'}).
%  Die Laenderschleife ist unabhaengig -> bei Bedarf 'for' durch 'parfor'
%  ersetzen (Parallel Computing Toolbox).
%
%  Requires: country_data_for_matlab.csv, weekly_mortality_matlab.csv,
%            theta_quarterly_CRI_JPN_TUR_frommonthly.csv
% =========================================================================
clear; clc; close all;
fprintf('=== TRILEMMA CROSS-COUNTRY SOLVER V23 ===\n  %s\n\n', datestr(now));

%% ------------------------------------------------------------------------
%  RUN FLAGS
% -------------------------------------------------------------------------
RUN.frontier          = true;    % Hauptbefund (langsam)
RUN.weighted          = true;    % sekundaere Metrik (iLQR, schnell)
RUN.figures           = true;
RUN.n_frontier_starts = 5;       % 5 = volles Set; fuer Tests 2-3
RUN.countries_subset  = {};      % leer = alle 38; z.B. {'JPN','DEU','USA'}
RUN.ifr_invariance_check = true; % verifiziert die IFR-Skalierungs-Invarianz
                                 % numerisch an zwei Laendern (billig)

%% ------------------------------------------------------------------------
%  Calibrated V15 parameters (identisch zu V21/V22)
% -------------------------------------------------------------------------
P.rho_y=0.231;
P.alpha_S=-0.095;
P.alpha_above=0.544;
P.alpha_below=0.261;
P.alpha_DI=1.470;
P.alpha_SDI=-0.041;
P.beta_d=0;
P.c_lo = 0.6;
P.c_gu = 0.25;
P.r=0.001;
P.gamma_y=0.117;
P.k_ab=0.664;
P.k_lo=0.836;
P.k_gu=0.536;
P.k_di=0.526;
P.phi_t= 0;
P.omega_ab_now = 0.0;

target_half_life_cap_q = 6;
P.decay_cap = 1 - 0.5^(1/target_half_life_cap_q);
P.alpha_cap   = 0.30 * P.alpha_above;
P.chi_cap_liq = 0.50;
P.cap_scale   = NaN;   % set after ub_cap

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

qord={'Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021', ...
      'Q3.2021','Q4.2021','Q1.2022','Q2.2022','Q3.2022','Q4.2022'};

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
%  DATA (identisch zu V21/V22, per-country Arrays bleiben erhalten)
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
for i = 1:n_c
    iso = countries{i};
    for k = 1:N
        row = T(strcmp(T.Country,iso) & strcmp(T.Quarter,qord{k}),:);
        if isempty(row), continue; end
        S_o(i,k)=row.S_mean_tw;  Fa(i,k)=row.F_CP_above_3;
        Fl(i,k) = P.c_lo * row.F_CP_loans;
        Fg(i,k) = P.c_gu * row.F_CP_guar;
        Fd(i,k)=row.F_DI;
        y_o(i,k)=row.y_t_pct;
        if ~ismissing(row.debt_dR), bd(i,k)=row.debt_dR; end
        key = sprintf('%s_%s', iso, qord{k});
        if isKey(th_map,key) && ~isnan(th_map(key)), th_o(i,k)=th_map(key); end
        if isKey(d_map,key)  && ~isnan(d_map(key)),  d_o(i,k)=d_map(key);  end
    end
end
b_lvl = b0_val' + cumsum(bd,2);

% Laenderspezifische epidemiologische Innovationen (wie V21, ohne Mittelung)
eps_th = zeros(n_c,N+1);
for i = 1:n_c
    for k = 2:N
        expct = P.rho_th_q(k)*(1-P.phi_S*S_o(i,k)/100)*th_o(i,k-1)*(1 - th_o(i,k-1)/P.th_max);
        eps_th(i,k+1) = th_o(i,k) - expct;
    end
end

%% ------------------------------------------------------------------------
%  GEMEINSAME Gewichte, Caps, dS (identisch zu V22-Baseline)
% -------------------------------------------------------------------------
W.beta = 0.99;
y_scale = 5; b_scale = 10; d_scale = 100;
tau_b = 0.050; lam_d = 75;
W.w_y = 1 / y_scale^2;
W.w_b = tau_b / b_scale^2;
W.w_d = 1 / d_scale^2;
W.lam_d = lam_d;
W.p_u = 1e-8 * ones(5,1);
W.p_stock = 0.00;
W.stock_scale = 1;
W.terminal_mode = 'debt_M';
W.lam_cons = 0.99;
W.M_term   = 1/(1 - W.beta*W.lam_cons^2);

ub_cap = [pctile(S_o(S_o>0),99); pctile(Fa(Fa>0),99); pctile(Fl(Fl>0),99); ...
          pctile(Fg(Fg>0),99);   pctile(Fd(Fd>0),99)];
P.cap_scale = max(ub_cap(2), 1);
W.u_scale = ub_cap;
W.u_scale(W.u_scale <= 0 | isnan(W.u_scale) | isinf(W.u_scale)) = 1;
lb = zeros(P.m, P.N);
ub = repmat(ub_cap, 1, P.N);
% Gemeinsames Politik-Menue: S ebenfalls am gepoolten 99. Perzentil (Baseline,
% konsistent mit der Average-Economy-Baseline).
ub(:, 1:(P.q_start-1)) = 0;

dS_obs = abs(diff(S_o,1,2));
dS_obs = dS_obs(dS_obs > 0);
P.dS_max = pctile(dS_obs, 95);

W = make_terminal(P, W, true);
fprintf('  common caps: S<=%.1f, F_ab<=%.2f, F_lo<=%.2f, F_gu<=%.2f, F_DI<=%.2f | dS_max=%.1f\n', ...
        ub_cap, P.dS_max);
fprintf('  common weights: tau_b=%.3f, lam_d=%.0f\n\n', tau_b, lam_d);

frontier.tie_eps = 1e-8;
frontier.display = 'off';        % fmincon quiet in der Laenderschleife

%% ------------------------------------------------------------------------
%  Auswahl der Laender
% -------------------------------------------------------------------------
if isempty(RUN.countries_subset)
    idx_run = 1:n_c;
else
    idx_run = find(ismember(countries, RUN.countries_subset))';
end
nR = numel(idx_run);
fprintf('  running %d of %d countries\n\n', nR, n_c);

%% ------------------------------------------------------------------------
%  Ergebnis-Container
% -------------------------------------------------------------------------
% inputs
IN  = nan(nR, 6);   % b0, mu_y, mu_b, onset_eps_y, wave_sev, cum_th_zero
% observed (model@obs) + Politik-Deskriptoren + Closure
OB  = nan(nR, 16);  % Ly, cum_y, Dcum, dbT, avgS, totAb, totBel, totDI,
                    % below_share, DI_share, frontload, exceed_cap,
                    % clo_y, clo_d_ratio, clo_bT, exceed_dS
% frontier
FR  = nan(nR, 12);  % impA_pct, impA_abs, feasA, impB_pct, impB_abs, feasB,
                    % impC_pp, impC_pct_of_increase, feasC, exitA, exitB, exitC
% weighted planner
PL  = nan(nR, 7);   % gain_pct, cum_y, Dcum, dbT, totAb, totBel, avgS
% counterfactual (zero policy / zero fiscal)
CF  = nan(nR, 7);   % cum_y_zero, Dcum_zero, dbT_zero, cum_y_nofisc,
                    % dbT_nofisc, dy_per_db_fiscal, dd_avoided
% Exceedance-Detail: welches Instrument, welches Quartal, wie stark
EXD = zeros(nR, 3); % ex_inst (1..5, 0=keine), ex_q, exceed_val

%% ------------------------------------------------------------------------
%  LAENDERSCHLEIFE
% -------------------------------------------------------------------------
t_all = tic;
for rr = 1:nR
    i   = idx_run(rr);
    iso = countries{i};
    fprintf('===== [%2d/%2d] %s =====\n', rr, nR, iso);

    % ---- Laenderspezifische Closure-Objekte -----------------------------
    Ci.b0   = b0_val(i);
    Ci.mu_y = cfe_y_val(i);
    Ci.mu_b = cfe_b_val(i);
    Ci.eps_th = eps_th(i,:);
    Ci.eps_y  = zeros(1,N+1);
    Ci.eps_y(4) = eps_v14(i);

    x0 = zeros(P.n,1); x0(2) = Ci.b0;
    Wi = W; Wi.xbar = x0;      % Terminal-Target: laendereigenes b0

    % ---- Beobachtete Politik --------------------------------------------
    U_obs = [S_o(i,:); Fa(i,:); Fl(i,:); Fg(i,:); Fd(i,:)];
    U_obs(:,1:(P.q_start-1)) = 0;
    Ex = max(U_obs - ub, 0);                           % m x N Exceedance-Matrix
    exceed_cap = max(Ex, [], 'all');                   % >0: obs ausserhalb Box-Menue
    if exceed_cap > 0
        [exceed_val, lin]  = max(Ex(:));
        [ex_inst, ex_q]    = ind2sub(size(Ex), lin);
    else
        exceed_val = 0; ex_inst = 0; ex_q = 0;
    end
    dSpath = [U_obs(1,1), diff(U_obs(1,:))];
    exceed_dS = max(0, max(abs(dSpath)) - P.dS_max);   % >0: obs schneller als dS_max (ITA-Fall)

    % ---- Rollouts --------------------------------------------------------
    Xo   = rollout(U_obs, x0, P, Ci);
    Xz   = rollout(zeros(P.m,P.N), x0, P, Ci);      % Voll-Null (inkl. S=0)
    U_nofisc = zeros(P.m, P.N);                      % Fiskal-Null, S beobachtet
    U_nofisc(1,:) = U_obs(1,:);
    Xnf  = rollout(U_nofisc, x0, P, Ci);
    Mobs = frontier_metrics(Xo, P, Ci, Wi);
    Mz   = frontier_metrics(Xz, P, Ci, Wi);
    Mnf  = frontier_metrics(Xnf, P, Ci, Wi);

    idx_eval = 2:P.N; state_idx = idx_eval + 1;

    % ---- Inputs / Bedingungen -------------------------------------------
    wave_sev = sum(max(Ci.eps_th, 0));
    IN(rr,:) = [Ci.b0, Ci.mu_y, Ci.mu_b, eps_v14(i), wave_sev, sum(Xz(3,2:end))];

    % ---- Closure-Check gegen Rohdaten ------------------------------------
    cum_y_raw = sum(y_o(i,:));
    cum_d_raw = sum(d_o(i,:))*13;
    clo_y  = Mobs.cum_y - cum_y_raw;
    clo_d  = (Mobs.Dcum) / max(cum_d_raw, 1e-6);
    clo_bT = Mobs.bT - b_lvl(i,end);

    % ---- Politik-Deskriptoren --------------------------------------------
    totAb  = sum(U_obs(2,idx_eval));
    totBel = sum(U_obs(3,idx_eval)+U_obs(4,idx_eval));
    totDI  = sum(U_obs(5,idx_eval));
    totAll = totAb + totBel + totDI;
    fisc_q = sum(U_obs(2:5, idx_eval), 1);
    if totAll > 1e-6
        frontload = sum(fisc_q(1:3)) / totAll;   % Anteil Q1.20-Q3.20
        below_share = totBel/totAll;  DI_share = totDI/totAll;
    else
        frontload = NaN; below_share = NaN; DI_share = NaN;
    end
    OB(rr,:) = [Mobs.Ly, Mobs.cum_y, Mobs.Dcum, Mobs.bT - Ci.b0, ...
                mean(U_obs(1,idx_eval)), totAb, totBel, totDI, ...
                below_share, DI_share, frontload, exceed_cap, ...
                clo_y, clo_d, clo_bT, exceed_dS];
    EXD(rr,:) = [ex_inst, ex_q, exceed_val];

    % ---- Counterfactuals --------------------------------------------------
    % (a) Fiskal-Wechselkurs: beobachtetes S, Fiskal = 0 -> isoliert den
    %     fiskalischen Output-Gewinn pro fiskalisch induzierter Schuldeneinheit.
    dy_fisc = Mobs.cum_y - Mnf.cum_y;
    db_fisc = Mobs.bT - Mnf.bT;
    if db_fisc > 0.5
        dy_per_db = dy_fisc / db_fisc;
    else
        dy_per_db = NaN;
    end
    % (b) Voll-Null (inkl. S=0): vermiedene Tote durch das Gesamtpaket.
    dd_pol = Mz.Dcum - Mobs.Dcum;
    CF(rr,:) = [Mz.cum_y, Mz.Dcum, Mz.bT - Ci.b0, ...
                Mnf.cum_y, Mnf.bT - Ci.b0, dy_per_db, dd_pol];

    % ---- Gewichteter Planner (sekundaer) ----------------------------------
    Uw = [];
    if RUN.weighted
        [Xw, Uw, Jw] = planner_multistart(planner_starts(U_obs, ub, P), ...
                                          x0, P, Ci, Wi, lb, ub, false);
        J_obs = total_cost(Xo, U_obs, P, Ci, Wi);
        PL(rr,:) = [100*(J_obs-Jw)/J_obs, sum(Xw(1,state_idx)), ...
                    sum(Xw(4,state_idx))*13, Xw(2,end) - Ci.b0, ...
                    sum(Uw(2,idx_eval)), sum(Uw(3,idx_eval)+Uw(4,idx_eval)), ...
                    mean(Uw(1,idx_eval))];
        fprintf('  weighted gain %.1f%%\n', PL(rr,1));
    end

    % ---- Frontier (Hauptbefund) -------------------------------------------
    if RUN.frontier
        starts = planner_starts(U_obs, ub, P);       % obs, zero, high-ab, high-bel
        if ~isempty(Uw), starts{end+1} = Uw; end
        starts = starts(1:min(numel(starts), RUN.n_frontier_starts));

        % A: Output
        try
            [~, UA, MA, exA] = solve_frontier_multistart('output', starts, x0, P, Ci, Wi, lb, ub, Mobs, frontier);
            FR(rr,1)  = 100*(Mobs.Ly - MA.Ly)/Mobs.Ly;
            FR(rr,2)  = MA.cum_y - Mobs.cum_y;
            FR(rr,3)  = 1;  FR(rr,10) = exA;
        catch ME
            fprintf('  frontier A infeasible/failed: %s\n', ME.message);
            FR(rr,3) = 0;
        end
        % B: Mortality
        try
            [~, UB, MB, exB] = solve_frontier_multistart('mortality', starts, x0, P, Ci, Wi, lb, ub, Mobs, frontier);
            if Mobs.Dcum > 1
                FR(rr,4) = 100*(Mobs.Dcum - MB.Dcum)/Mobs.Dcum;
            end
            FR(rr,5)  = Mobs.Dcum - MB.Dcum;
            FR(rr,6)  = 1;  FR(rr,11) = exB;
        catch ME
            fprintf('  frontier B infeasible/failed: %s\n', ME.message);
            FR(rr,6) = 0;
        end
        % C: Debt
        try
            [~, UC, MC, exC] = solve_frontier_multistart('debt', starts, x0, P, Ci, Wi, lb, ub, Mobs, frontier);
            FR(rr,7)  = Mobs.bT - MC.bT;                       % pp of GDP
            db_obs    = Mobs.bT - Ci.b0;
            if db_obs > 10
                FR(rr,8) = 100*(Mobs.bT - MC.bT)/db_obs;       % nur bei substanziellem Aufbau
            end
            FR(rr,9)  = 1;  FR(rr,12) = exC;
        catch ME
            fprintf('  frontier C infeasible/failed: %s\n', ME.message);
            FR(rr,9) = 0;
        end
        fprintf('  frontier: A %.1f%% | B %.1f%% | C %.1f pp (%.1f%% of increase)\n', ...
                FR(rr,1), FR(rr,4), FR(rr,7), FR(rr,8));
    end
end
fprintf('\nTotal runtime: %.1f min\n\n', toc(t_all)/60);

%% ------------------------------------------------------------------------
%  TABELLEN SCHREIBEN
% -------------------------------------------------------------------------
ISO = string(countries(idx_run));

% ---- Kontinuierlicher Fit-Score + IFR-Skalenfaktor ----------------------
% fit_i = max( |log DeathRatio| / MAD, |Delta cum_y| / MAD ), jeweils gegen
% die Median-Absolutabweichung des Panels normiert. Keine harte Schwelle:
% Reporting ueber das oberste Quartil; Ranking-Stabilitaet via Leave-one-out.
logDR   = log(OB(:,14));
abs_dy  = abs(OB(:,13));
mad_DR  = median(abs(logDR), 'omitnan');
mad_dy  = median(abs_dy, 'omitnan');
fit_score = max(abs(logDR)/max(mad_DR,eps), abs_dy/max(mad_dy,eps));

% s_i: multiplikativer IFR-Skalar, der die Modell-Toten auf die Rohdaten
% bringt. Unter beta_d = 0 sind die Frontier-Programme gegen diese Skalierung
% EXAKT invariant (Tote = linearer Readout, beide Constraint-Seiten skalieren
% identisch); s_i ist daher reine Diagnose, kein Rerun noetig.
raw_cum_d = arrayfun(@(i) sum(d_o(i,:))*13, idx_run(:));
ifr_scale = raw_cum_d ./ OB(:,3);

T_in = array2table(IN, 'VariableNames', ...
    {'b0','mu_y','mu_b','onset_eps_y','wave_severity','cum_theta_zeropolicy'});
T_in = addvars(T_in, ISO, 'Before', 1, 'NewVariableNames','Country');
writetable(T_in, 'table_countries_inputs.csv');

T_ob = array2table(OB, 'VariableNames', ...
    {'Ly_obs','Cum_y_obs','Dcum_obs','Delta_b_obs','Avg_S_obs', ...
     'Total_above','Total_below_eff','Total_DI', ...
     'Below_share','DI_share','Frontload_share_Q1Q3','Obs_exceeds_common_cap', ...
     'Closure_cum_y_model_minus_raw','Closure_deaths_model_over_raw','Closure_bT_model_minus_raw', ...
     'Obs_exceeds_dS_max'});
T_ob = addvars(T_ob, ISO, 'Before', 1, 'NewVariableNames','Country');
inst_lbl = ["none","S","F_above","F_loans","F_guar","F_DI"];
Exceed_instrument = inst_lbl(EXD(:,1) + 1)';
Exceed_quarter = strings(nR,1);
for rr = 1:nR
    if EXD(rr,2) > 0, Exceed_quarter(rr) = string(qord{EXD(rr,2)}); end
end
T_ob = addvars(T_ob, fit_score, ifr_scale, Exceed_instrument, Exceed_quarter, EXD(:,3), ...
               'NewVariableNames', ...
               {'Fit_score','IFR_scale_to_match_raw','Exceed_instrument', ...
                'Exceed_quarter','Exceed_value'});
writetable(T_ob, 'table_countries_observed.csv');

T_fr = array2table(FR, 'VariableNames', ...
    {'ImpA_output_pct','ImpA_cum_y_abs','FeasA', ...
     'ImpB_deaths_pct','ImpB_deaths_abs','FeasB', ...
     'ImpC_debt_pp','ImpC_pct_of_debt_increase','FeasC', ...
     'ExitA','ExitB','ExitC'});
T_fr = addvars(T_fr, ISO, 'Before', 1, 'NewVariableNames','Country');
writetable(T_fr, 'table_countries_frontier.csv');

T_pl = array2table(PL, 'VariableNames', ...
    {'Weighted_gain_pct','Cum_y_planner','Dcum_planner','Delta_b_planner', ...
     'Total_above_planner','Total_below_planner','Avg_S_planner'});
T_pl = addvars(T_pl, ISO, 'Before', 1, 'NewVariableNames','Country');
writetable(T_pl, 'table_countries_planner.csv');

T_cf = array2table(CF, 'VariableNames', ...
    {'Cum_y_zeropolicy','Dcum_zeropolicy','Delta_b_zeropolicy', ...
     'Cum_y_nofiscal','Delta_b_nofiscal', ...
     'Fiscal_dy_per_db','Deaths_avoided_by_policy'});
T_cf = addvars(T_cf, ISO, 'Before', 1, 'NewVariableNames','Country');
writetable(T_cf, 'table_countries_counterfactual.csv');

%% ------------------------------------------------------------------------
%  ZUSAMMENFASSUNG (Konsole)
% -------------------------------------------------------------------------
fprintf('=== SUMMARY ===\n');
fprintf('Frontier feasible: A %d/%d | B %d/%d | C %d/%d\n', ...
    nansum(FR(:,3)), nR, nansum(FR(:,6)), nR, nansum(FR(:,9)), nR);
fprintf('Median improvements: A %.1f%% | B %.1f%% | C %.1f pp (%.1f%% of increase)\n', ...
    nanmedian(FR(:,1)), nanmedian(FR(:,4)), nanmedian(FR(:,7)), nanmedian(FR(:,8)));

% Ranking: Naehe zur eigenen Frontier (kleines impA UND kleines impC in pp;
% pp statt %-of-increase, robust gegen kleine Schulden-Nenner).
% Ausschluss: Laender, deren beobachtete Politik das gemeinsame Menue
% ueberschreitet oder deren "Improvement" negativ ist (Menue kann die eigene
% Politik nicht replizieren) - diese separat berichten, nicht ranken.
% Ausschluss NUR ueber die Konsequenz (Infeasibility / negatives Improvement,
% der ITA-Fall). Box-/Ramping-Ueberschreitungen sind deskriptive Flags: 16 von
% 38 Laendern rampten Q1->Q2 2020 schneller als das gepoolte dS_max - fuer sie
% findet die Frontier dennoch zulaessige Verbesserungen.
ok_rank = FR(:,3) == 1 & FR(:,9) == 1 & FR(:,1) >= -1e-6 & FR(:,7) >= -1e-6;
n_dS = sum(OB(:,16) > 1.0);
fprintf('\nDeskriptiv: %d Laender mit beobachteter Ramp > dS_max (+1pt Toleranz)\n', n_dS);
if any(~ok_rank)
    fprintf('\nAusserhalb des Rankings (Menue nicht replizierbar / negatives Improvement):\n');
    fprintf('  %s\n', strjoin(ISO(~ok_rank), ', '));
end
rankA = nan(nR,1); rankC = nan(nR,1);
rankA(ok_rank) = tiedrank(FR(ok_rank,1));
rankC(ok_rank) = tiedrank(FR(ok_rank,7));
dist  = (rankA + rankC)/2;
[~, ord] = sort(dist, 'ascend', 'MissingPlacement','last');
nR_ok = sum(ok_rank);
nshow = min(5, floor(nR_ok/2));
fprintf('\nClosest to own frontier (avg rank of ImpA_pct, ImpC_pp):\n');
for j = 1:nshow
    k = ord(j);
    fprintf('  %s: A %.1f%% | C %.1f pp | B %.1f%%\n', ...
        ISO(k), FR(k,1), FR(k,7), FR(k,4));
end
fprintf('Furthest from own frontier:\n');
for j = nR_ok-nshow+1:nR_ok
    k = ord(j);
    fprintf('  %s: A %.1f%% | C %.1f pp | B %.1f%%\n', ...
        ISO(k), FR(k,1), FR(k,7), FR(k,4));
end

% ---- Fit-Diagnose (kontinuierlich, oberstes Quartil statt harter Schwelle)
q75 = quantile(fit_score, 0.75);
weak_fit = fit_score > q75;
fprintf('\nSchwaechster Closure-Fit (oberstes Quartil des Fit-Scores):\n');
fprintf('  %s\n', strjoin(ISO(weak_fit), ', '));
fprintf('IFR-Skalenfaktoren s_i: median %.2f, range [%.2f, %.2f]\n', ...
        median(ifr_scale,'omitnan'), min(ifr_scale), max(ifr_scale));
fprintf('  (Frontier-Distanzen sind unter beta_d=0 exakt invariant gegen s_i;\n');
fprintf('   s_i betrifft nur berichtete Toten-Niveaus und die gewichtete Metrik.)\n');

% ---- Leave-one-out-Rangstabilitaet ---------------------------------------
% Spearman zwischen Baseline-Distanz und neu berechneter Distanz nach
% Ausschluss der k schlechtest-gefitteten Laender (auf dem Restsample).
[~, fit_ord] = sort(fit_score, 'descend', 'MissingPlacement','first');
for kdrop = [5 10]
    drop = false(nR,1); drop(fit_ord(1:kdrop)) = true;
    sub  = ok_rank & ~drop;
    if sum(sub) > 5
        rA = nan(nR,1); rC = nan(nR,1);
        rA(sub) = tiedrank(FR(sub,1));
        rC(sub) = tiedrank(FR(sub,7));
        dist_sub = (rA + rC)/2;
        rho = corr(dist(sub), dist_sub(sub), 'Type','Spearman');
        fprintf('Leave-one-out (schlechteste %2d Fits raus, n=%d): Spearman rho = %.3f\n', ...
                kdrop, sum(sub), rho);
    end
end

if any(OB(:,12) > 0.5)
    fprintf('Beobachtete Politik ausserhalb des gemeinsamen Menues (Box-Exceedance > 0.5):\n');
    for rr = find(OB(:,12) > 0.5)'
        fprintf('  %s: %s in %s um %.2f ueber Cap\n', ISO(rr), ...
                inst_lbl(EXD(rr,1)+1), qord{max(EXD(rr,2),1)}, EXD(rr,3));
    end
end

%% ------------------------------------------------------------------------
%  IFR-INVARIANZ: numerische Verifikation an zwei Laendern      [V23 NEU]
% -------------------------------------------------------------------------
% Proposition: unter beta_d = 0 sind die Frontier-Programme exakt invariant
% gegen delta_q_i = s_i * delta_q (Tote = linearer Readout; beide Seiten der
% Mortalitaets-Constraint skalieren identisch; Ly und bT beruehren delta_q
% nicht). Hier numerisch verifiziert an zwei stark fehlgefitteten Laendern.
if RUN.ifr_invariance_check
    check_iso = {'CHE','NLD'};
    for cc = 1:numel(check_iso)
        i = find(strcmp(countries, check_iso{cc}));
        if isempty(i), continue; end
        rr = find(idx_run == i);
        if isempty(rr) || isnan(FR(rr,1)), continue; end

        Ci.b0 = b0_val(i); Ci.mu_y = cfe_y_val(i); Ci.mu_b = cfe_b_val(i);
        Ci.eps_th = eps_th(i,:);
        Ci.eps_y = zeros(1,N+1); Ci.eps_y(4) = eps_v14(i);
        x0 = zeros(P.n,1); x0(2) = Ci.b0;
        Wi = W; Wi.xbar = x0;
        U_obs = [S_o(i,:); Fa(i,:); Fl(i,:); Fg(i,:); Fd(i,:)];
        U_obs(:,1:(P.q_start-1)) = 0;

        P2 = P; P2.delta_q = ifr_scale(rr) * P.delta_q;
        Xo2 = rollout(U_obs, x0, P2, Ci);
        M_obs2 = frontier_metrics(Xo2, P2, Ci, Wi);
        starts_f = planner_starts(U_obs, ub, P2);
        try
            [~, ~, MA2] = solve_frontier_multistart('output', starts_f, x0, P2, Ci, Wi, lb, ub, M_obs2, frontier);
            impA2 = 100*(M_obs2.Ly - MA2.Ly)/M_obs2.Ly;
            fprintf('IFR-Invarianz %s (s=%.2f): ImpA baseline %.2f%% | skaliert %.2f%% | diff %.1e\n', ...
                    check_iso{cc}, ifr_scale(rr), FR(rr,1), impA2, abs(FR(rr,1)-impA2));
        catch ME
            fprintf('IFR-Invarianz-Check %s fehlgeschlagen: %s\n', check_iso{cc}, ME.message);
        end
    end
end

%% ------------------------------------------------------------------------
%  FIGUREN (paper style, grayscale)
% -------------------------------------------------------------------------
if RUN.figures && nR > 3

% --- Sortierte Balken: ImpA ---------------------------------------------
[~, oA] = sort(FR(:,1), 'ascend', 'MissingPlacement','last');
fig = figure('Color','w','Position',[60 60 1100 420]);
bar(FR(oA,1), 0.7, 'FaceColor', [0.45 0.45 0.45], 'EdgeColor', [0 0 0]);
set(gca,'XTick',1:nR,'XTickLabel',ISO(oA),'XTickLabelRotation',90, ...
    'TickDir','out','Box','off','FontSize',8);
ylabel('Output improvement at own frontier (% of \(L_y\))','Interpreter','none');
title('Distance to the country-specific frontier: output dimension','FontWeight','normal');
grid on; ax = gca; ax.GridAlpha = 0.12; ax.XGrid = 'off';
exportgraphics(fig, 'fig_countries_frontier_bars_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_countries_frontier_bars_aerstyle.png','Resolution',300);

% --- Schlagzeilen-Scatter: ImpA vs ImpC (pp) ------------------------------
fig = figure('Color','w','Position',[80 80 780 620]);
scatter(FR(:,1), FR(:,7), 28, [0.25 0.25 0.25], 'filled'); hold on;
text(FR(:,1)+0.6, FR(:,7), ISO, 'FontSize', 7, 'Color', [0.2 0.2 0.2]);
xlabel('Foregone output stabilization: ImpA (% of observed \(L_y\))','Interpreter','none');
ylabel('Wasted debt: ImpC (pp of GDP)','Interpreter','none');
title('Distance to the country-specific policy frontier','FontWeight','normal');
grid on; ax = gca; ax.GridAlpha = 0.12;
set(gca,'TickDir','out','Box','off','FontSize',9);
exportgraphics(fig, 'fig_countries_scatter_AC_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_countries_scatter_AC_aerstyle.png','Resolution',300);

% --- Schlagzeilen-Scatter 2: DI-Share vs Fiskal-Wechselkurs ---------------
fig = figure('Color','w','Position',[80 80 780 620]);
xr_v = CF(:,6); di_v = OB(:,10);
scatter(di_v, xr_v, 28, [0.25 0.25 0.25], 'filled'); hold on;
% Selektive Labels: Story-Laender (Negativ-Gruppe, Grenzfaelle, Extreme);
% dichtes Mittelfeld bleibt unbeschriftet (Label-Kollisionen).
lbl_iso = ["COL","MEX","CHL","ISR","IRL","USA","CAN","JPN","ITA","TUR", ...
           "NOR","FRA","DEU","FIN","SWE","AUS","NZL","LTU","ISL"];
for rr = 1:nR
    if ismember(ISO(rr), lbl_iso) && isfinite(di_v(rr)) && isfinite(xr_v(rr))
        text(di_v(rr)+0.008, xr_v(rr), ISO(rr), 'FontSize', 7, ...
             'Color', [0.2 0.2 0.2]);
    end
end
okc = isfinite(di_v) & isfinite(xr_v);
pfit = polyfit(di_v(okc), xr_v(okc), 1);
xrng = linspace(min(di_v(okc)), max(di_v(okc)), 50);
plot(xrng, polyval(pfit, xrng), '-', 'Color', [0 0 0], 'LineWidth', 1.2);
yline(0, ':', 'Color', [0.4 0.4 0.4]);
rho_dx = corr(di_v(okc), xr_v(okc));
fprintf('Exchange-rate figure: corr(DI share, XR) = %.2f (fuer Caption)\n', rho_dx);
title('Realized fiscal exchange rate and demand-injection share', ...
      'FontWeight','normal');
xlabel('Observed demand-injection share of fiscal package');
ylabel('Cumulative output gain per pp of fiscally induced debt');
set(gca,'TickDir','out','Box','off','FontSize',9);
grid on; ax = gca; ax.GridAlpha = 0.12;
exportgraphics(fig, 'fig_countries_exchange_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_countries_exchange_aerstyle.png','Resolution',300);

% --- Korrelate: ImpA gegen b0, Below-Share, Frontloading ------------------
fig = figure('Color','w','Position',[80 80 1100 380]);
tiledlayout(1,3,'TileSpacing','compact','Padding','compact');
corr_x = {IN(:,1), OB(:,9), OB(:,11)};
corr_lbl = {'Initial debt b_0 (% of GDP)', 'Observed below-the-line share', ...
            'Observed frontloading share (Q1.20-Q3.20)'};
for pnl = 1:3
    nexttile;
    scatter(corr_x{pnl}, FR(:,1), 24, [0.35 0.35 0.35], 'filled'); hold on;
    ok = isfinite(corr_x{pnl}) & isfinite(FR(:,1));
    if sum(ok) > 5
        pfit = polyfit(corr_x{pnl}(ok), FR(ok,1), 1);
        xr = linspace(min(corr_x{pnl}(ok)), max(corr_x{pnl}(ok)), 50);
        plot(xr, polyval(pfit, xr), '-', 'Color', [0 0 0], 'LineWidth', 1.2);
        rho = corr(corr_x{pnl}(ok), FR(ok,1));
        title(sprintf('corr = %.2f', rho), 'FontWeight','normal');
    end
    xlabel(corr_lbl{pnl}); ylabel('ImpA (%)');
    set(gca,'TickDir','out','Box','off','FontSize',9);
    grid on; ax = gca; ax.GridAlpha = 0.12;
end
exportgraphics(fig, 'fig_countries_correlates_aerstyle.pdf','ContentType','vector');
exportgraphics(fig, 'fig_countries_correlates_aerstyle.png','Resolution',300);

end

fprintf('\n=== V23 DONE. Cross-country outputs geschrieben. ===\n');

%% ========================================================================
%  LOCAL FUNCTIONS (identisch zu V22, ilqr/solve_frontier mit quiet-Option)
% =========================================================================

function starts = planner_starts(U_obs, ub, P)
    starts = cell(4,1);
    starts{1} = U_obs;
    starts{2} = zeros(P.m, P.N);
    U_ab0 = zeros(P.m, P.N);
    U_ab0(1,:) = U_obs(1,:);
    U_ab0(2,:) = ub(2,:);
    U_ab0(:,1:(P.q_start-1)) = 0;
    starts{3} = U_ab0;
    U_bl0 = zeros(P.m, P.N);
    U_bl0(1,:) = U_obs(1,:);
    U_bl0(3,:) = ub(3,:);
    U_bl0(4,:) = ub(4,:);
    U_bl0(:,1:(P.q_start-1)) = 0;
    starts{4} = U_bl0;
end

function [X, U, J, Js, best_ix] = planner_multistart(starts, x0, P, C, W, lb, ub, verbose)
    labels = {'observed','zero','high-above','high-below','extra'};
    nS = numel(starts);
    Js = zeros(nS,1); Xs = cell(nS,1); Us = cell(nS,1);
    for s = 1:nS
        if verbose
            fprintf('Start %d (%s)\n', s, labels{min(s,numel(labels))});
        end
        [Xs{s}, Us{s}, Js(s)] = ilqr(starts{s}, x0, P, C, W, lb, ub, verbose);
    end
    [J, best_ix] = min(Js);
    X = Xs{best_ix}; U = Us{best_ix};
end

function W = make_terminal(P, W, verbose)
    switch W.terminal_mode
        case 'riccati'
            [A_s,B_s,Q_s,R_s] = stationary_matrices(P, W);
            W.P_inf = solve_dare(A_s, B_s, Q_s, R_s, W.beta);
            if verbose
                fprintf('  [terminal] Riccati: P_inf(b,b)=%.3f\n', W.P_inf(2,2));
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
    flo = u(3);
    fgu = u(4);
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

function [X,U,J] = ilqr(U0, x0, P, C, W, lb, ub, verbose)
    if nargin < 8, verbose = true; end
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
                uq = min(max(U(:,q)+du, lb(:,q)), ub(:,q));
                if q == 1
                    Sprev = 0;
                else
                    Sprev = Un(1,q-1);
                end
                uq(1) = min(max(uq(1), Sprev - P.dS_max), Sprev + P.dS_max);
                uq(1) = min(max(uq(1), lb(1,q)), ub(1,q));
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
            if verbose && mod(it,25)==0
                fprintf('  iter %3d: J = %10.4f (rel %.1e, mu %.1e)\n',it,J,rel,mu);
            end
            if rel < tol
                if verbose, fprintf('  CONVERGED at iter %d: J = %.4f\n', it, J); end
                break;
            end
        else
            mu = min(mu*10, mu_max); reject_run = reject_run + 1;
            if reject_run >= reject_cap
                if verbose
                    fprintf('  stalled (no progress in %d steps) at iter %d: J = %.4f\n', ...
                            reject_cap, it, J);
                end
                break;
            end
            if mu >= mu_max && reject_run >= 5
                if verbose, fprintf('  converged (mu saturated) at iter %d: J = %.4f\n', it, J); end
                break;
            end
        end
    end
end

function [A,B,Q,R] = stationary_matrices(P, W)
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

function M = frontier_metrics(X, P, C, W)
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
    z0  = U_start(:);
    zlb = lb(:);
    zub = ub(:);

    if isfield(frontier, 'display'), disp_mode = frontier.display; else, disp_mode = 'final'; end

    opts = optimoptions('fmincon', ...
        'Algorithm','sqp', ...
        'Display', disp_mode, ...
        'MaxFunctionEvaluations', 2e5, ...
        'MaxIterations', 2000, ...
        'OptimalityTolerance', 1e-8, ...
        'StepTolerance', 1e-10, ...
        'ConstraintTolerance', 1e-8);

    [zopt, ~, exitflag] = fmincon(@frontier_obj_nested, z0, [], [], [], [], ...
                                  zlb, zub, @frontier_nonlcon_nested, opts);

    Uopt = reshape(zopt, P.m, P.N);
    Xopt = rollout(Uopt, x0, P, C);
    Mopt = frontier_metrics(Xopt, P, C, W);

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
    best_obj = Inf;
    Xbest = []; Ubest = []; Mbest = []; exitbest = NaN;
    tol_feas = 1e-4;

    for s = 1:numel(starts)
        try
            [Xs, Us, Ms, exits] = solve_frontier(mode, starts{s}, x0, P, C, W, lb, ub, M_obs, frontier);
        catch
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

        % Ex-post: Ramping- und Box-Feasibility (faengt exitflag<0-Loesungen ab)
        dSvec = [Us(1,1), diff(Us(1,:))];
        dS_ok = max(abs(dSvec)) <= P.dS_max + 1e-3;
        box_ok = all(Us(:) >= lb(:) - 1e-6) && all(Us(:) <= ub(:) + 1e-6);
        feas = feas && dS_ok && box_ok;

        if feas && obj_s < best_obj
            best_obj = obj_s;
            Xbest = Xs; Ubest = Us; Mbest = Ms; exitbest = exits;
        end
    end

    if isempty(Ubest)
        error('No feasible frontier solution found for mode %s.', mode);
    end
end