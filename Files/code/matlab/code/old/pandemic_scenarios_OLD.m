%% ========================================================================
%  PANDEMIC TRILEMMA — COUNTERFACTUAL SCENARIOS
%
%  Prerequisite: Run pandemic_calibration.m first (validation OK).
%
%  Step 2: Five counterfactual scenarios (pure forward rolls, no optimizer)
%    S1: No CP         (F_CP=0, F_DI=obs)
%    S2: No DI         (F_CP=obs, F_DI=0)
%    S3: No fiscal     (F_CP=0, F_DI=0)
%    S4: CP → DI       (F_CP=0, F_DI=obs+obs_CP, capped at u_hi)
%    S5: DI → CP       (F_DI=0, F_CP=obs+obs_DI, capped at u_hi)
%
%  Step 3: Decomposition of fiscal contributions
%    CP contribution   = y_obs - y_S1
%    DI contribution   = y_obs - y_S2
%    Total fiscal      = y_obs - y_S3
%    Interaction       = S3 - S1 - S2 + obs  (nonlinearity)
%    Output-per-Debt   = contribution / debt cost
%
%  All scenarios condition on observed S, theta, d (fixed).
% =========================================================================
clear; clc; close all;
fprintf('=== PANDEMIC TRILEMMA: Counterfactual Scenarios ===\n');
fprintf('  %s\n\n', datestr(now));


%% ========================================================================
%  PARAMETERS (identical to calibration file)
% =========================================================================
rho_y=0.372; psi=0.200; alpha_S=0.016; alpha_F_CP=0.249;
eta_tilde=-0.700; eta_p=2.600; alpha_F_DI=0.224; beta_fear=-0.022;
r_int=0.001; gamma_y=0.219; kappa_F_DI=0.379; c_H=0.02;

cfe_iso = {'AUS','AUT','BEL','CAN','CHE','CHL','COL','CRI', ...
           'CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR', ...
           'GRC','HUN','IRL','ISL','ISR','ITA','JPN','KOR', ...
           'LTU','LUX','LVA','MEX','NLD','NOR','NZL','POL', ...
           'PRT','SVK','SVN','SWE','TUR','USA'};
kappa_cp_i = [0.3908,0.3548,0.2079,0.2952,0.2425,0.1903,0.2290,0.2723, ...
              0.1757,0.2085,0.3273,0.1973,0.2982,0.2182,0.2081,0.2412, ...
              0.3514,0.2611,0.3521,0.3465,0.3243,0.1970,0.2602,0.2864, ...
              0.3250,0.2506,0.2764,0.3195,0.2994,0.2328,0.3647,0.3157, ...
              0.2030,0.3968,0.3290,0.2735,0.1727,0.3257];
qfe_pp = [-1.6945,-9.2225,1.1002,-0.8301,-0.1618, ...
            0.2676,0.3364,0.8448,-0.2020,-0.3219];
cfe_y_val = [0.1530,-1.3482,0.1786,-0.9951,0.7648,0.2349,0.9292,-0.5516, ...
            -2.7814,-1.2624,0.3376,-3.9030,-1.2179,-1.1787,-1.3129,-2.4354, ...
            -0.5643,-1.7510,5.1692,-3.8459,0.5602,-0.0380,-1.1328,0.0174, ...
             0.2433,0.9685,-0.6834,-2.4163,0.1424,0.4993,-0.9626,-0.8523, ...
            -2.4815,-0.4481,-1.5356,0.3285,2.8974,0.3696];
cfe_b_val = [-0.6746,-0.2777,-0.4649,-0.2143,-0.4844,-0.9781,0.1462,0.4048, ...
             -1.0619,-1.0287,-1.2539,-1.2272,-0.5830,-0.5348,-0.8409,-0.8974, ...
             -0.6096,-0.6704,0.9444,-1.0335,0.2331,-0.6479,-0.2022,-0.2991, ...
             -0.5080,0.2937,-0.0212,-1.6298,-0.4222,-0.4402,0.3454,-0.6348, ...
             -1.4319,-0.0641,-0.4026,-0.3780,0.3130,0.6681];

N=10; K_act=8; nx=3; nu=2;
eps_y_vec=zeros(1,N+1);
for k=1:N, if k<=length(qfe_pp), eps_y_vec(k+1)=qfe_pp(k)/100; end; end

cfe_y_map = containers.Map(cfe_iso, cfe_y_val/100);
cfe_b_map = containers.Map(cfe_iso, cfe_b_val/100);
kappa_map = containers.Map(cfe_iso, kappa_cp_i);
u_hi = [0.20; 0.10];

P = struct('rho_y',rho_y,'psi',psi,'alpha_S',alpha_S, ...
    'alpha_F_CP',alpha_F_CP,'eta_tilde',eta_tilde,'eta_p',eta_p, ...
    'alpha_F_DI',alpha_F_DI,'beta_fear',beta_fear, ...
    'r_int',r_int,'gamma_y',gamma_y,'kappa_F_DI',kappa_F_DI,'c_H',c_H, ...
    'eps_y_vec',eps_y_vec,'N',N,'K_act',K_act,'nx',nx,'nu',nu);


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
    if isKey(kappa_map,iso),  cdata(i).kappa_cp=kappa_map(iso); end
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
fprintf('  %d countries x %d quarters\n\n', n_c, N);


%% ========================================================================
%  STEP 2: COUNTERFACTUAL SCENARIOS
% =========================================================================
fprintf('========================================\n');
fprintf('  STEP 2: Counterfactual Scenarios\n');
fprintf('========================================\n\n');

scen_names = {'Observed','No CP','No DI','No Fiscal','CP to DI','DI to CP'};
n_scen = length(scen_names);

for i = 1:n_c
    S_i  = cdata(i).S;    th_i = cdata(i).theta;  d_i = cdata(i).d;
    my_i = cdata(i).mu_y; mb_i = cdata(i).mu_b;   kc_i = cdata(i).kappa_cp;
    fcp  = cdata(i).FCP;  fdi  = cdata(i).FDI;

    % --- S0: Observed (baseline) ---
    xs0 = forward_roll(fcp, fdi, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- S1: No CP ---
    fcp_s1 = zeros(1,N);
    fdi_s1 = fdi;
    xs1 = forward_roll(fcp_s1, fdi_s1, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- S2: No DI ---
    fcp_s2 = fcp;
    fdi_s2 = zeros(1,N);
    xs2 = forward_roll(fcp_s2, fdi_s2, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- S3: No Fiscal ---
    xs3 = forward_roll(zeros(1,N), zeros(1,N), S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- S4: CP → DI (budget-neutral reallocation) ---
    fcp_s4 = zeros(1,N);
    fdi_s4 = fdi;
    for k = 1:K_act
        fdi_s4(k) = fdi(k) + fcp(k);        % add CP budget to DI
        fdi_s4(k) = min(fdi_s4(k), u_hi(2)); % cap at DI ceiling
    end
    xs4 = forward_roll(fcp_s4, fdi_s4, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- S5: DI → CP (budget-neutral reallocation) ---
    fdi_s5 = zeros(1,N);
    fcp_s5 = fcp;
    for k = 1:K_act
        fcp_s5(k) = fcp(k) + fdi(k);        % add DI budget to CP
        fcp_s5(k) = min(fcp_s5(k), u_hi(1)); % cap at CP ceiling
    end
    xs5 = forward_roll(fcp_s5, fdi_s5, S_i, th_i, d_i, my_i, mb_i, kc_i, P);

    % --- Store trajectories ---
    cdata(i).xs = {xs0, xs1, xs2, xs3, xs4, xs5};

    % --- Terminal outcomes (pp) ---
    for s = 1:n_scen
        cdata(i).y_end(s) = cdata(i).xs{s}(1, N+1) * 100;
        cdata(i).b_end(s) = cdata(i).xs{s}(2, N+1) * 100;
    end

    % --- Cumulative output (sum over K_act quarters, pp) ---
    for s = 1:n_scen
        cdata(i).y_cum(s) = sum(cdata(i).xs{s}(1, 2:K_act+1)) * 100;
        cdata(i).b_cum(s) = cdata(i).xs{s}(2, K_act+1) * 100;
    end

    % Fiscal totals for reference
    cdata(i).F_total_CP = sum(fcp(1:K_act)) * 100;
    cdata(i).F_total_DI = sum(fdi(1:K_act)) * 100;
    cdata(i).F_total    = cdata(i).F_total_CP + cdata(i).F_total_DI;
    cdata(i).cp_share   = cdata(i).F_total_CP / max(cdata(i).F_total, 1e-10) * 100;

    % Budget spilled (what didn't fit under cap in S4/S5)
    cdata(i).spill_s4 = (sum(fcp(1:K_act)+fdi(1:K_act)) - sum(fcp_s4(1:K_act)+fdi_s4(1:K_act))) * 100;
    cdata(i).spill_s5 = (sum(fcp(1:K_act)+fdi(1:K_act)) - sum(fcp_s5(1:K_act)+fdi_s5(1:K_act))) * 100;
end

% --- Print per-country results ---
fprintf('  %5s %6s %6s | %7s %7s %7s %7s %7s | %7s %7s %7s %7s %7s\n', ...
    'ISO', 'F_tot', 'CP%', ...
    'dy_S1', 'dy_S2', 'dy_S3', 'dy_S4', 'dy_S5', ...
    'db_S1', 'db_S2', 'db_S3', 'db_S4', 'db_S5');
fprintf('  %s\n', repmat('-', 1, 100));

for i = 1:n_c
    dy = cdata(i).y_cum - cdata(i).y_cum(1);  % relative to observed
    db = cdata(i).b_cum - cdata(i).b_cum(1);
    fprintf('  %5s %5.1f%% %5.0f%% | %+7.2f %+7.2f %+7.2f %+7.2f %+7.2f | %+7.2f %+7.2f %+7.2f %+7.2f %+7.2f\n', ...
        cdata(i).iso, cdata(i).F_total, cdata(i).cp_share, ...
        dy(2), dy(3), dy(4), dy(5), dy(6), ...
        db(2), db(3), db(4), db(5), db(6));
end


%% ========================================================================
%  STEP 3: DECOMPOSITION
% =========================================================================
fprintf('\n========================================\n');
fprintf('  STEP 3: Fiscal Contribution Decomposition\n');
fprintf('========================================\n\n');

for i = 1:n_c
    % Cumulative output (pp, sum over K_act)
    y0 = cdata(i).y_cum(1);   % observed
    y1 = cdata(i).y_cum(2);   % no CP
    y2 = cdata(i).y_cum(3);   % no DI
    y3 = cdata(i).y_cum(4);   % no fiscal

    % Cumulative debt at K_act (pp)
    b0 = cdata(i).b_cum(1);
    b1 = cdata(i).b_cum(2);
    b2 = cdata(i).b_cum(3);
    b3 = cdata(i).b_cum(4);

    % --- Output contributions ---
    cdata(i).cp_contrib_y  = y0 - y1;          % what CP added
    cdata(i).di_contrib_y  = y0 - y2;          % what DI added
    cdata(i).tot_contrib_y = y0 - y3;          % total fiscal effect
    cdata(i).interaction_y = y3 - y1 - y2 + y0; % nonlinear interaction

    % --- Debt costs ---
    cdata(i).cp_cost_b  = b0 - b1;             % debt from CP
    cdata(i).di_cost_b  = b0 - b2;             % debt from DI
    cdata(i).tot_cost_b = b0 - b3;             % total debt from fiscal

    % --- Efficiency: output per debt (pp output / pp debt) ---
    cdata(i).cp_eff = cdata(i).cp_contrib_y / max(abs(cdata(i).cp_cost_b), 0.01);
    cdata(i).di_eff = cdata(i).di_contrib_y / max(abs(cdata(i).di_cost_b), 0.01);

    % --- Share of total ---
    tot = max(abs(cdata(i).tot_contrib_y), 0.01);
    cdata(i).cp_share_y = cdata(i).cp_contrib_y / tot * 100;
    cdata(i).di_share_y = cdata(i).di_contrib_y / tot * 100;
end

% --- Print decomposition ---
fprintf('  %5s | %8s %8s %8s %8s | %8s %8s %8s | %6s %6s\n', ...
    'ISO', 'CP_y', 'DI_y', 'Inter', 'Total_y', ...
    'CP_b', 'DI_b', 'Total_b', 'CP_eff', 'DI_eff');
fprintf('  %s\n', repmat('-', 1, 95));

for i = 1:n_c
    fprintf('  %5s | %+8.2f %+8.2f %+8.2f %+8.2f | %+8.2f %+8.2f %+8.2f | %6.2f %6.2f\n', ...
        cdata(i).iso, ...
        cdata(i).cp_contrib_y, cdata(i).di_contrib_y, ...
        cdata(i).interaction_y, cdata(i).tot_contrib_y, ...
        cdata(i).cp_cost_b, cdata(i).di_cost_b, cdata(i).tot_cost_b, ...
        cdata(i).cp_eff, cdata(i).di_eff);
end

% --- OECD Summary ---
fprintf('\n  OECD Summary (mean across 38 countries):\n');
fprintf('  %30s %8s %8s\n', '', 'CP', 'DI');
fprintf('  %30s %+8.2f %+8.2f\n', 'Output contribution (pp)', ...
    mean([cdata.cp_contrib_y]), mean([cdata.di_contrib_y]));
fprintf('  %30s %+8.2f %+8.2f\n', 'Debt cost (pp)', ...
    mean([cdata.cp_cost_b]), mean([cdata.di_cost_b]));
fprintf('  %30s %8.2f %8.2f\n', 'Output per Debt', ...
    mean([cdata.cp_eff]), mean([cdata.di_eff]));
fprintf('  %30s %+8.2f\n', 'Interaction term (pp)', ...
    mean([cdata.interaction_y]));
fprintf('  %30s %+8.2f\n', 'Total fiscal effect (pp)', ...
    mean([cdata.tot_contrib_y]));

% Share
fprintf('\n  Share of total output effect:\n');
fprintf('    CP: %.0f%%   DI: %.0f%%   Interaction: %.0f%%\n', ...
    mean([cdata.cp_share_y]), mean([cdata.di_share_y]), ...
    100 - mean([cdata.cp_share_y]) - mean([cdata.di_share_y]));

% S4/S5 spillage
fprintf('\n  Budget spillage (cap binding):\n');
fprintf('    S4 (CP->DI): %.0f/%d countries lost budget (DI cap)\n', ...
    sum([cdata.spill_s4] > 0.01), n_c);
fprintf('    S5 (DI->CP): %.0f/%d countries lost budget (CP cap)\n', ...
    sum([cdata.spill_s5] > 0.01), n_c);


%% ========================================================================
%  STEP 3b: SCENARIO COMPARISON TABLE (S4 vs S5)
% =========================================================================
fprintf('\n========================================\n');
fprintf('  Reallocation Scenarios: CP->DI vs DI->CP\n');
fprintf('========================================\n');

fprintf('\n  %5s | %8s %8s %8s | %8s %8s %8s | %6s\n', ...
    'ISO', 'dy_S4', 'db_S4', 'better?', 'dy_S5', 'db_S5', 'better?', 'winner');
fprintf('  %s\n', repmat('-', 1, 75));

n_s4_better = 0;  n_s5_better = 0;
for i = 1:n_c
    dy4 = cdata(i).y_cum(5) - cdata(i).y_cum(1);
    db4 = cdata(i).b_cum(5) - cdata(i).b_cum(1);
    dy5 = cdata(i).y_cum(6) - cdata(i).y_cum(1);
    db5 = cdata(i).b_cum(6) - cdata(i).b_cum(1);

    % Simple welfare proxy (higher y, lower b = better)
    better4 = (dy4 > 0 && db4 < 0);
    better5 = (dy5 > 0 && db5 < 0);
    if dy4 - db4 > dy5 - db5
        winner = 'CP->DI'; n_s4_better = n_s4_better + 1;
    else
        winner = 'DI->CP'; n_s5_better = n_s5_better + 1;
    end

    fprintf('  %5s | %+8.2f %+8.2f %8s | %+8.2f %+8.2f %8s | %s\n', ...
        cdata(i).iso, dy4, db4, iff(better4,'Pareto',''), ...
        dy5, db5, iff(better5,'Pareto',''), winner);
end
fprintf('\n  CP->DI wins: %d/%d   DI->CP wins: %d/%d\n', ...
    n_s4_better, n_c, n_s5_better, n_c);


%% ========================================================================
%  EXPORT RESULTS
% =========================================================================
T_out = table({cdata.iso}', ...
    [cdata.F_total]', [cdata.cp_share]', ...
    [cdata.cp_contrib_y]', [cdata.di_contrib_y]', ...
    [cdata.interaction_y]', [cdata.tot_contrib_y]', ...
    [cdata.cp_cost_b]', [cdata.di_cost_b]', [cdata.tot_cost_b]', ...
    [cdata.cp_eff]', [cdata.di_eff]', ...
    'VariableNames', {'Country', 'F_total_pct', 'CP_share_pct', ...
        'CP_output_pp', 'DI_output_pp', 'Interaction_pp', 'Total_output_pp', ...
        'CP_debt_pp', 'DI_debt_pp', 'Total_debt_pp', ...
        'CP_efficiency', 'DI_efficiency'});
writetable(T_out, 'scenario_results.csv');
fprintf('\n  Saved: scenario_results.csv\n');


%% ========================================================================
%  VISUALIZATION
% =========================================================================

% --- Fig 1: OECD Median Trajectories under all scenarios ---
figure('Name','Scenarios','Color','w','Position',[50 50 1100 450]);

for panel = 1:2
    subplot(1,2,panel); hold on;
    colors = {[0 0 0], [.8 .2 .2], [.2 .2 .8], [.5 .5 .5], [.8 .5 .2], [.2 .7 .2]};
    styles = {'-', '--', '--', ':', '-', '-'};
    for s = 1:n_scen
        traj = zeros(n_c, N);
        for i = 1:n_c
            if panel == 1
                traj(i,:) = cdata(i).xs{s}(1, 2:N+1) * 100;
            else
                traj(i,:) = cdata(i).xs{s}(2, 2:N+1) * 100;
            end
        end
        plot(1:N, median(traj), styles{s}, 'Color', colors{s}, ...
            'LineWidth', 1.8, 'MarkerSize', 3);
    end
    xline(K_act+0.5, ':', 'Color', [.5 .5 .5]);
    if panel == 1, yline(0, ':', 'Color', [.5 .5 .5]); end
    grid on;
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 7, ...
        'XTickLabelRotation', 45);
    if panel == 1
        ylabel('Output Gap (pp)');  title('Output');
    else
        ylabel('Cumulative Debt (pp GDP)');  title('Debt');
    end
    if panel == 1
        legend(scen_names, 'Location', 'SE', 'FontSize', 6);
    end
end
sgtitle('Counterfactual Scenarios: OECD Median', 'FontWeight', 'bold');

% --- Fig 2: Fiscal Contribution Decomposition (stacked bar) ---
figure('Name','Decomposition','Color','w','Position',[50 50 1200 400]);
[~, si] = sort([cdata.tot_contrib_y], 'descend');
bar_data = [[cdata(si).cp_contrib_y]; [cdata(si).di_contrib_y]; ...
            [cdata(si).interaction_y]]';
bh = bar(1:n_c, bar_data, 'stacked');
bh(1).FaceColor = [.2 .5 .8];
bh(2).FaceColor = [.8 .5 .2];
bh(3).FaceColor = [.7 .7 .7];
set(gca, 'XTick', 1:n_c, 'XTickLabel', {cdata(si).iso}, ...
    'XTickLabelRotation', 55, 'FontSize', 6);
ylabel('Cumulative Output Contribution (pp)');
title('Fiscal Contribution Decomposition');
legend('CP', 'DI', 'Interaction', 'Location', 'NE', 'FontSize', 7);
grid on;

% --- Fig 3: Efficiency scatter (output per debt) ---
figure('Name','Efficiency','Color','w','Position',[50 50 600 500]);
hold on;
scatter([cdata.cp_cost_b], [cdata.cp_contrib_y], 50, [.2 .5 .8], 'filled');
scatter([cdata.di_cost_b], [cdata.di_contrib_y], 50, [.8 .5 .2], 'filled');
% Label CP points
for i = 1:n_c
    text(cdata(i).cp_cost_b+0.05, cdata(i).cp_contrib_y+0.02, ...
        cdata(i).iso, 'FontSize', 5, 'Color', [.2 .5 .8]);
end
xlabel('Debt Cost (pp GDP)');  ylabel('Output Contribution (pp)');
title('Fiscal Efficiency: Output per Debt');
legend('CP', 'DI', 'Location', 'NW');
xline(0, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]);
grid on;

% --- Fig 4: Selected countries — scenario trajectories ---
selected = {'USA','DEU','ITA','GBR','JPN','CHL'};
n_sel = length(selected);
figure('Name','Country Scenarios','Color','w','Position',[30 30 1200 700]);
for s_idx = 1:n_sel
    iso = selected{s_idx};
    idx = find(strcmp({cdata.iso}, iso));

    % Output panel
    subplot(n_sel, 2, (s_idx-1)*2+1); hold on;
    for s = 1:n_scen
        plot(1:N, cdata(idx).xs{s}(1, 2:N+1)*100, styles{s}, ...
            'Color', colors{s}, 'LineWidth', 1.5);
    end
    xline(K_act+0.5, ':', 'Color', [.5 .5 .5]);
    yline(0, ':', 'Color', [.5 .5 .5]);
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 5); grid on;
    ylabel('y (pp)');
    text(0.02, 0.95, iso, 'Units', 'normalized', 'FontSize', 10, ...
        'FontWeight', 'bold', 'VerticalAlignment', 'top');
    if s_idx == 1
        title('Output Gap');
        legend(scen_names, 'FontSize', 4, 'Location', 'SE');
    end

    % Debt panel
    subplot(n_sel, 2, (s_idx-1)*2+2); hold on;
    for s = 1:n_scen
        plot(1:N, cdata(idx).xs{s}(2, 2:N+1)*100, styles{s}, ...
            'Color', colors{s}, 'LineWidth', 1.5);
    end
    xline(K_act+0.5, ':', 'Color', [.5 .5 .5]);
    set(gca, 'XTick', 1:N, 'XTickLabel', qlbl, 'FontSize', 5); grid on;
    ylabel('b (pp)');
    if s_idx == 1, title('Cumulative Debt'); end
end
sgtitle('Country Scenarios', 'FontWeight', 'bold');

fprintf('\n=== SCENARIOS COMPLETE ===\n');


%% ########################################################################
%  FUNCTIONS
%  ########################################################################

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

function out = iff(cond, a, b)
    if cond, out = a; else, out = b; end
end
