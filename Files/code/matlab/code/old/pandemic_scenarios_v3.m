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


%% FUNCTIONS (copied from calibration)

function xs = forward_roll(fcp, fdi, S, theta, d, mu_y, mu_b, kcp, P)
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
        rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;
        xs(1,k+1) = mu_y + rho_eff*y - P.alpha_S*Sk ...
            + P.alpha_F_DI*z + P.beta_fear*dk + ey;
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
            + kcp*fk + P.kappa_F_DI*gk + P.c_H*thk;
        xs(3,k+1) = fk;
        xs(4,k+1) = gk;
    end
end