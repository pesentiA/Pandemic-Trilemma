%% ========================================================================
%  PANDEMIC TRILEMMA — SCENARIO ANALYSIS (V4)
%
%  Requires: pandemic_calibration_v4.m to have been run first.
%  Uses the calibrated P struct and cdata array.
%
%  SCENARIOS (S fixed at observed, fiscal composition varied):
%    S0: Observed fiscal deployment (baseline)
%    S1: No CP  (F^CP_all = 0, F^DI = observed)
%    S2: No DI  (F^CP = observed, F^DI = 0)
%    S3: No Fiscal (F^CP_all = 0, F^DI = 0)
%    S4: CP -> DI  (all fiscal as DI, no CP)
%    S5: DI -> CP  (all fiscal as CP, preserving country above/below composition)
%
%  REALLOCATION LOGIC for S5 (DI -> CP):
%    Per country, compute the cumulative above/below split from observed
%    deployment. Apply this constant ratio to distribute reallocated DI
%    across the three CP sub-categories (above, loans, guarantees).
%    Below-the-line is further split 50/50 into loans/guarantees (same
%    working assumption as in the calibration).
%
%  HEALTH SPENDING (F^H):
%    Held constant at observed values across all scenarios. Health
%    spending is exogenous to the CP-DI composition decision.
%
%  OUTPUTS:
%    - OECD median trajectories under each scenario
%    - Country-level cumulative output gain and debt cost
%    - Fiscal decomposition (CP contribution, DI contribution, interaction)
%    - Reallocation outcomes: CP->DI vs DI->CP winners
%    - Visualizations: trajectory plot, persistence plot, country bars, Pareto
% =========================================================================

if ~exist('P', 'var') || ~exist('cdata', 'var')
    error(['Run pandemic_calibration_v4.m first to set up P and cdata. ' ...
           'This file uses the calibrated parameters from that script.']);
end

% Define quarter labels if not already in workspace
if ~exist('qlbl', 'var')
    qlbl = {'Q1.20','Q2.20','Q3.20','Q4.20', ...
            'Q1.21','Q2.21','Q3.21','Q4.21', ...
            'Q1.22','Q2.22'};
end

fprintf('\n\n');
fprintf('========================================\n');
fprintf('  SCENARIO ANALYSIS (V4)\n');
fprintf('========================================\n\n');

N_ = P.N;  Ka = P.K_act;  n_c = length(cdata);


%% ========================================================================
%  STEP 1: COMPUTE ALL SCENARIOS PER COUNTRY
% =========================================================================

scen = struct();
for i = 1:n_c
    iso   = cdata(i).iso;
    S_i   = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i  = cdata(i).mu_y;   mb_i = cdata(i).mu_b;
    fcp     = cdata(i).FCP;          % aggregate (for persistence channel)
    fcp_a   = cdata(i).FCP_above;
    fcp_l   = cdata(i).FCP_loans;
    fcp_g   = cdata(i).FCP_guar;
    fdi     = cdata(i).FDI;
    fh      = cdata(i).FH;

    % Country-specific cumulative above/below composition (for S5 reallocation)
    tot_above = sum(fcp_a(1:Ka));
    tot_below = sum(fcp_l(1:Ka)) + sum(fcp_g(1:Ka));
    tot_cp    = tot_above + tot_below;
    if tot_cp > 0.01
        frac_above_i = tot_above / tot_cp;
        frac_below_i = tot_below / tot_cp;
    else
        % Fallback to OECD-average composition for countries with no CP
        frac_above_i = 0.55;  % rough OECD mean (Panel B)
        frac_below_i = 0.45;
    end

    % ----- S0: Observed -----
    xs0 = forward_roll(fcp, fcp_a, fcp_l, fcp_g, fdi, fh, ...
                       S_i, th_i, d_i, my_i, mb_i, P);

    % ----- S1: No CP -----
    xs1 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       fdi, fh, S_i, th_i, d_i, my_i, mb_i, P);

    % ----- S2: No DI -----
    xs2 = forward_roll(fcp, fcp_a, fcp_l, fcp_g, zeros(1,N_), fh, ...
                       S_i, th_i, d_i, my_i, mb_i, P);

    % ----- S3: No Fiscal -----
    xs3 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       zeros(1,N_), fh, S_i, th_i, d_i, my_i, mb_i, P);

    % ----- S4: CP -> DI (all fiscal as DI) -----
    fdi_s4 = fcp + fdi;   % aggregate DI = CP_total + DI_observed (element-wise)
    xs4 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       fdi_s4, fh, S_i, th_i, d_i, my_i, mb_i, P);

    % ----- S5: DI -> CP (all fiscal as CP, country-proportional) -----
    %  Total fiscal (CP + DI) is redistributed as CP, preserving the
    %  country's cumulative above/below composition. Below is split
    %  50/50 into loans/guarantees (same assumption as calibration).
    fcp_s5_agg = fcp + fdi;   % new aggregate CP per quarter
    fcp_s5_a   = frac_above_i     * fcp_s5_agg;
    fcp_s5_bel = frac_below_i     * fcp_s5_agg;
    fcp_s5_l   = 0.5 * fcp_s5_bel;
    fcp_s5_g   = 0.5 * fcp_s5_bel;
    xs5 = forward_roll(fcp_s5_agg, fcp_s5_a, fcp_s5_l, fcp_s5_g, ...
                       zeros(1,N_), fh, S_i, th_i, d_i, my_i, mb_i, P);

    % --- Store outcomes (all in pp, ready to report) ---
    scen(i).iso = iso;
    scen(i).y0 = xs0(1, 2:end);  scen(i).b0 = xs0(2, 2:end);
    scen(i).y1 = xs1(1, 2:end);  scen(i).b1 = xs1(2, 2:end);
    scen(i).y2 = xs2(1, 2:end);  scen(i).b2 = xs2(2, 2:end);
    scen(i).y3 = xs3(1, 2:end);  scen(i).b3 = xs3(2, 2:end);
    scen(i).y4 = xs4(1, 2:end);  scen(i).b4 = xs4(2, 2:end);
    scen(i).y5 = xs5(1, 2:end);  scen(i).b5 = xs5(2, 2:end);

    % Cumulative output gap (sum over active quarters)
    scen(i).cum0 = sum(scen(i).y0(1:Ka));
    scen(i).cum1 = sum(scen(i).y1(1:Ka));
    scen(i).cum2 = sum(scen(i).y2(1:Ka));
    scen(i).cum3 = sum(scen(i).y3(1:Ka));
    scen(i).cum4 = sum(scen(i).y4(1:Ka));
    scen(i).cum5 = sum(scen(i).y5(1:Ka));

    % Terminal cumulative debt
    scen(i).bN0 = xs0(2, N_+1);
    scen(i).bN1 = xs1(2, N_+1);
    scen(i).bN2 = xs2(2, N_+1);
    scen(i).bN3 = xs3(2, N_+1);
    scen(i).bN4 = xs4(2, N_+1);
    scen(i).bN5 = xs5(2, N_+1);

    % Decomposition (Shapley-like, not exact due to non-linearity)
    scen(i).cp_eff   = scen(i).cum0 - scen(i).cum1;   % S0 minus No-CP
    scen(i).di_eff   = scen(i).cum0 - scen(i).cum2;   % S0 minus No-DI
    scen(i).tot_eff  = scen(i).cum0 - scen(i).cum3;   % S0 minus No-Fiscal
    scen(i).interact = scen(i).tot_eff - scen(i).cp_eff - scen(i).di_eff;

    % Fiscal composition (for reporting)
    scen(i).frac_above_i = frac_above_i;
    scen(i).frac_below_i = frac_below_i;
    scen(i).tot_cp = tot_cp;
    scen(i).tot_di = sum(fdi(1:Ka));
end

fprintf('  Computed %d countries x 6 scenarios\n\n', n_c);


%% ========================================================================
%  STEP 2: OECD MEDIAN TRAJECTORIES
% =========================================================================
fprintf('  OECD MEDIAN TRAJECTORIES (pp):\n');
fprintf('  %8s %7s %7s %7s %7s %7s %7s %7s\n', ...
    'Quarter', 'Obs', 'S0', 'S1', 'S2', 'S3', 'S4', 'S5');

for k = 1:Ka
    obs_k  = arrayfun(@(c) c.y(k),  cdata);
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
%  STEP 3: CUMULATIVE OUTPUT & FISCAL DECOMPOSITION
% =========================================================================
fprintf('\n  CUMULATIVE OUTPUT (sum %dQ, pp of GDP):\n', Ka);
fprintf('  %22s %9s %9s %9s\n', 'Scenario', 'Mean', 'Median', 'vs S0');

cum_names  = {'S0: Observed','S1: No CP','S2: No DI','S3: No Fiscal', ...
              'S4: CP->DI','S5: DI->CP (prop)'};
cum_fields = {'cum0','cum1','cum2','cum3','cum4','cum5'};
for j = 1:6
    vals  = arrayfun(@(s) s.(cum_fields{j}), scen);
    diffs = vals - [scen.cum0];
    fprintf('  %22s %+9.2f %+9.2f %+9.2f\n', ...
        cum_names{j}, mean(vals), median(vals), mean(diffs));
end

fprintf('\n  FISCAL DECOMPOSITION (cum %dQ, pp):\n', Ka);
fprintf('  %8s %9s %9s %10s %10s\n', '', 'CP contr', 'DI contr', 'Interact', 'Total F');
fprintf('  %8s %+9.2f %+9.2f %+10.2f %+10.2f\n', 'Mean', ...
    mean([scen.cp_eff]), mean([scen.di_eff]), ...
    mean([scen.interact]), mean([scen.tot_eff]));
fprintf('  %8s %+9.2f %+9.2f %+10.2f %+10.2f\n', 'Median', ...
    median([scen.cp_eff]), median([scen.di_eff]), ...
    median([scen.interact]), median([scen.tot_eff]));


%% ========================================================================
%  STEP 4: TERMINAL DEBT COMPARISON
% =========================================================================
fprintf('\n  TERMINAL DEBT (pp of 2019 GDP):\n');
fprintf('  %22s %9s %9s %9s\n', 'Scenario', 'Mean', 'Median', 'vs S0');

dbt_fields = {'bN0','bN1','bN2','bN3','bN4','bN5'};
for j = 1:6
    vals  = arrayfun(@(s) s.(dbt_fields{j}), scen);
    diffs = vals - [scen.bN0];
    fprintf('  %22s %+9.2f %+9.2f %+9.2f\n', ...
        cum_names{j}, mean(vals), median(vals), mean(diffs));
end


%% ========================================================================
%  STEP 5: REALLOCATION — S4 (CP->DI) vs S5 (DI->CP)
%  This is the central counterfactual: which composition is better?
% =========================================================================
fprintf('\n\n  REALLOCATION: S4 (CP->DI) vs S5 (DI->CP):\n');
fprintf('  Positive value = better output outcome (less negative cum output gap)\n');
fprintf('  %5s %9s %9s %9s %9s %9s %10s\n', ...
    'ISO', 'cum0', 'cum4', 'cum5', 'dy4-0', 'dy5-0', 'Winner');

s4_wins = 0;  s5_wins = 0;
for i = 1:n_c
    dy4 = scen(i).cum4 - scen(i).cum0;  % positive = S4 better
    dy5 = scen(i).cum5 - scen(i).cum0;  % positive = S5 better
    if scen(i).cum5 > scen(i).cum4
        winner = 'DI->CP'; s5_wins = s5_wins + 1;
    else
        winner = 'CP->DI'; s4_wins = s4_wins + 1;
    end
    fprintf('  %5s %+9.2f %+9.2f %+9.2f %+9.2f %+9.2f %10s\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum4, scen(i).cum5, ...
        dy4, dy5, winner);
end
fprintf('\n  CP->DI wins: %d / %d\n', s4_wins, n_c);
fprintf('  DI->CP wins: %d / %d\n', s5_wins, n_c);


%% ========================================================================
%  STEP 6: EFFECTIVE PERSISTENCE OVER TIME
%  Illustrates the CP preservation channel: rho_eff(with CP) < rho_eff(no CP)
% =========================================================================
fprintf('\n\n  EFFECTIVE PERSISTENCE (OECD median):\n');
fprintf('  %8s %10s %10s %10s\n', 'Quarter', 'S0 (obs)', 'S1 (noCP)', 'CP gain');

for k = 1:Ka
    rho_s0 = zeros(n_c, 1);  rho_s1 = zeros(n_c, 1);
    for i = 1:n_c
        Sk = cdata(i).S(k);
        % w_k = F^CP_{k-1} (aggregate)
        wk = 0; if k >= 2, wk = cdata(i).FCP(k-1); end
        rho_s0(i) = P.rho_y + P.psi*Sk + P.eta_p*wk;
        rho_s1(i) = P.rho_y + P.psi*Sk;   % w=0 in No-CP
    end
    gain = median(rho_s0) - median(rho_s1);
    fprintf('  %8s %10.3f %10.3f %+10.3f\n', ...
        qlbl{k}, median(rho_s0), median(rho_s1), gain);
end


%% ========================================================================
%  STEP 7: COUNTRY-LEVEL CP CONTRIBUTION
% =========================================================================
fprintf('\n\n  COUNTRY-LEVEL CP CONTRIBUTION (cum %dQ, pp):\n', Ka);
fprintf('  %5s %9s %9s %9s %9s %10s\n', ...
    'ISO', 'y_obs', 'y_noCP', 'CP_eff', 'F^CP_tot', 'bN_obs');

for i = 1:n_c
    fprintf('  %5s %+9.2f %+9.2f %+9.2f %9.2f %+10.2f\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum1, scen(i).cp_eff, ...
        scen(i).tot_cp, scen(i).bN0);
end
fprintf('\n  CP helps: %d / %d\n', sum([scen.cp_eff] > 0), n_c);
fprintf('  Mean CP effect:  %+.2f pp\n', mean([scen.cp_eff]));
fprintf('  Mean DI effect:  %+.2f pp\n', mean([scen.di_eff]));


%% ========================================================================
%  STEP 8: OUTPUT-DEBT TRADE-OFF (not Pareto, this is a trade-off analysis)
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  OUTPUT-DEBT TRADE-OFF (S0 vs S3)\n');
fprintf('========================================\n');

fprintf('\n  %5s %10s %10s %10s %10s %12s\n', ...
    'ISO', 'y_obs', 'y_noF', 'dy', 'db', 'Output/Debt');

n_improved = 0;
ratios = zeros(n_c, 1);
for i = 1:n_c
    dy = scen(i).cum0 - scen(i).cum3;   % positive = fiscal improved output
    db = scen(i).bN0  - scen(i).bN3;    % positive = fiscal raised debt
    if dy > 0, n_improved = n_improved + 1; end
    if abs(db) > 0.01
        ratios(i) = dy / db;
    end
    fprintf('  %5s %+10.2f %+10.2f %+10.2f %+10.2f %12.3f\n', ...
        scen(i).iso, scen(i).cum0, scen(i).cum3, dy, db, ratios(i));
end
fprintf('\n  Fiscal improved output: %d / %d\n', n_improved, n_c);
fprintf('  Mean output gain: %+.2f pp\n', mean([scen.tot_eff]));
fprintf('  Mean debt cost:   %+.2f pp\n', mean([scen.bN0] - [scen.bN3]));
if mean([scen.bN0] - [scen.bN3]) > 0.01
    fprintf('  Output/Debt ratio (mean): %.3f pp output per pp debt\n', ...
        mean([scen.tot_eff]) / mean([scen.bN0] - [scen.bN3]));
end


%% ========================================================================
%  STEP 8b: SENSITIVITY — alpha_DI = 0
%  DI coefficient is not statistically significant (p = 0.138) in the
%  paper's main specification. This block recomputes ALL scenarios under
%  the conservative assumption that DI has zero level effect. The CP
%  effectiveness remains unchanged; what changes is the counterfactual
%  value of reallocating CP into DI.
%
%  Interpretation: the baseline results take alpha_DI at face value;
%  this block gives the lower bound on DI's contribution. The truth
%  likely lies between the two, but the qualitative conclusion about
%  reallocation ordering is sensitive to this assumption.
% =========================================================================
fprintf('\n\n========================================\n');
fprintf('  STEP 8b: SENSITIVITY (alpha_DI = 0)\n');
fprintf('========================================\n');

% Build parameter struct with alpha_DI = 0
P_sens = P;
P_sens.alpha_F_DI = 0;

scen_s = struct();
for i = 1:n_c
    iso   = cdata(i).iso;
    S_i   = cdata(i).S;      th_i = cdata(i).theta;   d_i = cdata(i).d;
    my_i  = cdata(i).mu_y;   mb_i = cdata(i).mu_b;
    fcp     = cdata(i).FCP;
    fcp_a   = cdata(i).FCP_above;
    fcp_l   = cdata(i).FCP_loans;
    fcp_g   = cdata(i).FCP_guar;
    fdi     = cdata(i).FDI;
    fh      = cdata(i).FH;

    frac_above_i = scen(i).frac_above_i;
    frac_below_i = scen(i).frac_below_i;

    % ----- S0: Observed -----
    xs0 = forward_roll(fcp, fcp_a, fcp_l, fcp_g, fdi, fh, ...
                       S_i, th_i, d_i, my_i, mb_i, P_sens);
    % ----- S1: No CP -----
    xs1 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       fdi, fh, S_i, th_i, d_i, my_i, mb_i, P_sens);
    % ----- S2: No DI -----
    xs2 = forward_roll(fcp, fcp_a, fcp_l, fcp_g, zeros(1,N_), fh, ...
                       S_i, th_i, d_i, my_i, mb_i, P_sens);
    % ----- S3: No Fiscal -----
    xs3 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       zeros(1,N_), fh, S_i, th_i, d_i, my_i, mb_i, P_sens);
    % ----- S4: CP -> DI -----
    fdi_s4 = fcp + fdi;
    xs4 = forward_roll(zeros(1,N_), zeros(1,N_), zeros(1,N_), zeros(1,N_), ...
                       fdi_s4, fh, S_i, th_i, d_i, my_i, mb_i, P_sens);
    % ----- S5: DI -> CP -----
    fcp_s5_agg = fcp + fdi;
    fcp_s5_a   = frac_above_i     * fcp_s5_agg;
    fcp_s5_bel = frac_below_i     * fcp_s5_agg;
    fcp_s5_l   = 0.5 * fcp_s5_bel;
    fcp_s5_g   = 0.5 * fcp_s5_bel;
    xs5 = forward_roll(fcp_s5_agg, fcp_s5_a, fcp_s5_l, fcp_s5_g, ...
                       zeros(1,N_), fh, S_i, th_i, d_i, my_i, mb_i, P_sens);

    scen_s(i).iso = iso;
    scen_s(i).y0 = xs0(1, 2:end);   scen_s(i).bN0 = xs0(2, N_+1);
    scen_s(i).y3 = xs3(1, 2:end);   scen_s(i).bN3 = xs3(2, N_+1);
    scen_s(i).y4 = xs4(1, 2:end);   scen_s(i).bN4 = xs4(2, N_+1);
    scen_s(i).y5 = xs5(1, 2:end);   scen_s(i).bN5 = xs5(2, N_+1);
    scen_s(i).cum0 = sum(xs0(1, 2:Ka+1));
    scen_s(i).cum1 = sum(xs1(1, 2:Ka+1));
    scen_s(i).cum2 = sum(xs2(1, 2:Ka+1));
    scen_s(i).cum3 = sum(xs3(1, 2:Ka+1));
    scen_s(i).cum4 = sum(xs4(1, 2:Ka+1));
    scen_s(i).cum5 = sum(xs5(1, 2:Ka+1));
    scen_s(i).cp_eff  = scen_s(i).cum0 - scen_s(i).cum1;
    scen_s(i).di_eff  = scen_s(i).cum0 - scen_s(i).cum2;
    scen_s(i).tot_eff = scen_s(i).cum0 - scen_s(i).cum3;
end

% --- Comparison: baseline vs alpha_DI = 0 ---
fprintf('\n  CUMULATIVE OUTPUT (sum %dQ, pp) — side-by-side comparison:\n', Ka);
fprintf('  %22s %10s %10s %10s\n', 'Scenario', 'Baseline', 'alpha_DI=0', 'Delta');

cum_fields = {'cum0','cum1','cum2','cum3','cum4','cum5'};
cum_names  = {'S0: Observed','S1: No CP','S2: No DI','S3: No Fiscal', ...
              'S4: CP->DI','S5: DI->CP (prop)'};
for j = 1:6
    bvals = arrayfun(@(s) s.(cum_fields{j}), scen);
    svals = arrayfun(@(s) s.(cum_fields{j}), scen_s);
    fprintf('  %22s %+10.2f %+10.2f %+10.2f\n', ...
        cum_names{j}, mean(bvals), mean(svals), mean(svals)-mean(bvals));
end

fprintf('\n  FISCAL DECOMPOSITION (mean, pp):\n');
fprintf('  %22s %10s %10s\n', '', 'Baseline', 'alpha_DI=0');
fprintf('  %22s %+10.2f %+10.2f\n', 'CP contribution', ...
    mean([scen.cp_eff]), mean([scen_s.cp_eff]));
fprintf('  %22s %+10.2f %+10.2f\n', 'DI contribution', ...
    mean([scen.di_eff]), mean([scen_s.di_eff]));
fprintf('  %22s %+10.2f %+10.2f\n', 'Total fiscal effect', ...
    mean([scen.tot_eff]), mean([scen_s.tot_eff]));

% --- Reallocation winners under alpha_DI = 0 ---
s4_wins_s = 0;  s5_wins_s = 0;
for i = 1:n_c
    if scen_s(i).cum5 > scen_s(i).cum4
        s5_wins_s = s5_wins_s + 1;
    else
        s4_wins_s = s4_wins_s + 1;
    end
end

fprintf('\n  REALLOCATION WINNERS — comparison:\n');
fprintf('  %22s %10s %10s\n', '', 'Baseline', 'alpha_DI=0');
fprintf('  %22s %5d / %2d %5d / %2d\n', 'CP->DI wins', ...
    s4_wins, n_c, s4_wins_s, n_c);
fprintf('  %22s %5d / %2d %5d / %2d\n', 'DI->CP wins', ...
    s5_wins, n_c, s5_wins_s, n_c);

% --- Country-level winner switches ---
fprintf('\n  Countries where the winner SWITCHES between baseline and alpha_DI=0:\n');
fprintf('  %5s %12s %12s %10s %10s %10s %10s\n', ...
    'ISO', 'Base winner', 'Sens winner', 'cum4 base', 'cum5 base', 'cum4 sens', 'cum5 sens');
n_switch = 0;
for i = 1:n_c
    base_winner = 'CP->DI';
    if scen(i).cum5 > scen(i).cum4, base_winner = 'DI->CP'; end
    sens_winner = 'CP->DI';
    if scen_s(i).cum5 > scen_s(i).cum4, sens_winner = 'DI->CP'; end
    if ~strcmp(base_winner, sens_winner)
        n_switch = n_switch + 1;
        fprintf('  %5s %12s %12s %+10.2f %+10.2f %+10.2f %+10.2f\n', ...
            scen(i).iso, base_winner, sens_winner, ...
            scen(i).cum4,   scen(i).cum5, ...
            scen_s(i).cum4, scen_s(i).cum5);
    end
end
if n_switch == 0
    fprintf('  (no switches — conclusion robust to alpha_DI assumption)\n');
else
    fprintf('\n  %d / %d countries switch reallocation ranking.\n', n_switch, n_c);
end

% --- Terminal debt under sensitivity (debt unchanged — no DI multiplier on debt) ---
%  Note: Debt is unaffected by alpha_DI because the debt equation does not
%  depend on alpha_DI. The only change is in output, which feeds back
%  through the automatic stabilizer -gamma_y*y. So terminal debt differs
%  slightly, but the direct fiscal cost coefficients kappa are unchanged.
fprintf('\n  TERMINAL DEBT (mean, pp):\n');
fprintf('  %22s %10s %10s\n', 'Scenario', 'Baseline', 'alpha_DI=0');
dbt_fields = {'bN0','bN3','bN4','bN5'};
dbt_names  = {'S0: Observed','S3: No Fiscal','S4: CP->DI','S5: DI->CP'};
for j = 1:length(dbt_fields)
    bvals = arrayfun(@(s) s.(dbt_fields{j}), scen);
    svals = arrayfun(@(s) s.(dbt_fields{j}), scen_s);
    fprintf('  %22s %+10.2f %+10.2f\n', dbt_names{j}, mean(bvals), mean(svals));
end

fprintf('\n  INTERPRETATION:\n');
if s4_wins_s == s4_wins && s5_wins_s == s5_wins
    fprintf('    The reallocation ranking is INVARIANT to the alpha_DI assumption.\n');
    fprintf('    The conclusion (CP->DI %d/%d) is robust.\n', s4_wins, n_c);
elseif n_switch >= n_c/2
    fprintf('    The reallocation ranking FLIPS under alpha_DI = 0.\n');
    fprintf('    The baseline conclusion depends critically on the\n');
    fprintf('    insignificant DI coefficient. Report both scenarios\n');
    fprintf('    in the paper as bracketing the plausible range.\n');
else
    fprintf('    The reallocation ranking is PARTIALLY sensitive:\n');
    fprintf('    %d / %d countries switch ranking. Majority conclusion:\n', ...
        n_switch, n_c);
    if s4_wins_s > s5_wins_s
        fprintf('    CP->DI still dominates under alpha_DI = 0 (%d/%d).\n', s4_wins_s, n_c);
    else
        fprintf('    DI->CP dominates under alpha_DI = 0 (%d/%d).\n', s5_wins_s, n_c);
    end
end


%% ========================================================================
%  STEP 9: VISUALIZATION
% =========================================================================

% --- Fig 1: OECD median trajectory across scenarios ---
figure('Name','V4 Scenarios','Color','w','Position',[50 50 900 500]);
hold on;
cols = lines(6);
snames = {'S0: Observed','S1: No CP','S2: No DI','S3: No Fiscal', ...
          'S4: CP\rightarrowDI','S5: DI\rightarrowCP'};
yfields = {'y0','y1','y2','y3','y4','y5'};
lstyles = {'-','--','--',':','-.','-.'};
for j = 1:6
    med_k = zeros(1, Ka);
    for k = 1:Ka
        med_k(k) = median(arrayfun(@(s) s.(yfields{j})(k), scen));
    end
    plot(1:Ka, med_k, lstyles{j}, 'Color', cols(j,:), 'LineWidth', 1.8);
end
yline(0, ':', 'Color', [.5 .5 .5]);
set(gca, 'XTick', 1:Ka, 'XTickLabel', qlbl(1:Ka), 'FontSize', 8, ...
    'XTickLabelRotation', 45);
ylabel('Output gap (pp of potential GDP)'); grid on;
legend(snames, 'Location', 'SE', 'FontSize', 8);
title('Scenario Analysis: OECD Median Output Gap');

% --- Fig 2: Effective persistence (S0 vs S1) ---
figure('Name','V4 Persistence','Color','w','Position',[50 600 700 400]);
hold on;
rho_s0_med = zeros(1, Ka);  rho_s1_med = zeros(1, Ka);
for k = 1:Ka
    rho_s0_k = zeros(n_c, 1);  rho_s1_k = zeros(n_c, 1);
    for i = 1:n_c
        Sk = cdata(i).S(k);
        wk = 0; if k >= 2, wk = cdata(i).FCP(k-1); end
        rho_s0_k(i) = P.rho_y + P.psi*Sk + P.eta_p*wk;
        rho_s1_k(i) = P.rho_y + P.psi*Sk;
    end
    rho_s0_med(k) = median(rho_s0_k);
    rho_s1_med(k) = median(rho_s1_k);
end
area(1:Ka, rho_s1_med, 'FaceColor', [.9 .7 .7], 'EdgeColor', 'none', ...
    'FaceAlpha', 0.5);
area(1:Ka, rho_s0_med, 'FaceColor', [.7 .8 .9], 'EdgeColor', 'none', ...
    'FaceAlpha', 0.7);
plot(1:Ka, rho_s1_med, 'r--', 'LineWidth', 2);
plot(1:Ka, rho_s0_med, 'b-', 'LineWidth', 2);
yline(P.rho_y, ':', sprintf('\\rho_y = %.3f', P.rho_y), ...
    'Color', [.5 .5 .5]);
set(gca, 'XTick', 1:Ka, 'XTickLabel', qlbl(1:Ka), 'FontSize', 8, ...
    'XTickLabelRotation', 45);
ylabel('Effective persistence \rho_{eff}'); grid on;
legend('S destroys (no CP)', 'S destroys, CP preserves', ...
    '\rho_{eff} without CP', '\rho_{eff} with CP', ...
    'Location', 'NE', 'FontSize', 8);
title('CP Reduces Lockdown-Induced Persistence');

% --- Fig 3: Country-level CP contribution ---
figure('Name','V4 CP Contribution','Color','w','Position',[800 50 700 500]);
cp_effs = [scen.cp_eff];
[~, si] = sort(cp_effs, 'descend');
barh(1:n_c, cp_effs(si), 'FaceColor', [.2 .5 .8]);
hold on; xline(0, 'k-', 'LineWidth', 1);
set(gca, 'YTick', 1:n_c, 'YTickLabel', {scen(si).iso}, 'FontSize', 7);
xlabel('CP contribution to cumulative output (pp, 10Q)');
title('Country-Level CP Effectiveness');
grid on;

% --- Fig 4: Output-Debt Trade-off (S0 vs S3) ---
figure('Name','V4 Trade-off','Color','w','Position',[800 600 700 500]);
hold on;
for i = 1:n_c
    dy = scen(i).cum0 - scen(i).cum3;
    db = scen(i).bN0  - scen(i).bN3;
    plot(db, dy, 'ko', 'MarkerSize', 6, 'MarkerFaceColor', [.2 .5 .8]);
    text(db+0.15, dy+0.1, scen(i).iso, 'FontSize', 6);
end
xline(0, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]);
xlabel('Debt cost (terminal debt increase, pp)');
ylabel('Output gain (cumulative, pp)');
title('Output-Debt Trade-off: Observed vs No Fiscal');
grid on;

% --- Fig 5: Reallocation comparison (S4 vs S5) ---
figure('Name','V4 Reallocation','Color','w','Position',[100 100 800 500]);
hold on;
dy4 = [scen.cum4] - [scen.cum0];
dy5 = [scen.cum5] - [scen.cum0];
scatter(dy4, dy5, 50, 'filled', 'MarkerFaceColor', [.2 .5 .8]);
for i = 1:n_c
    text(dy4(i)+0.1, dy5(i)+0.1, scen(i).iso, 'FontSize', 6);
end
mn = min([dy4, dy5]); mx = max([dy4, dy5]);
plot([mn mx], [mn mx], 'k--', 'LineWidth', 1);
xline(0, ':', 'Color', [.5 .5 .5]);
yline(0, ':', 'Color', [.5 .5 .5]);
xlabel('Output change, S4 (CP\rightarrowDI) minus S0 (pp)');
ylabel('Output change, S5 (DI\rightarrowCP) minus S0 (pp)');
title('Reallocation Counterfactual: S4 vs S5');
text(0.02, 0.98, ...
    sprintf('Above 45° line: DI\\rightarrowCP preferred (%d/%d)', s5_wins, n_c), ...
    'Units', 'normalized', 'VerticalAlignment', 'top', ...
    'BackgroundColor', 'w', 'FontSize', 9);
grid on;

% --- Fig 6: Sensitivity — Reallocation under alpha_DI=0 vs baseline ---
figure('Name','V4 Sensitivity','Color','w','Position',[150 150 1100 500]);

subplot(1,2,1); hold on;
dy4_b = [scen.cum4] - [scen.cum0];
dy5_b = [scen.cum5] - [scen.cum0];
scatter(dy4_b, dy5_b, 50, 'filled', 'MarkerFaceColor', [.2 .5 .8]);
for i = 1:n_c
    text(dy4_b(i)+0.1, dy5_b(i)+0.1, scen(i).iso, 'FontSize', 5);
end
mn_b = min([dy4_b, dy5_b]); mx_b = max([dy4_b, dy5_b]);
plot([mn_b mx_b], [mn_b mx_b], 'k--', 'LineWidth', 1);
xline(0, ':', 'Color', [.5 .5 .5]); yline(0, ':', 'Color', [.5 .5 .5]);
xlabel('S4 (CP\rightarrowDI) - S0 (pp)');
ylabel('S5 (DI\rightarrowCP) - S0 (pp)');
title(sprintf('Baseline (\\alpha_{DI} = %.3f)', P.alpha_F_DI));
text(0.02, 0.98, sprintf('CP\\rightarrowDI wins: %d/%d', s4_wins, n_c), ...
    'Units', 'normalized', 'VerticalAlignment', 'top', ...
    'BackgroundColor', 'w', 'FontSize', 9);
grid on;

subplot(1,2,2); hold on;
dy4_s = [scen_s.cum4] - [scen_s.cum0];
dy5_s = [scen_s.cum5] - [scen_s.cum0];
scatter(dy4_s, dy5_s, 50, 'filled', 'MarkerFaceColor', [.8 .3 .3]);
for i = 1:n_c
    text(dy4_s(i)+0.1, dy5_s(i)+0.1, scen_s(i).iso, 'FontSize', 5);
end
mn_s = min([dy4_s, dy5_s]); mx_s = max([dy4_s, dy5_s]);
plot([mn_s mx_s], [mn_s mx_s], 'k--', 'LineWidth', 1);
xline(0, ':', 'Color', [.5 .5 .5]); yline(0, ':', 'Color', [.5 .5 .5]);
xlabel('S4 (CP\rightarrowDI) - S0 (pp)');
ylabel('S5 (DI\rightarrowCP) - S0 (pp)');
title('Conservative (\alpha_{DI} = 0)');
text(0.02, 0.98, sprintf('DI\\rightarrowCP wins: %d/%d', s5_wins_s, n_c), ...
    'Units', 'normalized', 'VerticalAlignment', 'top', ...
    'BackgroundColor', 'w', 'FontSize', 9);
grid on;

sgtitle('Reallocation Sensitivity: Baseline vs Conservative DI Assumption', ...
    'FontWeight', 'bold');

fprintf('\n\n=== SCENARIO ANALYSIS COMPLETE ===\n');
fprintf('Output: OECD median trajectories, country-level decomposition,\n');
fprintf('        reallocation winners (baseline + sensitivity), persistence gain,\n');
fprintf('        trade-off scatter.\n\n');


%% ########################################################################
%  LOCAL FUNCTION: forward_roll
%  (copy of the function from pandemic_calibration_v4.m; duplicated here
%  so this file can run standalone after the calibration script)
%  ########################################################################

function xs = forward_roll(fcp, fcp_above, fcp_loans, fcp_guar, ...
                           fdi, fh, S, theta, d, mu_y, mu_b, P)
% Forward-rolls the state (y, b, w, z) given controls and exogenous inputs.
%
% OUTPUT EQUATION (paper Table 3, col 3):
%   y_{k+1} = mu_y + (rho_y + psi*S_k + eta_p*w_k)*y_k
%             + alpha_S*S_k + alpha_DI*z_k + beta_d*d_k + eps_y(k+1)
%
%   Here w_k = F^CP_{k-1} (lagged by one period). CP enters only through
%   the persistence channel: it has no contemporaneous level effect on y.
%   The aggregate F^CP drives persistence; disaggregation is used only
%   for debt accounting.
%
% DEBT EQUATION (paper Table 8, col 5, with kappa_H calibrated to 1.0):
%   b_{k+1} = mu_b + (1+r)*b_k - gamma_y*y_k
%             + kappa_above*F^CP,above_k
%             + kappa_loans*F^CP,loans_k
%             + kappa_guar*F^CP,guar_k
%             + kappa_DI*F^DI_k
%             + kappa_H*F^H_k
%
% STATE: x = (y, b, w, z)
%   w = F^CP_{k-1} (aggregate, for persistence channel)
%   z = F^DI_{k-1} (for demand-injection level channel)
%
% All variables in pp. No /100 conversions.

    N_ = P.N;
    xs = zeros(P.nx, N_+1);
    for k = 1:N_
        y = xs(1,k); b = xs(2,k); w = xs(3,k); z = xs(4,k);
        fk = 0; fa = 0; fl = 0; fg = 0; gk = 0; hk = 0;
        Sk = 0; thk = 0; dk = 0; ey = 0;
        if k <= length(fcp),       fk  = fcp(k);       end
        if k <= length(fcp_above), fa  = fcp_above(k); end
        if k <= length(fcp_loans), fl  = fcp_loans(k); end
        if k <= length(fcp_guar),  fg  = fcp_guar(k);  end
        if k <= length(fdi),       gk  = fdi(k);       end
        if k <= length(fh),        hk  = fh(k);        end
        if k <= length(S),         Sk  = S(k);         end
        if k <= length(theta),     thk = theta(k);     end
        if k <= length(d),         dk  = d(k);         end
        if k+1 <= length(P.eps_y_vec), ey = P.eps_y_vec(k+1); end

        % Effective persistence (aggregate CP enters via w = F^CP_{k-1})
        rho_eff = P.rho_y + P.psi*Sk + P.eta_p*w;

        % Output: CP only through persistence; DI level via z = F^DI_{k-1}
        xs(1,k+1) = mu_y + rho_eff*y + P.alpha_S*Sk ...
                  + P.alpha_F_DI*z + P.beta_fear*dk + ey;

        % Debt: three-way CP disaggregation + DI + health (kappa_H = 1.0)
        xs(2,k+1) = mu_b + (1+P.r_int)*b - P.gamma_y*y ...
                  + P.kappa_above*fa + P.kappa_loans*fl + P.kappa_guar*fg ...
                  + P.kappa_F_DI*gk ...
                  + P.kappa_H*hk;

        % Lagged fiscal states
        xs(3,k+1) = fk;   % w_{k+1} = F^CP_k (aggregate)
        xs(4,k+1) = gk;   % z_{k+1} = F^DI_k
    end
end