"""
PANDEMIC SOCIAL PLANNER — V6 Python Cross-Check
================================================
Mirrors pandemic_sequentialALV6.m exactly, with one documented fix:

  BUG FIX (vs MATLAB V6):
    MATLAB:  Quu = R + B'*P*B  (R undiscounted)
    Correct: Quu = beta^(k-1)*R + B'*P*B  (R discounted, consistent with
             al_cost which evaluates beta^(k-1)*0.5*u'Ru)

  Purpose: numerical cross-check. Prints a comparison table at the end.

Transition system (5-state, full Section 2 model):
  y_{k+1}     = rho_y*y + (psi*y - alpha_S)*S
                + (alpha_F_CP + eta_tilde*S - eta_p*y)*F_CP
                + alpha_F_DI*z + beta_fear*d
  d_{k+1}     = delta_theta(k) * theta
  b_{k+1}     = (1+r)*b - gamma_y*y + kappa_CP*F_CP + kappa_DI*F_DI + c_H*theta
  theta_{k+1} = rho_theta(k)*(1 - phi_S*S)*theta  [+ wave shock]
  z_{k+1}     = F_DI

State:   x = (y, d, b, theta, z)'  in R^5
Control: u = (S, F_DI, F_CP)'      in R^3
"""

import numpy as np
from numpy.linalg import solve, norm
import warnings

# ============================================================
#  PARAMETERS  (identical to MATLAB V6)
# ============================================================
N          = 12
beta_disc  = 0.99

rho_y       = 0.00
psi         = 0.372
alpha_S     = 0.025
alpha_F_CP  = 0.264
eta_tilde   = -0.607
eta_p       = 0.024
alpha_F_DI  = 0.220
beta_fear   = -7.5

rho_theta_base = 1.30
phi_S          = 0.55
delta_theta    = 0.1

wave_quarters = [3, 5, 7, 8]       # 1-indexed quarters (same as MATLAB)
wave_shocks   = [0.02, 0.015, 0.025, 0.04]
wave_rho      = [1.30, 1.25, 1.40, 1.60]
wave_ifr      = [0.02, 0.025, 0.02, 0.005]

# Time-varying epi parameters (1-indexed: index k corresponds to MATLAB k)
rho_theta_t   = rho_theta_base * np.ones(N + 1)   # index 1..N
delta_theta_t = delta_theta    * np.ones(N + 1)
for w, kw in enumerate(wave_quarters):
    if kw <= N:
        rho_theta_t[kw] = wave_rho[w]
        if kw + 1 <= N:
            rho_theta_t[kw + 1] = 0.5 * (wave_rho[w] + rho_theta_base)
        delta_theta_t[kw:] = wave_ifr[w]

r_int      = 0.001
gamma_y    = 0.191
kappa_F_CP = 0.193
kappa_F_DI = 0.468
c_H        = 0.02

n_x = 5;  n_u = 3
S_max   = 0.86
FDI_max = 0.08
FCP_max = 0.10
u_min = np.array([0.0,    0.0,    0.0])
u_max = np.array([S_max,  FDI_max, FCP_max])

# Wave shock vector (index 1..N+1 matches MATLAB wave_shock_vec(k+1))
wave_shock_vec = np.zeros(N + 2)   # index 0..N+1; use [k+1] for step k (1-indexed)
for w, kw in enumerate(wave_quarters):
    k_arr = kw + 1
    if k_arr <= N + 1:
        wave_shock_vec[k_arr] = wave_shocks[w]

x0 = np.array([0.0, 0.0, 0.0, 0.03, 0.0])

# Objective weights
GDP_pc       = 45000
VSL_baseline = 60
MCPF         = 0.3
d_scale      = 0.001
y_scale      = 0.05
w_y          = 100.0
w_d_baseline = w_y * VSL_baseline * (y_scale / d_scale)**2
w_b_baseline = w_y * MCPF
W_b_baseline = w_b_baseline

r_S  = 10.0
r_DI = 5.0
r_CP = 0.5


# ============================================================
#  DYNAMICS
# ============================================================
def dynamics(x, u, k, p=None):
    """
    Full transition f(x,u,k).
    k is 1-indexed (1..N) matching MATLAB convention.
    """
    y, d, b, th, z = x
    S, Fdi, Fcp    = u

    rho_eff = rho_y + psi * S - eta_p * Fcp
    y_next  = (rho_eff * y
               - alpha_S * S
               + (alpha_F_CP + eta_tilde * S - eta_p * y) * Fcp
               + alpha_F_DI * z
               + beta_fear * d)
    d_next  = delta_theta_t[k] * th
    b_next  = (1 + r_int) * b - gamma_y * y + kappa_F_CP * Fcp + kappa_F_DI * Fdi + c_H * th
    th_next = rho_theta_t[k] * (1 - phi_S * S) * th + wave_shock_vec[k + 1]
    z_next  = Fdi

    return np.array([y_next, d_next, b_next, th_next, z_next])


def jacobians(x, u, k):
    """
    Analytical Jacobians A_k = df/dx, B_k = df/du.
    All entries verified against transition function.
    """
    y, d, b, th, z = x
    S, Fdi, Fcp    = u

    A = np.array([
        [rho_y + psi * S - eta_p * Fcp,  beta_fear, 0,       0,                              alpha_F_DI],
        [0,                               0,         0,       delta_theta_t[k],               0         ],
        [-gamma_y,                        0,         1+r_int, c_H,                            0         ],
        [0,                               0,         0,       rho_theta_t[k]*(1-phi_S*S),     0         ],
        [0,                               0,         0,       0,                              0         ],
    ])

    B = np.array([
        [psi * y - alpha_S + eta_tilde * Fcp,   0,          alpha_F_CP + eta_tilde * S - eta_p * y],
        [0,                                      0,          0                                      ],
        [0,                                      kappa_F_DI, kappa_F_CP                             ],
        [-rho_theta_t[k] * phi_S * th,           0,          0                                      ],
        [0,                                      1,          0                                      ],
    ])

    return A, B


# ============================================================
#  COST FUNCTION  (matches al_cost_v6)
# ============================================================
def al_cost(x_traj, u_traj, Q_base, Q_N, R,
            lam, mu, C_u, c_bounds, N):
    """
    Augmented Lagrangian cost (identical to al_cost_v6 in MATLAB).
    x_traj : (n_x, N+2) — columns 0..N
    u_traj : (n_u, N+1) — columns 1..N (1-indexed)
    """
    J = 0.0
    for k in range(1, N + 1):
        disc = beta_disc ** (k - 1)
        sc   = disc * (0.5 * x_traj[:, k] @ Q_base @ x_traj[:, k]
                     + 0.5 * u_traj[:, k] @ R     @ u_traj[:, k])
        cv   = C_u @ u_traj[:, k] - c_bounds
        act  = (cv > 0) | (lam[:, k] > 0)
        Im   = np.diag(mu[:, k] * act)
        ac   = lam[:, k] @ cv + 0.5 * cv @ Im @ cv
        J   += sc + ac
    J += beta_disc**N * 0.5 * x_traj[:, N+1] @ Q_N @ x_traj[:, N+1]
    return J


def simulate_forward(x0, u_traj, N):
    """
    Propagate through nonlinear dynamics.
    u_traj : (n_u, N+1) — columns 1..N (1-indexed)
    Returns x_traj : (n_x, N+2) — columns 0..N
    """
    x = np.zeros((n_x, N + 2))
    x[:, 0] = x0                           # x[:,0] unused (pre-initial)
    x[:, 1] = x0                           # x[:,1] = x_0
    for k in range(1, N + 1):
        x[:, k + 1] = dynamics(x[:, k], u_traj[:, k], k)
    return x


# ============================================================
#  CORE iLQR SOLVER
# ============================================================
def solve_pandemic_ilqr(w_y_, w_d_, w_b_, W_b_,
                         r_S_, r_DI_, r_CP_,
                         discount_R=True):
    """
    AL-iLQR solver.

    discount_R=True  : correct version  — R_k = beta^(k-1) * R  in Quu
    discount_R=False : MATLAB V6 behaviour — R undiscounted in Quu

    Note: al_cost always discounts R (correct). discount_R only affects
    the backward pass Quu computation.
    """
    Q_base = np.diag([w_y_, w_d_, w_b_, 0.0, 0.0])
    Q_N    = np.diag([0.0,  0.0,  W_b_, 0.0, 0.0])
    R      = np.diag([r_S_, r_DI_, r_CP_])

    # Box constraints: C_u * u <= c_bounds  (6 inequalities)
    C_u     = np.array([[-1,0,0],[1,0,0],[0,-1,0],[0,1,0],[0,0,-1],[0,0,1]], dtype=float)
    c_bounds = np.array([0, S_max, 0, FDI_max, 0, FCP_max], dtype=float)
    n_c     = 6

    # ---- Heuristic initialization ----
    # x_traj[:,0] unused; x_traj[:,1] = x0; x_traj[:,k+1] = f(x[:,k], u[:,k], k)
    x_bar = np.zeros((n_x, N + 2))
    u_bar = np.zeros((n_u, N + 1))   # u[:,k] for k=1..N
    x_bar[:, 1] = x0
    for k in range(1, N + 1):
        base_S  = max(0.1, 0.7 - 0.05 * (k - 1))
        if any(abs(k - q) <= 1 for q in wave_quarters):
            base_S = min(S_max, base_S + 0.2)
        base_CP = max(0.0, 0.06 - 0.005 * (k - 1))
        u_bar[:, k] = [base_S, 0.002, base_CP]
        x_bar[:, k + 1] = dynamics(x_bar[:, k], u_bar[:, k], k)

    # ---- AL parameters ----
    max_outer = 30
    max_inner = 60
    tol_inner = 1e-4
    tol_outer = 1e-4
    phi_pen   = 2.5
    rho_base  = 1e-4

    lam = np.zeros((n_c, N + 1))   # dual variables (1-indexed)
    mu  = 10.0 * np.ones((n_c, N + 1))

    for outer in range(max_outer):
        for inner in range(max_inner):

            # ---- Compute Jacobians along current trajectory ----
            Ak = {}
            Bk = {}
            for k in range(1, N + 1):
                Ak[k], Bk[k] = jacobians(x_bar[:, k], u_bar[:, k], k)

            # ---- Backward pass ----
            P_store = {}
            p_store = {}
            P_store[N + 1] = Q_N * beta_disc**N
            p_store[N + 1] = Q_N * beta_disc**N @ x_bar[:, N + 1]

            K_store   = {}
            k_ff_store = {}

            rho_reg = rho_base
            bw_ok   = False
            while not bw_ok and rho_reg < 1e6:
                bw_ok = True
                P = P_store[N + 1].copy()
                p = p_store[N + 1].copy()
                # Temporary storage for this regularization attempt
                K_tmp   = {}
                kff_tmp = {}
                P_tmp   = {N + 1: P_store[N + 1].copy()}
                p_tmp   = {N + 1: p_store[N + 1].copy()}

                for k in range(N, 0, -1):
                    disc  = beta_disc ** (k - 1)
                    Q_k   = Q_base * disc
                    R_k   = R * disc if discount_R else R    # <-- THE FIX

                    A = Ak[k];  B = Bk[k]
                    pk1 = p_tmp[k + 1]
                    Pk1 = P_tmp[k + 1]

                    c_val = C_u @ u_bar[:, k] - c_bounds
                    act   = (c_val > 0) | (lam[:, k] > 0)
                    I_mu  = np.diag(mu[:, k] * act)

                    Qx  = Q_k @ x_bar[:, k] + A.T @ pk1
                    Qxx = Q_k + A.T @ Pk1 @ A
                    Qux = B.T @ Pk1 @ A
                    Qu  = R_k @ u_bar[:, k] + B.T @ pk1 + C_u.T @ (lam[:, k] + I_mu @ c_val)
                    Quu = R_k + B.T @ Pk1 @ B + C_u.T @ I_mu @ C_u + rho_reg * np.eye(n_u)

                    # Cholesky check
                    try:
                        np.linalg.cholesky(Quu)
                    except np.linalg.LinAlgError:
                        bw_ok   = False
                        rho_reg = rho_reg * 10
                        break

                    K_tmp[k]   = solve(Quu, Qux)          # K = Q_uu^{-1} Q_ux (minus in fwd pass)
                    kff_tmp[k] = -solve(Quu, Qu)           # k_ff = -Q_uu^{-1} Q_u
                    p_tmp[k]   = Qx + K_tmp[k].T @ Quu @ kff_tmp[k]
                    P_tmp[k]   = Qxx - K_tmp[k].T @ Quu @ K_tmp[k]

                if bw_ok:
                    K_store   = K_tmp
                    k_ff_store = kff_tmp
                    P_store    = P_tmp
                    p_store    = p_tmp

            if not bw_ok:
                break

            # ---- Forward pass with line search ----
            cost_old = al_cost(x_bar, u_bar, Q_base, Q_N, R, lam, mu, C_u, c_bounds, N)
            al       = 1.0
            ls_ok    = False

            while al > 1e-8:
                xn = np.zeros((n_x, N + 2))
                un = np.zeros((n_u, N + 1))
                xn[:, 1] = x0
                for k in range(1, N + 1):
                    dx       = xn[:, k] - x_bar[:, k]
                    un[:, k] = u_bar[:, k] + al * k_ff_store[k] - K_store[k] @ dx
                    un[:, k] = np.clip(un[:, k], u_min, u_max)
                    xn[:, k + 1] = dynamics(xn[:, k], un[:, k], k)

                cost_new = al_cost(xn, un, Q_base, Q_N, R, lam, mu, C_u, c_bounds, N)
                if cost_new < cost_old:
                    ls_ok = True
                    break
                al *= 0.5

            if not ls_ok:
                break

            dx_norm = norm(xn - x_bar, 'fro') / (norm(x_bar, 'fro') + 1e-12)
            x_bar = xn.copy()
            u_bar = un.copy()
            if dx_norm < tol_inner:
                break

        # ---- Update multipliers ----
        mv = 0.0
        for k in range(1, N + 1):
            c_val   = C_u @ u_bar[:, k] - c_bounds
            mv      = max(mv, float(np.max(c_val)))
            lam[:, k] = np.maximum(0.0, lam[:, k] + mu[:, k] * c_val)
            mu[:, k] *= phi_pen
        if mv < tol_outer:
            break

    J_opt = al_cost(x_bar, u_bar, Q_base, Q_N, R,
                    np.zeros_like(lam), np.zeros_like(mu), C_u, c_bounds, N)
    return x_bar, u_bar, J_opt


# ============================================================
#  NO-INTERVENTION BENCHMARK
# ============================================================
def simulate_no_intervention():
    x_unc = np.zeros((n_x, N + 2))
    x_unc[:, 1] = x0
    for k in range(1, N + 1):
        x_unc[0, k+1] = rho_y * x_unc[0, k] + beta_fear * x_unc[1, k]
        x_unc[1, k+1] = delta_theta_t[k] * x_unc[3, k]
        x_unc[2, k+1] = (1+r_int)*x_unc[2,k] - gamma_y*x_unc[0,k] + c_H*x_unc[3,k]
        x_unc[3, k+1] = rho_theta_t[k]*x_unc[3,k] + wave_shock_vec[k+1]
        x_unc[4, k+1] = 0.0
    return x_unc


# ============================================================
#  HELPERS
# ============================================================
def print_results(label, x, u):
    print(f"\n=== {label} ===")
    print(f"  Avg output gap:       {np.mean(x[0, 1:N+1])*100:.2f}%")
    print(f"  Cumul. mortality:     {np.sum(x[1, 2:N+2])*100:.3f}%")
    print(f"  Terminal debt:        {x[2, N+1]*100:.1f} pp GDP")
    print(f"  Avg containment S:    {np.mean(u[0, 1:N+1]):.3f}")
    print(f"  Avg CP:               {np.mean(u[2, 1:N+1])*100:.2f}% GDP")
    print(f"  Avg DI:               {np.mean(u[1, 1:N+1])*100:.3f}% GDP")


# ============================================================
#  MAIN — BASELINE + COMPARISON TABLE
# ============================================================
if __name__ == "__main__":
    print("=" * 60)
    print("  PANDEMIC TRILEMMA V6 — Python Cross-Check")
    print("  VSL=60, MCPF=0.3 (baseline)")
    print("=" * 60)

    # ---- Run correct version (discounted R) ----
    print("\n[1/2] Solving with CORRECTED R (beta^k discounting)...")
    x_cor, u_cor, J_cor = solve_pandemic_ilqr(
        w_y, w_d_baseline, w_b_baseline, W_b_baseline,
        r_S, r_DI, r_CP, discount_R=True)
    print_results("Python — Corrected R (discount_R=True)", x_cor, u_cor)
    print(f"  Total cost J:         {J_cor:.4f}")

    # ---- Run MATLAB-equivalent (undiscounted R in backward pass) ----
    print("\n[2/2] Solving with MATLAB V6 R (undiscounted in backward pass)...")
    x_mat, u_mat, J_mat = solve_pandemic_ilqr(
        w_y, w_d_baseline, w_b_baseline, W_b_baseline,
        r_S, r_DI, r_CP, discount_R=False)
    print_results("Python — MATLAB-equivalent R (discount_R=False)", x_mat, u_mat)
    print(f"  Total cost J:         {J_mat:.4f}")

    # ---- No-intervention benchmark ----
    x_unc = simulate_no_intervention()
    print(f"\nNo-intervention benchmark:")
    print(f"  Avg output gap:       {np.mean(x_unc[0, 1:N+1])*100:.2f}%")
    print(f"  Cumul. mortality:     {np.sum(x_unc[1, 2:N+2])*100:.3f}%")
    print(f"  Terminal debt:        {x_unc[2, N+1]*100:.1f} pp GDP")

    # ---- Comparison table ----
    print("\n" + "=" * 60)
    print("  COMPARISON: Corrected vs MATLAB-equivalent")
    print("=" * 60)
    metrics = {
        "Avg output gap (%)":     (np.mean(x_cor[0,1:N+1])*100,  np.mean(x_mat[0,1:N+1])*100),
        "Cumul. mortality (%)":   (np.sum(x_cor[1,2:N+2])*100,   np.sum(x_mat[1,2:N+2])*100),
        "Terminal debt (pp)":     (x_cor[2,N+1]*100,              x_mat[2,N+1]*100),
        "Avg S":                  (np.mean(u_cor[0,1:N+1]),       np.mean(u_mat[0,1:N+1])),
        "Avg CP (% GDP)":         (np.mean(u_cor[2,1:N+1])*100,   np.mean(u_mat[2,1:N+1])*100),
        "Avg DI (% GDP)":         (np.mean(u_cor[1,1:N+1])*100,   np.mean(u_mat[1,1:N+1])*100),
        "Total cost J":           (J_cor,                          J_mat),
    }
    print(f"  {'Metric':<28} {'Corrected':>12} {'MATLAB-equiv':>14} {'Diff':>10}")
    print("  " + "-" * 66)
    for name, (v_cor, v_mat) in metrics.items():
        diff = v_cor - v_mat
        print(f"  {name:<28} {v_cor:>12.4f} {v_mat:>14.4f} {diff:>10.4f}")

    print("\n  NOTE: 'Diff' column isolates the effect of the R-discounting bug.")
    print("  A non-zero diff means the MATLAB backward pass over-penalises")
    print("  late-period controls (R not shrinking with beta^k), biasing")
    print("  the solution toward less active fiscal intervention in later quarters.")

    # ---- Control trajectory comparison ----
    print("\n  Quarter-by-quarter control paths (MATLAB-equivalent):")
    print(f"  {'k':>3} {'S_cor':>7} {'S_mat':>7} | {'CP_cor':>7} {'CP_mat':>7} | {'DI_cor':>7} {'DI_mat':>7}")
    print("  " + "-" * 55)
    for k in range(1, N + 1):
        print(f"  {k:>3} {u_cor[0,k]:>7.3f} {u_mat[0,k]:>7.3f} | "
              f"{u_cor[2,k]*100:>7.3f} {u_mat[2,k]*100:>7.3f} | "
              f"{u_cor[1,k]*100:>7.4f} {u_mat[1,k]*100:>7.4f}")

    # ---- VSL sensitivity (Python, corrected R) ----
    print("\n" + "=" * 60)
    print("  VSL SENSITIVITY (Python corrected, discount_R=True)")
    print("=" * 60)
    VSL_grid = [20, 40, 60, 80, 100, 140]
    print(f"  {'VSL':>6} | {'Avg y(%)':>9} | {'Cum d(%)':>9} | {'End b(pp)':>10} | {'Avg S':>6} | {'Avg CP':>7} | {'Avg DI':>7}")
    print("  " + "-" * 72)
    for vsl in VSL_grid:
        w_d_v = w_y * vsl * (y_scale / d_scale)**2
        xv, uv, Jv = solve_pandemic_ilqr(
            w_y, w_d_v, w_b_baseline, W_b_baseline,
            r_S, r_DI, r_CP, discount_R=True)
        print(f"  {vsl:>6} | {np.mean(xv[0,1:N+1])*100:>9.2f} | "
              f"{np.sum(xv[1,2:N+2])*100:>9.3f} | "
              f"{xv[2,N+1]*100:>10.1f} | "
              f"{np.mean(uv[0,1:N+1]):>6.2f} | "
              f"{np.mean(uv[2,1:N+1])*100:>7.2f} | "
              f"{np.mean(uv[1,1:N+1])*100:>7.3f}")

    print("\nDone.")
