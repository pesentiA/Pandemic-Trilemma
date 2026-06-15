"""
=========================================================================
 PANDEMIC TRILEMMA - iLQR SOLVER, AVERAGE OECD ECONOMY (Python)
 -------------------------------------------------------------------------
 Solves the finite-horizon planner problem on the calibrated V15 dynamics
 with iterative LQR (iLQR; Li & Todorov 2004, Tassa et al. 2014).

 ALGORITHM OVERVIEW (the 6 steps, marked in the code):
   STEP 0  State augmentation. iLQR needs Markovian dynamics x' = f_k(x,u).
           The V15 system has lagged controls (F_above enters output with a
           2-quarter lag, F_DI with 1 quarter) and a cumulated below-the-line
           stock. These become extra states:
              x = [ y, b, theta, d, fab_l1, fab_l2, fdi_l1, stock ]   (n=8)
              u = [ S, F_above, F_loans, F_guar, F_DI ]               (m=5)
   STEP 1  Nominal rollout: simulate forward with an initial control guess
           to obtain the nominal trajectory {x_k, u_k}.
   STEP 2  Linearize/quadratize along the trajectory: Jacobians A_k = df/dx,
           B_k = df/du (analytic; the only nonlinearities are the bilinear
           terms S*theta and S*F_DI_l1, so A_k, B_k depend on the nominal
           trajectory - this is precisely why plain LQR is not enough),
           and cost derivatives l_x, l_u, l_xx, l_uu.
   STEP 3  Backward pass (Riccati-like recursion): starting from the terminal
           value function, recurse
              Q_x = l_x + A' V_x,        Q_u  = l_u + B' V_x,
              Q_xx= l_xx+ A' V_xx A,     Q_uu = l_uu+ B' V_xx B + mu*I,
              Q_ux= B' V_xx A,
           and obtain feedforward k = -Q_uu^{-1} Q_u and feedback
           K = -Q_uu^{-1} Q_ux  (Gauss-Newton variant: tensor terms
           V_x * f_xx are dropped; exact here up to the two bilinear terms).
           mu is a Levenberg-Marquardt regularizer, adapted automatically.
   STEP 4  Forward pass with backtracking line search:
              u_k = clip( u_nom_k + alpha*k_k + K_k (x_k - x_nom_k), lb, ub )
           Box constraints (S and the fiscal instruments must stay inside
           the observed OECD support) are enforced by clamping - the simple
           variant of Tassa et al. (2014)'s control-limited DDP.
   STEP 5  Accept the new trajectory if cost decreased; shrink mu. Otherwise
           grow mu and retry. Iterate STEPs 2-5 until the relative cost
           improvement < tol.

 ECONOMIC OBJECT: J = sum_{q=1..N} beta^q [ w_y y_q^2 + lam_d w_d d_q^2
                  + w_b (b_q - b0)^2 + p_S S_q^2 + p_F sum F_q^2 ]
 Weights w = inverse pooled variance of the observed OECD paths
 (one observed standard deviation of each target costs the same);
 lam_d scales the health weight; p are small control-effort penalties.
=========================================================================
"""
import numpy as np
import pandas as pd

# ------------------------------------------------------------------------
# Calibrated V15 parameters (positive block, estimated on the OECD panel)
# ------------------------------------------------------------------------
rho_y, alpha_S = 0.231, -0.0952
alpha_above, alpha_below = 0.544, 0.261165
alpha_DI_lag1, alpha_S_DI = 1.470, -0.0406
beta_d = 0.0115
c_lo, c_gu = 0.40, 0.25/0.35           # take-up adjustments
r_int, gamma_y = 0.001, 0.176
kappa_above, kappa_loans, kappa_guar, kappa_DI = 0.392, 0.891, 0.111, 0.396
phi_t = -0.076
rho_theta, phi_S = 1.035, 0.314
ifr = np.array([0.009,0.007,0.006,0.004,0.003,0.0004])
delta_q = (ifr*1e6)[np.array([1,1,1,2,3,3,4,5,5,6,6,6,6])-1]   # wave-indexed
N = 13
yr = np.arange(4,17)
qord = ['Q4.2019','Q1.2020','Q2.2020','Q3.2020','Q4.2020','Q1.2021','Q2.2021',
        'Q3.2021','Q4.2021','Q1.2022','Q2.2022','Q3.2022','Q4.2022']
qlbl = ['Q4.19','Q1.20','Q2.20','Q3.20','Q4.20','Q1.21','Q2.21','Q3.21',
        'Q4.21','Q1.22','Q2.22','Q3.22','Q4.22']

cfe_y_val=[+1.1057,-1.0400,+0.3009,-0.0979,+1.2987,+1.3246,+1.9894,+0.1466,
           -3.5623,-1.7381,-0.0832,-4.8958,-1.8327,-1.4833,-1.8693,-3.3268,
           +0.2908,-2.0146,+8.3187,-4.8488,+2.3672,-0.5014,-1.9561,+0.6141,
           +0.7002,+2.3568,-0.6830,-3.2057,+1.0604,+1.2218,-1.0616,-0.5966,
           -2.7751,-0.2578,-1.8284,+1.0349,+4.0658,+1.0987]
cfe_b_val=[+0.2296,+0.3707,+0.3387,+0.7393,+0.5104,-0.0655,+1.4904,+0.9267,
           -0.0006,-0.0780,-0.5517,-0.2551,+0.3079,+0.2583,+0.0484,-0.0142,
           +0.1481,-0.0351,+1.3739,-0.1310,+0.7809,+0.1886,+0.0381,+0.3889,
           +0.3229,+1.0169,+0.8753,-0.5943,+0.4736,+0.7201,+1.2750,+0.0755,
           -0.9275,+0.6452,+0.0694,+0.3347,+0.9441,+1.3417]
b0_val=[37.6,69.7,77.9,44.2,16.3,29.6,47.7,53.2,30.2,34.0,34.1,86.0,11.8,54.5,
        85.4,107.0,205.0,52.1,54.3,69.9,56.9,122.0,199.0,35.1,29.3,26.6,39.7,
        7.25,43.9,16.6,73.0,45.6,111.0,48.4,72.1,27.7,54.0,97.3]
eps_v14=[-3.62,-8.55,-6.46,-7.79,-4.73,-10.10,-11.20,-3.75,-5.10,-5.98,-1.95,
         -9.57,-2.43,-1.97,-8.49,-12.50,-9.94,-8.99,-1.10,-4.70,-0.66,-10.90,
         -6.82,-1.83,-0.03,-5.31,-7.43,-7.97,-5.17,-2.45,-5.49,-1.80,-9.75,
         -4.87,-6.78,-4.70,-10.40,-4.79]

# ------------------------------------------------------------------------
# DATA: rebuild observed OECD paths -> representative agent + weights
# ------------------------------------------------------------------------
T = pd.read_csv("country_data_for_matlab.csv")
M = pd.read_csv("weekly_mortality_matlab.csv")
M["date"]=pd.to_datetime(M["date"])
M["qstr"]="Q"+M["date"].dt.quarter.astype(str)+"."+M["date"].dt.year.astype(str)
M["d_pmw"]=M["deaths_w"]/M["pop"]*1e6
g=M.groupby(["Country","qstr"]).agg(th=("theta_hat","mean"), d=("d_pmw","mean"))

countries=list(pd.unique(T["Country"])); n_c=len(countries)
S_o=np.zeros((n_c,N)); Fa=np.zeros((n_c,N)); Fl=np.zeros((n_c,N))
Fg=np.zeros((n_c,N)); Fd=np.zeros((n_c,N)); y_o=np.zeros((n_c,N))
bd=np.zeros((n_c,N)); th_o=np.zeros((n_c,N)); d_o=np.zeros((n_c,N))
for i,iso in enumerate(countries):
    Tc=T[T["Country"]==iso]
    for k in range(N):
        row=Tc[Tc["Quarter"]==qord[k]]
        if row.empty: continue
        r=row.iloc[0]
        S_o[i,k]=r["S_mean_tw"]; Fa[i,k]=r["F_CP_above_3"]; Fl[i,k]=r["F_CP_loans"]
        Fg[i,k]=r["F_CP_guar_adj"]; Fd[i,k]=r["F_DI"]; y_o[i,k]=r["y_t_pct"]
        if pd.notna(r["debt_dR"]): bd[i,k]=r["debt_dR"]
        key=(iso,qord[k])
        if key in g.index:
            v=g.loc[key]
            if pd.notna(v["th"]): th_o[i,k]=v["th"]
            if pd.notna(v["d"]):  d_o[i,k]=v["d"]
b_lvl=np.array(b0_val)[:,None]+np.cumsum(bd,axis=1)

# exogenous epidemic impulses: theta innovations under observed policy
eps_th=np.zeros((n_c,N+1))
for i in range(n_c):
    for k in range(1,N):
        eps_th[i,k+1]=th_o[i,k]-rho_theta*(1-phi_S*S_o[i,k]/100)*th_o[i,k-1]
eps_th_avg=eps_th.mean(axis=0)

b0   = float(np.mean(b0_val))
mu_y = float(np.mean(cfe_y_val)); mu_b = float(np.mean(cfe_b_val))
eps_y=np.zeros(N+1); eps_y[3]=float(np.mean(eps_v14))      # Q2.20 shock

# planner weights and control bounds
beta = 0.99
lam_d = 1.0
w_y = 1/np.var(y_o); w_d = 1/np.var(d_o)
w_b = 1/np.var(b_lvl-np.array(b0_val)[:,None])
p_S = 0.05/np.var(S_o[S_o>0])
p_F = 0.05/np.var(np.concatenate([Fa[Fa>0],Fl[Fl>0],Fg[Fg>0],Fd[Fd>0]]))
lb = np.zeros(5)
ub = np.array([np.percentile(S_o[S_o>0],95), np.percentile(Fa[Fa>0],95),
               np.percentile(Fl[Fl>0],95),   np.percentile(Fg[Fg>0],95),
               np.percentile(Fd[Fd>0],95)])

n, m = 8, 5
IY,IB,ITH,ID,IA1,IA2,IDI,IST = range(8)      # state indices

# ------------------------------------------------------------------------
# STEP 0/1: Markovian dynamics x' = f_q(x,u) and nominal rollout
# ------------------------------------------------------------------------
def f(x, u, j):
    """One-step transition; j = 0-based stage, quarter q = j+1."""
    q = j+1
    S,fab,flo,fgu,fdi = u
    y,b,th,d,a1,a2,di1,st = x
    st_used = st + c_lo*flo + c_gu*fgu                      # stock incl. current flow
    eth = eps_th_avg[q+1] if (q+1) <= N else 0.0
    xp = np.empty(n)
    xp[ITH] = rho_theta*(1-phi_S*S/100)*th + eth
    xp[ID]  = delta_q[q-1]*th
    xp[IY]  = (mu_y + rho_y*y + alpha_S*S + alpha_above*a2 + alpha_below*st_used
               + alpha_DI_lag1*di1 + alpha_S_DI*S*di1 - beta_d*d + eps_y[q])
    xp[IB]  = (mu_b + (1+r_int)*b - gamma_y*y + kappa_above*fab
               + kappa_loans*c_lo*flo + kappa_guar*c_gu*fgu + kappa_DI*di1
               + phi_t*yr[q-1])
    xp[IA1] = fab
    xp[IA2] = a1
    xp[IDI] = fdi
    xp[IST] = st_used
    return xp

def rollout(U, x0):
    X = np.zeros((N+1, n)); X[0] = x0
    for j in range(N): X[j+1] = f(X[j], U[j], j)
    return X

def state_cost(x):       # w'x^2 form on (y, d, b - b0)
    return w_y*x[IY]**2 + lam_d*w_d*x[ID]**2 + w_b*(x[IB]-b0)**2
def ctrl_cost(u):
    return p_S*u[0]**2 + p_F*np.sum(u[1:]**2)

def total_cost(X, U):
    J = 0.0
    for j in range(N):
        q = j+1
        J += beta**q * (state_cost(X[q]) + ctrl_cost(U[j]))
    return J

# ------------------------------------------------------------------------
# STEP 2: analytic derivatives along the nominal trajectory
# ------------------------------------------------------------------------
def jacobians(x, u, j):
    q = j+1
    S = u[0]; th = x[ITH]; di1 = x[IDI]
    A = np.zeros((n,n)); B = np.zeros((n,m))
    # output row
    A[IY,IY]=rho_y; A[IY,ID]=-beta_d; A[IY,IA2]=alpha_above
    A[IY,IDI]=alpha_DI_lag1 + alpha_S_DI*S; A[IY,IST]=alpha_below
    B[IY,0]=alpha_S + alpha_S_DI*di1
    B[IY,2]=alpha_below*c_lo; B[IY,3]=alpha_below*c_gu
    # debt row
    A[IB,IY]=-gamma_y; A[IB,IB]=1+r_int; A[IB,IDI]=kappa_DI
    B[IB,1]=kappa_above; B[IB,2]=kappa_loans*c_lo; B[IB,3]=kappa_guar*c_gu
    # theta row (bilinear S x theta)
    A[ITH,ITH]=rho_theta*(1-phi_S*S/100)
    B[ITH,0]=-rho_theta*phi_S/100*th
    # deaths row
    A[ID,ITH]=delta_q[q-1]
    # lag bookkeeping
    A[IA2,IA1]=1.0; A[IST,IST]=1.0
    B[IA1,1]=1.0; B[IDI,4]=1.0; B[IST,2]=c_lo; B[IST,3]=c_gu
    return A,B

def cost_derivs(x, u, j):
    q = j+1; dq = beta**q
    lx = np.zeros(n); lxx = np.zeros((n,n))
    lx[IY]=2*dq*w_y*x[IY]; lx[ID]=2*dq*lam_d*w_d*x[ID]; lx[IB]=2*dq*w_b*(x[IB]-b0)
    lxx[IY,IY]=2*dq*w_y; lxx[ID,ID]=2*dq*lam_d*w_d; lxx[IB,IB]=2*dq*w_b
    lu = 2*dq*np.r_[p_S*u[0], p_F*u[1:]]
    luu = 2*dq*np.diag(np.r_[p_S, p_F*np.ones(4)])
    return lx,lu,lxx,luu

# ------------------------------------------------------------------------
# STEPs 3-5: iLQR main loop (backward Riccati pass + line-searched forward)
# ------------------------------------------------------------------------
def ilqr(U0, x0, max_iter=300, tol=1e-9, verbose=True):
    U = U0.copy()
    X = rollout(U, x0)
    J = total_cost(X, U)
    mu = 1e-6
    for it in range(max_iter):
        # ---- backward pass --------------------------------------------
        # terminal value function: cost of x_N already counted in stage
        # j=N-1 (state cost attaches to the arrival state), so V_N = 0.
        # Equivalent formulation: attach state cost of x_{j+1} to stage j.
        Vx = np.zeros(n); Vxx = np.zeros((n,n))
        ks = np.zeros((N,m)); Ks = np.zeros((N,m,n)); fail=False
        for j in reversed(range(N)):
            A,B = jacobians(X[j], U[j], j)
            # stage cost = ctrl(u_j) at q=j+1 PLUS state cost of arrival x_{j+1}
            lxp,_,lxxp,_ = cost_derivs(X[j+1], U[j], j)   # state part at x_{j+1}
            _,lu,_,luu   = cost_derivs(X[j+1], U[j], j)   # ctrl part at q=j+1
            Vx_  = lxp + Vx                                # add arrival state cost
            Vxx_ = lxxp + Vxx
            Qx  = A.T @ Vx_
            Qu  = lu + B.T @ Vx_
            Qxx = A.T @ Vxx_ @ A
            Quu = luu + B.T @ Vxx_ @ B + mu*np.eye(m)
            Qux = B.T @ Vxx_ @ A
            try:
                L = np.linalg.cholesky(Quu)
            except np.linalg.LinAlgError:
                fail=True; break
            kj = -np.linalg.solve(Quu, Qu)
            Kj = -np.linalg.solve(Quu, Qux)
            ks[j], Ks[j] = kj, Kj
            Vx  = Qx + Kj.T@Quu@kj + Kj.T@Qu + Qux.T@kj
            Vxx = Qxx + Kj.T@Quu@Kj + Kj.T@Qux + Qux.T@Kj
            Vxx = 0.5*(Vxx+Vxx.T)
        if fail:
            mu = max(mu*10, 1e-6); continue
        # ---- forward pass with backtracking line search ----------------
        improved=False
        for alpha in 1.0*0.5**np.arange(10):
            Xn = np.zeros_like(X); Xn[0]=x0; Un=np.zeros_like(U)
            for j in range(N):
                du = alpha*ks[j] + Ks[j]@(Xn[j]-X[j])
                Un[j] = np.clip(U[j]+du, lb, ub)          # box constraints
                Xn[j+1] = f(Xn[j], Un[j], j)
            Jn = total_cost(Xn, Un)
            if Jn < J - 1e-12:
                improved=True; break
        if improved:
            rel = (J-Jn)/max(J,1e-12)
            X,U,J = Xn,Un,Jn
            mu = max(mu/2, 1e-8)
            if verbose and (it%10==0 or rel<tol):
                print(f"  iter {it:3d}: J = {J:10.4f}  (rel.impr {rel:.2e}, mu {mu:.1e})")
            if rel < tol:
                print(f"  CONVERGED at iter {it}: J = {J:.4f}")
                break
        else:
            mu *= 10
            if mu > 1e8:
                print(f"  stopped (regularizer maxed) at iter {it}: J = {J:.4f}")
                break
    return X,U,J,ks,Ks

# ------------------------------------------------------------------------
# RUN: warm-start from observed average policy, also from zeros
# ------------------------------------------------------------------------
x0 = np.zeros(n); x0[IB]=b0
U_obs = np.vstack([S_o.mean(0),Fa.mean(0),Fl.mean(0),Fg.mean(0),Fd.mean(0)]).T

print("=== iLQR, average OECD economy (b0 = %.1f%% GDP, lam_d = %.1f) ===" % (b0,lam_d))
print("\nStart 1: observed average policy")
X1,U1,J1,_,_ = ilqr(U_obs, x0)
print("\nStart 2: zero controls")
X2,U2,J2,_,_ = ilqr(np.zeros((N,m)), x0)
X,U,J = (X1,U1,J1) if J1<=J2 else (X2,U2,J2)
print(f"\nBest of multistart: J* = {J:.4f}")

Xo = rollout(U_obs, x0); Jo = total_cost(Xo, U_obs)
print(f"Observed average policy (same closure): J = {Jo:.4f}")

print("\nOptimal policy and trajectory:")
print(f"  {'Q':>6} {'S':>6} {'F_ab':>5} {'F_lo':>5} {'F_gu':>5} {'F_DI':>5} | "
      f"{'y':>7} {'b':>7} {'theta':>8} {'d':>6}")
for j in range(N):
    q=j+1
    print(f"  {qlbl[j]:>6} {U[j,0]:6.1f} {U[j,1]:5.2f} {U[j,2]:5.2f} {U[j,3]:5.2f} "
          f"{U[j,4]:5.2f} | {X[q,IY]:+7.2f} {X[q,IB]:7.2f} {X[q,ITH]:8.5f} {X[q,ID]:6.1f}")
print(f"\n  Cum. output gap: {X[1:,IY].sum():+.2f} ppQ | cum. deaths: "
      f"{X[1:,ID].sum()*13:.0f}/million | terminal debt: {X[N,IB]:.2f}%% GDP "
      f"(Δb {X[N,IB]-b0:+.2f})")
