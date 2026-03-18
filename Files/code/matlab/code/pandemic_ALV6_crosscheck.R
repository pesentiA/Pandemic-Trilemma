# =============================================================================
#  PANDEMIC TRILEMMA V6 — R Cross-Check
#  Mirrors pandemic_sequentialALV6.m exactly.
#  Serves as numerical verification of the MATLAB AL-iLQR.
#
#  Also implements the R-discounting fix and compares results.
#  Run: Rscript pandemic_ALV6_crosscheck.R
# =============================================================================

# --------------- Parameters (identical to MATLAB V6) -----------------------
N         <- 12
beta_disc <- 0.99

rho_y      <- 0.00;  psi       <- 0.372;  alpha_S    <- 0.025
alpha_F_CP <- 0.264; eta_tilde <- -0.607; eta_p      <- 0.024
alpha_F_DI <- 0.220; beta_fear <- -7.5

phi_S          <- 0.55
rho_theta_base <- 1.30
delta_theta    <- 0.1
wave_quarters  <- c(3, 5, 7, 8)
wave_shocks    <- c(0.02, 0.015, 0.025, 0.04)
wave_rho       <- c(1.30, 1.25, 1.40, 1.60)
wave_ifr       <- c(0.02, 0.025, 0.02, 0.005)

# Time-varying params (index 1..N, matching MATLAB k=1..N)
rho_theta_t   <- rep(rho_theta_base, N)
delta_theta_t <- rep(delta_theta, N)
for (w in seq_along(wave_quarters)) {
  kw <- wave_quarters[w]
  if (kw <= N) {
    rho_theta_t[kw] <- wave_rho[w]
    if (kw + 1 <= N) rho_theta_t[kw + 1] <- 0.5 * (wave_rho[w] + rho_theta_base)
    delta_theta_t[kw:N] <- wave_ifr[w]
  }
}

r_int      <- 0.001; gamma_y <- 0.191
kappa_F_CP <- 0.193; kappa_F_DI <- 0.468; c_H <- 0.02

n_x <- 5L; n_u <- 3L
S_max <- 0.86; FDI_max <- 0.08; FCP_max <- 0.10
u_min <- c(0, 0, 0); u_max <- c(S_max, FDI_max, FCP_max)

# Wave shocks: index 1..(N+1), wave_shock_vec[k+1] used at step k
wave_shock_vec <- numeric(N + 2)   # indices 1..(N+2)
for (w in seq_along(wave_quarters)) {
  k_arr <- wave_quarters[w] + 1
  if (k_arr <= N + 1) wave_shock_vec[k_arr + 1] <- wave_shocks[w]  # +1 for R 1-indexing
}

x0 <- c(0, 0, 0, 0.03, 0)

# Weights
w_y          <- 100; d_scale <- 0.001; y_scale <- 0.05
VSL_baseline <- 60;  MCPF    <- 0.3
w_d_baseline <- w_y * VSL_baseline * (y_scale / d_scale)^2
w_b_baseline <- w_y * MCPF
W_b_baseline <- w_b_baseline
r_S <- 10; r_DI <- 5; r_CP <- 0.5


# --------------- Transition dynamics f(x, u, k) --------------------------
# k is 1-indexed (1..N)
dynamics <- function(x, u, k) {
  y <- x[1]; d <- x[2]; b <- x[3]; th <- x[4]; z <- x[5]
  S <- u[1]; Fdi <- u[2]; Fcp <- u[3]
  c(
    (rho_y + psi*S - eta_p*Fcp)*y - alpha_S*S +
      (alpha_F_CP + eta_tilde*S - eta_p*y)*Fcp + alpha_F_DI*z + beta_fear*d,
    delta_theta_t[k] * th,
    (1 + r_int)*b - gamma_y*y + kappa_F_CP*Fcp + kappa_F_DI*Fdi + c_H*th,
    rho_theta_t[k] * (1 - phi_S*S) * th + wave_shock_vec[k + 2],  # +2: k+1 in paper, +1 for R
    Fdi
  )
}


# --------------- Analytical Jacobians ------------------------------------
jacobians <- function(x, u, k) {
  y <- x[1]; d <- x[2]; b <- x[3]; th <- x[4]; z <- x[5]
  S <- u[1]; Fdi <- u[2]; Fcp <- u[3]

  A <- matrix(c(
    rho_y + psi*S - eta_p*Fcp, beta_fear, 0,      0,                           alpha_F_DI,
    0,                          0,         0,      delta_theta_t[k],            0,
    -gamma_y,                   0,         1+r_int, c_H,                        0,
    0,                          0,         0,      rho_theta_t[k]*(1-phi_S*S),  0,
    0,                          0,         0,      0,                           0
  ), nrow = 5, ncol = 5, byrow = TRUE)

  B <- matrix(c(
    psi*y - alpha_S + eta_tilde*Fcp,  0,          alpha_F_CP + eta_tilde*S - eta_p*y,
    0,                                 0,          0,
    0,                                 kappa_F_DI, kappa_F_CP,
    -rho_theta_t[k]*phi_S*th,         0,          0,
    0,                                 1,          0
  ), nrow = 5, ncol = 3, byrow = TRUE)

  list(A = A, B = B)
}


# --------------- Cost function (identical to al_cost_v6) -----------------
# x_traj: (n_x) x (N+2) matrix, columns 1..(N+1) used
# u_traj: (n_u) x (N+1) matrix, columns 1..N used (1-indexed)
al_cost <- function(x_traj, u_traj, Q_base, Q_N, R_mat,
                    lam, mu_mat, C_u, c_bounds) {
  J <- 0
  for (k in 1:N) {
    disc <- beta_disc^(k - 1)
    xk   <- x_traj[, k + 1]     # state at step k: column k+1 (k=1 → col 2)
    uk   <- u_traj[, k]
    sc   <- disc * (0.5 * as.numeric(t(xk) %*% Q_base %*% xk) +
                    0.5 * as.numeric(t(uk)  %*% R_mat  %*% uk))
    cv   <- C_u %*% uk - c_bounds
    act  <- (cv > 0) | (lam[, k] > 0)
    Im   <- diag(as.numeric(mu_mat[, k] * act))
    ac   <- as.numeric(t(lam[, k]) %*% cv) + 0.5 * as.numeric(t(cv) %*% Im %*% cv)
    J    <- J + sc + ac
  }
  xN1 <- x_traj[, N + 2]   # terminal state: column N+2
  J   <- J + beta_disc^N * 0.5 * as.numeric(t(xN1) %*% Q_N %*% xN1)
  J
}


# --------------- AL-iLQR solver ------------------------------------------
solve_ilqr <- function(w_y_, w_d_, w_b_, W_b_,
                       r_S_, r_DI_, r_CP_,
                       discount_R = TRUE) {
  Q_base <- diag(c(w_y_, w_d_, w_b_, 0, 0))
  Q_N    <- diag(c(0, 0, W_b_, 0, 0))
  R_mat  <- diag(c(r_S_, r_DI_, r_CP_))

  # Box constraints: C_u %*% u <= c_bounds
  C_u     <- rbind(-diag(3), diag(3))
  c_bounds <- c(0, 0, 0, S_max, FDI_max, FCP_max)
  n_c     <- 6L

  # x_traj: (n_x) x (N+2); column 1 unused, col 2 = x0, col k+2 = x_{k+1}
  # u_traj: (n_u) x (N+1); column k = u_k for k=1..N
  x_bar <- matrix(0, n_x, N + 2)
  u_bar <- matrix(0, n_u, N + 1)
  x_bar[, 2] <- x0
  for (k in 1:N) {
    base_S  <- max(0.1, 0.7 - 0.05*(k - 1))
    if (any(abs(k - wave_quarters) <= 1)) base_S <- min(S_max, base_S + 0.2)
    base_CP <- max(0, 0.06 - 0.005*(k - 1))
    u_bar[, k] <- c(base_S, 0.002, base_CP)
    x_bar[, k + 2] <- dynamics(x_bar[, k + 1], u_bar[, k], k)
  }

  lam    <- matrix(0,    n_c, N + 1)
  mu_mat <- matrix(10.0, n_c, N + 1)

  max_outer <- 30L; max_inner <- 60L
  tol_inner <- 1e-4; tol_outer <- 1e-4
  phi_pen   <- 2.5;  rho_base  <- 1e-4

  for (outer in 1:max_outer) {
    for (inner in 1:max_inner) {

      # Precompute Jacobians
      jac_list <- lapply(1:N, function(k) jacobians(x_bar[, k+1], u_bar[, k], k))

      # Backward pass
      P_store <- vector("list", N + 2)
      p_store <- vector("list", N + 2)
      P_store[[N + 2]] <- Q_N * beta_disc^N
      p_store[[N + 2]] <- (Q_N * beta_disc^N) %*% x_bar[, N + 2]

      K_store  <- vector("list", N + 1)
      kff_store <- vector("list", N + 1)

      rho_reg <- rho_base
      bw_ok   <- FALSE

      while (!bw_ok && rho_reg < 1e6) {
        bw_ok    <- TRUE
        K_tmp    <- vector("list", N + 1)
        kff_tmp  <- vector("list", N + 1)
        P_tmp    <- P_store
        p_tmp    <- p_store

        for (k in N:1) {
          disc <- beta_disc^(k - 1)
          Q_k  <- Q_base * disc
          R_k  <- if (discount_R) R_mat * disc else R_mat

          A    <- jac_list[[k]]$A
          B    <- jac_list[[k]]$B
          pk1  <- as.numeric(p_tmp[[k + 2]])
          Pk1  <- P_tmp[[k + 2]]

          uk   <- u_bar[, k]
          cv   <- C_u %*% uk - c_bounds
          act  <- (cv > 0) | (lam[, k] > 0)
          I_mu <- diag(as.numeric(mu_mat[, k] * act))

          Qx  <- Q_k  %*% x_bar[, k + 1] + t(A) %*% pk1
          Qxx <- Q_k  + t(A) %*% Pk1 %*% A
          Qux <- t(B) %*% Pk1 %*% A
          Qu  <- R_k  %*% uk  + t(B) %*% pk1 + t(C_u) %*% (lam[, k] + I_mu %*% cv)
          Quu <- R_k  + t(B) %*% Pk1 %*% B + t(C_u) %*% I_mu %*% C_u + rho_reg * diag(n_u)

          # Cholesky check
          ch <- tryCatch(chol(Quu), error = function(e) NULL)
          if (is.null(ch)) {
            bw_ok   <- FALSE
            rho_reg <- rho_reg * 10
            break
          }

          K_tmp[[k]]   <- solve(Quu, Qux)
          kff_tmp[[k]] <- -solve(Quu, as.numeric(Qu))
          p_tmp[[k + 1]] <- as.numeric(Qx) + t(K_tmp[[k]]) %*% Quu %*% kff_tmp[[k]]
          P_tmp[[k + 1]] <- Qxx - t(K_tmp[[k]]) %*% Quu %*% K_tmp[[k]]
        }

        if (bw_ok) {
          K_store   <- K_tmp
          kff_store  <- kff_tmp
          P_store   <- P_tmp
          p_store   <- p_tmp
        }
      }

      if (!bw_ok) break

      # Forward pass with line search
      cost_old <- al_cost(x_bar, u_bar, Q_base, Q_N, R_mat, lam, mu_mat, C_u, c_bounds)
      al       <- 1.0
      ls_ok    <- FALSE
      xn <- NULL; un <- NULL

      while (al > 1e-8) {
        xn <- matrix(0, n_x, N + 2)
        un <- matrix(0, n_u, N + 1)
        xn[, 2] <- x0
        for (k in 1:N) {
          dx       <- xn[, k + 1] - x_bar[, k + 1]
          un[, k]  <- u_bar[, k] + al * as.numeric(kff_store[[k]]) - K_store[[k]] %*% dx
          un[, k]  <- pmax(un[, k], u_min)
          un[, k]  <- pmin(un[, k], u_max)
          xn[, k + 2] <- dynamics(xn[, k + 1], un[, k], k)
        }
        cost_new <- al_cost(xn, un, Q_base, Q_N, R_mat, lam, mu_mat, C_u, c_bounds)
        if (cost_new < cost_old) { ls_ok <- TRUE; break }
        al <- al * 0.5
      }

      if (!ls_ok) break

      dx_norm <- norm(xn - x_bar, "F") / (norm(x_bar, "F") + 1e-12)
      x_bar <- xn; u_bar <- un
      if (dx_norm < tol_inner) break
    }

    # Update multipliers
    mv <- 0
    for (k in 1:N) {
      cv <- C_u %*% u_bar[, k] - c_bounds
      mv <- max(mv, max(cv))
      lam[, k] <- pmax(0, lam[, k] + mu_mat[, k] * as.numeric(cv))
      mu_mat[, k] <- mu_mat[, k] * phi_pen
    }
    if (mv < tol_outer) break
  }

  J_opt <- al_cost(x_bar, u_bar, Q_base, Q_N, R_mat,
                    matrix(0, n_c, N+1), matrix(0, n_c, N+1), C_u, c_bounds)
  list(x = x_bar, u = u_bar, J = J_opt)
}


# --------------- No-intervention benchmark --------------------------------
simulate_no_intervention <- function() {
  x_unc <- matrix(0, n_x, N + 2)
  x_unc[, 2] <- x0
  for (k in 1:N) {
    x_unc[1, k+2] <- rho_y*x_unc[1,k+1] + beta_fear*x_unc[2,k+1]
    x_unc[2, k+2] <- delta_theta_t[k] * x_unc[4, k+1]
    x_unc[3, k+2] <- (1+r_int)*x_unc[3,k+1] - gamma_y*x_unc[1,k+1] + c_H*x_unc[4,k+1]
    x_unc[4, k+2] <- rho_theta_t[k]*x_unc[4,k+1] + wave_shock_vec[k+2]
    x_unc[5, k+2] <- 0
  }
  x_unc
}


# --------------- Print helper ---------------------------------------------
print_results <- function(label, res) {
  x <- res$x; u <- res$u
  cat(sprintf("\n=== %s ===\n", label))
  cat(sprintf("  Avg output gap:       %.2f%%\n",   mean(x[1, 2:(N+1)])*100))
  cat(sprintf("  Cumul. mortality:     %.3f%%\n",   sum(x[2, 3:(N+2)])*100))
  cat(sprintf("  Terminal debt:        %.1f pp\n",  x[3, N+2]*100))
  cat(sprintf("  Avg S:                %.3f\n",     mean(u[1, 1:N])))
  cat(sprintf("  Avg CP (%%GDP):         %.2f%%\n",  mean(u[3, 1:N])*100))
  cat(sprintf("  Avg DI (%%GDP):         %.3f%%\n",  mean(u[2, 1:N])*100))
  cat(sprintf("  Total cost J:         %.4f\n",     res$J))
}


# =============================================================================
#  MAIN
# =============================================================================
cat("=============================================================\n")
cat("  PANDEMIC TRILEMMA V6 — R Cross-Check (VSL=60, MCPF=0.3)\n")
cat("=============================================================\n")

cat("\n[1/2] Corrected R (discount_R = TRUE)...\n")
res_cor <- solve_ilqr(w_y, w_d_baseline, w_b_baseline, W_b_baseline,
                      r_S, r_DI, r_CP, discount_R = TRUE)
print_results("R — Corrected (discount_R=TRUE)", res_cor)

cat("\n[2/2] MATLAB-equivalent R (discount_R = FALSE)...\n")
res_mat <- solve_ilqr(w_y, w_d_baseline, w_b_baseline, W_b_baseline,
                      r_S, r_DI, r_CP, discount_R = FALSE)
print_results("R — MATLAB-equivalent (discount_R=FALSE)", res_mat)

x_unc <- simulate_no_intervention()
cat(sprintf("\nNo-intervention: y=%.2f%%, d=%.3f%%, b=%.1fpp\n",
    mean(x_unc[1, 2:(N+1)])*100, sum(x_unc[2, 3:(N+2)])*100, x_unc[3, N+2]*100))

# ---- Comparison table ----
cat("\n=============================================================\n")
cat("  COMPARISON: Corrected vs MATLAB-equivalent\n")
cat("=============================================================\n")
cat(sprintf("  %-28s %12s %14s %10s\n", "Metric", "Corrected", "MATLAB-equiv", "Diff"))
cat("  ", strrep("-", 66), "\n", sep="")
metrics <- list(
  "Avg output gap (%)"   = c(mean(res_cor$x[1,2:(N+1)])*100, mean(res_mat$x[1,2:(N+1)])*100),
  "Cumul. mortality (%)" = c(sum(res_cor$x[2,3:(N+2)])*100,  sum(res_mat$x[2,3:(N+2)])*100),
  "Terminal debt (pp)"   = c(res_cor$x[3,N+2]*100,           res_mat$x[3,N+2]*100),
  "Avg S"                = c(mean(res_cor$u[1,1:N]),          mean(res_mat$u[1,1:N])),
  "Avg CP (% GDP)"       = c(mean(res_cor$u[3,1:N])*100,      mean(res_mat$u[3,1:N])*100),
  "Avg DI (% GDP)"       = c(mean(res_mat$u[2,1:N])*100,      mean(res_mat$u[2,1:N])*100),
  "Total cost J"         = c(res_cor$J,                        res_mat$J)
)
for (nm in names(metrics)) {
  v <- metrics[[nm]]
  cat(sprintf("  %-28s %12.4f %14.4f %10.4f\n", nm, v[1], v[2], v[1] - v[2]))
}

cat("\n  Quarter-by-quarter control paths (MATLAB-equivalent):\n")
cat(sprintf("  %3s %7s %7s | %7s %7s | %7s %7s\n",
    "k", "S_cor", "S_mat", "CP_cor", "CP_mat", "DI_cor", "DI_mat"))
cat("  ", strrep("-", 55), "\n", sep="")
for (k in 1:N) {
  cat(sprintf("  %3d %7.3f %7.3f | %7.3f %7.3f | %7.4f %7.4f\n",
      k,
      res_cor$u[1, k], res_mat$u[1, k],
      res_cor$u[3, k]*100, res_mat$u[3, k]*100,
      res_cor$u[2, k]*100, res_mat$u[2, k]*100))
}

# ---- VSL sensitivity ----
cat("\n=============================================================\n")
cat("  VSL SENSITIVITY (R corrected, discount_R=TRUE)\n")
cat("=============================================================\n")
VSL_grid <- c(20, 40, 60, 80, 100, 140)
cat(sprintf("  %6s | %9s | %9s | %10s | %6s | %7s | %7s\n",
    "VSL", "Avg y(%)", "Cum d(%)", "End b(pp)", "Avg S", "Avg CP", "Avg DI"))
cat("  ", strrep("-", 72), "\n", sep="")
for (vsl in VSL_grid) {
  w_d_v <- w_y * vsl * (y_scale / d_scale)^2
  rv    <- solve_ilqr(w_y, w_d_v, w_b_baseline, W_b_baseline,
                      r_S, r_DI, r_CP, discount_R = TRUE)
  cat(sprintf("  %6d | %9.2f | %9.3f | %10.1f | %6.2f | %7.2f | %7.3f\n",
      vsl,
      mean(rv$x[1, 2:(N+1)])*100, sum(rv$x[2, 3:(N+2)])*100,
      rv$x[3, N+2]*100, mean(rv$u[1, 1:N]),
      mean(rv$u[3, 1:N])*100, mean(rv$u[2, 1:N])*100))
}

cat("\nDone.\n")
