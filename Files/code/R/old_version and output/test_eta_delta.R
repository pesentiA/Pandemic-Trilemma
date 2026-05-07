library(readxl); library(dplyr); library(plm); library(lmtest); library(sandwich)
library(tidyr); library(stringr); library(lubridate); library(conflicted)
conflicted::conflict_prefer("select","dplyr"); conflicted::conflict_prefer("filter","dplyr")
conflicted::conflicts_prefer(dplyr::lag); conflicted::conflicts_prefer(dplyr::lead)
conflicted::conflicts_prefer(lubridate::quarter); conflicted::conflicts_prefer(lubridate::year)

safedata <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/processed"
load(file.path(safedata, "dataforanalysis.RData"))
set.seed(1234)

fm_path <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"
pandemic_qs <- c("Q1.2019","Q2.2019","Q3.2019","Q4.2019",
  "Q1.2020","Q2.2020","Q3.2020","Q4.2020",
  "Q1.2021","Q2.2021","Q3.2021","Q4.2021",
  "Q1.2022","Q2.2022","Q3.2022","Q4.2022")

fm1 <- readxl::read_excel(fm_path) %>%
  mutate(YQ = paste0("Q", Quarter, ".", Year),
         YQ_ord = factor(YQ, levels = sort(unique(paste0("Q", Quarter, ".", Year))), ordered = TRUE)) %>%
  filter(!PolicyCode %in% c(5, 6, 11, 12, 15, 16))

df_qdata <- qdata %>% filter(Quarter %in% pandemic_qs) %>%
  select(Country, Quarter, year_only, quarter_only, y_t_pct, DebtR_share2019,
         Qpopulation_th, inflation_index, vax_rate, rGDP_pc_2019, debt_2019, d_t_pct)
pop_2019 <- df_qdata[df_qdata$Quarter=="Q4.2019", c("Country","Qpopulation_th")]
names(pop_2019)[2] <- "pop_2019"
df_qdata <- merge(df_qdata, pop_2019, by="Country")

df_theta <- theta_quarterly_full %>% mutate(Quarter = as.character(YQ)) %>%
  filter(Quarter %in% pandemic_qs) %>% select(Country, Quarter, theta_mean)

df_fiscal <- fm1 %>% filter(broad_fiscal == 1) %>%
  mutate(Quarter = as.character(YQ_ord)) %>% filter(Quarter %in% pandemic_qs) %>%
  group_by(Country, Quarter) %>%
  summarise(F_CP = sum(broad_fiscal_gdp[transmission_channel=="CP"], na.rm=TRUE),
            F_DI = sum(broad_fiscal_gdp[transmission_channel=="DI"], na.rm=TRUE),
            .groups = "drop")

df_stringency <- panel_w %>% filter(!is.na(S_mean)) %>%
  mutate(year=year(date), quarter=quarter(date), Quarter=paste0("Q",quarter,".",year)) %>%
  filter(Quarter %in% pandemic_qs) %>% group_by(Country, Quarter) %>%
  summarise(S_mean_pw = mean(S_mean, na.rm=TRUE), .groups="drop")

df <- df_qdata %>%
  left_join(df_theta, by=c("Country","Quarter")) %>%
  left_join(df_fiscal, by=c("Country","Quarter")) %>%
  left_join(df_stringency, by=c("Country","Quarter")) %>%
  mutate(Quarter = factor(Quarter, levels=pandemic_qs, ordered=TRUE),
         F_CP = replace_na(F_CP,0), F_DI = replace_na(F_DI,0),
         S_mean_pw = replace_na(S_mean_pw,0)) %>%
  arrange(Country, Quarter)

pdata <- df %>%
  mutate(S = S_mean_pw*100, F_CP = F_CP*100, F_DI = F_DI*100,
         theta_pct = theta_mean*100) %>%
  mutate(F_DI_lag2 = lag(F_DI,2), y_lag1 = lag(y_t_pct,1))

pdataY <- pdata %>%
  filter(Quarter %in% c("Q1.2020","Q2.2020","Q3.2020","Q4.2020",
                         "Q1.2021","Q2.2021","Q3.2021","Q4.2021","Q1.2022"))

# Construct all interaction terms explicitly
pdataY <- pdataY %>%
  mutate(
    S_ylag     = S * y_lag1,
    FCP_ylag   = F_CP * y_lag1,
    S_FCP      = S * F_CP,
    S_FCP_ylag = S * F_CP * y_lag1
  )

cat(strrep("=", 70), "\n")
cat("  TESTING eta vs delta: CHANNEL SEPARATION\n")
cat(strrep("=", 70), "\n\n")

# ==============================================================================
# MODEL 0: Current specification (S*F_CP only)
# ==============================================================================
cat("--- MODEL 0: Current spec (S*y_lag + S*F_CP + F_DI_lag2) ---\n\n")

m0 <- plm(y_t_pct ~ S + y_lag1 + F_CP + S_ylag + S_FCP + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct0 <- coeftest(m0, vcov = vcovHC(m0, cluster = "group", type = "HC1"))
print(ct0)

# ==============================================================================
# MODEL 1: FULL SPEC — both channels separated
# Theory: F_CP*y_lag = -eta (NEGATIVE), S*F_CP = +delta (POSITIVE)
# ==============================================================================
cat("\n\n--- MODEL 1: FULL SPEC (persistence + cushioning separated) ---\n")
cat("    F_CP*y_lag = -eta (expected: NEGATIVE)\n")
cat("    S*F_CP     = +delta (expected: POSITIVE)\n\n")

m1 <- plm(y_t_pct ~ S + y_lag1 + F_CP + S_ylag + FCP_ylag + S_FCP + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct1 <- coeftest(m1, vcov = vcovHC(m1, cluster = "group", type = "HC1"))
print(ct1)

# ==============================================================================
# MODEL 2: Persistence channel only (F_CP * y_lag, no S*F_CP)
# ==============================================================================
cat("\n\n--- MODEL 2: PERSISTENCE ONLY (F_CP*y_lag, no S*F_CP) ---\n\n")

m2 <- plm(y_t_pct ~ S + y_lag1 + F_CP + S_ylag + FCP_ylag + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct2 <- coeftest(m2, vcov = vcovHC(m2, cluster = "group", type = "HC1"))
print(ct2)

# ==============================================================================
# MODEL 3: Effective stringency with three-way interaction
# S_eff = S * (1 - lambda * F_CP), so:
# y = rho*y_lag + psi*S*y_lag - psi*lambda*S*F_CP*y_lag
#     - alpha_S*S + alpha_S*lambda*S*F_CP + alpha_DI*F_DI
# ==============================================================================
cat("\n\n--- MODEL 3: EFFECTIVE STRINGENCY (includes S*F_CP*y_lag) ---\n")
cat("    If lambda > 0: S*F_CP > 0 AND S*F_CP*y_lag < 0\n\n")

m3 <- plm(y_t_pct ~ S + y_lag1 + F_CP + S_ylag + S_FCP + S_FCP_ylag + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct3 <- coeftest(m3, vcov = vcovHC(m3, cluster = "group", type = "HC1"))
print(ct3)

# ==============================================================================
# MODEL 4: KITCHEN SINK — all channels
# ==============================================================================
cat("\n\n--- MODEL 4: KITCHEN SINK (all interaction terms) ---\n\n")

m4 <- plm(y_t_pct ~ S + y_lag1 + F_CP + S_ylag + FCP_ylag + S_FCP + S_FCP_ylag + F_DI_lag2,
           data = pdataY, model = "within", effect = "twoways")
ct4 <- coeftest(m4, vcov = vcovHC(m4, cluster = "group", type = "HC1"))
print(ct4)

# ==============================================================================
# COMPARISON TABLE
# ==============================================================================
cat("\n\n")
cat(strrep("=", 80), "\n")
cat("  COMPARISON TABLE\n")
cat(strrep("=", 80), "\n\n")

get_str <- function(ct, v) {
  idx <- which(rownames(ct) == v)
  if (length(idx) == 0) return("          ---   ")
  est <- ct[idx, 1]; pv <- ct[idx, 4]
  stars <- ifelse(pv<0.001,"***",ifelse(pv<0.01,"** ",ifelse(pv<0.05,"*  ",ifelse(pv<0.1,".  ","   "))))
  sprintf("%9.5f%s", est, stars)
}

vars <- c("S", "y_lag1", "F_CP", "S_ylag", "FCP_ylag", "S_FCP", "S_FCP_ylag", "F_DI_lag2")
labels <- c("S (-alpha_S)", "y_lag1 (rho_y)", "F_CP (level)", "S*y_lag (psi)",
            "F_CP*y_lag (-eta)", "S*F_CP (delta)", "S*F_CP*y_lag", "F_DI_lag2 (alpha_DI)")

cat(sprintf("%-22s", ""))
cat(sprintf("  %15s  %15s  %15s  %15s  %15s\n",
            "M0:current", "M1:full", "M2:persist", "M3:S_eff", "M4:kitchen"))
cat(strrep("-", 22 + 5*17), "\n")

cts <- list(ct0, ct1, ct2, ct3, ct4)
for (i in seq_along(vars)) {
  cat(sprintf("%-22s", labels[i]))
  for (ct in cts) cat(sprintf("  %15s", get_str(ct, vars[i])))
  cat("\n")
}

# Adj R2
cat(sprintf("\n%-22s", "Adj R2"))
for (m in list(m0, m1, m2, m3, m4)) {
  cat(sprintf("  %15.4f", summary(m)$r.squared["adjrsq"]))
}
cat("\n")

# Correlation matrix
cat("\n\n--- Correlation matrix of interaction terms ---\n")
cor_data <- pdataY %>%
  select(S_ylag, FCP_ylag, S_FCP, S_FCP_ylag) %>%
  filter(complete.cases(.))
colnames(cor_data) <- c("S*y_lag", "FCP*y_lag", "S*FCP", "S*FCP*y_lag")
print(round(cor(cor_data), 3))

# Marginal effect evaluation
cat("\n\n")
cat(strrep("=", 70), "\n")
cat("  MARGINAL EFFECT OF F_CP AT SAMPLE MEANS\n")
cat(strrep("=", 70), "\n\n")

S_bar <- mean(pdataY$S, na.rm=TRUE)
y_bar <- mean(pdataY$y_lag1, na.rm=TRUE)
cat(sprintf("  Sample mean S = %.1f,  mean y_lag1 = %.2f\n\n", S_bar, y_bar))

# Model 0: dY/dF_CP = alpha_CP + delta * S
if ("F_CP" %in% rownames(ct0) && "S_FCP" %in% rownames(ct0)) {
  me0 <- ct0["F_CP", 1] + ct0["S_FCP", 1] * S_bar
  cat(sprintf("  M0 (current):  dY/dF_CP = %.4f + (%.5f)*%.1f = %.4f\n",
              ct0["F_CP",1], ct0["S_FCP",1], S_bar, me0))
}

# Model 1: dY/dF_CP = alpha_CP + (-eta)*y_lag + delta*S
if ("F_CP" %in% rownames(ct1) && "FCP_ylag" %in% rownames(ct1) && "S_FCP" %in% rownames(ct1)) {
  me1 <- ct1["F_CP",1] + ct1["FCP_ylag",1]*y_bar + ct1["S_FCP",1]*S_bar
  cat(sprintf("  M1 (full):     dY/dF_CP = %.4f + (%.5f)*(%.2f) + (%.5f)*%.1f = %.4f\n",
              ct1["F_CP",1], ct1["FCP_ylag",1], y_bar, ct1["S_FCP",1], S_bar, me1))
  cat(sprintf("    of which: persistence channel = %.4f, cushioning channel = %.4f\n",
              ct1["FCP_ylag",1]*y_bar, ct1["S_FCP",1]*S_bar))
}

# Model 2: dY/dF_CP = alpha_CP + (-eta)*y_lag
if ("F_CP" %in% rownames(ct2) && "FCP_ylag" %in% rownames(ct2)) {
  me2 <- ct2["F_CP",1] + ct2["FCP_ylag",1]*y_bar
  cat(sprintf("  M2 (persist):  dY/dF_CP = %.4f + (%.5f)*(%.2f) = %.4f\n",
              ct2["F_CP",1], ct2["FCP_ylag",1], y_bar, me2))
}

cat("\n========== DONE ==========\n")
