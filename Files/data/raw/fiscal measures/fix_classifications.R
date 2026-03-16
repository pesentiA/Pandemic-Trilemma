library(readxl)
library(writexl)

df <- read_excel("C:/Users/pesent0000/fiscal.xlsx")
suppressWarnings({})

cat("Original dimensions:", nrow(df), "x", ncol(df), "\n")
cat("Original channel distribution (broad_fiscal 1/2/3):\n")
sub <- df[df$broad_fiscal %in% c(1, 2, 3), ]
print(table(sub$transmission_channel, useNA = "always"))

# Helper: apply fix by ID
fix <- function(df, id, channel = NULL, code = NULL, categ = NULL) {
  idx <- which(df$ID == id)
  if (length(idx) == 0) {
    cat("WARNING: ID not found:", id, "\n")
    return(df)
  }
  if (length(idx) > 1) {
    cat("WARNING: Multiple rows for ID:", id, "- fixing all\n")
  }
  old_ch <- df$transmission_channel[idx[1]]
  old_code <- df$PolicyCode[idx[1]]
  changes <- c()
  if (!is.null(channel)) {
    df$transmission_channel[idx] <- channel
    changes <- c(changes, paste0("channel: ", old_ch, "->", channel))
  }
  if (!is.null(code)) {
    df$PolicyCode[idx] <- as.character(code)
    changes <- c(changes, paste0("PolicyCode: ", old_code, "->", code))
  }
  if (!is.null(categ)) {
    df$PolicyCategory[idx] <- categ
    changes <- c(changes, paste0("PolicyCategory->", categ))
  }
  cat(sprintf("  Fixed %s (%s): %s\n", id, df$Country[idx[1]], paste(changes, collapse = ", ")))
  return(df)
}

# ===== ISSUE 1: Code 30 rows that are tourism VAT cuts -> Code 26, keep DI =====
cat("\n--- Issue 1: Code 30 -> Code 26 (tourism VAT, not medical) ---\n")
df <- fix(df, "CovidID-30285", code = "26", categ = 3)   # IRL tourism VAT
df <- fix(df, "CovidID-30726", code = "26", categ = 3)   # PRT IVAucher

# ===== ISSUE 2: Code 32 rows that are NOT medical supply =====
cat("\n--- Issue 2: Code 32 -> correct codes (not medical supply) ---\n")
df <- fix(df, "CovidID-30272", code = "27", categ = 4)                    # IRL training investment -> DI (keep)
df <- fix(df, "CovidID-30282", channel = "CP", code = "47", categ = 8)    # IRL tourism adaptation fund -> CP
df <- fix(df, "CovidID-30284", channel = "CP", code = "45", categ = 8)    # IRL business closure support -> CP
df <- fix(df, "CovidID-41391", code = "40", categ = 7)                    # POL interest subsidies -> CP (keep)
df <- fix(df, "CovidID-30710", code = "25", categ = 3)                    # HUN tourism levy exemption -> DI (keep)
df <- fix(df, "CovidID-99836", channel = "CP", code = "47", categ = 8)    # GBR Omicron hospitality grant -> CP

# ===== ISSUE 3: Code 33, DI -> H =====
cat("\n--- Issue 3: Code 33, DI -> H (health infrastructure) ---\n")
df <- fix(df, "CovidID-9860", channel = "H")   # BEL health crisis management

# ===== ISSUE 4: Code 34 -> Code 38, keep DI =====
cat("\n--- Issue 4: Code 34 -> Code 38 (essential worker top-up, not health HR) ---\n")
df <- fix(df, "CovidID-4274", code = "38", categ = 6)   # CAN essential worker top-up

# ===== ISSUE 5: Code 36 classified as CP =====
cat("\n--- Issue 5: Code 36 fixes ---\n")
# These should be DI per protocol (Code 36 = expansion of unemployment benefits)
df <- fix(df, "CovidID-1059", channel = "DI")    # CHL Emergency Family Income -> DI
df <- fix(df, "CovidID-9160", channel = "DI")    # FIN unemployment benefit expansion -> DI
df <- fix(df, "CovidID-2033", channel = "DI")    # ISR unemployment benefits -> DI
df <- fix(df, "CovidID-7295", channel = "DI")    # TUR cash wage support for workers on unpaid leave -> DI

# These are actually short-time work schemes -> recode to 39, keep CP
df <- fix(df, "CovidID-10144", code = "39", categ = 6)   # SVN partial reimbursement for leave -> CP
df <- fix(df, "CovidID-2316", code = "39", categ = 6)    # CHE Kurzarbeit expansion -> CP
df <- fix(df, "CovidID-1297", code = "39", categ = 6)    # TUR minimum wage support/labor incentives -> CP

# ===== ISSUE 6: Code 37 classified as CP =====
cat("\n--- Issue 6: Code 37 fixes ---\n")
# These should be DI per protocol (Code 37 = temporary expansion of existing benefits)
df <- fix(df, "CovidID-10096", channel = "DI")    # IRL income support/sick pay -> DI
df <- fix(df, "CovidID-9537", channel = "DI")     # PRT parental income compensation -> DI
df <- fix(df, "CovidID-9780", channel = "DI")     # GBR Universal Credit increase -> DI
df <- fix(df, "CovidID-30655", channel = "DI")    # CZE quarantine allowance -> DI

# These are actually other types of CP -> recode PolicyCode
df <- fix(df, "CovidID-6868", code = "39", categ = 6)     # COL employer-side premium support -> CP
df <- fix(df, "CovidID-30135", code = "39", categ = 6)    # DEU Kurzarbeitergeld extension -> CP
df <- fix(df, "CovidID-13706", code = "6", categ = 1)     # LUX social security contribution deferral -> CP
df <- fix(df, "CovidID-13720", code = "45", categ = 8)    # LUX micro-enterprise support -> CP

# ===== ISSUE 7: EIB Guarantee Fund (NA PolicyCode) -> Code 43, keep CP =====
cat("\n--- Issue 7: EIB Guarantee Fund -> Code 43 ---\n")
eib_ids <- c("CovidID-9369", "CovidID-9370", "CovidID-9357", "CovidID-9379",
             "CovidID-9376", "CovidID-9359", "CovidID-9361")
for (id in eib_ids) {
  df <- fix(df, id, code = "43", categ = 7)
}

# ===== VERIFICATION =====
cat("\n\n========== VERIFICATION ==========\n")
sub2 <- df[df$broad_fiscal %in% c(1, 2, 3), ]
cat("New channel distribution (broad_fiscal 1/2/3):\n")
print(table(sub2$transmission_channel, useNA = "always"))

cat("\nNew PolicyCode x transmission_channel cross-tab:\n")
print(table(sub2$PolicyCode, sub2$transmission_channel, useNA = "always"))

# Check: any remaining mismatches with protocol?
cat("\n--- Checking protocol compliance ---\n")
# Define expected channel per PolicyCode
expected <- list(
  "1"="CP","2"="CP","3"="CP","4"="CP","5"="CP","6"="CP","7"="CP","8"="CP",
  "9"="CP","10"="CP","11"="CP","12"="CP","13"="CP","14"="CP",
  "15"="CP","16"="CP",
  "17"="DI","18"="DI","19"="DI","20"="DI","21"="DI","22"="DI",
  "23"="H","24"="H",
  "25"="DI","26"="DI",
  "27"="DI","28"="DI","29"="DI",
  "30"="H","31"="H","32"="H","33"="H","34"="H","general"="H",
  "35"="DI","36"="DI","37"="DI","38"="DI",
  "39"="CP",
  "40"="CP","41"="CP","42"="DI","43"="CP",
  "45"="CP","46"="CP","47"="CP","48"="CP",
  "49"="CP"
)

mismatches <- 0
for (i in 1:nrow(sub2)) {
  pc <- as.character(sub2$PolicyCode[i])
  ch <- sub2$transmission_channel[i]
  if (!is.na(pc) && pc != "other" && pc %in% names(expected)) {
    exp_ch <- expected[[pc]]
    if (!is.na(ch) && ch != exp_ch) {
      mismatches <- mismatches + 1
      if (mismatches <= 20) {
        cat(sprintf("  MISMATCH row %d: ID=%s, Country=%s, Code=%s, channel=%s (expected %s)\n",
            i, sub2$ID[i], sub2$Country[i], pc, ch, exp_ch))
      }
    }
  }
}
cat(sprintf("\nTotal remaining mismatches: %d\n", mismatches))

# Save
outpath <- "C:/Users/pesent0000/fiscal.xlsx"
write_xlsx(df, outpath)
cat("\nSaved to:", outpath, "\n")
