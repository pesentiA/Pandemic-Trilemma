library(readxl)
library(writexl)

# Re-read from the ORIGINAL file
df <- read_excel("C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_6.xlsx")
suppressWarnings({})

cat("Original dimensions:", nrow(df), "x", ncol(df), "\n")
cat("Original channel distribution (broad_fiscal 1/2/3):\n")
sub <- df[df$broad_fiscal %in% c(1, 2, 3), ]
print(table(sub$transmission_channel, useNA = "always"))

# Helper: apply fix by ID + Country (to avoid duplicate ID issues)
fix <- function(df, id, country, channel = NULL, code = NULL, categ = NULL) {
  idx <- which(df$ID == id & df$Country == country)
  if (length(idx) == 0) {
    cat("WARNING: ID+Country not found:", id, country, "\n")
    return(df)
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
  cat(sprintf("  Fixed %s (%s): %s\n", id, country, paste(changes, collapse = ", ")))
  return(df)
}

# ===== ISSUE 1: Code 30 rows that are tourism VAT cuts -> Code 26, keep DI =====
cat("\n--- Issue 1: Code 30 -> Code 26 (tourism VAT, not medical) ---\n")
df <- fix(df, "CovidID-30285", "IRL", code = "26", categ = 3)
df <- fix(df, "CovidID-30726", "PRT", code = "26", categ = 3)

# ===== ISSUE 2: Code 32 rows that are NOT medical supply =====
cat("\n--- Issue 2: Code 32 -> correct codes (not medical supply) ---\n")
df <- fix(df, "CovidID-30272", "IRL", code = "27", categ = 4)                    # training investment -> keep DI
df <- fix(df, "CovidID-30282", "IRL", channel = "CP", code = "47", categ = 8)    # tourism adaptation fund
df <- fix(df, "CovidID-30284", "IRL", channel = "CP", code = "45", categ = 8)    # business closure support
df <- fix(df, "CovidID-41391", "POL", code = "40", categ = 7)                    # interest subsidies -> keep CP
df <- fix(df, "CovidID-30710", "HUN", code = "25", categ = 3)                    # tourism levy exemption -> keep DI
df <- fix(df, "CovidID-99836", "GBR", channel = "CP", code = "47", categ = 8)    # Omicron hospitality grant

# ===== ISSUE 3: Code 33, DI -> H =====
cat("\n--- Issue 3: Code 33, DI -> H (health infrastructure) ---\n")
df <- fix(df, "CovidID-9860", "BEL", channel = "H")

# ===== ISSUE 4: Code 34 -> Code 38, keep DI =====
cat("\n--- Issue 4: Code 34 -> Code 38 (essential worker top-up) ---\n")
df <- fix(df, "CovidID-4274", "CAN", code = "38", categ = 6)

# ===== ISSUE 5: Code 36 classified as CP =====
cat("\n--- Issue 5: Code 36 fixes ---\n")
# These should be DI per protocol (Code 36 = expansion of unemployment benefits)
df <- fix(df, "CovidID-1059",  "CHL", channel = "DI")
df <- fix(df, "CovidID-9160",  "FIN", channel = "DI")
df <- fix(df, "CovidID-2033",  "ISR", channel = "DI")
df <- fix(df, "CovidID-7295",  "TUR", channel = "DI")

# These are actually short-time work schemes -> recode to 39, keep CP
df <- fix(df, "CovidID-10144", "SVN", code = "39", categ = 6)
df <- fix(df, "CovidID-2316",  "CHE", code = "39", categ = 6)
df <- fix(df, "CovidID-1297",  "TUR", code = "39", categ = 6)

# ===== ISSUE 6: Code 37 classified as CP =====
cat("\n--- Issue 6: Code 37 fixes ---\n")
# These should be DI per protocol (Code 37 = temporary expansion of existing benefits)
df <- fix(df, "CovidID-10096", "IRL", channel = "DI")
df <- fix(df, "CovidID-9537",  "PRT", channel = "DI")
df <- fix(df, "CovidID-9780",  "GBR", channel = "DI")
df <- fix(df, "CovidID-30655", "CZE", channel = "DI")

# These are actually other CP mechanisms -> recode PolicyCode
df <- fix(df, "CovidID-6868",  "COL", code = "39", categ = 6)     # employer-side premium
df <- fix(df, "CovidID-30135", "DEU", code = "39", categ = 6)     # Kurzarbeitergeld
df <- fix(df, "CovidID-13706", "LUX", code = "6",  categ = 1)     # social security deferral
df <- fix(df, "CovidID-13720", "LUX", code = "45", categ = 8)     # micro-enterprise support

# ===== ISSUE 7: EIB Guarantee Fund (NA PolicyCode) -> Code 43, keep CP =====
cat("\n--- Issue 7: EIB Guarantee Fund -> Code 43 ---\n")
eib <- data.frame(
  id = c("CovidID-9369","CovidID-9370","CovidID-9357","CovidID-9379",
         "CovidID-9376","CovidID-9359","CovidID-9361"),
  country = c("HUN","IRL","ITA","LVA","LTU","NLD","POL"),
  stringsAsFactors = FALSE
)
for (r in 1:nrow(eib)) {
  df <- fix(df, eib$id[r], eib$country[r], code = "43", categ = 7)
}

# ===== VERIFICATION =====
cat("\n\n========== VERIFICATION ==========\n")
sub2 <- df[df$broad_fiscal %in% c(1, 2, 3), ]
cat("New channel distribution (broad_fiscal 1/2/3):\n")
print(table(sub2$transmission_channel, useNA = "always"))

cat("\nCross-tab PolicyCode x transmission_channel:\n")
xt <- table(sub2$PolicyCode, sub2$transmission_channel, useNA = "always")
print(xt)

# Check protocol compliance
cat("\n--- Checking protocol compliance ---\n")
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
mismatch_details <- list()
for (i in 1:nrow(sub2)) {
  pc <- as.character(sub2$PolicyCode[i])
  ch <- sub2$transmission_channel[i]
  if (!is.na(pc) && pc != "other" && pc %in% names(expected)) {
    exp_ch <- expected[[pc]]
    if (!is.na(ch) && ch != exp_ch) {
      mismatches <- mismatches + 1
      if (mismatches <= 20) {
        cat(sprintf("  MISMATCH: ID=%s, Country=%s, Code=%s, channel=%s (expected %s)\n    desc: %s\n",
            sub2$ID[i], sub2$Country[i], pc, ch, exp_ch,
            substr(as.character(sub2$description[i]), 1, 150)))
      }
    }
  }
}
cat(sprintf("\nTotal remaining mismatches: %d\n", mismatches))

# Save as v1_7
outpath <- "C:/Users/pesent0000/OneDrive/Studium/Wirtschaftswissenschaften/Doktorat/Paper 1/Pandemic-Trilemma/Files/data/raw/fiscal measures/fiscal_classified_v1_7.xlsx"
write_xlsx(df, outpath)
cat("\nSaved to:", outpath, "\n")
