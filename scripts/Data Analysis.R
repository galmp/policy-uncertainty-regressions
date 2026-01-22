# ======================================
# STEP 0: Clean session + sanity checks
# ======================================
rm(list = ls())
cat("\014")  # clears console

getwd()
list.files()
# ======================================
# STEP 1: Install & load required packages
# ======================================
pkgs <- c("tidyverse", "zoo", "lmtest", "sandwich")

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if(length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))
# ======================================
# STEP 2: Confirm packages loaded correctly
# ======================================
library(tidyverse)
library(zoo)
library(lmtest)
library(sandwich)

sessionInfo()
# ======================================
# STEP 3: Load CPU and UCT datasets
# ======================================

CPU <- read.csv("CPU index.csv")
UCT <- read.csv("UCT.csv")

# Validation checks
dim(CPU)
dim(UCT)

head(CPU, 3)
head(UCT, 3)
# ======================================
# STEP 4: Load Ken French datasets (guaranteed method)
# ======================================

library(data.table)

# Read files safely
Portfolios_raw <- fread("10_Industry_Portfolios.txt", skip = 11)
Fama_raw       <- fread("F-F_Research_Data_Factors.txt", skip = 3)

# Keep only numeric data (remove footer rows)
Portfolios <- Portfolios_raw[grepl("^[0-9]{6}", Portfolios_raw[[1]]), ]
Fama <- Fama_raw[grepl("^[0-9]{6}", Fama_raw[[1]]), ]

# Convert to data.frame
Portfolios <- as.data.frame(Portfolios)
Fama <- as.data.frame(Fama)

# Validation checks
dim(Portfolios)
dim(Fama)

head(Portfolios, 3)
head(Fama, 3)
# ======================================
# STEP 5: Ensure numeric format
# ======================================

# Convert everything except the first column (date) to numeric
Portfolios[,-1] <- lapply(Portfolios[,-1], as.numeric)
Fama[,-1] <- lapply(Fama[,-1], as.numeric)

# Quick check
str(Portfolios)
str(Fama)
# ======================================
# STEP 6: Convert to time series objects
# ======================================

# CPU and UCT
CPU.ts <- ts(CPU[,2], start = c(1987,4), frequency = 12)
UCT.ts <- ts(UCT[,2], start = c(1993,1), frequency = 12)

# Portfolios and Fama-French factors
Port.ts <- ts(Portfolios[,-1], start = c(1926,7), frequency = 12)
Fama.ts <- ts(Fama[,-1], start = c(1926,7), frequency = 12)

# Quick sanity check
start(CPU.ts); end(CPU.ts)
start(UCT.ts); end(UCT.ts)

start(Port.ts); end(Port.ts)
start(Fama.ts); end(Fama.ts)
# ======================================
# STEP 7: Align samples (t predicts t+1)
# ======================================

# For CPU sample
Port_CPU <- window(Port.ts, start = c(1987,5), end = c(2025,10))
Fama_CPU <- window(Fama.ts, start = c(1987,5), end = c(2025,10))

# For UCT sample
Port_UCT <- window(Port.ts, start = c(1993,2), end = c(2024,3))
Fama_UCT <- window(Fama.ts, start = c(1993,2), end = c(2024,3))

# Sanity checks
dim(Port_CPU)
dim(Fama_CPU)

dim(Port_UCT)
dim(Fama_UCT)
# ======================================
# STEP 8: Excess returns
# ======================================

Excess_CPU <- Port_CPU - Fama_CPU[,4]
Excess_UCT <- Port_UCT - Fama_UCT[,4]

head(Excess_CPU)
head(Excess_UCT)
# ============================================================
# FINAL BULLETPROOF PIPELINE (handles all naming + alignment)
# ============================================================

library(zoo)
library(dplyr)

# ----------------------------
# 1) Build a robust "date" index (monthly) from existing ts
# ----------------------------
cpu_df <- data.frame(date = as.yearmon(time(CPU.ts)), cpu = as.numeric(CPU.ts))
uct_df <- data.frame(date = as.yearmon(time(UCT.ts)), uct = as.numeric(UCT.ts))

# ----------------------------
# 2) Clean Ken French Portfolios + Factors (robust column handling)
#    Portfolios and Fama already exist as data.frames from your fread cleaning
#    - Portfolios: first column is YYYYMM (often called V1)
#    - Fama: first column is YYYYMM (often called V1)
# ----------------------------
stopifnot(exists("Portfolios"), exists("Fama"))

# Helper: convert YYYYMM -> yearmon safely
yyyymm_to_yearmon <- function(x) {
  x <- as.character(x)
  # keep only first 6 digits if something weird appears
  x <- substr(x, 1, 6)
  as.yearmon(x, format = "%Y%m")
}

# Portfolio df
port_df <- Portfolios %>%
  mutate(date = yyyymm_to_yearmon(.[[1]])) %>%
  select(-1) %>%
  relocate(date)

# Factor df
fama_df <- Fama %>%
  mutate(date = yyyymm_to_yearmon(.[[1]])) %>%
  select(-1) %>%
  relocate(date)

# Standardize column names (turn '-' to '.', remove spaces)
names(fama_df) <- names(fama_df) %>%
  gsub("-", ".", .) %>%
  gsub("\\s+", "", .)

# ----------------------------
# 3) Detect factor columns robustly (works with Mkt-RF, Mkt.RF, etc.)
# ----------------------------
nms <- names(fama_df)

find_one <- function(pattern_vec) {
  idx <- rep(TRUE, length(nms))
  for(p in pattern_vec) idx <- idx & grepl(p, nms, ignore.case = TRUE)
  hits <- which(idx)
  if(length(hits) == 0) return(NA_character_)
  nms[hits[1]]
}

RF_col  <- find_one(c("^RF$"))              # exact RF preferred
if(is.na(RF_col)) RF_col <- find_one(c("RF"))  # fallback

MKT_col <- find_one(c("Mkt", "RF"))         # Mkt.*RF (excess market)
SMB_col <- find_one(c("^SMB$"))
if(is.na(SMB_col)) SMB_col <- find_one(c("SMB"))
HML_col <- find_one(c("^HML$"))
if(is.na(HML_col)) HML_col <- find_one(c("HML"))

# Hard stop with clear message if something is missing
missing <- c(RF_col, MKT_col, SMB_col, HML_col)
if(any(is.na(missing))) {
  cat("\nERROR: Could not detect all factor columns.\n")
  cat("Detected names:\n")
  cat("RF =", RF_col, "\n")
  cat("MKT =", MKT_col, "\n")
  cat("SMB =", SMB_col, "\n")
  cat("HML =", HML_col, "\n\n")
  cat("Available columns in fama_df:\n")
  print(names(fama_df))
  stop("Fix: Your factor file format is unusual. Send me the printed names(fama_df).")
}

# Rename to standard names so the rest is stable
fama_df <- fama_df %>%
  rename(RF = all_of(RF_col),
         MKT = all_of(MKT_col),
         SMB = all_of(SMB_col),
         HML = all_of(HML_col))

# ----------------------------
# 4) Merge Portfolios with Factors, compute excess returns
# ----------------------------
base_df <- merge(port_df, fama_df[, c("date","RF","MKT","SMB","HML")], by = "date")

industry_cols <- setdiff(names(port_df), "date")

# Compute excess returns: Ri - RF (vectorized)
for(col in industry_cols) {
  base_df[[col]] <- base_df[[col]] - base_df$RF
}

# ----------------------------
# 5) Create t -> t+1 structure (lead returns by 1 month)
#    uncertainty at date t explains excess return at t+1
# ----------------------------
for(col in industry_cols) {
  base_df[[paste0(col, "_lead")]] <- dplyr::lead(base_df[[col]], 1)
}

# Keep only rows where lead exists
base_df <- base_df %>% filter(!is.na(.data[[paste0(industry_cols[1], "_lead")]]))

lead_cols <- paste0(industry_cols, "_lead")

# ----------------------------
# 6) Build CPU and UCT regression datasets (merged on date)
# ----------------------------
cpu_data <- merge(base_df, cpu_df, by = "date")
uct_data <- merge(base_df, uct_df, by = "date")

# ----------------------------
# 7) Regression runner (robust, no ts mismatch)
# ----------------------------
run_regs <- function(df, unc_name, controls = FALSE) {
  results <- data.frame()
  for(i in seq_along(industry_cols)) {
    yname <- lead_cols[i]
    if(!controls) {
      model <- lm(df[[yname]] ~ df[[unc_name]])
    } else {
      model <- lm(df[[yname]] ~ df[[unc_name]] + df$MKT + df$SMB + df$HML)
    }
    beta <- coef(summary(model))[2,1]
    pval <- coef(summary(model))[2,4]
    results <- rbind(results,
                     data.frame(Industry = industry_cols[i],
                                Beta = round(beta, 6),
                                P_value = round(pval, 4)))
  }
  results
}

# ----------------------------
# 8) Run regressions and print
# ----------------------------
CPU_simple  <- run_regs(cpu_data, "cpu", controls = FALSE)
CPU_control <- run_regs(cpu_data, "cpu", controls = TRUE)

UCT_simple  <- run_regs(uct_data, "uct", controls = FALSE)
UCT_control <- run_regs(uct_data, "uct", controls = TRUE)

cat("\n===== CPU (simple) =====\n");  print(CPU_simple)
cat("\n===== CPU (controls) =====\n"); print(CPU_control)
cat("\n===== UCT (simple) =====\n");  print(UCT_simple)
cat("\n===== UCT (controls) =====\n"); print(UCT_control)

cat("\n===== Significant (p < 0.10) with controls =====\n")
cat("\nCPU:\n"); print(CPU_control %>% filter(P_value < 0.10))
cat("\nUCT:\n"); print(UCT_control %>% filter(P_value < 0.10))
# ============================
# STEP 9: Publication-ready tables + exports (NO errors)
# Produces:
#   results/results.csv
#   results/results_control.csv
#   results/CPU_table.csv, results/UCT_table.csv
#   results/regression_tables.docx  (Word-ready)
# ============================

# Packages (minimal & stable for Word tables)
pkgs_out <- c("dplyr", "readr", "flextable", "officer")
to_install_out <- pkgs_out[!pkgs_out %in% installed.packages()[, "Package"]]
if (length(to_install_out) > 0) install.packages(to_install_out)
invisible(lapply(pkgs_out, library, character.only = TRUE))

# Create results folder
if (!dir.exists("results")) dir.create("results")

# ---------- vectorized significance stars ----------
sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

fmt_beta <- function(beta, p) {
  sprintf("%.4f%s", beta, sig_stars(p))
}

fmt_pval <- function(p) {
  sprintf("(%.4f)", p)
}

# ---------- Build publication table: No controls vs FF3 controls ----------
build_pub_table <- function(simple_df, control_df, title_label) {
  s <- simple_df %>%
    mutate(beta_txt = fmt_beta(Beta, P_value),
           p_txt    = fmt_pval(P_value)) %>%
    select(Industry, beta_txt, p_txt) %>%
    rename(beta_s = beta_txt, p_s = p_txt)
  
  c <- control_df %>%
    mutate(beta_txt = fmt_beta(Beta, P_value),
           p_txt    = fmt_pval(P_value)) %>%
    select(Industry, beta_txt, p_txt) %>%
    rename(beta_c = beta_txt, p_c = p_txt)
  
  out <- dplyr::left_join(s, c, by = "Industry") %>%
    mutate(
      `No controls`   = paste0(beta_s, "  ", p_s),
      `FF3 controls`  = paste0(beta_c, "  ", p_c)
    ) %>%
    select(Industry, `No controls`, `FF3 controls`)
  
  attr(out, "title") <- title_label
  out
}

CPU_table <- build_pub_table(CPU_simple, CPU_control, "CPU (Climate Policy Uncertainty)")
UCT_table <- build_pub_table(UCT_simple, UCT_control, "UCT (US–China Tension)")

# ---------- Save the raw regression outputs exactly as requested ----------
# results.csv = simple specs (CPU + UCT)
results_simple_all <- dplyr::bind_rows(
  CPU_simple %>% mutate(Uncertainty = "CPU"),
  UCT_simple %>% mutate(Uncertainty = "UCT")
) %>% select(Uncertainty, Industry, Beta, P_value)

# results_control.csv = FF3 controls specs (CPU + UCT)
results_control_all <- dplyr::bind_rows(
  CPU_control %>% mutate(Uncertainty = "CPU"),
  UCT_control %>% mutate(Uncertainty = "UCT")
) %>% select(Uncertainty, Industry, Beta, P_value)

readr::write_csv(results_simple_all,  file.path("results", "results.csv"))
readr::write_csv(results_control_all, file.path("results", "results_control.csv"))

# Also save the nice publication tables as CSV
readr::write_csv(CPU_table, file.path("results", "CPU_table.csv"))
readr::write_csv(UCT_table, file.path("results", "UCT_table.csv"))

# ---------- Export Word-ready tables (single professional docx) ----------
doc <- officer::read_docx()

add_table_to_doc <- function(doc, df) {
  title <- attr(df, "title")
  ft <- flextable::flextable(df)
  ft <- flextable::autofit(ft)
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, align = "left", part = "all")
  
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- officer::body_add_flextable(doc, ft)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc
}

doc <- add_table_to_doc(doc, CPU_table)
doc <- add_table_to_doc(doc, UCT_table)

print(doc, target = file.path("results", "regression_tables.docx"))

# ---------- Console confirmation ----------
cat("\n✅ Exports completed. Files created in /results:\n")
cat(" - results.csv\n")
cat(" - results_control.csv\n")
cat(" - CPU_table.csv\n")
cat(" - UCT_table.csv\n")
cat(" - regression_tables.docx (Word-ready)\n")
library(modelsummary)

# ============================
# BUILD PROPER REGRESSION MODELS (not summaries)
# ============================

models_CPU_controls <- list()
models_CPU_simple   <- list()

for(i in seq_along(industry_cols)) {
  
  yname <- lead_cols[i]
  ind   <- industry_cols[i]
  
  # Simple
  models_CPU_simple[[ind]] <- 
    lm(cpu_data[[yname]] ~ cpu_data$cpu)
  
  # FF3 controls
  models_CPU_controls[[ind]] <- 
    lm(cpu_data[[yname]] ~ cpu_data$cpu + cpu_data$MKT + cpu_data$SMB + cpu_data$HML)
}

# ============================
# EXPORT BEAUTIFUL WORD TABLE
# ============================

modelsummary(
  models_CPU_controls,
  stars = TRUE,
  statistic = "({std.error})",
  output = "results/CPU_table_paper.docx",
  title = "Regressions of Industry Excess Returns on CPU (FF3 controls)"
)

cat("\n✔ Table created: results/CPU_table_paper.docx\n")
# ===============================
# FILE PATHS (robust)
# ===============================
data_dir <- "data"
if (!dir.exists(data_dir)) data_dir <- "."  # fallback if data folder not used

CPU <- read.csv(file.path(data_dir, "CPU index.csv"))
UCT <- read.csv(file.path(data_dir, "UCT.csv"))

library(data.table)
Portfolios_raw <- fread(file.path(data_dir, "10_Industry_Portfolios.txt"), skip = 11)
Fama_raw       <- fread(file.path(data_dir, "F-F_Research_Data_Factors.txt"), skip = 3)
# ===============================
# STEP 10: VISUALIZATIONS (PNG OUTPUT - FINAL WORKING VERSION)
# ===============================
library(ggplot2)
library(dplyr)

# Ensure results folder exists
if (!dir.exists("results")) dir.create("results")

# Safety checks
stopifnot(exists("CPU_control"), exists("UCT_control"))
stopifnot(nrow(CPU_control) > 0, nrow(UCT_control) > 0)

# -------------------------------
# Volcano plot
# -------------------------------
plot_volcano <- function(df, title, filename) {
  
  df <- df %>% mutate(sig = P_value < 0.10)
  
  p <- ggplot(df, aes(x = Beta, y = -log10(P_value))) +
    geom_point(aes(shape = sig), size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = title,
      x = "Beta",
      y = "-log10(p-value)"
    ) +
    theme_minimal(base_size = 12)
  
  out_path <- file.path("results", filename)
  ggsave(out_path, plot = p, width = 8, height = 5, dpi = 300)
  
  cat("✅ Saved:", out_path, "| exists =", file.exists(out_path), "\n")
}

# -------------------------------
# Bar plot (betas)
# -------------------------------
plot_betas <- function(df, title, filename) {
  
  df <- df %>% mutate(sig = P_value < 0.10)
  
  p <- ggplot(df, aes(x = reorder(Industry, Beta), y = Beta)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title,
      x = "Industry",
      y = "Beta"
    ) +
    theme_minimal(base_size = 12)
  
  out_path <- file.path("results", filename)
  ggsave(out_path, plot = p, width = 8, height = 5, dpi = 300)
  
  cat("✅ Saved:", out_path, "| exists =", file.exists(out_path), "\n")
}

# -------------------------------
# Create all PNGs
# -------------------------------
plot_betas(CPU_control, "CPU Betas Across Industries (FF3 controls)", "CPU_betas.png")
plot_betas(UCT_control, "UCT Betas Across Industries (FF3 controls)", "UCT_betas.png")

plot_volcano(CPU_control, "CPU Significance Across Industries (FF3 controls)", "CPU_volcano.png")
plot_volcano(UCT_control, "UCT Significance Across Industries (FF3 controls)", "UCT_volcano.png")

cat("\n✅ DONE. Files in results/:\n")
print(list.files("results"))
