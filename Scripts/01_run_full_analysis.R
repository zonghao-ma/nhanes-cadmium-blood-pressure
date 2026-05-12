############################################################
# Project: NHANES Cadmium and Blood Pressure
# Script: 02_import_nhanes.R
# Purpose:
#   1. Import NHANES 2017-2018 data
#   2. Clean analytic variables
#   3. Create survey design
#   4. Export:
#      - data_clean/analysis_df.rds
#      - data_clean/analysis_df.csv
#      - outputs/tables/Table1_weighted.docx
#      - outputs/tables/Table2_weighted_clean.docx
#      - outputs/tables/Table2_weighted_clean.csv
#      - outputs/tables/Table3_sensitivity_clean.docx
#      - outputs/tables/Table3_sensitivity_clean.csv
#      - data_clean/model_objects.rds
############################################################

# ==========================================================
# 0. Clean environment
# ==========================================================

rm(list = ls())

# ==========================================================
# 1. Install and load packages
# ==========================================================

required_packages <- c(
  "tidyverse",
  "haven",
  "survey",
  "srvyr",
  "gtsummary",
  "flextable",
  "officer",
  "broom"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

library(tidyverse)
library(haven)
library(survey)
library(srvyr)
library(gtsummary)
library(flextable)
library(officer)
library(broom)

# ==========================================================
# 2. Create output folders
# ==========================================================

dir.create("data_clean", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# 3. Import raw NHANES files
# ==========================================================

demo <- read_xpt("data_raw/DEMO_J.XPT")
bpx  <- read_xpt("data_raw/BPX_J.XPT")
um   <- read_xpt("data_raw/UM_J.XPT")
bmx  <- read_xpt("data_raw/BMX_J.XPT")
smq  <- read_xpt("data_raw/SMQ_J.XPT")
cot  <- read_xpt("data_raw/COT_J.XPT")
ucr  <- read_xpt("data_raw/ALB_CR_J.XPT")

# ==========================================================
# 4. Select and clean variables
# ==========================================================

# ------------------------------
# Demographics
# ------------------------------

demo2 <- demo %>%
  select(
    SEQN,
    RIDAGEYR,
    RIAGENDR,
    RIDRETH3,
    INDFMPIR,
    DMDEDUC2,
    WTMEC2YR,
    SDMVPSU,
    SDMVSTRA
  ) %>%
  mutate(
    sex = factor(
      RIAGENDR,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    race_ethnicity = factor(
      RIDRETH3,
      levels = c(1, 2, 3, 4, 6, 7),
      labels = c(
        "Mexican American",
        "Other Hispanic",
        "Non-Hispanic White",
        "Non-Hispanic Black",
        "Non-Hispanic Asian",
        "Other Race"
      )
    ),
    education = case_when(
      DMDEDUC2 %in% c(1, 2) ~ "<High school",
      DMDEDUC2 == 3 ~ "High school",
      DMDEDUC2 == 4 ~ "Some college",
      DMDEDUC2 == 5 ~ "College+",
      TRUE ~ NA_character_
    ),
    education = factor(
      education,
      levels = c("<High school", "High school", "Some college", "College+")
    )
  )

# ------------------------------
# Blood pressure
# ------------------------------

bpx2 <- bpx %>%
  select(
    SEQN,
    BPXSY1, BPXSY2, BPXSY3,
    BPXDI1, BPXDI2, BPXDI3
  ) %>%
  mutate(
    SBP = rowMeans(
      across(c(BPXSY1, BPXSY2, BPXSY3)),
      na.rm = TRUE
    ),
    DBP = rowMeans(
      across(c(BPXDI1, BPXDI2, BPXDI3)),
      na.rm = TRUE
    ),
    SBP = if_else(is.nan(SBP), NA_real_, SBP),
    DBP = if_else(is.nan(DBP), NA_real_, DBP)
  ) %>%
  select(SEQN, SBP, DBP)

# ------------------------------
# Urinary cadmium
# ------------------------------

um2 <- um %>%
  select(SEQN, URXUCD) %>%
  rename(cadmium = URXUCD) %>%
  mutate(
    log2_cadmium = if_else(
      cadmium > 0,
      log2(cadmium),
      NA_real_
    )
  )

# ------------------------------
# BMI
# ------------------------------

bmx2 <- bmx %>%
  select(SEQN, BMXBMI) %>%
  rename(BMI = BMXBMI)

# ------------------------------
# Smoking questionnaire
# ------------------------------

smq2 <- smq %>%
  select(SEQN, SMQ020) %>%
  mutate(
    smoker = case_when(
      SMQ020 == 1 ~ "Ever smoker",
      SMQ020 == 2 ~ "Never smoker",
      TRUE ~ NA_character_
    ),
    smoker = factor(
      smoker,
      levels = c("Never smoker", "Ever smoker")
    )
  ) %>%
  select(SEQN, smoker)

# ------------------------------
# Serum cotinine
# ------------------------------

cot2 <- cot %>%
  select(SEQN, LBXCOT) %>%
  rename(cotinine = LBXCOT) %>%
  mutate(
    log_cotinine = if_else(
      cotinine >= 0,
      log(cotinine + 0.01),
      NA_real_
    )
  )

# ------------------------------
# Urinary creatinine
# ------------------------------

ucr2 <- ucr %>%
  select(SEQN, URXUCR) %>%
  rename(urine_creatinine = URXUCR)

# ==========================================================
# 5. Merge datasets
# ==========================================================

nhanes <- demo2 %>%
  left_join(bpx2, by = "SEQN") %>%
  left_join(um2, by = "SEQN") %>%
  left_join(bmx2, by = "SEQN") %>%
  left_join(smq2, by = "SEQN") %>%
  left_join(cot2, by = "SEQN") %>%
  left_join(ucr2, by = "SEQN") %>%
  filter(RIDAGEYR >= 20)

# ==========================================================
# 6. Create cadmium quartiles
# ==========================================================

nhanes <- nhanes %>%
  mutate(
    cadmium_q = ntile(cadmium, 4),
    cadmium_q = factor(
      cadmium_q,
      levels = c(1, 2, 3, 4),
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )

# ==========================================================
# 7. Create analysis dataset
# ==========================================================

analysis_df <- nhanes %>%
  drop_na(
    SBP,
    DBP,
    cadmium,
    log2_cadmium,
    cadmium_q,
    RIDAGEYR,
    sex,
    race_ethnicity,
    INDFMPIR,
    education,
    BMI,
    smoker,
    cotinine,
    log_cotinine,
    urine_creatinine,
    WTMEC2YR,
    SDMVPSU,
    SDMVSTRA
  )

# Save clean analytic dataset
saveRDS(analysis_df, "data_clean/analysis_df.rds")
write_csv(analysis_df, "data_clean/analysis_df.csv")

# Quick sample size check
cat("Final analytic sample size:", nrow(analysis_df), "\n")

# ==========================================================
# 8. Define NHANES survey design
# ==========================================================

options(survey.lonely.psu = "adjust")

nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = analysis_df,
  nest = TRUE
)

# ==========================================================
# 9. Table 1: Weighted baseline characteristics
# ==========================================================

table1_weighted <- nhanes_design %>%
  tbl_svysummary(
    include = c(
      RIDAGEYR,
      sex,
      race_ethnicity,
      INDFMPIR,
      education,
      BMI,
      smoker,
      cotinine,
      urine_creatinine,
      cadmium,
      SBP,
      DBP
    ),
    label = list(
      RIDAGEYR ~ "Age, years",
      sex ~ "Sex",
      race_ethnicity ~ "Race/ethnicity",
      INDFMPIR ~ "Family income-to-poverty ratio",
      education ~ "Education",
      BMI ~ "Body mass index, kg/m²",
      smoker ~ "Smoking status",
      cotinine ~ "Serum cotinine",
      urine_creatinine ~ "Urinary creatinine",
      cadmium ~ "Urinary cadmium, μg/L",
      SBP ~ "Systolic blood pressure, mmHg",
      DBP ~ "Diastolic blood pressure, mmHg"
    ),
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Table 1. Survey-weighted baseline characteristics of NHANES 2017–2018 adults**")

save_as_docx(
  "Table 1. Survey-weighted baseline characteristics" =
    as_flex_table(table1_weighted),
  path = "outputs/tables/Table1_weighted.docx"
)

# ==========================================================
# 10. Primary survey-weighted regression models
# ==========================================================

model_svy1 <- svyglm(
  SBP ~ log2_cadmium,
  design = nhanes_design
)

model_svy2 <- svyglm(
  SBP ~ log2_cadmium +
    RIDAGEYR +
    sex +
    race_ethnicity,
  design = nhanes_design
)

model_svy3 <- svyglm(
  SBP ~ log2_cadmium +
    RIDAGEYR +
    sex +
    race_ethnicity +
    INDFMPIR +
    education +
    BMI +
    smoker +
    log_cotinine +
    urine_creatinine,
  design = nhanes_design
)

# ==========================================================
# 10B. Helper functions for manuscript-style tables
# ==========================================================

get_svy_tcrit <- function(model) {
  
  df_resid <- tryCatch(
    degf(model$survey.design),
    error = function(e) NA_real_
  )
  
  if (is.na(df_resid) || df_resid <= 0) {
    return(1.96)
  }
  
  t_crit <- qt(0.975, df = df_resid)
  
  if (is.na(t_crit) || is.nan(t_crit) || is.infinite(t_crit)) {
    return(1.96)
  }
  
  return(t_crit)
}

get_lm_tcrit <- function(model) {
  
  df_resid <- df.residual(model)
  
  if (is.na(df_resid) || df_resid <= 0) {
    return(1.96)
  }
  
  t_crit <- qt(0.975, df = df_resid)
  
  if (is.na(t_crit) || is.nan(t_crit) || is.infinite(t_crit)) {
    return(1.96)
  }
  
  return(t_crit)
}

calc_p_normal <- function(estimate, se) {
  z_value <- estimate / se
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  return(p_value)
}

format_p <- function(p) {
  p <- as.numeric(p)
  
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

format_ci <- function(low, high) {
  paste0(
    sprintf("%.2f", low),
    ", ",
    sprintf("%.2f", high)
  )
}

extract_svy_result <- function(model, model_name, term_name = "log2_cadmium") {
  
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$term <- rownames(coef_table)
  
  t_crit <- get_svy_tcrit(model)
  
  result <- coef_table %>%
    filter(term == term_name) %>%
    transmute(
      Model = model_name,
      Exposure = "Log2 urinary cadmium",
      Beta_raw = Estimate,
      SE = `Std. Error`,
      p_raw = if_else(
        is.na(`Pr(>|t|)`) | is.nan(`Pr(>|t|)`),
        calc_p_normal(Estimate, `Std. Error`),
        `Pr(>|t|)`
      )
    ) %>%
    mutate(
      CI_low = Beta_raw - t_crit * SE,
      CI_high = Beta_raw + t_crit * SE,
      Beta = round(Beta_raw, 2),
      `95% CI` = format_ci(CI_low, CI_high),
      `p-value` = format_p(p_raw)
    ) %>%
    select(Model, Exposure, Beta, `95% CI`, `p-value`)
  
  return(result)
}

extract_svy_terms <- function(model, analysis_name, terms_keep) {
  
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$term <- rownames(coef_table)
  
  t_crit <- get_svy_tcrit(model)
  
  coef_table %>%
    filter(term %in% terms_keep) %>%
    mutate(
      Exposure = case_when(
        term == "log2_cadmium" ~ "Log2 urinary cadmium",
        term == "cadmium_qQ2" ~ "Cadmium quartile Q2 vs Q1",
        term == "cadmium_qQ3" ~ "Cadmium quartile Q3 vs Q1",
        term == "cadmium_qQ4" ~ "Cadmium quartile Q4 vs Q1",
        TRUE ~ term
      ),
      CI_low = Estimate - t_crit * `Std. Error`,
      CI_high = Estimate + t_crit * `Std. Error`,
      Beta = round(Estimate, 2),
      p_raw = if_else(
        is.na(`Pr(>|t|)`) | is.nan(`Pr(>|t|)`),
        calc_p_normal(Estimate, `Std. Error`),
        `Pr(>|t|)`
      ),
      `95% CI` = format_ci(CI_low, CI_high),
      `p-value` = format_p(p_raw)
    ) %>%
    transmute(
      `Sensitivity analysis` = analysis_name,
      Exposure,
      Beta,
      `95% CI`,
      `p-value`
    )
}

extract_lm_terms <- function(model, analysis_name, terms_keep) {
  
  coef_table <- as.data.frame(summary(model)$coefficients)
  coef_table$term <- rownames(coef_table)
  
  t_crit <- get_lm_tcrit(model)
  
  coef_table %>%
    filter(term %in% terms_keep) %>%
    mutate(
      Exposure = case_when(
        term == "log2_cadmium" ~ "Log2 urinary cadmium",
        TRUE ~ term
      ),
      CI_low = Estimate - t_crit * `Std. Error`,
      CI_high = Estimate + t_crit * `Std. Error`,
      Beta = round(Estimate, 2),
      p_raw = `Pr(>|t|)`,
      `95% CI` = format_ci(CI_low, CI_high),
      `p-value` = format_p(p_raw)
    ) %>%
    transmute(
      `Sensitivity analysis` = analysis_name,
      Exposure,
      Beta,
      `95% CI`,
      `p-value`
    )
}

# ==========================================================
# 10C. Clean manuscript-style Table 2
# ==========================================================

table2_clean_df <- bind_rows(
  extract_svy_result(model_svy1, "Model 1: Unadjusted"),
  extract_svy_result(model_svy2, "Model 2: Demographic-adjusted"),
  extract_svy_result(model_svy3, "Model 3: Fully adjusted")
)

table2_clean_ft <- table2_clean_df %>%
  flextable() %>%
  autofit() %>%
  set_caption("Table 2. Survey-weighted association between urinary cadmium and systolic blood pressure")

save_as_docx(
  "Table 2. Survey-weighted association between urinary cadmium and SBP" =
    table2_clean_ft,
  path = "outputs/tables/Table2_weighted_clean.docx"
)

# ==========================================================
# 11. Sensitivity analysis models
# ==========================================================

model_svy_dbp <- svyglm(
  DBP ~ log2_cadmium +
    RIDAGEYR +
    sex +
    race_ethnicity +
    INDFMPIR +
    education +
    BMI +
    smoker +
    log_cotinine +
    urine_creatinine,
  design = nhanes_design
)

model_svy_quartile <- svyglm(
  SBP ~ cadmium_q +
    RIDAGEYR +
    sex +
    race_ethnicity +
    INDFMPIR +
    education +
    BMI +
    smoker +
    log_cotinine +
    urine_creatinine,
  design = nhanes_design
)

model_unweighted_full <- lm(
  SBP ~ log2_cadmium +
    RIDAGEYR +
    sex +
    race_ethnicity +
    INDFMPIR +
    education +
    BMI +
    smoker +
    log_cotinine +
    urine_creatinine,
  data = analysis_df
)

# ==========================================================
# 11B. Clean manuscript-style Table 3
# ==========================================================

table3_clean_df <- bind_rows(
  extract_svy_terms(
    model_svy_dbp,
    "DBP outcome, weighted",
    "log2_cadmium"
  ),
  extract_svy_terms(
    model_svy_quartile,
    "Cadmium quartiles, weighted",
    c("cadmium_qQ2", "cadmium_qQ3", "cadmium_qQ4")
  ),
  extract_lm_terms(
    model_unweighted_full,
    "SBP outcome, unweighted",
    "log2_cadmium"
  )
)

table3_clean_ft <- table3_clean_df %>%
  flextable() %>%
  autofit() %>%
  set_caption("Table 3. Sensitivity analyses")

save_as_docx(
  "Table 3. Sensitivity analyses" =
    table3_clean_ft,
  path = "outputs/tables/Table3_sensitivity_clean.docx"
)

# ==========================================================
# 12. Export model summaries and clean result tables
# ==========================================================

saveRDS(
  list(
    model_svy1 = model_svy1,
    model_svy2 = model_svy2,
    model_svy3 = model_svy3,
    model_svy_dbp = model_svy_dbp,
    model_svy_quartile = model_svy_quartile,
    model_unweighted_full = model_unweighted_full
  ),
  "data_clean/model_objects.rds"
)

write_csv(
  table2_clean_df,
  "outputs/tables/Table2_weighted_clean.csv"
)

write_csv(
  table3_clean_df,
  "outputs/tables/Table3_sensitivity_clean.csv"
)

# ==========================================================
# 13. Print clean results in console
# ==========================================================

cat("\nTable 2 clean results:\n")
print(table2_clean_df)

cat("\nTable 3 clean results:\n")
print(table3_clean_df)

cat("\nSurvey design degrees of freedom:\n")
print(degf(nhanes_design))

# ==========================================================
# 13B. Figures for manuscript-style output
# ==========================================================

library(ggplot2)

# ------------------------------
# Figure 1. Distribution of urinary cadmium
# ------------------------------

fig1_cadmium_distribution <- analysis_df %>%
  ggplot(aes(x = cadmium)) +
  geom_histogram(
    bins = 40,
    fill = "#4E79A7",
    color = "white"
  ) +
  scale_x_log10() +
  labs(
    title = "Distribution of urinary cadmium",
    x = "Urinary cadmium, μg/L (log10 scale)",
    y = "Number of participants"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "outputs/figures/Figure1_cadmium_distribution.png",
  plot = fig1_cadmium_distribution,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/Figure1_cadmium_distribution.pdf",
  plot = fig1_cadmium_distribution,
  width = 7,
  height = 5
)

# ------------------------------
# Figure 2. SBP by cadmium quartiles
# ------------------------------

fig2_sbp_by_cadmium_quartile <- analysis_df %>%
  ggplot(aes(x = cadmium_q, y = SBP)) +
  geom_boxplot(
    fill = "#59A14F",
    color = "gray30",
    outlier.alpha = 0.25
  ) +
  labs(
    title = "Systolic blood pressure by urinary cadmium quartile",
    x = "Urinary cadmium quartile",
    y = "Systolic blood pressure, mmHg"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = "outputs/figures/Figure2_sbp_by_cadmium_quartile.png",
  plot = fig2_sbp_by_cadmium_quartile,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/Figure2_sbp_by_cadmium_quartile.pdf",
  plot = fig2_sbp_by_cadmium_quartile,
  width = 7,
  height = 5
)

# ------------------------------
# Figure 3. Clean forest plot
# ------------------------------

forest_df <- bind_rows(
  table2_clean_df %>%
    mutate(
      Analysis = case_when(
        str_detect(Model, "Unadjusted") ~ "Model 1: Unadjusted",
        str_detect(Model, "Demographic") ~ "Model 2: Demographic-adjusted",
        str_detect(Model, "Fully") ~ "Model 3: Fully adjusted",
        TRUE ~ Model
      ),
      Source = "Primary analysis"
    ) %>%
    select(Analysis, Exposure, Beta, `95% CI`, `p-value`, Source),
  
  table3_clean_df %>%
    mutate(
      Analysis = case_when(
        str_detect(`Sensitivity analysis`, "DBP") ~ "DBP outcome",
        str_detect(Exposure, "Q2") ~ "Cadmium Q2 vs Q1",
        str_detect(Exposure, "Q3") ~ "Cadmium Q3 vs Q1",
        str_detect(Exposure, "Q4") ~ "Cadmium Q4 vs Q1",
        str_detect(`Sensitivity analysis`, "unweighted") ~ "SBP unweighted",
        TRUE ~ `Sensitivity analysis`
      ),
      Source = "Sensitivity analysis"
    ) %>%
    select(Analysis, Exposure, Beta, `95% CI`, `p-value`, Source)
) %>%
  mutate(
    CI_low = as.numeric(str_extract(`95% CI`, "^-?\\d+\\.\\d+")),
    CI_high = as.numeric(str_extract(`95% CI`, "-?\\d+\\.\\d+$")),
    Plot_label = case_when(
      Analysis == "Model 1: Unadjusted" ~ "Model 1: Unadjusted",
      Analysis == "Model 2: Demographic-adjusted" ~ "Model 2: Demographic-adjusted",
      Analysis == "Model 3: Fully adjusted" ~ "Model 3: Fully adjusted",
      Analysis == "DBP outcome" ~ "DBP outcome",
      Analysis == "Cadmium Q2 vs Q1" ~ "Q2 vs Q1",
      Analysis == "Cadmium Q3 vs Q1" ~ "Q3 vs Q1",
      Analysis == "Cadmium Q4 vs Q1" ~ "Q4 vs Q1",
      Analysis == "SBP unweighted" ~ "SBP unweighted",
      TRUE ~ Analysis
    ),
    Plot_label = factor(
      Plot_label,
      levels = rev(c(
        "Model 1: Unadjusted",
        "Model 2: Demographic-adjusted",
        "Model 3: Fully adjusted",
        "DBP outcome",
        "Q2 vs Q1",
        "Q3 vs Q1",
        "Q4 vs Q1",
        "SBP unweighted"
      ))
    )
  ) %>%
  filter(!is.na(Plot_label))

fig3_forest_plot_clean <- forest_df %>%
  ggplot(aes(x = Beta, y = Plot_label)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high),
    orientation = "y",
    width = 0.18,
    linewidth = 0.8,
    color = "gray35"
  ) +
  geom_point(
    aes(color = Source),
    size = 3.5
  ) +
  scale_color_manual(
    values = c(
      "Primary analysis" = "#E15759",
      "Sensitivity analysis" = "#4E79A7"
    )
  ) +
  labs(
    title = "Association between urinary cadmium and blood pressure",
    subtitle = "Regression estimates with 95% confidence intervals",
    x = "Beta estimate, mmHg",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "outputs/figures/Figure3_forest_plot_clean.png",
  plot = fig3_forest_plot_clean,
  width = 8,
  height = 5.5,
  dpi = 300
)

ggsave(
  filename = "outputs/figures/Figure3_forest_plot_clean.pdf",
  plot = fig3_forest_plot_clean,
  width = 8,
  height = 5.5
)

cat("\nFigures exported:\n")
cat("- outputs/figures/Figure1_cadmium_distribution.png\n")
cat("- outputs/figures/Figure1_cadmium_distribution.pdf\n")
cat("- outputs/figures/Figure2_sbp_by_cadmium_quartile.png\n")
cat("- outputs/figures/Figure2_sbp_by_cadmium_quartile.pdf\n")
cat("- outputs/figures/Figure3_forest_plot.png\n")
cat("- outputs/figures/Figure3_forest_plot.pdf\n")

# ==========================================================
# 14. Final completion message
# ==========================================================

cat("\nAnalysis completed successfully.\n")
cat("Files exported:\n")
cat("- data_clean/analysis_df.rds\n")
cat("- data_clean/analysis_df.csv\n")
cat("- outputs/tables/Table1_weighted.docx\n")
cat("- outputs/tables/Table2_weighted_clean.docx\n")
cat("- outputs/tables/Table2_weighted_clean.csv\n")
cat("- outputs/tables/Table3_sensitivity_clean.docx\n")
cat("- outputs/tables/Table3_sensitivity_clean.csv\n")
cat("- data_clean/model_objects.rds\n")

library(survey)

analysis_df <- readRDS("data_clean/analysis_df.rds")

options(survey.lonely.psu = "adjust")

nhanes_design <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WTMEC2YR,
  data = analysis_df,
  nest = TRUE
)

svyquantile(
  ~RIDAGEYR + cadmium + SBP + DBP,
  design = nhanes_design,
  quantiles = c(0.25, 0.5, 0.75),
  na.rm = TRUE
)
