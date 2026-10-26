# ============================================================================
# OLDER ADULTS' ADOPTION OF A MAAS MOBILE APPLICATION
# Reproducible analysis for Journal of Transport & Health
# ============================================================================
# INPUT:  data.csv  (place in the same folder as this script)
# OUTPUT: results/  (created automatically)
# TO RUN: In RStudio, open this folder and run source("analysis.R")
# ============================================================================

# ============================================================================
# 1. LOAD PACKAGES AND IMPORT DATA
# ============================================================================
required_packages <- c("cSEM", "openxlsx")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                             logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install these packages first: install.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
library(cSEM)
library(openxlsx)

if (!file.exists("data.csv")) {
  stop("Cannot find data.csv. Place it alongside analysis.R and set this folder as the working directory.")
}
data <- read.csv("data.csv", check.names = FALSE)
dir.create("results", showWarnings = FALSE)
set.seed(123)

# ============================================================================
# 2. PREPARE SURVEY VARIABLES (REVERSE-CODED ITEMS AND MODEL)
# ============================================================================
# For five-point items, reverse-scored value = 6 - original value.
reverse_items <- c(ATT4 = "ATT4_R", RA4 = "RA4_R", OBS4 = "OBS4_R",
                   COM4 = "COM4_R", CPX4 = "CPX4_R", PEOU4 = "PEOU4_R",
                   PU4 = "PU4_R", BI4 = "BI4_R")
for (item in names(reverse_items)) {
  original <- reverse_items[[item]]
  if (original %in% names(data)) {
    data[[item]] <- 6 - as.numeric(data[[original]])
  }
}

# Measurement and structural model specification.
model_TAM <- "
  BI   ~ ATT + AGE
  ATT  ~ PEOU + PU
  PEOU ~ RA + COM + CPX + OBS + TRI
  PU   ~ RA + COM + CPX + OBS + TRI + PEOU

  ATT  =~ ATT2 + ATT3 + ATT4
  PEOU =~ PEOU1 + PEOU2 + PEOU4
  PU   =~ PU1 + PU2 + PU3
  TRI  =~ TRI1 + TRI3 + TRI4
  OBS  =~ OBS3 + OBS4
  CPX  =~ CPX1 + CPX2 + CPX3
  COM  =~ COM1 + COM2 + COM3
  RA   =~ RA2 + RA3
  BI   =~ BI1 + BI2 + BI4
  AGE  =~ Age
"

model_items <- c("ATT2", "ATT3", "ATT4", "PEOU1", "PEOU2", "PEOU4",
                 "PU1", "PU2", "PU3", "TRI1", "TRI3", "TRI4",
                 "OBS3", "OBS4", "CPX1", "CPX2", "CPX3",
                 "COM1", "COM2", "COM3", "RA2", "RA3",
                 "BI1", "BI2", "BI4", "Age")
needed_columns <- c(model_items, "Sex", "Education", "Public_transport_Use",
                    "Ride_User", "Education_Level")
missing_columns <- setdiff(needed_columns, names(data))
if (length(missing_columns) > 0) {
  stop("Missing columns in data.csv: ", paste(missing_columns, collapse = ", "))
}

# ============================================================================
# 3. DESCRIPTIVE STATISTICS
# ============================================================================
age <- as.numeric(as.character(data$Age))
age_groups <- cut(age, breaks = c(60, 65, 75, 85, Inf), right = FALSE,
                  labels = c("60-64", "65-74", "75-84", "85+"))

frequency_table <- function(x) {
  tab <- as.data.frame(table(x, useNA = "ifany"))
  names(tab) <- c("Category", "Frequency")
  tab$Percentage <- round(100 * tab$Frequency / sum(tab$Frequency), 1)
  tab
}

# Descriptive categories retain their numerical codes from data.csv.
descriptive <- list(
  Age = frequency_table(age_groups),
  Sex = frequency_table(data$Sex),
  Education = frequency_table(data$Education),
  `Public Transport` = frequency_table(data$Public_transport_Use)
)
write.xlsx(descriptive, "results/descriptive_statistics.xlsx", overwrite = TRUE)

# ============================================================================
# 4. FIT THE MAIN PLS-SEM MODEL
# ============================================================================
tam <- csem(.data = data, .model = model_TAM,
            .approach_weights = "PLS-PM",
            .PLS_weight_scheme_inner = "path",
            .disattenuate = FALSE)
print(verify(tam))

# ============================================================================
# 5. MEASUREMENT MODEL (RELIABILITY, AVE AND HTMT2)
# ============================================================================
measurement <- assess(tam, .quality_criterion =
                        c("ave", "rho_T", "rho_C", "htmt2"))
exportToExcel(.postestimation_object = measurement,
              .filename = "measurement.xlsx", .path = "results")

# HTMT2 estimates and percentile confidence intervals.
# cSEM uses 1 - 2 * .alpha for the two-sided HTMT2 interval.
htmt2 <- calculateHTMT(.object = tam, .type_htmt = "htmt2",
                       .absolute = FALSE, .inference = "bootstrap",
                       .R = 5000, .seed = 123,
                       .ci = "CI_percentile", .alpha = 0.025,
                       .only_common_factors = TRUE)

wb <- loadWorkbook("results/measurement.xlsx")
addWorksheet(wb, "HTMT2 bootstrap")
writeData(wb, "HTMT2 bootstrap", as.data.frame(htmt2$htmts), rowNames = TRUE)
addWorksheet(wb, "HTMT2 CI")
writeData(wb, "HTMT2 CI", as.data.frame(htmt2$quantiles), rowNames = TRUE)
addWorksheet(wb, "HTMT2 settings")
writeData(wb, "HTMT2 settings", data.frame(
  Setting = c("Bootstrap requested", "Valid bootstrap runs", "Interval"),
  Value = c("5000", as.character(htmt2$nr_admissibles), "95% two-sided percentile")
))
saveWorkbook(wb, "results/measurement.xlsx", overwrite = TRUE)

# ============================================================================
# 6. STRUCTURAL MODEL (PATHS, BOOTSTRAP TESTS, R-SQUARED AND VIF)
# ============================================================================
boot_tam <- csem(.data = data, .model = model_TAM,
                 .approach_weights = "PLS-PM",
                 .PLS_weight_scheme_inner = "path",
                 .disattenuate = FALSE,
                 .resample_method = "bootstrap",
                 .R = 10000, .seed = 123)
structural <- summarize(boot_tam)
exportToExcel(.postestimation_object = structural,
              .filename = "structural.xlsx", .path = "results")

structural_checks <- assess(tam, .quality_criterion = c("vif", "r2", "r2_adj"))
exportToExcel(.postestimation_object = structural_checks,
              .filename = "structural_checks.xlsx", .path = "results")

# ============================================================================
# 7. COMMON METHOD BIAS (SINGLE-INDICATOR DUMMY VARIABLE, FCVIF)
# ============================================================================
# Auxiliary model: the nine questionnaire constructs predict a random,
# single-indicator dummy construct. Age is excluded from this check.
# Use the Dummy column from data.csv, or generate it with a fixed random seed.
if (!"Dummy" %in% names(data)) {
  set.seed(123)
  data$Dummy <- sample(c(0, 1), nrow(data), replace = TRUE)
}

# Use the same measurement indicators as the main model, excluding AGE.
measurement_lines <- grep("=~", strsplit(model_TAM, "\n")[[1]], value = TRUE)
measurement_lines <- measurement_lines[!grepl("^[[:space:]]*AGE[[:space:]]*=~", measurement_lines)]

model_TAM_FCVIF <- paste(c(
  "DUM ~ BI + ATT + PEOU + PU + RA + COM + CPX + OBS + TRI",
  measurement_lines,
  "DUM =~ Dummy"
), collapse = "\n")

tam_fcvif <- csem(.data = data, .model = model_TAM_FCVIF,
                  .approach_weights = "PLS-PM",
                  .PLS_weight_scheme_inner = "path",
                  .disattenuate = FALSE)

# VIFs for predictors of the DUM structural equation (Kock, 2015).
questionnaire_constructs <- c("BI", "ATT", "PEOU", "PU", "RA",
                              "COM", "CPX", "OBS", "TRI")
vif_matrix <- assess(tam_fcvif, .quality_criterion = "vif")$VIF

# Extract the VIF for each predictor in the auxiliary DUM equation.
if (!is.matrix(vif_matrix) || !"DUM" %in% rownames(vif_matrix)) {
  stop("The dummy-model VIF output has an unexpected structure. Inspect assess(tam_fcvif)$VIF.")
}
if (!all(questionnaire_constructs %in% colnames(vif_matrix))) {
  stop("One or more substantive constructs are missing from the dummy-model VIF output.")
}
full_vif <- vif_matrix["DUM", questionnaire_constructs]

fcvif_results <- data.frame(
  Construct = questionnaire_constructs,
  FCVIF = round(as.numeric(full_vif), 3)
)
print(fcvif_results)
write.xlsx(fcvif_results, "results/full_collinearity_VIF.xlsx",
           overwrite = TRUE)

# ============================================================================
# 8. OUT-OF-SAMPLE PREDICTION (PLSPREDICT)
# ============================================================================
prediction <- predict(tam, .benchmark = "lm", .cv_folds = 10,
                      .r = 1, .seed = 123)
exportToExcel(.postestimation_object = prediction,
              .filename = "PLSpredict.xlsx", .path = "results")

# ============================================================================
# 9. GROUP INVARIANCE ASSESSMENT (MICOM)
# ============================================================================
# Binary grouping variables are coded 0 and 1.
run_micom <- function(group_column, group_labels, filename) {
  group0 <- data[data[[group_column]] == 0 & !is.na(data[[group_column]]), ]
  group1 <- data[data[[group_column]] == 1 & !is.na(data[[group_column]]), ]
  if (nrow(group0) < 2 || nrow(group1) < 2) {
    warning("Skipping ", group_column, ": 0/1 group coding not found or too few observations.")
    return(invisible(NULL))
  }
  grouped <- csem(.data = setNames(list(group0, group1), group_labels),
                  .model = model_TAM, .approach_weights = "PLS-PM",
                  .PLS_weight_scheme_inner = "path", .disattenuate = FALSE)
  micom <- testMICOM(grouped, .R = 5000, .seed = 123)
  capture.output(print(micom), file = file.path("results", filename))
  invisible(micom)
}

micom_user <- run_micom("Ride_User", c("Non-user", "User"), "MICOM_users.txt")
micom_sex  <- run_micom("Sex", c("Group 0", "Group 1"), "MICOM_sex.txt")
micom_edu  <- run_micom("Education_Level", c("No degree", "Degree"), "MICOM_education.txt")

# ============================================================================
# 10. EXPLORATORY GROUP COMPARISONS (MANN-WHITNEY TESTS)
# ============================================================================
# Use the retained behavioural-intention indicators from the main model.
# Mann-Whitney compares ranked responses. Means/SDs are descriptive.
data$BI_mean <- rowMeans(data[, c("BI1", "BI2", "BI4")], na.rm = TRUE)

mann_whitney <- function(group_column, label) {
  x <- data$BI_mean[data[[group_column]] == 0]
  y <- data$BI_mean[data[[group_column]] == 1]
  x <- x[is.finite(x)]
  y <- y[is.finite(y)]
  if (length(x) < 2 || length(y) < 2) {
    warning("Skipping Mann-Whitney comparison: ", label)
    return(NULL)
  }
  result <- wilcox.test(x, y, exact = FALSE)
  data.frame(Comparison = label, N_Group0 = length(x), N_Group1 = length(y),
             Mean_Group0 = mean(x), Mean_Group1 = mean(y),
             SD_Group0 = sd(x), SD_Group1 = sd(y),
             W = unname(result$statistic), p_value = result$p.value)
}

mw_results <- do.call(rbind, Filter(Negate(is.null), list(
  mann_whitney("Ride_User", "Non-user vs user"),
  mann_whitney("Sex", "Sex: code 0 vs 1"),
  mann_whitney("Education_Level", "No degree vs degree")
)))
if (!is.null(mw_results)) {
  write.xlsx(mw_results, "results/Mann_Whitney.xlsx", overwrite = TRUE)
}

capture.output(sessionInfo(), file = "results/session_info.txt")
cat("\nAnalysis completed. Outputs are in the results/ folder.\n")
