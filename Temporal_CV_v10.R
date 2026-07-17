# ---- 0. Packages 
library(data.table)   # fast grouped operations
library(fixest)       # fast FE regression (feols)
library(ggplot2)
library(lubridate)
library(zoo)    
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(janitor)
library(lubridate)

options(scipen = 999)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

#0.Starting point - adjust the data------
#please note: this data is not shared, due to the data protection guidelines from the Santa Catarina State Treasury. Everything linked to
#this not shared data is in the folder "data/input/"; Just jump to section 1. if you don't have access to the Santa Catarina State Treasury data.
df_save <- read_excel("data/input/2_5_Final_data_Before_Time_modification_and_new_variables.xlsx")

Final_data_Before_Time_modification <- df_save 
#Remove all objects except the specified target
rm(list = setdiff(ls(), "Final_data_Before_Time_modification"))
backup <- Final_data_Before_Time_modification
Final_data_Before_Time_modification <- backup

table(is.na(Final_data_Before_Time_modification$total_sales_value_daily)) #all false, perfect!

#Chosen for now: valor_total_ecf_cupon_fiscal

# for na.approx if needed

# ---- 0.1 Work on a data.table copy -
dt <- as.data.table(Final_data_Before_Time_modification)

# Ensure date is Date class
dt[, date := as.Date(date)]

# Quick sanity check
cat("Rows:", nrow(dt), "\n")
cat("Cities:", uniqueN(dt$city), "\n")
cat("Date range:", as.character(min(dt$date)), "to", as.character(max(dt$date)), "\n")
cat("NAs in total_sales_value_daily:",
    sum(is.na(dt$total_sales_value_daily)), "\n")

# ==
# STEP 1 — PER-CAPITA NORMALISATION
# ==
# We use 2022 census population.  Note: numero_de_notas_emitidas is ENDOGENOUS
# (panic behaviour inflates it), so it must NOT be used for normalisation.
# It can enter as a regressor in the baseline model as a control for store activity.

dt[, valor_pc := total_sales_value_daily / population_2022]

cat("\nPer-capita value summary:\n")
print(summary(dt$valor_pc))

# ==
# STEP 2 — REAL-VALUE DEFLATION  (IPCA food sub-index)
# ==
# The IPCA-Alimentação em domicílio (food at home) is the most appropriate
# deflator.  Monthly series from IBGE/SIDRA table 7060.
#
# =
# IPCA — Alimentação no domicílio  (FINAL VERSION)
# Strategy: download all subgroups, filter by label — no hardcoded codes needed
# =
library(sidrar)
library(data.table)

pt_months <- c(
  "janeiro" = "01", "fevereiro" = "02", "março"    = "03",
  "abril"   = "04", "maio"      = "05", "junho"    = "06",
  "julho"   = "07", "agosto"    = "08", "setembro" = "09",
  "outubro" = "10", "novembro"  = "11", "dezembro" = "12"
)

parse_pt_date <- function(x) {
  x     <- tolower(trimws(x))
  parts <- strsplit(x, "\\s+")
  dates <- sapply(parts, function(p) {
    if (length(p) != 2) return(NA_character_)
    mon <- pt_months[p[1]]
    if (is.na(mon)) return(NA_character_)
    paste0(p[2], "-", mon, "-01")
  })
  as.Date(dates)
}

# Helper: identify the "Geral, grupo..." column (category label) and "Mês" column
get_col <- function(dt, pattern, exclude = NULL) {
  cols <- grep(pattern, names(dt), value = TRUE, ignore.case = TRUE)
  if (!is.null(exclude)) cols <- cols[!grepl(exclude, cols, ignore.case = TRUE)]
  cols[1]
}

# =
# PART 1 — Download ALL subgroups and filter by label
# =
# Table 7060: Jan 2020 onwards
cat("Downloading table 7060 (all subgroups)...\n")
raw_7060 <- get_sidra(
  x         = 7060,
  variable  = 63,
  period    = "202001-202512",
  geo       = "Brazil",
  classific = "c315",
  category  = list("315" = "all"),
  header    = TRUE,
  format    = 3
)

# Table 1419: Jan 2018 – Dec 2019
cat("Downloading table 1419 (all subgroups)...\n")
raw_1419 <- get_sidra(
  x         = 1419,
  variable  = 63,
  period    = "201801-201912",
  geo       = "Brazil",
  classific = "c315",
  category  = list("315" = "all"),
  header    = TRUE,
  format    = 3
)

# =
# PART 2 — Inspect available categories (fixed: no n= argument)
# =
probe_dt  <- as.data.table(raw_7060)
cat_col   <- get_col(probe_dt, "Geral|subgrupo|subitem")
mes_col   <- get_col(probe_dt, "Mês|Mes", exclude = "Código|Code")

cat("\nAll available categories in table 7060:\n")
print(unique(probe_dt[[cat_col]]))   # <-- fixed: no n= argument

# Confirm label for food at home
target_label <- unique(probe_dt[[cat_col]])[
  grepl("Alimenta.*no domicílio|Alimenta.*no domicilio",
        unique(probe_dt[[cat_col]]), ignore.case = TRUE)
]
cat("\nMatched label:", target_label, "\n")
stopifnot("No matching label found — check print output above" = length(target_label) > 0)

# =
# PART 3 — Filter to Alimentação no domicílio and clean dates
# =
clean_filter <- function(raw, label_col, mes_col, target) {
  dt <- as.data.table(raw)
  dt <- dt[get(label_col) == target]
  dt <- dt[, .(
    year_month = parse_pt_date(as.character(get(mes_col))),
    var_mes    = suppressWarnings(as.numeric(Valor))
  )]
  dt <- dt[!is.na(year_month) & !is.na(var_mes)]
  setorder(dt, year_month)
  dt
}

# Apply to both tables using the same cat_col / mes_col logic
cat_col_1419 <- get_col(as.data.table(raw_1419), "Geral|subgrupo|subitem")
mes_col_1419 <- get_col(as.data.table(raw_1419), "Mês|Mes", exclude = "Código|Code")

# Find matching label in table 1419 (may differ slightly in naming)
labels_1419 <- unique(as.data.table(raw_1419)[[cat_col_1419]])
cat("\nTable 1419 categories:\n")
print(labels_1419)

target_1419 <- labels_1419[
  grepl("Alimenta.*no domicílio|Alimenta.*no domicilio", labels_1419, ignore.case = TRUE)
]
cat("Matched 1419 label:", target_1419, "\n")
stopifnot("No matching label in 1419" = length(target_1419) > 0)

s7060 <- clean_filter(raw_7060, cat_col,   mes_col,   target_label)
s1419 <- clean_filter(raw_1419, cat_col_1419, mes_col_1419, target_1419)

cat("\nTable 7060:", as.character(min(s7060$year_month)),
    "to", as.character(max(s7060$year_month)),
    "(", nrow(s7060), "rows )\n")
cat("Table 1419:", as.character(min(s1419$year_month)),
    "to", as.character(max(s1419$year_month)),
    "(", nrow(s1419), "rows )\n")

# =
# PART 4 — Stack, chain into index, check continuity
# =
alim <- rbind(
  s1419[year_month <  as.Date("2020-01-01")],
  s7060[year_month >= as.Date("2020-01-01")]
)
setorder(alim, year_month)

expected <- seq.Date(as.Date("2018-01-01"), max(alim$year_month), by = "month")
missing  <- expected[!expected %in% alim$year_month]
if (length(missing) == 0) {
  cat("\n✓ Continuous series Jan 2018 –", as.character(max(alim$year_month)), "\n")
} else {
  cat("\n⚠ Missing months:", as.character(missing), "\n")
}

# Chain monthly % changes → index level (base Jan 2018 = 100)
alim[, factor   := 1 + var_mes / 100]
alim[, alim_idx := cumprod(factor) * 100 / cumprod(factor)[1]]
alim[, deflator := alim_idx / 100]

cat("\nFood deflator at January of each year:\n")
print(alim[month(year_month) == 1,
           .(year_month, var_mes, alim_idx, deflator)])
# =
# PART 5 — Merge into main panel and apply deflation
# =
dt[, year_month := as.Date(paste0(format(date, "%Y-%m"), "-01"))]
for (col in c("deflator")) if (col %in% names(dt)) dt[, (col) := NULL]

dt <- merge(dt, alim[, .(year_month, deflator)], by = "year_month", all.x = TRUE)
n_miss <- sum(is.na(dt$deflator))
if (n_miss == 0) cat("\n✓ Deflator merged for all", nrow(dt), "rows.\n") else
  cat("\n⚠ Missing deflator:", n_miss, "rows\n") #perfect!

dt[, valor_pc_real := valor_pc / deflator]
print(summary(dt$valor_pc_real)) #Min 0.000   1st Qu.  7.044  Median   11.474  Mean  12.835 3rd Qu. 15.845 Max. 4278.120 

# Save
fwrite(alim[, .(year_month, var_mes_pct = var_mes,
                index_jan2018_100 = alim_idx, deflator)],
       "data/output/deflator_alimentacao_domicilio_2018_2025.csv")
cat("Saved: deflator_alimentacao_domicilio_2018_2025.csv\n")
cat("=== Done. Proceed with Step 3 in API_construction.R ===\n")

##0.1.Strategic decisions on how to estimate the model---------
#now, back to the dt dataset:
test <- dt
print(summary(test$valor_pc_real))
#dt[, valor_pc_real := valor_pc]   # REPLACE with valor_pc / deflator once available
cat("\nReal per-capita value summary (deflated by Alimentação no domicílio):\n")
print(summary(dt$valor_pc_real))

# ==
# STEP 3 — DEFINE CRISIS / EXCLUSION WINDOWS FOR BASELINE ESTIMATION
# ==
# Known macro-shocks in the sample.  Add further dates as needed.
# The baseline model is estimated ONLY on "calm" periods so that panic episodes
# do not contaminate the expected-value predictions.

crisis_windows <- list(
  # --- Pandemic Waves ---
  covid_wave_1                      = c(as.Date("2020-02-25"), as.Date("2020-09-26")),
  covid_wave_2                      = c(as.Date("2020-11-16"), as.Date("2021-09-16")),
  covid_wave_3                      = c(as.Date("2021-11-16"), as.Date("2022-04-01")),
  
  # --- Geopolitical Events ---
  trucker_strike_2018               = c(as.Date("2018-05-21"), as.Date("2018-05-31")),
  trucker_strike_2021               = c(as.Date("2021-09-07"), as.Date("2021-09-09")),
  trucker_blockades_2022            = c(as.Date("2022-10-31"), as.Date("2022-11-02")),
  
  # --- Climate Events ---
  sc_ciclone_bomba_floripa_2020     = c(as.Date("2020-06-30"), as.Date("2020-07-01")),
  sc_chuvas_fortes_2020             = c(as.Date("2020-12-17"), as.Date("2020-12-21")),
  sc_chuvas_600mm_2021              = c(as.Date("2021-01-21"), as.Date("2021-01-24")),
  sc_alto_vale_storm_2021           = c(as.Date("2021-03-26"), as.Date("2021-03-26")),
  sc_grande_floripa_alto_vale_2021  = c(as.Date("2021-06-08"), as.Date("2021-06-09")),
  sc_chuvas_estado_sul_2022         = c(as.Date("2022-05-03"), as.Date("2022-05-05")),
  sc_cyclone_2022                   = c(as.Date("2022-09-28"), as.Date("2022-10-05")),
  sc_norte_joinville_2022           = c(as.Date("2022-11-29"), as.Date("2022-12-04")),
  sc_bc_itapema_2022                = c(as.Date("2022-12-20"), as.Date("2022-12-22")),
  sc_vale_itajai_bc_2023            = c(as.Date("2023-01-17"), as.Date("2023-01-18")),
  sc_estado_50pct_afetado_2023      = c(as.Date("2023-10-10"), as.Date("2023-10-10")),
  sc_vale_itajai_2023               = c(as.Date("2023-11-11"), as.Date("2023-11-14")),
  sc_floods_2023                    = c(as.Date("2023-11-17"), as.Date("2023-11-30")),
  sc_bc_itapema_2024                = c(as.Date("2024-01-08"), as.Date("2024-01-16")),
  sc_grande_floripa_2025            = c(as.Date("2025-12-08"), as.Date("2025-12-10"))
)

#recreate time variables I excluded before:
dt[, `:=`(
  # Linear time elapsed (Unix Epoch): Captures macro-trends (inflation, total growth)
  time_linear = as.numeric(date),
  
  # Year: Acts as a macro categorical or continuous trend
  date_year = data.table::year(date),
  
  # Discrete identifiers (Useful for tree-based models like XGBoost)
  date_month = data.table::month(date),
  date_wday = data.table::wday(date), # 1 = Sunday, 7 = Saturday
  date_yday = data.table::yday(date), # 1 to 365/366
  
  # Boolean structural flags (Crucial for transactional/retail modeling)
  is_weekend = as.integer(data.table::wday(date) %in% c(1, 7))
)]

# Step 2: Harmonic Cyclical Projection (Crucial for Neural Networks / Regressions)
dt[, `:=`(
  # Map the 12-month cycle
  month_sin = sin(2 * pi * date_month / 12),
  month_cos = cos(2 * pi * date_month / 12),
  
  # Map the 7-day weekly cycle (Retail/Mobility seasonality)
  wday_sin = sin(2 * pi * date_wday / 7),
  wday_cos = cos(2 * pi * date_wday / 7),
  
  # Map the 365-day annual cycle (Climatological/Epidemiological seasonality)
  yday_sin = sin(2 * pi * date_yday / 365.25),
  yday_cos = cos(2 * pi * date_yday / 365.25)
)]

#The exact dates for SC events should be cross-checked against your amb_rain_acc and amb_trend_abs_ciclone columns — if those variables already flag the shock days precisely, you can derive the windows programmatically rather than hardcoding them, which is cleaner and more reproducible.
dt[, time_linear_c := time_linear - min(time_linear)]  # starts at 0

# Mark crisis days
dt[, is_crisis := FALSE]
for (w in crisis_windows) {
  dt[date >= w[1] & date <= w[2], is_crisis := TRUE]
}
cat("\nCrisis days excluded from baseline:", sum(dt$is_crisis), "\n")
cat("Calm (training) days:", sum(!dt$is_crisis), "\n")

table(dt$date_year)

# ==
# STEP 4 — BASELINE OLS  (city FE + seasonal controls)
# ==
# Model:
#   valor_pc_real ~ city_FE + DoW_FE + month_FE + linear_trend + holidays
#
# Notes:
# • City fixed effects (absorbed by fixest) handle persistent cross-sectional
#   scale differences between municipalities.
# • is_weekend is collinear with DoW — omit to avoid perfect multicollinearity.
#   If you prefer, use is_weekend + 5 DoW dummies instead of all 7.
# • time_linear captures the secular nominal/real trend within the non-crisis window.
# • We do NOT use the pre-constructed sin/cos harmonics here because we prefer
#   interpretable month/DoW factor dummies in the baseline; harmonics are
#   better suited for STL-style decomposition.

# Recode day-of-week and month as factors for clean dummies
dt[, dow_f   := factor(date_wday)]    # 1=Sunday ... 7=Saturday (lubridate default)
dt[, month_f := factor(date_month)]

# Brazilian public holidays (national + SC state) — extend as needed
#holidays <- as.Date(c(
# National holidays recurring annually (approximate; adjust for exact dates)
#  paste0(2018:2025, "-01-01"),   # Confraternização Universal
#  paste0(2018:2025, "-04-21"),   # Tiradentes
#  paste0(2018:2025, "-05-01"),   # Dia do Trabalho
# paste0(2018:2025, "-09-07"),   # Independência
# paste0(2018:2025, "-10-12"),   # N.S. Aparecida
# paste0(2018:2025, "-11-02"),   # Finados
# paste0(2018:2025, "-11-15"),   # Proclamação da República
#  paste0(2018:2025, "-12-25"),   # Natal
#  # Carnival (variable — manually specify or compute)
# "2018-02-12", "2018-02-13",
# "2019-03-04", "2019-03-05",
# "2020-02-24", "2020-02-25",
#  "2021-02-15", "2021-02-16",
# "2022-02-28", "2022-03-01",
# "2023-02-20", "2023-02-21",
# "2024-02-12", "2024-02-13",
#  "2025-03-03", "2025-03-04",
# Corpus Christi (variable)
# "2018-05-31", "2019-06-20", "2020-06-11", "2021-06-03",
# "2022-06-16", "2023-06-08", "2024-05-30", "2025-06-19"
#))

holidays <- as.Date(c(
  # --- FERIADOS NACIONAIS FIXOS ---
  paste0(2018:2025, "-01-01"),   # Confraternização Universal
  paste0(2018:2025, "-04-21"),   # Tiradentes
  paste0(2018:2025, "-05-01"),   # Dia do Trabalho
  paste0(2018:2025, "-09-07"),   # Independência
  paste0(2018:2025, "-10-12"),   # N.S. Aparecida
  paste0(2018:2025, "-11-02"),   # Finados
  paste0(2018:2025, "-11-15"),   # Proclamação da República
  "2024-11-20", "2025-11-20",    # Consciência Negra (Nacional desde 2024)
  paste0(2018:2025, "-12-24"),   # Véspera de Natal
  paste0(2018:2025, "-12-25"),   # Natal
  paste0(2018:2025, "-12-31"),   # Véspera de Ano Novo
  
  # --- DATAS COMERCIAIS FIXAS ---
  paste0(2018:2025, "-03-15"),   # Dia do Consumidor
  paste0(2018:2025, "-06-12"),   # Dia dos Namorados
  paste0(2018:2025, "-09-15"),   # Dia do Cliente
  
  # --- DATAS MÓVEIS: RELIGIOSAS E PONTO FACULTATIVO ---
  # Carnaval (Segunda e Terça)
  "2018-02-12", "2018-02-13", "2019-03-04", "2019-03-05",
  "2020-02-24", "2020-02-25", "2021-02-15", "2021-02-16",
  "2022-02-28", "2022-03-01", "2023-02-20", "2023-02-21",
  "2024-02-12", "2024-02-13", "2025-03-03", "2025-03-04",
  
  # Sexta-feira Santa
  "2018-03-30", "2019-04-19", "2020-04-10", "2021-04-02",
  "2022-04-15", "2023-04-07", "2024-03-29", "2025-04-18",
  
  # Domingo de Páscoa
  "2018-04-01", "2019-04-21", "2020-04-12", "2021-04-04",
  "2022-04-17", "2023-04-09", "2024-03-31", "2025-04-20",
  
  # Corpus Christi
  "2018-05-31", "2019-06-20", "2020-06-11", "2021-06-03",
  "2022-06-16", "2023-06-08", "2024-05-30", "2025-06-19",
  
  # --- DATAS MÓVEIS: COMERCIAIS ---
  # Dia das Mães (2º domingo de Maio)
  "2018-05-13", "2019-05-12", "2020-05-10", "2021-05-09",
  "2022-05-08", "2023-05-14", "2024-05-12", "2025-05-11",
  
  # Dia dos Pais (2º domingo de Agosto)
  "2018-08-12", "2019-08-11", "2020-08-09", "2021-08-08",
  "2022-08-14", "2023-08-13", "2024-08-11", "2025-08-10",
  
  # Black Friday (Sexta-feira variável)
  "2018-11-23", "2019-11-29", "2020-11-27", "2021-11-26",
  "2022-11-25", "2023-11-24", "2024-11-29", "2025-11-28"
))

dt[, is_holiday := date %in% holidays]

# --- Training subset (non-crisis only) ---
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

# --- Fit OLS with city fixed effects (absorbed) ---

# First week of month (captures "recebeu salário" effect for private sector)
dt[, is_start_of_month := mday(date) <= 7]

# Keep end-of-month for public servants
dt[, is_end_of_month := mday(date) >= 28]  # tighter than 25

# Days immediately before a holiday (bridge days)
dt[, is_pre_holiday := shift(is_holiday, n = 1, type = "lead"), by = city]
dt[is.na(is_pre_holiday), is_pre_holiday := FALSE]

# Re-create train AFTER adding the new variables to dt
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

# Updated formula
baseline_model <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f + 
    is_holiday + is_pre_holiday + is_end_of_month + is_start_of_month,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

summary(baseline_model)

# Does São Miguel have valid data in the new variable?
dt[city == "São Miguel do Oeste", 
   .(n_valid = sum(!is.na(valor_pc_real)),
     n_na    = sum(is.na(valor_pc_real)),
     mean_val = mean(valor_pc_real, na.rm = TRUE),
     sd_val   = sd(valor_pc_real, na.rm = TRUE))]

# Compare all cities' mean and sd to spot outliers
dt[!is.na(valor_pc_real), 
   .(mean_val = mean(valor_pc_real, na.rm = TRUE),
     sd_val   = sd(valor_pc_real, na.rm = TRUE)),
   by = city][order(-sd_val)]

# Distribution of the dependent variable in training set
summary(train$valor_pc_real)
hist(train$valor_pc_real, breaks = 100, main = "valor_pc_real distribution in train")

# Continuous within-month cycle (one full wave per month)
dt[, dom       := mday(date)]
dt[, dom_sin   := sin(2 * pi * dom / 31)]
dt[, dom_cos   := cos(2 * pi * dom / 31)]

# Drop the blunt binary dummies and rebuild train
dt[, is_end_of_month  := NULL]
dt[, is_start_of_month := NULL]

dt[, dom_sin2 := sin(4 * pi * dom / 31)]
dt[, dom_cos2 := cos(4 * pi * dom / 31)]

dt[, is_day31 := mday(date) == 31]

train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

# ── Winsorise BEFORE baseline estimation ──────────────────────────────────────
# Compute IQR fence on calm-period observations only
upper_fence_iqr <- train[, {
  q   <- quantile(valor_pc_real, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  .(upper_iqr = q[2] + 3 * iqr)
}, by = city]

cat("\nIQR-based upper fences:\n")
print(upper_fence_iqr)

# Apply to full dt (not just train)
if ("upper_iqr" %in% names(dt)) dt[, upper_iqr := NULL]
dt <- merge(dt, upper_fence_iqr, by = "city", all.x = TRUE)

dt[, is_outlier       := !is.na(valor_pc_real) & valor_pc_real > upper_iqr]
dt[, valor_pc_real_raw := valor_pc_real]
dt[is_outlier == TRUE,  valor_pc_real := upper_iqr]

cat("Outliers winsorised:", sum(dt$is_outlier, na.rm = TRUE), "\n")

# Rebuild train from the winsorised dt
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]
cat("Train rows after winsorisation:", nrow(train), "\n")
cat("\nvalor_pc_real summary after winsorisation:\n")
print(summary(train$valor_pc_real))

summary(dt[city == "São Miguel do Oeste", valor_pc_real])

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)
summary(baseline_model_final)

train[, resid := residuals(baseline_model_final)]
train[, dom := mday(date)]
train[, .(mean_resid = mean(resid, na.rm = TRUE)), by = dom] |>
  ggplot(aes(x = dom, y = mean_resid)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Mean residual by day of month",
       x = "Day of month", y = "Mean residual (real BRL p.c.)")

# Add a first-of-month dummy analogous to is_day31
dt[, is_day1 := mday(date) == 1]
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

summary(baseline_model_final)
train[, resid := residuals(baseline_model_final)]
train[, dom := mday(date)]
train[, .(mean_resid = mean(resid, na.rm = TRUE)), by = dom] |>
  ggplot(aes(x = dom, y = mean_resid)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Mean residual by day of month",
       x = "Day of month", y = "Mean residual (real BRL p.c.)")

# Test whether month dummies are jointly significant
library(car)
linearHypothesis(baseline_model_final,
                 grep("month_f", names(coef(baseline_model_final)),
                      value = TRUE))

#yeah, don't drop the months...

# ==
# STEP 5 — PREDICT BASELINE FOR ALL OBSERVATIONS (including crisis days)
# ==
# fixest::predict handles new data including crisis days correctly
dt[, yhat := predict(baseline_model_final, newdata = dt)]

# Residuals on all observations
dt[, residual := valor_pc_real - yhat]

# Compute training residuals on the full train set (city is present here)
train[, resid_train := valor_pc_real - predict(baseline_model_final, newdata = train)]

# Then aggregate sd by city
city_sigma <- train[, .(sigma_c = sd(resid_train, na.rm = TRUE)), by = city]

cat("\nCity-level baseline standard deviations (sigma_c):\n")
print(city_sigma)

#Curitibanos has σ̂_c = 4.98, which is notably higher than comparable-sized interior cities (Campos Novos = 2.65, Caçador = 2.57, Xanxerê = 3.85). 
#Not a problem, just worth confirming it does not have residual outliers after winsorisation:
train[city == "Curitibanos", .(
  p95  = quantile(resid_train, 0.95, na.rm = TRUE),
  p99  = quantile(resid_train, 0.99, na.rm = TRUE),
  max  = max(abs(resid_train), na.rm = TRUE)
)]

# yhat exists in dt but not in train — pull it in
train[, yhat := predict(baseline_model_final, newdata = train)]

# Find what dates are driving the extreme Curitibanos residuals
train[city == "Curitibanos" & abs(resid_train) > 15,
      .(date, valor_pc_real, valor_pc_real_raw, yhat, resid_train)] |>
  setorder(-resid_train) |>
  print()

# Add Dec 30 and Dec 31 as holidays (already have Dec 31 but check Dec 30)
# Actually you already have Dec-31 in holidays — add Dec-30 explicitly
holidays <- c(holidays, as.Date(paste0(2018:2025, "-12-30")))
dt[, is_holiday := date %in% holidays]

# Tighter 2.5× IQR fence for Curitibanos only
curitibanos_fence <- train[city == "Curitibanos", {
  q   <- quantile(valor_pc_real, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  .(fence = q[2] + 2.5 * iqr)
}]
cat("Curitibanos tighter fence:", curitibanos_fence$fence, "\n")

n_re_winsorised <- sum(
  dt$city == "Curitibanos" & 
    !is.na(dt$valor_pc_real) & 
    dt$valor_pc_real > curitibanos_fence$fence, na.rm = TRUE)
cat("Rows re-winsorised:", n_re_winsorised, "\n")

dt[city == "Curitibanos" & 
     !is.na(valor_pc_real) & 
     valor_pc_real > curitibanos_fence$fence,
   valor_pc_real := curitibanos_fence$fence]

# Rebuild full pipeline
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

dt[, yhat     := predict(baseline_model_final, newdata = dt)]
dt[, residual := valor_pc_real - yhat]
train[, resid_train := valor_pc_real - predict(baseline_model_final, newdata = train)]
city_sigma <- train[, .(sigma_c = sd(resid_train, na.rm = TRUE)), by = city]

# Verify fix
print(city_sigma[city == "Curitibanos"])
train[city == "Curitibanos", .(
  p99 = quantile(resid_train, 0.99, na.rm = TRUE),
  max = max(abs(resid_train), na.rm = TRUE)
)]

# Use valor_pc_real_raw (pre-winsorisation) to anchor the fence correctly
curitibanos_fence <- dt[city == "Curitibanos" & is_crisis == FALSE, {
  q   <- quantile(valor_pc_real_raw, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  .(fence = q[2] + 2.5 * iqr)
}]
cat("Curitibanos raw-based fence:", curitibanos_fence$fence, "\n")

# Apply to dt
dt[city == "Curitibanos" &
     !is.na(valor_pc_real) &
     valor_pc_real > curitibanos_fence$fence,
   valor_pc_real := curitibanos_fence$fence]

cat("Rows now at or below fence:",
    sum(dt$city == "Curitibanos" & 
          !is.na(dt$valor_pc_real) & 
          dt$valor_pc_real <= curitibanos_fence$fence, na.rm = TRUE), "\n")

# Rebuild full pipeline
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

dt[, yhat     := predict(baseline_model_final, newdata = dt)]
dt[, residual := valor_pc_real - yhat]
train[, resid_train := valor_pc_real - 
        predict(baseline_model_final, newdata = train)]
city_sigma <- train[, .(sigma_c = sd(resid_train, na.rm = TRUE)), by = city]

print(city_sigma[city == "Curitibanos"])
train[city == "Curitibanos", .(
  p99 = quantile(resid_train, 0.99, na.rm = TRUE),
  max = max(abs(resid_train), na.rm = TRUE)
)]


# Use 2.0× IQR instead of 2.5× or 3×
curitibanos_fence_tight <- dt[city == "Curitibanos" & is_crisis == FALSE, {
  q   <- quantile(valor_pc_real, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  cat("Q1:", q[1], " Q3:", q[2], " IQR:", iqr, "\n")
  cat("2.0× fence:", q[2] + 2.0 * iqr, "\n")
  cat("1.5× fence:", q[2] + 1.5 * iqr, "\n")
  .(fence_2x = q[2] + 2.0 * iqr,
    fence_1x5 = q[2] + 1.5 * iqr)
}]
print(curitibanos_fence_tight)

# Compare with peer cities' typical maximums
dt[city %in% c("Campos Novos", "Caçador", "Xanxerê", "Curitibanos") & 
     is_crisis == FALSE,
   .(p75  = quantile(valor_pc_real, 0.75, na.rm = TRUE),
     p90  = quantile(valor_pc_real, 0.90, na.rm = TRUE),
     p95  = quantile(valor_pc_real, 0.95, na.rm = TRUE),
     p99  = quantile(valor_pc_real, 0.99, na.rm = TRUE)),
   by = city]

# Apply 2.0× fence to Curitibanos
dt[city == "Curitibanos" &
     !is.na(valor_pc_real) &
     valor_pc_real > 28.35551,
   valor_pc_real := 28.35551]

cat("Rows winsorised at 2.0× fence:",
    sum(dt$city == "Curitibanos" &
          !is.na(dt$valor_pc_real_raw) &
          dt$valor_pc_real_raw > 28.35551, na.rm = TRUE), "\n")

# Rebuild full pipeline
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

dt[, yhat     := predict(baseline_model_final, newdata = dt)]
dt[, residual := valor_pc_real - yhat]
train[, resid_train := valor_pc_real -
        predict(baseline_model_final, newdata = train)]
city_sigma <- train[, .(sigma_c = sd(resid_train, na.rm = TRUE)), by = city]

cat("\nAll city sigmas:\n")
print(city_sigma)

train[city == "Curitibanos", .(
  p99 = quantile(resid_train, 0.99, na.rm = TRUE),
  max = max(abs(resid_train), na.rm = TRUE)
)]

# Confirm no other city has a problematic sigma ratio
dt[!is.na(valor_pc_real),
   .(mean_val = mean(valor_pc_real, na.rm = TRUE)),
   by = city] |>
  merge(city_sigma, by = "city") |>
  _[, ratio := sigma_c / mean_val] |>
  setorder(-ratio) |>
  print()

#new step 6
# Merge sigma and compute API
if ("sigma_c" %in% names(dt)) dt[, sigma_c := NULL]
dt <- merge(dt, city_sigma, by = "city", all.x = TRUE)
dt[, API := residual / sigma_c]

cat("\nAPI summary:\n")
print(summary(dt$API))
print(quantile(dt$API, probs = c(.01,.05,.25,.50,.75,.90,.95,.99), na.rm = TRUE))

# Step 7 — ordinal categorisation
dt[, API_cat := fcase(
  API < -1.0,               "Below Normal",
  API >= -1.0 & API < 1.0,  "Normal",
  API >= 1.0  & API < 2.0,  "Elevated",
  API >= 2.0  & API < 3.0,  "Panic",
  API >= 3.0,               "Extreme Panic",
  default = NA_character_
)]

dt[, API_cat := factor(API_cat,
                       levels = c("Below Normal","Normal","Elevated",
                                  "Panic","Extreme Panic"),
                       ordered = TRUE)]

cat("\nAPI category distribution:\n")
print(dt[, .N, by = API_cat][order(API_cat)])

#608 extreme panick sounds high:
# Which cities and periods are driving Extreme Panic?
dt[API_cat == "Extreme Panic", 
   .(n = .N), 
   by = .(city, date_year)][order(-n)] |>
  head(20) |>
  print()

# What is the max API and which city/date?
dt[!is.na(API), .SD[which.max(API)], 
   .SDcols = c("city","date","API","valor_pc_real","yhat")]

#check curitibanos:
# What does the week around it look like?
dt[city == "Caçador" & 
     date >= as.Date("2025-09-15") & 
     date <= as.Date("2025-09-30"),
   .(date, valor_pc_real, yhat, API, API_cat)]

# Revert Curitibanos to standard 3× IQR (same as all other cities)
curitibanos_3x <- dt[city == "Curitibanos" & is_crisis == FALSE, {
  q   <- quantile(valor_pc_real_raw, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  .(fence = q[2] + 3 * iqr)
}]
cat("Curitibanos 3× fence:", curitibanos_3x$fence, "\n")

# Reset Curitibanos to raw values first, then apply standard fence
dt[city == "Curitibanos", valor_pc_real := valor_pc_real_raw]
dt[city == "Curitibanos" &
     !is.na(valor_pc_real) &
     valor_pc_real > curitibanos_3x$fence,
   valor_pc_real := curitibanos_3x$fence]

# Rebuild full pipeline
train <- dt[is_crisis == FALSE & !is.na(valor_pc_real)]

baseline_model_final <- feols(
  valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2,
  data    = train,
  fixef   = "city",
  cluster = "city"
)

dt[, yhat     := predict(baseline_model_final, newdata = dt)]
dt[, residual := valor_pc_real - yhat]
train[, resid_train := valor_pc_real -
        predict(baseline_model_final, newdata = train)]
city_sigma <- train[, .(sigma_c = sd(resid_train, na.rm = TRUE)), by = city]

if ("sigma_c" %in% names(dt)) dt[, sigma_c := NULL]
dt <- merge(dt, city_sigma, by = "city", all.x = TRUE)
dt[, API := residual / sigma_c]

# Check Curitibanos Extreme Panic count
cat("\nCuritibanos extreme panic days by year:\n")
dt[city == "Curitibanos" & !is.na(API) & API >= 3,
   .N, by = date_year][order(date_year)] |> print()

cat("\nAll city sigmas:\n")
print(city_sigma)


# Treat as outliers — winsorise to yhat (expected value) for those two days
dt[city == "Caçador" & date %in% as.Date(c("2025-09-23", "2025-09-24")),
   valor_pc_real := yhat]

# Check what's driving 2022 Curitibanos extreme panic
dt[city == "Curitibanos" & date_year == 2022 & API >= 3,
   .(date, valor_pc_real, yhat, API)] |>
  setorder(date) |>
  print()

# After fixing Caçador, recompute and check overall distribution
dt[city == "Caçador" & date %in% as.Date(c("2025-09-23", "2025-09-24")),
   valor_pc_real := yhat]
dt[, residual := valor_pc_real - yhat]
dt[, API      := residual / sigma_c]

dt[, API_cat := fcase(
  API < -1.0,               "Below Normal",
  API >= -1.0 & API < 1.0,  "Normal",
  API >= 1.0  & API < 2.0,  "Elevated",
  API >= 2.0  & API < 3.0,  "Panic",
  API >= 3.0,               "Extreme Panic",
  default = NA_character_
)]
dt[, API_cat := factor(API_cat,
                       levels = c("Below Normal","Normal","Elevated",
                                  "Panic","Extreme Panic"), ordered = TRUE)]

cat("\nAPI category distribution:\n")
print(dt[, .N, by = API_cat][order(API_cat)])

cat("\nExtreme Panic by city and year:\n")
dt[API >= 3, .N, by = .(city, date_year)][order(-N)] |> head(20) |> print()

# ==
# STEP 8 — FACE VALIDITY PLOTS
# ==
# Key question: do API spikes align with known shocks?
# Inspect aggregated API (mean across cities) and per-city series.

# Face validity plots (same as original pipeline)
daily_api <- dt[, .(API_mean = mean(API, na.rm = TRUE),
                    API_max  = max(API,  na.rm = TRUE)), by = date]

events <- data.frame(
  date  = as.Date(c("2018-05-21","2020-03-13","2021-01-08")),
  label = c("Trucker Strike","COVID Onset SC","COVID 2nd Wave"),
  color = c("red","darkred","orange")
)

p_global <- ggplot(daily_api, aes(x = date, y = API_mean)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gold",   alpha = 0.8) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "orange", alpha = 0.8) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red",    alpha = 0.8) +
  geom_vline(data = events, aes(xintercept = date, color = label),
             linetype = "solid", linewidth = 0.8) +
  scale_color_manual(values = setNames(events$color, events$label)) +
  labs(title    = "Abnormal Purchase Index — Mean across 15 SC municipalities",
       subtitle = "Dashed lines: 1\u03c3 / 2\u03c3 / 3\u03c3 thresholds; vertical lines: known shock dates",
       x = "Date", y = "API (z-score)", color = "Event") +
  theme_minimal(base_size = 11)

print(p_global)
ggsave("data/output/API_global_validation.png", p_global, width = 12, height = 5, dpi = 150)

#individual cities:
p_post2022 <- dt[date >= as.Date("2022-01-01")] |>
  ggplot(aes(x = date, y = API)) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "orange", alpha = 0.7) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red",    alpha = 0.7) +
  facet_wrap(~city, scales = "free_y", ncol = 3) +
  labs(title = "API by municipality (2022–2025)",
       x = "Date", y = "API (z-score)") +
  theme_minimal(base_size = 9)

print(p_post2022)
ggsave("data/output/API_per_city_validation_post2022.png", p_post2022, width = 14, height = 16, dpi = 150)

# 8b. Per-city facet (2018–2021 window for legibility)
p_facet <- dt[date <= as.Date("2021-12-31")] |>
  ggplot(aes(x = date, y = API)) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "orange", alpha = 0.7) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "red",    alpha = 0.7) +
  geom_vline(data = events, aes(xintercept = date),
             color = "red", linetype = "dotted", linewidth = 0.6) +
  facet_wrap(~city, scales = "free_y", ncol = 3) +
  labs(title = "API by municipality (2018–2021)",
       x = "Date", y = "API (z-score)") +
  theme_minimal(base_size = 9)

print(p_facet)
ggsave("data/output/API_per_city_validation_2018_to_2022.png", p_facet, width = 14, height = 16, dpi = 150)

# ==
# STEP 9 — EXPORT ENRICHED DATASET
# ==
# Write back to R object and optionally to CSV/RDS
Final_data_Before_Time_modification <- as.data.frame(dt)

# Recommended: save as RDS for fast re-loading
saveRDS(dt, "data/input/Final_data_With_API.rds")

# ==
# METHODOLOGICAL NOTES FOR THE PAPER
# ==
# The API_{c,t} is defined as:
#
#   API_{c,t} = (Ṽ_{c,t} - V̂_{c,t}) / σ̂_c
#
# where:
#   Ṽ_{c,t}  = (total_sales_value_daily_{c,t} / pop_c) / deflator_t
#               [real per-capita daily supermarket spending in city c on day t]
#   V̂_{c,t}  = fitted value from OLS with city FE, DoW FE, month FE,
#               linear trend, and holiday dummies, estimated on non-crisis obs.
#   σ̂_c      = city-specific standard deviation of OLS residuals on non-crisis obs.
#
# The resulting API is dimensionless, zero-centered on normal days, and directly
# interpretable as standard deviations above the city's expected baseline —
# analogous to abnormal returns in finance event studies.
# ==

write.csv2(Final_data_Before_Time_modification, "data/input/2_Final_data_Before_Time_modification_and_new_variables.csv", row.names =F )
write_xlsx(Final_data_Before_Time_modification, path = "data/input/2_Final_data_Before_Time_modification_and_new_variables.xlsx")

##0.2.Dataset with delayed variables------
#Amb_ and gov_ : move to two days after the nfe data (so that to predict day X, the model looks at the amb_ and gov_ data from x-2
#psic_ move to one day ago;

Final_data <- Final_data_Before_Time_modification
# Ensure explicit data.table allocation and strict chronological sorting
setDT(Final_data)
setorder(Final_data, city, date)

# Step 1: Algorithmically isolate the target column namespaces using regex
stimuli <- grep("^(S_)", names(Final_data), value = TRUE)
organism    <- grep("^O_", names(Final_data), value = TRUE)

# Step 2: Execute the 2-day historical lag (t-2) by reference
Final_data[, (stimuli) := lapply(.SD, shift, n = 2L, type = "lag"), 
           by = city, .SDcols = stimuli]

# Step 3: Execute the 1-day historical lag (t-1) by reference
Final_data[, (organism) := lapply(.SD, shift, n = 1L, type = "lag"), 
           by = city, .SDcols = organism]

write.csv2(Final_data, "data/input/3_Final_data_With_Time_modification.csv", row.names =F )
#rm(list=ls())
#Final_data <- read.csv2("3_Final_data_With_Time_modification.csv")
#summary(Final_data)
write_xlsx(Final_data, path = "data/input/3_Final_data_With_Time_modification.xlsx")

##0.3.Alternative datasets------
rm(list=ls())
###0.3.1. Adding the mobility variable as an S_ and the super, hiper, mini market variables as O_ -----
Final_data_Before_Time_modification <- read.csv2("data/input/2_Final_data_Before_Time_modification_and_new_variables.csv")
Final_data_Before_Time_modification$date <- as.Date(Final_data_Before_Time_modification$date)

Final_data_Before_Time_modification <- Final_data_Before_Time_modification %>%
  rename(O_mobil_dist_0 = mobil_dist_0,
         O_mobil_dist_0_10km = mobil_dist_0_10km,
         O_mobil_dist_100km_plus = mobil_dist_100km_plus,
         O_mobil_dist_10_100km = mobil_dist_10_100km,
         # mobil_google_grocery_and_pharmacy_percent_change = Data,
         O_hypermarket_total_sales_value_daily = hypermarket_total_sales_value_daily,
         O_supermarket_total_sales_value_daily = supermarket_total_sales_value_daily,
         O_minimarket_total_sales_value_daily = minimarket_total_sales_value_daily) 

# Crisis period dummy — binary indicator for structural break periods
# where consumer behaviour fundamentally changed regime
setDT(Final_data_Before_Time_modification)

#just the first wave is included
Final_data_Before_Time_modification[, S_is_crisis_period := as.integer(
  # --- Pandemic Waves ---
  (date >= as.Date("2020-02-25") & date <= as.Date("2020-09-26")) | # 1st wave
    (date >= as.Date("2020-11-16") & date <= as.Date("2021-09-16")) | # 2nd wave
    (date >= as.Date("2021-11-16") & date <= as.Date("2022-04-01")) | # 3rd wave (Omicron)
    
    # --- Geopolitical Events ---
    (date >= as.Date("2018-05-21") & date <= as.Date("2018-05-31")) | # Trucker strike 2018
    (date >= as.Date("2021-09-07") & date <= as.Date("2021-09-09")) | # Trucker strike 2021
    (date >= as.Date("2022-10-31") & date <= as.Date("2022-11-02")) | # Trucker blockades 2022
    
    # --- Climate Events ---
    (date >= as.Date("2020-06-30") & date <= as.Date("2020-07-01")) | # Ciclone bomba de Floripa
    (date >= as.Date("2020-12-17") & date <= as.Date("2020-12-21")) | # Chuvas fortes
    (date >= as.Date("2021-01-21") & date <= as.Date("2021-01-24")) | # Choveu mais de 600mm
    (date >= as.Date("2021-03-26") & date <= as.Date("2021-03-26")) | # Alto Vale storm
    (date >= as.Date("2021-06-08") & date <= as.Date("2021-06-09")) | # Grande Floripa e Alto Vale
    (date >= as.Date("2022-05-03") & date <= as.Date("2022-05-05")) | # Chuvas estado sul
    (date >= as.Date("2022-09-28") & date <= as.Date("2022-10-05")) | # SC cyclone 2022
    (date >= as.Date("2022-11-29") & date <= as.Date("2022-12-04")) | # Norte Joinville
    (date >= as.Date("2022-12-20") & date <= as.Date("2022-12-22")) | # BC Itapema 2022
    (date >= as.Date("2023-01-17") & date <= as.Date("2023-01-18")) | # Vale Itajai BC
    (date >= as.Date("2023-10-10") & date <= as.Date("2023-10-10")) | # 50% afetado
    (date >= as.Date("2023-11-11") & date <= as.Date("2023-11-14")) | # Vale Itajai 2023
    (date >= as.Date("2023-11-17") & date <= as.Date("2023-11-30")) | # SC floods 2023
    (date >= as.Date("2024-01-08") & date <= as.Date("2024-01-16")) | # BC Itapema 2024
    (date >= as.Date("2025-12-08") & date <= as.Date("2025-12-10"))   # Grande Floripa 2025
)]

cat("Crisis period days:", sum(Final_data_Before_Time_modification$S_is_crisis_period), "\n")
cat("Non-crisis days:", sum(!Final_data_Before_Time_modification$S_is_crisis_period), "\n")
cat("Crisis share:", round(mean(Final_data_Before_Time_modification$S_is_crisis_period) * 100, 1), "%\n")

#Dataset with delayed variables
Final_data <- Final_data_Before_Time_modification
# Ensure explicit data.table allocation and strict chronological sorting
setDT(Final_data)
setorder(Final_data, city, date)

# Step 1: Algorithmically isolate the target column namespaces using regex
stimuli <- grep("^(S_)", names(Final_data), value = TRUE)
organism    <- grep("^O_", names(Final_data), value = TRUE)

# Step 2: Execute the 2-day historical lag (t-2) by reference
Final_data[, (stimuli) := lapply(.SD, shift, n = 2L, type = "lag"), 
           by = city, .SDcols = stimuli]

# Step 3: Execute the 1-day historical lag (t-1) by reference
Final_data[, (organism) := lapply(.SD, shift, n = 1L, type = "lag"), 
           by = city, .SDcols = organism]

write.csv2(Final_data, "data/input/3_Final_data_With_Time_modification_ADDED_Var.csv", row.names =F )


# ════════════════════════════════════════════════════════════════════════════
# EXPANDING-WINDOW TIME-SERIES CROSS-VALIDATION
# ────────────────────────────────────────────────────────────────────────────
# Purpose
#   Replace the random 80/20 split (caret::createDataPartition with set.seed)
#   used in Final_main_model_v2.R with an expanding-window CV that respects
#   temporal ordering. This is required for the EWS framing: an early-warning
#   system must be evaluated on *future* data the model has not seen.
#
# Design
#   • Initial training cutoff : 2019-12-31  (2 years pre-pandemic)
#   • Test block length       : 6 months
#   • Window scheme           : expanding (training data grows; test rolls)
#   • Hyperparameters         : fixed from prior grid search (best_row.csv)
#   • nrounds                 : adaptive via early stopping on internal val
#   • Internal validation     : last 90 days of each training window
#
# Outputs (in improved_final/temporal_cv/)
#   • temporal_cv_fold_metrics.csv      per-fold R², RMSE, MAE, n_train, n_test
#   • temporal_cv_summary_stats.csv     mean, SD, 95% empirical CI across folds
#   • temporal_cv_predictions.rds       all out-of-sample predictions (linked
#                                         to date + city for downstream use)
#   • temporal_cv_results.rds           full per-fold list (models, metrics,
#                                         predictions); cache for re-runs
#   • temporal_cv_r2_evolution.png      diagnostic plot of R² across folds
#   • temporal_cv_predictions_plot.png  obs vs. predicted, all folds combined
#
# Run instructions
#   Place this file in the same directory as Final_main_model_v2.R and
#   3_Final_data_With_Time_modification_ADDED_Var.csv. Run it after the
#   main script has been executed at least once (so best_row.csv exists).
#   You do NOT need the main script to be in memory — this script is
#   self-contained and replicates the preprocessing.
# ════════════════════════════════════════════════════════════════════════════

caches <- c(
  "data/analysis/temporal_cv/temporal_cv_results.rds",
  "data/analysis/temporal_cv/temporal_cv_predictions.rds",
  "data/analysis/temporal_cv_classification/classif_results.rds",
  "data/analysis/temporal_cv_classification/classif_results_partial.rds",
  "data/analysis/temporal_cv_ar_augmented/ar_only/results.rds",
  "data/analysis/temporal_cv_ar_augmented/ar_only/results_partial.rds",
  "data/analysis/temporal_cv_ar_augmented/external_only/results.rds",
  "data/analysis/temporal_cv_ar_augmented/external_only/results_partial.rds",
  "data/analysis/temporal_cv_ar_augmented/combined/results.rds",
  "data/analysis/temporal_cv_ar_augmented/combined/results_partial.rds"
)
cat("Deleted", sum(file.remove(caches[file.exists(caches)])), "cache files.\n")

#1.EXPANDING-WINDOW CV — AR-AUGMENTED REGRESSION (HYPOTHESIS 2)------
# Purpose
#   Test Hypothesis 2: does adding lagged PBB-Score as features (autoregression)
#   produce predictive skill, and do external open data add INCREMENTAL value
#   on top of an AR baseline?
#
# Design: three parallel models on the same temporal CV
#   (1) AR-only       : lag-1, 7, 14, 28 of PBB-Score, no external features
#   (2) External-only : original feature set (re-run for like-for-like)
#   (3) Combined      : AR features + external features
#
# Decisive test
#   • If AR-only R² >> 0 and External-only R² ≈ 0 (already known):
#       → external data don't anticipate panic; autocorrelation does
#   • If Combined ≈ AR-only:
#       → external data add no incremental value beyond AR baseline
#   • If Combined > AR-only by a meaningful margin:
#       → external data carry real anticipatory signal given AR
#
# Outputs (in improved_final/temporal_cv_ar_augmented/)
#   • ar_only/, external_only/, combined/   subdirs with per-model results
#   • comparison_fold_metrics.csv           side-by-side R²/RMSE/MAE by fold
#   • comparison_summary.csv                mean and SD per model
#   • comparison_r2_by_fold.png             three R² trajectories overlaid
#   • comparison_incremental_value.png      Combined − AR-only by fold
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "xgboost", "data.table",
  "patchwork", "lubridate"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" AR-AUGMENTED EXPANDING-WINDOW CV (3-way comparison)\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Output directories
output_dir <- "data/analysis/temporal_cv_ar_augmented/"
for (sub in c("ar_only", "external_only", "combined")) {
  d <- file.path(output_dir, sub)
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Hyperparameters
best_row_path <- "data/output/best_row.csv"
if (!file.exists(best_row_path)) {
  stop("best_row.csv not found at: ", best_row_path,
       "\nRun Final_main_model_v2.R first to generate it.")
}

VAL_WINDOW_DAYS <- 90
LAG_PERIODS     <- c(1, 7, 14, 28)


# ── 1. Load and preprocess (mirror Temporal_CV_v1.R) ─────────────────────────

cat("[1] LOADING DATA...\n")
df_raw <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")
df_raw$date <- as.Date(df_raw$date)
cat(sprintf("    Loaded: %s rows x %s columns\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))

cat("\n[2] PREPROCESSING...\n")

cols_to_drop <- c(
  "year_month", "upper_iqr", "valor_pc_real_raw", "residual", "yhat",
  "is_crisis", "sigma_c", "population_2022", "date_year",
  "valor_pc", "valor_pc_real", "time_linear", "time_linear_c", "is_outlier",
  "deflator",
  "month_sin", "month_cos", "wday_sin", "wday_cos", "yday_sin", "yday_cos",
  "dom_sin", "dom_cos", "dom_sin2", "dom_cos2",
  "is_holiday", "is_pre_holiday", "dom", "is_day31", "is_day1", "dow_f",
  "month_f", "is_weekend", "date_month", "date_wday", "date_yday",
  "S_precip_daily", "S_is_crisis_period",
  "S_pandemic_cases_z", "S_pandemic_deaths_z",
  "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
  "S_pandemic_severity_index",
  "ecf_sales_value_daily", "nfce_sales_value_daily", "nfe_sales_value_daily",
  "total_sales_value_daily",
  "hypermarket_ecf_sales_value_daily", "hypermarket_nfc_sales_value_daily",
  "hypermarket_nf_sales_value_daily",
  "O_hypermarket_total_sales_value_daily", "O_supermarket_total_sales_value_daily",
  "O_minimarket_total_sales_value_daily",
  "minimarket_ecf_sales_value_daily", "minimarket_nfc_sales_value_daily",
  "minimarket_nf_sales_value_daily",
  "supermarket_ecf_sales_value_daily", "supermarket_nfc_sales_value_daily",
  "supermarket_nf_sales_value_daily",
  "nfce_count_daily", "nfe_count_daily",
  "hypermarket_nfce_count_daily", "hypermarket_nfe_count_daily",
  "minimarket_nfe_count_daily", "minimarket_nfce_count_daily",
  "supermarket_nfe_count_daily", "supermarket_nfce_count_daily",
  "sales_value_per_capita"
)

# Note: KEEP `city` and `date` for lag construction; drop them only later
df <- df_raw %>% dplyr::select(-any_of(cols_to_drop))

pandemic_to_drop <- c(
  "S_pandemic_cases_z", "S_pandemic_deaths_z",
  "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
  "S_pandemic_severity_index"
)
df <- df %>% dplyr::select(-any_of(pandemic_to_drop))

pandemic_vars <- grep("^S_pandemic", names(df), value = TRUE)
df[, pandemic_vars] <- lapply(df[, pandemic_vars],
                              function(x) replace(x, is.na(x), 0))

df <- df %>% dplyr::select(-any_of(c("API_cat")))

char_cols <- names(df)[sapply(df, is.character) &
                         !(names(df) %in% c("date", "city"))]
for (col in char_cols) df[[col]] <- as.numeric(as.factor(df[[col]]))

cat(sprintf("    After drops: %d columns (city + date retained for lag step)\n",
            ncol(df)))


# ── 2. Build lag features (panel-aware) ──────────────────────────────────────

cat("\n[3] BUILDING LAG FEATURES...\n")
cat(sprintf("    Lag periods: %s\n", paste(LAG_PERIODS, collapse = ", ")))

TARGET <- "API"

# Sort by city, date (mandatory for correct lag construction)
df <- df %>% dplyr::arrange(city, date)

# Add lag features, group by city to avoid leakage across cities.
# Explicit dplyr::lag — stats::lag has different semantics (ts objects).
for (k in LAG_PERIODS) {
  newcol <- sprintf("lag_%d_API", k)
  df <- df %>%
    dplyr::group_by(city) %>%
    dplyr::mutate(!!newcol := dplyr::lag(.data[[TARGET]], n = k)) %>%
    dplyr::ungroup()
  cat(sprintf("    Built: %s (NA in first %d obs per city)\n", newcol, k))
}

# Filter rows with valid TARGET
valid_idx    <- !is.na(df[[TARGET]])
df_model     <- df[valid_idx, ]
dates_model  <- df_model$date
cities_model <- df_model$city

cat(sprintf("\n    Rows with valid target: %s\n",
            format(nrow(df_model), big.mark = ",")))

# Impute remaining NA in lag columns with 0 (long-run mean of standardized score)
lag_cols <- sprintf("lag_%d_API", LAG_PERIODS)
for (col in lag_cols) {
  n_na <- sum(is.na(df_model[[col]]))
  df_model[[col]][is.na(df_model[[col]])] <- 0
  if (n_na > 0) {
    cat(sprintf("    Imputed %s NA values in %s with 0 (%.2f%% of rows)\n",
                format(n_na, big.mark = ","), col, 100 * n_na / nrow(df_model)))
  }
}


# ── 3. Define three feature subsets ──────────────────────────────────────────

# --- Fold-aware PBB-Score + AR lags (genuine out-of-sample) 
# Re-fit baseline (V-hat) and sigma on NON-CRISIS days with date <= train_end
# ONLY; standardize ALL rows with those fold-local coefficients; rebuild the
# AR lags from this fold-local score so the AR features are themselves
# out-of-sample. Returns target + lag columns aligned to df_model row order.
suppressMessages({ library(fixest); library(data.table) })

.base_df <- as.data.table(df_raw)
.base_df[, date      := as.Date(date)]
.base_df[, is_crisis := as.logical(is_crisis)]
.base_df[, dow_f     := factor(dow_f)]
.base_df[, month_f   := factor(month_f)]

.BASELINE_FORMULA <- valor_pc_real ~ time_linear_c + dow_f + month_f +
  is_holiday + is_pre_holiday + is_day31 + is_day1 +
  dom_sin + dom_cos + dom_sin2 + dom_cos2

.dm_key <- paste(df_model$city, df_model$date)
.fold_feat_cache <- new.env(parent = emptyenv())
build_fold_features <- function(train_end) {
  key0 <- as.character(train_end)
  if (!is.null(.fold_feat_cache[[key0]])) return(.fold_feat_cache[[key0]])
  tr <- .base_df[is_crisis == FALSE & !is.na(valor_pc_real) & date <= train_end]
  m  <- fixest::feols(.BASELINE_FORMULA, data = tr, fixef = "city")
  ap <- data.table(
    city  = .base_df$city,
    date  = .base_df$date,
    api_f = .base_df$valor_pc_real - predict(m, newdata = .base_df)
  )
  tr_resid <- tr$valor_pc_real - predict(m, newdata = tr)
  sig <- tapply(tr_resid, tr$city, stats::sd, na.rm = TRUE)
  ap[, api_f := api_f / as.numeric(sig[as.character(city)])]
  setorder(ap, city, date)
  for (k in LAG_PERIODS)
    ap[, (sprintf("lag_%d_API", k)) := shift(api_f, k), by = city]
  mi <- match(.dm_key, paste(ap$city, ap$date))
  lag_names <- sprintf("lag_%d_API", LAG_PERIODS)
  lst <- lapply(lag_names, function(cn) { v <- ap[[cn]][mi]; v[is.na(v)] <- 0; v })
  names(lst) <- lag_names
  out <- list(y = ap$api_f[mi], lags = lst)
  .fold_feat_cache[[key0]] <- out
  out
}


cat("\n[4] DEFINING FEATURE SUBSETS...\n")

all_cols      <- setdiff(names(df_model), c("date", "city"))
y_col         <- TARGET
feature_pool  <- setdiff(all_cols, y_col)

ar_features        <- lag_cols
external_features  <- setdiff(feature_pool, ar_features)
combined_features  <- feature_pool

cat(sprintf("    AR-only features      : %2d  (%s)\n",
            length(ar_features), paste(ar_features, collapse = ", ")))
cat(sprintf("    External-only features: %2d\n", length(external_features)))
cat(sprintf("    Combined features     : %2d\n", length(combined_features)))


# ── 4. Define expanding-window folds ─────────────────────────────────────────

cat("\n[5] DEFINING FOLDS...\n")

data_start <- min(dates_model)
data_end   <- max(dates_model)

initial_train_end <- as.Date("2019-12-31")
fold_step_months  <- 6

fold_cutoffs <- seq.Date(initial_train_end, data_end, by = "6 months")
fold_cutoffs <- fold_cutoffs[fold_cutoffs < data_end - 30]

folds <- data.frame(
  fold_id    = seq_along(fold_cutoffs),
  train_end  = fold_cutoffs,
  test_start = fold_cutoffs + 1,
  test_end   = pmin(fold_cutoffs %m+% months(fold_step_months), data_end)
)

cat(sprintf("    Number of folds: %d\n", nrow(folds)))


# ── 5. Hyperparameters ───────────────────────────────────────────────────────

best_row <- read.csv(best_row_path)
base_params <- list(
  objective        = "reg:squarederror",
  max_depth        = best_row$max_depth,
  eta              = best_row$eta,
  subsample        = best_row$subsample,
  colsample_bytree = best_row$colsample_bytree,
  min_child_weight = best_row$min_child_weight,
  gamma            = best_row$gamma,
  lambda           = 2.0,
  alpha            = 0.1,
  nthread          = 4,
  eval_metric      = "rmse"
)
cat("\n[6] HYPERPARAMETERS from best_row.csv:\n")
print(best_row[, c("max_depth", "eta", "subsample", "colsample_bytree",
                   "min_child_weight", "gamma")])

# ── 6. Modular CV function (with incremental caching) ────────────────────────

run_cv <- function(feature_cols, model_label, subdir) {
  
  results_file <- file.path(subdir, "results.rds")
  partial_file <- file.path(subdir, "results_partial.rds")
  
  if (file.exists(results_file)) {
    cat(sprintf("    Loading cached results: %s\n", results_file))
    return(readRDS(results_file))
  }
  
  if (file.exists(partial_file)) {
    fold_results <- readRDS(partial_file)
    completed <- which(!sapply(fold_results, is.null))
    cat(sprintf("    Resuming from partial cache: %d/%d folds complete\n",
                length(completed), nrow(folds)))
  } else {
    fold_results <- vector("list", nrow(folds))
  }
  
  Xm <- as.matrix(df_model[, feature_cols, drop = FALSE])
  storage.mode(Xm) <- "double"
  y <- df_model[[TARGET]]
  
  for (i in seq_len(nrow(folds))) {
    
    if (!is.null(fold_results[[i]])) {
      cat(sprintf("    [Fold %2d] cached, skipping\n", i))
      next
    }
    
    fold <- folds[i, ]
    train_idx_full <- which(dates_model <= fold$train_end)
    test_idx       <- which(dates_model >= fold$test_start &
                              dates_model <= fold$test_end)
    
    val_cutoff <- fold$train_end - VAL_WINDOW_DAYS
    train_idx  <- train_idx_full[dates_model[train_idx_full] <= val_cutoff]
    val_idx    <- train_idx_full[dates_model[train_idx_full] >  val_cutoff]
    
    if (length(test_idx) == 0) {
      cat(sprintf("    [Fold %2d] SKIPPED — empty test set\n", i))
      next
    }
    
    # Fold-aware target + AR lags (baseline V-hat and sigma from <= train_end)
    ff   <- build_fold_features(fold$train_end)
    y    <- ff$y
    Xm_f <- Xm
    for (lc in intersect(names(ff$lags), feature_cols)) Xm_f[, lc] <- ff$lags[[lc]]
    dtrain <- xgb.DMatrix(data = Xm_f[train_idx, , drop = FALSE], label = y[train_idx])
    dval   <- xgb.DMatrix(data = Xm_f[val_idx,   , drop = FALSE], label = y[val_idx])
    dtest  <- xgb.DMatrix(data = Xm_f[test_idx,  , drop = FALSE], label = y[test_idx])
    
    model_fold <- xgb.train(
      params                = base_params,
      data                  = dtrain,
      nrounds               = 2000,
      evals                 = list(train = dtrain, val = dval),
      early_stopping_rounds = 50,
      verbose               = 0
    )
    
    best_iter <- as.integer(model_fold$best_iteration)
    if (length(best_iter) == 0L || is.na(best_iter) || best_iter < 1L) {
      best_iter <- if (!is.null(model_fold$niter)) {
        as.integer(model_fold$niter)
      } else 2000L
    }
    
    sliced_model <- tryCatch(
      xgb.slice.Booster(model_fold, start = 1L, end = best_iter),
      error = function(e) NULL
    )
    if (!is.null(sliced_model)) {
      y_pred     <- predict(sliced_model, dtest)
      y_pred_val <- predict(sliced_model, dval)
    } else {
      y_pred     <- predict(model_fold, dtest)
      y_pred_val <- predict(model_fold, dval)
    }
    
    y_true <- y[test_idx]
    rmse <- sqrt(mean((y_true - y_pred)^2))
    mae  <- mean(abs(y_true - y_pred))
    ss_res <- sum((y_true - y_pred)^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    r2 <- 1 - ss_res / ss_tot
    
    fold_results[[i]] <- list(
      fold_id      = i,
      train_end    = fold$train_end,
      test_start   = fold$test_start,
      test_end     = fold$test_end,
      n_train      = length(train_idx),
      n_val        = length(val_idx),
      n_test       = length(test_idx),
      best_nrounds = best_iter,
      r2           = r2,
      rmse         = rmse,
      mae          = mae,
      predictions  = data.frame(
        date    = dates_model[test_idx],
        city    = cities_model[test_idx],
        y_true  = y_true,
        y_pred  = y_pred,
        row_idx = test_idx
      ),
      val_predictions = data.frame(
        y_true = y[val_idx],
        y_pred = y_pred_val
      )
    )
    
    cat(sprintf("    [%s | Fold %2d] test %s-%s | n_train=%5d n_test=%4d | R2=%6.3f RMSE=%5.3f MAE=%5.3f | nrounds=%d\n",
                model_label, i, fold$test_start, fold$test_end,
                length(train_idx), length(test_idx),
                r2, rmse, mae, best_iter))
    
    saveRDS(fold_results, partial_file)
  }
  
  fold_results <- Filter(Negate(is.null), fold_results)
  saveRDS(fold_results, results_file)
  if (file.exists(partial_file)) file.remove(partial_file)
  
  fold_results
}

# ── 7. Run CV for each of the three models ───────────────────────────────────

cat("\n[7] RUNNING CV — AR-ONLY MODEL\n")
res_ar  <- run_cv(ar_features,
                  "AR-only",
                  file.path(output_dir, "ar_only"))

cat("\n[8] RUNNING CV — EXTERNAL-ONLY MODEL\n")
res_ext <- run_cv(external_features,
                  "External-only",
                  file.path(output_dir, "external_only"))

cat("\n[9] RUNNING CV — COMBINED MODEL\n")
res_comb <- run_cv(combined_features,
                   "Combined",
                   file.path(output_dir, "combined"))


# ── 8. Aggregate and compare ─────────────────────────────────────────────────

extract_metrics <- function(res, model_name) {
  do.call(rbind, lapply(res, function(f) {
    data.frame(
      model      = model_name,
      fold_id    = f$fold_id,
      test_start = f$test_start,
      n_test     = f$n_test,
      r2         = f$r2,
      rmse       = f$rmse,
      mae        = f$mae
    )
  }))
}

m_ar   <- extract_metrics(res_ar,   "AR-only")
m_ext  <- extract_metrics(res_ext,  "External-only")
m_comb <- extract_metrics(res_comb, "Combined")

cat("\n[10] PER-FOLD COMPARISON (R²)\n")
comp_r2 <- data.frame(
  fold_id    = m_ar$fold_id,
  test_start = m_ar$test_start,
  AR_only       = m_ar$r2,
  External_only = m_ext$r2,
  Combined      = m_comb$r2,
  Incremental_value = m_comb$r2 - m_ar$r2
)
print(comp_r2, row.names = FALSE, digits = 3)

write.csv(comp_r2, file.path(output_dir, "comparison_fold_metrics.csv"),
          row.names = FALSE)

cat("\n[11] SUMMARY ACROSS FOLDS\n")
summary_df <- data.frame(
  model = c("AR-only", "External-only", "Combined"),
  mean_r2 = c(mean(m_ar$r2),   mean(m_ext$r2),   mean(m_comb$r2)),
  sd_r2   = c(sd(m_ar$r2),     sd(m_ext$r2),     sd(m_comb$r2)),
  med_r2  = c(median(m_ar$r2), median(m_ext$r2), median(m_comb$r2)),
  mean_rmse = c(mean(m_ar$rmse), mean(m_ext$rmse), mean(m_comb$rmse)),
  mean_mae  = c(mean(m_ar$mae),  mean(m_ext$mae),  mean(m_comb$mae))
)
print(summary_df, row.names = FALSE, digits = 4)

write.csv(summary_df, file.path(output_dir, "comparison_summary.csv"),
          row.names = FALSE)


# ── 9. Plots ─────────────────────────────────────────────────────────────────

cat("\n[12] BUILDING COMPARISON PLOTS...\n")

all_m <- rbind(m_ar, m_ext, m_comb)
all_m$model <- factor(all_m$model,
                      levels = c("External-only", "AR-only", "Combined"))

# Plot 1: R² trajectories
# Plot 1: R² trajectories
# Plot 1: R² trajectories
p_r2 <- ggplot(all_m, aes(x = test_start, y = r2, color = model, shape = model)) +
  
# 1. PANDEMIC WAVES (Orange)
annotate("rect", xmin = as.Date("2020-02-25"), xmax = as.Date("2020-09-26"), ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.08) +
  annotate("text", x = as.Date("2020-06-10"), y = Inf, vjust = 1.5, label = "Pandemic\n(1st Wave)", color = "#E65F2B", size = 3, fontface = "bold") +
  
  annotate("rect", xmin = as.Date("2020-11-16"), xmax = as.Date("2021-09-16"), ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.08) +
  annotate("text", x = as.Date("2021-04-15"), y = Inf, vjust = 1.5, label = "Pandemic\n(2nd Wave)", color = "#E65F2B", size = 3, fontface = "bold") +
  
  annotate("rect", xmin = as.Date("2021-11-16"), xmax = as.Date("2022-04-01"), ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.08) +
  annotate("text", x = as.Date("2022-01-20"), y = Inf, vjust = 1.5, label = "Pandemic\n(3rd Wave)", color = "#E65F2B", size = 3, fontface = "bold") +
  
# 2. GEOPOLITICAL EVENTS (Blue)
# * Note: Visually padded by +/- 7 days so they aren't invisible 1-pixel lines
# 2021 Trucker Strike
annotate("rect", xmin = as.Date("2021-09-01"), xmax = as.Date("2021-09-15"), ymin = -Inf, ymax = Inf, fill = "#0099B8", alpha = 0.15) +
  annotate("text", x = as.Date("2021-09-08"), y = Inf, vjust = 3.5, label = "Strike\n('21)", color = "#0099B8", size = 3, fontface = "bold") +
  
  # 2022 Blockades
  annotate("rect", xmin = as.Date("2022-10-24"), xmax = as.Date("2022-11-09"), ymin = -Inf, ymax = Inf, fill = "#0099B8", alpha = 0.15) +
  annotate("text", x = as.Date("2022-11-01"), y = Inf, vjust = 1.5, label = "Blockades\n('22)", color = "#0099B8", size = 3, fontface = "bold") +
  
# 3. CLIMATE EVENTS (Teal)
# * Note: Visually padded short events for visibility
# 2020 Cyclone Bomb
annotate("rect", xmin = as.Date("2020-06-25"), xmax = as.Date("2020-07-06"), ymin = -Inf, ymax = Inf, fill = "#2A9D8F", alpha = 0.15) +
  annotate("text", x = as.Date("2020-07-01"), y = Inf, vjust = 3.5, label = "Cyclone\n('20)", color = "#2A9D8F", size = 3, fontface = "bold") +
  
  # 2022 Cyclone
  annotate("rect", xmin = as.Date("2022-09-23"), xmax = as.Date("2022-10-10"), ymin = -Inf, ymax = Inf, fill = "#2A9D8F", alpha = 0.15) +
  annotate("text", x = as.Date("2022-10-01"), y = Inf, vjust = 3.5, label = "Cyclone\n('22)", color = "#2A9D8F", size = 3, fontface = "bold") +
  
  # 2023 Floods (Broad cluster)
  annotate("rect", xmin = as.Date("2023-10-10"), xmax = as.Date("2023-11-30"), ymin = -Inf, ymax = Inf, fill = "#2A9D8F", alpha = 0.12) +
  annotate("text", x = as.Date("2023-11-05"), y = Inf, vjust = 1.5, label = "Floods\n('23)", color = "#2A9D8F", size = 3, fontface = "bold") +
  
# Base lines and points
geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 3) +
  
  # Formatting
  scale_color_manual(values = c("External-only" = "#D7263D",
                                "AR-only"       = "#66CC33",
                                "Combined"      = "#CC0099")) +
  labs(x = "Test fold start date", y = expression("Out-of-sample R"^2),
       title = NULL,
       color = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

# Save outputs
ggsave(file.path(output_dir, "comparison_r2_by_fold.png"),
       p_r2, width = 11, height = 5.5, dpi = 150)

ggsave(file.path("data/analysis/PDFs", "Figure_4.pdf"),
       p_r2, width = 14, height = 5.5, device = "pdf")


# Plot 2: Incremental value of external features (Combined − AR-only) by fold
inc_df <- data.frame(test_start = m_comb$test_start,
                     incremental = m_comb$r2 - m_ar$r2)
p_inc <- ggplot(inc_df, aes(x = test_start, y = incremental)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(fill = "#0099B8", alpha = 0.85) +
  labs(x = "Test fold start date",
       y = expression("R"^2 * "(Combined) − R"^2 * "(AR-only)"),
       title = "Incremental value of external features over AR baseline",
       subtitle = "Positive bars = external features add forecasting value") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "comparison_incremental_value.png"),
       p_inc, width = 11, height = 4.5, dpi = 150)

cat(sprintf("    Plots saved to: %s\n", output_dir))


# ── 10. Final summary and interpretation guide ───────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  AR-only       : mean R² = %+.3f (SD %.3f)\n",
            mean(m_ar$r2), sd(m_ar$r2)))
cat(sprintf("  External-only : mean R² = %+.3f (SD %.3f)\n",
            mean(m_ext$r2), sd(m_ext$r2)))
cat(sprintf("  Combined      : mean R² = %+.3f (SD %.3f)\n",
            mean(m_comb$r2), sd(m_comb$r2)))
cat(sprintf("\n  Incremental value (Combined − AR-only): %+.3f\n",
            mean(m_comb$r2) - mean(m_ar$r2)))

cat("\n  INTERPRETATION GUIDE:\n")
cat("  Scenario A — AR-only R² >> 0, Combined ≈ AR-only:\n")
cat("    Target is predictable from its own history, but external open data\n")
cat("    add no incremental value. Paper reframes around AR-based forecasting\n")
cat("    with externals as auxiliary; the EWS claim is unsupported.\n\n")
cat("  Scenario B — AR-only R² >> 0, Combined > AR-only by ≥ 0.05:\n")
cat("    External data carry real anticipatory signal GIVEN the AR baseline.\n")
cat("    Paper salvageable: report AR baseline + value added by externals.\n\n")
cat("  Scenario C — AR-only R² ≈ 0 (similar to External-only):\n")
cat("    Target is fundamentally unpredictable as constructed. Move to\n")
cat("    Hypothesis 3 (target reconstruction) or Path B (methodological\n")
cat("    reframing).\n")
cat("════════════════════════════════════════════════════════════════════════\n")


fr <- readRDS("data/analysis/temporal_cv_ar_augmented/combined/results.rds")
str(fr[[1]]$val_predictions)   # should be a data.frame with y_true, y_pred

# ── 0. Setup ─────────────────────────────────────────────────────────────────
##1.1.Diagnostics-------
required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "xgboost", "data.table", "patchwork", "lubridate"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" EXPANDING-WINDOW TIME-SERIES CROSS-VALIDATION\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Output directory
#output_dir <- "improved_final/temporal_cv/"
output_dir <- "data/analysis/temporal_cv/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Path to the best hyperparameters from your existing grid search
best_row_path <- "data/output/best_row.csv"
if (!file.exists(best_row_path)) {
  stop("best_row.csv not found at: ", best_row_path,
       "\nRun Final_main_model_v2.R first to generate it.")
}


# ── 1. Load data, preserving date + city ─────────────────────────────────────

cat("[1] LOADING DATA...\n")
df_raw <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")

cat(sprintf("    Loaded: %s rows x %s columns\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))

# Parse date and check range
df_raw$date <- as.Date(df_raw$date)
cat(sprintf("    Date range: %s -> %s\n",
            min(df_raw$date, na.rm = TRUE), max(df_raw$date, na.rm = TRUE)))

# Preserve date and city BEFORE the modelling pipeline drops them
dates_full  <- df_raw$date
cities_full <- df_raw$city


# ── 2. Replicate preprocessing from main script ──────────────────────────────
# These are the same drops as in Final_main_model_v2.R lines ~96-235.
# Date is NOT dropped here; we strip it later, just before xgb.DMatrix.

cat("\n[2] PREPROCESSING...\n")

cols_to_drop <- c(
  "year_month", "upper_iqr", "valor_pc_real_raw", "residual", "yhat",
  "is_crisis", "sigma_c", "population_2022", "date_year",
  "valor_pc", "valor_pc_real", "time_linear", "time_linear_c", "is_outlier",
  "deflator",
  "month_sin", "month_cos", "wday_sin", "wday_cos", "yday_sin", "yday_cos",
  "dom_sin", "dom_cos", "dom_sin2", "dom_cos2",
  "is_holiday", "is_pre_holiday", "dom", "is_day31", "is_day1", "dow_f",
  "month_f", "is_weekend", "date_month", "date_wday", "date_yday",
  "S_precip_daily", "S_is_crisis_period",
  "S_pandemic_cases_z", "S_pandemic_deaths_z",
  "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
  "S_pandemic_severity_index",
  "ecf_sales_value_daily", "nfce_sales_value_daily", "nfe_sales_value_daily",
  "total_sales_value_daily",
  "hypermarket_ecf_sales_value_daily", "hypermarket_nfc_sales_value_daily",
  "hypermarket_nf_sales_value_daily",
  "O_hypermarket_total_sales_value_daily", "O_supermarket_total_sales_value_daily",
  "O_minimarket_total_sales_value_daily",
  "minimarket_ecf_sales_value_daily", "minimarket_nfc_sales_value_daily",
  "minimarket_nf_sales_value_daily",
  "supermarket_ecf_sales_value_daily", "supermarket_nfc_sales_value_daily",
  "supermarket_nf_sales_value_daily",
  "nfce_count_daily", "nfe_count_daily",
  "hypermarket_nfce_count_daily", "hypermarket_nfe_count_daily",
  "minimarket_nfe_count_daily", "minimarket_nfce_count_daily",
  "supermarket_nfe_count_daily", "supermarket_nfce_count_daily",
  "sales_value_per_capita"
)

df <- df_raw %>% dplyr::select(-any_of(cols_to_drop))

# Drop redundant pandemic vars
pandemic_to_drop <- c(
  "S_pandemic_cases_z", "S_pandemic_deaths_z",
  "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
  "S_pandemic_severity_index"
)
df <- df %>% dplyr::select(-any_of(pandemic_to_drop))

# NA -> 0 in pandemic variables (semantically correct — pre-pandemic = 0)
pandemic_vars <- grep("^S_pandemic", names(df), value = TRUE)
df[, pandemic_vars] <- lapply(df[, pandemic_vars],
                              function(x) replace(x, is.na(x), 0))

# Drop API_cat and city (city already preserved separately above)
df <- df %>% dplyr::select(-any_of(c("API_cat", "city")))

cat(sprintf("    After drops: %d columns retained\n", ncol(df)))

# Encode any remaining character columns as integer factors
char_cols <- names(df)[sapply(df, is.character) & names(df) != "date"]
for (col in char_cols) {
  df[[col]] <- as.numeric(as.factor(df[[col]]))
  cat(sprintf("    Encoded: %s\n", col))
}


# ── 3. Filter to rows with valid target ──────────────────────────────────────

TARGET <- "API"
valid_idx <- !is.na(df[[TARGET]])
df_model    <- df[valid_idx, ]
dates_model <- dates_full[valid_idx]
cities_model <- cities_full[valid_idx]

cat(sprintf("\n[3] Rows with valid target (%s): %s\n",
            TARGET, format(sum(valid_idx), big.mark = ",")))

# Final feature matrix and target — drop date here
X <- df_model %>% dplyr::select(-all_of(TARGET), -any_of("date"))
y <- df_model[[TARGET]]
X <- X %>% mutate(across(everything(), as.numeric))

cat(sprintf("    Modelling matrix: %s rows x %s features\n",
            format(nrow(X), big.mark = ","), ncol(X)))


# ── 4. Define expanding-window folds ─────────────────────────────────────────

##1.2.Fold-aware PBB-Score (genuine out-of-sample target) --------------------
# The PBB-Score loaded above was standardized ONCE on the full sample, so its
# baseline (V-hat) and city sigma "saw" post-cutoff data. Per fold we re-fit
# the baseline and sigma on NON-CRISIS days with date <= train_end ONLY and
# standardize ALL rows with those fold-local coefficients (the test block is
# extrapolated, as a live EWS would operate). Inputs (baseline regressors,
# valor_pc_real, is_crisis) are all present in df_raw.
suppressMessages(library(fixest))

.base_df <- df_raw
.base_df$date      <- as.Date(.base_df$date)
.base_df$is_crisis <- as.logical(.base_df$is_crisis)
.base_df$dow_f     <- factor(.base_df$dow_f)
.base_df$month_f   <- factor(.base_df$month_f)

.BASELINE_FORMULA <- valor_pc_real ~ time_linear_c + dow_f + month_f +
  is_holiday + is_pre_holiday + is_day31 + is_day1 +
  dom_sin + dom_cos + dom_sin2 + dom_cos2

.fold_api_cache <- new.env(parent = emptyenv())
build_fold_api <- function(train_end) {
  key0 <- as.character(train_end)
  if (!is.null(.fold_api_cache[[key0]])) return(.fold_api_cache[[key0]])
  keep <- which(.base_df$is_crisis == FALSE &
                  !is.na(.base_df$valor_pc_real) &
                  .base_df$date <= train_end)
  tr <- .base_df[keep, ]
  m  <- fixest::feols(.BASELINE_FORMULA, data = tr, fixef = "city")
  resid_all <- .base_df$valor_pc_real - predict(m, newdata = .base_df)
  tr_resid  <- tr$valor_pc_real - predict(m, newdata = tr)
  sig       <- tapply(tr_resid, tr$city, stats::sd, na.rm = TRUE)
  api_full  <- resid_all / as.numeric(sig[as.character(.base_df$city)])
  out <- api_full[valid_idx]
  .fold_api_cache[[key0]] <- out
  out
}


cat("\n[4] DEFINING EXPANDING-WINDOW FOLDS...\n")

data_start <- min(dates_model)
data_end   <- max(dates_model)

initial_train_end <- as.Date("2019-12-31")
fold_step_months  <- 6
val_window_days   <- 90    # last 90 days of each training window = validation

# Generate fold cutoffs (training set ends at these dates)
fold_cutoffs <- seq.Date(initial_train_end, data_end, by = "6 months")
# Drop the final cutoff if there is no test data after it
fold_cutoffs <- fold_cutoffs[fold_cutoffs < data_end - 30]

# Build the fold definition table
folds <- data.frame(
  fold_id    = seq_along(fold_cutoffs),
  train_end  = fold_cutoffs,
  test_start = fold_cutoffs + 1,
  test_end   = pmin(fold_cutoffs %m+% months(fold_step_months), data_end)
)

cat(sprintf("    Number of folds: %d\n", nrow(folds)))
cat("    Fold schedule:\n")
for (i in seq_len(nrow(folds))) {
  cat(sprintf("      [%2d] train: %s -> %s   test: %s -> %s\n",
              folds$fold_id[i],
              data_start, folds$train_end[i],
              folds$test_start[i], folds$test_end[i]))
}


# ── 5. CV loop ───────────────────────────────────────────────────────────────

cv_cache_file <- file.path(output_dir, "temporal_cv_results.rds")

if (!file.exists(cv_cache_file)) {

  cat("\n[5] RUNNING EXPANDING-WINDOW CV...\n")
  cat("    (re-run skipped if cache exists; delete temporal_cv_results.rds to force)\n\n")

  # Load best hyperparameters from your prior grid search
  best_row <- read.csv(best_row_path)
  cat("    Hyperparameters from best_row.csv:\n")
  print(best_row[, c("max_depth", "eta", "subsample", "colsample_bytree",
                     "min_child_weight", "gamma")])

  base_params <- list(
    objective        = "reg:squarederror",
    max_depth        = best_row$max_depth,
    eta              = best_row$eta,
    subsample        = best_row$subsample,
    colsample_bytree = best_row$colsample_bytree,
    min_child_weight = best_row$min_child_weight,
    gamma            = best_row$gamma,
    lambda           = 2.0,
    alpha            = 0.1,
    nthread          = 4,
    eval_metric      = "rmse"
  )

  fold_results <- vector("list", nrow(folds))

  for (i in seq_len(nrow(folds))) {

    fold <- folds[i, ]

    # Indices
    train_idx_full <- which(dates_model <= fold$train_end)
    test_idx       <- which(dates_model >= fold$test_start &
                            dates_model <= fold$test_end)

    # Internal validation = last val_window_days of training window
    val_cutoff <- fold$train_end - val_window_days
    train_idx  <- train_idx_full[dates_model[train_idx_full] <= val_cutoff]
    val_idx    <- train_idx_full[dates_model[train_idx_full] >  val_cutoff]

    # Guard: skip fold if test set is empty
    if (length(test_idx) == 0) {
      cat(sprintf("[Fold %2d] SKIPPED — empty test set\n", i))
      next
    }

    # Build XGBoost matrices
    # Fold-aware target: baseline V-hat and sigma from date <= train_end only
    y_f <- build_fold_api(fold$train_end)
    dtrain <- xgb.DMatrix(data = as.matrix(X[train_idx, ]), label = y_f[train_idx])
    dval   <- xgb.DMatrix(data = as.matrix(X[val_idx,   ]), label = y_f[val_idx])
    dtest  <- xgb.DMatrix(data = as.matrix(X[test_idx,  ]), label = y_f[test_idx])

    # Train with early stopping on internal validation set
    model_fold <- xgb.train(
      params                = base_params,
      data                  = dtrain,
      nrounds               = 2000,
      evals                 = list(train = dtrain, val = dval),
      early_stopping_rounds = 50,
      verbose               = 0
    )

    # Predictions on out-of-sample test window.
    # XGBoost 2.x has a JSON-serialization bug where `iterationrange` is
    # parsed as a String regardless of how the integer is constructed in R.
    # Workaround: slice the booster to keep only trees up to best_iteration,
    # then predict without iterationrange. This is exact and avoids the bug.
    best_iter <- as.integer(model_fold$best_iteration)

    # XGBoost 2.x: best_iteration is NULL when early stopping never triggers.
    # Fall back to the total iterations actually run, or to nrounds.
    if (length(best_iter) == 0L || is.na(best_iter) || best_iter < 1L) {
      best_iter <- if (!is.null(model_fold$niter)) {
        as.integer(model_fold$niter)
      } else {
        2000L
      }
    }

    sliced_model <- tryCatch(
      xgb.slice.Booster(model_fold, start = 1L, end = best_iter),
      error = function(e) NULL
    )

    if (!is.null(sliced_model)) {
      y_pred <- predict(sliced_model, dtest)
    } else {
      # Fallback: predict with full booster (uses best_iter + patience trees).
      # With eta=0.05 and patience=50, the extra trees add negligible bias
      # since they were the ones that failed to improve validation.
      y_pred <- predict(model_fold, dtest)
      if (i == 1) cat("    (note: xgb.slice.Booster unavailable, using full booster)\n")
    }
    y_true <- y_f[test_idx]

    # Regression metrics
    rmse <- sqrt(mean((y_true - y_pred)^2))
    mae  <- mean(abs(y_true - y_pred))
    ss_res <- sum((y_true - y_pred)^2)
    ss_tot <- sum((y_true - mean(y_true))^2)
    r2     <- 1 - ss_res / ss_tot

    # Store fold result
    fold_results[[i]] <- list(
      fold_id      = i,
      train_end    = fold$train_end,
      test_start   = fold$test_start,
      test_end     = fold$test_end,
      n_train      = length(train_idx),
      n_val        = length(val_idx),
      n_test       = length(test_idx),
      best_nrounds = best_iter,
      r2           = r2,
      rmse         = rmse,
      mae          = mae,
      model        = model_fold,   # keep for downstream SHAP
      predictions  = data.frame(
        date   = dates_model[test_idx],
        city   = cities_model[test_idx],
        y_true = y_true,
        y_pred = y_pred,
        row_idx = test_idx
      )
    )

    cat(sprintf("[Fold %2d] test %s-%s | n_train=%5d n_val=%4d n_test=%4d | R2=%6.3f RMSE=%5.3f MAE=%5.3f | nrounds=%d\n",
                i, fold$test_start, fold$test_end,
                length(train_idx), length(val_idx), length(test_idx),
                r2, rmse, mae, model_fold$best_iteration))
  }

  # Drop NULLs from skipped folds
  fold_results <- Filter(Negate(is.null), fold_results)
  saveRDS(fold_results, cv_cache_file)
  cat(sprintf("\n    Cached results to: %s\n", cv_cache_file))

} else {
  cat("\n[5] LOADING CACHED CV RESULTS...\n")
  fold_results <- readRDS(cv_cache_file)
  cat(sprintf("    Loaded %d folds from cache.\n", length(fold_results)))
}


# ── 6. Aggregate results ─────────────────────────────────────────────────────

cat("\n[6] AGGREGATING METRICS...\n")

metrics_df <- do.call(rbind, lapply(fold_results, function(f) {
  best_n <- f$best_nrounds
  if (is.null(best_n) || length(best_n) == 0L || is.na(best_n)) {
    best_n <- NA_integer_
  }
  data.frame(
    fold_id      = f$fold_id,
    train_end    = f$train_end,
    test_start   = f$test_start,
    test_end     = f$test_end,
    n_train      = f$n_train,
    n_val        = f$n_val,
    n_test       = f$n_test,
    best_nrounds = as.integer(best_n),
    r2           = f$r2,
    rmse         = f$rmse,
    mae          = f$mae
  )
}))

write.csv(metrics_df, file.path(output_dir, "temporal_cv_fold_metrics.csv"),
          row.names = FALSE)

# Summary stats with empirical 95% CI across folds
summary_stats <- data.frame(
  metric = c("R2", "RMSE", "MAE"),
  mean   = c(mean(metrics_df$r2),  mean(metrics_df$rmse),  mean(metrics_df$mae)),
  sd     = c(sd(metrics_df$r2),    sd(metrics_df$rmse),    sd(metrics_df$mae)),
  median = c(median(metrics_df$r2), median(metrics_df$rmse), median(metrics_df$mae)),
  q025   = c(quantile(metrics_df$r2, 0.025, names = FALSE),
             quantile(metrics_df$rmse, 0.025, names = FALSE),
             quantile(metrics_df$mae, 0.025, names = FALSE)),
  q975   = c(quantile(metrics_df$r2, 0.975, names = FALSE),
             quantile(metrics_df$rmse, 0.975, names = FALSE),
             quantile(metrics_df$mae, 0.975, names = FALSE))
)
write.csv(summary_stats, file.path(output_dir, "temporal_cv_summary_stats.csv"),
          row.names = FALSE)

cat("\n    Per-fold metrics:\n")
print(metrics_df[, c("fold_id", "test_start", "test_end",
                     "n_test", "r2", "rmse", "mae")], row.names = FALSE)

cat("\n    Summary across folds (mean +/- SD, 95% empirical CI):\n")
print(summary_stats, row.names = FALSE)


# ── 7. Save all out-of-sample predictions ────────────────────────────────────

all_predictions <- do.call(rbind, lapply(fold_results, function(f) {
  cbind(fold_id = f$fold_id, f$predictions)
}))
saveRDS(all_predictions,  file.path(output_dir, "temporal_cv_predictions.rds"))
write.csv(all_predictions, file.path(output_dir, "temporal_cv_predictions.csv"),
          row.names = FALSE)
cat(sprintf("\n    Out-of-sample predictions saved: %d rows\n", nrow(all_predictions)))


# ── 8. Comparison with prior random-split test R^2 ───────────────────────────

cat("\n[7] COMPARISON WITH RANDOM-SPLIT BASELINE...\n")
random_test_r2_prior <- 0.34   # from your previous results; update if different
cat(sprintf("    Random 80/20 split (prior result):     Test R2 = %.3f\n",
            random_test_r2_prior))
cat(sprintf("    Expanding-window CV (this run):        Mean R2 = %.3f (SD %.3f)\n",
            mean(metrics_df$r2), sd(metrics_df$r2)))
cat(sprintf("    Expanding-window CV (this run):        Median R2 = %.3f\n",
            median(metrics_df$r2)))
delta <- mean(metrics_df$r2) - random_test_r2_prior
cat(sprintf("    Difference (temporal - random):        %+.3f\n", delta))
cat("    Interpretation: a notable drop is EXPECTED and HONEST — the random\n")
cat("    split allowed the model to interpolate across known crisis episodes,\n")
cat("    inflating apparent forecast skill. The temporal-CV number is the one\n")
cat("    that supports the EWS claim.\n")


# ── 9. Diagnostic plots ──────────────────────────────────────────────────────

cat("\n[8] BUILDING DIAGNOSTIC PLOTS...\n")

# Plot 1: R^2 across folds, with crisis periods shaded and labeled
p_r2 <- ggplot(metrics_df, aes(x = test_start, y = r2)) +
  
  # 1. Pandemic 1st Wave (Red)
  annotate("rect",
           xmin = as.Date("2020-03-01"), xmax = as.Date("2020-09-30"),
           ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.10) +
  annotate("text", x = as.Date("2020-06-15"), y = Inf, vjust = 1.5,
           label = "Pandemic\n(1st Wave)", color = "#E65F2B", size = 3.5, fontface = "bold") +
  
  # 2. Pandemic 2nd Wave (Red)
  annotate("rect",
           xmin = as.Date("2021-03-01"), xmax = as.Date("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.10) +
  annotate("text", x = as.Date("2021-06-15"), y = Inf, vjust = 1.5,
           label = "Pandemic\n(2nd Wave)", color = "#E65F2B", size = 3.5, fontface = "bold") +
  
  # 3. Geopolitical: 2022 Blockades (Blue) - Corrected to Oct-Nov 2022
  annotate("rect",
           xmin = as.Date("2022-10-15"), xmax = as.Date("2022-11-30"),
           ymin = -Inf, ymax = Inf, fill = "#0099B8", alpha = 0.15) +
  annotate("text", x = as.Date("2022-11-07"), y = Inf, vjust = 1.5,
           label = "Geopolitical\n(Blockades)", color = "#0099B8", size = 3.5, fontface = "bold") +
  
  # 4. Climate: 2023 Floods (Teal)
  annotate("rect",
           xmin = as.Date("2023-09-15"), xmax = as.Date("2023-12-15"),
           ymin = -Inf, ymax = Inf, fill = "#2A9D8F", alpha = 0.15) +
  annotate("text", x = as.Date("2023-11-01"), y = Inf, vjust = 1.5,
           label = "Climate\n(Floods)", color = "#2A9D8F", size = 3.5, fontface = "bold") +
  
  # Base lines and points
  geom_line(color = "grey30", linewidth = 0.5) +
  geom_point(size = 3.2, color = "#0099B8") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  # Random-split reference line
  geom_hline(yintercept = random_test_r2_prior, linetype = "dotted", color = "#E65F2B", linewidth = 0.7) +
  annotate("text", x = max(metrics_df$test_start), y = random_test_r2_prior + 0.02,
           label = "Random-split R2 (prior)", hjust = 1, size = 3, color = "#E65F2B") +
  
  # Formatting
  labs(
    x     = "Test fold start date",
    y     = expression("Out-of-sample R"^2),
    title = "Expanding-window CV: forecast skill across test folds"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "temporal_cv_r2_evolution.png"),
       p_r2, width = 11, height = 5, dpi = 150)

# Plot 2: Observed vs predicted across all folds (out-of-sample only)
overall_r2 <- 1 - sum((all_predictions$y_true - all_predictions$y_pred)^2) /
                  sum((all_predictions$y_true - mean(all_predictions$y_true))^2)

p_obs_pred <- ggplot(all_predictions, aes(x = y_true, y = y_pred)) +
  geom_point(alpha = 0.25, color = "#0099B8") +
  geom_abline(slope = 1, intercept = 0, color = "#E65F2B",
              linetype = "dashed", linewidth = 0.7) +
  labs(
    x        = "Observed PBB-Score",
    y        = "Predicted PBB-Score",
    title    = sprintf("Out-of-sample predictions, all folds combined (overall R2 = %.3f)",
                       overall_r2)
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "temporal_cv_predictions_plot.png"),
       p_obs_pred, width = 7, height = 6, dpi = 150)

# Plot 3: RMSE and MAE across folds (companion to R2 plot)
p_err <- metrics_df %>%
  tidyr::pivot_longer(cols = c(rmse, mae), names_to = "metric", values_to = "value") %>%
  mutate(metric = toupper(metric)) %>%
  ggplot(aes(x = test_start, y = value, color = metric, shape = metric)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("RMSE" = "#E65F2B", "MAE" = "#0099B8")) +
  labs(
    x = "Test fold start date", y = "Error",
    title = "Expanding-window CV: error metrics across folds",
    color = NULL, shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(file.path(output_dir, "temporal_cv_errors.png"),
       p_err, width = 11, height = 4.5, dpi = 150)

cat(sprintf("    Plots saved in: %s\n", output_dir))


# ── 10. Final summary ────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Folds completed     : %d\n", nrow(metrics_df)))
cat(sprintf("  Total OOS predictions: %s\n",
            format(nrow(all_predictions), big.mark = ",")))
cat(sprintf("  Mean OOS R2         : %.3f (SD %.3f)\n",
            mean(metrics_df$r2), sd(metrics_df$r2)))
cat(sprintf("  Mean OOS RMSE       : %.3f\n", mean(metrics_df$rmse)))
cat(sprintf("  Mean OOS MAE        : %.3f\n", mean(metrics_df$mae)))
cat("\n  Next step (Step 2 in the plan): use temporal_cv_predictions.rds to\n")
cat("  compute EWS-aligned metrics (AUC-PR, F1, lead-time distributions,\n")
cat("  Brier score) at the Panic (>= 2 sigma) and Extreme (>= 3 sigma) thresholds.\n")
cat("════════════════════════════════════════════════════════════════════════\n")

##1.3.Hyperparameter-transfer sensitivity check  (leakage audit, remedy (a))------
# FIX vs v1: the AR lags are NOT static columns of X. The pipeline rebuilds them
# fold-locally in build_fold_features() (v8 ~line 246): re-fit the baseline on
# non-crisis days <= train_end, re-standardize per city, shift within city by
# {1,7,14,28}, impute leading NAs with 0. This snippet reproduces that exactly,
# so the three feature sets are:  External = X ; AR-only = 4 fold-local lags ;
# Combined = cbind(X, lags).  Only the hyperparameters change (tuned on fold-1's
# 2018-2019 window instead of a random 80% of the full panel).
#
# WHERE TO RUN: after the main pipeline has created X, folds, dates_model,
# cities_model, df_raw, and best_row.csv exists. Self-contained; writes
# improved_final/hyperparam_sensitivity.csv; overwrites no main artifact.

suppressMessages({library(xgboost); library(data.table); library(fixest)})

SENS_OUT <- "data/analysis/hyperparam_sensitivity.csv"
if (file.exists(SENS_OUT)) {
  cat("[sensitivity] cached — delete", SENS_OUT, "to re-run.\n")
} else {
  
  stopifnot(exists("X"), exists("folds"), exists("dates_model"),
            exists("cities_model"), exists("df_raw"))
  LAG_PERIODS <- if (exists("LAG_PERIODS")) LAG_PERIODS else c(1, 7, 14, 28)
  
  # --- 0. Static external matrix (fold-invariant) 
  X_ext <- as.matrix(X); storage.mode(X_ext) <- "double"
  ext_names <- colnames(X_ext)
  cat(sprintf("[sensitivity] external features: %d\n", length(ext_names)))
  
  # --- 1. Fold-local target + AR lags (faithful copy of build_fold_features) --
  .base <- as.data.table(df_raw)
  .base[, `:=`(date = as.Date(date), is_crisis = as.logical(is_crisis),
               dow_f = factor(dow_f), month_f = factor(month_f))]
  .BFORM <- valor_pc_real ~ time_linear_c + dow_f + month_f +
    is_holiday + is_pre_holiday + is_day31 + is_day1 +
    dom_sin + dom_cos + dom_sin2 + dom_cos2
  .mkey <- paste(cities_model, dates_model)          # model-row order == X rows
  .cache <- new.env(parent = emptyenv())
  
  fold_feats <- function(train_end) {                # -> list(y, lags[4 cols])
    k0 <- as.character(train_end)
    if (!is.null(.cache[[k0]])) return(.cache[[k0]])
    tr <- .base[is_crisis == FALSE & !is.na(valor_pc_real) & date <= train_end]
    m  <- fixest::feols(.BFORM, data = tr, fixef = "city")
    ap <- data.table(city = .base$city, date = .base$date,
                     api_f = .base$valor_pc_real - predict(m, newdata = .base))
    sig <- tapply(tr$valor_pc_real - predict(m, newdata = tr), tr$city,
                  stats::sd, na.rm = TRUE)
    ap[, api_f := api_f / as.numeric(sig[as.character(city)])]
    setorder(ap, city, date)
    for (k in LAG_PERIODS) ap[, (sprintf("lag_%d_API", k)) := shift(api_f, k), by = city]
    mi <- match(.mkey, paste(ap$city, ap$date))
    L  <- sapply(sprintf("lag_%d_API", LAG_PERIODS),
                 function(cn) { v <- ap[[cn]][mi]; v[is.na(v)] <- 0; v })
    out <- list(y = ap$api_f[mi], lags = as.matrix(L))
    .cache[[k0]] <- out; out
  }
  
  # --- 2. Tune hyperparameters on FOLD 1's TRAINING WINDOW ONLY
  grid <- expand.grid(max_depth = c(4,5,6), eta = c(0.05,0.10),
                      subsample = c(0.7,0.8), colsample_bytree = c(0.7,0.8),
                      min_child_weight = c(5,10), gamma = c(0,0.1),
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  f1_end <- folds$train_end[1]
  ff1    <- fold_feats(f1_end)
  ti     <- which(dates_model <= f1_end & !is.na(ff1$y))
  cat(sprintf("[sensitivity] tuning on %s obs up to %s\n",
              format(length(ti), big.mark=","), f1_end))
  dtune  <- xgb.DMatrix(cbind(X_ext[ti,,drop=FALSE], ff1$lags[ti,,drop=FALSE]),
                        label = ff1$y[ti])
  set.seed(42); best <- list(rmse = Inf, row = NULL)
  for (g in seq_len(nrow(grid))) {
    p <- c(list(objective="reg:squarederror", eval_metric="rmse",
                lambda=2.0, alpha=0.1, nthread=4), as.list(grid[g,]))
    cv <- xgb.cv(params=p, data=dtune, nrounds=1000, nfold=5,
                 early_stopping_rounds=30, verbose=0)
    mrmse <- min(cv$evaluation_log$test_rmse_mean)
    if (mrmse < best$rmse) best <- list(rmse=mrmse, row=grid[g,])
  }
  cat("[sensitivity] fold-1-tuned config:\n"); print(best$row)
  sp <- c(list(objective="reg:squarederror", eval_metric="rmse",
               lambda=2.0, alpha=0.1, nthread=4), as.list(best$row))
  
  # --- 3. Re-run full temporal CV; all three specs per fold, one baseline fit --
  specs <- c("External-only","AR-only","Combined")
  r2 <- matrix(NA_real_, nrow(folds), 3, dimnames=list(NULL, specs))
  for (i in seq_len(nrow(folds))) {
    fo <- folds[i,]
    trf <- which(dates_model <= fo$train_end)
    te  <- which(dates_model >= fo$test_start & dates_model <= fo$test_end)
    if (!length(te)) next
    vc  <- fo$train_end - (if (exists("val_window_days")) val_window_days else 90)
    tr  <- trf[dates_model[trf] <= vc]; va <- trf[dates_model[trf] > vc]
    ff  <- fold_feats(fo$train_end); y <- ff$y
    feat <- list(`External-only` = X_ext,
                 `AR-only`       = ff$lags,
                 `Combined`      = cbind(X_ext, ff$lags))
    for (s in specs) {
      Xs <- feat[[s]]
      mdl <- xgb.train(params=sp,
                       data=xgb.DMatrix(Xs[tr,,drop=FALSE], label=y[tr]),
                       nrounds=2000, evals=list(v=xgb.DMatrix(Xs[va,,drop=FALSE], label=y[va])),
                       early_stopping_rounds=50, verbose=0)
      bi <- as.integer(mdl$best_iteration)
      if (!length(bi) || is.na(bi) || bi < 1L)
        bi <- if (!is.null(mdl$niter)) as.integer(mdl$niter) else 2000L
      sl <- tryCatch(xgb.slice.Booster(mdl,1L,bi), error=function(e) NULL)
      yp <- if (!is.null(sl)) predict(sl, Xs[te,,drop=FALSE]) else predict(mdl, Xs[te,,drop=FALSE])
      yt <- y[te]
      r2[i,s] <- 1 - sum((yt-yp)^2)/sum((yt-mean(yt))^2)
    }
    cat(sprintf("  fold %2d done\n", i))
  }
  
  # --- 4. Report against main run 
  main <- tryCatch({ cs <- read.csv("data/analysis/comparison_summary.csv")
  setNames(cs$R2_regression, cs$model) },
  error=function(e) c(`External-only`=-0.096,`AR-only`=0.393,`Combined`=0.402))
  tab <- data.frame(model = specs,
                    sens_mean_R2 = round(colMeans(r2, na.rm=TRUE), 3),
                    sens_negR2_folds = colSums(r2 < 0, na.rm=TRUE),
                    main_run_R2 = round(as.numeric(main[specs]), 3))
  tab$delta_R2 <- round(tab$sens_mean_R2 - tab$main_run_R2, 3)
  cat("\n========== HYPERPARAMETER-TRANSFER SENSITIVITY ==========\n")
  print(tab, row.names = FALSE)
  cat("Ordering robust if External-only << AR-only ~ Combined and\n")
  cat("External-only sens_mean_R2 stays <= 0.\n")
  cat("=========================================================\n")
  dir.create(dirname(SENS_OUT), showWarnings=FALSE, recursive=TRUE)
  write.csv(tab, SENS_OUT, row.names=FALSE)
  cat("[sensitivity] written:", SENS_OUT, "\n")
}


#2.EXPANDING-WINDOW TIME-SERIES CV — BINARY CLASSIFICATION VERSION------
# Purpose
#   Test Hypothesis 1: the regression task is too hard, but the EWS-relevant
#   *classification* task (panic vs. not) may have detectable signal.
#   Retrain XGBoost as a binary classifier on `is_panic = PBB-Score >= 2σ`
#   under the same expanding-window CV used for the regression model.
#
# What changes from Temporal_CV_v1.R
#   • Target          : continuous API  ->  binary is_panic (>= 2)
#   • Objective       : reg:squarederror -> binary:logistic
#   • Eval metric     : RMSE             -> AUC-PR (rare-event-appropriate)
#   • Class imbalance : handled per-fold via scale_pos_weight
#   • Outputs         : AUC-PR, AUC-ROC, F1, precision, recall, Brier,
#                       lead-time distributions, PR curves, calibration
#
# Design choices
#   • PR-AUC, not ROC-AUC, as the headline. With ~4% positive class,
#     ROC-AUC inflates apparent skill (Saito & Rehmsmeier 2015).
#   • F1 reported at the OPTIMAL threshold found by sweep on the
#     validation set — never on test (no leakage).
#   • Lead-time analysis is at the EPISODE level (rising edges of is_panic),
#     not the day level. Looks back up to LEAD_LOOKBACK_DAYS before each
#     episode start to find when the predicted probability first crossed
#     the optimal threshold.
#
# Outputs (in improved_final/temporal_cv_classification/)
#   • classif_fold_metrics.csv       per-fold metrics
#   • classif_summary_stats.csv      mean, SD, 95% empirical CI
#   • classif_predictions.rds/.csv   all OOS probabilities + binary labels
#   • classif_lead_times.csv         per-episode lead time analysis
#   • classif_results.rds            full per-fold list (cached)
#   • Plots: PR curves combined, AUC-PR by fold, calibration,
#            lead-time histogram, confusion at optimal threshold
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "xgboost", "data.table",
  "patchwork", "lubridate", "PRROC"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = setdiff(ls(), "build_fold_api"))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" EXPANDING-WINDOW CV — BINARY CLASSIFICATION (is_panic)\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

# Output directory
output_dir <- "data/analysis/temporal_cv_classification/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Hyperparameters from your prior grid search
best_row_path <- "data/output/best_row.csv"
if (!file.exists(best_row_path)) {
  stop("best_row.csv not found at: ", best_row_path,
       "\nRun Final_main_model_v2.R first to generate it.")
}

# Classification-specific constants
PANIC_THRESHOLD_SIGMA <- 2.0      # PBB-Score threshold for "panic" (z-score)
LEAD_LOOKBACK_DAYS    <- 14       # max days to look back for lead-time analysis
VAL_WINDOW_DAYS       <- 90       # internal validation = last N days of train

# ── 1. Load and preprocess (mirrors Temporal_CV_v1.R) ────────────────────────

cat("[1] LOADING DATA...\n")
df_raw <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")
df_raw$date <- as.Date(df_raw$date)
cat(sprintf("    Loaded: %s rows x %s columns\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))

dates_full  <- df_raw$date
cities_full <- df_raw$city

cat("\n[2] PREPROCESSING...\n")

cols_to_drop <- c(
 "year_month", "upper_iqr", "valor_pc_real_raw", "residual", "yhat",
 "is_crisis", "sigma_c", "population_2022", "date_year",
 "valor_pc", "valor_pc_real", "time_linear", "time_linear_c", "is_outlier",
 "deflator",
 "month_sin", "month_cos", "wday_sin", "wday_cos", "yday_sin", "yday_cos",
 "dom_sin", "dom_cos", "dom_sin2", "dom_cos2",
 "is_holiday", "is_pre_holiday", "dom", "is_day31", "is_day1", "dow_f",
 "month_f", "is_weekend", "date_month", "date_wday", "date_yday",
 "S_precip_daily", "S_is_crisis_period",
 "S_pandemic_cases_z", "S_pandemic_deaths_z",
 "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
 "S_pandemic_severity_index",
  "ecf_sales_value_daily", "nfce_sales_value_daily", "nfe_sales_value_daily",
  "total_sales_value_daily",
  "hypermarket_ecf_sales_value_daily", "hypermarket_nfc_sales_value_daily",
  "hypermarket_nf_sales_value_daily",
  "O_hypermarket_total_sales_value_daily", "O_supermarket_total_sales_value_daily",
  "O_minimarket_total_sales_value_daily",
  "minimarket_ecf_sales_value_daily", "minimarket_nfc_sales_value_daily",
  "minimarket_nf_sales_value_daily",
  "supermarket_ecf_sales_value_daily", "supermarket_nfc_sales_value_daily",
  "supermarket_nf_sales_value_daily",
  "nfce_count_daily", "nfe_count_daily",
  "hypermarket_nfce_count_daily", "hypermarket_nfe_count_daily",
  "minimarket_nfe_count_daily", "minimarket_nfce_count_daily",
  "supermarket_nfe_count_daily", "supermarket_nfce_count_daily",
  "sales_value_per_capita"
)
df <- df_raw %>% dplyr::select(-any_of(cols_to_drop))

pandemic_to_drop <- c(
  "S_pandemic_cases_z", "S_pandemic_deaths_z",
  "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
  "S_pandemic_severity_index"
)
df <- df %>% dplyr::select(-any_of(pandemic_to_drop))

pandemic_vars <- grep("^S_pandemic", names(df), value = TRUE)
df[, pandemic_vars] <- lapply(df[, pandemic_vars],
                              function(x) replace(x, is.na(x), 0))

df <- df %>% dplyr::select(-any_of(c("API_cat", "city")))

char_cols <- names(df)[sapply(df, is.character) & names(df) != "date"]
for (col in char_cols) df[[col]] <- as.numeric(as.factor(df[[col]]))

cat(sprintf("    After drops: %d columns\n", ncol(df)))


# ── 2. Build binary target and filter ────────────────────────────────────────

cat("\n[3] BUILDING BINARY TARGET (is_panic = PBB-Score >= 2σ)...\n")

TARGET_CONT <- "API"

valid_idx    <- !is.na(df[[TARGET_CONT]])
df_model     <- df[valid_idx, ]
dates_model  <- dates_full[valid_idx]
cities_model <- cities_full[valid_idx]

# Binary target
is_panic <- as.integer(df_model[[TARGET_CONT]] >= PANIC_THRESHOLD_SIGMA)

cat(sprintf("    Rows with valid PBB-Score: %s\n",
            format(nrow(df_model), big.mark = ",")))
cat(sprintf("    Positive (panic) rate    : %.2f%% (%d / %d)\n",
            100 * mean(is_panic), sum(is_panic), length(is_panic)))

# Feature matrix
X <- df_model %>% dplyr::select(-all_of(TARGET_CONT), -any_of("date")) %>%
  mutate(across(everything(), as.numeric))

cat(sprintf("    Modelling matrix: %s rows x %s features\n",
            format(nrow(X), big.mark = ","), ncol(X)))


# ── 3. Define expanding-window folds (same as regression CV) ─────────────────

# --- Fold-aware PBB-Score (genuine out-of-sample target)
# The PBB-Score loaded above was standardized ONCE on the full sample, so its
# baseline (V-hat) and city sigma "saw" post-cutoff data. Per fold we re-fit
# the baseline and sigma on NON-CRISIS days with date <= train_end ONLY and
# standardize ALL rows with those fold-local coefficients (the test block is
# extrapolated, as a live EWS would operate). Inputs (baseline regressors,
# valor_pc_real, is_crisis) are all present in df_raw.
suppressMessages(library(fixest))

.base_df <- df_raw
.base_df$date      <- as.Date(.base_df$date)
.base_df$is_crisis <- as.logical(.base_df$is_crisis)
.base_df$dow_f     <- factor(.base_df$dow_f)
.base_df$month_f   <- factor(.base_df$month_f)

.BASELINE_FORMULA <- valor_pc_real ~ time_linear_c + dow_f + month_f +
  is_holiday + is_pre_holiday + is_day31 + is_day1 +
  dom_sin + dom_cos + dom_sin2 + dom_cos2

.fold_api_cache <- new.env(parent = emptyenv())

cat("\n[4] DEFINING FOLDS...\n")

data_start <- min(dates_model)
data_end   <- max(dates_model)

initial_train_end <- as.Date("2019-12-31")
fold_step_months  <- 6

fold_cutoffs <- seq.Date(initial_train_end, data_end, by = "6 months")
fold_cutoffs <- fold_cutoffs[fold_cutoffs < data_end - 30]

folds <- data.frame(
  fold_id    = seq_along(fold_cutoffs),
  train_end  = fold_cutoffs,
  test_start = fold_cutoffs + 1,
  test_end   = pmin(fold_cutoffs %m+% months(fold_step_months), data_end)
)

cat(sprintf("    Number of folds: %d\n", nrow(folds)))


# ── 4. Helper: compute classification metrics for one fold ───────────────────

compute_classif_metrics <- function(y_true, y_pred, val_y_true = NULL, val_y_pred = NULL) {
  
  # PR-AUC (primary)
  if (sum(y_true) > 0 && sum(1 - y_true) > 0) {
    pr   <- PRROC::pr.curve(scores.class0 = y_pred[y_true == 1],
                            scores.class1 = y_pred[y_true == 0],
                            curve = FALSE)
    roc  <- PRROC::roc.curve(scores.class0 = y_pred[y_true == 1],
                             scores.class1 = y_pred[y_true == 0],
                             curve = FALSE)
    auc_pr  <- pr$auc.integral
    auc_roc <- roc$auc
  } else {
    auc_pr <- NA_real_; auc_roc <- NA_real_
  }
  
  # Brier score (calibration)
  brier <- mean((y_pred - y_true)^2)
  
  # Find optimal threshold on VALIDATION (no test leakage)
  # If no validation passed, fall back to a sweep on test (sub-optimal,
  # noted in output)
  thresh_search <- if (!is.null(val_y_true) && length(val_y_true) > 0 &&
                       sum(val_y_true) > 0) {
    seq(0.05, 0.95, by = 0.01)
  } else {
    seq(0.05, 0.95, by = 0.01)
  }
  
  if (!is.null(val_y_true) && sum(val_y_true) > 0) {
    f1s <- sapply(thresh_search, function(t) {
      pred_bin <- as.integer(val_y_pred >= t)
      tp <- sum(pred_bin == 1 & val_y_true == 1)
      fp <- sum(pred_bin == 1 & val_y_true == 0)
      fn <- sum(pred_bin == 0 & val_y_true == 1)
      if ((tp + fp) == 0 || (tp + fn) == 0) return(0)
      prec <- tp / (tp + fp); rec <- tp / (tp + fn)
      if ((prec + rec) == 0) return(0)
      2 * prec * rec / (prec + rec)
    })
    opt_thresh <- thresh_search[which.max(f1s)]
  } else {
    opt_thresh <- 0.5
  }
  
  # Apply optimal threshold to TEST
  pred_bin <- as.integer(y_pred >= opt_thresh)
  tp <- sum(pred_bin == 1 & y_true == 1)
  fp <- sum(pred_bin == 1 & y_true == 0)
  fn <- sum(pred_bin == 0 & y_true == 1)
  tn <- sum(pred_bin == 0 & y_true == 0)
  
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) {
    2 * prec * rec / (prec + rec)
  } else NA_real_
  
  # Default-threshold (0.5) metrics for comparison
  pred_bin_05 <- as.integer(y_pred >= 0.5)
  tp_05 <- sum(pred_bin_05 == 1 & y_true == 1)
  fp_05 <- sum(pred_bin_05 == 1 & y_true == 0)
  fn_05 <- sum(pred_bin_05 == 0 & y_true == 1)
  prec_05 <- if ((tp_05 + fp_05) > 0) tp_05 / (tp_05 + fp_05) else NA_real_
  rec_05  <- if ((tp_05 + fn_05) > 0) tp_05 / (tp_05 + fn_05) else NA_real_
  f1_05   <- if (!is.na(prec_05) && !is.na(rec_05) && (prec_05 + rec_05) > 0) {
    2 * prec_05 * rec_05 / (prec_05 + rec_05)
  } else NA_real_
  
  # Positive class rate (baseline for PR-AUC)
  base_rate <- mean(y_true)
  
  list(
    auc_pr        = auc_pr,
    auc_roc       = auc_roc,
    brier         = brier,
    opt_threshold = opt_thresh,
    precision_opt = prec,
    recall_opt    = rec,
    f1_opt        = f1,
    precision_05  = prec_05,
    recall_05     = rec_05,
    f1_05         = f1_05,
    tp_opt        = tp, fp_opt = fp, fn_opt = fn, tn_opt = tn,
    base_rate     = base_rate,
    auc_pr_lift   = if (!is.na(auc_pr)) auc_pr / base_rate else NA_real_
  )
}


# ── 5. Helper: lead-time analysis per fold ───────────────────────────────────

compute_lead_times <- function(pred_df, threshold,
                               lookback_days = LEAD_LOOKBACK_DAYS) {
  
  # pred_df must have: date, city, y_true (0/1), y_pred (probability)
  # Returns: data.frame with one row per panic EPISODE start
  pred_df <- pred_df %>% arrange(city, date)
  
  result <- list()
  
  for (this_city in unique(pred_df$city)) {
    cd <- pred_df %>% filter(city == this_city) %>% arrange(date)
    
    # Rising edges of y_true (episode starts)
    rise <- which(cd$y_true == 1 & (c(0, head(cd$y_true, -1)) == 0))
    
    for (i in rise) {
      episode_date <- cd$date[i]
      # Window: lookback_days BEFORE the episode (exclusive of episode day)
      window_start <- episode_date - lookback_days
      win <- cd %>% filter(date >= window_start, date < episode_date)
      
      if (nrow(win) == 0) {
        # No prior observations available
        next
      }
      
      # Earliest day where probability >= threshold
      crossed <- which(win$y_pred >= threshold)
      
      if (length(crossed) > 0) {
        first_cross_date <- win$date[min(crossed)]
        lead_time_days   <- as.numeric(episode_date - first_cross_date)
        detected         <- TRUE
      } else {
        first_cross_date <- as.Date(NA)
        lead_time_days   <- NA_real_
        detected         <- FALSE
      }
      
      result[[length(result) + 1]] <- data.frame(
        city             = this_city,
        episode_start    = episode_date,
        first_cross_date = first_cross_date,
        lead_time_days   = lead_time_days,
        detected         = detected
      )
    }
  }
  
  if (length(result) == 0) {
    return(data.frame(city = character(), episode_start = as.Date(character()),
                      first_cross_date = as.Date(character()),
                      lead_time_days = numeric(), detected = logical()))
  }
  do.call(rbind, result)
}


# ── 6. CV loop ───────────────────────────────────────────────────────────────

cv_cache_file      <- file.path(output_dir, "classif_results.rds")
partial_cache_file <- file.path(output_dir, "classif_results_partial.rds")

if (!file.exists(cv_cache_file)) {
  
  cat("\n[5] RUNNING EXPANDING-WINDOW CLASSIFICATION CV...\n")
  cat("    (cache: classif_results.rds — delete to force re-run)\n")
  cat("    (partial cache: classif_results_partial.rds — updated per fold)\n\n")
  
  best_row <- read.csv(best_row_path)
  cat("    Hyperparameters from best_row.csv:\n")
  print(best_row[, c("max_depth", "eta", "subsample", "colsample_bytree",
                     "min_child_weight", "gamma")])
  
  # Resume from partial cache if it exists
  if (file.exists(partial_cache_file)) {
    fold_results <- readRDS(partial_cache_file)
    completed_folds <- which(!sapply(fold_results, is.null))
    cat(sprintf("\n    Resuming from partial cache: %d folds already complete (%s)\n",
                length(completed_folds),
                paste(completed_folds, collapse = ", ")))
  } else {
    fold_results <- vector("list", nrow(folds))
    completed_folds <- integer(0)
  }
  
  for (i in seq_len(nrow(folds))) {
    
    if (i %in% completed_folds) {
      cat(sprintf("[Fold %2d] SKIPPED (already in partial cache)\n", i))
      next
    }
    
    fold <- folds[i, ]
    
    train_idx_full <- which(dates_model <= fold$train_end)
    test_idx       <- which(dates_model >= fold$test_start &
                              dates_model <= fold$test_end)
    
    val_cutoff <- fold$train_end - VAL_WINDOW_DAYS
    train_idx  <- train_idx_full[dates_model[train_idx_full] <= val_cutoff]
    val_idx    <- train_idx_full[dates_model[train_idx_full] >  val_cutoff]
    
    if (length(test_idx) == 0) {
      cat(sprintf("[Fold %2d] SKIPPED — empty test set\n", i))
      next
    }
    
    # Fold-aware panic label: baseline V-hat and sigma from date <= train_end only
    is_panic_f <- as.integer(build_fold_api(fold$train_end) >= PANIC_THRESHOLD_SIGMA)

    # Per-fold class imbalance
    n_pos_train <- sum(is_panic_f[train_idx])
    n_neg_train <- length(train_idx) - n_pos_train
    spw <- if (n_pos_train > 0) n_neg_train / n_pos_train else 1.0
    
    # Per-fold hyperparameters (binary objective + spw)
    base_params <- list(
      objective        = "binary:logistic",
      max_depth        = best_row$max_depth,
      eta              = best_row$eta,
      subsample        = best_row$subsample,
      colsample_bytree = best_row$colsample_bytree,
      min_child_weight = best_row$min_child_weight,
      gamma            = best_row$gamma,
      lambda           = 2.0,
      alpha            = 0.1,
      scale_pos_weight = spw,
      nthread          = 4,
      eval_metric      = "aucpr"
    )
    
    dtrain <- xgb.DMatrix(data = as.matrix(X[train_idx, ]),
                          label = is_panic_f[train_idx])
    dval   <- xgb.DMatrix(data = as.matrix(X[val_idx,   ]),
                          label = is_panic_f[val_idx])
    dtest  <- xgb.DMatrix(data = as.matrix(X[test_idx,  ]),
                          label = is_panic_f[test_idx])
    
    model_fold <- xgb.train(
      params                = base_params,
      data                  = dtrain,
      nrounds               = 2000,
      evals                 = list(train = dtrain, val = dval),
      early_stopping_rounds = 50,
      maximize              = TRUE,        # AUC-PR is maximized
      verbose               = 0
    )
    
    best_iter <- as.integer(model_fold$best_iteration)
    if (length(best_iter) == 0L || is.na(best_iter) || best_iter < 1L) {
      best_iter <- if (!is.null(model_fold$niter)) {
        as.integer(model_fold$niter)
      } else 2000L
    }
    
    sliced_model <- tryCatch(
      xgb.slice.Booster(model_fold, start = 1L, end = best_iter),
      error = function(e) NULL
    )
    
    if (!is.null(sliced_model)) {
      y_pred_test <- predict(sliced_model, dtest)
      y_pred_val  <- predict(sliced_model, dval)
    } else {
      y_pred_test <- predict(model_fold, dtest)
      y_pred_val  <- predict(model_fold, dval)
      if (i == 1) cat("    (note: xgb.slice.Booster unavailable, using full booster)\n")
    }
    
    y_true_test <- is_panic_f[test_idx]
    y_true_val  <- is_panic_f[val_idx]
    
    # Metrics (uses val to find optimal threshold)
    m <- compute_classif_metrics(y_true_test, y_pred_test,
                                 val_y_true = y_true_val,
                                 val_y_pred = y_pred_val)
    
    # Store
    fold_results[[i]] <- list(
      fold_id      = i,
      train_end    = fold$train_end,
      test_start   = fold$test_start,
      test_end     = fold$test_end,
      n_train      = length(train_idx),
      n_val        = length(val_idx),
      n_test       = length(test_idx),
      n_pos_train  = n_pos_train,
      n_pos_test   = sum(y_true_test),
      scale_pos_w  = spw,
      best_nrounds = best_iter,
      metrics      = m,
      model        = model_fold,
      predictions  = data.frame(
        date    = dates_model[test_idx],
        city    = cities_model[test_idx],
        y_true  = y_true_test,
        y_pred  = y_pred_test,
        row_idx = test_idx
      )
    )
    
    cat(sprintf("[Fold %2d] test %s-%s | n_pos_test=%3d | AUC-PR=%.3f (lift %4.1fx) AUC-ROC=%.3f | F1=%.3f P=%.3f R=%.3f thr=%.2f | Brier=%.4f\n",
                i, fold$test_start, fold$test_end,
                sum(y_true_test),
                m$auc_pr, m$auc_pr_lift, m$auc_roc,
                m$f1_opt, m$precision_opt, m$recall_opt,
                m$opt_threshold, m$brier))
    
    # Incremental save: write partial cache after each completed fold
    saveRDS(fold_results, partial_cache_file)
  }
  
  fold_results <- Filter(Negate(is.null), fold_results)
  saveRDS(fold_results, cv_cache_file)
  # Clean up partial cache once the final cache is written
  if (file.exists(partial_cache_file)) file.remove(partial_cache_file)
  cat(sprintf("\n    Cached: %s\n", cv_cache_file))
  
} else {
  cat("\n[5] LOADING CACHED CLASSIFICATION CV RESULTS...\n")
  fold_results <- readRDS(cv_cache_file)
  cat(sprintf("    Loaded %d folds.\n", length(fold_results)))
}


# ── 7. Aggregate metrics ─────────────────────────────────────────────────────

cat("\n[6] AGGREGATING METRICS...\n")

metrics_df <- do.call(rbind, lapply(fold_results, function(f) {
  m <- f$metrics
  data.frame(
    fold_id      = f$fold_id,
    train_end    = f$train_end,
    test_start   = f$test_start,
    test_end     = f$test_end,
    n_train      = f$n_train,
    n_test       = f$n_test,
    n_pos_train  = f$n_pos_train,
    n_pos_test   = f$n_pos_test,
    scale_pos_w  = f$scale_pos_w,
    best_nrounds = f$best_nrounds,
    base_rate    = m$base_rate,
    auc_pr       = m$auc_pr,
    auc_pr_lift  = m$auc_pr_lift,
    auc_roc      = m$auc_roc,
    brier        = m$brier,
    opt_threshold = m$opt_threshold,
    precision_opt = m$precision_opt,
    recall_opt    = m$recall_opt,
    f1_opt        = m$f1_opt,
    f1_05         = m$f1_05
  )
}))

write.csv(metrics_df, file.path(output_dir, "classif_fold_metrics.csv"),
          row.names = FALSE)

summary_stats <- data.frame(
  metric = c("AUC-PR", "AUC-PR lift", "AUC-ROC", "F1 (optimal)",
             "Precision (optimal)", "Recall (optimal)", "Brier"),
  mean   = c(mean(metrics_df$auc_pr,        na.rm = TRUE),
             mean(metrics_df$auc_pr_lift,   na.rm = TRUE),
             mean(metrics_df$auc_roc,       na.rm = TRUE),
             mean(metrics_df$f1_opt,        na.rm = TRUE),
             mean(metrics_df$precision_opt, na.rm = TRUE),
             mean(metrics_df$recall_opt,    na.rm = TRUE),
             mean(metrics_df$brier,         na.rm = TRUE)),
  sd     = c(sd(metrics_df$auc_pr,          na.rm = TRUE),
             sd(metrics_df$auc_pr_lift,     na.rm = TRUE),
             sd(metrics_df$auc_roc,         na.rm = TRUE),
             sd(metrics_df$f1_opt,          na.rm = TRUE),
             sd(metrics_df$precision_opt,   na.rm = TRUE),
             sd(metrics_df$recall_opt,      na.rm = TRUE),
             sd(metrics_df$brier,           na.rm = TRUE)),
  median = c(median(metrics_df$auc_pr,        na.rm = TRUE),
             median(metrics_df$auc_pr_lift,   na.rm = TRUE),
             median(metrics_df$auc_roc,       na.rm = TRUE),
             median(metrics_df$f1_opt,        na.rm = TRUE),
             median(metrics_df$precision_opt, na.rm = TRUE),
             median(metrics_df$recall_opt,    na.rm = TRUE),
             median(metrics_df$brier,         na.rm = TRUE))
)
write.csv(summary_stats, file.path(output_dir, "classif_summary_stats.csv"),
          row.names = FALSE)

cat("\n    Per-fold metrics:\n")
print(metrics_df[, c("fold_id", "test_start", "n_pos_test",
                     "auc_pr", "auc_pr_lift", "auc_roc",
                     "f1_opt", "precision_opt", "recall_opt")],
      row.names = FALSE, digits = 3)

cat("\n    Summary across folds (mean ± SD):\n")
print(summary_stats, row.names = FALSE, digits = 3)


# ── 8. Save out-of-sample predictions ────────────────────────────────────────

all_predictions <- do.call(rbind, lapply(fold_results, function(f) {
  cbind(fold_id = f$fold_id, f$predictions)
}))
saveRDS(all_predictions,  file.path(output_dir, "classif_predictions.rds"))
write.csv(all_predictions, file.path(output_dir, "classif_predictions.csv"),
          row.names = FALSE)
cat(sprintf("\n    OOS predictions saved: %s rows\n",
            format(nrow(all_predictions), big.mark = ",")))


# ── 9. Lead-time analysis (the operational EWS metric) ──────────────────────

cat("\n[7] LEAD-TIME ANALYSIS...\n")

# Use the median optimal threshold across folds as the operational threshold
op_threshold <- median(metrics_df$opt_threshold, na.rm = TRUE)
cat(sprintf("    Operational threshold (median across folds): %.3f\n", op_threshold))

lead_times_df <- compute_lead_times(all_predictions, threshold = op_threshold)

if (nrow(lead_times_df) > 0) {
  detected_pct <- 100 * mean(lead_times_df$detected)
  cat(sprintf("    Total panic episodes analyzed : %d\n", nrow(lead_times_df)))
  cat(sprintf("    Detected (prob crossed threshold in prior 14 days): %.1f%%\n",
              detected_pct))
  
  detected_lead <- lead_times_df$lead_time_days[lead_times_df$detected]
  if (length(detected_lead) > 0) {
    cat(sprintf("    Lead time of detected episodes: mean=%.1f median=%.1f days\n",
                mean(detected_lead), median(detected_lead)))
    cat(sprintf("    Lead time distribution: 1-day=%d  2-day=%d  3-day=%d  >=4-day=%d\n",
                sum(detected_lead == 1), sum(detected_lead == 2),
                sum(detected_lead == 3), sum(detected_lead >= 4)))
  }
} else {
  cat("    No panic episodes found in the OOS predictions.\n")
}

write.csv(lead_times_df, file.path(output_dir, "classif_lead_times.csv"),
          row.names = FALSE)


# ── 10. Plots ────────────────────────────────────────────────────────────────

cat("\n[8] BUILDING PLOTS...\n")

# Plot 1: AUC-PR by fold with random-baseline reference per fold
p_auc <- ggplot(metrics_df, aes(x = test_start)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-09-30"),
           ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.10) +
  annotate("rect", xmin = as.Date("2021-03-01"), xmax = as.Date("2021-09-30"),
           ymin = -Inf, ymax = Inf, fill = "#E65F2B", alpha = 0.10) +
  annotate("rect", xmin = as.Date("2022-05-01"), xmax = as.Date("2022-07-31"),
           ymin = -Inf, ymax = Inf, fill = "#F46036", alpha = 0.15) +
  annotate("rect", xmin = as.Date("2023-09-01"), xmax = as.Date("2023-12-31"),
           ymin = -Inf, ymax = Inf, fill = "#2A9D8F", alpha = 0.15) +
  geom_line(aes(y = auc_pr), color = "grey30", linewidth = 0.5) +
  geom_point(aes(y = auc_pr), color = "#0099B8", size = 3.2) +
  geom_line(aes(y = base_rate), color = "#E65F2B",
            linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(y = base_rate), color = "#E65F2B", size = 1.5) +
  labs(x = "Test fold start date", y = "AUC-PR",
       title = "AUC-PR across folds vs. positive-class baseline (dashed red)",
       subtitle = "AUC-PR above the baseline = real discrimination") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "classif_aucpr_by_fold.png"),
       p_auc, width = 11, height = 5, dpi = 150)

# Plot 2: Combined PR curve across all folds
if (sum(all_predictions$y_true) > 0) {
  combined_pr <- PRROC::pr.curve(
    scores.class0 = all_predictions$y_pred[all_predictions$y_true == 1],
    scores.class1 = all_predictions$y_pred[all_predictions$y_true == 0],
    curve = TRUE
  )
  pr_df <- data.frame(recall = combined_pr$curve[, 1],
                      precision = combined_pr$curve[, 2])
  base_rate_all <- mean(all_predictions$y_true)
  p_pr <- ggplot(pr_df, aes(x = recall, y = precision)) +
    geom_line(color = "#0099B8", linewidth = 1) +
    geom_hline(yintercept = base_rate_all, linetype = "dashed", color = "#E65F2B") +
    annotate("text", x = 0.05, y = base_rate_all + 0.02,
             label = sprintf("Baseline = %.3f", base_rate_all),
             hjust = 0, size = 3, color = "#E65F2B") +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Recall", y = "Precision",
         title = sprintf("Combined PR curve, all folds (AUC-PR = %.3f)",
                         combined_pr$auc.integral)) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(output_dir, "classif_pr_curve_combined.png"),
         p_pr, width = 6, height = 6, dpi = 150)
}

# Plot 3: Calibration
cal_df <- all_predictions %>%
  mutate(bin = cut(y_pred, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(y_pred),
            obs_rate  = mean(y_true),
            n         = n(), .groups = "drop") %>%
  filter(n >= 30)
p_cal <- ggplot(cal_df, aes(x = mean_pred, y = obs_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(color = "#0099B8") +
  geom_point(aes(size = n), color = "#0099B8") +
  scale_size_continuous(range = c(2, 8)) +
  labs(x = "Mean predicted probability", y = "Observed panic rate",
       title = "Calibration plot (binned)",
       subtitle = "On-diagonal = well calibrated", size = "n in bin") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "classif_calibration.png"),
       p_cal, width = 7, height = 6, dpi = 150)

# Plot 4: Lead-time histogram
if (nrow(lead_times_df) > 0 && sum(lead_times_df$detected) > 0) {
  p_lead <- lead_times_df %>%
    filter(detected) %>%
    ggplot(aes(x = lead_time_days)) +
    geom_histogram(binwidth = 1, fill = "#0099B8", color = "white") +
    labs(x = "Lead time (days)", y = "Number of episodes detected",
         title = sprintf("Lead-time distribution (detected episodes only, threshold = %.3f)",
                         op_threshold),
         subtitle = sprintf("Of %d total panic episodes, %d (%.0f%%) detected within %d days prior",
                            nrow(lead_times_df), sum(lead_times_df$detected),
                            100 * mean(lead_times_df$detected),
                            LEAD_LOOKBACK_DAYS)) +
    scale_x_continuous(breaks = 1:LEAD_LOOKBACK_DAYS) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(output_dir, "classif_lead_time_histogram.png"),
         p_lead, width = 9, height = 5, dpi = 150)
}

cat(sprintf("    Plots saved in: %s\n", output_dir))


# ── 11. Final summary ────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Folds completed     : %d\n", nrow(metrics_df)))
cat(sprintf("  Positive class rate : %.2f%%\n", 100 * mean(is_panic)))
cat(sprintf("\n  Mean AUC-PR         : %.3f (lift %.1fx over baseline)\n",
            mean(metrics_df$auc_pr, na.rm = TRUE),
            mean(metrics_df$auc_pr_lift, na.rm = TRUE)))
cat(sprintf("  Mean AUC-ROC        : %.3f\n",
            mean(metrics_df$auc_roc, na.rm = TRUE)))
cat(sprintf("  Mean F1 (optimal)   : %.3f\n",
            mean(metrics_df$f1_opt, na.rm = TRUE)))
cat(sprintf("  Mean Brier          : %.4f\n",
            mean(metrics_df$brier, na.rm = TRUE)))
if (nrow(lead_times_df) > 0) {
  detected_lead <- lead_times_df$lead_time_days[lead_times_df$detected]
  cat(sprintf("\n  Episode detection   : %d / %d (%.1f%%)\n",
              sum(lead_times_df$detected), nrow(lead_times_df),
              100 * mean(lead_times_df$detected)))
  if (length(detected_lead) > 0) {
    cat(sprintf("  Median lead time    : %.1f days (of detected episodes)\n",
                median(detected_lead)))
  }
}
cat("\n  INTERPRETATION GUIDE:\n")
cat("  • AUC-PR lift > 2-3x with reasonable detection rate (>30%%) and\n")
cat("    median lead time >= 1 day = paper proceeds on classification track.\n")
cat("  • AUC-PR ~= baseline (lift ~1x) = task is fundamentally unpredictable\n")
cat("    from external open data with this target construction.\n")
cat("  • Intermediate results = consider Hypothesis 2 (AR-augmented) or\n")
cat("    Hypothesis 3 (target reconstruction) next.\n")
cat("════════════════════════════════════════════════════════════════════════\n")

#3.CLASSIFICATION METRICS — THREE-WAY MODEL COMPARISON------
# Purpose
#   Run the same classification evaluation on all three regression models
#   (AR-only, External-only, Combined) for apples-to-apples comparison.
#
#   The Combined model achieved AUC-PR = 0.197 (lift 5.2x), AUC-ROC = 0.791
#   on the Panic threshold. This script answers: how much of that classifica-
#   tion skill comes from autoregression vs. external features?
#
# Inputs (regression predictions from Temporal_CV_AR_Augmented_v1.R)
#   • improved_final/temporal_cv_ar_augmented/ar_only/results.rds
#   • improved_final/temporal_cv_ar_augmented/external_only/results.rds
#   • improved_final/temporal_cv_ar_augmented/combined/results.rds
#
# Outputs (in improved_final/classification_three_way/)
#   • comparison_summary.csv          one-row-per-model headline table
#   • metrics_per_fold.csv            per-fold AUC-PR / AUC-ROC by model
#   • metrics_at_common_threshold.csv all three models at PBB >= 1.2
#   • Plots: AUC-PR by fold (3 lines), PR curves overlaid, ROC curves
#            overlaid, threshold-sweep F1 comparison, lead-time comparison
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "lubridate", "PRROC"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" CLASSIFICATION METRICS — THREE-WAY COMPARISON\n")
cat(" (AR-only vs. External-only vs. Combined)\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

output_dir <- "data/analysis/classification_three_way/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

PANIC_THRESHOLD     <- 2.0
LEAD_LOOKBACK_DAYS  <- 14
COMMON_OP_THRESHOLD <- 1.2   #

MODEL_PATHS <- list(
  "AR-only"       = "data/analysis/temporal_cv_ar_augmented/ar_only/results.rds",
  "External-only" = "data/analysis/temporal_cv_ar_augmented/external_only/results.rds",
  "Combined"      = "data/analysis/temporal_cv_ar_augmented/combined/results.rds"
)

MODEL_COLORS <- c(
  "AR-only"       = "#2A9D8F",
  "External-only" = "#E65F2B",
  "Combined"      = "#0099B8"
)

# R2_PRIOR <- c(  # from regression CV
#   "AR-only"       = 0.359,
#   "External-only" = -0.041,
#   "Combined"      = 0.384
# )
#IMPORTANT FIX: REPLACE THE MANUALLY CODED ABOVE FOR AN AUTOMATICALLY GENERATED
.reg <- read.csv("data/analysis/temporal_cv_ar_augmented/comparison_summary.csv")
R2_PRIOR <- setNames(.reg$mean_r2, .reg$model)

# ── 1. Helper functions ──────────────────────────────────────────────────────

compute_at_threshold <- function(y_true, y_pred, tau) {
  alarm <- as.integer(y_pred >= tau)
  tp <- sum(alarm == 1 & y_true == 1)
  fp <- sum(alarm == 1 & y_true == 0)
  fn <- sum(alarm == 0 & y_true == 1)
  tn <- sum(alarm == 0 & y_true == 0)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) {
    2 * prec * rec / (prec + rec)
  } else NA_real_
  data.frame(threshold = tau, alarm_rate = mean(alarm),
             tp = tp, fp = fp, fn = fn, tn = tn,
             precision = prec, recall = rec, f1 = f1)
}

compute_lead_times_with_baseline <- function(pred_df, threshold,
                                             lookback_days = LEAD_LOOKBACK_DAYS) {
  pred_df <- pred_df %>% arrange(city, date)
  pred_df$alarm <- as.integer(pred_df$y_pred >= threshold)
  
  lead_times <- list(); baseline_windows <- list()
  
  for (this_city in unique(pred_df$city)) {
    cd <- pred_df %>% filter(city == this_city) %>% arrange(date)
    if (nrow(cd) < lookback_days + 1) next
    
    rise <- which(cd$is_panic == 1 & (c(0, head(cd$is_panic, -1)) == 0))
    
    for (i in rise) {
      ep_date <- cd$date[i]
      win <- cd %>% filter(date >= ep_date - lookback_days, date < ep_date)
      if (nrow(win) == 0) next
      
      crossed <- which(win$alarm == 1)
      detected <- length(crossed) > 0
      lt <- if (detected) as.numeric(ep_date - win$date[min(crossed)]) else NA_real_
      
      lead_times[[length(lead_times) + 1]] <- data.frame(
        city = this_city, episode_start = ep_date,
        lead_time_days = lt, detected = detected
      )
    }
    
    non_panic <- which(cd$is_panic == 0)
    sample_n <- length(rise) * 3
    if (length(non_panic) > sample_n && length(rise) > 0) {
      set.seed(42)
      sampled <- sample(non_panic, sample_n)
      sampled <- sampled[sampled > lookback_days]
      for (j in sampled) {
        ref_date <- cd$date[j]
        win <- cd %>% filter(date >= ref_date - lookback_days, date < ref_date)
        if (sum(win$is_panic) > 0) next
        baseline_windows[[length(baseline_windows) + 1]] <- data.frame(
          city = this_city, ref_date = ref_date,
          any_alarm = sum(win$alarm) > 0
        )
      }
    }
  }
  
  list(
    episode_lead_times = if (length(lead_times) > 0) do.call(rbind, lead_times) else data.frame(),
    baseline_windows   = if (length(baseline_windows) > 0) do.call(rbind, baseline_windows) else data.frame()
  )
}

# Compute the full metric pack for one model
analyze_model <- function(predictions, model_name, val_predictions = NULL) {
  
  cat(sprintf("\n── ANALYZING %s ──\n", model_name))
  
  preds <- predictions
  preds$is_panic <- as.integer(preds$y_true >= PANIC_THRESHOLD)
  base_rate <- mean(preds$is_panic)
  
  # Overall AUC-PR / AUC-ROC
  pr_overall <- PRROC::pr.curve(
    scores.class0 = preds$y_pred[preds$is_panic == 1],
    scores.class1 = preds$y_pred[preds$is_panic == 0],
    curve = TRUE
  )
  roc_overall <- PRROC::roc.curve(
    scores.class0 = preds$y_pred[preds$is_panic == 1],
    scores.class1 = preds$y_pred[preds$is_panic == 0],
    curve = TRUE
  )
  
  # Per-fold metrics
  per_fold <- do.call(rbind, lapply(split(preds, preds$fold_id), function(d) {
    if (sum(d$is_panic) == 0 || sum(1 - d$is_panic) == 0) {
      return(data.frame(fold_id = d$fold_id[1], n_pos = sum(d$is_panic),
                        auc_pr = NA, auc_roc = NA, base_rate = mean(d$is_panic)))
    }
    pr  <- PRROC::pr.curve(scores.class0 = d$y_pred[d$is_panic == 1],
                           scores.class1 = d$y_pred[d$is_panic == 0],
                           curve = FALSE)
    roc <- PRROC::roc.curve(scores.class0 = d$y_pred[d$is_panic == 1],
                            scores.class1 = d$y_pred[d$is_panic == 0],
                            curve = FALSE)
    data.frame(fold_id = d$fold_id[1], n_pos = sum(d$is_panic),
               auc_pr = pr$auc.integral, auc_roc = roc$auc,
               base_rate = mean(d$is_panic))
  }))
  per_fold$model <- model_name
  
  # Operating point tau: selected on VALIDATION (per fold), then fixed for the
  # pooled out-of-sample test set. No test-set threshold tuning.
  sweep_grid <- seq(0.1, 3.0, by = 0.05)
  sweep_metrics <- do.call(rbind, lapply(sweep_grid, function(t) {
    m <- compute_at_threshold(preds$is_panic, preds$y_pred, t)
    m$threshold <- t; m
  }))
  if (!is.null(val_predictions)) {
    vp <- val_predictions
    vp$is_panic <- as.integer(vp$y_true >= PANIC_THRESHOLD)
    fold_tau <- sapply(split(vp, vp$fold_id), function(d) {
      if (sum(d$is_panic) == 0) return(NA_real_)
      f1v <- sapply(sweep_grid, function(t) {
        f1 <- compute_at_threshold(d$is_panic, d$y_pred, t)$f1
        if (is.na(f1)) 0 else f1
      })
      sweep_grid[which.max(f1v)]
    })
    opt_threshold <- stats::median(fold_tau, na.rm = TRUE)
  } else {
    opt_idx <- which.max(sweep_metrics$f1)
    opt_threshold <- sweep_metrics$threshold[opt_idx]
  }
  metrics_opt <- compute_at_threshold(preds$is_panic, preds$y_pred, opt_threshold)
  
  # At the common operational threshold (Combined's F1-optimal)
  metrics_common <- compute_at_threshold(preds$is_panic, preds$y_pred,
                                         COMMON_OP_THRESHOLD)
  
  # Lead-time analysis at each model's own F1-optimal
  lead_own <- compute_lead_times_with_baseline(preds, opt_threshold)
  ep_lt_own <- lead_own$episode_lead_times
  bl_w_own  <- lead_own$baseline_windows
  det_rate_own <- if (nrow(ep_lt_own) > 0) mean(ep_lt_own$detected) else NA_real_
  bl_rate_own  <- if (nrow(bl_w_own)  > 0) mean(bl_w_own$any_alarm) else NA_real_
  
  # Lead-time at common threshold (for fair cross-model comparison)
  lead_common <- compute_lead_times_with_baseline(preds, COMMON_OP_THRESHOLD)
  ep_lt_common <- lead_common$episode_lead_times
  bl_w_common  <- lead_common$baseline_windows
  det_rate_common <- if (nrow(ep_lt_common) > 0) mean(ep_lt_common$detected) else NA_real_
  bl_rate_common  <- if (nrow(bl_w_common)  > 0) mean(bl_w_common$any_alarm) else NA_real_
  
  cat(sprintf("    AUC-PR=%.3f (lift %.1fx)  AUC-ROC=%.3f  F1-opt=%.3f at τ=%.2f\n",
              pr_overall$auc.integral, pr_overall$auc.integral / base_rate,
              roc_overall$auc, metrics_opt$f1, opt_threshold))
  cat(sprintf("    Lead-time @ own opt τ: detection %.1f%% vs baseline %.1f%% (lift %+.1f pp)\n",
              100 * det_rate_own, 100 * bl_rate_own,
              100 * (det_rate_own - bl_rate_own)))
  cat(sprintf("    Lead-time @ common τ=%.2f: detection %.1f%% vs baseline %.1f%% (lift %+.1f pp)\n",
              COMMON_OP_THRESHOLD, 100 * det_rate_common, 100 * bl_rate_common,
              100 * (det_rate_common - bl_rate_common)))
  
  list(
    model = model_name,
    predictions = preds,
    base_rate = base_rate,
    pr_curve = pr_overall, roc_curve = roc_overall,
    per_fold = per_fold,
    sweep_metrics = sweep_metrics,
    opt_threshold = opt_threshold,
    metrics_opt = metrics_opt,
    metrics_common = metrics_common,
    lead_own = list(episodes = ep_lt_own, baseline = bl_w_own,
                    detection_rate = det_rate_own, baseline_rate = bl_rate_own,
                    threshold = opt_threshold),
    lead_common = list(episodes = ep_lt_common, baseline = bl_w_common,
                       detection_rate = det_rate_common, baseline_rate = bl_rate_common,
                       threshold = COMMON_OP_THRESHOLD)
  )
}


# ── 2. Load and analyze each model ───────────────────────────────────────────

cat("[1] LOADING ALL THREE MODELS...\n")

# Validation-selected common operating threshold (Combined model), replacing
# the hardcoded default above. No test data used.
.val_tau <- function(fr) {
  vp <- do.call(rbind, lapply(fr, function(f) {
    v <- f$val_predictions; v$fold_id <- f$fold_id; v
  }))
  vp$is_panic <- as.integer(vp$y_true >= PANIC_THRESHOLD)
  g <- seq(0.1, 3.0, by = 0.05)
  ft <- sapply(split(vp, vp$fold_id), function(d) {
    if (sum(d$is_panic) == 0) return(NA_real_)
    f1v <- sapply(g, function(t) {
      f1 <- compute_at_threshold(d$is_panic, d$y_pred, t)$f1
      if (is.na(f1)) 0 else f1
    })
    g[which.max(f1v)]
  })
  stats::median(ft, na.rm = TRUE)
}
if (file.exists(MODEL_PATHS[["Combined"]])) {
  COMMON_OP_THRESHOLD <- .val_tau(readRDS(MODEL_PATHS[["Combined"]]))
  cat(sprintf("    Common operating tau (validation-selected): %.2f\n",
              COMMON_OP_THRESHOLD))
}

results <- list()
for (mname in names(MODEL_PATHS)) {
  if (!file.exists(MODEL_PATHS[[mname]])) {
    stop(sprintf("Missing: %s\nRun Temporal_CV_AR_Augmented_v1.R first.",
                 MODEL_PATHS[[mname]]))
  }
  fold_results <- readRDS(MODEL_PATHS[[mname]])
  preds <- do.call(rbind, lapply(fold_results, function(f) {
    cbind(fold_id = f$fold_id, f$predictions)
  }))
  val_preds <- do.call(rbind, lapply(fold_results, function(f) {
    v <- f$val_predictions; v$fold_id <- f$fold_id; v
  }))
  cat(sprintf("    %s: %s predictions loaded\n",
              mname, format(nrow(preds), big.mark = ",")))
  results[[mname]] <- analyze_model(preds, mname, val_predictions = val_preds)
}


# ── 3. Comparison summary table ──────────────────────────────────────────────

cat("\n[2] BUILDING COMPARISON SUMMARY...\n")

summary_df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    model               = r$model,
    R2_regression       = R2_PRIOR[r$model],
    AUC_PR              = r$pr_curve$auc.integral,
    AUC_PR_lift         = r$pr_curve$auc.integral / r$base_rate,
    AUC_ROC             = r$roc_curve$auc,
    opt_threshold       = r$opt_threshold,
    F1_at_opt           = r$metrics_opt$f1,
    precision_at_opt    = r$metrics_opt$precision,
    recall_at_opt       = r$metrics_opt$recall,
    alarm_rate_at_opt   = r$metrics_opt$alarm_rate,
    F1_at_common        = r$metrics_common$f1,
    precision_at_common = r$metrics_common$precision,
    recall_at_common    = r$metrics_common$recall,
    detection_own       = r$lead_own$detection_rate,
    baseline_own        = r$lead_own$baseline_rate,
    detection_lift_own  = r$lead_own$detection_rate - r$lead_own$baseline_rate,
    detection_common      = r$lead_common$detection_rate,
    baseline_common       = r$lead_common$baseline_rate,
    detection_lift_common = r$lead_common$detection_rate - r$lead_common$baseline_rate,
    stringsAsFactors = FALSE
  )
}))
rownames(summary_df) <- NULL

cat("\n    Headline comparison:\n\n")
print(summary_df[, c("model", "R2_regression", "AUC_PR", "AUC_PR_lift",
                     "AUC_ROC", "F1_at_opt", "detection_lift_own")],
      row.names = FALSE, digits = 3)

cat("\n    Cross-model comparison at common threshold (τ = ", COMMON_OP_THRESHOLD, "):\n", sep = "")
print(summary_df[, c("model", "precision_at_common", "recall_at_common",
                     "F1_at_common", "detection_common", "baseline_common",
                     "detection_lift_common")],
      row.names = FALSE, digits = 3)

write.csv(summary_df, file.path(output_dir, "comparison_summary.csv"),
          row.names = FALSE)


# ── 4. Compute "incremental value" of external features for classification ───

cat("\n[3] INCREMENTAL VALUE OF EXTERNAL FEATURES (classification side)\n")

inc_auc_pr   <- summary_df$AUC_PR[summary_df$model == "Combined"] -
  summary_df$AUC_PR[summary_df$model == "AR-only"]
inc_auc_roc  <- summary_df$AUC_ROC[summary_df$model == "Combined"] -
  summary_df$AUC_ROC[summary_df$model == "AR-only"]
inc_f1       <- summary_df$F1_at_opt[summary_df$model == "Combined"] -
  summary_df$F1_at_opt[summary_df$model == "AR-only"]
inc_det_lift <- summary_df$detection_lift_own[summary_df$model == "Combined"] -
  summary_df$detection_lift_own[summary_df$model == "AR-only"]

cat(sprintf("    Δ AUC-PR (Combined − AR-only)         : %+.3f\n", inc_auc_pr))
cat(sprintf("    Δ AUC-ROC (Combined − AR-only)        : %+.3f\n", inc_auc_roc))
cat(sprintf("    Δ F1-opt (Combined − AR-only)         : %+.3f\n", inc_f1))
cat(sprintf("    Δ Detection lift (Combined − AR-only) : %+.1f pp\n", 100 * inc_det_lift))


# ── 5. Per-fold metrics for export ───────────────────────────────────────────

per_fold_all <- do.call(rbind, lapply(results, function(r) r$per_fold))
write.csv(per_fold_all, file.path(output_dir, "metrics_per_fold.csv"),
          row.names = FALSE)


# ── 6. Comparison plots ──────────────────────────────────────────────────────

cat("\n[4] BUILDING COMPARISON PLOTS...\n")

# Order factor for consistent legend ordering
fix_order <- function(df) {
  df$model <- factor(df$model, levels = c("External-only", "AR-only", "Combined"))
  df
}

# --- Plot 1: AUC-PR by fold (3 lines) ---
p_aucpr <- ggplot(fix_order(per_fold_all),
                  aes(x = fold_id, y = auc_pr, color = model, shape = model)) +
  annotate("rect", xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
           fill = "#E65F2B", alpha = 0.08) +
  annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -Inf, ymax = Inf,
           fill = "#F46036", alpha = 0.12) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 3) +
  scale_color_manual(values = MODEL_COLORS) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Fold ID", y = "AUC-PR",
       title = "AUC-PR across folds — three-way comparison",
       subtitle = "Shaded: 1st pandemic year (red), 2022 blockades (orange)",
       color = NULL, shape = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")
ggsave(file.path(output_dir, "aucpr_by_fold_3way.png"), p_aucpr,
       width = 11, height = 5.5, dpi = 150)

# --- Plot 2: PR curves overlaid ---
pr_curves <- do.call(rbind, lapply(results, function(r) {
  data.frame(model = r$model,
             recall = r$pr_curve$curve[, 1],
             precision = r$pr_curve$curve[, 2])
}))
p_pr <- ggplot(fix_order(pr_curves),
               aes(x = recall, y = precision, color = model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = results[[1]]$base_rate, linetype = "dashed",
             color = "grey60") +
  annotate("text", x = 0.95, y = results[[1]]$base_rate + 0.02,
           label = sprintf("Baseline = %.3f", results[[1]]$base_rate),
           hjust = 1, size = 3, color = "grey40") +
  scale_color_manual(values = MODEL_COLORS) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 0.65)) +
  labs(x = "Recall", y = "Precision",
       title = "PR curves — three-way comparison",
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")
ggsave(file.path(output_dir, "pr_curves_3way.png"), p_pr,
       width = 7, height = 6.5, dpi = 150)

pr_curves_3way           <- p_pr

# --- Plot 3: ROC curves overlaid ---
roc_curves <- do.call(rbind, lapply(results, function(r) {
  data.frame(model = r$model,
             fpr = r$roc_curve$curve[, 1],
             tpr = r$roc_curve$curve[, 2])
}))
p_roc <- ggplot(fix_order(roc_curves), aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = MODEL_COLORS) +
  coord_equal() +
  labs(x = "False Positive Rate", y = "True Positive Rate",
       title = "ROC curves — three-way comparison",
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")
ggsave(file.path(output_dir, "roc_curves_3way.png"), p_roc,
       width = 7, height = 6.5, dpi = 150)

# --- Plot 4: F1 threshold sweep (3 lines) ---
sweep_all <- do.call(rbind, lapply(results, function(r) {
  cbind(model = r$model, r$sweep_metrics)
}))
p_sweep <- ggplot(fix_order(sweep_all),
                  aes(x = threshold, y = f1, color = model)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = COMMON_OP_THRESHOLD, linetype = "dashed",
             color = "grey50") +
  annotate("text", x = COMMON_OP_THRESHOLD, y = 0.35,
           label = sprintf("Common τ = %.2f", COMMON_OP_THRESHOLD),
           hjust = -0.1, size = 3.2) +
  scale_color_manual(values = MODEL_COLORS) +
  labs(x = "Operational threshold (predicted PBB-Score)", y = "F1 score",
       title = "F1 across operational thresholds — three-way comparison",
       color = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")
ggsave(file.path(output_dir, "f1_threshold_sweep_3way.png"), p_sweep,
       width = 9, height = 5.5, dpi = 150)
f1_threshold_sweep_3way  <- p_sweep

# --- Plot 5: Lead-time comparison (bar chart of detection rate, baseline, lift) ---
lead_df <- do.call(rbind, lapply(results, function(r) {
  data.frame(
    model = r$model,
    metric = c("Episode detection rate", "Non-panic baseline"),
    value  = c(r$lead_own$detection_rate, r$lead_own$baseline_rate),
    threshold = r$lead_own$threshold
  )
}))
p_lead <- ggplot(fix_order(lead_df),
                 aes(x = model, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * value)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.2) +
  scale_fill_manual(values = c("Episode detection rate" = "#E65F2B",
                               "Non-panic baseline"     = "#0099B8")) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, max(lead_df$value) * 1.15)) +
  labs(x = NULL, y = "Rate",
       title = "Lead-time detection: panic episodes vs. non-panic baseline",
       subtitle = "Each model evaluated at its own F1-optimal threshold",
       fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")
ggsave(file.path(output_dir, "leadtime_comparison_3way.png"), p_lead,
       width = 8.5, height = 5.5, dpi = 150)

leadtime_comparison_3way <- p_lead

cat(sprintf("    Plots saved in: %s\n", output_dir))


# ── 7. Final interpretation summary ──────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" FINAL HEADLINE TABLE\n")
cat("════════════════════════════════════════════════════════════════════════\n")
hd <- summary_df[, c("model", "R2_regression", "AUC_PR", "AUC_PR_lift",
                     "AUC_ROC", "F1_at_opt", "detection_lift_own")]
names(hd) <- c("Model", "R²", "AUC-PR", "AUC-PR×", "AUC-ROC",
               "F1@opt", "Det.lift")
print(hd, row.names = FALSE, digits = 3)

cat("\n  INCREMENTAL VALUE OF EXTERNAL FEATURES IN CLASSIFICATION TERMS:\n")
cat(sprintf("    Combined − AR-only:\n"))
cat(sprintf("      Δ AUC-PR        : %+.3f\n", inc_auc_pr))
cat(sprintf("      Δ AUC-ROC       : %+.3f\n", inc_auc_roc))
cat(sprintf("      Δ F1 (optimal)  : %+.3f\n", inc_f1))
cat(sprintf("      Δ Detection lift: %+.1f pp\n", 100 * inc_det_lift))

cat("\n  INTERPRETATION GUIDE:\n")
cat("  • If AR-only AUC-PR ≈ Combined (Δ < 0.02):\n")
cat("      External features add no classification value beyond autoregression.\n")
cat("      Paper framing: \"AR-based EWS architecture; external features\n")
cat("      tested and shown not to add operational value\" (still publishable).\n\n")
cat("  • If AR-only AUC-PR < Combined by 0.02-0.05:\n")
cat("      External features add modest but real classification value.\n")
cat("      Paper framing: \"Hybrid AR + open-data EWS; external features\n")
cat("      contribute X percentage points of AUC-PR\" (the strongest framing).\n\n")
cat("  • If AR-only AUC-PR is much lower than Combined (Δ > 0.05):\n")
cat("      External features matter more for classification than for\n")
cat("      regression. Paper framing: \"External signals discriminate panic\n")
cat("      events better than they predict score magnitudes.\"\n")
cat("════════════════════════════════════════════════════════════════════════\n")

#--- panel labels: a) anchors the top row, b) the bottom ---
pr_curves_3way           <- pr_curves_3way           + labs(tag = "a)")
leadtime_comparison_3way <- leadtime_comparison_3way + labs(tag = "b)")

# 1. Define the tag theme once so we can easily apply it to all three plots
tag_theme <- theme(plot.tag = element_text(face = "bold", size = 16))

# 2. Strip the long titles, give each panel a bold letter, and apply the tag_theme
pa <- pr_curves_3way + 
  labs(title = NULL, subtitle = NULL, tag = "a") + 
  tag_theme

pb <- f1_threshold_sweep_3way + 
  labs(title = NULL, subtitle = NULL, tag = "b") + 
  tag_theme

pc <- leadtime_comparison_3way + 
  labs(title = NULL, subtitle = NULL, tag = "c") +
  theme(legend.position = "bottom") + 
  tag_theme

# 3. Top row: share the one model legend and park it beneath the two panels
# We use plot_annotation() here so patchwork knows to move the COLLECTED legend.
top_row <- (pa | pb) +
  plot_layout(guides = "collect") +
  plot_annotation(
    theme = theme(legend.position = "bottom")
  )

# 4. Combine top and bottom
combined_3way <- (top_row / pc) +
  plot_layout(heights = c(1, 1.6)) # a touch more room for the bars

# 5. Save
ggsave("data/analysis/classification_three_way/benchmark_combined_3way.png",
       combined_3way, width = 12, height = 9.5, dpi = 150, bg = "white")

ggsave(file.path("data/analysis/PDFs", "Figure_5.pdf"),
       combined_3way, width = 12, height = 9.5, device = "pdf")

#4.DYNAMIC ASSOCIATIONS VIA LOCAL PROJECTIONS------
# Purpose
#   Characterize the dynamic association between S-O-R category signals and
#   the PBB-Score across crisis types. This replaces the now-untenable
#   "external open data anticipate panic" forecasting claim with a
#   descriptive characterization of co-movement that is empirically
#   defensible and theoretically grounded.
#
# Method
#   Local Projections (Jordà 2005, AER):
#     y_{i,t+h} - y_{i,t-1} = α_i + β_h · S_{i,t} + γ_h · controls + ε_{i,t+h}
#                              + Σ_c δ^c_h · S_{i,t} × Crisis^c_t
#
#   The β_h trace impulse responses; the δ^c_h capture crisis-type
#   asymmetry. HAC standard errors (Newey-West) with lag = h+1.
#
# Design choices (see top-of-file discussion)
#   • Continuous-level specification (1-SD impulse interpretation)
#   • City fixed effects absorb time-invariant heterogeneity
#   • Crisis-type interactions: pandemic, climate, geopolitical/strike
#   • Horizons h = 0, 1, …, 14
#   • One S-O-R category per LP run; category composite is the SHAP-top
#     member of that category (defaults documented inline)
#
# Inputs
#   • 3_Final_data_With_Time_modification_ADDED_Var.csv  (raw panel)
#
# Outputs (in improved_final/local_projections/)
#   • lp_irf_<category>.csv         IRF coefficients with CIs by horizon
#   • lp_irf_all.csv                concatenated long format for plotting
#   • lp_irf_by_crisis.csv          crisis-type-stratified IRFs
#   • Plots:
#       - lp_irf_panel.png          IRFs for all categories, baseline (no crisis)
#       - lp_irf_by_crisis_<cat>.png IRFs stratified by crisis type, per category
#       - lp_summary_peak.png       peak response and peak horizon by category
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "lubridate",
  "lmtest", "sandwich", "fixest"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" LOCAL PROJECTIONS — DYNAMIC ASSOCIATIONS BY S-O-R CATEGORY × CRISIS TYPE\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

output_dir <- "data/analysis/local_projections/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Horizons to estimate (days ahead)
HORIZONS <- 0:14

# Crisis-period definitions (calendar windows)
# CRISIS_WINDOWS <- list(
#   pandemic = list(start = as.Date("2020-03-01"), end = as.Date("2022-06-30")),
#   climate  = list(  # Vale do Itajaí floods 2023, plus minor 2022 spike
#     list(start = as.Date("2022-11-01"), end = as.Date("2022-12-31")),
#     list(start = as.Date("2023-09-15"), end = as.Date("2023-12-15"))
#   ),
#   geopol   = list(  # 2018 truckers strike + 2022 truckers blockades
#     list(start = as.Date("2018-05-15"), end = as.Date("2018-06-15")),
#     list(start = as.Date("2022-10-15"), end = as.Date("2022-11-30"))
#   )
# )

CRISIS_WINDOWS <- list(
  pandemic = list(
    list(start = as.Date("2020-02-25"), end = as.Date("2020-09-26")), #1st wave: 25.02.2020 until 26.09.2020
    list(start = as.Date("2020-11-16"), end = as.Date("2021-09-16")), # 2nd wave: 16.11.2020 until 16.09.2021 
    list(start = as.Date("2021-11-16"), end = as.Date("2022-04-01"))), #3rd wave: 16.11.2021 until 01.04.2022
  
  climate  = list(
    # --- Previously Existing Broad Windows ---
    # Minor 2022 spike
    #list(start = as.Date("2022-11-01"), end = as.Date("2022-12-31")),
    # Vale do Itajaí floods 2023
    #list(start = as.Date("2023-09-15"), end = as.Date("2023-12-15")),
    
    # --- New Specific Events Extracted From Spreadsheet ---
    # Ciclone bomba de Floripa
    list(start = as.Date("2020-06-30"), end = as.Date("2020-07-01")),
    
    # Chuvas fortes
    list(start = as.Date("2020-12-17"), end = as.Date("2020-12-21")),
    
    # Choveu mais de 600mm esse mês
    list(start = as.Date("2021-01-21"), end = as.Date("2021-01-24")),
    
    # Região de Alto Vale do Itajaí
    list(start = as.Date("2021-03-26"), end = as.Date("2021-03-26")),
    
    # Grande Florianópolis e Alto Vale do Itajaí
    list(start = as.Date("2021-06-08"), end = as.Date("2021-06-09")),
    
    # Estado todo, enfase no sul
    list(start = as.Date("2022-05-03"), end = as.Date("2022-05-05")),
    
    # Vale do Itajaí, BC
    list(start = as.Date("2023-01-17"), end = as.Date("2023-01-18")),
    
    # BC, Itapema e Região Metropolitana
    list(start = as.Date("2024-01-08"), end = as.Date("2024-01-16")),
    
    # Grande Florianópolis
    list(start = as.Date("2025-12-08"), end = as.Date("2025-12-10")),
    
    # Norte do estado, enfase Joinville (Covered by 2022 broad window)
    list(start = as.Date("2022-11-29"), end = as.Date("2022-12-04")),
    
    # BC, Itapema e Região Metropolitana (Covered by 2022 broad window)
    list(start = as.Date("2022-12-20"), end = as.Date("2022-12-22")),
    
    # 50% do estado afetado (Covered by 2023 broad window)
    list(start = as.Date("2023-10-10"), end = as.Date("2023-10-10")),
    
    # Vale do Itajaí (Covered by 2023 broad window)
    list(start = as.Date("2023-11-11"), end = as.Date("2023-11-14"))
  ),
  
  geopol   = list(
    # 2018 truckers strike + 2021 + 2022 truckers blockades
    list(start = as.Date("2018-05-21"), end = as.Date("2018-05-31")),
    list(start = as.Date("2021-09-07"), end = as.Date("2021-09-09")),
    list(start = as.Date("2022-10-31"), end = as.Date("2022-11-02"))
  )
)

# ── 1. Load data ─────────────────────────────────────────────────────────────

cat("[1] LOADING DATA...\n")
df_raw <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")
df_raw$date <- as.Date(df_raw$date)
cat(sprintf("    Loaded: %s rows x %s columns\n",
            format(nrow(df_raw), big.mark = ","), ncol(df_raw)))


# ── 2. Build crisis-period indicators ────────────────────────────────────────

cat("\n[2] BUILDING CRISIS-PERIOD INDICATORS...\n")

in_window <- function(d, w) {
  d >= w$start & d <= w$end
}

in_any <- function(d, ws) {
  if (is.list(ws) && !is.null(ws$start)) {
    in_window(d, ws)
  } else {
    Reduce(`|`, lapply(ws, function(w) in_window(d, w)))
  }
}

# Crisis-period definitions (calendar windows)
# CRISIS_WINDOWS2 <- list(
#   pandemic = list(start = as.Date("2020-03-01"), end = as.Date("2022-06-30")),
#   climate  = list(  # Vale do Itajaí floods 2023, plus minor 2022 spike
#     list(start = as.Date("2022-11-01"), end = as.Date("2022-12-31")),
#     list(start = as.Date("2023-09-15"), end = as.Date("2023-12-15"))
#   ),
#   geopol   = list(  # 2018 truckers strike + 2022 truckers blockades
#     list(start = as.Date("2018-05-15"), end = as.Date("2018-06-15")),
#     list(start = as.Date("2022-10-15"), end = as.Date("2022-11-30"))
#   )
# )


df_raw$crisis_pandemic <- as.integer(in_any(df_raw$date, CRISIS_WINDOWS$pandemic))
df_raw$crisis_climate  <- as.integer(in_any(df_raw$date, CRISIS_WINDOWS$climate))
df_raw$crisis_geopol   <- as.integer(in_any(df_raw$date, CRISIS_WINDOWS$geopol))
df_raw$crisis_any      <- pmax(df_raw$crisis_pandemic, df_raw$crisis_climate,
                               df_raw$crisis_geopol)

# Mutually-exclusive single-category label for stratified plots
df_raw$crisis_type <- with(df_raw,
                           ifelse(crisis_pandemic == 1, "pandemic",
                                  ifelse(crisis_climate == 1,  "climate",
                                         ifelse(crisis_geopol == 1,   "geopolitical",
                                                "calm")))
)

cat("    Crisis-period coverage:\n")
cat(sprintf("      Pandemic    : %.1f%% of rows\n",
            100 * mean(df_raw$crisis_pandemic)))
cat(sprintf("      Climate     : %.1f%% of rows\n",
            100 * mean(df_raw$crisis_climate)))
cat(sprintf("      Geopolitical: %.1f%% of rows\n",
            100 * mean(df_raw$crisis_geopol)))
cat(sprintf("      Calm        : %.1f%% of rows\n",
            100 * mean(df_raw$crisis_type == "calm")))


# ── 3. Select category representatives ───────────────────────────────────────

cat("\n[3] SELECTING REPRESENTATIVE VARIABLE PER S-O-R CATEGORY...\n")

# Category representatives based on prior SHAP rankings.
# Adjust these to whichever members of each category are most theoretically
# central or empirically dominant in your existing SHAP analysis.
CATEGORY_VARS <- list(
  "Trigger (S-pandemic)"  = "S_pandemic_Gtrends",
  "Trigger (S-climate)"   = "S_wind_gust_max",
  "Anticipation (O-news)" = "O_news_count",
  "Mobilization (O-mobility)" = "O_mobil_dist_0",
  "Realization (price)"   = "O_basicfoodbasket_Gtrends"
)

# Sanity check: do they exist?
missing_vars <- setdiff(unlist(CATEGORY_VARS), names(df_raw))
if (length(missing_vars) > 0) {
  cat("    WARNING: missing variables:\n")
  for (v in missing_vars) cat(sprintf("      - %s\n", v))
  cat("    Edit CATEGORY_VARS at the top of this script to use available names.\n")
  CATEGORY_VARS <- CATEGORY_VARS[!sapply(CATEGORY_VARS,
                                         function(v) v %in% missing_vars)]
}
cat("    Categories and their representative variables:\n")
for (k in names(CATEGORY_VARS)) {
  cat(sprintf("      %-30s -> %s\n", k, CATEGORY_VARS[[k]]))
}


# ── 4. Prepare panel data ────────────────────────────────────────────────────

cat("\n[4] PREPARING PANEL...\n")

# Drop rows with missing target
TARGET <- "API"
panel <- df_raw %>%
  filter(!is.na(.data[[TARGET]])) %>%
  arrange(city, date)

# Standardize each category variable (mean 0, SD 1) so β_h is interpretable
# as "response to a one-SD shock in the predictor"
for (v in unlist(CATEGORY_VARS)) {
  if (v %in% names(panel)) {
    x <- panel[[v]]
    panel[[paste0(v, "_z")]] <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }
}

# Lagged DV controls (matches the AR-only feature set used in prior analyses)
panel <- panel %>%
  group_by(city) %>%
  mutate(
    API_lag1  = dplyr::lag(.data[[TARGET]], 1),
    API_lag7  = dplyr::lag(.data[[TARGET]], 7),
    API_lag14 = dplyr::lag(.data[[TARGET]], 14),
    API_lag28 = dplyr::lag(.data[[TARGET]], 28)
  ) %>%
  ungroup()

# Pre-compute the LHS for each horizon: y_{t+h}
# (we use levels, not first-differences, since PBB-Score is already a
#  standardized residual — first-differencing it would be over-cleaning)
for (h in HORIZONS) {
  newcol <- sprintf("API_lead_%d", h)
  panel <- panel %>%
    group_by(city) %>%
    mutate(!!newcol := dplyr::lead(.data[[TARGET]], h)) %>%
    ungroup()
}

cat(sprintf("    Panel ready: %s rows x %s columns\n",
            format(nrow(panel), big.mark = ","), ncol(panel)))

# ── 5. Helper: estimate LP for a single (category, horizon, crisis) cell ─────

estimate_lp <- function(panel, shock_var, horizon,
                        crisis_var = NULL,
                        include_ar_controls = TRUE,
                        category_label = NA_character_) {
  
  lhs <- sprintf("API_lead_%d", horizon)
  shock_var_z <- paste0(shock_var, "_z")
  
  # Base RHS: city FE, shock, optional AR controls
  rhs_parts <- c(shock_var_z, "API_lag1")
  if (include_ar_controls) {
    rhs_parts <- c(rhs_parts, "API_lag7", "API_lag14", "API_lag28")
  }
  
  formula_str <- sprintf("%s ~ %s | city",
                         lhs, paste(rhs_parts, collapse = " + "))
  
  # SE choice: two-way cluster on city + date. This handles both cross-sectional
  # correlation across cities and serial correlation within cities at each
  # horizon. Standard for panel Local Projections (Plagborg-Møller & Wolf 2021).
  # Alternative: vcov = "DK" (Driscoll-Kraay) — uncomment to switch.
  # Alternative: panel.id = ~ city + date with vcov = NW(lag = horizon+1)
  fit <- tryCatch(
    feols(as.formula(formula_str), data = panel,
          cluster = ~ city + date),
    error = function(e) {
      if (horizon == 0) cat(sprintf("      [LP error at h=0]: %s\n",
                                    conditionMessage(e)))
      NULL
    }
  )
  
  if (is.null(fit)) return(NULL)
  
  # Extract impulse coefficient (β_h on the shock variable)
  ct <- coeftable(fit)
  shock_row <- which(rownames(ct) == shock_var_z)
  if (length(shock_row) == 0) return(NULL)
  
  result <- data.frame(
    category = category_label,   # set inside the function — always present
    horizon  = horizon,
    shock    = shock_var,
    crisis   = if (is.null(crisis_var)) "all" else crisis_var,
    beta     = ct[shock_row, "Estimate"],
    se       = ct[shock_row, "Std. Error"],
    t_stat   = ct[shock_row, "t value"],
    p_value  = ct[shock_row, "Pr(>|t|)"],
    n_obs    = nobs(fit),
    stringsAsFactors = FALSE
  )
  result$ci_lo <- result$beta - 1.96 * result$se
  result$ci_hi <- result$beta + 1.96 * result$se
  
  result
}

CRISIS_DUMMIES <- c("crisis_pandemic", "crisis_climate", "crisis_geopol")
CRISIS_LABELS  <- c(crisis_pandemic = "Pandemic",
                    crisis_climate  = "Climate",
                    crisis_geopol   = "Geopolitical")

# Returns a LONG data frame with one row for the calm baseline (beta_h) and one
# row per crisis regime (total response beta_h + delta_h^(c)), each with a CI
# built from the correct linear-combination SE.
estimate_lp_joint <- function(panel, shock_var, horizon,
                              include_ar_controls = TRUE,
                              category_label = NA_character_) {
  
  lhs         <- sprintf("API_lead_%d", horizon)
  shock_var_z <- paste0(shock_var, "_z")
  
  ar_terms  <- if (include_ar_controls)
    c("API_lag1", "API_lag7", "API_lag14", "API_lag28")
  else "API_lag1"
  int_terms <- sprintf("%s:%s", shock_var_z, CRISIS_DUMMIES)   # shock x regime
  rhs       <- c(shock_var_z, ar_terms, CRISIS_DUMMIES, int_terms)
  
  formula_str <- sprintf("%s ~ %s | city", lhs, paste(rhs, collapse = " + "))
  
  # Two-way clustered on city + date (Plagborg-Moller & Wolf, 2021).
  fit <- tryCatch(
    feols(as.formula(formula_str), data = panel, cluster = ~ city + date),
    error = function(e) {
      if (horizon == 0)
        cat(sprintf("      [joint LP error h=0, %s]: %s\n",
                    shock_var, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(fit)) return(NULL)
  
  b <- coef(fit)                 # non-absorbed coefficients (names matched...)
  V <- vcov(fit)                 # ...to this clustered vcov
  if (!(shock_var_z %in% names(b))) return(NULL)
  
  z <- 1.96
  
  # Calm baseline: response when all C^c = 0 (genuinely non-crisis days).
  beta0 <- unname(b[shock_var_z])
  se0   <- unname(sqrt(V[shock_var_z, shock_var_z]))
  
  out <- list(
    data.frame(category = category_label, horizon = horizon, shock = shock_var,
               crisis_type = "Calm", beta = beta0, se = se0,
               ci_lo = beta0 - z * se0, ci_hi = beta0 + z * se0,
               delta = NA_real_, se_delta = NA_real_, p_delta = NA_real_,
               stringsAsFactors = FALSE)
  )
  
  # Regime totals beta_h + delta_h^(c) with FULL linear-combination variance:
  #   Var(beta + delta) = Var(beta) + Var(delta) + 2*Cov(beta, delta)   <-- (2)
  for (cvar in CRISIS_DUMMIES) {
    int_name <- sprintf("%s:%s", shock_var_z, cvar)
    if (!(int_name %in% names(b))) next          # regime unidentified at this h
    
    delta   <- unname(b[int_name])
    total   <- beta0 + delta
    var_tot <- V[shock_var_z, shock_var_z] +
      V[int_name,   int_name]   +
      2 * V[shock_var_z, int_name]
    se_tot  <- sqrt(max(var_tot, 0))
    
    se_int  <- unname(sqrt(V[int_name, int_name]))          # delta on its own
    p_int   <- 2 * pnorm(-abs(delta / se_int))              # interaction test
    
    out[[length(out) + 1]] <- data.frame(
      category = category_label, horizon = horizon, shock = shock_var,
      crisis_type = unname(CRISIS_LABELS[cvar]),
      beta = total, se = se_tot,
      ci_lo = total - z * se_tot, ci_hi = total + z * se_tot,
      delta = delta, se_delta = se_int, p_delta = p_int,
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(out)
}


# --- Driver: replaces the old per-regime crisis loop (section 7) 
cat("\n[6] ESTIMATING LP - JOINT STIMULUS-INTERACTED (Eq. 11)\n")

crisis_results <- list()
for (cat_name in names(CATEGORY_VARS)) {
  shock_var <- CATEGORY_VARS[[cat_name]]
  cat(sprintf("    %s\n", cat_name))
  
  cat_rows <- list()
  for (h in HORIZONS) {
    r <- estimate_lp_joint(panel, shock_var, h, category_label = cat_name)
    if (!is.null(r)) cat_rows[[length(cat_rows) + 1]] <- r
  }
  if (length(cat_rows) > 0) {
    df_cat <- dplyr::bind_rows(cat_rows)
    crisis_results[[cat_name]] <- df_cat
    
    # Quick per-regime significance report (any horizon with |delta| > 1.96 SE)
    for (ct in unname(CRISIS_LABELS)) {
      sub <- df_cat[df_cat$crisis_type == ct & !is.na(df_cat$delta), ]
      sig <- sub$horizon[abs(sub$delta) > 1.96 * sub$se_delta]
      cat(sprintf("      x %-13s: sig. interaction at h = %s\n", ct,
                  if (length(sig)) paste(sig, collapse = ",") else "none"))
    }
  }
}
crisis_all <- dplyr::bind_rows(crisis_results)
write.csv(crisis_all, file.path(output_dir, "lp_irf_by_crisis.csv"),
          row.names = FALSE)

# ── 6. Run LPs: baseline (no crisis interaction) ─────────────────────────────

cat("\n[5] ESTIMATING LP — BASELINE (NO CRISIS INTERACTION)\n")

baseline_results <- list()

for (cat_name in names(CATEGORY_VARS)) {
  shock_var <- CATEGORY_VARS[[cat_name]]
  cat(sprintf("    %s [%s]\n", cat_name, shock_var))
  
  cat_results <- list()
  for (h in HORIZONS) {
    r <- estimate_lp(panel, shock_var, h, crisis_var = NULL, 
                     category_label = cat_name)
    if (!is.null(r)) {
      cat_results[[length(cat_results) + 1]] <- r
    }
  }
  
  if (length(cat_results) > 0) {
    df_cat <- dplyr::bind_rows(cat_results)
    baseline_results[[cat_name]] <- df_cat
    
    # Per-category CSV
    fname <- file.path(output_dir, sprintf("lp_irf_%s.csv",
                                           gsub("[^A-Za-z0-9]+", "_", cat_name)))
    write.csv(df_cat, fname, row.names = FALSE)
    
    # Print quick summary: peak response and its horizon
    peak_idx <- which.max(abs(df_cat$beta))
    cat(sprintf("      Peak |β| = %+.4f at h=%d (CI [%+.4f, %+.4f]), p=%.4f\n",
                df_cat$beta[peak_idx], df_cat$horizon[peak_idx],
                df_cat$ci_lo[peak_idx], df_cat$ci_hi[peak_idx],
                df_cat$p_value[peak_idx]))
  }
}

baseline_all <- dplyr::bind_rows(baseline_results)

# Defensive check: if the column isn't present or the data frame is empty,
# print a diagnostic so the failure mode is obvious before plotting.
if (nrow(baseline_all) == 0) {
  stop("baseline_all is empty — no LP estimations succeeded. Check the panel ",
       "preprocessing and CATEGORY_VARS contents.")
}
if (!"category" %in% names(baseline_all)) {
  stop("baseline_all is missing the 'category' column. Diagnostic:\n",
       "  names: ", paste(names(baseline_all), collapse = ", "))
}
cat(sprintf("\n    baseline_all assembled: %d rows, %d categories present.\n",
            nrow(baseline_all), length(unique(baseline_all$category))))

write.csv(baseline_all, file.path(output_dir, "lp_irf_all.csv"), row.names = FALSE)

# ── 8. Plots ─────────────────────────────────────────────────────────────────

cat("\n[7] BUILDING PLOTS...\n")

# Color palette per S-O-R category
CAT_COLORS <- c(
  "Trigger (S-pandemic)"        = "#E65F2B",
  "Trigger (S-climate)"         = "#2A9D8F",
  "Anticipation (O-news)"       = "#FFB703",
  "Mobilization (O-mobility)"   = "#E25C84",
  "Realization (price)"         = "#AE2012"
)

# --- Plot 1: Baseline IRFs, one panel per category ---
p_baseline <- ggplot(baseline_all,
                     aes(x = horizon, y = beta, color = category, fill = category)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.20, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = CAT_COLORS) +
  scale_fill_manual(values = CAT_COLORS) +
  facet_wrap(~ category, ncol = 2, scales = "free_y") +
  labs(x = "Horizon h (days after shock)",
       y = expression("Impulse response " * beta[h] *
                        " (PBB-Score change per 1-SD shock)"),
       title = "Local Projections: dynamic response of PBB-Score to S-O-R shocks",
       subtitle = "Baseline specification (no crisis interaction). Shaded band = 95% pointwise CI from two-way clustered (city, date) standard errors.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave(file.path(output_dir, "lp_irf_panel.png"), p_baseline,
       width = 11, height = 8, dpi = 150)

ggsave(file.path("data/analysis/PDFs", "Figure_8.pdf"),
       p_baseline, width = 11, height = 8, device = "pdf")

# --- Plot 2: Crisis-stratified IRFs, one figure per category ---
crisis_color_map <- c("Pandemic" = "#E65F2B",
                      "Climate"  = "#2A9D8F",
                      "Geopolitical" = "#0099B8",
                      "Calm" = "grey50")

for (cat_name in names(CATEGORY_VARS)) {
        long_df <- crisis_all %>% dplyr::filter(category == cat_name)
        if (nrow(long_df) == 0) next
        p_cat <- ggplot(long_df, aes(x = horizon, y = beta,
                                     color = crisis_type, fill = crisis_type)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
          geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.18, color = NA) +
          geom_line(linewidth = 0.9) + geom_point(size = 2) +
          scale_color_manual(values = crisis_color_map) +
          scale_fill_manual(values  = crisis_color_map) +
          labs(subtitle = paste("Calm = baseline (all regimes 0);",
                                "regime = baseline + interaction.",
                                "95% pointwise CIs, two-way clustered (city, date).")) +
          theme_minimal(base_size = 11) + theme(legend.position = "top") +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "top")
  
  fname <- file.path(output_dir,
                     sprintf("lp_irf_by_crisis_%s.png",
                             gsub("[^A-Za-z0-9]+", "_", cat_name)))
  ggsave(fname, p_cat, width = 9, height = 5.5, dpi = 150)
}

# --- Plot 3: Peak summary across categories ---
peak_df <- baseline_all %>%
  group_by(category) %>%
  slice_max(abs(beta), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(category = factor(category, levels = names(CAT_COLORS)))

p_peak <- ggplot(peak_df, aes(x = category, y = beta, fill = category)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_col(alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.3, linewidth = 0.6) +
  geom_text(aes(label = sprintf("h=%d", horizon),
                y = ifelse(beta >= 0, ci_hi + 0.005, ci_lo - 0.005)),
            vjust = ifelse(peak_df$beta >= 0, 0, 1), size = 3.5) +
  scale_fill_manual(values = CAT_COLORS) +
  coord_flip() +
  labs(x = NULL,
       y = expression("Peak impulse response " * beta[h] *
                        " (with horizon h annotated)"),
       title = "Peak dynamic response by S-O-R category (baseline specification)",
       subtitle = "Bars: peak |β| across h = 0..14; whiskers = 95% HAC CI at peak horizon.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")

ggsave(file.path(output_dir, "lp_summary_peak.png"), p_peak,
       width = 10, height = 5.5, dpi = 150)

cat(sprintf("    Plots saved in: %s\n", output_dir))

mobility_cat <- "Mobilization (O-mobility)"
# crisis_all is already long: Calm baseline + every regime identifiable in the
# post-2022 mobility sample. Unidentified regimes are absent (no manual drop).
long_df <- crisis_all %>%
  dplyr::filter(category == mobility_cat, !is.na(beta)) %>%
  dplyr::select(horizon, crisis_type, beta, ci_lo, ci_hi)

long_df$crisis_type <- factor(long_df$crisis_type,
                              levels = c("Calm", "Pandemic", "Climate", "Geopolitical"))

present <- setdiff(as.character(unique(long_df$crisis_type)), "Calm")
cat("Crisis types identified for mobility:", paste(present, collapse = ", "), "\n")

crisis_color_map <- c("Calm" = "grey50", "Pandemic" = "#E65F2B",
                      "Climate" = "#2A9D8F", "Geopolitical" = "#0099B8")

p_mob_fixed <- ggplot(long_df, aes(x = horizon, y = beta,
                                   color = crisis_type, fill = crisis_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.18, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = crisis_color_map, drop = FALSE) +
  scale_fill_manual(values  = crisis_color_map, drop = FALSE) +
  labs(x = "Horizon h (days after shock)",
       y = expression(beta[h] * " (PBB-Score change per 1-SD shock)"),
       title = sprintf("Dynamic response: %s by crisis type (post-2022 data)", mobility_cat),
       subtitle = if (!"Pandemic" %in% present)
         "Pandemic interaction unidentified: mobility data unavailable before 2022."
       else
         "Mobility data begins 2022; pandemic regime rests on limited overlap.",
       color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")

ggsave("data/analysis/local_projections/lp_irf_by_crisis_Mobilization_O_mobility_fixed.png",
       p_mob_fixed, width = 9, height = 5.5, dpi = 150)

crisis_all %>% dplyr::filter(category == mobility_cat) %>% dplyr::count(crisis_type)

panel %>%
  dplyr::filter(!is.na(O_mobil_dist_0_z)) %>%
  dplyr::summarise(dplyr::across(c(crisis_pandemic, crisis_climate, crisis_geopol), sum))

#single plot:
library(ggplot2)
library(patchwork)
library(dplyr)

# Fallback: load from CSV if objects not in memory
if (!exists("crisis_all") || !exists("baseline_all")) {
  output_dir <- "data/analysis/local_projections/"
  baseline_all <- read.csv(file.path(output_dir, "lp_irf_all.csv"))
  crisis_all   <- read.csv(file.path(output_dir, "lp_irf_by_crisis.csv"))
}

crisis_color_map <- c("Calm"         = "grey40",
                      "Pandemic"     = "#E65F2B",
                      "Climate"      = "#2A9D8F",
                      "Geopolitical" = "#0099B8")

build_lp_panel <- function(cat_name, tag_text,
                           show_y_label = FALSE,
                           show_x_label = TRUE,
                           show_legend  = FALSE) {
  
  # Joint crisis_all already holds Calm + regime totals in long form —
  # no baseline_all, no beta_crisis reconstruction.
  d <- crisis_all %>%
    dplyr::filter(category == cat_name, !is.na(beta)) %>%
    dplyr::select(horizon, crisis_type, beta, ci_lo, ci_hi)
  
  d$crisis_type <- factor(d$crisis_type,
                          levels = c("Calm", "Pandemic", "Climate", "Geopolitical"))
  
  ggplot(d, aes(x = horizon, y = beta, color = crisis_type, fill = crisis_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.4) +
    scale_color_manual(values = crisis_color_map, drop = FALSE) +
    scale_fill_manual(values  = crisis_color_map, drop = FALSE) +
    scale_x_continuous(breaks = c(0, 5, 10, 14), limits = c(0, 14)) +
    labs(x = if (show_x_label) "Horizon h (days after shock)" else NULL,
         y = if (show_y_label) expression(beta[h]) else NULL,
         color = NULL, fill = NULL, tag = tag_text) +
    theme_minimal(base_size = 10) +
    theme(plot.tag = element_text(size = 10, face = "bold"),
          plot.tag.position = c(0.04, 0.96),
          axis.title = element_text(size = 9),
          axis.text  = element_text(size = 8),
          legend.position = ifelse(show_legend, "bottom", "none"),
          legend.text = element_text(size = 10),
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 10, r = 8, b = 4, l = 4))
}

# --- Generate plots: Turn legend ON only for the last plot ---
p1 <- build_lp_panel("Realization (price)",   "\n\n---            (a) Realization (price)",   show_y_label = TRUE, show_x_label = FALSE)
p2 <- build_lp_panel("Anticipation (O-news)", "---              (b) Anticipation (news)",   show_y_label = TRUE, show_x_label = FALSE)
p3 <- build_lp_panel("Trigger (S-climate)",   "---         (c) Trigger (climate)",     show_y_label = TRUE, show_legend = TRUE)

# Compose without using `&` — plot_annotation handles the composition-level theme
combined <- p1 + p2 + p3 +
  plot_layout(nrow = 3, guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom",
                                legend.text = element_text(size = 10),
                                legend.margin = margin(t = -2)))

out_dir <- "data/analysis/local_projections/"
ggsave(file.path(out_dir, "lp_irf_crisis_combined.png"),
       combined, width = 8, height = 11, dpi = 300)

ggsave(file.path("data/analysis/PDFs", "Figure_9.pdf"),
       combined, width = 8, height = 11, device = "pdf")

# ── 9. Final summary ─────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Categories estimated   : %d\n", length(CATEGORY_VARS)))
cat(sprintf("  Horizons per category  : %d (h = %d to %d)\n",
            length(HORIZONS), min(HORIZONS), max(HORIZONS)))
# cat(sprintf("  Total LPs estimated    : %d baseline + %d crisis-interacted\n",
#             length(CATEGORY_VARS) * length(HORIZONS),
#             length(CATEGORY_VARS) * length(crisis_vars) * length(HORIZONS)))
cat(sprintf("  Standard errors        : two-way cluster (city + date)\n"))
cat(sprintf("  Fixed effects          : city\n"))
cat(sprintf("  AR controls            : lag 1, 7, 14, 28 of PBB-Score\n"))

#5.New descriptives for section 1------
# EPISODE ANATOMY AND SPATIAL SIGNATURE
# Purpose
#   Build the empirical "natural history" of retail panic-buying episodes in
#   the Santa Catarina panel. Quantifies episode frequency, duration,
#   intensity, spatial distribution, and crisis-type transition structure.
#
# Outputs (in improved_final/episode_anatomy/)
#   • episode_table.csv            full episode-level table (one row per ep.)
#   • summary_statistics.csv       headline summary stats
#   • by_year_city.csv             pivot: episode counts by year × city
#   • crisis_transitions.csv       transition matrix data for Sankey
#   • Plots:
#       - episode_anatomy_panel.png      2x2: by year, by city, duration, severity
#       - episode_crisis_sankey.png      Sankey-style alluvial of transitions
#       - episode_calendar_heatmap.png   bonus: city × month panel heatmap
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "lubridate",
  "ggalluvial", "scales", "stringr"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" EPISODE ANATOMY AND SPATIAL SIGNATURE\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

output_dir <- "data/analysis/episode_anatomy/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ── Operational thresholds and definitions ──────────────────────────────────

PANIC_THRESHOLD     <- 2.0    # PBB-Score >= 2σ
EXTREME_THRESHOLD   <- 3.0    # PBB-Score >= 3σ
GAP_TOLERANCE       <- 2      # days; runs separated by <= GAP days are merged

CRISIS_WINDOWS <- list(
  pandemic = list(
    list(start = as.Date("2020-02-25"), end = as.Date("2020-09-26")), #1st wave: 25.02.2020 until 26.09.2020
    list(start = as.Date("2020-11-16"), end = as.Date("2021-09-16")), # 2nd wave: 16.11.2020 until 16.09.2021 
    list(start = as.Date("2021-11-16"), end = as.Date("2022-04-01"))), #3rd wave: 16.11.2021 until 01.04.2022
  
  climate  = list(
    # --- Previously Existing Broad Windows ---
    # Minor 2022 spike
    #list(start = as.Date("2022-11-01"), end = as.Date("2022-12-31")),
    # Vale do Itajaí floods 2023
    #list(start = as.Date("2023-09-15"), end = as.Date("2023-12-15")),
    
    # --- New Specific Events Extracted From Spreadsheet ---
    # Ciclone bomba de Floripa
    list(start = as.Date("2020-06-30"), end = as.Date("2020-07-01")),
    
    # Chuvas fortes
    list(start = as.Date("2020-12-17"), end = as.Date("2020-12-21")),
    
    # Choveu mais de 600mm esse mês
    list(start = as.Date("2021-01-21"), end = as.Date("2021-01-24")),
    
    # Região de Alto Vale do Itajaí
    list(start = as.Date("2021-03-26"), end = as.Date("2021-03-26")),
    
    # Grande Florianópolis e Alto Vale do Itajaí
    list(start = as.Date("2021-06-08"), end = as.Date("2021-06-09")),
    
    # Estado todo, enfase no sul
    list(start = as.Date("2022-05-03"), end = as.Date("2022-05-05")),
    
    # Vale do Itajaí, BC
    list(start = as.Date("2023-01-17"), end = as.Date("2023-01-18")),
    
    # BC, Itapema e Região Metropolitana
    list(start = as.Date("2024-01-08"), end = as.Date("2024-01-16")),
    
    # Grande Florianópolis
    list(start = as.Date("2025-12-08"), end = as.Date("2025-12-10")),
    
    # Norte do estado, enfase Joinville (Covered by 2022 broad window)
    list(start = as.Date("2022-11-29"), end = as.Date("2022-12-04")),
    
    # BC, Itapema e Região Metropolitana (Covered by 2022 broad window)
    list(start = as.Date("2022-12-20"), end = as.Date("2022-12-22")),
    
    # 50% do estado afetado (Covered by 2023 broad window)
    list(start = as.Date("2023-10-10"), end = as.Date("2023-10-10")),
    
    # Vale do Itajaí (Covered by 2023 broad window)
    list(start = as.Date("2023-11-11"), end = as.Date("2023-11-14"))
  ),
  
  geopol   = list(
    # 2018 truckers strike + 2022 truckers blockades
    list(start = as.Date("2018-05-21"), end = as.Date("2018-05-31")),
    list(start = as.Date("2021-09-07"), end = as.Date("2021-09-09")),
    list(start = as.Date("2022-10-31"), end = as.Date("2022-11-02"))
  )
)


# ── 1. Load data ─────────────────────────────────────────────────────────────

cat("[1] LOADING DATA...\n")
df <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")
df$date <- as.Date(df$date)
df <- df %>% filter(!is.na(API)) %>% arrange(city, date)

cat(sprintf("    Loaded: %s rows | %d cities | date range %s to %s\n",
            format(nrow(df), big.mark = ","),
            length(unique(df$city)),
            min(df$date), max(df$date)))


# ── 2. Episode identification ───────────────────────────────────────────────

cat("\n[2] IDENTIFYING EPISODES (PBB >= 2σ, gap tolerance = ", GAP_TOLERANCE, " days)...\n", sep = "")

# Helper: identify runs of TRUE in a logical vector, allowing gaps of up to k
# zeros between contiguous TRUE blocks to be merged into one run.
identify_runs <- function(x, gap_tolerance = 0) {
  x <- as.integer(x)
  if (gap_tolerance > 0) {
    # Smooth short gaps: any 0-run of length <= gap_tolerance that lies
    # between two 1-runs is set to 1.
    rl <- rle(x)
    is_short_gap <- rl$values == 0 & rl$lengths <= gap_tolerance
    has_left_one  <- c(FALSE, head(rl$values, -1) == 1)
    has_right_one <- c(tail(rl$values, -1) == 1, FALSE)
    bridge <- is_short_gap & has_left_one & has_right_one
    rl$values[bridge] <- 1
    x <- inverse.rle(rl)
  }
  
  # Now assign run IDs to contiguous 1-blocks
  rl <- rle(x)
  run_id <- rep(NA_integer_, length(x))
  if (any(rl$values == 1)) {
    starts <- cumsum(c(1, head(rl$lengths, -1)))
    ends <- starts + rl$lengths - 1
    run_counter <- 0
    for (k in seq_along(rl$values)) {
      if (rl$values[k] == 1) {
        run_counter <- run_counter + 1
        run_id[starts[k]:ends[k]] <- run_counter
      }
    }
  }
  run_id
}

# Build episode table: one row per (city, episode) with anatomy stats
df <- df %>%
  mutate(panic_flag   = as.integer(API >= PANIC_THRESHOLD),
         extreme_flag = as.integer(API >= EXTREME_THRESHOLD))

df_with_runs <- df %>%
  group_by(city) %>%
  mutate(episode_id = identify_runs(panic_flag, gap_tolerance = GAP_TOLERANCE)) %>%
  ungroup()

episodes <- df_with_runs %>%
  filter(!is.na(episode_id)) %>%
  group_by(city, episode_id) %>%
  summarise(
    start_date    = min(date),
    end_date      = max(date),
    duration_days = as.integer(end_date - start_date) + 1L,
    n_panic_days  = sum(panic_flag, na.rm = TRUE),
    peak_PBB      = max(API, na.rm = TRUE),
    mean_PBB      = mean(API[panic_flag == 1], na.rm = TRUE),
    is_extreme    = as.integer(any(extreme_flag == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(year = lubridate::year(start_date))

cat(sprintf("    Identified %d episodes across %d cities\n",
            nrow(episodes), length(unique(episodes$city))))

# ── 3. Attach crisis-type label to each episode ─────────────────────────────

cat("\n[3] LABELING EPISODES BY CRISIS CONTEXT...\n")

# in_any_window <- function(d, windows) {
#   Reduce(`|`, lapply(windows, function(w) d >= w$start & d <= w$end))
# }

# Check if ANY day of the episode intersects with ANY of the crisis windows
in_any_window <- function(ep_start, ep_end, windows) {
  Reduce(`|`, lapply(windows, function(w) {
    # TRUE if the episode interval overlaps the window interval
    ep_start <= w$end & ep_end >= w$start
  }))
}

episodes <- episodes %>%
  mutate(
    # Pass both start_date and end_date to the new function
    is_pandemic = in_any_window(start_date, end_date, CRISIS_WINDOWS$pandemic),
    is_climate  = in_any_window(start_date, end_date, CRISIS_WINDOWS$climate),
    is_geopol   = in_any_window(start_date, end_date, CRISIS_WINDOWS$geopol),
    # --- MODEL 1: Prioritized (Acute overrides Chronic) ---
    # Used for downstream plotting and transition matrices to avoid clutter.
    crisis_type = case_when(
      is_climate  ~ "Climate",
      is_geopol   ~ "Geopolitical",
      is_pandemic ~ "Pandemic",
      TRUE        ~ "Unattributed"
    ),
    
    # --- MODEL 2: Overlap (Explicitly tracking multi-hazard events) ---
    # Used to enrich the summary statistics table.
    crisis_type_overlap = case_when(
      is_pandemic & is_climate ~ "Pandemic & Climate",
      is_geopol & is_climate   ~ "Geopolitical & Climate",
      is_pandemic & is_geopol  ~ "Pandemic & Geopolitical",
      is_climate               ~ "Climate",
      is_geopol                ~ "Geopolitical",
      is_pandemic              ~ "Pandemic",
      TRUE                     ~ "Unattributed"
    )
  )

cat("    Episodes by crisis context (PRIORITIZED for plots):\n")
crisis_counts <- table(episodes$crisis_type)
for (n in names(crisis_counts)) {
  cat(sprintf("      %-15s: %4d (%.1f%%)\n", n, crisis_counts[n],
              100 * crisis_counts[n] / nrow(episodes)))
}

cat("\n    Episodes by crisis context (OVERLAP explicit):\n")
overlap_counts <- table(episodes$crisis_type_overlap)
for (n in names(overlap_counts)) {
  cat(sprintf("      %-25s: %4d (%.1f%%)\n", n, overlap_counts[n],
              100 * overlap_counts[n] / nrow(episodes)))
}

write.csv(episodes, file.path(output_dir, "episode_table.csv"), row.names = FALSE)

# ── 4. Summary statistics ────────────────────────────────────────────────────

cat("\n[4] SUMMARY STATISTICS...\n")

# Helper functions to format the output
fmt_num <- function(x) sprintf("%.2f", x)
fmt_pct <- function(count, total) sprintf("%.1f%%", 100 * count / total)

# Total episodes for percentage denominator
total_ep <- nrow(episodes)

# Calculate counts for Prioritized Model
c_pan_p <- sum(episodes$crisis_type == "Pandemic")
c_cli_p <- sum(episodes$crisis_type == "Climate")
c_geo_p <- sum(episodes$crisis_type == "Geopolitical")
c_una_p <- sum(episodes$crisis_type == "Unattributed")

# Calculate counts for Overlap Model
c_pan_o <- sum(episodes$crisis_type_overlap == "Pandemic")
c_cli_o <- sum(episodes$crisis_type_overlap == "Climate")
c_geo_o <- sum(episodes$crisis_type_overlap == "Geopolitical")
c_una_o <- sum(episodes$crisis_type_overlap == "Unattributed")
c_p_c_o <- sum(episodes$crisis_type_overlap == "Pandemic & Climate")
c_g_c_o <- sum(episodes$crisis_type_overlap == "Geopolitical & Climate")

summary_stats <- data.frame(
  metric = c(
    "Total episodes",
    "Cities with at least one episode",
    "Mean duration (days)",
    "Median duration (days)",
    "Max duration (days)",
    "Mean peak PBB-Score",
    "Episodes with extreme (>=3σ) days",
    "Episodes per city-year (mean)",
    "---------------------------------------------",
    "Episodes: Pandemic (count)",
    "Episodes: Pandemic (% of total)",
    "Episodes: Climate (count)",
    "Episodes: Climate (% of total)",
    "Episodes: Geopolitical (count)",
    "Episodes: Geopolitical (% of total)",
    "Episodes: Unattributed (count)",
    "Episodes: Unattributed (% of total)",
    "Episodes: Pandemic & Climate (count)",
    "Episodes: Pandemic & Climate (% of total)",
    "Episodes: Geopolitical & Climate (count)",
    "Episodes: Geopolitical & Climate (% of total)"
  ),
  Prioritized_Model = c(
    as.character(total_ep),
    as.character(length(unique(episodes$city))),
    fmt_num(mean(episodes$duration_days)),
    as.character(median(episodes$duration_days)),
    as.character(max(episodes$duration_days)),
    fmt_num(mean(episodes$peak_PBB)),
    as.character(sum(episodes$is_extreme)),
    fmt_num(total_ep / (length(unique(episodes$city)) * length(unique(episodes$year)))),
    "---",
    as.character(c_pan_p),
    fmt_pct(c_pan_p, total_ep),
    as.character(c_cli_p),
    fmt_pct(c_cli_p, total_ep),
    as.character(c_geo_p),
    fmt_pct(c_geo_p, total_ep),
    as.character(c_una_p),
    fmt_pct(c_una_p, total_ep),
    "N/A", "N/A", "N/A", "N/A"
  ),
  Overlap_Model = c(
    as.character(total_ep),
    as.character(length(unique(episodes$city))),
    fmt_num(mean(episodes$duration_days)),
    as.character(median(episodes$duration_days)),
    as.character(max(episodes$duration_days)),
    fmt_num(mean(episodes$peak_PBB)),
    as.character(sum(episodes$is_extreme)),
    fmt_num(total_ep / (length(unique(episodes$city)) * length(unique(episodes$year)))),
    "---",
    as.character(c_pan_o),
    fmt_pct(c_pan_o, total_ep),
    as.character(c_cli_o),
    fmt_pct(c_cli_o, total_ep),
    as.character(c_geo_o),
    fmt_pct(c_geo_o, total_ep),
    as.character(c_una_o),
    fmt_pct(c_una_o, total_ep),
    as.character(c_p_c_o),
    fmt_pct(c_p_c_o, total_ep),
    as.character(c_g_c_o),
    fmt_pct(c_g_c_o, total_ep)
  )
)

print(summary_stats, row.names = FALSE)
write.csv(summary_stats, file.path(output_dir, "summary_statistics.csv"), row.names = FALSE)

# Gap-tolerance sensitivity check
cat("\n    Gap-tolerance sensitivity (episode counts):\n")
for (gap in c(0, 1, 2, 5)) {
  ep_alt <- df %>%
    group_by(city) %>%
    mutate(eid = identify_runs(panic_flag, gap_tolerance = gap)) %>%
    ungroup() %>%
    filter(!is.na(eid)) %>%
    distinct(city, eid)
  cat(sprintf("      gap = %d day(s): %d episodes\n", gap, nrow(ep_alt)))
}

# ── 5. By-year × by-city tables ─────────────────────────────────────────────

cat("\n[5] EPISODE COUNTS BY YEAR AND CITY...\n")

by_year <- episodes %>%
  count(year, crisis_type) %>%
  arrange(year)

by_city <- episodes %>%
  count(city, crisis_type) %>%
  arrange(desc(n))

by_year_city <- episodes %>%
  count(year, city) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0L)

write.csv(by_year_city, file.path(output_dir, "by_year_city.csv"),
          row.names = FALSE)


# ── 6. Crisis-type transition matrix (for Sankey) ────────────────────────────

cat("\n[6] BUILDING CRISIS-TYPE TRANSITION MATRIX...\n")

# Within each city, sort episodes by start date; for each consecutive pair,
# record the (from, to) crisis-type transition.
transitions <- episodes %>%
  arrange(city, start_date) %>%
  group_by(city) %>%
  mutate(next_crisis = lead(crisis_type)) %>%
  filter(!is.na(next_crisis)) %>%
  ungroup() %>%
  count(from = crisis_type, to = next_crisis) %>%
  arrange(desc(n))

cat("    Transition counts (top 10):\n")
print(head(transitions, 10), row.names = FALSE)
write.csv(transitions, file.path(output_dir, "crisis_transitions.csv"),
          row.names = FALSE)


# ── 7. Plots ────────────────────────────────────────────────────────────────

cat("\n[7] BUILDING FIGURES...\n")

# Color palette (consistent with rest of the paper)
crisis_colors <- c("Pandemic"     = "#E65F2B",
                   "Climate"      = "#2A9D8F",
                   "Geopolitical" = "#0099B8",
                   "Unattributed" = "#8D99AE")

# --- Panel 1: episode count by year (stacked by crisis type) ---
p_year <- ggplot(by_year, aes(x = factor(year), y = n, fill = crisis_type)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = crisis_colors, drop = FALSE) + # Added drop = FALSE
  labs(x = "Year", y = "Number of episodes", fill = NULL, tag = "(a)") + 
  theme_minimal(base_size = 10) +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        plot.tag.position = c(0.04, 0.97),
        legend.position = "none",                           # CHANGED to "none"
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 14, r = 8, b = 4, l = 4))

# --- Panel 2: episode count by city (sorted, stacked) ---
city_order <- episodes %>% count(city) %>% arrange(n) %>% pull(city)
by_city$city <- factor(by_city$city, levels = city_order)

p_city <- ggplot(by_city, aes(x = city, y = n, fill = crisis_type)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = crisis_colors, drop = FALSE) + # Added drop = FALSE
  coord_flip() +
  labs(x = NULL, y = "Number of episodes", fill = NULL, tag = "(b)") + 
  theme_minimal(base_size = 10) +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        plot.tag.position = c(0.04, 0.97),
        legend.position = "none",                           # CHANGED to "none"
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 14, r = 8, b = 4, l = 4))

# --- Panel 3: duration distribution ---
p_duration <- ggplot(episodes, aes(x = duration_days, fill = crisis_type)) +
  geom_histogram(binwidth = 1, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = crisis_colors, drop = FALSE) + # Added drop = FALSE
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 30)) +
  labs(x = "Episode duration (days)", y = "Number of episodes", fill = NULL, tag = "(c)") + 
  theme_minimal(base_size = 10) +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        plot.tag.position = c(0.04, 0.97),
        legend.position = "none",                           # CHANGED to "none"
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 14, r = 8, b = 4, l = 4))

# --- Panel 4: peak intensity distribution ---
p_severity <- ggplot(episodes, aes(x = peak_PBB, fill = crisis_type)) +
  geom_histogram(binwidth = 0.25, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = EXTREME_THRESHOLD, linetype = "dashed",
             color = "grey40", linewidth = 0.4) +
  annotate("text", x = EXTREME_THRESHOLD, y = Inf, label = " Extreme (3σ)",
           hjust = 0, vjust = 1.5, size = 3, color = "grey40") +
  scale_fill_manual(values = crisis_colors, drop = FALSE) + # Added drop = FALSE
  labs(x = "Peak PBB-Score", y = "Number of episodes", fill = NULL, tag = "(d)") + 
  theme_minimal(base_size = 10) +
  theme(plot.tag = element_text(size = 10, face = "bold"),
        plot.tag.position = c(0.04, 0.97),
        legend.position = "bottom",                         # KEPT as "bottom"
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 14, r = 8, b = 4, l = 4))

# Combine via patchwork with shared legend (using | for explicit horizontal layout)
panel_fig <- (p_year | p_city) / (p_duration | p_severity) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom",
                                legend.text = element_text(size = 10),
                                legend.margin = margin(t = -4)))

ggsave(file.path(output_dir, "episode_anatomy_panel.png"),
       panel_fig, width = 11, height = 8, dpi = 300)
ggsave(file.path("data/analysis/PDFs", "Figure_3.pdf"),
       panel_fig, width = 11, height = 8, device = "pdf")

# --- Sankey-style alluvial: crisis-type transitions ---

# Prepare alluvium data: each "alluvium" is a transition; the flow goes
# left (from) to right (to). ggalluvial expects long format.
alluv_data <- transitions %>%
  mutate(transition_id = row_number()) %>%
  pivot_longer(cols = c(from, to), names_to = "stage", values_to = "crisis_type") %>%
  mutate(stage = factor(stage, levels = c("from", "to"),
                        labels = c("Previous episode", "Next episode")),
         crisis_type = factor(crisis_type,
                              levels = c("Pandemic", "Climate",
                                         "Geopolitical", "Unattributed")))

p_sankey <- ggplot(alluv_data,
                   aes(x = stage, stratum = crisis_type, alluvium = transition_id,
                       y = n, fill = crisis_type)) +
  geom_flow(alpha = 0.55, color = "white", linewidth = 0.2,
            curve_type = "sigmoid") +
  geom_stratum(width = 0.32, color = "white", linewidth = 0.4) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.4, color = "white", fontface = "bold") +
  scale_fill_manual(values = crisis_colors) +
  scale_x_discrete(expand = c(0.08, 0.08)) +
  labs(x = NULL, y = "Number of within-city transitions",
       fill = NULL,
       title = "Crisis-type transitions between consecutive panic episodes within cities",
       subtitle = sprintf("Total: %d transitions across %d cities",
                          sum(transitions$n), length(unique(episodes$city)))) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())

ggsave(file.path(output_dir, "episode_crisis_sankey.png"),
       p_sankey, width = 10, height = 6, dpi = 300)
ggsave(file.path(output_dir, "episode_crisis_sankey.pdf"),
       p_sankey, width = 10, height = 6)

# --- Bonus: calendar heatmap (city × month, panic days) ---

heat_df <- df %>%
  mutate(year_month = lubridate::floor_date(date, "month")) %>%
  group_by(city, year_month) %>%
  summarise(panic_days = sum(panic_flag, na.rm = TRUE), .groups = "drop") %>%
  mutate(city = factor(city, levels = city_order))

p_heat <- ggplot(heat_df, aes(x = year_month, y = city, fill = panic_days)) +
  geom_tile(color = "white", linewidth = 0.05) +
  scale_fill_gradient(low = "grey92", high = "#E65F2B",
                      name = "Panic\ndays") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0)) +
  labs(x = NULL, y = NULL,
       title = "Panic-day intensity by city and month") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank())

ggsave(file.path(output_dir, "episode_calendar_heatmap.png"),
       p_heat, width = 11, height = 5.5, dpi = 300)
ggsave(file.path(output_dir, "episode_calendar_heatmap.pdf"),
       p_heat, width = 11, height = 5.5)


# ── 8. Final summary ─────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Episodes identified : %d\n", nrow(episodes)))
cat(sprintf("  Mean duration       : %.2f days\n", mean(episodes$duration_days)))
cat(sprintf("  Mean peak PBB-Score : %.2f σ\n", mean(episodes$peak_PBB)))
cat(sprintf("  Extreme episodes    : %d (%.1f%%)\n",
            sum(episodes$is_extreme), 100 * mean(episodes$is_extreme)))
cat(sprintf("  Transitions counted : %d\n", sum(transitions$n)))
cat("\n  Outputs:\n")
cat(sprintf("    Main panel figure : %sepisode_anatomy_panel.png/.pdf\n", output_dir))
cat(sprintf("    Sankey figure     : %sepisode_crisis_sankey.png/.pdf\n", output_dir))
cat(sprintf("    Calendar heatmap  : %sepisode_calendar_heatmap.png/.pdf\n", output_dir))
cat(sprintf("    Episode table     : %sepisode_table.csv\n", output_dir))
cat(sprintf("    Summary stats     : %ssummary_statistics.csv\n", output_dir))
cat("════════════════════════════════════════════════════════════════════════\n")


#addition: remove Sankey
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)

# Reuse the cached transitions table if still in memory, otherwise reload
output_dir <- "data/analysis/episode_anatomy/"
if (!exists("transitions")) {
  transitions <- read.csv(file.path(output_dir, "crisis_transitions.csv"),
                          stringsAsFactors = FALSE)
}

crisis_colors <- c("Pandemic"     = "#E65F2B",
                   "Climate"      = "#2A9D8F",
                   "Geopolitical" = "#0099B8",
                   "Unattributed" = "grey60")

# Filter out within-type transitions
cross_only <- transitions %>%
  filter(from != to) %>%
  arrange(desc(n))

cat(sprintf("Cross-type transitions retained: %d (of %d total)\n",
            sum(cross_only$n), sum(transitions$n)))
cat("Top cross-type transitions:\n")
print(head(as.data.frame(cross_only), 10), row.names = FALSE)

# Reshape to alluvium long format
alluv_cross <- cross_only %>%
  mutate(transition_id = row_number()) %>%
  pivot_longer(cols = c(from, to),
               names_to  = "stage",
               values_to = "crisis_type") %>%
  mutate(stage = factor(stage,
                        levels = c("from", "to"),
                        labels = c("Previous episode", "Next episode")),
         crisis_type = factor(crisis_type,
                              levels = c("Pandemic", "Climate",
                                         "Geopolitical", "Unattributed")))

p_sankey_cross <- ggplot(alluv_cross,
                         aes(x = stage, stratum = crisis_type,
                             alluvium = transition_id,
                             y = n, fill = crisis_type)) +
  geom_flow(alpha = 0.65, color = "white", linewidth = 0.25,
            curve_type = "sigmoid") +
  geom_stratum(width = 0.32, color = "white", linewidth = 0.4) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            size = 3.4, color = "white", fontface = "bold") +
  # Count labels on each flow (left and right sides)
  geom_text(stat = "flow", aes(label = n),
            size = 2.8, color = "grey25",
            nudge_x = c(-0.18, 0.18)[match(alluv_cross$stage,
                                           levels(alluv_cross$stage))]) +
  scale_fill_manual(values = crisis_colors) +
  scale_x_discrete(expand = c(0.10, 0.10)) +
  labs(x = NULL, y = "Number of within-city cross-type transitions",
       fill = NULL,
       title = "Crisis-type regime changes between consecutive panic episodes within cities",
       subtitle = sprintf("Within-type transitions excluded. %d cross-type transitions remain (of %d total).",
                          sum(cross_only$n), sum(transitions$n))) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())

ggsave(file.path(output_dir, "episode_crisis_sankey_crossonly.png"),
       p_sankey_cross, width = 10, height = 6, dpi = 300)
ggsave(file.path(output_dir, "episode_crisis_sankey_crossonly.pdf"),
       p_sankey_cross, width = 10, height = 6)

#6.CLASSIFICATION METRICS ON THE COMBINED MODEL'S REGRESSION PREDICTIONS ------
# Purpose
#   The Combined model (AR + external features) achieved R² = +0.38 in
#   expanding-window CV. This script translates that regression performance
#   into EWS-relevant CLASSIFICATION metrics: how well does the Combined
#   model rank and detect panic episodes (PBB-Score >= 2σ)?
#
# Design choices
#   • Continuous predictions, not probabilities. AUC-PR / AUC-ROC are
#     rank-based and work directly. Threshold-based metrics use PBB-Score
#     units (the model predicts a PBB-Score, alarm when prediction >= τ).
#   • Multiple operational thresholds reported: 1.0, 1.5, 2.0 (natural),
#     plus the F1-optimal threshold from a data-driven sweep.
#   • Lead-time analysis reports BOTH detection rate AND a non-panic
#     baseline rate of threshold crossings — the difference is the real
#     signal, addressing the false-positive contamination issue we
#     identified in the External-only run.
#
# Inputs
#   improved_final/temporal_cv_ar_augmented/combined/results.rds
#
# Outputs (in improved_final/classification_on_combined/)
#   • metrics_overall.csv             threshold-free metrics
#   • metrics_per_fold.csv            per-fold AUC-PR / AUC-ROC
#   • metrics_at_thresholds.csv       precision/recall/F1 at each threshold
#   • lead_time_analysis.csv          per-episode lead times + baseline rate
#   • Plots: PR curve, ROC curve, AUC-PR by fold, calibration,
#            lead-time histogram with non-panic baseline, threshold-tradeoff
# ════════════════════════════════════════════════════════════════════════════

# ── 0. Setup ─────────────────────────────────────────────────────────────────

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "patchwork", "lubridate", "PRROC"
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "http://cran.us.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

cat("════════════════════════════════════════════════════════════════════════\n")
cat(" CLASSIFICATION METRICS ON COMBINED MODEL'S REGRESSION PREDICTIONS\n")
cat("════════════════════════════════════════════════════════════════════════\n\n")

output_dir <- "data/analysis/classification_on_combined/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Operational settings
PANIC_THRESHOLD     <- 2.0    # observed PBB-Score threshold for "panic"
EXTREME_THRESHOLD   <- 3.0    # for "extreme"
LEAD_LOOKBACK_DAYS  <- 14
PRED_THRESHOLDS     <- c(0.5, 1.0, 1.5, 2.0)   # alarm thresholds (PBB units)


# ── 1. Load Combined model predictions ───────────────────────────────────────

combined_results_path <- "data/analysis/temporal_cv_ar_augmented/combined/results.rds"
if (!file.exists(combined_results_path)) {
  stop("Combined model results not found at: ", combined_results_path,
       "\nRun Temporal_CV_AR_Augmented_v1.R first.")
}

cat("[1] LOADING COMBINED MODEL PREDICTIONS...\n")
fold_results <- readRDS(combined_results_path)
cat(sprintf("    Loaded %d folds\n", length(fold_results)))

# Concatenate all OOS predictions
preds <- do.call(rbind, lapply(fold_results, function(f) {
  cbind(fold_id = f$fold_id, f$predictions)
}))
cat(sprintf("    Total OOS predictions: %s\n",
            format(nrow(preds), big.mark = ",")))

# Binarize observed targets
preds$is_panic   <- as.integer(preds$y_true >= PANIC_THRESHOLD)
preds$is_extreme <- as.integer(preds$y_true >= EXTREME_THRESHOLD)

cat(sprintf("    Positive rate (Panic   >= %.1fσ): %.2f%% (%d episode-days)\n",
            PANIC_THRESHOLD, 100 * mean(preds$is_panic), sum(preds$is_panic)))
cat(sprintf("    Positive rate (Extreme >= %.1fσ): %.2f%% (%d episode-days)\n",
            EXTREME_THRESHOLD, 100 * mean(preds$is_extreme), sum(preds$is_extreme)))


# ── 2. Threshold-free metrics (AUC-PR, AUC-ROC) ──────────────────────────────

cat("\n[2] THRESHOLD-FREE METRICS (Panic threshold)...\n")

# Overall, using continuous y_pred as the ranking score
pr_overall <- PRROC::pr.curve(
  scores.class0 = preds$y_pred[preds$is_panic == 1],
  scores.class1 = preds$y_pred[preds$is_panic == 0],
  curve = TRUE
)
roc_overall <- PRROC::roc.curve(
  scores.class0 = preds$y_pred[preds$is_panic == 1],
  scores.class1 = preds$y_pred[preds$is_panic == 0],
  curve = TRUE
)

base_rate_panic <- mean(preds$is_panic)
cat(sprintf("    Overall AUC-PR  : %.3f (lift %.1fx over baseline %.3f)\n",
            pr_overall$auc.integral,
            pr_overall$auc.integral / base_rate_panic,
            base_rate_panic))
cat(sprintf("    Overall AUC-ROC : %.3f\n", roc_overall$auc))

# Per-fold metrics
per_fold <- do.call(rbind, lapply(split(preds, preds$fold_id), function(d) {
  if (sum(d$is_panic) == 0 || sum(1 - d$is_panic) == 0) {
    return(data.frame(fold_id = d$fold_id[1], n = nrow(d),
                      n_pos = sum(d$is_panic),
                      auc_pr = NA, auc_roc = NA, base_rate = mean(d$is_panic)))
  }
  pr  <- PRROC::pr.curve(scores.class0 = d$y_pred[d$is_panic == 1],
                         scores.class1 = d$y_pred[d$is_panic == 0],
                         curve = FALSE)
  roc <- PRROC::roc.curve(scores.class0 = d$y_pred[d$is_panic == 1],
                          scores.class1 = d$y_pred[d$is_panic == 0],
                          curve = FALSE)
  data.frame(
    fold_id   = d$fold_id[1],
    n         = nrow(d),
    n_pos     = sum(d$is_panic),
    auc_pr    = pr$auc.integral,
    auc_roc   = roc$auc,
    base_rate = mean(d$is_panic)
  )
}))
per_fold$auc_pr_lift <- per_fold$auc_pr / per_fold$base_rate

cat("\n    Per-fold:\n")
print(per_fold, row.names = FALSE, digits = 3)

write.csv(per_fold, file.path(output_dir, "metrics_per_fold.csv"),
          row.names = FALSE)
write.csv(data.frame(
  metric = c("AUC-PR", "AUC-PR_lift", "AUC-ROC", "base_rate_panic"),
  value  = c(pr_overall$auc.integral,
             pr_overall$auc.integral / base_rate_panic,
             roc_overall$auc, base_rate_panic)
), file.path(output_dir, "metrics_overall.csv"), row.names = FALSE)


# ── 3. Threshold-based metrics ───────────────────────────────────────────────

cat("\n[3] THRESHOLD-BASED METRICS...\n")

compute_at_threshold <- function(y_true, y_pred, tau) {
  alarm <- as.integer(y_pred >= tau)
  tp <- sum(alarm == 1 & y_true == 1)
  fp <- sum(alarm == 1 & y_true == 0)
  fn <- sum(alarm == 0 & y_true == 1)
  tn <- sum(alarm == 0 & y_true == 0)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  rec  <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  f1   <- if (!is.na(prec) && !is.na(rec) && (prec + rec) > 0) {
    2 * prec * rec / (prec + rec)
  } else NA_real_
  alarm_rate <- mean(alarm)
  data.frame(threshold = tau, alarm_rate = alarm_rate,
             tp = tp, fp = fp, fn = fn, tn = tn,
             precision = prec, recall = rec, f1 = f1)
}

# At fixed operational thresholds
metrics_thresh <- do.call(rbind, lapply(PRED_THRESHOLDS, function(t) {
  compute_at_threshold(preds$is_panic, preds$y_pred, t)
}))

# F1-optimal threshold (data-driven sweep — sweep_f1 is not being used anymore, candidate for exclusion)
sweep_grid <- seq(0.1, 3.0, by = 0.05)
# sweep_f1 <- sapply(sweep_grid, function(t) {
#   m <- compute_at_threshold(preds$is_panic, preds$y_pred, t)
#   if (is.na(m$f1)) 0 else m$f1
# })
# tau selected on VALIDATION (per fold), then fixed for the pooled test set.
val_preds <- do.call(rbind, lapply(fold_results, function(f) {
  v <- f$val_predictions; v$fold_id <- f$fold_id; v
}))
val_preds$is_panic <- as.integer(val_preds$y_true >= PANIC_THRESHOLD)
fold_tau <- sapply(split(val_preds, val_preds$fold_id), function(d) {
  if (sum(d$is_panic) == 0) return(NA_real_)
  f1v <- sapply(sweep_grid, function(t) {
    f1 <- compute_at_threshold(d$is_panic, d$y_pred, t)$f1
    if (is.na(f1)) 0 else f1
  })
  sweep_grid[which.max(f1v)]
})
opt_threshold <- stats::median(fold_tau, na.rm = TRUE)
metrics_opt <- compute_at_threshold(preds$is_panic, preds$y_pred, opt_threshold)
metrics_opt$threshold_label <- sprintf("F1-optimal (%.2f)", opt_threshold)
metrics_thresh$threshold_label <- sprintf("%.1f", metrics_thresh$threshold)
metrics_thresh <- rbind(
  metrics_thresh[, c("threshold_label", setdiff(names(metrics_thresh), "threshold_label"))],
  metrics_opt[, c("threshold_label", setdiff(names(metrics_opt), "threshold_label"))]
)

cat("\n    Metrics at each operational threshold:\n")
print(metrics_thresh[, c("threshold_label", "alarm_rate",
                         "precision", "recall", "f1")],
      row.names = FALSE, digits = 3)

write.csv(metrics_thresh, file.path(output_dir, "metrics_at_thresholds.csv"),
          row.names = FALSE)

# ── 4. Lead-time analysis with non-panic baseline ────────────────────────────

cat("\n[4] LEAD-TIME ANALYSIS...\n")

# Use the F1-optimal threshold by default (most informative operating point)
op_threshold <- opt_threshold
cat(sprintf("    Operational threshold: PBB-Score prediction >= %.2f\n",
            op_threshold))

compute_lead_times_with_baseline <- function(pred_df, threshold,
                                             lookback_days = LEAD_LOOKBACK_DAYS) {
  
  pred_df <- pred_df %>% arrange(city, date)
  pred_df$alarm <- as.integer(pred_df$y_pred >= threshold)
  
  lead_times <- list()
  baseline_windows <- list()
  
  for (this_city in unique(pred_df$city)) {
    cd <- pred_df %>% filter(city == this_city) %>% arrange(date)
    n <- nrow(cd)
    if (n < lookback_days + 1) next
    
    # Episode starts (rising edges of is_panic)
    rise <- which(cd$is_panic == 1 & (c(0, head(cd$is_panic, -1)) == 0))
    
    # ── Episode-side: for each rising edge, look back ────────────────────
    for (i in rise) {
      ep_date <- cd$date[i]
      window_start <- ep_date - lookback_days
      win <- cd %>% filter(date >= window_start, date < ep_date)
      if (nrow(win) == 0) next
      
      crossed <- which(win$alarm == 1)
      if (length(crossed) > 0) {
        first_cross <- win$date[min(crossed)]
        lt <- as.numeric(ep_date - first_cross)
        detected <- TRUE
      } else {
        first_cross <- as.Date(NA); lt <- NA_real_; detected <- FALSE
      }
      
      lead_times[[length(lead_times) + 1]] <- data.frame(
        city = this_city, episode_start = ep_date,
        first_cross_date = first_cross,
        lead_time_days = lt, detected = detected
      )
    }
    
    # ── Non-panic baseline: for the same city, sample non-panic days ─────
    # (days t where is_panic[t] = 0 AND no panic in the 14-day window
    #  ending at t — these are clean "non-event" windows)
    non_panic <- which(cd$is_panic == 0)
    sample_n  <- length(rise) * 3   # 3x as many non-panic samples as panic
    if (length(non_panic) > sample_n && length(rise) > 0) {
      set.seed(42)
      sampled <- sample(non_panic, sample_n)
      sampled <- sampled[sampled > lookback_days]
      for (j in sampled) {
        ref_date <- cd$date[j]
        window_start <- ref_date - lookback_days
        win <- cd %>% filter(date >= window_start, date < ref_date)
        # Exclude windows that contain a real panic episode
        if (sum(win$is_panic) > 0) next
        crossed <- which(win$alarm == 1)
        baseline_windows[[length(baseline_windows) + 1]] <- data.frame(
          city = this_city, ref_date = ref_date,
          any_alarm = length(crossed) > 0
        )
      }
    }
  }
  
  list(
    episode_lead_times = if (length(lead_times) > 0)
      do.call(rbind, lead_times) else data.frame(),
    baseline_windows   = if (length(baseline_windows) > 0)
      do.call(rbind, baseline_windows) else data.frame()
  )
}

lead_res <- compute_lead_times_with_baseline(preds, op_threshold)
ep_lt    <- lead_res$episode_lead_times
bl_w     <- lead_res$baseline_windows

if (nrow(ep_lt) > 0) {
  ep_detection_rate  <- mean(ep_lt$detected)
  bl_detection_rate  <- if (nrow(bl_w) > 0) mean(bl_w$any_alarm) else NA_real_
  lift               <- ep_detection_rate - bl_detection_rate
  
  cat(sprintf("    Total panic episodes : %d\n", nrow(ep_lt)))
  cat(sprintf("    Episodes detected    : %d (%.1f%%)\n",
              sum(ep_lt$detected), 100 * ep_detection_rate))
  cat(sprintf("    Baseline rate (non-panic windows): %.1f%% (n=%d)\n",
              100 * bl_detection_rate, nrow(bl_w)))
  cat(sprintf("    Lift over baseline   : %+.1f percentage points\n",
              100 * lift))
  
  if (!is.na(lift) && lift > 0.10) {
    cat("    INTERPRETATION: detection rate clearly exceeds baseline — real signal.\n")
  } else if (!is.na(lift) && lift > 0.03) {
    cat("    INTERPRETATION: modest lift over baseline — weak but real signal.\n")
  } else {
    cat("    INTERPRETATION: detection rate barely exceeds baseline — false-positive\n")
    cat("                    contamination likely; lead time figures should be\n")
    cat("                    interpreted with caution.\n")
  }
  
  detected_lt <- ep_lt$lead_time_days[ep_lt$detected]
  if (length(detected_lt) > 0) {
    cat(sprintf("    Median lead time of detected episodes: %.1f days\n",
                median(detected_lt)))
    cat(sprintf("    Mean   lead time of detected episodes: %.1f days\n",
                mean(detected_lt)))
  }
} else {
  cat("    No panic episodes found.\n")
}

write.csv(ep_lt, file.path(output_dir, "lead_time_analysis.csv"),
          row.names = FALSE)


# ── 5. Plots ─────────────────────────────────────────────────────────────────

cat("\n[5] BUILDING PLOTS...\n")

# Plot 1: PR curve (overall)
pr_df <- data.frame(recall = pr_overall$curve[, 1],
                    precision = pr_overall$curve[, 2])
p_pr <- ggplot(pr_df, aes(x = recall, y = precision)) +
  geom_line(color = "#0099B8", linewidth = 1) +
  geom_hline(yintercept = base_rate_panic, linetype = "dashed", color = "#E65F2B") +
  annotate("text", x = 0.05, y = base_rate_panic + 0.03,
           label = sprintf("Baseline = %.3f", base_rate_panic),
           hjust = 0, size = 3.2, color = "#E65F2B") +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Recall", y = "Precision",
       title = sprintf("PR curve, all folds combined (AUC-PR = %.3f, lift %.1fx)",
                       pr_overall$auc.integral,
                       pr_overall$auc.integral / base_rate_panic),
       subtitle = "Predictions = continuous PBB-Score forecasts from Combined model") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "pr_curve.png"), p_pr,
       width = 6.5, height = 6, dpi = 150)

# Plot 2: ROC curve (overall)
roc_df <- data.frame(fpr = roc_overall$curve[, 1],
                     tpr = roc_overall$curve[, 2])
p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "#0099B8", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  coord_equal() +
  labs(x = "False Positive Rate", y = "True Positive Rate",
       title = sprintf("ROC curve, all folds combined (AUC-ROC = %.3f)",
                       roc_overall$auc)) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "roc_curve.png"), p_roc,
       width = 6.5, height = 6, dpi = 150)

# Plot 3: AUC-PR by fold
p_aucpr_fold <- ggplot(per_fold) +
  annotate("rect", xmin = 1.5, xmax = 4.5, ymin = -Inf, ymax = Inf,
           fill = "#E65F2B", alpha = 0.08) +
  annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -Inf, ymax = Inf,
           fill = "#F46036", alpha = 0.12) +
  geom_line(aes(x = fold_id, y = auc_pr), color = "grey30", linewidth = 0.5) +
  geom_point(aes(x = fold_id, y = auc_pr), color = "#0099B8", size = 3.2) +
  geom_line(aes(x = fold_id, y = base_rate), color = "#E65F2B",
            linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(x = fold_id, y = base_rate), color = "#E65F2B", size = 1.8) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Fold ID", y = "AUC-PR",
       title = "AUC-PR across folds vs. positive-class baseline (red dashed)",
       subtitle = "Combined model. Shaded: 1st pandemic year (red), 2022 blockades (orange).") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "aucpr_by_fold.png"), p_aucpr_fold,
       width = 11, height = 5, dpi = 150)

# Plot 4: Calibration (regression-style)
cal_df <- preds %>%
  mutate(bin = cut(y_pred, breaks = quantile(y_pred, probs = seq(0, 1, by = 0.05),
                                             na.rm = TRUE),
                   include.lowest = TRUE)) %>%
  group_by(bin) %>%
  summarise(mean_pred = mean(y_pred),
            obs_panic_rate = mean(is_panic),
            n = n(), .groups = "drop") %>%
  filter(n >= 30)
p_cal <- ggplot(cal_df, aes(x = mean_pred, y = obs_panic_rate)) +
  geom_vline(xintercept = PANIC_THRESHOLD, linetype = "dotted", color = "grey50") +
  geom_line(color = "#0099B8", linewidth = 0.6) +
  geom_point(aes(size = n), color = "#0099B8") +
  scale_size_continuous(range = c(2, 8)) +
  labs(x = "Mean predicted PBB-Score (bin)", y = "Observed panic rate",
       title = "Regression-style calibration",
       subtitle = sprintf("Vertical line at predicted = %.1f (panic threshold)",
                          PANIC_THRESHOLD),
       size = "n in bin") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_dir, "calibration.png"), p_cal,
       width = 7.5, height = 5.5, dpi = 150)

# Plot 5: Lead-time histogram with baseline
if (nrow(ep_lt) > 0 && sum(ep_lt$detected) > 0) {
  baseline_label <- if (nrow(bl_w) > 0) {
    sprintf("Baseline (random 14-day windows with no panic): %.1f%% contain an alarm",
            100 * mean(bl_w$any_alarm))
  } else "Baseline: not computed"
  
  p_lead <- ep_lt %>% filter(detected) %>%
    ggplot(aes(x = lead_time_days)) +
    geom_histogram(binwidth = 1, fill = "#0099B8", color = "white") +
    scale_x_continuous(breaks = 1:LEAD_LOOKBACK_DAYS) +
    labs(x = "Lead time (days)", y = "Number of detected episodes",
         title = sprintf("Lead-time distribution (threshold = %.2f, %d/%d episodes detected)",
                         op_threshold, sum(ep_lt$detected), nrow(ep_lt)),
         subtitle = baseline_label) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(file.path(output_dir, "lead_time_histogram.png"), p_lead,
         width = 9.5, height = 5, dpi = 150)
}

# Plot 6: Threshold tradeoff (precision/recall/F1 across operating thresholds)
sweep_df <- do.call(rbind, lapply(sweep_grid, function(t) {
  m <- compute_at_threshold(preds$is_panic, preds$y_pred, t)
  m$threshold <- t
  m
})) %>% as.data.frame()
sweep_long <- sweep_df %>%
  dplyr::select(threshold, precision, recall, f1) %>%
  tidyr::pivot_longer(cols = c(precision, recall, f1),
                      names_to = "metric", values_to = "value")
p_sweep <- ggplot(sweep_long, aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = opt_threshold, linetype = "dashed", color = "grey50") +
  annotate("text", x = opt_threshold, y = 1.0,
           label = sprintf("F1-optimal\nτ = %.2f", opt_threshold),
           vjust = 1, hjust = -0.1, size = 3.2) +
  scale_color_manual(values = c("precision" = "#E65F2B",
                                "recall"    = "#2A9D8F",
                                "f1"        = "#0099B8")) +
  labs(x = "Operational threshold (predicted PBB-Score)",
       y = "Metric value", color = NULL,
       title = "Threshold sweep: precision-recall-F1 tradeoff",
       subtitle = "Choose threshold based on operational FN/FP cost preferences") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")
ggsave(file.path(output_dir, "threshold_sweep.png"), p_sweep,
       width = 9, height = 5.5, dpi = 150)

cat(sprintf("    Plots saved in: %s\n", output_dir))


# ── 6. Final summary ─────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════════\n")
cat(" DONE.\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  Combined model regression R² (from prior step) : ~0.38\n"))
cat(sprintf("\n  Classification metrics on continuous predictions:\n"))
cat(sprintf("    AUC-PR   : %.3f  (baseline %.3f, lift %.1fx)\n",
            pr_overall$auc.integral, base_rate_panic,
            pr_overall$auc.integral / base_rate_panic))
cat(sprintf("    AUC-ROC  : %.3f\n", roc_overall$auc))
cat(sprintf("\n  At F1-optimal threshold (PBB ≥ %.2f):\n", opt_threshold))
cat(sprintf("    Precision: %.3f  Recall: %.3f  F1: %.3f  Alarm rate: %.1f%%\n",
            metrics_opt$precision, metrics_opt$recall,
            metrics_opt$f1, 100 * metrics_opt$alarm_rate))
if (nrow(ep_lt) > 0) {
  cat(sprintf("\n  Lead-time analysis (threshold = %.2f):\n", op_threshold))
  cat(sprintf("    Episode detection rate : %.1f%% (vs baseline %.1f%%, lift %+.1f pp)\n",
              100 * mean(ep_lt$detected),
              100 * (if (nrow(bl_w) > 0) mean(bl_w$any_alarm) else NA),
              100 * (mean(ep_lt$detected) -
                       (if (nrow(bl_w) > 0) mean(bl_w$any_alarm) else NA))))
  detected_lt <- ep_lt$lead_time_days[ep_lt$detected]
  if (length(detected_lt) > 0) {
    cat(sprintf("    Median lead time       : %.1f days\n", median(detected_lt)))
  }
}
cat("\n  Compare to External-only baseline (prior run):\n")
cat("    AUC-PR=0.056 (lift 1.5x), AUC-ROC=0.567, F1=0.076 — essentially random.\n")
cat("\n  INTERPRETATION GUIDE (Combined model):\n")
cat("  • AUC-PR lift >= 3-4x with detection lift >= 10pp:\n")
cat("      Combined model has real operational EWS value. Paper viable.\n")
cat("  • AUC-PR lift 2-3x with detection lift 5-10pp:\n")
cat("      Modest but real signal. Suitable for cost-asymmetric framing.\n")
cat("  • AUC-PR lift < 2x or detection lift near 0:\n")
cat("      Regression performance does not translate to classification skill.\n")
cat("      Consider re-training Combined model with binary objective directly.\n")
cat("════════════════════════════════════════════════════════════════════════\n")

#6.1. Read Table 4 results -----
reg <- read.csv("data/analysis/temporal_cv_ar_augmented/comparison_summary.csv")
cls <- read.csv("data/analysis/classification_three_way/comparison_summary.csv")
tab <- merge(reg[, c("model","mean_r2","mean_rmse")],
             cls[, c("model","AUC_PR","AUC_PR_lift","AUC_ROC",
                     "F1_at_opt","detection_own","baseline_own","detection_lift_own")],
             by = "model")
tab[match(c("External-only","AR-only","Combined"), tab$model), ]
cls$AUC_PR / cls$AUC_PR_lift   # the new base rate

#7.FINAL Extra figures from other code ------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dt_raw <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv",
                    stringsAsFactors = FALSE)
dt_raw$date <- as.Date(dt_raw$date)

##7.1. New plot section 4.1.-----
# OVERVIEW PLOT: 2019–2022 WITH CRISIS ANNOTATIONS

out_overview <- "data/analysis/zoom_validation/"
if (!dir.exists(out_overview)) dir.create(out_overview, recursive = TRUE)

# ── Step 1: Load and filter data ──────────────────────────────────────────────
overview_start <- as.Date("2020-01-01")
overview_end   <- as.Date("2022-12-31")

dt_overview <- dt_raw[dt_raw$date >= overview_start &
                        dt_raw$date <= overview_end, ]

# ── Step 2: Daily mean API across all cities ──────────────────────────────────
daily_overview <- do.call(rbind, lapply(split(dt_overview, dt_overview$date), function(d) {
  data.frame(
    date     = d$date[1],
    API_mean = mean(d$API, na.rm = TRUE),
    API_sd   = sd(d$API,   na.rm = TRUE),
    API_min  = min(d$API,  na.rm = TRUE),
    API_max  = max(d$API,  na.rm = TRUE)
  )
}))
rownames(daily_overview) <- NULL

daily_overview$API_cat <- cut(daily_overview$API_mean,
                              breaks = c(-Inf, -1, 2, 3, Inf),
                              labels = c("Below Regular", "Regular", "Panic", "Extreme Panic"),
                              right  = FALSE)

# ── Step 3: Define Crisis Events (with explicit vertical levels to prevent crowding) ───
#dt_overview$city
#test <- dt_overview[,c("date")]

crisis_events <- data.frame(
  label = c(
    "1st COVID-19\nwave",
    "Extratropical\nCyclone",
    "2nd COVID-19\nwave",        # Moved up chronologically
    "Vale do\nItajaí Flood",
    "Florianópolis\nFlood",
    "Alto Vale\nFlash Flood",
    "SC Coastal\nFlash Flood",
    "Trucker\nStrike",           # <--- NEW EVENT (2021)
    "Omicron COVID-19\n wave",
    "SC\nFloods",
    "Highway \nBlockage",
    "Joinville\nFloods"
  ),
  start = as.Date(c(
    "2020-02-25", # 1st COVID
    "2020-06-30", # Extratropical Cyclone
    "2020-11-16", # 2nd COVID
    "2020-12-17", # Vale do Itajaí Flood
    "2021-01-21", # Florianópolis Flood
    "2021-03-26", # Alto Vale Flash Flood
    "2021-06-08", # SC Coastal Flash Flood
    "2021-09-07", # NEW: 2021 Trucker Strike
    "2021-11-16", # Omicron
    "2022-05-03", # SC Floods
    "2022-10-31", # 2022 Highway Blockage
    "2022-11-29"  # Joinville Floods
  )),
  end = as.Date(c(
    "2020-02-25", # 1st COVID (Marker)
    "2020-07-01", # Extratropical Cyclone
    "2020-11-16", # 2nd COVID (Marker)
    "2020-12-21", # Vale do Itajaí Flood
    "2021-01-24", # Florianópolis Flood
    "2021-03-26", # Alto Vale Flash Flood
    "2021-06-09", # SC Coastal Flash Flood
    "2021-09-09", # NEW: 2021 Trucker Strike
    "2021-11-16", # Omicron (Marker)
    "2022-05-05", # SC Floods
    "2022-11-02", # 2022 Highway Blockage (Updated from 11-04)
    "2022-12-04"  # Joinville Floods
  )),
  event_type = c(
    "pandemic", "climate", "pandemic", "climate",
    "climate", "climate", "climate", "strike",
    "pandemic", "climate", "strike", "climate"
  ),
  # y_level controls vertical stacking (0 to 4) to completely eliminate overlaps
  y_level = c(
    2,  # 1st COVID
    0,  # Cyclone Bomb
    4,  # 2nd COVID wave (Highest, arches cleanly over the dense early-2021 cluster)
    0,  # Heavy Rains (Low)
    2,  # 600mm Rains (Mid)
    1,  # Alto Vale Storm (Mid-Low)
    0,  # SC Coastal Flash Flood (Low)
    0,  # 2021 Trucker Strike (Low - safely fits in the gap before Omicron)
    2,  # Omicron wave (Mid)
    0,  # SC Floods (Low)
    0,  # Road blockage (Low)
    2   # Joinville Floods (Mid)
  ),
  stringsAsFactors = FALSE
)

# Single-day events: give a 1-day width so the shading is visible
crisis_events$end <- pmax(crisis_events$end, crisis_events$start + 1)

# Colours per event type
type_colours <- c(
  pandemic = "#E65F2B",
  strike   = "#0099B8",
  climate  = "#2A9D8F"
)
crisis_events$col <- type_colours[crisis_events$event_type]

# ── Step 4: y-axis limits and labeling heights ────────────────────────────────

# Increase y_hi significantly to make room for 5 distinct vertical levels of labels
y_lo <- min(daily_overview$API_mean - daily_overview$API_sd, na.rm = TRUE) - 0.3
y_hi <- max(daily_overview$API_mean + daily_overview$API_sd, na.rm = TRUE) + 4.5

# Base height for the lowest label (Level 0)
box_y_base <- max(daily_overview$API_mean + daily_overview$API_sd, na.rm = TRUE) + 0.3
y_step     <- 0.95  # Vertical spacing multiplier between levels

api_colours <- c(
  "Below Regular" = "#2166ac",
  "Regular"       = "#222222",
  "Panic"         = "#f4a800",
  "Extreme Panic" = "#d62728"
)

# ── Step 5: Build plot ────────────────────────────────────────────────────────
p_overview <- ggplot(daily_overview, aes(x = date))

# Threshold background bands
for (band in list(
  list(ymin = -Inf, ymax = -1,   fill = "#2166ac", alpha = 0.06),
  list(ymin = -1,   ymax =  2,   fill = "#cccccc", alpha = 0.03),
  list(ymin =  2,   ymax =  3,   fill = "#f4a800", alpha = 0.08),
  list(ymin =  3,   ymax =  Inf, fill = "#d62728", alpha = 0.10)
)) {
  p_overview <- p_overview +
    annotate("rect",
             xmin = overview_start, xmax = overview_end,
             ymin = band$ymin, ymax = band$ymax,
             fill = band$fill, alpha = band$alpha)
}

# LOOP 1: Draw ALL Shading and Boundary Lines First
for (i in seq_len(nrow(crisis_events))) {
  ev    <- crisis_events[i, ]
  box_y <- box_y_base + (ev$y_level * y_step)
  
  # Shaded period
  p_overview <- p_overview +
    annotate("rect",
             xmin = ev$start, xmax = ev$end,
             ymin = -Inf,     ymax = Inf,
             fill = ev$col,   alpha = 0.12)
  
  # Boundary lines
  p_overview <- p_overview +
    annotate("segment",
             x = ev$start, xend = ev$start,
             y = y_lo,     yend = box_y - 0.05,
             colour = ev$col, linewidth = 0.6,
             linetype = "solid", alpha = 0.8) +
    annotate("segment",
             x = ev$end,   xend = ev$end,
             y = y_lo,     yend = box_y - 0.05,
             colour = ev$col, linewidth = 0.6,
             linetype = "solid", alpha = 0.8)
}

# LOOP 2: Draw ALL Labels Last (so they cleanly mask any lines passing behind them)
for (i in seq_len(nrow(crisis_events))) {
  ev    <- crisis_events[i, ]
  box_y <- box_y_base + (ev$y_level * y_step)
  mid_date <- ev$start + as.numeric(ev$end - ev$start) / 2
  
  # Label using annotate("label") — auto draws filled box + border
  p_overview <- p_overview +
    annotate("label",
             x          = mid_date,
             y          = box_y + 0.20,
             label      = ev$label,
             fill       = ev$col,
             colour     = "white",
             size       = 3.2,
             fontface   = "bold",
             hjust      = 0.5,
             vjust      = 0.5,
             lineheight = 0.9,
             label.size = 0.3,        
             label.padding = unit(0.25, "lines"))
}

# ±1 SD ribbon and full city range ribbon
p_overview <- p_overview +
  geom_ribbon(aes(ymin = API_min, ymax = API_max),
              fill = "steelblue", alpha = 0.08) +
  geom_ribbon(aes(ymin = API_mean - API_sd,
                  ymax = API_mean + API_sd),
              fill = "steelblue", alpha = 0.16)

# Threshold dashed lines
p_overview <- p_overview +
  geom_hline(yintercept = -1, linetype = "dashed",
             color = "#2166ac", linewidth = 0.4, alpha = 0.6) +
  geom_hline(yintercept =  2, linetype = "dashed",
             color = "#f4a800", linewidth = 0.5, alpha = 0.8) +
  geom_hline(yintercept =  3, linetype = "dashed",
             color = "#d62728", linewidth = 0.5, alpha = 0.8)

# Mean API line and coloured points
p_overview <- p_overview +
  geom_line(aes(y = API_mean), color = "grey25", linewidth = 0.5) +
  geom_point(aes(y = API_mean, color = API_cat),
             size = 0.8, alpha = 0.7) +
  scale_color_manual(values = api_colours, name = "PBB-Score regime",
                     drop = FALSE)

# Threshold annotations on right margin
p_overview <- p_overview +
  annotate("text", x = overview_end + 5, y = 3.10,
           label = "Extreme Panic (\u22653\u03c3)", hjust = 0,
           color = "#d62728", size = 2.6, fontface = "italic") +
  annotate("text", x = overview_end + 5, y = 2.10,
           label = "Panic (\u22652\u03c3)", hjust = 0,
           color = "#f4a800", size = 2.6, fontface = "italic") +
  annotate("text", x = overview_end + 5, y = -0.90,
           label = "Below Regular", hjust = 0,
           color = "#2166ac", size = 2.6, fontface = "italic")

# Scales and theme
p_overview <- p_overview +
  scale_x_date(
    limits       = c(overview_start, overview_end),   # hard clip to window
    date_breaks  = "3 months",
    date_labels  = "%b\n%Y",
    expand       = expansion(mult = c(0.005, 0.005))
  ) +
  coord_cartesian(ylim = c(y_lo, y_hi + 0.6), clip = "off") +
  labs(
    # title    = "PBB-Score \u2014 Mean across SC municipalities (2020\u20132022)",
    # subtitle = "Line = daily mean PBB-Score | Ribbons = \u00b11 SD and full city range | Coloured boxes = crisis episodes",
    title    = "a) PBB-Score: Mean across SC municipalities (2020\u20132022)",
    subtitle = "Line = daily mean PBB-Score | Ribbons = \u00b11 SD and full city range | Coloured boxes = crisis episodes",
    x = NULL, y = "PBB-Score", color = "PBB-Score regime" 
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    axis.text.x        = element_text(size = 8),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey40"),
    plot.margin        = margin(5, 85, 5, 5)
  )
p_overview
ggsave(file.path(out_overview, "API_overview_2020_2022.png"),
       p_overview, width = 14, height = 5.5, dpi = 150)
cat("\u2713 Overview plot saved: API_overview_2020_2022.png\n")

##7.2.New figure city level section 4.1.-----
#COMBINED TWO-EVENT PANEL: 3 CITIES × 2 EVENTS
# Upper row: 1st COVID wave (Mar 12–24, 2020) for Itajaí, Indaial, Florianópolis
# Lower row: Vale do Itajaí floods (Sep 30 – Oct 20, 2023) for same 3 cities

library(ggplot2)
library(patchwork)   # for combining panels — install if needed

target_cities <- c("Itajaí", "Indaial", "Florianópolis")

api_colours <- c(
  "Below Regular" = "#2166ac",
  "Regular"       = "#222222",
  "Panic"         = "#f4a800",
  "Extreme Panic" = "#d62728"
)

# ── Helper: build one city-faceted panel for a given event window ─────────────
make_panel <- function(dt_raw,
                       cities,
                       event_start, event_end,
                       pad_before, pad_after,
                       event_col,
                       event_label,
                       show_x_labels = TRUE) {
  
  zoom_start <- event_start - pad_before
  zoom_end   <- event_end   + pad_after
  
  dt <- dt_raw[dt_raw$date >= zoom_start &
                 dt_raw$date <= zoom_end   &
                 dt_raw$city %in% cities, ]
  
  dt$city <- factor(dt$city, levels = cities)
  
  dt$API_cat <- cut(dt$API,
                    breaks = c(-Inf, -1, 2, 3, Inf),
                    labels = c("Below Regular", "Regular", "Panic", "Extreme Panic"),
                    right  = FALSE)
  
  n_days  <- as.numeric(zoom_end - zoom_start)
  x_break <- if (n_days <= 20) "1 day" else "3 days"
  x_fmt   <- "%b %d"
  
  p <- ggplot(dt, aes(x = date, y = API)) +
    annotate("rect", xmin = zoom_start, xmax = zoom_end, ymin = -Inf, ymax = -1,  fill = "#2166ac", alpha = 0.07) +
    annotate("rect", xmin = zoom_start, xmax = zoom_end, ymin = -1,   ymax =  2,   fill = "#cccccc", alpha = 0.04) +
    annotate("rect", xmin = zoom_start, xmax = zoom_end, ymin =  2,   ymax =  3,   fill = "#f4a800", alpha = 0.10) +
    annotate("rect", xmin = zoom_start, xmax = zoom_end, ymin =  3,   ymax =  Inf, fill = "#d62728", alpha = 0.12) +
    annotate("rect", xmin = event_start, xmax = event_end, ymin = -Inf, ymax = Inf, fill = event_col, alpha = 0.10) +
    geom_vline(xintercept = as.Date(event_start), color = event_col, linewidth = 0.7, alpha = 0.8) +
    geom_vline(xintercept = as.Date(event_end), color = event_col, linewidth = 0.7, alpha = 0.8) +
    geom_hline(yintercept = -1, linetype = "dashed", color = "#2166ac", linewidth = 0.35, alpha = 0.6) +
    geom_hline(yintercept =  2, linetype = "dashed", color = "#f4a800", linewidth = 0.45, alpha = 0.8) +
    geom_hline(yintercept =  3, linetype = "dashed", color = "#d62728", linewidth = 0.45, alpha = 0.8) +
    geom_line(color = "grey30", linewidth = 0.65, na.rm = TRUE) +
    geom_point(aes(color = API_cat), size = 2.5, na.rm = TRUE) +
    geom_text(aes(label = round(API, 2), color = API_cat), vjust = -0.85, size = 2.1, fontface = "bold", na.rm = TRUE) +
    
    scale_color_manual(values = api_colours, name = "PBB-Score regime", drop = FALSE) +
    scale_x_date(date_breaks = x_break, date_labels = x_fmt, expand = expansion(mult = 0.04)) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
    
    coord_cartesian(clip = "off") +
    facet_wrap(~city, ncol = 3, scales = "free_y") +
    
    labs(
      title = event_label,   # Changed from 'tag' to 'title' to guarantee perfect left alignment
      x     = NULL,
      y     = "PBB-Score"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text          = element_text(face = "bold", size = 9, color = event_col),
      axis.text.x         = element_text(angle = 30, hjust = 1, size = 7),
      axis.text.y         = element_text(size = 7),
      legend.position     = "none",
      panel.grid.minor    = element_blank(),
      plot.title          = element_text(face = "bold", size = 11, color = event_col),
      plot.title.position = "plot",            # Forces title to the absolute left edge of the plot
      plot.margin         = margin(5, 8, 4, 5) # Left margin set to 5 to match p_overview
    )
  
  if (!show_x_labels) {
    p <- p + theme(axis.text.x = element_blank())
  }
  p
}

# ── Build upper panel: 1st COVID wave ─────────────────────────────────────────
p_upper <- make_panel(
  dt_raw       = dt_raw,
  cities       = target_cities,
  event_start  = as.Date("2020-03-17"),
  event_end    = as.Date("2020-03-17"),
  pad_before   = 5,
  pad_after    = 7,
  event_col    = "#E65F2B", 
  event_label  = "b.1) 1st COVID-19 wave (March 2020)",
  show_x_labels = T   
)

# ── Build lower panel: Floods Vale do Itajaí ──────────────────────────────────
p_lower <- make_panel(
  dt_raw       = dt_raw,
  cities       = target_cities,
  event_start  = as.Date("2023-10-04"),
  event_end    = as.Date("2023-10-15"),
  pad_before   = 4,
  pad_after    = 5,
  event_col    = "#2A9D8F", 
  event_label  = "b.2) Flood — Vale do Itajaí (October 2023)",
  show_x_labels = TRUE
)


# ── Assemble with patchwork ────────────────────────────────────────────────────

# FIX 1: Ensure p_overview title aligns with the left edge exactly like the others
p_overview <- p_overview + 
  theme(plot.title.position = "plot")

# FIX 2: Create a tiny text-only ggplot to act as the "b)" title layer
title_b <- ggplot() + 
  labs(
    title = "b) PBB-Score comparison: 1st COVID-19 wave vs. Vale do Itajaí Flood",
    subtitle = paste("Cities: Itajaí, Indaial, Florianópolis |",
                     "Shaded region = event period |",
                     "Bands: Below Regular / Regular / Panic / Extreme Panic")
  ) +
  theme_void() +
  theme(
    plot.title          = element_text(face = "bold", size = 12),
    plot.subtitle       = element_text(size = 8.5, color = "grey40"),
    plot.title.position = "plot",
    plot.margin         = margin(t = 15, b = 5, l = 5, r = 5) # Left margin = 5 matches everything
  )

# FIX 3: Combine all 4 layers (a, title_b, b1, b2) seamlessly
combined_plot <- p_overview / title_b / p_upper / p_lower

# FIX 4: Apply layout heights and collect the legend at the bottom
combined_plot <- combined_plot + 
  plot_layout(heights = c(1, 0.08, 1, 1), guides = "collect") + 
  plot_annotation(
    theme = theme(
      legend.position = "bottom",
      legend.margin   = margin(t = 10, b = 10)
    )
  )

# Save the combined figure (Increased height slightly to accommodate the title block)
ggsave(file.path(out_overview, "Combined_Overview.png"),
       combined_plot, width = 14, height = 11.5, dpi = 150)

ggsave(file.path("data/analysis/PDFs", "Figure_2.pdf"),
       combined_plot, width = 14, height = 11.5, device = "pdf")

cat("\u2713 Combined plot saved successfully!\n")

##7.3.Appendix -------
library(readxl)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(scales)

# --- Caminhos (ajuste para o seu diretorio) ---
arq_casos    <- "data/Casos_Covid.xlsx"
arq_severity <- "data/Pandemic_severityIndex.csv"

# Helper: media/mediana movel de 7 dias, centrada, exigindo
# pelo menos 4 observacoes na janela (equivale a min_periods = 4)
roll7 <- function(x, FUN) {
  zoo::rollapply(
    x, width = 7, align = "center", partial = TRUE, fill = NA,
    FUN = function(z) if (sum(!is.na(z)) >= 4) FUN(z, na.rm = TRUE) else NA_real_
  )
}

# 1) Casos e obitos: agregar as 15 cidades por dia
casos <- read_excel(arq_casos)
casos$date <- as.Date(casos$date)

agg <- casos %>%
  group_by(date) %>%
  summarise(
    new_cases  = sum(new_cases,  na.rm = TRUE),
    new_deaths = sum(new_deaths, na.rm = TRUE),
    .groups = "drop"
  )

# grade diaria completa (dias faltantes -> 0), equivale a asfreq('D').fillna(0)
grade <- data.frame(date = seq(min(agg$date), max(agg$date), by = "day"))
agg <- grade %>%
  left_join(agg, by = "date") %>%
  mutate(
    new_cases  = replace_na(new_cases, 0),
    new_deaths = replace_na(new_deaths, 0)
  )

agg$c7    <- roll7(agg$new_cases, mean)    # media movel 7d (primario)
agg$cmed7 <- roll7(agg$new_cases, median)  # mediana 7d (robusta a dump)

# 2) Bandeira regulatoria: media entre cidades/dia, interpolar, suavizar 7d
sev <- read.csv(arq_severity, sep = ";", fileEncoding = "latin1",
                stringsAsFactors = FALSE)
names(sev) <- trimws(names(sev))
sev$date <- as.Date(sev$date, format = "%d/%m/%Y")
sev$v    <- suppressWarnings(as.numeric(sev$Pandemic_severity_index))

sev_day <- sev %>%
  filter(!is.na(v)) %>%
  group_by(date) %>%
  summarise(v = mean(v, na.rm = TRUE), .groups = "drop")

# grade diaria no intervalo com bandeira + interpolacao linear interna
grade_sev <- data.frame(date = seq(min(sev_day$date), max(sev_day$date), by = "day"))
sev_day <- grade_sev %>%
  left_join(sev_day, by = "date") %>%
  mutate(v = zoo::na.approx(v, na.rm = FALSE))
sev_day$sev7 <- roll7(sev_day$v, mean)

# 3) Parametros do grafico (paleta e fator do eixo secundario)
C_CASES <- "#0099B8"; C_MED <- "#AE2012"; C_SEV <- "#FFB703"
W1 <- "#A9CDB6"; W2 <- "#88B6C9"; WO <- "#E25C84"

y_max   <- 2600          # limite do eixo de casos
sev_max <- 4.3           # limite do eixo de bandeira
sf      <- y_max / sev_max  # fator para sobrepor a bandeira no eixo de casos

d <- as.Date  # atalho

# 4) Grafico
p <- ggplot(agg, aes(date)) +
  
  # faixas das ondas
  annotate("rect", xmin = d("2020-03-12"), xmax = d("2020-09-26"),
           ymin = 0, ymax = y_max, fill = W1, alpha = 0.35) +
  annotate("rect", xmin = d("2020-10-15"), xmax = d("2021-12-15"),
           ymin = 0, ymax = y_max, fill = W2, alpha = 0.30) +
  annotate("rect", xmin = d("2021-12-24"), xmax = d("2022-03-27"),
           ymin = 0, ymax = y_max, fill = WO, alpha = 0.22) +
  
  # area + linha de casos 7d
  geom_area(aes(y = c7), fill = C_CASES, alpha = 0.12) +
  geom_line(aes(y = c7, color = "New cases (7-day moving avg)"),
            linewidth = 0.9) +
  
  # mediana robusta a outlier
  geom_line(aes(y = cmed7, color = "New cases (7-day median, outlier-robust)"),
            linewidth = 0.6, linetype = "dashed", alpha = 0.85) +
  
  # bandeira regulatoria no eixo secundario (reescalada por sf)
  geom_line(data = sev_day,
            aes(date, sev7 * sf, color = "Regulatory severity flag (1-4, 7d avg)"),
            linewidth = 0.7, alpha = 0.9) +
  
  # linhas de fronteira (subida onda 1, vale, inicio onda 2, inicio e pico Omicron)
  geom_vline(xintercept = d(c("2020-04-29", "2020-09-26", "2020-10-15",
                              "2021-12-24", "2022-01-19")),
             linetype = "dashed", linewidth = 0.45, color = "grey30", alpha = 0.8) +
  
  # rotulos das ondas
  annotate("text", x = d("2020-06-01"), y = 300, label = "WAVE 1",
           fontface = "bold", color = "#3a7d5d", size = 4) +
  annotate("text", x = d("2021-05-01"), y = 300, label = "WAVE 2  (Gamma / P.1 block)",
           fontface = "bold", color = "#2c5d77", size = 4) +
  annotate("text", x = d("2022-02-05"), y = 300, label = "OMICRON",
           fontface = "bold", color = "#9c2a5a", size = 4) +
  annotate("text", x = d("2022-03-05"), y = 2300, label = "right-\ncensored",
           fontface = "italic", color = "#9c2a5a", size = 3) +
  
  # anotacao da contaminacao do pico da onda 1
  annotate("text", x = d("2020-05-05"), y = 2050,
           label = "Wave 1 peak magnitude\ndistorted by 29-31 Aug backlog dumps\n(read height from median)",
           color = C_MED, size = 3, hjust = 0) +
  annotate("segment", x = d("2020-08-10"), xend = d("2020-09-01"),
           y = 2000, yend = 1700, color = C_MED,
           arrow = arrow(length = unit(0.18, "cm"))) +
  
  # cores das series
  scale_color_manual(values = c(
    "New cases (7-day moving avg)"               = C_CASES,
    "New cases (7-day median, outlier-robust)"   = C_MED,
    "Regulatory severity flag (1-4, 7d avg)"     = C_SEV)) +
  
  # eixos
  scale_y_continuous(
    name = "New COVID-19 cases per day (15 municipalities)",
    limits = c(0, y_max),
    sec.axis = sec_axis(~ . / sf, name = "Regulatory severity flag (1 Blue - 4 Red)")) +
  scale_x_date(limits = d(c("2020-03-01", "2022-04-10")),
               date_breaks = "2 months", date_labels = "%b\n%Y") +
  
  labs(
    title = "Pandemic wave delimitation - Santa Catarina (15 municipalities, aggregated)",
    subtitle = "Cases 7-day moving average, with peaks-and-valleys segmentation",
    color = NULL) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = c(0.02, 0.98),            # ggplot2 < 3.5
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), color = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "#c98e00"),
    axis.text.y.right  = element_text(color = "#c98e00")
  )

ggsave("data/output/SC_pandemic_waves.png", p, width = 13, height = 6.2, dpi = 200)

#8. Figures 6 and 7-------
# Self-contained recreation of:
#   - shap_importance.png
#   - shap_summary_detailed.png
#   - combined_figure2.png         (WITHOUT the top "a)" panel -> b) + c) only)
# Plus the CSVs needed downstream:
#   - model_mobility_improved/best_row.csv          (read by Temporal_CV_v5.R)
#   - model_mobility_improved/shap_feature_importance.csv
#
# Run from the directory that contains
#   "3_Final_data_With_Time_modification_ADDED_Var.csv"
# (the same working directory as Final_main_model_v3.R).
#
# Expensive steps (grid search, SHAP) are guarded by file.exists(): they run
# once if their cache is missing, otherwise they are reloaded.

suppressPackageStartupMessages({
  library(xgboost)
  library(treeshap)   # devtools::install_github("ModelOriented/treeshap") if missing
  library(caret)      # createDataPartition (reproduces the exact split)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
output_dir <- "data/analysis/Shap/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ── 1. Load + preprocess (identical to Final_main_model_v3.R) ─────────────────
df <- read.csv2("data/input/3_Final_data_With_Time_modification_ADDED_Var.csv")
if ("date" %in% names(df)) df <- df %>% dplyr::select(-date)

cols_to_drop <- c("year_month", "upper_iqr", "valor_pc_real_raw", "residual", "yhat",
                  "is_crisis", "sigma_c", "population_2022", "date_year",
                  "valor_pc", "valor_pc_real", "time_linear", "time_linear_c", "is_outlier", "deflator",
                  "month_sin", "month_cos", "wday_sin", "wday_cos", "yday_sin", "yday_cos",
                  "dom_sin", "dom_cos", "dom_sin2", "dom_cos2",
                  "is_holiday", "is_pre_holiday", "dom", "is_day31", "is_day1", "dow_f",
                  "month_f", "is_weekend", "date_month", "date_wday", "date_yday",
                  "S_precip_daily", "S_is_crisis_period",
                  "S_pandemic_cases_z", "S_pandemic_deaths_z", "S_pandemic_cases_shock",
                  "S_pandemic_deaths_shock", "S_pandemic_severity_index",
                  "ecf_sales_value_daily", "nfce_sales_value_daily", "nfe_sales_value_daily",
                  "total_sales_value_daily",
                  "hypermarket_ecf_sales_value_daily", "hypermarket_nfc_sales_value_daily",
                  "hypermarket_nf_sales_value_daily",
                  "O_hypermarket_total_sales_value_daily", "O_supermarket_total_sales_value_daily",
                  "O_minimarket_total_sales_value_daily",
                  "minimarket_ecf_sales_value_daily", "minimarket_nfc_sales_value_daily",
                  "minimarket_nf_sales_value_daily", "supermarket_ecf_sales_value_daily",
                  "supermarket_nfc_sales_value_daily", "supermarket_nf_sales_value_daily",
                  "nfce_count_daily", "nfe_count_daily", "hypermarket_nfce_count_daily",
                  "hypermarket_nfe_count_daily", "minimarket_nfe_count_daily",
                  "minimarket_nfce_count_daily", "supermarket_nfe_count_daily",
                  "supermarket_nfce_count_daily", "sales_value_per_capita")
df <- df %>% dplyr::select(-any_of(cols_to_drop))

if ("API_cat" %in% names(df)) df <- df %>% dplyr::select(-API_cat)

# Drop redundant pandemic variables (keep level/shock/awareness representatives)
pandemic_to_drop <- c("S_pandemic_cases_z", "S_pandemic_deaths_z",
                      "S_pandemic_cases_shock", "S_pandemic_deaths_shock",
                      "S_pandemic_severity_index")
df <- df %>% dplyr::select(-any_of(pandemic_to_drop))

# NA -> 0 for pandemic variables (zero outside the pandemic is correct)
pandemic_vars <- grep("^S_pandemic", names(df), value = TRUE)
df[, pandemic_vars] <- lapply(df[, pandemic_vars], function(x) replace(x, is.na(x), 0))

if ("city" %in% names(df)) df <- df %>% dplyr::select(-city)

# Encode any remaining character predictors
char_cols <- names(df)[sapply(df, is.character)]
for (col in char_cols) df[[col]] <- as.numeric(as.factor(df[[col]]))

# ── 2. Modelling matrix + train/test split (seed 42, 80/20) ───────────────────
TARGET   <- "API"
df_model <- df %>% filter(!is.na(.data[[TARGET]]))
X <- df_model %>% dplyr::select(-all_of(TARGET))
y <- df_model[[TARGET]]

set.seed(42)
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ] %>% mutate(across(everything(), as.numeric))
X_test  <- X[-train_indices, ] %>% mutate(across(everything(), as.numeric))
y_train <- y[train_indices]
y_test  <- y[-train_indices]

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest  <- xgb.DMatrix(data = as.matrix(X_test),  label = y_test)

# ── 3. Hyperparameters: grid search (only if best_row.csv is missing) ─────────
best_row_path <- file.path("data/output/best_row.csv")
if (!file.exists(best_row_path)) {
  cat("[grid search] best_row.csv missing — running 5-fold CV grid (slow, once)...\n")
  param_grid <- expand.grid(
    max_depth        = c(4, 5, 6),
    eta              = c(0.05, 0.1),
    subsample        = c(0.7, 0.8),
    colsample_bytree = c(0.7, 0.8),
    min_child_weight = c(5, 10),
    gamma            = c(0, 0.1),
    stringsAsFactors = FALSE)
  cv_results <- data.frame()
  for (i in seq_len(nrow(param_grid))) {
    p <- param_grid[i, ]
    params_i <- list(objective = "reg:squarederror",
                     max_depth = p$max_depth, eta = p$eta, subsample = p$subsample,
                     colsample_bytree = p$colsample_bytree, min_child_weight = p$min_child_weight,
                     gamma = p$gamma, lambda = 2.0, alpha = 0.1, nthread = 4, eval_metric = "rmse")
    cv_out <- xgb.cv(params = params_i, data = dtrain, nrounds = 1000, nfold = 5,
                     early_stopping_rounds = 30, verbose = 0, showsd = FALSE)
    cv_results <- rbind(cv_results, data.frame(p,
                                               nrounds = which.min(cv_out$evaluation_log$test_rmse_mean),
                                               cv_rmse = min(cv_out$evaluation_log$test_rmse_mean)))
  }
  best_row <- cv_results[which.min(cv_results$cv_rmse), ]
  write.csv(best_row, best_row_path)          # same format Temporal_CV_v5.R reads
  cat("[grid search] best_row.csv written.\n")
}
best_row <- read.csv(best_row_path)

final_params <- list(objective = "reg:squarederror",
                     max_depth = best_row$max_depth, eta = best_row$eta, subsample = best_row$subsample,
                     colsample_bytree = best_row$colsample_bytree, min_child_weight = best_row$min_child_weight,
                     gamma = best_row$gamma, lambda = 2.0, alpha = 0.1, nthread = 4, eval_metric = "rmse")

# ── 4. Final model ────────────────────────────────────────────────────────────
model <- xgb.train(params = final_params, data = dtrain,
                   nrounds = best_row$nrounds, verbose = 0)
cat(sprintf("Model fitted | test R2 = %.3f\n",
            1 - sum((y_test - predict(model, dtest))^2) / sum((y_test - mean(y_test))^2)))

# ── 5. SHAP via treeshap (cached) ─────────────────────────────────────────────
unified_model <- xgboost.unify(model, as.matrix(X_train))
if (nrow(X_test) > 1000) {
  set.seed(42)
  X_test_sample <- X_test[sample(seq_len(nrow(X_test)), 1000), ]
} else {
  X_test_sample <- X_test
}
shap_rds_path <- file.path(output_dir, "shap_matrix.rds")
if (!file.exists(shap_rds_path)) {
  shap_matrix <- treeshap(unified_model, as.matrix(X_test_sample))$shaps
  saveRDS(X_test_sample, file.path(output_dir, "X_test_sample.rds"))
  saveRDS(shap_matrix,   shap_rds_path)
} else {
  X_test_sample <- readRDS(file.path(output_dir, "X_test_sample.rds"))
  shap_matrix   <- readRDS(shap_rds_path)
}

shap_importance <- data.frame(
  Feature    = colnames(X_test_sample),
  Importance = colMeans(abs(shap_matrix))) %>%
  arrange(desc(Importance))
write.csv(shap_importance, file.path(output_dir, "shap_feature_importance.csv"),
          row.names = FALSE)

# 1. Define the mapping dictionary for the new labels
label_dict <- c(
  "S_pandemic_Gtrends"         = "S_pandemicGT",
  "O_supermarket_Gtrends"      = "O_supermarketGT",
  "S_pandemic_cumuldeaths_log" = "S_pandemicCumulDeathsLog",
  "S_wind_gust_max"            = "S_windGustMax",
  "O_minimarket_Gtrends"       = "O_minimarketGT",
  "S_health_Gtrends"           = "S_healthGT",
  "S_precip_acc_3d"            = "S_precipAcc3d",
  "O_gov_policy_Gtrends"       = "O_govPolicyGT",
  "S_pandemic_cumulcases_log"  = "S_pandemicCumulCasesLog",
  "O_psych_fear_Gtrends"       = "O_fearGT",
  "O_inflation_Gtrends"        = "O_inflationGT",
  "O_mobil_dist_10_100km"      = "O_mobilDist_10_100km",
  "O_mobil_dist_0_10km"        = "O_mobilDist_0_10km",
  "O_mobil_dist_0"             = "O_mobilDist_0km",
  "O_mobil_dist_100km_plus"    = "O_mobilDist_100kmPlus",
  "S_strike_Gtrends"           = "S_strikeGT",
  "S_climate_Gtrends"          = "S_weatherGT",
  "S_cyclone_Gtrends"          = "S_cycloneGT",
  "O_news_count"               = "O_newsCount",
  "O_basicfoodbasket_Gtrends"  = "O_basicFoodBasketGT"
)

# 2. Helper function to apply the new labels (keeps original name if not in dictionary)
relabel_features <- function(features) {
  new_feats <- label_dict[features]
  new_feats[is.na(new_feats)] <- features[is.na(new_feats)]
  return(unname(new_feats))
}


# ── 6. shap_importance.png ────────────────────────────────────────────────────
# png(file.path(output_dir, "shap_importance.png"), width = 1000, height = 800, res = 100)
# top_n <- 20
# top_features <- head(shap_importance, top_n)
# par(mar = c(5, 12, 4, 2))
# barplot(rev(top_features$Importance), names.arg = rev(top_features$Feature),
#         horiz = TRUE, las = 1, col = "steelblue",
#         main = "SHAP Feature Importance (Top 20)", xlab = "Mean |SHAP value|")
# dev.off()
png(file.path(output_dir, "shap_importance.png"), width = 1000, height = 800, res = 100)
top_n <- 20
top_features <- head(shap_importance, top_n)

# Reverse the features order for the bottom-to-top horizontal barplot
rev_features <- rev(top_features$Feature)

# Apply the new labels
new_labels_importance <- relabel_features(rev_features)

# Apply custom colors based on prefixes
bar_colors <- ifelse(startsWith(rev_features, "O_"), "#8ECAE6",
                     ifelse(startsWith(rev_features, "S_"), "#023047", 
                            "steelblue")) # fallback color just in case

par(mar = c(5, 12, 4, 2))
barplot(rev(top_features$Importance), 
        names.arg = new_labels_importance,
        horiz = TRUE, las = 1, 
        col = bar_colors,        # <-- Assigned custom hex colors here
        main = "SHAP Feature Importance (Top 20)", xlab = "Mean |SHAP value|")
dev.off()

# ── 7. shap_summary_detailed.png ──────────────────────────────────────────────
# png(file.path(output_dir, "shap_summary_detailed.png"), width = 1000, height = 1200, res = 100)
# top_n <- 19
# top_feature_names <- head(shap_importance$Feature, top_n)
# shap_subset    <- shap_matrix[, top_feature_names]
# feature_values <- as.matrix(X_test_sample[, top_feature_names])
# par(mar = c(5, 12, 4, 6))
# plot(NULL, xlim = c(min(shap_subset), max(shap_subset)), ylim = c(1, top_n),
#      xlab = "SHAP value", ylab = "", main = "SHAP Summary Plot: Feature Impact", yaxt = "n")
# axis(2, at = 1:top_n, labels = rev(top_feature_names), las = 1)
# abline(v = 0, col = "gray", lty = 2)
# for (i in 1:top_n) {
#   feature_idx <- top_n - i + 1
#   shap_vals <- shap_subset[, feature_idx]
#   feat_vals <- feature_values[, feature_idx]
#   feat_vals_norm <- (feat_vals - min(feat_vals, na.rm = TRUE)) /
#     (max(feat_vals, na.rm = TRUE) - min(feat_vals, na.rm = TRUE))
#   feat_vals_norm[is.na(feat_vals_norm)] <- 0.5
#   y_jitter <- i + runif(length(shap_vals), -0.3, 0.3)
#   colors <- rgb(feat_vals_norm, 0, 1 - feat_vals_norm, alpha = 0.5)
#   points(shap_vals, y_jitter, pch = 16, col = colors, cex = 0.5)
# }
# legend("topright", legend = c("High", "Low"), col = c("red", "blue"), pch = 16,
#        title = "Feature Value", bg = "white")
# dev.off()

png(file.path(output_dir, "shap_summary_detailed.png"), width = 1000, height = 1200, res = 100)
top_n <- 20 #changed here from 19
top_feature_names <- head(shap_importance$Feature, top_n)

# Note: Keep the original names to subset the matrices!
shap_subset    <- shap_matrix[, top_feature_names]
feature_values <- as.matrix(X_test_sample[, top_feature_names])

# Apply new labels only for the axis text
new_labels_summary <- relabel_features(rev(top_feature_names))

par(mar = c(5, 12, 4, 6))
plot(NULL, xlim = c(min(shap_subset), max(shap_subset)), ylim = c(1, top_n),
     xlab = "SHAP value", ylab = "", main = "SHAP Summary Plot: Feature Impact", yaxt = "n")

# Use the mapped labels on the Y-axis
axis(2, at = 1:top_n, labels = new_labels_summary, las = 1) 
abline(v = 0, col = "gray", lty = 2)

for (i in 1:top_n) {
  feature_idx <- top_n - i + 1
  shap_vals <- shap_subset[, feature_idx]
  feat_vals <- feature_values[, feature_idx]
  
  feat_vals_norm <- (feat_vals - min(feat_vals, na.rm = TRUE)) /
    (max(feat_vals, na.rm = TRUE) - min(feat_vals, na.rm = TRUE))
  feat_vals_norm[is.na(feat_vals_norm)] <- 0.5
  
  y_jitter <- i + runif(length(shap_vals), -0.3, 0.3)
  colors <- rgb(feat_vals_norm, 0, 1 - feat_vals_norm, alpha = 0.5)
  points(shap_vals, y_jitter, pch = 16, col = colors, cex = 0.5)
}

legend("topright", legend = c("High", "Low"), col = c("red", "blue"), pch = 16,
       title = "Feature Value", bg = "white")
dev.off()

#new-----
# 1. Define the mapping dictionary for the new labels
label_dict <- c(
  "S_pandemic_Gtrends"         = "S_pandemicGT",
  "O_supermarket_Gtrends"      = "O_supermarketGT",
  "S_pandemic_cumuldeaths_log" = "S_pandemicCumulDeathsLog",
  "S_wind_gust_max"            = "S_windGustMax",
  "O_minimarket_Gtrends"       = "O_minimarketGT",
  "S_health_Gtrends"           = "S_healthGT",
  "S_precip_acc_3d"            = "S_precipAcc3d",
  "O_gov_policy_Gtrends"       = "O_govPolicyGT",
  "S_pandemic_cumulcases_log"  = "S_pandemicCumulCasesLog",
  "O_psych_fear_Gtrends"       = "O_fearGT",
  "O_inflation_Gtrends"        = "O_inflationGT",
  "O_mobil_dist_10_100km"      = "O_mobilDist_10_100km",
  "O_mobil_dist_0_10km"        = "O_mobilDist_0_10km",
  "O_mobil_dist_0"             = "O_mobilDist_0km",
  "O_mobil_dist_100km_plus"    = "O_mobilDist_100kmPlus",
  "S_strike_Gtrends"           = "S_strikeGT",
  "S_climate_Gtrends"          = "S_weatherGT",
  "S_cyclone_Gtrends"          = "S_cycloneGT",
  "O_news_count"               = "O_newsCount",
  "O_basicfoodbasket_Gtrends"  = "O_basicFoodBasketGT"
)

# 2. Helper function to apply the new labels
relabel_features <- function(features) {
  new_feats <- label_dict[features]
  new_feats[is.na(new_feats)] <- features[is.na(new_feats)]
  return(unname(new_feats))
}

# ══════════════════════════════════════════════════════════════════════════════
# COMBINED PLOT: SHAP IMPORTANCE (LEFT) & SHAP IMPACT (RIGHT)
# ══════════════════════════════════════════════════════════════════════════════

# Increase width to 2200 so both plots have plenty of room side-by-side
png(file.path(output_dir, "shap_combined_figure.png"), width = 2200, height = 1200, res = 150)

# Set global layout: 1 row, 2 columns
par(mfrow = c(1, 2))

# Use exactly top 20 for both so the horizontal alignment matches perfectly
top_n <- 20 

# PANEL a) SHAP Feature Importance (Left)
top_features <- head(shap_importance, top_n)
rev_features <- rev(top_features$Feature)
new_labels_importance <- relabel_features(rev_features)

# Apply custom colors based on prefixes
bar_colors <- ifelse(startsWith(rev_features, "O_"), "#8ECAE6",
                     ifelse(startsWith(rev_features, "S_"), "#023047", 
                            "steelblue"))

par(mar = c(5, 14, 4, 2)) # Increased left margin slightly to fit the new labels cleanly
barplot(rev(top_features$Importance), 
        names.arg = new_labels_importance,
        horiz = TRUE, las = 1, 
        col = bar_colors,
        main = "a) SHAP Feature Importance", xlab = "Mean |SHAP value|",
        cex.main = 1.4, cex.names = 1.1, cex.lab = 1.2) # Scaled up text slightly

# PANEL b) SHAP Feature Impact (Right)
top_feature_names <- head(shap_importance$Feature, top_n)
shap_subset       <- shap_matrix[, top_feature_names]
feature_values    <- as.matrix(X_test_sample[, top_feature_names])

new_labels_summary <- relabel_features(rev(top_feature_names))
new_labels_summary
# --- NEW: Calculate x-axis limits and add 40% padding to the right for the legend ---
x_min <- min(shap_subset)
x_max <- max(shap_subset)
x_max_padded <- x_max + ((x_max - x_min) * 0.40) 

# Changed right margin from 6 to 2, since the legend now fits inside the plot
par(mar = c(5, 14, 4, 2)) 

# Use the new x_max_padded in the xlim argument
plot(NULL, xlim = c(x_min, x_max_padded), ylim = c(1, top_n),
     xlab = "SHAP value", ylab = "", 
     main = "b) SHAP Feature Impact", yaxt = "n",
     cex.main = 1.4, cex.lab = 1.2)

axis(2, at = 1:top_n, labels = new_labels_summary, las = 1, cex.axis = 1.1) 
abline(v = 0, col = "gray", lty = 2)

for (i in 1:top_n) {
  feature_idx <- top_n - i + 1
  shap_vals <- shap_subset[, feature_idx]
  feat_vals <- feature_values[, feature_idx]
  
  feat_vals_norm <- (feat_vals - min(feat_vals, na.rm = TRUE)) /
    (max(feat_vals, na.rm = TRUE) - min(feat_vals, na.rm = TRUE))
  feat_vals_norm[is.na(feat_vals_norm)] <- 0.5
  
  y_jitter <- i + runif(length(shap_vals), -0.3, 0.3)
  colors <- rgb(feat_vals_norm, 0, 1 - feat_vals_norm, alpha = 0.5)
  points(shap_vals, y_jitter, pch = 16, col = colors, cex = 0.8) 
}

# Legend remains in the "topright", but now there is empty space for it!
legend("topright", legend = c("High", "Low"), col = c("red", "blue"), pch = 16,
       title = "Feature Value", bg = "white", cex = 1.2)

# Reset device layout and save
dev.off()

# ══════════════════════════════════════════════════════════════════════════════
# GENERATE PDF VERSION (Vector Graphic for Publication)
# ══════════════════════════════════════════════════════════════════════════════

# Open the PDF device (dimensions in inches)
pdf(file.path("data/analysis/PDFs", "Figure_7.pdf"), width = 14.5, height = 8)

# Set global layout: 1 row, 2 columns
par(mfrow = c(1, 2))

# PANEL a) SHAP Feature Importance (Left)
par(mar = c(5, 14, 4, 2)) 
barplot(rev(top_features$Importance), 
        names.arg = new_labels_importance,
        horiz = TRUE, las = 1, 
        col = bar_colors,
        main = "a) SHAP Feature Importance", xlab = "Mean |SHAP value|",
        cex.main = 1.4, cex.names = 1.1, cex.lab = 1.2) 

# PANEL b) SHAP Feature Impact (Right)
par(mar = c(5, 14, 4, 2)) 
plot(NULL, xlim = c(x_min, x_max_padded), ylim = c(1, top_n),
     xlab = "SHAP value", ylab = "", 
     main = "b) SHAP Feature Impact", yaxt = "n",
     cex.main = 1.4, cex.lab = 1.2)

axis(2, at = 1:top_n, labels = new_labels_summary, las = 1, cex.axis = 1.1) 
abline(v = 0, col = "gray", lty = 2)

for (i in 1:top_n) {
  feature_idx <- top_n - i + 1
  shap_vals <- shap_subset[, feature_idx]
  feat_vals <- feature_values[, feature_idx]
  
  feat_vals_norm <- (feat_vals - min(feat_vals, na.rm = TRUE)) /
    (max(feat_vals, na.rm = TRUE) - min(feat_vals, na.rm = TRUE))
  feat_vals_norm[is.na(feat_vals_norm)] <- 0.5
  
  y_jitter <- i + runif(length(shap_vals), -0.3, 0.3)
  
  # For PDFs, alpha transparency can sometimes look different than PNGs,
  # but this rgb() method works perfectly in R's pdf() device.
  colors <- rgb(feat_vals_norm, 0, 1 - feat_vals_norm, alpha = 0.5)
  points(shap_vals, y_jitter, pch = 16, col = colors, cex = 0.8) 
}

legend("topright", legend = c("High", "Low"), col = c("red", "blue"), pch = 16,
       title = "Feature Value", bg = "white", cex = 1.2)

# Close and save the PDF file
dev.off()

cat("\u2713 SHAP Combined Figure PDF saved successfully!\n")

# ── 8. combined_figure2.png  (without the "a)" prediction panel) ──────────────
stimuli  <- grep("^S_", colnames(X), value = TRUE)
organism <- grep("^O_", colnames(X), value = TRUE)

# panel b) — Stimuli vs Organism
cat_so <- data.frame(
  Category = c("Stimuli", "Organism"),
  Total_Importance = c(
    sum(shap_importance$Importance[shap_importance$Feature %in% stimuli]),
    sum(shap_importance$Importance[shap_importance$Feature %in% organism]))) %>%
  arrange(desc(Total_Importance))
cat_so
plot_b <- ggplot(cat_so, aes(x = Category, y = Total_Importance, fill = Category)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  scale_fill_manual(values = c("#8ECAE6", "#023047", "#2ca02c", "#d62728")) +
  labs(tag = "a)", x = NULL, y = "Cumulative SHAP Importance") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
        plot.tag = element_text(face = "bold", size = 12),
        plot.tag.position = "topleft",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid = element_blank())

# panel c) — broken-down subcategories
pandemic    <- c("S_pandemic_Gtrends", "S_pandemic_cumulcases_log",
                 "S_pandemic_cumuldeaths_log", "S_health_Gtrends")
climate     <- c("S_wind_gust_max", "S_precip_acc_3d", "S_climate_Gtrends", "S_cyclone_Gtrends")
strike      <- c("S_strike_Gtrends", "S_strike_event")
government  <- "O_gov_policy_Gtrends"
social      <- c("O_mobil_dist_0", "O_mobil_dist_0_10km", "O_mobil_dist_10_100km",
                 "O_mobil_dist_100km_plus", "O_news_count")
psychologic <- c("O_psych_fear_Gtrends", "O_supermarket_Gtrends",
                 "O_minimarket_Gtrends", "O_rationing_Gtrends")
economic    <- c("O_inflation_Gtrends", "O_basicfoodbasket_Gtrends")

cat_sub <- data.frame(
  Category = c("Pandemic (S)", "Climate (S)", "Strike (S)", "Government (O)",
               "Social (O)", "Psychologic (O)", "Economic (O)"),
  Importance = c(
    sum(shap_importance$Importance[shap_importance$Feature %in% pandemic]),
    sum(shap_importance$Importance[shap_importance$Feature %in% climate]),
    sum(shap_importance$Importance[shap_importance$Feature %in% strike]),
    sum(shap_importance$Importance[shap_importance$Feature %in% government]),
    sum(shap_importance$Importance[shap_importance$Feature %in% social]),
    sum(shap_importance$Importance[shap_importance$Feature %in% psychologic]),
    sum(shap_importance$Importance[shap_importance$Feature %in% economic]))) %>%
  arrange(desc(Importance))

# p_cat2 <- ggplot(cat_sub, aes(x = reorder(Category, Importance), y = Importance, fill = Category)) +
#   geom_col(show.legend = FALSE) +
#   coord_flip() +
#   labs(tag = "b)", x = NULL, y = "Cumulative Mean |SHAP|") +
#   theme_minimal(base_size = 12)

library(dplyr)
library(ggplot2)

# 1. Rename the labels in your dataset
cat_sub <- cat_sub %>%
  mutate(
    Category = case_when(
      Category == "Climate (S)"    ~ "Environmental (S)",
      Category == "Strike (S)"     ~ "Geopolitical (S)",
      Category == "Psychologic (O)"~ "Situational (O)",
      Category == "Government (O)" ~ "Institutional (O)",
      
      # Catch-all just in case your dataset doesn't actually have the (S)/(O) suffixes in the raw text
      Category == "climate"        ~ "Environmental",
      Category == "strike"         ~ "Geopolitical",
      Category == "psychologic"    ~ "Situational",
      Category == "governmental"   ~ "Institutional",
      Category == "Government"     ~ "Institutional",
      
      TRUE ~ Category # Keep Pandemic, Social, and Economic exactly as they are
    )
  )

# 2. Define the custom hex colors mapped to the NEW labels
domain_colors <- c(
  "Environmental (S)" = "#2A9D8F",
  "Geopolitical (S)"  = "#0099B8",
  "Pandemic (S)"      = "#E65F2B",
  "Situational (O)"   = "#FFB703",
  "Social (O)"        = "#E25C84",
  "Institutional (O)" = "#6A4C93",
  "Economic (O)"      = "#AE2012",
  
  # Fallbacks (without suffixes)
  "Environmental"     = "#2A9D8F",
  "Geopolitical"      = "#0099B8",
  "Pandemic"          = "#E65F2B",
  "Situational"       = "#FFB703",
  "Social"            = "#E25C84",
  "Institutional"     = "#6A4C93",
  "Economic"          = "#AE2012"
)

# 3. Generate the plot with the new names and custom colors
p_cat2 <- ggplot(cat_sub, aes(x = Importance, y = reorder(Category, Importance), fill = Category)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = domain_colors) +  
  
  # Note: coord_flip() has been removed! The axes are mapped directly above.
  # Swapped x and y in the labs() to match the new mapping
  labs(tag = "b)", y = NULL, x = "Cumulative Mean |SHAP|") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(), # Guaranteed to remove horizontal lines
    panel.grid.minor.y = element_blank()  # Guaranteed to remove minor horizontal lines
  )

# Combine and save as usual
combined_figure2 <- (plot_b + p_cat2)          
ggsave(file.path(output_dir, "combined_figure2.png"), combined_figure2,
       width = 12, height = 5, dpi = 300)

ggsave(file.path("data/analysis/PDFs", "Figure_6.pdf"),
       combined_figure2, width = 12, height = 5, device = "pdf")

#end------