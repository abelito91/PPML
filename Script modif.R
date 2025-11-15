# ============== Charger les packages nécessaires ================
library("readxl")
library("dplyr")
library("fixest")
library("modelsummary")
library("pandoc")

# ============== Charger les données ================
bilateral_data <- read_xlsx("bilateral_data.xlsx")
migration_data <- readRDS("migration_data.rds")

# ============== Joindre les bases de données ================
data_combined <- bilateral_data %>%
  inner_join(migration_data, by = c("iso3_o", "iso3_d", "year"))

# ============== Remplacer les valeurs manquantes par la médiane ================
data_combined <- data_combined %>%
  mutate(
    gdp_ppp_o = ifelse(is.na(gdp_ppp_o), median(gdp_ppp_o, na.rm = TRUE), gdp_ppp_o),
    unem_o = ifelse(is.na(unem_d), median(unem_d, na.rm = TRUE), unem_d),
    life_expectancy_o = ifelse(is.na(life_expectancy_o), median(life_expectancy_o, na.rm = TRUE), life_expectancy_o),
    homicide_rate_o = ifelse(is.na(homicide_rate_o), median(homicide_rate_o, na.rm = TRUE), homicide_rate_o),
    corruption_index_o = ifelse(is.na(corruption_index_o), median(corruption_index_o, na.rm = TRUE), corruption_index_o)
  )

# ============== Créer les variables nécessaires ================

data_combined <- data_combined %>%
  mutate(
    gdp_pc_o = gdp_ppp_o / pop_o,
    log_gdppc_o = log(gdp_pc_o)
  )

# ============== Régression PPML avec mig_rate comme variable dépendante ================
reg_PPML <- glm(
  formula = mig_rate ~ log_gdppc_o + unem_o + life_expectancy_o + homicide_rate_o + corruption_index_o,
  data = data_combined,
  family = quasipoisson(link = "log")
)

# ============== Résumé des résultats ================
summary(reg_PPML)

# ============== Exportation des résultats vers Word ================
modelsummary(
  reg_PPML,
  output = "regression_mig_rate_ppml.docx",
  fmt = 4,
  estimate = "{estimate}{stars}"
)
