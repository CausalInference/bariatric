if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(here, speedglm); prefix <- here()
source(here("clean-full-data_functions.R"))
source(here("analysis-functions", "model_formulas.R"))
source(here("analysis-functions", "gp-functions", "bootstrap_gp_wrapper.R"))


gp_results <- bootstrap_gp_wrapper_function(bootstrap=TRUE, seed_vector=0:1000, name_dat="diabetics-cv", number.processors=1,
                                            grace.intervals=12, min.interval=6, trim=TRUE, 
                                            censor_surgery=TRUE, followup_years=7,
                                            ipw.formula_d = paste0("y_surgery ~ ",  formulas$final_model_diabetics)
)
