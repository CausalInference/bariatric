if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(here, speedglm, tidyverse, reshape2, pbapply); prefix <- here()
source(here("clean-full-data_functions.R"))
source(here("analysis-functions", "model_formulas.R"))
source(here("analysis-functions", "single-interval-functions", "bootstrap-single-interval-main-function.R"))

single_interval_results <- bootstrap_single_interval_function(seed_vector=0:1000, number.processors=1,
                                   followup_years=7, censor_surgery=TRUE,
                                   stabilized=TRUE, trim=TRUE,
                                   ipw.formula_n = NULL, 
                                   ipw.formula_d =  paste0("y_surgery ~ ",
                                                           formulas$final_model_diabetics)
)
