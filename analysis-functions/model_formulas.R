spline_function <- function(v){
  paste0("ns(",v,", knots = quantile(",v,", probs=c(0.5)), Boundary.knots = quantile(",v,", probs=c(0.1, 0.9)))")
}

# formulas
formulas <- list()
formulas$t.splines2 <- "ns(tstart.new, knots=quantile(tstart.new, probs=c(0.5)), Boundary.knots=quantile(tstart.new, probs=c(0.1, 0.9)))"

#final model diabetics
formulas$final_model_diabetics =  paste0("female + afam + ",
                        spline_function("age_bl"), " + ",
                        spline_function("calendar.time_bl"), " + ",
                        "dyslipidemia_y_lag12 + hypertension_y_lag12 + osteoarthritis_y_lag12 + sleepapnea_y_lag12 + ",
                        "dyslipidemia_y_bl + hypertension_y_bl + osteoarthritis_y_bl + sleepapnea_y_bl +",
                        "as.factor(smoking_status)", " + ", 
                        spline_function("value_bmi_change_bl"), " + ",
                        spline_function("value_bmi_bl"), " + ",
                        spline_function("value_a1c_change_bl"), " + ",
                        spline_function("value_a1c_bl"), " + ",
                        "pastyear_med_oralhypoglycemic_bl + pastyear_med_insulin_bl")


#final model - matching
formulas$final_model_ps_matching = paste0("female + afam + 
ns(age_bl, knots = quantile(age_bl, probs=c(0.5)), Boundary.knots = quantile(age_bl, probs=c(0.1, 0.9))) + 
ns(calendar.time_bl, knots = quantile(calendar.time_bl, probs=c(0.5)), Boundary.knots = quantile(calendar.time_bl, probs=c(0.1, 0.9))) + 
dyslipidemia_y_bl + hypertension_y_bl + osteoarthritis_y_bl + sleepapnea_y_bl + 
ns(value_bmi_bl, knots = quantile(value_bmi_bl, probs=c(0.5)), Boundary.knots = quantile(value_bmi_bl, probs=c(0.1, 0.9))) + 
ns(value_a1c_bl, knots = quantile(value_a1c_bl, probs=c(0.5)), Boundary.knots = quantile(value_a1c_bl, probs=c(0.1, 0.9))) + 
pastyear_med_oralhypoglycemic_bl + pastyear_med_insulin_bl") # used with missing smoking status / propensity score
formulas$exact_matching = as.formula("~ female + value_bmi_bl_cat + value_a1c_bl_cat + age_bl_cat + year")

