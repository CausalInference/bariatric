full_data_preamble <- function(){
  # clean_data_combo_function <- function(startdate="01-01-2005"){
  lapply(c("tidyverse", "splines", "reshape2", "lubridate", "speedglm", "data.table"),
         function(x) suppressPackageStartupMessages(library(x, character.only=TRUE, quietly=TRUE)))
  visitdate <<- as.Date("01-01-2005", format="%m-%d-%Y")
}



