# single-interval restriction
single_interval_restriction_function <- function(.dat,
                                                 .prefix=prefix, nosmoking, ipw.formula_d,
                                                 ...){
  
  if(nosmoking){
    # add smoking status
    smoking <- data.table::fread(file=paste0(.prefix, "/covariate-objects/smoking-grid.csv"))
    .dat <- smoking[.dat, on=.(scrssn, date_baseline)]
    
    remove_smoking_ids <- unique(c(.dat[is.na(smoking_status)]$id.new, .dat[smoking_status==2]$id.new)) # remove current smokers and missing smoking status
    .dat <- .dat[!(id.new %in% remove_smoking_ids)]
  }
  
  if(!nosmoking & stringr::str_detect(ipw.formula_d, "smoking_status")){
    # add smoking status
    smoking <- data.table::fread(file=paste0(.prefix, "/covariate-objects/smoking-grid.csv"))
    .dat <- smoking[.dat, on=.(scrssn, date_baseline)]
    
    remove_smoking_ids <- unique(.dat[is.na(smoking_status)]$id.new) # remove missing smoking status
    .dat <- .dat[!(id.new %in% remove_smoking_ids)]
  }
  
  return(.dat)
}
