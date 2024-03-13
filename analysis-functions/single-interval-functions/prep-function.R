data_prep_function <- function(dat, truncate_years){
  dat[
    , months := tstart.new / 30][
      , event := ifelse(event_y==1 & months > truncate_years*12, 0, event_y)][
        , tstart.new := ifelse(event==0 & tstart.new > truncate_years*12*30, truncate_years*12*30, tstart.new )
      ][
        , months := ifelse(event==0 & months > truncate_years*12, truncate_years*12, months )
      ]
  
  return(dat)
}


