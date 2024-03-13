# plot-function
tt_plot_function <- function(resultdat, filename=NULL, followup_years=8, yname="Cumulative incidence (%)", title=NULL, ymax=0.5){
  plotdat <- resultdat %>% mutate(value=1-surv) %>% 
    filter(t<=followup_years*12*365) %>% mutate(t = t/30)
  p <- ggplot(data=plotdat,
              mapping=aes(x=t, y=value, group=group)) +
    geom_line(size=1, aes(color=group, linetype=group)) +
    scale_y_continuous(limits = c(0, ymax),
                       name = yname,
                       labels = scales::percent_format(accuracy=1)) +
    scale_x_continuous(breaks = seq(0, 12*followup_years, by=6),
                       limits = c(0, followup_years*12+2),
                       name = "Month") +
    theme_classic(base_size=22) +
    theme(legend.justification = c(0,0), legend.position = c(0.05, 0.8),
          legend.title=element_blank())
  
  if (!is.null(title)){
    p <- p + ggtitle(title)
  }
  
  if (!is.null(filename)){
    ggsave(p, width=14, height=7, filename=paste0(prefix, "/output/figures/",filename,".pdf"))
  }
  return(p)
}


single_interval_plot_wrapper <- function(estimand, .output, .save_name){
  tt_plot_function(resultdat = .output[[estimand]]$ci %>% reshape2::melt(id.vars=c("month")) %>% 
                     mutate(surv=1-value, t=month*30, group=ifelse(variable=="ci_1", "surgery", "no surgery")), 
                   filename=paste0(.save_name, "-", estimand))
}