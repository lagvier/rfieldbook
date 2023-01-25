#' design_splitplot function to create `split plot` design with r replications in `s` sites
#'@params main_plots list of main plot factors
#'@params sub_plots list of sub-plot factors
#'@params reps number of replications for each site
#'@param sites number of sites to carry out the experiment
#'@export

design_splitplot <- function(main_plots, sub_plots, reps, sites){
  
  sdata = lapply(sites, function(site){
    
    rdata = lapply(c(1:reps), function(Rep){
      main_plots  = unique(main_plots)
      main_plot  = unique(sub_plots)
      
      mplots = sample(main_plot, length(main_plots), replace = F)
      mdata = lapply(mplots, function(mp){
        
        splots = sample(sub_plots, length(sub_plots), replace = F)
        df = as.data.frame(cbind(
          SITENO = site,
          REP = Rep,
          MainPlot = mp,
          SubPlot = splots
        ))
        
        df$s_plot = c(1:nrow(df))
        return(df)
        
      })
      mdata = do.call('rbind', mdata)
      mdata$Plot = c(1:nrow(mdata))
      
      return(mdata)
      
    })
    data = do.call('rbind', rdata)
    return(data)
  })
  
  data = do.call('rbind', sdata)
  
  return(data)
}
