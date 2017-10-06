# load libray
library(yaml)

report.base <- function(
  conf
){
  # init global configurations
  #conf <- yaml.load_file("yaml/conf.yaml")
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  g.var <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat$var
  
  source(paste0(g.dir$R,"report/report.R"))
  source(paste0(g.dir$R,"report/plot.R"))
  source(paste0(g.dir$R,"report/dataframe.R"))
  #######################################################
  
  reports <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$report
  n <- length(reports$report)
  for(i in 1:n){
    report <-  reports$report[[i]]
    if(report$process){
      report$plot.out  <- paste0(getwd(),"/",g.dir$report.out.plot)
      report$skip <- reports$skip
      if(is.null(report$table)){
        report$table <-  reports$table
      }
      if(is.null(report$aliasTable)){
        report$aliasTable <-  reports$aliasTable
      }
      if(is.null(report$aliasFlag)){
        report$aliasFlag <-  reports$aliasFlag
        print(report$aliasFlag)
      }
      
      if(is.null(report$output)){
        report$output  <- reports$output
      }
      if(is.null(report$dummy.fig)){
        report$dummy.fig  <- reports$dummy.fig
      }
      if(is.null(report$geom)){
        report$geom  <- reports$geom
      }
      report.render(report,conf)
    }
    
  }
  
}
