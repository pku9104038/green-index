# load libray
library(yaml)

statistics.data <- function(){
  # init global configurations
  conf <- yaml.load_file("yaml/conf.yaml")
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  g.test <- conf$test
  g.stat <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat
  
  
  # source scripts
  source(paste0(g.dir$R,"check/check.R"))
  source(paste0(g.dir$R,"statistics/statistics.R"))
  
  # test switch
  g.test$loop.limit <- FALSE
  #  main 
  
  algorithm <- g.stat$def$statistics
  subjects <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$survey
  #subjects <- yaml.load_file(paste0(g.dir$yaml,"survey_2016-1.yaml"))$survey
  n_subject <- mini.loop(g.test$loop.limit,length(subjects))
  for(i in 1:n_subject){
    subject <-  subjects[[i]]
    if(subject$process){
      print(subject$subject)
      statistics.subject(subject = subject, algorithms = algorithm)
    }
    
  }
  
}
