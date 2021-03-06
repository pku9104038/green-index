# load libray
library(yaml)

variable.data <- function(){
  # init global configurations
  conf <- yaml.load_file("yaml/conf.yaml")
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  g.test <- conf$test
  g.stat <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat
  
  
  # source scripts
  source(paste0(g.dir$R,"check/check.R"))
  source(paste0(g.dir$R,"variable/variable.R"))
  
  # test switch
  g.test$loop.limit <- FALSE
  #  main 
  
  algorithm <- g.stat$def$variable$type
  subjects <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$survey
  n_subject <- mini.loop(g.test$loop.limit,length(subjects))
  for(i in 1:n_subject){
    subject <-  subjects[[i]]
    if(subject$process){
      print(subject$subject)
      variable.subject(subject = subject, algorithms = algorithm )
      
    }
    
  }
}

#####################################
variable.data2 <- function(){
  # init global configurations
  conf <- yaml.load_file("yaml/conf.yaml")
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  g.test <- conf$test
  g.stat <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat
  
  
  # source scripts
  source(paste0(g.dir$R,"check/check.R"))
  source(paste0(g.dir$R,"variable/variable.R"))
  
  # test switch
  g.test$loop.limit <- FALSE
  #  main 
  
  algorithm <- g.stat$def$variable$type
  subjects <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$survey
  n_subject <- mini.loop(g.test$loop.limit,length(subjects))
  for(i in 1:n_subject){
    subject <-  subjects[[i]]
    if(subject$process){
      print(subject$subject)
      variable.subject2(subject = subject, algorithms = algorithm )
      
    }
    
  }
}

