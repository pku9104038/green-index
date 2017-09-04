# load libray
library(yaml)
library(RPostgreSQL)
# init global configurations
conf <- yaml.load_file("yaml/conf.yaml")
g.dir <- conf$dir
g.yaml <- conf$yaml
g.test <- conf$test
g.stat <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$global$stat

g.test$loop.limit <- FALSE
# source scripts
source(paste0(g.dir$R,"ETL/db.R"))
source(paste0(g.dir$R,"check/check.R"))
source(paste0(g.dir$R,"merge/merge.R"))



#  main 
subjects <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$survey

n_subject <- mini.loop(g.test$loop.limit,length(subjects))
for(i in 1:n_subject){
  subject <-  subjects[[i]]
  print(subject$subject)
  merge.subject(subject = subject)
}