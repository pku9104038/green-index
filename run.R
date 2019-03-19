## Entry script for green index data analyse

options(java.parameters = "-Xmx16g")  # set java heap size before run.R


## set instance config for global.R
gi.dir.script <- "R/pipeline/"
gi.config.yaml <- "yaml/config.yaml"
gi.db.user <- "wangpeifeng"
gi.db.pwd <- ""


## source global.R
# this will create and init all global objects
if (!exists("global.loaded", mode = "variable")){
  source(paste0(gi.dir.script,"global.R"))
}

######################################
 

## source scheduler.R, this must sourced after global.R
gi.dir.schedule <- "R/schedule/"
if(!exists("scheduler.loaded", mode = "variable")) {
  source(paste0(gi.dir.schedule,"scheduler.R"))
}
## init scheduler
gio.scheduler <- GreenIndexScheduler$new()
gio.scheduler$Init("Scheduler", gio.config)


## Running
gio.scheduler$Run()
