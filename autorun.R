## set script directory as working directory 
## run in command line mode

library(here)
here()
setwd(here())
options(java.parameters = "-Xmx8g")
source('run.R')



