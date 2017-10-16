# load libray
library(yaml)

combine.table <-  function(conf){
  # init global configurations
  g.dir <- conf$dir
  g.yaml <- conf$yaml
  
  # source scripts
  source(paste0(g.dir$R,"combine/combine.R"))
  
  bind <- yaml.load_file(paste0(g.dir$yaml,g.yaml$survey))$combine$bind
  index.combine(bind)
}
