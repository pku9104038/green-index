library(yaml)
# init global configurations
check.yaml <- function(){
  yaml_file <- "yaml/conf.yaml"
  print(paste("Check",yaml_file))
  check_define <- yaml.load_file(yaml_file)
  
  #check_survey <- yaml.load_file("yaml/survey_2016.yaml")
  yaml_file <- "yaml/survey_2016.yaml"
  print(paste("Check",yaml_file))
  check_define <- yaml.load_file(yaml_file)
  
  #check_source <- yaml.load_file("yaml/source_2016.yaml")
  yaml_file <- "yaml/source_2016.yaml"
  print(paste("Check",yaml_file))
  check_define <- yaml.load_file(yaml_file)
  
}

############################
check.yaml()