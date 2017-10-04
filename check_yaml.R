library(yaml)
# init global configurations
check.yaml <- function(){
  check_define <- yaml.load_file("yaml/conf.yaml")
  check_survey <- yaml.load_file("yaml/survey_2016.yaml")
  check_source <- yaml.load_file("yaml/source_2016.yaml")
}

############################
check.yaml()