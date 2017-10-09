library(yaml)

########################

source("check_yaml.R")
check.yaml()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"ETL/db_init.R"))
db.init()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"merge/merge_table.R"))
merge.table()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"weight/weight_table.R"))
weight.table()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"clean/clean_data.R"))
clean.data()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"variable/variable_data.R"))
variable.data()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"statistics/statistics_data.R"))
statistics.data()  #topic.name = "得分率"

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"score/score_data.R"))
score.data()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"index/index_data.R"))
index.data()

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"report/report_base.R"))
report.base(conf = yaml.load_file("yaml/conf.yaml"))

########################
