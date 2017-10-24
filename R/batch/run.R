

########################
library(yaml)
source("check_yaml.R")
check.yaml()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"ETL/db_init.R"))
db.init()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"merge/merge_table.R"))
merge.table()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"weight/weight_table.R"))
weight.table()


########################
library(yaml)
conf = yaml.load_file("yaml/conf.yaml")
yaml <- yaml.load_file(paste0(conf$dir$yaml,conf$yaml$survey))
source(paste0(conf$dir$R,"count/count.R"))
count.subject(yaml$count)


########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"clean/clean_data.R"))
clean.data()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"variable/variable_data.R"))
variable.data()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"variable/variable_data.R"))
variable.data2()

########################

library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"statistics/statistics_data.R"))
statistics.data()  #topic.name = "得分率"

########################

source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"score/score_data.R"))
score.data()

########################
library(yaml)
source(paste0(yaml.load_file("yaml/conf.yaml")$dir$R,"index/index_data.R"))
index.data()

########################
library(yaml)
conf = yaml.load_file("yaml/conf.yaml")
source(paste0(g.dir$R,"combine/combine.R"))
bind <- yaml.load_file(paste0(conf$dir$yaml,conf$yaml$survey))$combine$bind
index.combine(bind)

########################
library(yaml)
conf = yaml.load_file("yaml/conf.yaml")
yaml <- yaml.load_file(paste0(conf$dir$yaml,conf$yaml$survey))
source(paste0(conf$dir$R,"report/report_base.R"))
report.base(conf = conf, yaml = yaml)

########################
