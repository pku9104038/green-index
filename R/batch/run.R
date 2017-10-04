########################

conf <- yaml.load_file("yaml/conf.yaml")

########################

source(paste0(conf$dir$R,"ETL/db_init.R"))
db.init()

########################

source(paste0(conf$dir$R,"merge/merge_table.R"))
merge.table()

########################

source(paste0(conf$dir$R,"weight/weight_table.R"))
weight.table()

########################

source(paste0(conf$dir$R,"clean/clean_data.R"))
clean.data()

########################

source(paste0(conf$dir$R,"variable/variable_data.R"))
variable.data()

########################

source(paste0(conf$dir$R,"statistics/statistics_data.R"))
statistics.data()

########################

source(paste0(conf$dir$R,"score/score_data.R"))
score.data()

########################

source(paste0(conf$dir$R,"index/index_data.R"))
index.data()

########################

source(paste0(conf$dir$R,"report/report_base.R"))
report.base()

########################