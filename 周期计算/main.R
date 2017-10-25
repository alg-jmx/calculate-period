rm(list = ls())
gc()
setwd("/Volumes/wangting/2017/1024/第三季度执行情况/wtfile/")
source("./周期计算/get_holiday.R", encoding = "utf-8")
source("./周期计算/utils.R", encoding = "utf-8")
require(readxl)
require(xlsx)
# require(quantmod)
# getSymbols("000001.SZ",
#            env = globalenv(),
#            from = "2015-01-01",
#            to = Sys.Date())
# workDayList = row.names(data.frame(`000001.SZ`))
# write.xlsx(data.frame(`000001.SZ`), "/Volumes/WANGTING/2017/1024/第三季度执行情况/wtfile/data.xlsx", row.names = T)
data = read_excel("./data.xlsx")
workDayList = as.vector(t(data[, 1]))
root_path = "./data/type2/"
path_list = dir(root_path)
get_data(
  path_list = path_list,
  root_path = root_path,
  sheetName = "预算类采购计划IT组",
  workDayList = workDayList,
  savePath = "./result/"
)
