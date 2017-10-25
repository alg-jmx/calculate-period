library(readxl)
options(warn = -1)
dataSet = read_excel("/Volumes/wangting/2017/第三季度执行情况/wtfile/data/分析-综合采购部.xlsx", sheet = "服务及货物",
                     col_types = c("text", 
                                  "text", "numeric", "text", 
                                   "text", "text", "text", "text", "date", 
                                   "date", "date", "date", "date", "text", 
                                   "text", "text", "text", "text", "text", 
                                   "date", "date", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "text", "text", "text", 
                                   "text", "text", "text"))
