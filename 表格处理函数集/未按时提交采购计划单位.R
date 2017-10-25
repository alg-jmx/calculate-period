departmentCount = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "项目进度", "2017年启动采购实施时间", "采购申请单位", "采购实施单位")]
  colnames(dataSet) = c("num", "itemProcess", "actionTime", "applyDepartment", "actionDepartment")
  season = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月")
  result = dataSet %>%
    filter(! num %in% c("C", "D"), !is.na(num), actionTime %in% season, itemProcess == "未启动") %>%
    group_by(applyDepartment, actionDepartment) %>%
    summarise(count = n()) %>%
    ungroup()
  result
}
departmentCount(dataSet)