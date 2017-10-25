
actionCount = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "项目进度", "2017年启动采购实施时间")]
  colnames(dataSet) = c("num", "itemProcess", "actionTime")
  season = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月")
  result = dataSet %>%
    filter(! num %in% c("C", "D"), !is.na(num), actionTime %in% season) %>%
    group_by(itemProcess) %>%
    summarise(count = n()) %>%
    ungroup()
  result
}
actionCount(dataSet)