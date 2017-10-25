library(readxl)
library(dplyr)
library(magrittr)

# �截止三季度应启动accturalAction=F，截止三季度实际启动�
statisticSeason = function(data, season = 1, accturalAction = T) {
  season4 = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月")
  newSeason = season4[1: (3 * season)]
  dataSet = data[, c("序号", "物料属性", "2017年启动采购实施时间", "项目进度")]
  colnames(dataSet) = c("num", "type", "actionTime", "itemProcess")
  if(accturalAction) {
    newData = dataSet %>%
      filter(!is.na(num), num != "C", num != "D", itemProcess != "未启动")
  } else {
    newData = dataSet %>%
      filter(!is.na(num), num != "C", num != "D")
  }
  result = newData %>%
    filter(actionTime %in% newSeason) %>%
    group_by(type) %>%
    summarise(count = n()) %>%
    ungroup()
  result
}

# 截止第三季度至少应完成accturalAction=F
statisticSeasonAtLeast = function(data, season = 1, accturalAction = T) {
  season4 = c("1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月")
  newSeason = season4[1: (3 * (season-1))]
  dataSet = data[, c("序号", "物料属性", "2017年启动采购实施时间", "项目进度")]
  colnames(dataSet) = c("num", "type", "actionTime", "itemProcess")
  if(accturalAction) {
    newData = dataSet %>%
      filter(!is.na(num), num != "C", num != "D", itemProcess %in% c("合同获批", "履约结束", "履约中"))
  } else {
    newData = dataSet %>%
      filter(!is.na(num), num != "C", num != "D")
  }
  result = newData %>%
    filter(actionTime %in% newSeason) %>%
    group_by(type) %>%
    summarise(count = n()) %>%
    ungroup()
  result
}
statisticSeason(dataSet,3,F)
statisticSeason(dataSet,3,T)
statisticSeasonAtLeast(dataSet,3,F)
statisticSeasonAtLeast(dataSet,3,T)

