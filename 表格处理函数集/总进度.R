# 返回结果里第一个是对增减计数， 第二个是对项目进度计数
statisticCount = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "项目进度", "合同级别")]
  colnames(dataSet) = c("num", "itemProcess", "contractLevel")
  numCount = dataSet %>%
    filter(num %in% c("C", "D", "N")) %>%
    group_by(num) %>%
    summarise(count = n()) %>%
    ungroup()
  
  itemProcessCount = dataSet %>%
    filter(!num %in% c("C", "D"), !is.na(num)) %>%
    group_by(itemProcess) %>%
    summarise(count = n()) %>%
    ungroup()
  
  contractLevelCount = dataSet %>%
    filter(!num %in% c("C", "D"), !is.na(num)) %>%
    group_by(contractLevel) %>%
    summarise(count = n()) %>%
    ungroup()
  list(numCount=numCount, itemProcessCount=itemProcessCount, contractLevelCount=contractLevelCount)
}
statisticCount(dataSet)