purchaseCount = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "项目进度", "采购方式")]
  colnames(dataSet) = c("num", "itemProcess", "purchaseType")
  result = dataSet %>%
    filter(!num %in% c("C", "D"), !is.na(num), itemProcess != "未启动") %>%
    group_by(purchaseType) %>%
    summarise(count = n()) %>%
    ungroup()
  result
}

itemPay = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "项目进度", "采购方式", "项目预算", "合同金额")]
  colnames(dataSet) = c("num", "itemProcess", "purchaseType", "itemPre", "dealAmo")
  result = dataSet %>%
    filter(!num %in% c("C", "D"), !is.na(num), itemProcess != "未启动", !is.na(itemPre), !is.na(dealAmo)) %>%
    group_by(purchaseType) %>%
    summarise(itemPreSum = sum(itemPre), dealAmoSum = sum(dealAmo)) %>%
    ungroup()
  result
}
purchaseCount(dataSet)
itemPay(dataSet)