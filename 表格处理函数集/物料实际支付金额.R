getGroupData = function(dataSet) {
  library(dplyr)
  library(magrittr)
  dataSet = dataSet[, c("序号", "一级物料分类目录", 
                        "2017年预算", "2017年实际支付金额", "项目预算", "合同金额")]
  colnames(dataSet) = c("num", "type", "pre1", "accPre1", "itemPre", "dealAmo")
  dataSet = filter(dataSet, !is.na(num), num != "C", num != "D")
  # newData1过滤"2017年预算", "2017年实际支付金额"同时不为空
  newData1 = dataSet %>%
    filter(!is.na(pre1) & !is.na(accPre1)) %>%
    group_by(type) %>%
    summarise(budgetSum = sum(pre1),
              payAmoSum = sum(accPre1)) %>%
    ungroup()
  # newData1过滤"项目预算", "合同金额"同时不为空
  newData2 = dataSet %>%
    filter(!is.na(itemPre) & !is.na(dealAmo)) %>%
    group_by(type) %>%
    summarise(itemPreSum = sum(itemPre),
              dealAmoSum = sum(dealAmo)) %>%
    ungroup()
  # newData3直接计算"2017年预算", "2017年实际支付金额", "项目预算", "合同金额"各自的和
  newData3 = dataSet %>%
    group_by(type) %>%
    summarise(itemPreSum = sum(itemPre, na.rm = T),
              dealAmoSum = sum(dealAmo, na.rm = T),
              budgetSum = sum(pre1, na.rm = T),
              payAmoSum = sum(accPre1, na.rm = T)) %>%
    ungroup()
  list(newData1, newData2, newData3)
}
getGroupData(dataSet)