GetDateDiff = function(startDate, endDate, workDayList) {
  if(is.na(startDate)|is.na(endDate)) {
    return(NA)
  } else {
    start = as.Date(startDate)
    end = as.Date(endDate)
    # except = c("工作日")
    result = c()
    for (elem in start:end) {
      tempDate = as.Date(elem, origin = "1970-01-01")
      result = append(result, sum(as.character(tempDate) %in% workDayList))
    }
    # cat("工作日总计:",sum(result),"\n")
    return(sum(result))
  }
}

dateTransform = function(date) {
  if(is.na(date)) {
    return(NA)
  }else if (length(strsplit(as.character(date), "-")[[1]]) > 1) {
    as.Date(date, format = "%Y-%m-%d")
  } else {
    format(as.Date(date, format = "%Y/%m/%d"), "%Y-%m-%d")
  }
}





