get_data = function(path_list,
                    root_path,
                    sheetName,
                    workDayList,
                    savePath) {
  suppressMessages(require(xlsx))
  
  for (path in path_list) {
    # path = "分析-培训部.xls"
    # cat("当前文件：", path, "\n")
    data1 = tryCatch({
      # Just to highlight: if you want to use more than one
      # R expression in the "try" part then you'll have to
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression
      # in case the "try" part was completed successfully
      
      # message("This is the 'try' part")
      
      data.frame(read_excel(
        paste(root_path, path, sep = ""),
        sheet = sheetName,
        col_types = c(
          "text",
          "text",
          "numeric",
          "text",
          "text",
          "text",
          "text",
          "text",
          "date",
          "date",
          "date",
          "date",
          "date",
          "text",
          "text",
          "text",
          "text",
          "text",
          "text",
          "date",
          "date",
          "numeric",
          "numeric",
          "numeric",
          "numeric",
          "numeric",
          "numeric",
          "text",
          "text",
          "text",
          "text",
          "text",
          "text"
        )
      ))
      # The return value of `readLines()` is the actual value
      # that will be returned in case there is no condition
      # (e.g. warning or error).
      # You don't need to state the return value via `return()` as code
      # in the "try" part is not wrapped insided a function (unlike that
      # for the condition handlers for warnings and error below)
    },
    error = function(cond) {
      message(paste("The error file is:", path))
      message("Here's the original error message:")
      message(cond)
      message(cat("", "\n"))
      
      # Choose a return value in case of error
      return(NA)
    },
    # warning = function(cond) {
    #   message(paste("file caused a warning:", path))
    #   message("Here's the original warning message:")
    #   message(cond)
    #   # Choose a return value in case of warning
    #   return()
    # },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>'
      message(paste("Processed file:", path))
      # message("Some other message at the end")
    })
    if (!is.na(data1)) {
      dataNew = data.frame(data1[, c("序号", "采购方式")])
      dataZero = data.frame(matrix(0, nrow(data1), 8))
      colnames(dataZero) = c(
        "采购准备周期",
        "采购需求确认周期",
        "采购实施方案获批周期",
        "采购实施结果获批周期",
        "合同审批周期",
        "采购实施周期",
        "到结果中选周期",
        "到合同获批周期"
      )
      dataNew = cbind(dataNew, dataZero)
      for (i in 1:nrow(data1)) {
        # cat("循环第", i, "次", "\n")
        # cat("开始日期: ", data1[i, '采购实施方案获批日期'], '\n')
        # cat("截止日期：", data1[i, '接到采购申请日期'], '\n')
        dataNew$采购准备周期[i] = GetDateDiff(
          dateTransform(data1$接到采购申请日期[i]),
          dateTransform(data1$采购实施方案获批日期[i]),
          workDayList
        )
        dataNew$采购需求确认周期[i] = GetDateDiff(
          dateTransform(data1$接到采购申请日期[i]),
          dateTransform(data1$采购需求确认时间[i]),
          workDayList
        )
        dataNew$采购实施方案获批周期[i] = GetDateDiff(
          dateTransform(data1$采购需求确认时间[i]),
          dateTransform(data1$采购实施方案获批日期[i]),
          workDayList
        )
        dataNew$采购实施结果获批周期[i] = GetDateDiff(
          dateTransform(data1$采购实施方案获批日期[i]),
          dateTransform(data1$采购结果获批日期[i]),
          workDayList
        )
        dataNew$采购实施周期[i] = GetDateDiff(
          dateTransform(data1$采购需求确认时间[i]),
          dateTransform(data1$采购结果获批日期[i]),
          workDayList
        )
        dataNew$合同审批周期[i] = GetDateDiff(dateTransform(data1$采购结果获批日期[i]),
                                        dateTransform(data1$合同获批日期[i]),
                                        workDayList)
        dataNew$到结果中选周期[i] = GetDateDiff(
          dateTransform(data1$接到采购申请日期[i]),
          dateTransform(data1$采购结果获批日期[i]),
          workDayList
        )
        dataNew$到合同获批周期[i] = GetDateDiff(dateTransform(data1$接到采购申请日期[i]),
                                         dateTransform(data1$合同获批日期[i]),
                                         workDayList)
      }
      dataNew = dataNew[!is.na(dataNew$序号) &
                          dataNew$序号  != "C" & dataNew$序号  != "D", ]
      index = which(apply(dataNew, 1, function(x)
        sum(is.na(x))) >= 8)
      dataNew = dataNew[-index,]
      
      if (file.exists(paste(savePath, path))) {
        wb <- xlsx::loadWorkbook(paste(savePath, path))
        sheetNameList = xlsx::getSheets(wb)
        if (sheetName %in% names(sheetNameList)) {
          xlsx::removeSheet(wb, sheetName = sheetName)
        }
        mysheet = xlsx::createSheet(wb, sheetName = sheetName)
        addDataFrame(dataNew, mysheet, row.names = F)
        xlsx::saveWorkbook(wb, paste(savePath, path))
      } else {
        write.xlsx2(dataNew,
                    paste(savePath, path),
                    sheetName = sheetName,
                    row.names = F)
      }
      # print(paste(savePath, path))
    }
  }
}