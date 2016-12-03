hashmap = function() {
  return(new.env())
}

#------------- data frame stuff ----------------
#Remove cols from a dataframe
removeCols = function(data, colNames) {
  return(data[, setdiff(names(data), colNames)])
}
removeRows = function(data, rows) {
  return(data[setdiff(rownames(data), rownames(rows)),])
}
shuffle = function(data) {
  return(data[sample(nrow(data)),])
}
splitData = function(data, yName) {
  require(caret) #createDataPartition

  #split data into train and cv
  set.seed(837)
  partitionIndices = caret::createDataPartition(data[, yName], p=0.8, list=FALSE)
  train = data[partitionIndices,]
  cv = data[-partitionIndices,]
  return(list(train=train, cv=cv))
}
convertToDataFrame = function(data, colNames) {
  df = data.frame(data)
  colnames(df) = colNames
  return(df)
}

getFormula = function(yName, xNames) {
  return(as.formula(paste(yName, '~', paste(xNames, collapse='+'))))
}

computeTrainCVErrors = function(data, yName, xNames, createModel, createPrediction, computeError) {
  #split data into train, cv
  split = splitData(data, yName)
  train = split$train
  cv = split$cv

  #compute train and cv errors
  model = createModel(train, yName, xNames)
  trainError = computeError(train[, yName], createPrediction(model, train, xNames))
  cvError = computeError(cv[, yName], createPrediction(model, cv, xNames))

  return(list(train=trainError, cv=cvError))
}
printTrnCvTrainErrors = function(model, data, yName, xNames, createModel, createPrediction, computeError) {
  cat('Computing Errors...\n')
  trnCvErrors = computeTrainCVErrors(data, yName, xNames, createModel, createPrediction, computeError)
  trnError = trnCvErrors$train
  cvError = trnCvErrors$cv
  trainError = computeError(data[, yName], createPrediction(model, data, xNames))
  cat('    Trn/CV/Train: ', trnError, '/', cvError, '/', trainError, '\n', sep='')
}

outputSolution = function(createPrediction, model, testData, idName, yName, xNames, filename, extraColNames=c()) {
  cat('Outputing solution...\n')
  cat('    Creating prediction...\n')
  prediction = createPrediction(model, testData, xNames)
  solution = data.frame(testData[, idName], prediction, testData[, extraColNames])
  colnames(solution) = c(idName, yName, extraColNames)
  cat('    Writing solution to file: ', filename, '...\n', sep='')
  write.csv(solution, file=filename, row.names=F, quote=F)
}

printNaCols = function(data, cumulate=T) {
  naCounts = Filter(function(x) sum(x) > 0, sapply(data, function(x) sum(is.na(x))))

  if (cumulate) {
    naCounts = sort(naCounts)
    colNames = c(names(naCounts[1]))
    numNAs = naCounts[1]
    cnt = 2
    while (cnt <= length(naCounts)) {
      if (naCounts[cnt] == numNAs) {
        colNames = c(colNames, names(naCounts[cnt]))
      } else {
        #print colnames
        cat('Cols with ', numNAs, ' NAs: ', paste(colNames, collapse=', '), '\n', sep='')
        numNAs = naCounts[cnt]
        colNames = c(names(naCounts[cnt]))
      }
      cnt = cnt + 1
    }
    cat('Cols with ', numNAs, ' NAs: ', paste(colNames, collapse=', '), '\n', sep='')

  } else {
    print(naCounts)
  }
}
printFactorCols = function(data) {
  print(colnames(Filter(is.factor, data)))
}

sumna = function(data) {
  print(sum(is.na(data)))
}
lu = function(data) {
  return(length(unique(data)))
}
ludates = function(d, condition) {
  return(lu(d[condition, 'Date']))
}
lunames = function(d, condition) {
  return(lu(d[condition, 'Name']))
}
unames = function(d, condition) {
  return(u(d[condition, 'Name']))
}
udates = function(d, condition) {
  return(u(d[condition, 'Date']))
}
lw = function(condition) {
  return(length(which(condition)))
}
w = function(condition) {
  return(which(condition))
}
u = function(data) {
  return(unique(data))
}
h = function(d, condition, extraColNames=c(), numRows=5) {
  return(head(d[condition, c(F.ID, extraColNames)], numRows))
}
t = function(d, condition, extraColNames=c()) {
  return(tail(d[condition, c(F.ID, extraColNames)], 5))
}
l = function(d) {
  return(length(d))
}

convertBinaryColToFactor = function(binaryCol, label) {
  return(factor(ifelse(binaryCol, label, paste0('Not', label))))
}

removeNAsInColName = function(data, colName) {
  return(data[-which(is.na(data[[colName]])),])
}

getMinOfLists = function(lists) {
  minOfLists = Inf
  for (i in 1:length(lists)) {
    minOfLists = min(minOfLists, lists[[i]], na.rm=T)
  }
  return(minOfLists)
}
getMaxOfLists = function(lists) {
  maxOfLists = -Inf
  for (i in 1:length(lists)) {
    maxOfLists = max(maxOfLists, lists[[i]], na.rm=T)
  }
  return(maxOfLists)
}

#------------- Testing --------------------
testForDuplicates = function(d, col1, col2, extrCols=c()) {
  cat('col1 != col2: ', lw(d[[col1]] != d[[col2]]), '\n')
  cat('col1 != col2 AND col1 != 0: ', lw((d[[col1]] != d[[col2]]) & (d[[col1]] != 0)), '\n')
  cat('col1 != col2 AND col2 != 0: ', lw((d[[col1]] != d[[col2]]) & (d[[col2]] != 0)), '\n')
  d[head(w((d[[col1]] != d[[col2]]) & (d[[col2]] != 0))), c(F.ID, col1, col2, extrCols)]
}
findColsThatAreDifferent = function(d1, d2) {
  colNamesThatAreDifferent = c()
  numRowsThatAreDifferent = c()
  for (colName in colnames(d1)) {
    if (!identical(d1[[colName]], d2[[colName]])) {
      colNamesThatAreDifferent = c(colNamesThatAreDifferent, colName)
      numRowsThatAreDifferent = c(numRowsThatAreDifferent, lw(oldd[[colName]] != newd[[colName]]))
    }
  }
  cat('Cols that are different:\n')
  #cat('Num rows that are different (respectively): ', paste(numRowsThatAreDifferent, collapse=', '), '\n', sep='')
  for (i in 1:length(colNamesThatAreDifferent)) {
    cat(colNamesThatAreDifferent[i], ' (', numRowsThatAreDifferent[i], '), ', sep='')
  }
  cat('\n')
}

#------------- misc ----------------
#population standard deviation
psd = function(arr) {
  arrLen = length(arr)
  if (arrLen < 2) {
    return(0)
  }
  return(sd(arr) * sqrt((arrLen-1) / arrLen))
}
getCurrencyString = function(number) {
  if (number < 0) {
    return(paste0('-$', round(abs(number), 2)))
  }
  return(paste0('$', round(number, 2)))
}
