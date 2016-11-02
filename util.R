#Remove cols from a dataframe
removeCols = function(data, colNames) {
  colsToKeep = setdiff(names(data), colNames)
  return(data[, colsToKeep])
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
  cat('    Trn/CV, Train: ', trnError, '/', cvError, ', ', trainError, '\n', sep='')
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

printNaCols = function(data) {
  print(Filter(function(x) sum(x) > 0, sapply(data, function(x) sum(is.na(x)))))
}