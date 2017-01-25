createPlotFilename = function(plotName, suffix, plotDir='plots/') {
  return(paste0(plotDir, plotName, '_', suffix, '.png'))
}
startSavePlot = function(plotName, suffix, plotDir='plots/', width=1000, height=700, res=150) {
  png(createPlotFilename(plotName, suffix, plotDir), width=width, height=height, res=res)
}
endSavePlot = function() {
  dev.off()
}
addLegend = function(labels, colors) {
  legend(x='topright', legend=labels, fill=colors, inset=0.02, cex=0.7)
}

#Plot learning curve
#E.g. plotLearningCurve(train, 'SalePrice', createModel, createPrediction, computeError, save=PROD_RUN)
plotLearningCurve = function(data, yName, xNames, createModel, createPrediction, computeError, increment=round(nrow(data)/1000)*100, ylim=NULL, save=FALSE) {
  cat('Plotting learning curve (dataSize=', nrow(data),', increment=', increment, ')...\n', sep='')

  #split data into train and cv
  split = splitData(data, yName)
  train = split$train
  cv = split$cv

  increments = c(1, seq(increment, nrow(train), increment))
  numIterations = length(increments)
  trainErrors = numeric(numIterations)
  cvErrors = numeric(numIterations)

  count = 1
  for (i in increments) {
    if (i %% 100 == 0) cat('    On training example ', i, '\n', sep='')
    trainSubset = train[1:i,]

    modelTry = tryCatch(createModel(trainSubset, yName, xNames), warning=function(w) w)
    if(is(modelTry, 'warning') && modelTry$message == 'The response has five or fewer unique values.  Are you sure you want to do regression?' && i == 1) {
      #suppress the warning
      model = suppressWarnings(createModel(trainSubset, yName, xNames))
    } else {
      #don't suppress the warning so that it gets raised normally
      model = createModel(trainSubset, yName, xNames)
    }
    trainErrors[count] = computeError(trainSubset[, yName], createPrediction(model, trainSubset, xNames))
    cvErrors[count] = computeError(cv[, yName], createPrediction(model, cv, xNames))

    count = count + 1
  }

  if (is.null(ylim)) {
    ylim = c(0, max(cvErrors, trainErrors))
  }

  if (save) png(paste0('LearningCurve_', FILENAME, '.png'), width=1000, height=700, res=150)
  plot(increments, trainErrors, col='blue', type='l', ylim=ylim, main='Learning Curve', xlab = "Number of Training Examples", ylab = "Error")
  lines(increments, cvErrors, col='red')
  legend('topright', legend=c('train', 'cv'), fill=c('blue', 'red'), inset=.02, text.width=150)
  if (save) dev.off()
}

#This will plot a histogram of all the cols in data, so be careful
#to pass in data that only contains the columns you want plotted
#Note: All cols must be factors, and the number of cols should be <= 8
#E.g. plotTrainAndTestHistograms(Filter(is.factor, train), Filter(is.factor, test))
#E.g. plotTrainAndTestHistograms(Filter(is.factor, train), Filter(is.factor, test), 5)
#E.g. plotTrainAndTestHistograms(Filter(is.factor, train), Filter(is.factor, test), 10, 1)
plotTrainAndTestHistograms = function(trainData, testData, startIndex=1, numColsToPlot=4, numGridCols=2) {
  numCols = ncol(trainData)
  numTestCols = ncol(testData)

  #error checking
  if (numCols != numTestCols) stop(paste('Train and test data have a different number of columns (test has', numCols, 'columns, test has', numTestCols, 'columns). They must be the same.'))
  if (numCols != ncol(Filter(is.factor, trainData))) stop('Train contains non-factor columns.  All columns must be factors.')
  if (numCols != ncol(Filter(is.factor, testData))) stop('Test contains non-factor columns.  All columns must be factors.')

  require(gridExtra) #grid.arrange
  plots = list()
  endIndex = startIndex + numColsToPlot - 1
  for (i in startIndex:endIndex) {
    #add train plot
    trainDf = data.frame(x=trainData[[i]])
    trainPlot = ggplot(data=trainDf, aes(x=factor(x))) + stat_count() + xlab(colnames(trainData)[i]) + theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust =1))

    #add test plot
    testDf = data.frame(x=testData[[i]])
    testPlot = ggplot(data=testDf, aes(x=factor(x))) + stat_count() + xlab(colnames(testData)[i]) + theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust =1))
    plots = c(plots, list(trainPlot, testPlot))
  }

  do.call('grid.arrange', c(plots, ncol=numGridCols))
}
#E.g. plotTrainHistograms(Filter(is.factor, train))
plotTrainHistograms = function(trainData, startIndex=1, numColsToPlot=4, numGridCols=2) {
  numCols = ncol(trainData)

  #error checking
  if (numCols != ncol(Filter(is.factor, trainData))) stop('Train contains non-factor columns.  All columns must be factors.')

  require(gridExtra) #grid.arrange
  plots = list()
  endIndex = startIndex + numColsToPlot - 1
  for (i in startIndex:endIndex) {
    #add train plot
    trainDf = data.frame(x=trainData[[i]])
    trainPlot = ggplot(data=trainDf, aes(x=factor(x))) + stat_count() + xlab(colnames(trainData)[i]) + theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust =1))
    plots = c(plots, list(trainPlot))
  }

  do.call('grid.arrange', c(plots, ncol=numGridCols))
}
#E.g. plotTrainHistogram(train$COLLEGE)
plotTrainHistogram = function(col) {
    #add train plot
    trainDf = data.frame(x=col)
    print(ggplot(data=trainDf, aes(x=factor(x))) + stat_count() + theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust =1)))
}

#Plot the correlations between all numeric cols in data
#E.g. plotCorrelations(d, Y_NAME)
plotCorrelations = function(d, yName, xNames=setdiff(colnames(d), F.TOEXCLUDE), numElements=30, save=F) {
  cat('Plotting correlations...\n')

  require(corrplot) #cor, corrplot
  correlationsMatrix = cor(Filter(is.numeric, d[,c(yName, xNames)]))

  #print correlations in order to console
  sortedCorrelations = sort(abs(correlationsMatrix[yName, ]), decreasing=T)[1:min(nrow(correlationsMatrix), numElements)]
  cat('Correlations with ', yName, ' (in order):\n', sep='')
  print(stack(sortedCorrelations))

  #plot the correlations
  if (save) {
    filename = 'Correlations.png'
    cat('Saving plot to:', filename, '\n')
    png(filename, width=1600, height=1600, res=150)
  }
  corrplot(correlationsMatrix[names(sortedCorrelations), names(sortedCorrelations)])
  if (save) dev.off()
}

#Plot the missing data for each column
plotMissingData = function(data) {
  require('VIM')
  aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
}

#I'm not sure what this does, but I used it at one point in the past, so
#maybe i'll find it useful again
plotNAs = function(data) {
  require(Amelia)
  missmap(data, main = "NAs by Column")
}

plotLinesByDate = function(dateStrs, ys=list(), labels=c(), main='Title', ylab='Y', save=FALSE, name='plot', filename='') {
  cat('    Plotting ', name, '\n', sep='')

  if (save) startSavePlot(name, filename)

  dates = as.Date(dateStrs)

  colors = c('red', 'blue', 'green', 'purple', 'orange')

  #plot the last one
  numYs = length(ys)
  plot(dates, ys[[numYs]], col=colors[numYs], type='l', main=main, xlab='Date', ylab=ylab, ylim=c(getMinOfLists(ys), getMaxOfLists(ys) + 1), xaxt='n')

  #plot the others in reverse order (so that the first ones show on top of the later ones)
  if (numYs > 1) {
    for (i in (numYs-1):1) {
      lines(dates, ys[[i]], col=colors[i])
    }
  }

  addLegend(labels[1:numYs], colors[1:numYs])
  axis.Date(side=1, dates, format="%m/%d")
  grid()

  if (save) endSavePlot()
}
plotByDate2Axis = function(dateStrs, y1, ylim=NULL, ylab='y1', y2=NULL, y2lab='y2', y2lim=NULL, main='Title', save=FALSE, name='plot', filename='') {
  cat('    Plotting ', name, '\n', sep='')

  if (save) startSavePlot(name, filename)

  dates = as.Date(dateStrs)

  #make the margin wider on side 4 (right side) if there is a second y
  if (is.null(y2)) {
    par(mar=c(5, 4, 4, 2) + 0.1)
  } else {
    par(mar=c(5, 4, 4, 5) + 0.1)
  }

  plot(dates, y1, ylim=ylim, type='l', col='red', main=main, xlab='Date', ylab=ylab, xaxt='n')
  axis.Date(side=1, dates, format="%m/%d")

  if (!is.null(y2)) {
    par(new=TRUE)
    plot(dates, y2, ylim=y2lim, type='l', col='blue', xaxt='n', yaxt='n', xlab='', ylab='')
    axis(side=4)
    mtext(y2lab, side=4, line=3)

    addLegend(c(ylab, y2lab), c('red', 'blue'))
  }

  if (save) endSavePlot()
}

