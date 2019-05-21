
#' Wrapper for callTrain applied over vector of models types
#'
#' @param models list of models
#' @param trainControl list object outputted by caret::trainControl
#' @param metric metric of interest
#' @data training data




callTrainModels<-function(data,models,trainControl,metric='AUC',preProcess=NULL,...){
  fits<-lapply(models, callTrain, data=data, trainControl, metric,...)
  names(fits)<-models
  return(fits)
}