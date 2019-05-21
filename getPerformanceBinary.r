#' Compares predicted and actual outcomes to calculate performance stats for a designated prediction model object
#'
#' @param fit caret::Train object
#' @param newdata data to apply prediction on
#' @param testing TRUE if newdata is validation set; FALSE if newdata is discovery set
 




getPerformanceBinary<-function(fit, newdata=fit$trainingData, testing=FALSE){
  require(ModelMetrics)
  
  if (testing==FALSE) {
    pred<-predict.train(fit,fit$trainingData)
    actual<-factor(fit$trainingData$.outcome)
  } else {
    db.test<-na.omit(newdata[,c(gsub(' ~ .','',as.character(fit$call)[2]),
                                colnames(fit$trainingData)[-1])])
    pred<-predict(fit,db.test)
    actual<-factor(db.test$Response)
  }
  
  stats<-caret::confusionMatrix(pred,actual,dnn = c("Prediction", "Reference"))
  print(stats$table)
  t<-as.vector(stats$table)
  Perf<-c(stats$byClass, stats$overall)[c(1:4,7,11:13)]
  OOB<-ifelse(fit$method=='rf',mean(fit$finalModel$err.rate[,1]),NA)
  Perf<-c(Perf,c('AUC'=ModelMetrics::auc(actual,pred),
                 'Brier'=ModelMetrics::brier(actual,pred),
                 'Mathews'=ModelMetrics::mcc(actual,pred,0.5),
                 'FP'=t[2],'FN'=t[3],OOB=OOB))
  
  
  
  return(Perf)
}