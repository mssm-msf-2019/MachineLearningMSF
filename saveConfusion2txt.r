#' Compares predicted and actual outcomes of a model fit object applies over a dataset and writes output to .csv file
#' @param fit caret::Train object
#' @param testing.data dataset for which confusion matrix is to be calculated
#' @param dir.pred location of directly to save outputs




saveConfusion2txt<-function(fit,testing.data,dir.pred) {
  a<-paste(unique(stringr::str_extract(colnames(fit$trainingData)[-1],"IgE|IgG4|ARAH")),collapse="")
  a<-ifelse(a=="ARAH","comp",a)
  b<-paste(unique(stringr::str_extract(colnames(fit$trainingData)[-1],"(v1|v12|v30|v60)$")),collapse="")
  b<-mgsub(c('v1$','v1v12','v1v30','v12$','v30','v60'),c('Y0','Y0Y1','Y0Y2','Y1','Y2','Y5'),b)
  name<-paste0(a,"_",b,
               ifelse(any(grepl('Peanut.Ara',colnames(fit$trainingData),fixed=T)),"wComp",""))
  db.test<-na.omit(testing.data[,c(gsub(' ~ .','',as.character(fit$call)[2]),
                                   colnames(fit$trainingData)[-1])])
  sink.file<-file(paste0(dir.pred,fit$method,'.',name,".ConfusionOutputs.txt"),'w')
  sink(sink.file,type="output")
  print('Model Search')
  print.train(fit)
  print('')
  print('Final Model')
  print(fit$finalModel$confusion)
  print(fit$finalModel$importance)
  print('')
  print('Stats training data')
  conf.train<-caret::confusionMatrix(predict.train(fit,newdata=fit$trainingData),
                                     fit$trainingData$.outcome,dnn = c("Prediction", "Reference"))
  print(conf.train)
  print('')
  print('Stats testing data')
  conf.test<-caret::confusionMatrix(predict(fit,newdata=db.test),
                                    db.test[,gsub(' ~ .','',as.character(fit$call)[2])],
                                    dnn = c("Prediction", "Reference"))
  print(conf.test)
  sink()
  close(sink.file)
}
