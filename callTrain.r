#' Call training functions delineated by model type and metric of interest.
#'
#' @param m desired model
#' @param trainControl list object outputted by caret::trainControl
#' @param metric metric of interest




callTrain<-function(m,data,trainControl,metric='AUC',ntree=10,nbagg=10,preProcess=NULL){
  if (m=='rf'){
    set.seed(463253329)
    fit<-caret::train(Response~.,data=data, method=m, trControl=trainControl,
                      ntree=ntree,metric=metric,tuneLength=10,preProcess)
  } else {
    if (m=='treebag')  {
      set.seed(463253329)
      fit<-caret::train(Response~.,data=data, method=m, trControl=trainControl,
                        nbagg=nbagg,metric=metric,preProcess) 
    } else {
      set.seed(463253329)
      fit<-caret::train(Response~.,data=data, method=m, trControl=trainControl,
                        tuneLength=10,metric=metric,preProcess)
    }
  }
  return(fit)
}