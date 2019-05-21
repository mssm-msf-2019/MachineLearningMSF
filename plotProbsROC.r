

plotProbsROC<-function(fit,testing.data,dir.pred, pos.class){
  
  a<-paste(unique(stringr::str_extract(colnames(fit$trainingData)[-1],"IgE|IgG4|ARAH")),collapse="")
  a<-ifelse(a=="ARAH","comp",a)
  b<-paste(unique(stringr::str_extract(colnames(fit$trainingData)[-1],"(v1|v12|v30|v60|Comp)$")),collapse="")
  b<-mgsub(c('v1$','v1v12','v1v30','v12$','v30','v60'),c('Y0','Y0Y1','Y0Y2','Y1','Y2','Y5'),b)
  name<-paste0(a,"_",b,
               ifelse(any(grepl('Peanut.Ara',colnames(fit$trainingData),fixed=T)),"wComp",""))
  db.test<-na.omit(testing.data[,c(gsub(' ~ .','',as.character(fit$call)[2]),
                                   colnames(fit$trainingData)[-1])])
  
  conf.train<-caret::confusionMatrix(predict.train(fit,newdata=fit$trainingData),
                                     fit$trainingData$.outcome,dnn = c("Prediction", "Reference"))
  probs.train<-predict.train(fit,newdata=fit$trainingData,type='prob')
  out<-cbind(Outcome=fit$trainingData$.outcome,Prediction=predict(fit,newdata=fit$trainingData),probs.train)
  rownames(out)<-rownames(fit$trainingData)
  
  auc.train<-pROC::roc(out[,'Outcome'],out[,colnames(probs.train)[1]])
  
  db<-mutate(arrange(cbind(melt(cbind(Subject=rownames(out),out),id.vars=c('Outcome','Subject','Prediction')),
                           Ord=probs.train[,conf.train$positive]),
                     Outcome,Prediction,plyr::desc(Ord),Subject),
             Subject=factor(as.character(Subject),levels=unique(as.character(Subject))))
  
  
  p<-ggplot(db[db$variable==pos.class,],aes(x=Subject, y=value)) +
    geom_bar(stat='identity') + labs(y=paste0('Predicted probabilities of ',pos.class),title='Prediction Training') +
    scale_fill_manual(values=col.groups) +
    geom_text(aes(y=-0.025,label=substr(Outcome,1,1),colour=Outcome),size=2,show.legend = F) +
    scale_colour_manual(values=col.groups) +
    geom_text(aes(y=1.025,label=substr(Prediction,1,1),colour=Prediction),size=2,show.legend = F) +
    geom_hline(yintercept=0.5, color="navy", linetype=9) + coord_flip() + theme_bw() +
    theme(axis.text.y=element_text(size=3))
  
  
  ggsave(file=paste0(dir.pred,name,".",fit$method,'.PredictedProbs.Training.pdf'),plot=p,height=6,width=2) 
  ggsave(file=paste0(dir.pred,name,".",fit$method,'.PredictedProbs.Training.jpeg'),plot=p,height=6,width=2) 
  
  
  conf.test<-caret::confusionMatrix(predict(fit,newdata=db.test),
                                    db.test[,gsub(' ~ .','',as.character(fit$call)[2])],
                                    dnn = c("Prediction", "Reference"))
  probs.test<-predict.train(fit,newdata=db.test,type='prob')
  out.test<-cbind(Outcome=db.test[,gsub(' ~ .','',as.character(fit$call)[2])],
                  Prediction=predict(fit,newdata=db.test),probs.test)
  rownames(out.test)<-rownames(db.test)
  db.test<-mutate(arrange(cbind(melt(cbind(Subject=rownames(out.test),out.test),id.vars=c('Outcome','Subject','Prediction')),
                                Ord=probs.test[,conf.train$positive])
                          ,Outcome,Prediction,plyr::desc(Ord),Subject),
                  Subject=factor(as.character(Subject),levels=unique(as.character(Subject))))
  
  p<-ggplot(db.test[db.test$variable==pos.class,],aes(x=Subject, y=value)) +
    geom_bar(stat='identity') + labs(y=paste0('Predicted probabilities of ',pos.class),title='Prediction Testing') +
    scale_fill_manual(values=col.groups) +
    geom_text(aes(y=-0.025,label=substr(Outcome,1,1),colour=Outcome),size=1.5,show.legend = F) +
    scale_colour_manual(values=col.groups) +
    geom_text(aes(y=1.025,label=substr(Prediction,1,1),colour=Prediction),size=1.5,show.legend = F) +
    geom_hline(yintercept=0.5, color="navy", linetype=9) + coord_flip() + theme_bw() +
    theme(axis.text.y=element_text(size=4))
  ggsave(file=paste0(dir.pred,name,".",fit$method,'.PredictedProbs.Testing.pdf'),plot=p,height=2.5,width=2.25) 
  ggsave(file=paste0(dir.pred,name,".",fit$method,'.PredictedProbs.Testing.jpeg'),plot=p,height=2,width=2.25)
  
  # ROC for training/testing
  auc.train<-pROC::roc(out[,'Outcome'],out[,colnames(probs.train)[1]])
  auc.test<-pROC::roc(out.test[,'Outcome'],out.test[,colnames(probs.test)[1]])
  
  pdf(file=paste0(dir.pred,name,".",fit$method,".ROCcurve.pdf"),onefile=TRUE, height=5,width=5)
  plot( pROC::roc(out[,'Outcome'],out[,colnames(probs.train)[1]]), type = "S",col="lightsteelblue4",lty=1, main=paste0('ROC: ',fit$method))
  plot( pROC::roc(out.test[,'Outcome'],out.test[,colnames(probs.test)[1]]), type = "S",col="mediumvioletred",lty=1, add=T)
  text(x=0.25,y=0.1,cex=0.7,labels=paste("AUC Train/Test:", round(auc.train$auc[1],2)," / ",round(auc.test$auc[1],2),
                                         "\n","Accuracy Train: ", round(conf.train$overall["Accuracy"],2),
                                         "[",round(conf.train$overall["AccuracyLower"],2),",",round(conf.train$overall["AccuracyUpper"],2),"]",
                                         "\n","Accuracy Test: ", round(conf.test$overall["Accuracy"],2),
                                         "[",round(conf.test$overall["AccuracyLower"],2),",",round(conf.test$overall["AccuracyUpper"],2),"]"))
  legend(0.98,0.98,lty=c(1,1),c("Train","Test"),col=c("lightsteelblue4","mediumvioletred"),cex=0.7)
  dev.off()
}
