#' Calculates cross-validation statistics on resamples of a designated training set
#'
#' @param all.fits list of model objects as computed by caret::Train


get.CV.resamples<-function(all.fits,horiz=FALSE){
  #horiz=TRUE gives me the Kappa and accuracy in horizontal way, not stancked by rows
  n<- nrow(resamples(all.fits)$value)
  Stats.CV<-apply(summary(resamples(all.fits))$value,2,
                  function(x){c(m=mean(x,na.rm=T),sd=sd(x,na.rm=T),median=median(x,na.rm=T))})
  Stats.CV.db<-melt(Stats.CV)
  b<-t(sapply(as.character(Stats.CV.db$Var2),function(x){unlist(strsplit(x,'~',fixed=T))}))
  colnames(b)<-c('Model','Stat')
  rownames(b)<-1:nrow(b)
  Stats.CV.db<-cbind(Stats.CV.db,b)
  Stats.CV.db<-dcast(Stats.CV.db,Stat+Model~Var1)
  Stats.CV.db<-mutate(Stats.CV.db, LCI=m-1.98*(sd/sqrt(n)),UCI=m+1.98*(sd/sqrt(n)))
  Stats.CV.db$variables<-sapply(as.character(Stats.CV.db$Model),
                                function(x){unlist(strsplit(x,'.',fixed=T))[1]})
  if (horiz) {
    z<-subset(Stats.CV.db, Stat=='ROC',select=-Stat)
    colnames(z)[-c(1,ncol(z))]<-paste0(colnames(z)[-c(1,ncol(z))],".CV.ROC")
    Stats.CV.db1<-merge(subset(Stats.CV.db, Stat=='Sens',select=-Stat),
                        subset(Stats.CV.db, Stat=='Spec',select=-Stat),
                        by=c('Model','variables'),suffixes = c('.CV.Sense','.CV.Spec'))
    Stats.CV.db<-merge(z, Stats.CV.db1, by=c('Model','variables'))
  }
  
  return(Stats.CV.db)
}