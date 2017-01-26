# These functions are from Rand R. Wilcox's Rallfun-v32.txt
# URL: https://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v32.txt
# Accessed: Jan. 20, 2017

winvar<-function(x,tr=.2,na.rm=FALSE,STAND=NULL){
  #
  #  Compute the gamma Winsorized variance for the data in the vector x.
  #  tr is the amount of Winsorization which defaults to .2.
  #
  if(na.rm)x<-x[!is.na(x)]
  y<-sort(x)
  n<-length(x)
  ibot<-floor(tr*n)+1
  itop<-length(x)-ibot+1
  xbot<-y[ibot]
  xtop<-y[itop]
  y<-ifelse(y<=xbot,xbot,y)
  y<-ifelse(y>=xtop,xtop,y)
  winvar<-var(y)
  winvar
}

trimse<-function(x,tr=.2,na.rm=FALSE){
  #
  #  Estimate the standard error of the gamma trimmed mean
  #  The default amount of trimming is tr=.2.
  #
  if(na.rm)x<-x[!is.na(x)]
  trimse<-sqrt(winvar(x,tr))/((1-2*tr)*sqrt(length(x)))
  trimse
}

yuenbt<-function(x,y,tr=.2,alpha=.05,nboot=599,side=TRUE,nullval=0,pr=TRUE,
                 plotit=FALSE,op=1,SEED=TRUE){
  #
  #  Compute a 1-alpha confidence interval for the difference between
  #  the trimmed means corresponding to two independent groups.
  #  The bootstrap-t method is used.
  #
  #  The default amount of trimming is tr=.2
  #  side=T indicates two-sided method using absolute value of the
  #  test statistics within the bootstrap; otherwise the equal-tailed method
  #  is used.
  #
  #  This function uses trimse.
  #
  side<-as.logical(side)
  p.value<-NA
  yuenbt<-vector(mode="numeric",length=2)
  if(SEED)set.seed(2) # set seed of random number generator so that
  #             results can be duplicated.
  x<-x[!is.na(x)]  # Remove missing values in x
  y<-y[!is.na(y)]  # Remove missing values in y
  xcen<-x-mean(x,tr)
  ycen<-y-mean(y,tr)
  if(!side){
    if(pr)print("NOTE: p-value computed only when side=T")
  }
  test<-(mean(x,tr)-mean(y,tr))/sqrt(trimse(x,tr=tr)^2+trimse(y,tr=tr)^2)
  datax<-matrix(sample(xcen,size=length(x)*nboot,replace=TRUE),nrow=nboot)
  datay<-matrix(sample(ycen,size=length(y)*nboot,replace=TRUE),nrow=nboot)
  top<-apply(datax,1,mean,tr)-apply(datay,1,mean,tr)
  botx<-apply(datax,1,trimse,tr)
  boty<-apply(datay,1,trimse,tr)
  tval<-top/sqrt(botx^2+boty^2)
  if(plotit){
    if(op == 1)
      akerd(tval)
    if(op == 2)
      rdplot(tval)
  }
  if(side)tval<-abs(tval)
  tval<-sort(tval)
  icrit<-floor((1-alpha)*nboot+.5)
  ibot<-floor(alpha*nboot/2+.5)
  itop<-floor((1-alpha/2)*nboot+.5)
  se<-sqrt((trimse(x,tr))^2+(trimse(y,tr))^2)
  yuenbt[1]<-mean(x,tr)-mean(y,tr)-tval[itop]*se
  yuenbt[2]<-mean(x,tr)-mean(y,tr)-tval[ibot]*se
  if(side){
    yuenbt[1]<-mean(x,tr)-mean(y,tr)-tval[icrit]*se
    yuenbt[2]<-mean(x,tr)-mean(y,tr)+tval[icrit]*se
    p.value<-(sum(abs(test)<=abs(tval)))/nboot
  }
  list(ci=yuenbt,test.stat=test,p.value=p.value,est.1=mean(x,tr),est.2=mean(y,tr),est.dif=mean(x,tr)-mean(y,tr),
       n1=length(x),n2=length(y))
}