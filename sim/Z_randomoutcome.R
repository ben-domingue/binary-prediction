## source("/home/bd/Dropbox/projects/hrs/cognition_attrition/src/00_bigfun.R")
## load(file="/home/bd/Dropbox/projects/hrs/cognition_attrition/wd/df.Rdata")

## ########################################################################################
## ##baseline
## maketab<-function(df) {
##     L<-list()
##     df$status<-rbinom(nrow(df),1,.05)
##     L$rando<-bigfun(df,allmodels=FALSE)
##     tmp<-data.frame(do.call("rbind",L))
##     tab<-proc(tmp) #zz<-lapply(L,proc) #no extenion: apm quantites, .1=p^m, .2=win
##     tab$van.1-tab$base.1 -> tab$delta
##     tab2<-tab[,c("base.1","van","delta","van.2")]
##     tab2
## }
## tab<-maketab(df)
## tabL<-list()
## for (i in 1:10) {
##     print(i)
##     index<-sample(1:nrow(df),nrow(df),replace=TRUE)
##     tabL[[i]]<-maketab(df[index,])
## }
## nm<-grep("van.2",names(tab),fixed=TRUE)
## L<-lapply(tabL,function(x) x[,nm])
## est<-do.call("rbind",L)
## est<-apply(est,2,quantile,c(.025,.975))
## tab2<-cbind(tab,t(est))
