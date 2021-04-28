
##################################################################################################
source("/home/bd/Dropbox/projects/binaryprediction/src/00_bigfun.R")
load(file="/home/bd/Dropbox/projects/binaryprediction/data/df.Rdata")

results<-list()

fm0<-formula("outcome~factor(wave)+age*raracem*ragender+raedyrs+wealth")
fm1L<-list(formula("outcome~factor(wave)+age*raracem*ragender+raedyrs+wealth+cog"),formula("outcome~factor(wave)+age*raracem*ragender+raedyrs+wealth+grip+gait"))

df->hold
results.out<-list()
for (iii in 1:length(fm1L)) {
    hold->df
    fm1<-fm1L[[iii]]
    vars<-all.vars(fm1)[-1]
    tmp<-df[,vars]
    df<-df[rowSums(is.na(tmp))==0,]
    ## 1. how well can cognitive functioning predict death, nursing home admission, dementia dx in next 2y?
    for (st in c("proxy","attrit","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$outcome<-ifelse(tmp$status==st,1,0)
        results[[paste(st,"",sep='')]]<-bigfun(tmp,fm1=fm0,fm2=fm1)
    }
    ##older respondents
    for (st in c("proxy","attrit","dead")) {
        tmp<-df[df$status %in% c(st,"eligible"),]
        tmp$outcome<-ifelse(tmp$status==st,1,0)
        results[[paste(st,".75+",sep='')]]<-bigfun(tmp[tmp$age>=75,],fm1=fm0,fm2=fm1)
    }
    ##nursing home
    ## df$nhome<-ifelse(df$nhome=='1.yes',1,0)
    ## tmp<-df[,c("hhidpn","wave","nhome")]
    ## tmp$wave<-tmp$wave-1
    ## df2<-df
    ## NULL->df2$nhome
    ## df2<-merge(df2,tmp)
    ## df2$outcome<-df2$nhome
    ## results[["nhome.75+"]]<-bigfun(df2[df2$age>=75,],fm1=fm0,fm2=fm1)
    ## 3. how well does education predict the full range of health outcomes in the hrs (the self-reported variables)? [look at 0-> 1 between waves]
    conds<-c("hibp","diab","cancr","lung","heart","strok","psych","arthr")
    for (nm.tmp in conds) {
        nm<-paste(nm.tmp,".next",sep="")
        df$outcome<-df[[nm]]
        print(table(df$outcome))
        results[[nm.tmp]]<-bigfun(df,fm1=fm0,fm2=fm1)
    }
    results.out[[iii]]<-results
}

pdf("/home/bd/Dropbox/Apps/Overleaf/BinaryPrediction/nice_predictors.pdf",width=5,height=2.5)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
for (i in 1:length(results.out)) {
    results<- results.out[[i]]
    mat<-do.call("rbind",results)
    r00<-do.call("rbind",results00)
    tmp<-merge(r00[,4,drop=FALSE],mat[,4,drop=FALSE],by=0)
    plot(tmp[,-1],type='n',xlab="E(W), baseline",xlim=c(0,.04),ylim=c(0,.04),ylab=ifelse(i==1,"E(W), cognition","E(W), grip+gait"))
    text(tmp[,2],tmp[,3],tmp[,1],cex=.85)
}
dev.off()
