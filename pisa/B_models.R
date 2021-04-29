load("pisa.Rdata")


predict<-function(x) {
    test<-FALSE
    while (!test) {
        index<-sample(1:nrow(x),round(nrow(x)/10))
        oos<-x[index,]
        ins<-1:nrow(x)
        ins<-ins[!(ins %in% index)]
        ins<-x[ins,]
        oid<-unique(oos$id)
        iid<-unique(ins$id)
        test<-length(iid)==length(unique(x$id))
    }
    x<-ins
    #make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id
    nr<-apply(resp,2,function(x) length(table(x)))
    resp<-resp[,nr>1]
    resp<-resp[rowSums(!is.na(resp))>1,]
    ##three models
    library(mirt)
    index<-grep("id",names(resp))
    ni<-ncol(resp)-1
    ##now throw away x
    rm("x")
    x<-oos
    ###################
    ##2pl
    s<-paste("F=1-",ni,"
            PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2)",sep="") #-1.5
    model<-mirt.model(s)
    m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000))
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],easy=co[,2],load=co[,1])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
    ##
    kk<-x$load*x$th+x$easy
    kk<-exp(kk)
    x$pv2<-kk/(1+kk)
    m2pl<-m
    x$easy<-x$th<-x$load<-NULL
    ## ###################
    ## ##3pl
    s<-paste("F=1-",ni,"
            PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", g, norm, -1.5, 1)",sep="") #-1.5
    model<-mirt.model(s)
    m<-mirt(resp[,-index],model,itemtype=rep("3PL",ni),method="EM",technical=list(NCYCLES=10000))->mirt.mod
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],easy=co[,2],load=co[,1],guess=co[,3])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th=th[,1])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
    ##
    kk<-x$load*x$th+x$easy
    kk<-exp(kk)
    x$pv3<-x$guess+(1-x$guess)*kk/(1+kk)
    m3pl<-m
    x$easy<-x$th<-x$load<-NULL
    ###################
    ##2pl 2f
    s<-paste("F1=1-",ni,",
F2=1-",ni,"
            PRIOR = (1-",ni,", a1, lnorm, 0.2, 0.2),(1-",ni,", a2, lnorm, 0.2, 0.2)",sep="") #-1.5
    model<-mirt.model(s)
    m<-mirt(resp[,-index],model,itemtype=rep("2PL",ni),method="EM",technical=list(NCYCLES=10000))
    co<-coef(m)
    co<-do.call("rbind",co[-length(co)])
    item<-data.frame(item=names(resp)[-index],easy=co[,3],load1=co[,1],load2=co[,2])
    ##
    th<-fscores(m)
    stud<-data.frame(id=resp$id,th1=th[,1],th2=th[,2])
    ##
    x<-merge(x,stud)
    x<-merge(x,item)
    ##
    kk<-x$load1*x$th1+x$load2*x$th2+x$easy
    kk<-exp(kk)
    x$pv2f<-kk/(1+kk)
    x$easy<-x$th<-x$load<-NULL
    x$guess<-x$easy<-x$th1<-x$load1<-x$th2<-x$load2<-NULL
    x
}
imv<-function(pr) {
    ll<-function(x,p='pv') {
        z<-log(x[[p]])*x$resp+log(1-x[[p]])*(1-x$resp)
        z<-sum(z)/nrow(x)
        exp(z)
    }    
    loglik2<-ll(pr,'pv2')
    loglik3<-ll(pr,'pv3')
    loglik2f<-ll(pr,'pv2f')
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c2<-getcoins(loglik2)
    c3<-getcoins(loglik3)
    c2f<-getcoins(loglik2f)
    ew<-function(p1,p0) (p1-p0)/p0
    imv1<-ew(c3,c2)
    imv2<-ew(c2f,c2)
    c(imv1,imv2)
}

ew<-list()
for (i in 1:10) {
    pr<-predict(x)
    ew[[i]]<-imv(pr)
}
colMeans(do.call("rbind",ew))
