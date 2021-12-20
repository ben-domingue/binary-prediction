imv<-function(y,p1,p2) {
    ##
    ll<-function(x,p) {
        z<-log(p)*x+log(1-p)*(1-x)
        z<-sum(z)/length(x)
        exp(z)
    }    
    loglik1<-ll(y,p1)
    loglik2<-ll(y,p2)
    getcoins<-function(a) {
        f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
        nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
    }
    c1<-getcoins(loglik1)
    c2<-getcoins(loglik2)
    ew<-function(p1,p0) (p1-p0)/p0
    imv<-ew(c2,c1)
    imv
}
fun<-function(df) {
    gr<-sample(1:10,nrow(df),replace=TRUE)
    xx<-data.frame(y=df$y,df[,nms],gr=gr)
    fm<-paste(nms,collapse="+")
    fm<-as.formula(paste('y~1+',fm))
    fm0<-as.formula("y~1")
    rs<-rowSums(is.na(xx))
    xx<-xx[rs==0,]
    om0<-om1<-om<-numeric()
    for (g in 1:10) {
        z<-xx[xx$gr!=g,]
        m<-glm(fm,z,family="binomial")
        xx0<-xx[xx$gr==g,]
        y<-data.frame(p2=predict(m,xx0,type="response"),resp=xx0$y)
        m<-glm(fm0,z,family="binomial")
        y$p1<-predict(m,xx0,type="response")
        om[g]<-imv(y$resp,y$p1,y$p2)
        y0<-y[y$resp==0,]
        y1<-y[y$resp==1,]
        om0[g]<-imv(y0$resp,y0$p1,y0$p2)
        om1[g]<-imv(y1$resp,y1$p1,y1$p2)
    }
        mean(om)
}

x<-read.csv("facies_training_data.csv")
nms<-c("Depth","GR","ILD_log10","DeltaPHI","PHIND","PE","NM_M","RELPOS")
fac.adj<-list(2,c(1,3),2,5,c(4,6),c(5,7,8),c(6,8),c(6,7,9),c(7,8)) #see table 2 https://library.seg.org/doi/10.1190/tle35100906.1#_i2
tab<-list()
for (fac in 1:9) {
    df<-x
    df$y<-ifelse(df$Facies==fac,1,0)
    m0<-mean(df$y)
    oma<-fun(df)
    df<-df[df$Facies %in% c(fac,fac.adj[[fac]]),]
    omb<-fun(df)
    m1<-mean(df$y)
    tab[[fac]]<-c(m0,oma,m1,omb)
}

tab<-do.call("rbind",tab)

 ##       [,1]  [,2]  [,3]  [,4]
 ## [1,] 0.080 0.058 0.260 0.217
 ## [2,] 0.228 0.162 0.458 0.214
 ## [3,] 0.190 0.127 0.455 0.318
 ## [4,] 0.057 0.027 0.459 0.435
 ## [5,] 0.067 0.021 0.251 0.065
 ## [6,] 0.143 0.082 0.362 0.186
 ## [7,] 0.030 0.016 0.093 0.047
 ## [8,] 0.154 0.082 0.409 0.143
 ## [9,] 0.050 0.038 0.213 0.183

