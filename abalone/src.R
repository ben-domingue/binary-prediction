set.seed(1234)
df<-read.csv("abalone.data",header=TRUE)

	## Name		Data Type	Meas.	Description
	## ----		---------	-----	-----------
	## Sex		nominal			M, F, and I (infant)
	## Length		continuous	mm	Longest shell measurement
	## Diameter	continuous	mm	perpendicular to length
	## Height		continuous	mm	with meat in shell
	## Whole weight	continuous	grams	whole abalone
	## Shucked weight	continuous	grams	weight of meat
	## Viscera weight	continuous	grams	gut weight (after bleeding)
	## Shell weight	continuous	grams	after being dried
	## Rings		integer			+1.5 gives the age in years
names(df)<-c("sex","length","diam","ht","whole","shuck","viscera","shell","rings")
df$y<-ifelse(df$rings>median(df$rings),1,0)

nms<-c("sex","length","diam","ht","whole","shuck","viscera","shell")

z<-df[,nms]

gr<-sample(1:10,nrow(df),replace=TRUE)
xx<-data.frame(y=df$y,z,gr=gr)

fm<-paste(nms,collapse="+")

fm<-as.formula(paste('y~1+',fm))
fm0<-as.formula("y~1")

rs<-rowSums(is.na(xx))
xx<-xx[rs==0,]

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
mean(om)*1000
sd(om)


mean(om0)
mean(om1)

#> mean(om)
#[1] 0.67
