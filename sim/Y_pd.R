## ent<-function(w) -1*(w*log(w)+(1-w)*log(1-w))
## pv<-seq(.5,1,by=.01)
## plot(pv,ent(pv),type='l')


## #f=del/p
## #df/dp

## ew<-function(del,p) del/p
## par(mgp=c(2,1,0))
## plot(NULL,xlim=c(.5,1),ylim=c(0,.2),bty='n',xlab="p",ylab="E(W)")
## pv<-seq(.5,1,by=.01)
## f<-colorRampPalette(c("blue","red"))
## cols<-f(10)
## del.list<-seq(.01,.1,by=.01)
## for (i in 1:length(del.list)) {
##     del<-del.list[i]
##     pv0<-pv[pv+del<1]
##     lines(pv0,ew(del,pv0),col=cols[i])
## }
## legend("topright",bty='n',fill=cols[c(1,10)],legend=del.list[c(1,10)],title=expression(delta))


par(mgp=c(2,1,0),mfrow=c(1,2),mar=c(3,3,1,1),oma=rep(.5,4))
f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
av<-seq(0.5,.99,by=.01)
y<-numeric()
for (i in 1:length(av)) nlminb(.5,f,lower=0.001,upper=.999,a=av[i])$par->y[i]
plot(av,y,type='l',bty='n',xlab=expression(A[p]),ylab='w',lwd=2)
mtext(side=3,adj=0,"A")

##
ew<-function(p1,p) (p1-p)/p
vw<-function(p1,p) ((1-p)/p)^2*p1+1-p1
plot(NULL,xlim=c(.5,1.1),ylim=c(0,.5),bty='n',xlab="w",ylab="",xaxt='n')
axis(side=1,at=seq(.5,1,by=.1))
pv<-seq(.5,1,by=.01)
f<-colorRampPalette(c("blue","red"))
cols<-f(10)
del.list<-seq(.01,.1,by=.01)
for (i in 1:length(del.list)) {
    del<-del.list[i]
    pv0<-pv[pv+del<1]
    p1<-pv0+del
    lines(pv0,ew(p1,pv0),col=cols[i],lwd=2)
    lines(pv0,ew(p1,pv0)/sqrt(vw(p1,pv0)),col=cols[i],lty=3)
}
legend("topright",bty='n',fill=cols[c(1,10)],legend=del.list[c(1,10)],title=expression(delta))
legend("topleft",bty='n',lty=c(1,3),c("E(W)","Z(W)"))
mtext(side=3,adj=0,"B")


