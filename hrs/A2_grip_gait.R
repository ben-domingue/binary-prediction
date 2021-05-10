read.dat.dct <- function(dat, dct, labels.included = "no") {
  #from http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r
  temp <- readLines(dct)
    temp <- temp[grepl("_column", temp)]
    switch(labels.included,
           yes = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
               classes <- c("numeric", "character", "character", "numeric", "character")
               N <- 5
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
           },
           no = {
               pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
               classes <- c("numeric", "character", "character", "numeric")
               N <- 4
               NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
           })
    metadata <- setNames(lapply(1:N, function(x) {
        out <- gsub(pattern, paste("\\", x, sep = ""), temp)
        out <- gsub("^\\s+|\\s+$", "", out)
        out <- gsub('\"', "", out, fixed = TRUE)
        class(out) <- classes[x] ; out }), NAMES)
    metadata[["ColName"]] <- make.names(gsub("\\s", "", metadata[["ColName"]]))
    myDF <- read.fwf(dat, widths = metadata[["ColWidth"]], 
             col.names = metadata[["ColName"]])
    if (labels.included == "yes") {
        attr(myDF, "col.label") <- metadata[["ColLabel"]]
    }
    myDF
}
#read.dat.dct(dat="H96PR_R.DA",dct="H96PR_R.DCT")->df

getvars<-function(tmp,letter,wave) {
    names(tmp)->nms
    gsub(paste("^",letter,sep=""),"J",nms)->nms
    nms->names(tmp)
    tmp[,c("JI816","JI851","JI852","JI853")]->grip
    for (i in 1:ncol(grip)) ifelse(grip[,i]>1000,NA,grip[,i])->grip[,i]
    apply(grip,1,max,na.rm=TRUE)->grip
    ifelse(!is.finite(grip),NA,grip)->grip
    #rowMeans(grip,na.rm=TRUE)->grip
    tmp[,c("JI823","JI824")]->gait
    for (i in 1:ncol(gait)) ifelse(gait[,i]>1000,NA,gait[,i])->gait[,i]
    rowMeans(gait,na.rm=TRUE)->gait
    ifelse(grip>80,NA,grip)->grip
    ifelse(gait>30,NA,gait)->gait
    data.frame(wave=wave,hhid=tmp$HHID,pn=tmp$PN,grip=grip,gait=gait)->tmp
}


L<-list()
##2004
read.dat.dct(dat="/hrsshare/public_release_data/2004/H04I_R.da",dct="/hrsshare/public_release_data/2004/H04I_R.dct")->tmp
getvars(tmp,letter="J",wave=7)->L[["2004"]]

##2006
read.dat.dct(dat="/hrsshare/public_release_data/2006/H06I_R.da",dct="/hrsshare/public_release_data/2006/H06I_R.dct")->tmp
getvars(tmp,letter="K",wave=8)->L[["2006"]]

##2008
read.dat.dct(dat="/hrsshare/public_release_data/2008/H08I_R.da",dct="/hrsshare/public_release_data/2008/H08I_R.dct")->tmp
getvars(tmp,letter="L",wave=9)->L[["2008"]]

##2010
read.dat.dct(dat="/hrsshare/public_release_data/2010/H10I_R.da",dct="/hrsshare/public_release_data/2010/H10I_R.dct")->tmp
getvars(tmp,letter="M",wave=10)->L[["2010"]]

##2012
read.dat.dct(dat="/hrsshare/public_release_data/2012/H12I_R.da",dct="/hrsshare/public_release_data/2012/H12I_R.dct")->tmp
getvars(tmp,letter="N",wave=11)->L[["2012"]]

##2014
read.dat.dct(dat="/hrsshare/public_release_data/2014/H14I_R.da",dct="/hrsshare/public_release_data/2014/H14I_R.dct")->tmp
getvars(tmp,letter="O",wave=12)->L[["2014"]]

do.call("rbind",L)->x
data.frame(x)->x
as.character(as.numeric(x$hhid)*1000+as.numeric(x$pn))->x$hhidpn
NULL->x$hhid
NULL->x$pn
x[!is.na(x$grip) | !is.na(x$gait),]->x

ifelse(x$gait<1,NA,x$gait)->x$gait
log(x$gait)->x$gait

save(x,file="~/hrs/grip_gait/grip_gait.Rdata")
