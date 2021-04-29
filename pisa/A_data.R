##Borrowing from https://github.com/ben-domingue/rt_meta/blob/master/data/pisa2018.R
##which took data from:
##https://www.oecd.org/pisa/data/2018database/
##cognitive item data file, https://webfs.oecd.org/pisa2018/SPSS_STU_COG.zip

##That used a bigger sample. I just want 10k
load("/home/bd/Dropbox/projects/rt_meta/data/1_raw_main/raw_pisa2018math.Rdata")
ids<-sample(unique(x$id),10000)
x<-x[x$id %in% ids,]
x$rt<-NULL

save(x,file="pisa.Rdata")
