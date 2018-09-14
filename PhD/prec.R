## Metis precipitation and cluster data has been used to calculate the prec composites
#####################################################################

rm(list=ls())
#setwd("/project/mjo/samini/ERAI_T42/clusters/DJF/5day_means/1980_2014/PNA/PREC/")

#####################################################################
## PARAMETERS
#####################################################################
nv       = 7
nc       = 5   #### this can change depend on how many partition we need
nc.t     = 5
nx.t     = 128
ny.t     = 64
nx       = 55
ny       = 23
nt       = 86
nt.c     = 88
nyear    = 30
nens     = 12
nind     = 3
t.total  = (nt*nyear*nens)
ns       = 1000 #number of simulation
iensbeg  = 12
#####################################################################
## IMPORT Index 
#####################################################################
setwd(paste("/project/sphinx/samini/Metis/clusters/DJF/5day_means/1986-2015/PNA/Z500U250/centroids/ensbeg",sprintf("%02d",iensbeg),"/",sep=""))
fname1 = "cls_ind_Z500U250_PNA_12pcs_full.dat"
file2read = file(fname1,'rb')
all.dat = readBin(file2read,'numeric',n=nv*nt.c*nyear*nens*nind,size='4',endian='little')
dim(all.dat)=c(nv,nt.c,nyear,nens,nind)
close(file2read)

index=all.dat[2:6,1:nt,1:nyear,,1]
dim(index) = c(nc.t,nt,nyear,nens)

#####################################################################
## IMPORT Prec
#####################################################################

setwd("/project/sphinx/samini/Metis/clusters/DJF/5day_means/1986-2015/PNA/Z500U250/precipitation/data/DJF/")
var = array(NA,dim = c(nx,ny,nt,nyear,nens))
ecount = 1
for (ie in iensbeg:(iensbeg+11)){
  for (iy in 1986:2015){
    fname2 = paste("ICMGGgm8w_",iy,"110100_ensemble_",sprintf("%02d",ie),".dat",sep="")
    file2read = file(fname2,'rb')
    var.sub   = readBin(file2read,'numeric',n=nx.t*ny.t*nt,size='4',endian='little')
    dim(var.sub) = c(nx.t,ny.t,nt)
    var[,,,(iy-1985),ecount] = var.sub[54:108,39:61,]
    close(file2read)
  }
  ecount = ecount + 1
}

#####################################################################
#####################################################################
setwd("/project/sphinx/samini/Metis/clusters/DJF/5day_means/1986-2015/PNA/Z500U250/precipitation/")

vt         = nyear*nens
dim(index) = c(nc.t,nt,vt)
dim(var)   = c(nx,ny,nt,vt)

source("shift.mean.R")
l.shift.mean.var = shift.mean(t.total,nt,vt,nx,ny,index[(nc-1),,],var,nc,ns)
source("norm.shift.mean.R")
l.norm.shift.mean.var = norm.shift.mean(t.total,nt,vt,nx,ny,index[(nc-1),,],var,nc,ns)
source("ratio_bottom5.R")
l.var.ratio.b5 = ratio_bottom5(t.total,nt,vt,nx,ny,index[(nc-1),,],var,nc,ns)
source("ratio_top5.R")
l.var.ratio.t5 = ratio_top5(t.total,nt,vt,nx,ny,index[(nc-1),,],var,nc,ns)
#####################################################################
#####################################################################

#####################################################################
## make global data for each and save
#####################################################################
setwd("/project/sphinx/samini/Metis/clusters/DJF/5day_means/1986-2015/PNA/Z500U250/precipitation/output/")

if (ns==1){
  sub.name = paste("ensbeg",sprintf("%02d",iensbeg),"_Z500U250_PNA_DJF_T42_12pcs_full_nc",sep="")
}else{
  sub.name = paste("ensbeg",sprintf("%02d",iensbeg),"_conlev90_Z500U250_PNA_DJF_T42_12pcs_full_nc",sep="")
}
#####################################################################
## Shift
#####################################################################
ALL.DATA = array(NA, dim=c(nx.t,ny.t,nc))
for (i in 1:nc){
ALL.DATA[54:108,39:61,i] = l.shift.mean.var[[i]]
}
ALL.DATA = as.vector(ALL.DATA)
fname = paste("mean_shift_prec_",sub.name,nc,".dat",sep="")
to.write=file(fname,"wb")
writeBin(ALL.DATA,to.write,size='4',endian="little")
close(to.write)

#####################################################################
## Normalized Shift
#####################################################################
ALL.DATA = array(NA, dim=c(nx.t,ny.t,nc))
for (i in 1:nc){
  ALL.DATA[54:108,39:61,i] = l.norm.shift.mean.var[[i]]
}
ALL.DATA = as.vector(ALL.DATA)
fname = paste("mean_norshift_prec_",sub.name,nc,".dat",sep="")
to.write=file(fname,"wb")
writeBin(ALL.DATA,to.write,size='4',endian="little")
close(to.write)

#####################################################################
## RATIO top 5%
#####################################################################
ALL.DATA = array(NA, dim=c(nx.t,ny.t,nc))
for (i in 1:nc){
  ALL.DATA[54:108,39:61,i] = l.var.ratio.t5[[i]]
}
ALL.DATA = as.vector(ALL.DATA)
fname = paste("ratio_top5_prec_",sub.name,nc,".dat",sep="")
to.write=file(fname,"wb")
writeBin(ALL.DATA,to.write,size='4',endian="little")
close(to.write)

#####################################################################
## RATIO bottom 5%
#####################################################################
ALL.DATA = array(NA, dim=c(nx.t,ny.t,nc))
for (i in 1:nc){
  ALL.DATA[54:108,39:61,i] = l.var.ratio.b5[[i]]
}
ALL.DATA = as.vector(ALL.DATA)
fname = paste("ratio_bottom5_prec_",sub.name,nc,".dat",sep="")
to.write=file(fname,"wb")
writeBin(ALL.DATA,to.write,size='4',endian="little")
close(to.write)



















