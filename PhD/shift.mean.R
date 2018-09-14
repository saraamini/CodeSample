#Function: shift in mean:
#########eval(parse(text = (paste("x", 4,6, sep = ""))))
shift.mean = function(t.total,nt,vt,nx,ny,index,var,nc,ns){
  dim(var)     = c(nx*ny,t.total)
  dim(index)   = c(t.total)
#  dim(syn.var) = c(nx*ny,t.total,ns)
  
  l.var = list()   # var15,var25,...
  
  for(i in 1:nc){
    l.var[[i]] = var[,which(index==i)]  
  }
  
  mean.var     = apply(var, c(1),  mean, na.rm=T )
  
  l.shift.mean.var = list()   # var15,var25,...
  for(i in 1:nc){
    l.shift.mean.var[[i]]= (apply(l.var[[i]],c(1),mean,na.rm=T ) - mean.var)
  }
  
  if (ns !=1){
    l.sig.shift = list()
    for(i in 1:nc){
      l.sig.shift[[i]] = array(0,dim=c(nx*ny))
    }

    for (k in 1:ns){
      source("syn.data.R")
      syn.var = syn.data(t.total,nt,vt,nx,ny,index,var,nc,ns)$syn.var  
      mean.syn.var = apply(syn.var, c(1),  mean, na.rm=T)
      l.shift.mean.syn.var = list()
    
      for (i in 1:nc){  
        l.shift.mean.syn.var[[i]] = (apply(syn.var[,which(index==i)],c(1),mean,na.rm=T ) - mean.syn.var)
 
        sig.count = array(0,dim=c(nx*ny))
        sig.count[abs(l.shift.mean.syn.var[[i]])>abs(l.shift.mean.var[[i]])] = 1
        l.sig.shift[[i]] = l.sig.shift[[i]] + sig.count  
      }
    }
    for (i in 1:nc){
      l.sig.shift[[i]] = l.sig.shift[[i]] / ns
      l.shift.mean.var[[i]][l.sig.shift[[i]]>0.1]=NA
    }
  
    return(l.shift.mean.var = l.shift.mean.var)
  }else{
    return(l.shift.mean.var = l.shift.mean.var)
  }
  
}


