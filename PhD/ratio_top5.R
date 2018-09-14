#Function: shift in mean:
#########eval(parse(text = (paste("x", 4,6, sep = ""))))
ratio_top5 = function(t.total,nt,vt,nx,ny,index,var,nc,ns,alpha=0.05){
  dim(var)     = c(nx*ny,t.total)
  dim(index)   = c(t.total)
#  dim(syn.var) = c(nx*ny,t.total,ns)
  
  l.var = list()   # var15,var25,...
  
  for(i in 1:nc){
    l.var[[i]] = var[,which(index==i)]  
  }


  l.extreme = list()  

  for(i in 1:nc){
    l.extreme[[i]] = vector(length = (nx*ny))  
  }  
  
  quantile = apply(var, 1,  quantile, probs= c(1-alpha), na.rm=T )  ##dim=x*y

  l.clp = list() 
  for(i in 1:nc){
    l.clp[[i]] = dim(l.var[[i]])[2] 
  } 

  for (i in 1:(nx*ny)){
    for (k in 1:nc){
      l.extreme[[k]][i] = length(which(l.var[[k]][i,]>quantile[i]))
    }
  }

  l.var.ratio = list()  
  for (i in 1:nc){
    l.var.ratio[[i]] = l.extreme[[i]]/l.clp[[i]]/alpha 
  }
  if (ns!=1){
    syn.var.ratio = array(NA, dim=c(nx*ny,nc))
  
    l.syn.var         = list()
    l.syn.var.extreme = list() 
    l.syn.clp         = list()
    l.syn.var.extreme = list()

    l.sig.ratio = list()
    for(i in 1:nc){
      l.sig.ratio[[i]] = array(0,dim=c(nx*ny))
    }

    for (k in 1:ns){
        
      source("syn.data.R")
      syn.var = syn.data(t.total,nt,vt,nx,ny,index,var,nc,ns)$syn.var   
      syn.quantile = apply(syn.var, 1,  quantile, probs= c(1-alpha), na.rm=T )
      for (j in 1:nc){
        l.syn.var[[j]] = syn.var[,which(index==1)]
        l.syn.var.extreme[[j]] = vector(length = (nx*ny))      
        l.syn.clp[[j]] = dim(l.syn.var[[j]])[2]          
        for (i in 1:(nx*ny)){        
          l.syn.var.extreme[[j]][i] = length(which(l.syn.var[[j]][i,]>syn.quantile[i]))      
        }     
        syn.var.ratio[,j] = l.syn.var.extreme[[j]]/l.syn.clp[[j]]/alpha

        sig.count = array(0,dim=c(nx*ny))
        sig.count[abs(syn.var.ratio[,j])>abs(l.var.ratio[[j]])] = 1
        l.sig.ratio[[j]] = l.sig.ratio[[j]] + sig.count  
      }
    }
    for (i in 1:nc){
      l.sig.ratio[[i]] = l.sig.ratio[[i]] / ns    
      l.var.ratio[[i]][l.sig.ratio[[i]]>0.1]=NA
    }
  
    return(l.var.ratio = l.var.ratio)
  }else{
    return(l.var.ratio = l.var.ratio)
  }  
}


