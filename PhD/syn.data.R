#Synthetic dataset

syn.data = function(t.total,nt,vt,nx,ny,index,var,nc,ns){
  dim(var)   = c(nx*ny,nt*vt)
  dim(index) = c(nt,vt)
  
  syn.var = array(NA,dim=c(nx*ny,nt*vt))
  
#  for (k in 1:ns){
    synthetic.day = sample(1:t.total, t.total, replace=TRUE)
    dim(synthetic.day) = c(nt,vt)
    
    for (i in 1:vt){
      for (j in 2:nt){ 
        if (index[j,i]==index[(j-1),i]){
          if (synthetic.day[(j-1),i]!=t.total){
            synthetic.day[j,i] = (synthetic.day[(j-1),i]) + 1
          } else {
            synthetic.day[j,i] = 1
          }
          
        }
      }
    }
    
    dim(synthetic.day) = c(nt*vt)
    for (it in 1:t.total){
      syn.var[,it] = var[,synthetic.day[it]]
    }
      
#  }
  
  list(syn.var = syn.var)
}
  
  
