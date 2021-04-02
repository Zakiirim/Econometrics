library(igraph)
library(dplyr)
library(FRACTION)

# check for aperiodicity and strong connectivity

convergence_check <- function(matrix){
  aper_flag <- F
  conect_flag <-F
  # make graph form matrix
  graph<-graph_from_adjacency_matrix(M2, mode=c("directed"), weighted=TRUE, diag=F, add.colnames=NULL, add.rownames=NA)
  #APERIODICITY
  # run bfs
  tmp <- bfs(graph, root=2, "out", dist =T, father = T)
  # get search tree
  h <- graph( rbind( tmp$father[tmp$order, na_ok=T],tmp$order)[,-1], directed=TRUE )
  # get non-tree edges
  f<-as.data.frame(get.edgelist(graph))
  b<-as.data.frame(get.edgelist(h))
  nT<-setdiff(f,b)
  # calculate val
  nT$val <- tmp$dist[nT$V1] - tmp$dist[nT$V2] +1
  # check gcd of the vals
  cur_g =nT$val[1]
  for(i in 2:nrow(nT)){
    
    if(gcd(nT$val[i],cur_g) == 1){
      print('Aperiodicity OK')
      aper_flag <- T
      break
    } 
    else{
      cur_g <- gcd(nT$val[i],cur_g)
    }
    
  }
  
  #CONECTIVITY
  if (vertex_connectivity(graph,checks = T) >0){
    print('Connectivity OK')
    conect_flag <-T
  }
  # OUTPUT
  if (conect_flag & aper_flag){
    return(TRUE)
  }
  else if (!conect_flag){
    print('Not connected')
    return(FALSE)
  }
  else if(!aper_flag){
    print(paste('Not aperiodic, period =',as.character(cur_g)))
    return(FALSE)
  }
}