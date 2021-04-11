library(igraph)
library(dplyr)
library(FRACTION)
library(tidyverse)

# check for aperiodicity and strong connectivity
show_graph<- function(W){
  G = graph_from_adjacency_matrix(W,mode = 'directed', weighted = TRUE)
  w<-as.factor(E(G)$weight)
  n<-length(levels(w))
  plot(G, edge.color= w, arrow.size=0.2, curved=T)
  
  
}

make_opinon_plot<-function(sym){
  t<-ncol(sym)-1
  matplot(t(sym), ylim = c(0,1.4), type = c("b"),pch=1,col = as.factor(sym[,t+1]))
         legend("topright", legend = unique(sym[,t+1]), col=1:length(levels(as.factor(sym[,t+1]))), pch=19, bty ="n")  #plot
  #
}

convergence_check <- function(matrix, shout = F){
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
      if(shout) print('Aperiodicity OK')
      aper_flag <- T
      break
    } 
    else{
      cur_g <- gcd(nT$val[i],cur_g)
    }
    
  }
  
  #CONECTIVITY
  if (vertex_connectivity(graph,checks = T) >0){
    if(shout) print('Connectivity OK')
    conect_flag <-T
  }
  # OUTPUT
  if (conect_flag & aper_flag){
    return(TRUE)
  }
  else if (!conect_flag){
    if(shout) print('Not connected')
    return(FALSE)
  }
  else if(!aper_flag){
    if(shout) print(paste('Not aperiodic, period =',as.character(cur_g)))
    return(FALSE)
  }
  
  
}

# simulating the process
# basic model
markov_chains_2 <-  function(M, x0, iter ){
  sym <- matrix(0, nrow = nrow(M), ncol =  iter )
  M_to_p_i <- M
  sym[,1] <- x0
  for(i in 2:iter) {
    
    sym[,i] <- M_to_p_i %*% x0 
    M_to_p_i <- M %*% M_to_p_i
  }
  return(sym)
}

update_rule_0 <- function(x,M,...){
  x_next <-M %*% x
  M_next <- M
  return(list('x_next' = x_next, 'matrix' = M_next))
}

simulate <-  function(M, x0, iter, rule, agent_names,...){
  #sym <- matrix(0, nrow = nrow(M), ncol =  iter )
  sym <- matrix(0, nrow = nrow(M), ncol =  iter )
  sym[,1] <- x0
  cur_M<-M
  for(i in 2:iter) {
    cur_x <- sym[,i-1]
    sym[,i] <- rule(cur_x,cur_M,i,...)[['x_next']]
    cur_M <- rule(cur_x,cur_M,i,...)[['matrix']]
  }
  sym<-cbind(sym,agent_names)
  return(sym)
}




