library(igraph)
library(dplyr)
M2<-matrix(c(1,0,1,0,0,0,
             1,0,0,0,0,0,
             0,0,0,1,1,0,
             0,1,0,0,0,0,
             0,0,0,0,0,1,
             0,0,0,1,0,0), 6, byrow=T)
#colnames(M2)<-c("A", "B", "C", "D", "E","F")
#row.names(M2)=colnames(M2)
M2
M3<-matrix(c(1,0,0,0,0,1,
             0,0,1,1,0,0,
             0,1,1,1,0,0,
             0,1,0,1,1,0,
             0,0,0,1,1,0,
             0,0,0,0,1,1), 6, byrow=T)
colnames(M3)<-c("A", "B", "C", "D", "E","F")
row.names(M3)=colnames(M3)
M3
graph<-graph_from_adjacency_matrix(M2, mode=c("directed"), weighted=NULL, diag=F, add.colnames=NULL, add.rownames=NA)
graph1<-graph_from_adjacency_matrix(M3, mode=c("directed"), weighted=NULL, diag=F, add.colnames=NULL, add.rownames=NA)

plot(graph)
plot(graph1)

all_simple_paths(graph,'B','D')
neighbors(graph,'B','out')
vertex_connectivity(graph,checks = T)

f.in <- function(graph, data, extra) {
  cat("in:", paste(collapse=", ", colnames(M2)[data['vid']+1]), data['dist'], "\n")
  FALSE
}
f.out <- function(graph, data, extra) {
  cat("out:", paste(collapse=", ",  colnames(M2)[data['vid']+1]), data['dist'], "\n")
  FALSE
}
bfs_data = list()
f <- function(graph, data, extra) {
  
  append(bfs_data,data)
  TRUE
}

tmp <- bfs(graph, root=2, "out", dist =T, rank=T,succ = T, pred = T, father = T)
h <- graph( rbind( tmp$father[tmp$order, na_ok=T],tmp$order)[,-1], directed=TRUE )
plot(h)
f<-as.data.frame(get.edgelist(graph))
b<-as.data.frame(get.edgelist(h))
nT<-setdiff(f,b)
nT$val <- tmp$dist[nT$V1] - tmp$dist[nT$V2] +1
cur_g =nT$val[1]
for(i in 2:nrow(nT)){
  
  if(gcd(nT$val[i],cur_g) == 1){
    print('Aperiodicity OK')
    break
  } 
  else{
    cur_g <- gcd(nT$val[i],cur_g)
  }
  
}
print(cur_g)
get.edge.ids(h,c(2,3))
