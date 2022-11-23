library(ISLR)

data<-Credit

metric<-function(x,y){
  return(sqrt(sum((x-y)^2)))
}

KNN<-function(data,z,k,t){
  ndata<-sapply(data, unclass)
  l<-nrow(ndata)
  dist<-c()
  for(i in 1:l){
    dist[i]<-metric(z,ndata[i,-11])
  }
  indexes<-order(dist)
  
  if(t==1){
    Classes<-levels(data[,11])
    chosen<-data[indexes[1:k],]
    v<-c()
    for(i in 1:length(Classes)){
      w<-which(chosen[,11]==Classes[i])
      v[i]<-length(w)
    }
    ind<-which.max(v)
    label<-Classes[ind]
    return(label)
    }
  
  else{
    chosen<-ndata[indexes[1:k],]
    label<-sum(chosen[indexes[1:k],11])/k
    return(label)
  }
}


# Usando o KNN para estimar uma função desconhecida com base nos dados 
# gerados por ela. Usaremos os 10 vizinhos mais próximos e plotaremos a 
# função estimada e a função verdadeira para compararmos a qualidade das
# predições realizadas

rm(list=ls())

x<-runif(50,0,1)
y<-x^2

plot(x,y)

#KNN K vizinhos mais proximos

k<-10

auxx<-sort(x)
auxy<-auxx^2

coordx<-seq(0,1,0.05)
coordy<-c()
n<-length(coordx)

for(i in 1:n){
distaux<-c()
diferenca<-abs(coordx[i]-auxx)
diford<-sort(diferenca)
qmen<-diford[1:k]
idqmen<-which(diferenca<=qmen[k])
vetor<-auxx[idqmen]
coordy[i]<-mean(vetor^2)
}

lines(coordx,coordy,'l',col='red')

fdados<-function(aux,k){
  coordx<-aux
  coordy<-c()
  n<-length(coordx)
  
  for(i in 1:n){
    distaux<-c()
    diferenca<-abs(coordx[i]-aux)
    diford<-sort(diferenca)
    qmen<-diford[1:k]
    idqmen<-which(diferenca<=qmen[k])
    vetor<-aux[idqmen]
    coordy[i]<-mean(vetor^2)
  }
  return(coordy)
}

ferro<-function(y,y1){
  aux<-(y-y1)^2
  aux1<-mean(aux)
  return(aux1)
}



