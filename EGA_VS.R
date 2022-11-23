 library(ISLR)# Biblioteca onde os dados 'Credit' est�o.


#Este algoritmo se baseia na gera��o de uma popula��o aleat�ria de 
#configura��es de vari�veis. Tal popula��o � representada por uma lista
#bin�ria.

#A fun��o random_pop() gera uma popula��o bin�ria com n elementos, onde 
#cada indiv�duo da popula��o t�m nvar bits.
random_pop<-function(n,nvar){
  pop<-list()
  for (i in 1:n){
    pop[[i]]<-sample(0:1,nvar,replace = TRUE)
  }
  return(pop)
}

#A fun��o selection executa a etapa de sele��o dada uma popula��o.
selection<-function(pop,data){
  data<-as.data.frame(data)
  value<-c()
  w<-c()
  for(i in 1:length(pop)){
    if(sum(pop[[i]]!=0)){
      aux<-append(1,pop[[i]])
      ndata<-as.data.frame(data[,!!aux])
      model<-lm(Y~.,data=ndata)
      value[i]<-AIC(model)
    }
    else{
      model<-lm(Y~1,data=data)
      value[i]<-AIC(model)  }
  }
  minAIC<-(max(value)-value+1)
  w<-minAIC/sum(minAIC)
  chosen<-sample(1:length(pop),length(pop),prob =w,replace =TRUE)
  pop<-pop[chosen]
  return(pop)
}

#A fun��o exchange realiza a troca de genes entre dois indiv�duos de
#comprimento nvar no ponto de quebra breakp.
exchange<-function(v,w,breakp,nvar){
  a<-v[breakp+1:nvar]
  b<-w[breakp+1:nvar]
  v<-v[1:breakp]
  w<-w[1:breakp]
  v<-append(v,b)
  w<-append(w,a)
  v<-v[1:nvar]
  w<-w[1:nvar]
  return(list(v,w))
}

#A fun��o cross realiza o processo de cruzamento em uma subpopula��o 
#extra�da da popula��o pop.
cross<-function(pop,aux,nvar){
  if(length(aux)!=0){
    for(i in 1:length(aux)){
      if(i%%2!=0){
        breakp<-sample(1:nvar-1,1)
        nind<-exchange(aux[[i]],aux[[i+1]],breakp,nvar)
        pop<-append(pop,nind)
      }
    }
    return(pop)
  }
  return(pop)
}

#A fun��o crossover realiza todo o procedimento de cruzamento 
#na popula��o.
crossover<-function(pop,pc,n,nvar){
  vec<-rbinom(n,1,pc)
  aux<-pop[!!vec]
  pop<-pop[!vec]
  isodd<-ifelse((length(aux))%%2==0,FALSE,TRUE)
  if(isodd==TRUE){
    pop<-append(pop,aux[length(aux)])
    aux<-aux[-length(aux)]
    cross(pop,aux,nvar)
   
  }

  else{
   cross(pop,aux,nvar)
  
  }
}

#A fun��o mutation executa o procedimento de muta��o em uma popula��o 
mutation<-function(pop,pm,nvar){
  vec<-rbinom(nvar,1,pm)
  i<-1
  for(ind in pop){
    
    ind<-ifelse(ind==1,0,1)
    pop[[i]]<-ind
    i<-i+1
    
  }
  return(pop)
}

#A fun��o fittest seleciona em uma popula��o o indiv�duo com menor valor
#de AIC.
fittest<-function(pop,data){
  data<-as.data.frame(data)
  value<-c()
  for(i in 1:length(pop)){
    if(sum(pop[[i]]!=0)){
      aux<-append(1,pop[[i]])
      ndata<-as.data.frame(data[,!!aux])
      model<-lm(Y~.,data=ndata)
      value[i]<-AIC(model) 
    }
    
    else{
      model<-lm(Y~1,data=data)
      value[i]<-AIC(model)}
  }
  
  index<-which.min(value)
  best<-pop[index]
  return(list(best,value[index]))
}

#In�cio do algoritmo gen�tico elitista.
#O algoritmo retorna a configura��o de vari�veis que apresenta
#o menor valor de AIC encontrada at� a itera��o final do mesmo.
#A configura��o de vari�veis � definida um vetor bin�rio em que o bit 1 
#indica o pertencimento da vari�vel ao modelo e o bit 0 indica o contr�rio.

#data<-sapply(Credit,unclass)  #convertendo as vari�veis categ�ricas em num�ricas.

AGE<-function(data,n,pc,pm,iter){
 
 nvar<-ncol(data[,-1])
 it<-1

 while(it<iter){
   if(it==1){
     pop<-random_pop(n,nvar)
     best<-fittest(pop,data)
   }
   s<-selection(pop,data)
   c<-crossover(s,pc,n,nvar)
   m<-mutation(c,pm,nvar)
   pop<-m
   nbest<-fittest(pop,data)
   if(nbest[[2]]< best[[2]]){
     best<-nbest
   }
   it<-it+1
 }
 
 return(list(best[[1]],best[[2]]))
}


