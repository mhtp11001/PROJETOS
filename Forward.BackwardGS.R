#Desenvolveremos dois procedimentos aqui, no primeiro tiraremos variaveis ate que
#a soma dos residuos chegue ao minimo e no segundo acrescentaremos variaveis
#ate que essa soma tambem chegue ao minimo e compararemos os modelos obtidos


rm(list=ls())

load('R/datasets/DATASET1.RData')

dadosrecorte<-dados
variaveis<-matrix(rep(0,31*32),ncol=32)
lm.fit=lm(Y~., data=dadosrecorte)
menor<-sum(abs(residuals(lm.fit)))
escolhidos<-rep(1,31)
g<-1
h<-2
erros<-c(menor)
variaveis[1,]<-c(escolhidos,erros[1])
while(g==1){
  resultado<-c()
  vetor<-which(escolhidos==1)
  tam<-length(vetor)
  for(i in 1:tam){
    vesc<-c()
    escolhatemp<-escolhidos
    escolhatemp[vetor[i]]<-0
    vesc<-which(escolhatemp==1)
    dadosaux<-as.data.frame(dadosrecorte[,c(1,(vesc+1))])
    lm.fit=lm(Y~., data=dadosaux)
    resultado[i]<-sum(abs(residuals(lm.fit)))
  }
  menormodelo<-which.min(resultado)
  ifelse(length(menormodelo)>1,menormodelo<-sample(menormodelo,1),menormodelo<-menormodelo)
  menorvalor<-resultado[menormodelo]
  if(menor<menorvalor){
    g<-0
  }
  if(menor>=menorvalor){
    g<-1
    escolhatemp<-escolhidos
    escolhatemp[vetor[menormodelo]]<-0
    escolhidos<-escolhatemp
    menor<-menorvalor
  }
  erros[h]<-menor
  variaveis[h,]<-c(escolhidos,erros[h])
  h<-h+1
  }
  
  

#Acrescentando variaveis

ybar<-mean(dadosrecorte$Y)
menor<-sum(abs(dadosrecorte$Y-ybar))

#Qualquer coisa eu posso inventar um erro grande para que ele iniciaze o algoritmo
#em caso do algoritmo nao inicializar


escolhidos<-rep(0,31)
g<-1
h<-2
erros1<-c(menor)
variaveis1<-matrix(rep(0,32*32),ncol=32)

variaveis1[1,]<-c(escolhidos,erros1[1])
while(g==1){
  resultado<-c()
  vetor<-which(escolhidos==0)
  tam<-length(vetor)
  for(i in 1:tam){
    vesc<-c()
    escolhatemp<-escolhidos
    escolhatemp[vetor[i]]<-1
    vesc<-which(escolhatemp==1)
    dadosaux<-as.data.frame(dadosrecorte[,c(1,(vesc+1))])
    lm.fit=lm(Y~., data=dadosaux)
    resultado[i]<-sum(abs(residuals(lm.fit)))
  }
  menormodelo<-which.min(resultado)
  ifelse(length(menormodelo)>1,menormodelo<-sample(menormodelo,1),menormodelo<-menormodelo)
  menorvalor<-resultado[menormodelo]
  if(menor<menorvalor){
    g<-0
  }
  if(menor>=menorvalor){
    g<-1
    escolhatemp<-escolhidos
    escolhatemp[vetor[menormodelo]]<-1
    escolhidos<-escolhatemp
    menor<-menorvalor
  }
  erros1[h]<-menor
  variaveis1[h,]<-c(escolhidos,erros1[h])
  h<-h+1
}


  