

#Esta fun��o divide o conjunto de dados 'set' em dois conjuntos baseado 
#na indicadora definida pelo �ndice da vari�vel 'var' e valor de 'theta'.
#Especificamente , os dados cujo valor na vari�vel 'var' s�o menores do 
# 'theta' v�o para o conjunto da esquerda e os dados cujo valor s�o
# maiores ou iguais a 'theta' v�o para o conjunto da direita .
split_data<-function(set, var ,theta){
  B<-which(set[,var]<theta)
  set_left<-set[B,]
  set_right<-set[-B,]
  res<-list(set_left,set_right)
  return(res)
  
}

#Esta fun��o determina a classifica��o da maioria dos elementos em 'set' .  
label_leaf<-function(set){
  v1<-which(set[,ncol(set)]==1)
  v2<-which(set[,ncol(set)]==0)
  l1<-length(v1)
  l2<-length(v2)
  if (l1>=l2){
    label<-1
  }
  else{
    label<-0
  }
  return(label)
}


#Esta fun��o gera o �ndice de gini de uma vari�vel considerando um dado valor real 'theta' .
#Em usos posteriores queremos minimizar esta fun��o .
cost_function<-function(data,var,theta){
  setr_y<-which((data[,var]< theta)&(data[,ncol(data)]==1))
  setr_n<-which((data[,var]< theta)&(data[,ncol(data)]==0))
  setl_y<-which((data[,var]>= theta)&(data[,ncol(data)]==1))
  setl_n<-which((data[,var]>= theta)&(data[,ncol(data)]==0))
  
  ry<-length(setr_y)
  rn<-length(setr_n)
  ly<-length(setl_y)
  ln<-length(setl_n)
  
  left_gini<-1-(ly/(ly+ln))^2 - (ln/(ly+ln))^2
  right_gini<-1-(ry/(ry+rn))^2 - (rn/(ry+rn))^2
  
  gini <- left_gini*((ly+ln)/(ly+ln+ry+rn)) + right_gini*((ry+rn)/(ly+ln+ry+rn))
  return(gini)
}


#Esta fun��o gera um valor real entre o segundo e o terceiro quantil de acordo com os valores do conjunto
#'set' na vari�vel 'vari�vel' .
thetaf<-function(set,variavel){
  a<-quantile(set[,variavel])[2]
  b<-quantile(set[,variavel])[3]
  t<-runif(1,a,b)
  return(t)
}




  
  #Fun��o para obter o melhor valor da vari�vel e o valor de 'theta' para 
  #gerar a parti��o do conjunto de dados 'set'.
  
  parameters<-function(set,variables){
    
    variables<-sort(variables) #Vari�veis ordenadas .
    gini_vector<-c()     #vetor onde os valores de gini de cada vari�vel ser�o armazenados .
    theta<-c() #vetor onde ser�o armazenados os valores de 'theta' gerados para cada vari�vel em'variables' .
              #(observe que a ordem das posi��es em 'theta' coincidem com a orem das vari�veis) .
    
    
    # No loop 'for' � gerado para cada vari�vel 'var' em variables 
    #um valor 'theta[var]' e um valor 'gini_vector[var]' os quais s�o 
    #s�o armazenados em vetores para uso posterior .
    for(var in variables) {
      theta[var]<-thetaf(set ,var)
      gini_vector[var]<-cost_function(set,var,theta[var])
      
    }
    #Selecionando uma vari�vel em 'variables' que gera a melhor divis�o 
    #do conjunto 'set' de acordo com a minimiza��o da 'cost_function' .
    x<-which.min(gini_vector)
    #caso tenha mais de uma vari�vel que minimiza a 'cost_function '
    # selecionamos uma .
    if (length(x)>1){
      x<-sample(x,1)
      
    }
   
   return(c(x,theta[x])) #como resultado da fun��o obtemos um vetor cujos
   #elementos s�o o �ndice  'x' da vari�vel em 'variables' que gera a melhor parti��o
  # de 'set' e o valor correspondente 'theta[x]' que define a fun��o 
  #indicadora para dividir 'set' de acordo com a fun��o 'split_data' .

  }
  
  prediction<-function(x,w,partition){
    if (x[,w[[1]][1]]< w[[1]][2]){
      if (x[,w[[2]][1]]< w[[2]][2]){
        if (x[,w[[4]][1]]< w[[4]][2]){
          if (x[,w[[8]][1]]< w[[8]][2]){
            if (x[,w[[16]][1]]< w[[16]][2]){
              label<-label_leaf(partition[[1]])
            }
            else{
              label<-label_leaf(partition[[2]])
            }
          }
          else{
            if (x[,w[[17]][1]]< w[[17]][2]){
              label<-label_leaf(partition[[3]])
            }
            else{
              label<-label_leaf(partition[[4]])
            }
          }
        }
        else{
          if (x[,w[[9]][1]]< w[[9]][2]){
            if (x[w[[18]][1]]< w[[18]][2]){
              label<-label_leaf(partition[[5]])
            }
            else{
              label<-label_leaf(partition[[6]])
            }
          }
          else{
            if (x[,w[[19]][1]]< w[[19]][2]){
              label<-label_leaf(partition[[7]])
            }
            else{
              label<-label_leaf(partition[[8]])
            }
          }
        }
      }
      
      else{
        if (x[,w[[5]][1]]< w[[5]][2]){
          if (x[,w[[10]][1]]< w[[10]][2]){
            if (x[,w[[20]][1]]< w[[20]][2]){
              label<-label_leaf(partition[[9]])
            }
            else{
              label<-label_leaf(partition[[10]])
            }
          }
          else{
            if (x[,w[[21]][1]]< w[[21]][2]){
              label<-label_leaf(partition[[11]])
            }
            else{
              label<-label_leaf(partition[[12]])
            }
          }
        }
        else{
          if (x[,w[[11]][1]]< w[[11]][2]){
            if (x[,w[[22]][1]]< w[[22]][2]){
              label<-label_leaf(partition[[13]])
            }
            else{
              label<-label_leaf(partition[[14]])
            }
          }
          else{
            if (x[,w[[23]][1]]< w[[23]][2]){
              label<-label_leaf(partition[[15]])
            }
            else{
              label<-label_leaf(partition[[16]])
            }
          }
        }
      }
    } 
    else{
      if (x[,w[[3]][1]]< w[[3]][2]){
        if (x[,w[[6]][1]]< w[[6]][2]){
          if (x[,w[[12]][1]]< w[[12]][2]){
            if (x[,w[[24]][1]]< w[[24]][2]){
              label<-label_leaf(partition[[17]])
            }
            else{
              label<-label_leaf(partition[[18]])
            }
          }
          else{
            if (x[,w[[25]][1]]< w[[25]][2]){
              label<-label_leaf(partition[[19]])
            }
            else{
              label<-label_leaf(partition[[20]])
            }
          }
        }
        else{
          if (x[,w[[13]][1]]< w[[13]][2]){
            if (x[,w[[26]][1]]< w[[26]][2]){
              label<-label_leaf(partition[[21]])
            }
            else{
              label<-label_leaf(partition[[22]])
            }
          }
          else{
            if (x[,w[[27]][1]]< w[[27]][2]){
              label<-label_leaf(partition[[23]])
            }
            else{
              label<-label_leaf(partition[[24]])
            }
          }
        }
      }
      else{
        if (x[,w[[7]][1]]< w[[7]][2]){
          if (x[,w[[14]][1]]< w[[14]][2]){
            if (x[,w[[28]][1]]< w[[28]][2]){
              label<-label_leaf(partition[[25]])
            }
            else{
              label<-label_leaf(partition[[26]])
            }
          }
          else{
            if (x[,w[[29]][1]]< w[[29]][2]){
              label<-label_leaf(partition[[27]])
            }
            else{
              label<-label_leaf(partition[[28]])
            }
          }
        }
        else{
          if (x[,w[[15]][1]]< w[[15]][2]){
            if (x[w[[30]][1]]< w[[30]][2]){
              label<-label_leaf(partition[[29]])
            }
            else{
              label<-label_leaf(partition[[30]])
            }
          }
          else{
            if (x[,w[[31]][1]]< w[[31]][2]){
              label<-label_leaf(partition[[31]])
            }
            else{
              label<-label_leaf(partition[[32]])
            }
          }
        }
      }
    }
    
    return(label)
  }
  
  
  
  
 
 
  
  #in�cio do programa 
  
  #Gerando os dados e extraindo do mesmo o conjunto de treinamento
  


decision_tree<-function(train){
  
  
  variables<-c(1,2,3,4) #conjunto de vari�veis que eu selecionei para testar ,
  #mas esse conjunto na pr�tica � gerado aleatoriamente dentre as vari�veis do conjunto de dados  .
  
   #Profundidade m�xima que a �rvore atingir� .
  count<-1 #contador usado para indexar o vetor 'w' definido abaixo .
  #Na pr�tica 'count' conta quantas parti��es eu criei at� chegar na terceira camada ( que ter� 8 conjuntos )
  set<-train
  w<-list()    #Lista criada para armazenar os valores '(x,theta[x])'das parti��es geradas abaixo .
  partition<-list()#Lista criada para armazenar a parti��o gerada em uma determinada camada .
  
  p<-parameters(set,variables)#Armazenando o vetor '(x,theta[x])' de vari�vel e theta para dividir o conjunto train (set) em dois conjuntos (X1 E X2) .
  
  w[[count]]<-p #Armazenando o vetor '(x,theta[x])' de vari�vel e theta referente a primeira divis�o do conjunto 'train' ( gerando X1 E X2) .
  partition<-split_data(set,p[[1]],p[[2]]) #Primeira parti��o do conjunto 'train' gerando X1 E X2 ( os conjuntos da primeira camada ).
  
  # o loop 'while' ser� usado para gerar as camadas 2 e 3 ( que � a parti��o final desejada ) .
 
    
   
  len<-length(partition) #Calculando quantos conjuntos existem na parti��o .
      #Atualiza��o da camada atual .
  count<-count+1  #Atualiza��o do n�mero de parti��es feitas at� o momento .
  aux<-list()   # Lista auxiliar criada para armazenar as parti��es feitas em cada conjunto da lista 'partition' .
    
    #No la�o 'for' computaremos para cada conjunto em 'partition' a parti��o do conjunto em dois novos conjuntos .
    #Na pr�tica , vamos pegar cada elemento da parti��o gerada na camada anterior e
    #particiona-lo em dois para criar os elementos da camada atual .
  for (j in 1:len) {
    p<-parameters(partition[[j]],variables)#Gerando a vari�vel e o theta para particionar o conjunto 'partition[[j]]'.
    w[[count]]<-p # Armazenando o vetor '(x,theta[x])' referente a vari�vel e ao theta que ser� usado na parti��o .
     
      
    aux[[j]]<-split_data(partition[[j]],p[[1]],p[[2]])# particionado o conjunto 'partition[[j]]'em dois de acordo com os par�metros do vetor p calculado acima .
     
    count<-count+1 # Atualizando o n�mero de parti��es feitas at� o momento .
      
    }
    #No final do 'for' temos a lista auxiliar 'aux' com as parti��es feitas em cada conjunto da parti��o anterior 'partition'.
    #A parti��o que vamos armazenar em 'partition' (que � a parti��o que desejamos obter ) � a reuni�o das listas armazenadas em 'aux'.
    
    
    
    #Apartir daqui s�o apenas algumas tentativas frustradas de fazer a uni�o das listas em 'aux' para obter  a parti�ao 'partition' desejada .
    #Tentei usar o la�o 'for' para fazer a uni�o das listas dois-a-dois .
   
    
    partition<-unique(c(aux[[1]],aux[[2]]))#Com essa linha de comando consigo obter a uni�o das listas em 'aux' obtendo a parti��o da camada 2 ( que � o desejado ).
      # O problema � que quando o la�o 'while' executar o loop final ( para obter a camada 3 ) teremos a lista 'aux' com quatro parti��es (cada qual com dois conjuntos ) obtidas no la�o 'for' :
      #aux[[1]] , aux[[2]] , aux[[3]] e aux[[4]] . Ent�o , para obtermos a parti��o final 'partition' temos que fazer a uni�o das parti��es em 'aux' .
   
    len<-length(partition)
     
    for (j in 1:len) {
      p<-parameters(partition[[j]],variables)#Gerando a vari�vel e o theta para particionar o conjunto 'partition[[j]]'.
      w[[count]]<-p # Armazenando o vetor '(x,theta[x])' referente a vari�vel e ao theta que ser� usado na parti��o .
       
       
      aux[[j]]<-split_data(partition[[j]],p[[1]],p[[2]])# particionado o conjunto 'partition[[j]]'em dois de acordo com os par�metros do vetor p calculado acima .
       
      count<-count+1 # Atualizando o n�mero de parti��es feitas at� o momento .
       
     }
     
     partition<-unique(c(aux[[1]],aux[[2]],aux[[3]],aux[[4]]))
     
     len<-length(partition)
     
     for (j in 1:len) {
       p<-parameters(partition[[j]],variables)#Gerando a vari�vel e o theta para particionar o conjunto 'partition[[j]]'.
       w[[count]]<-p # Armazenando o vetor '(x,theta[x])' referente a vari�vel e ao theta que ser� usado na parti��o .
       
       
       aux[[j]]<-split_data(partition[[j]],p[[1]],p[[2]])# particionado o conjunto 'partition[[j]]'em dois de acordo com os par�metros do vetor p calculado acima .
       
       count<-count+1 # Atualizando o n�mero de parti��es feitas at� o momento .
       
     }
     
     partition<-unique(c(aux[[1]],aux[[2]],aux[[3]],aux[[4]],aux[[5]],aux[[6]],aux[[7]],aux[[8]]))
     
     len<-length(partition)
     
     for (j in 1:len) {
       p<-parameters(partition[[j]],variables)#Gerando a vari�vel e o theta para particionar o conjunto 'partition[[j]]'.
       w[[count]]<-p # Armazenando o vetor '(x,theta[x])' referente a vari�vel e ao theta que ser� usado na parti��o .
       
       
       aux[[j]]<-split_data(partition[[j]],p[[1]],p[[2]])# particionando o conjunto 'partition[[j]]'em dois de acordo com os par�metros do vetor p calculado acima .
       
       count<-count+1 # Atualizando o n�mero de parti��es feitas at� o momento .
       
     }
     
     partition<-unique(c(aux[[1]],aux[[2]],aux[[3]],aux[[4]],aux[[5]],aux[[6]],aux[[7]],aux[[8]],aux[[9]],aux[[10]],aux[[11]],aux[[12]],aux[[13]],aux[[14]],aux[[15]],aux[[16]]))
    
    
    len<-length(partition)
    labelv<-c()
    
    for (j in 1:len) {
      labelv[j]<-label_leaf(partition[[j]])
    }
    
    return(list(w,partition))
    
}

data<-read.delim(file.choose(),sep=',',dec='.')
indices<-sample.int(nrow(data) , floor(0.7*nrow(data)), replace=FALSE)
train<-data[indices,] #Conjunto de treinamento 
test<-data[-indices,]

random_forest<-function(train,n){
  trees<-list()
  for (i in 1:n) {
    trees[[i]]<-decision_tree(train)
  }
  return(trees)
}


forest_prediction<-function(x,trees){
  n<-length(trees)
  label_vector<-c()
  for (i in 1:n){
    
    label_vector[i]<-prediction(x,trees[[i]][[1]],trees[[i]][[2]])
    
  }
  a<-which(label_vector==1)
  b<-which(label_vector==0)
  
  l1<-length(a)
  l2<-length(b)
  
  if (l1 < l2){
    label<-0
  }
  
  if(l1>=l2) {
    label<-1   }
  
  return(label)
}

error<-function(test,trees){
  n<-length(trees)
  v<-data.frame()
  
  for (i in 1:n) {  
    for (r in 1:nrow(test)) { 
       v[r,1]<-forest_prediction(test[r,-ncol(test)],trees)
    }
  }
  
  z<-which(v!=test[,ncol(test)])
  num<-length(z)
  error<-num/nrow(test)
  return(error)
  
  
}
    
    
    
