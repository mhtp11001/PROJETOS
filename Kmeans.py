# Este algoritmo é uma versão personalizada do algoritmo Kmeans, 
# conhecido algoritmo de aprendizagem não-supervisionada usado
# para clusterização de dados. Nesta versão, implemento tal algoritmo 
# do zero, sem o uso de funções e métodos presentes em bibliotecas 
# do python especializadas em tarefas de machine learning e ciência de
# dados. 

# Carregando bibliotecas a serem usadas
import random
import sklearn.datasets # Será usada para carregar dados para testar o algoritmo
import math

# Carregando dados para serem usados após a construção do algoritmo
data, target = sklearn.datasets.load_iris(return_X_y=True, as_frame=True)
data["target"] = target

# Definindo uma função para ser usada como métrica no processo de 
# aprendizagem do algoritmo. Nossa métrica será a distância euclidiana
def Euclid(x,y):
 dist=[(a-b)**2 for a,b in zip(list(x),list(y))]
 dist=math.sqrt(sum(dist))
 return(dist)


# Definindo uma função para calcular os centros dos clusters a serem 
# no processo de aprendizagem do algoritmo
def Centers(data,k,Clustering,it):
 Centroids=[]
 if(it==0): 
  for i in range(k):
    l=len(data)
    center=random.randint(0,l)
    row=list(data.loc[center])
    Centroids.append(row)
  return(Centroids) 

 else:
   for i in range(k):
     aux=[]
     for j in range(len(data)):
        aux.append(Clustering[j]==i)
     ndata=data[aux]
     Sumlist=[0]*len(ndata.columns)
     for ic in range(len(ndata)):
       Sumlist=[sum(x) for x in zip(Sumlist, list(data.loc[ic]))]
     for j in range(len(Sumlist)):
       Sumlist[j]=Sumlist[j]/len(Sumlist)
     Centroids.append(Sumlist)
   return(Centroids)



# Definindo uma função para determinar os clusters nos dados baseado
# nos centros obtidos na função 'Centers'
def Clusters(data,Centroids):
    Clustering=[]
    for i in range(len(data)):
      vec=[]
      for center in Centroids:
        dist= Euclid(data.loc[i],center)
        vec.append(dist)
      pos=vec.index(min(vec))
      Clustering.append(pos)
    return(Clustering)

# Essa função será a parte mais importante do algoritmo. Ela executará
# o processo de aprendizagem não-supervisionada nos dados com base no
# número de clusters desejados pelo usuário e o número de iterações
# fixada
def train_clusters(data,itmax,k):
  it=0  
  Clustering=[]
  while(it<itmax):
     Centroids=Centers(data,k,Clustering,it) 
     Clustering=Clusters(data,Centroids)
     it=it+1
  return(Centroids)
  
# Essa função será usada após a execução da função 'train_clusters'.
# A função 'Predict' será usada para classificar/prever em qual cluster
# um dado novo, não presente nos dados de treinamento será designado.
# A função 'Predict' é usada após a etapa de treinamento dos dados 
# executada pela função 'train_clusters' pois esta última calcula os
# parâmetros do algoritmo Kmeans e com base nesses parâmetros calculados  
# podemos prever em qual clusters será designado dados novos, ainda não 
# processados pelo algoritmo
def Predict(newdata,Centroids):
    Predictions=[]
    for data in newdata:
      vec=[]
      for center in Centroids:
        dist= Euclid(data,center)
        vec.append(dist)
      pos=vec.index(min(vec))
      Predictions.append(pos)
    return(Predictions)        
