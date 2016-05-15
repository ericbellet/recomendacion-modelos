install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalo automaticamente los paquetes.
install('arules')
install('arulesViz')
install('FactoMineR')



#Cargo las librerias.
library(arules)
library(arulesViz)
library(FactoMineR)


##-------------------------------LECTURA Y ANALISIS-----------------------------------

setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
ejemplo <- read.csv("data/ejemplo.csv")
periodico <- read.csv("data/periodico.csv")

##------------------------------------------------------------------------------------
##----------------------------GENERACION DE ARTICULOS---------------------------------
'''
1. Modifcar su dataset de tal manera que no se lean los identificadores de los artículos
como itemN sino por su tipo de contenido contenido/articuloN. 
Ejemplo: {item1, item10, item81} 
es la transacción {deportes/articulo1, politica
/articulo1, opinion/articulo9}.
'''
#deportes 1-9
#politica 10-18
#variedades 19-27
#internacional 28-36
#nacionales 37-45
#sucesos 46-54
#comunidad 55-63
#negocios 64-72
#opinion 73-81
{item1, item8, item56, item61}
{item1,item28,item56,item61}
#------------------FUNCTION genArticles-----------------
genArticles <- function(articles){
  articulo <- ""
  for (i in 1:length(articles)) {
    
    if (as.integer(articles[i]) <= 9 & as.integer(articles[i]) >= 1){
      articulo <- paste(articulo, gsub(" ","",paste("deportes/articulo",articles[i])))
      
    }
    if (as.integer(articles[i]) <= 18 & as.integer(articles[i]) >= 10){
      articulo <- paste(articulo, gsub(" ","",paste("politica/articulo",(as.integer(articles[i])-9))))
      
    }
    if (as.integer(articles[i]) <= 27 & as.integer(articles[i]) >= 19){
      articulo <- paste(articulo, gsub(" ","",paste("variedades/articulo",(as.integer(articles[i])-18))))
      
    }
    if (as.integer(articles[i]) <= 36 & as.integer(articles[i]) >= 28){
      articulo <- paste(articulo, gsub(" ","",paste("internacional/articulo",(as.integer(articles[i])-27))))
      
    }
    if (as.integer(articles[i]) <= 45 & as.integer(articles[i]) >= 37){
      articulo <- paste(articulo, gsub(" ","",paste("nacionales/articulo",(as.integer(articles[i])-36))))
      
    }
    if (as.integer(articles[i]) <= 54 & as.integer(articles[i]) >= 46){
      articulo <- paste(articulo, gsub(" ","",paste("sucesos/articulo",(as.integer(articles[i])-45))))
      
    }
    if (as.integer(articles[i]) <= 63 & as.integer(articles[i]) >= 55){
      articulo <- paste(articulo, gsub(" ","",paste("comunidad/articulo",(as.integer(articles[i])-54))))
      
    }
    if (as.integer(articles[i]) <= 72 & as.integer(articles[i]) >= 64){
      articulo <- paste(articulo, gsub(" ","",paste("negocios/articulo",(as.integer(articles[i])-63))))
      
    }
    if (as.integer(articles[i]) <= 81 & as.integer(articles[i]) >= 73){
      articulo <- paste(articulo, gsub(" ","",paste("opinion/articulo",(as.integer(articles[i])-72))))
      
    }
  }
  return(articulo)
  
}
#--------------END FUNCTION genArticles-----------------
#------------------FUNCTION llenar-----------------
llenar <- function(periodico,fila){
  items <- as.numeric(unlist(strsplit(gsub("[{}item]","",unlist(periodico)), ",")))
  fila[items]=1
  return(fila)
}
#Generar la matriz de transacciones.
#fila es un row inicializado en 0.
fila <- matrix(data = 0, nrow = 1, ncol = 81)

#Lleno la matriz con 1 donde un usuario observo un articulo
matriz <- lapply(periodico$items, llenar,fila)

#Transformo matriz en una matrix.
matriz <- matrix(unlist(matriz), byrow=T, ncol=81)
#--------------END FUNCTION llenar-----------------

#Cambio el nombre de la columna para que tenga coherencia con el ejemplo dado.
colnames(periodico)[5] <- "items"
#Creo la columna de los articulos
periodico$articles <- periodico$items

#Se sabe que el portal ofrece 9 tipos de contenidos 
#y nos ofrecen solo información de 9 artículos.

#Obtengo el numero de los articulos.
periodico$articles <- strsplit(gsub("[{}item]","",periodico$articles), ",")

#Modifico el dataset con las condiciones dadas.
periodico$articles <- lapply(periodico$articles, genArticles)

#Convierto los espacios en ,
periodico$articles <- gsub(" ",",",periodico$articles)

#Elimino la primer valor del string.
periodico$articles <- substring(periodico$articles, 2)

#Calculo el tiempo totan el segundos que dura el usuario en la pagina.
periodico$tiempototal <- difftime(periodico$exit, periodico$entry, units =  "secs")

#Generar la matriz de transacciones.
#fila es un row inicializado en 0.
fila <- matrix(data = 0, nrow = 1, ncol = 81)

#Lleno la matriz con 1 donde un usuario observo un articulo
matriz <- lapply(periodico$items, llenar,fila)

#Transformo matriz en una matrix.
matriz <- matrix(unlist(matriz), byrow=T, ncol=81)

#Nombro las columnas
colnames(matriz) <-  c(gsub(" ","",paste("deportes/articulo",1:9)),gsub(" ","",paste("politica/articulo",1:9)),
                       gsub(" ","",paste("variedades/articulo",1:9)), gsub(" ","",paste("internacional/articulo",1:9)),
                       gsub(" ","",paste("nacionales/articulo",1:9)), gsub(" ","",paste("sucesos/articulo",1:9)),
                       gsub(" ","",paste("comunidad/articulo",1:9)), gsub(" ","",paste("negocios/articulo",1:9)),
                       gsub(" ","",paste("opinion/articulo",1:9)))
                       
#El número de posibles transacciones bot que tienen en su dataset 
#(ellos aceptan que si una persona ve un artículo más de 20 segundos entonces no es un bot). 
periodico$numItems <- rowSums(matriz)
numerobots <- periodico[periodico$numItems >= periodico$tiempototal/20,]
print(paste("El numero de transacciones bot es:",nrow(numerobots)))  

matriz <- matriz[-numerobots$X,]
#Matriz de transacciones
matriz <- as(matriz, "transactions")

summary(matriz)   


#4. Conocer las 10 visitas con mayor tiempo de estadía en la página y 
#las 10 visitas con menor tiempo de estadía en la página.
timemayor10 <- periodico[order(periodico$tiempototal,decreasing = T),][1:10,c(1,7)]
print("10 visitas con mayor tiempo de estadía en la página:")
print(timemayor10)

timemenor10 <-  periodico[order(periodico$tiempototal,decreasing = F),][1:10,c(1,7)]
print("10 visitas con menor tiempo de estadía en la página:")
print(timemenor10)

#5. Conocer las 10 transacciones con mayor número de apariciones en el dataset
top10 <- sort(itemFrequency(matriz, type = "absolute"),decreasing = T)[1:10]
print("Las 10 transacciones con mayor numero de apariciones son:")
print(top10)

itemFrequencyPlot(matriz,topN=10,type="absolute")




rules <- apriori(matriz,parameter = list(support = 0.000001, confidence = 0.0))
summary(rules) 
rules<-sort(rules, by="support", decreasing=TRUE)
options(digits=2)
inspect(rules[1:5])

plot(rules,method="graph",interactive=TRUE,shading=NA)
head(quality(rules));

plot(rules, measure=c("support","lift"), shading="confidence");

plot(rules, shading="order", control=list(main ="Two-key plot"));

inspect(subset(rules, subset = rhs %in% deporte/articulo1))
inspect(subset(rules, subset = lhs %ain% c("item1")))

inspect(subset(rules, subset = lhs %ain% c("deportes/articulo1","internacional/articulo1","comunidad/articulo2") ))
rules<-sort(x, by="support", decreasing=TRUE)
inspect(x)
