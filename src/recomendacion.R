###############CAMBIAR##########################
setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
################################################

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
install('apcluster')
#Cargo las librerias.
library(arules)
library(arulesViz)
library(apcluster)
##-------------------------------LECTURA -----------------------------------

ejemplo <- read.csv("data/ejemplo.csv")
periodico <- read.csv("data/periodico.csv")

#CREA LA COLUMNA ARTICLES
#------------------FUNCTION genArticles-----------------
genArticles <- function(articles){
  # Genera la columna articles utilizando la columna items.
  #
  # Args:
  #   articles: Son arreglos númericos que representan los items (EJ: {item1,item9,item63} -> 1,9,63)
  #
  # Returns:
  #   Retorna la columna articles.
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

genArticles2 <- function(articles){
  # Genera la columna articles utilizando la columna items.
  #
  # Args:
  #   articles: Son arreglos númericos que representan los items (EJ: {item1,item9,item63} -> 1,9,63)
  #
  # Returns:
  #   Retorna la columna articles.
  articulo <- ""

  for (i in 1:length(articles)) {
    if (as.integer(articles[i]) <= 9 & as.integer(articles[i]) >= 1){
      articulo <- paste(articulo, gsub(" ","",paste("deportes")))
      
    }
    if (as.integer(articles[i]) <= 18 & as.integer(articles[i]) >= 10){
      articulo <- paste(articulo, gsub(" ","",paste("politica")))
      
    }
    if (as.integer(articles[i]) <= 27 & as.integer(articles[i]) >= 19){
      articulo <- paste(articulo, gsub(" ","",paste("variedades")))
      
    }
    if (as.integer(articles[i]) <= 36 & as.integer(articles[i]) >= 28){
      articulo <- paste(articulo, gsub(" ","",paste("internacional")))
      
    }
    if (as.integer(articles[i]) <= 45 & as.integer(articles[i]) >= 37){
      articulo <- paste(articulo, gsub(" ","",paste("nacionales")))
      
    }
    if (as.integer(articles[i]) <= 54 & as.integer(articles[i]) >= 46){
      articulo <- paste(articulo, gsub(" ","",paste("sucesos")))
      
    }
    if (as.integer(articles[i]) <= 63 & as.integer(articles[i]) >= 55){
      articulo <- paste(articulo, gsub(" ","",paste("comunidad")))
      
    }
    if (as.integer(articles[i]) <= 72 & as.integer(articles[i]) >= 64){
      articulo <- paste(articulo, gsub(" ","",paste("negocios")))
      
    }
    if (as.integer(articles[i]) <= 81 & as.integer(articles[i]) >= 73){
      articulo <- paste(articulo, gsub(" ","",paste("opinion")))
      
    }
  }
  return(paste(unique(unlist(strsplit(articulo, " "))), sep="", collapse=", "))

}

#--------------END FUNCTION genArticles-----------------
#LLENA LA MATRIZ DE TRANSACCIONES,
#------------------FUNCTION llenar-----------------
llenar <- function(periodico,fila){
  # Llena la matriz de transacciones con 1 en caso de que el usuario observo los articulos.
  #
  # Args:
  #   periodico: Recibe los items.
  #   fila: recibe una fila vacia.
  #
  # Returns:
  #   Retorna la matriz de transacciones llena.
  items <- as.numeric(unlist(strsplit(gsub("[{}item]","",unlist(periodico)), ",")))
  fila[items]=1
  return(fila)
}

#--------------END FUNCTION llenar-----------------
#------------------FUNCTION recomendar-----------------
recomendar <- function(n, matriz){
  #plot(rules,method="graph",interactive=TRUE,shading=NA)
  #plot(rules, measure=c("support","lift"), shading="confidence");
  #plot(rules, shading="order", control=list(main ="Two-key plot"));
 

  rules <- apriori(matriz,parameter = list(support = 0.1, confidence = 0.0))
  
  reglas <- subset(rules, subset = lhs %ain% n )

  len <- length(reglas)
  div <- 0.1
  cont <- 0
  #En el caso que no se generaron reglas con ese soporte, voy disminuyendo el soporte
  while (len == 0){
    div <- div /10
    rules <- apriori(matriz,parameter = list(support = div, confidence = 0.0))
    reglas <- subset(rules, subset = lhs %ain% n )
    len <- length(reglas)
    cont <- cont + 1
    #Un criterio de parada ya que puede ser infinito
    if (cont == 7){
      break()
    }
  }
  if (length(reglas)==0){
    trendigtop<-inspect(rules@rhs)
    return(row.names(sort(table(trendigtop),decreasing=TRUE))[1])
  }else{
 
  confianzaAlta <-sort(reglas, decreasing = TRUE, 
                       na.last = NA,by = "confidence",
                       order = FALSE)

  #Obtengo la confianza maxima para luego tomar todos los articulos que posean esa confianza
  maxConfianza <- max(quality((confianzaAlta))[2])
  #Posiciones que poseen la misma confianza.
  confianzaAlta<- subset(confianzaAlta, subset = confidence == maxConfianza)
  
  
  #Ahora ordeno por soporte las que tienen la confianza mas alta
  soportealto <- (sort(confianzaAlta, decreasing = TRUE, 
                                 na.last = NA,by = "support",
                                 order = FALSE)[1])
  articulorecomendar <- inspect(soportealto@rhs[1])
  return(articulorecomendar$items[1])
  }
}

#------------------END FUNCTION recomendar-----------------


##---------------------------PARTE 1--------------------------------
##----------------------------GENERACION DE ARTICULOS---------------------------------

#1. Modifcar su dataset de tal manera que no se lean los identificadores de los artículos
#como itemN sino por su tipo de contenido contenido/articuloN. 
#Ejemplo: {item1, item10, item81} 
#es la transacción {deportes/articulo1, politica
#/articulo1, opinion/articulo9}.

#deportes 1-9
#politica 10-18
#variedades 19-27
#internacional 28-36
#nacionales 37-45
#sucesos 46-54
#comunidad 55-63
#negocios 64-72
#opinion 73-81

#Cambio el nombre de la columna para que tenga coherencia con el ejemplo dado.
colnames(periodico)[5] <- "items"
#Creo la columna de los articulos
periodico$articles <- periodico$items
periodico$individual <- periodico$items
#Se sabe que el portal ofrece 9 tipos de contenidos 
#y nos ofrecen solo información de 9 artículos.

#Obtengo el numero de los articulos.
periodico$articles <- strsplit(gsub("[{}item]","",periodico$articles), ",")

periodico$individual<- strsplit(gsub("[{}item]","",periodico$individual), ",")
periodico$individual <- lapply(periodico$individual, genArticles2)


periodico$individual <- substring(periodico$individual, 2)
#Modifico el dataset con las condiciones dadas.
periodico$articles <- lapply(periodico$articles, genArticles)

#Convierto los espacios en ,
periodico$articles <- gsub(" ",",",periodico$articles)

#Elimino la primer valor del string.
periodico$articles <- substring(periodico$articles, 2)

#Calculo el tiempo totan el segundos que dura el usuario en la pagina.
periodico$tiempototal <- difftime(periodico$exit, periodico$entry, units =  "secs")


##-------------------------------FIN PARTE 1--------------------------------

#GENERACION DE MATRIZ DE TRANSACCIONES Y DETECTAR USUARIOS ROBOTS

#Generar la matriz de transacciones para recomendar articulos.
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

#-------------------------------TRANSACCIONES BOTS---------------------------------------                     
#El número de posibles transacciones bot que tienen en su dataset 
#(ellos aceptan que si una persona ve un artículo más de 20 segundos entonces no es un bot). 
periodico$numItems <- rowSums(matriz)
numerobots <- periodico[periodico$numItems >= periodico$tiempototal/20,]
print(paste("El numero de transacciones bot es:",nrow(numerobots)))  

periodicoSinBots <- periodico[-numerobots$X,]
#Utilizamos la matriz de transacciones sin las transacciones bots.
matriz <- matriz[-numerobots$X,]



##-------------------------------PARTE 2------------------------------------
#2. Conocer los tipos de usuarios que ingresan a su página (ellos creen que 
#son 8 tipos de usuarios) y tratar de determinar la proporción de cada tipo de usuario.

#GENERACION DE MATRIZ DE TRANSACCIONES 

#Matriz de transacciones para detectar grupos
#matrix2 <- matrix(data = 0, nrow = nrow(matriz), ncol = 9)

  #matrix2[,1] <- rowSums(matriz[,1:9])
#matrix2[,2] <- rowSums(matriz[,10:18])
#matrix2[,3] <- rowSums(matriz[,19:27])
#matrix2[,4] <- rowSums(matriz[,28:36])
# matrix2[,5] <- rowSums(matriz[,37:45])
#matrix2[,6] <- rowSums(matriz[,46:54])
# matrix2[,7] <- rowSums(matriz[,55:63])
# matrix2[,8] <- rowSums(matriz[,64:72])
# matrix2[,9] <- rowSums(matriz[,73:81])
 
#  matrix2[,1] <- apply(matriz[,1:9],1,max)
# matrix2[,2] <- apply(matriz[,10:18],1,max)
# matrix2[,3] <- apply(matriz[,19:27],1,max)
# matrix2[,4] <- apply(matriz[,28:36],1,max)
#  matrix2[,5] <- apply(matriz[,37:45],1,max)
#  matrix2[,6] <- apply(matriz[,46:54],1,max)
#  matrix2[,7] <- apply(matriz[,55:63],1,max)
# matrix2[,8] <- apply(matriz[,64:72],1,max)
# matrix2[,9] <- apply(matriz[,73:81],1,max) 
  
#colnames(matrix2) <- c("deportes","politica","variedades","internacional","nacionales", "sucesos","comunidad","negocios","opinion")

kmedias <- kmeans(matriz, 8,algorithm = "Hartigan-Wong")
periodicoSinBots$cluster <- kmedias$cluster
table(periodicoSinBots$cluster)

#Calculamos la matriz de similaridad utilizando el inverso del error cuadrado (distancia euclidea).
sim <- crossprod(matriz)
sim <- sim / sqrt(sim)
#Corremos la affinity propagation
clust_ap <- apcluster(sim) 
show(clust_ap)


matriz <<- as(matriz, "transactions")

rules <- apriori(matriz,parameter = list(support = 0.000008019181883, confidence = 1.0))

plot(rules, method = "grouped", control = list(k = 8))

#ruledf = data.frame(
#  lhs = labels(lhs(rules)),
# rhs = labels(rhs(rules)))

##-------------------------------FIN PARTE 2------------------------------------
##-------------------------------PARTE 3------------------------------------
#3. Dado un usuario nuevo que haya ingresado a n artículos (n variable), 
#poder recomendar un artículo n+1 y así aumentar el compromiso del cliente 
#con su portal web. Como usted sabe, para poder calcular las reglas necesita 
#como entrada MinSupport y MinConfianza. Sin embargo, el cliente desconoce cuáles 
#son estos valores en consecuencia es tarea de usted determinar y justi???car los 
#mismos de acuerdo a su criterio

print("Introduzca los n articulos:")
#n <- c("deportes/articulo1","internacional/articulo1", "comunidad/articulo1")
#n <- c("deportes/articulo6","deportes/articulo9")
n <- c("deportes/articulo6","internacional/articulo9")
articuloARecomendar <- recomendar(n, matriz)
print(paste("El artículo que se recomienda es:", articuloARecomendar))

##-------------------------------FIN PARTE 3--------------------------------


##-------------------------------PARTE 4------------------------------------
#4. Conocer las 10 visitas con mayor tiempo de estadía en la página y 
#las 10 visitas con menor tiempo de estadía en la página.
timemayor10 <- periodicoSinBots[order(periodicoSinBots$tiempototal,decreasing = T),][1:10,c(1,7)]
print("10 visitas con mayor tiempo de estadía en la página:")
print(timemayor10)

timemenor10 <-  periodicoSinBots[order(periodicoSinBots$tiempototal,decreasing = F),][1:10,c(1,7)]
print("10 visitas con menor tiempo de estadía en la página:")
print(timemenor10)

##-------------------------------FIN PARTE 4--------------------------------


##-------------------------------PARTE 5------------------------------------

top10 <- sort(itemFrequency(matriz, type = "absolute"),decreasing = T)[1:10]
print("Los 10 articulos con mayor numero de apariciones son:")
print(top10)

itemFrequencyPlot(matriz,topN=10,type="absolute", main ="Los 10 articulos con mayor numero de apariciones.")

#5. Conocer las 10 transacciones con mayor número de apariciones en el dataset

MatrizSinBots = split(periodicoSinBots$articles,periodicoSinBots$X)
MatrizSinBots = as(MatrizSinBots,"transactions")
top10transacciones <- sort(itemFrequency(MatrizSinBots, type = "absolute"),decreasing = T)[1:10]
print("Las 10 transacciones con mayor numero de apariciones son:")
print(top10transacciones)

itemFrequencyPlot(MatrizSinBots,topN=10,type="absolute", main = "Las 10 transacciones con mayor numero de apariciones.")



