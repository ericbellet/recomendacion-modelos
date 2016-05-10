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
install('pROC')


#Cargo las librerias.
library(arules)
library(arulesViz)
library(FactoMineR)
library(pROC)

##-------------------------------LECTURA Y ANALISIS-----------------------------------

setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
ejemplo <- read.csv("data/ejemplo.csv")
periodico <- read.csv("data/periodico.csv")

##------------------------------------------------------------------------------------
##----------------------------GENERACION DE ARTICULOS---------------------------------
'''
1. Modi???car su dataset de tal manera que no se lean los identi???cadores de los artículos
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


#Cambio el nombre de la columna para que tenga coherencia con el ejemplo dado.
colnames(periodico)[5] <- "items"
#Creo la columna de los articulos
periodico$articles <- periodico$items

#Se sabe que el portal ofrece 9 tipos de contenidos 
#y nos ofrecen solo información de 9 artículos.


#Obtengo el numero de los articulos.
periodico$articles <- strsplit(gsub("[{}item]","",periodico$articles), ",")

periodico$articles <- lapply(periodico$articles, genArticles)

periodico$articles <- gsub(" ",",",periodico$articles)

periodico$articles <- substring(periodico$articles, 2)

