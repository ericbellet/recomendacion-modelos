generate_ROC <- function(scores, real, target){ 
  # Genera una curva de ROC.
  #
  # Args:
  #   scores: Los scores por instancia (no necesariamente ordenados). 
  #   real: La verdadera clase de las instancias.
  #   target:  La clase target. En el caso de que nclass > 2 entonces haga un enfoque 1 vs all.
  #
  # Returns:
  #   Genera la curva ROC.
  
  
  #En caso que hayan 2 clases nada mas.
  if (length(unique(real)) <= 2){
    df <- data.frame(scores,real)
    df <- df[order(df$scores, decreasing = TRUE),]
    graficador(df$scores, df$real, target)
    
  }else{
    #En caso que hayan mas de 2 clases.
   
   clases <- unique(real)
   clases <- clases[!clases %in% target]
   
  #-------------1 vs all---------------------
    for (i in 1:length(clases)) {
       #Se toma el valor positivo como uno solo, y los demas como negativos.
       df <- data.frame(scores,real)
       class1 <- df[df$real == target,]
       class2 <- df[df$real == clases[i],]
       df <- merge(x = class1, y = class2, all = TRUE)
       df <- df[order(df$scores, decreasing = TRUE),]
       graficador(df$scores, df$real, target)
    }
   
  }#endif
  
}#endfunction


graficador <- function(scores, real, target){
  # Genera una curva de ROC.
  #
  # Args:
  #   scores: Los scores por instancia (no necesariamente ordenados). 
  #   real: La verdadera clase de las instancias.
  #   target:  La clase target. En el caso de que nclass > 2 entonces haga un enfoque 1 vs all.
  #
  # Returns:
  #   Genera la curva ROC.
  divy <- 1/length(which(real==target))
  divx <- 1/(length(real)-length(which(real==target)))
  contx <- 0
  conty <- 0
 
  negativaclase <- unique(real)
  negativaclase <- negativaclase[!negativaclase %in% target]
 
  
  plot(x=NULL,y=NULL,xlim=c(0, 1), ylim=c(0, 1), xlab="False positive rate", ylab="True positive rate",main=paste("Clase target:",target,".","Clase negativa:", negativaclase[1],"."))
  lines(x = c(0,1), y = c(0,1), col = "blue")
  puntosx <<- c(contx)
  puntosy <<- c(conty)
  id <- order(puntosx)
  
  
  i <- 1
  while (i != (length(scores)+1)){
    puntosx <<- c(puntosx, contx)
    puntosy <<- c(puntosy, conty)
    #Existen varios elementos con el mismo score??
    samescore <- length(which(scores==scores[i]))
    if (samescore > 1){
      contador <- 0
      contxORIGEN <- contx
      contyORIGEN <- conty
      points(contx, conty, col = "red")
      while (contador != samescore ) {
        #Si es target
        if (real[i] == target){
          
          #points(contx, conty, col = "red")
          
          conty <- conty + divy
          
          
        }else{
          #Si es negativo
          #points(contx, conty, col = "red")
          
          contx <- contx + divx
        }
        #capaz hay que restar
        i <- i + 1
        contador <- contador + 1
        
      }#endwhile
     
      lines(x = c(contxORIGEN, contx) , y = c(contyORIGEN,conty), col = "green")
    }else{
    
    
    
    #Si es target
    if (real[i] == target){
     
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx), y = c(conty, conty + divy) , col = "green")
      
      conty <- conty + divy
      i <- i + 1
      
    }else{
    #Si es negativo
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx + divx) , y = c(conty,conty), col = "green")
      
      contx <- contx + divx
      i <- i + 1
    }
    
    }
    
  }#endfor
  #Grafico el ultimo punto.
  puntosx <<- c(puntosx,1)
  puntosy <<- c(puntosy,1)
  points(1, 1, col = "red")
  lines(x = c(contx, 1), y = c(conty, 1) , col = "green")
  
  #???legend("bottomright", title = paste("ROC area:",auc(puntosx, puntosy)))
}#endgraficador


#EJEMPLO PIAZZA
y = c(2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1)
scores = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.5, 0.5, 0.5, 0.5, 0.38, 0.37, 0.36, 0.35, 0.34, 0.33, 0.30, 0.1)
target = 2
#Llamando a la funcion.
generate_ROC(scores, y, 2)

setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
#EJEMPLO PAPER 1
ejemplo <- read.csv("data/roc1.csv")
generate_ROC(ejemplo$scores, ejemplo$real, "p")

#EJEMPLO PAPER 2
ejemplo <- read.csv("data/roc2.csv")
generate_ROC(ejemplo$scores, ejemplo$real, "p")

#EJEMPLO PAPER 3
ejemplo <- read.csv("data/roc3.csv")
generate_ROC(ejemplo$scores, ejemplo$real, "p")


#scores = c(0.90, 0.80, 0.70, 0.60, 0.55, 0.54, 0.53, 0.53, 0.53, 0.53, 0.53, 0.53, 0.53, 0.37, 0.36,
#  0.35, 0.34, 0.33, 0.30, 0.10)

#c("p","e", "n", "p", "p", "p", "n", "e", "p", "n", "p","n", "e", "r", "n", "n", "r", "n", "p", "r")
