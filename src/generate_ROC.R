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
  divy <- 1/length(which(real==target))
  divx <- 1/(length(real)-length(which(real==target)))
  contx <- 0
  conty <- 0
  plot(x=NULL,y=NULL,xlim=c(0, 1), ylim=c(0, 1), xlab="False positive rate", ylab="True positive rate")
  lines(x = c(0,1), y = c(0,1), col = "blue")
  for (i in 1:length(scores)) {
    if (real[i] == target){
      
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx), y = c(conty, conty + divy) , col = "green")
      
      conty <- conty + divy
      
    }else{
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx + divx) , y = c(conty,conty), col = "green")
      
      contx <- contx + divx
    }
    
    
  }#endfor
  #Grafico el ultimo punto.
  points(1, 1, col = "red")
  lines(x = c(contx, 1), y = c(conty, 1) , col = "green")
}#endgraficador



setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
ejemplo <- read.csv("data/roc1.csv")

generate_ROC(ejemplo$SCORE, ejemplo$CLASS, "p")

