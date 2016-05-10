    
setwd("C:/Users/Eric/Desktop/recomendacion-modelos/")
ejemplo <- read.csv("data/roc2.csv")

#ORDENAMOS EL DATAFRAME
ejemplo <- ejemplo[order(ejemplo$SCORE, decreasing = TRUE),]




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
  if (target <= 2){
    graficador(scores, real)
  }else{
    #En caso que hayan mas de 2 clases.
    target <- 5
   combinatoria <- factorial(target)/(factorial(2)*factorial(target-2))
   clases <- unique(ejemplo$CLASS)
   
   df <- data.frame(scores,real)
  #-------------1 vs all---------------------
   cont <- 0
   inicio <- 1
   while (cont != combinatoria) {
     for (i in inicio:length(clases)) {
       
       class1 <- ejemplo$CLASS[ejemplo$CLASS == clases[inicio]]
       
     }
    inicio <- inicio + 1
    
    cont <- cont + 1
   }#endwhile
   
  }#endif
  
}#endfunction


graficador <- function(scores, real){
  div <- 1/(length(scores)/2)
  contx <- 0
  conty <- 0
  plot(x=NULL,y=NULL,xlim=c(0, 1), ylim=c(0, 1), xlab="False positive rate", ylab="True positive rate")
  lines(x = c(0,1), y = c(0,1), col = "blue")
  for (i in 1:length(scores)) {
    if (real[i] == "p"){
      
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx), y = c(conty, conty + div) , col = "green")
      
      conty <- conty + div
      
    }else{
      points(contx, conty, col = "red")
      
      lines(x = c(contx, contx + div) , y = c(conty,conty), col = "green")
      
      contx <- contx + div
    }
    
    
  }#endfor
  #Grafico el ultimo punto.
  points(1, 1, col = "red")
  lines(x = c(contx, 1), y = c(conty, 1) , col = "green")
}#endgraficador

generate_ROC(ejemplo$SCORE, ejemplo$CLASS, length(unique(ejemplo$CLASS)))
scores<-1:10
real<-1:10
df<-data.frame(scores,real)
class1 <- ejemplo$CLASS[ejemplo$CLASS == clases[inicio]]
f1<-ejemplo[scores == 3,]
merge(f,f1, by = c("scores","real"))
merge(x = f, y = f1, all = TRUE)
