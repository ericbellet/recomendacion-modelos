require(shiny)

####################CAMBIAR###################
setwd("C:/Users/Eric/Desktop/recomendacion-modelos")
##############################################
source("src/generate_ROC.r")
runApp(
  list(
    ui = fluidPage(
      headerPanel('Generador de curvas ROC'),
      
      sidebarPanel(
        fileInput('file1', 'Seleccione el .csv para generar la curva ROC'),
        textInput("target", label="Target", value="Introduzca el target"),
        actionButton("addButton", "GENERAR CURVA DE ROC"),
        p('En la carpeta data hay 3 datasets con los que puede probar, si quiere introducir otro .csv asegurese que el nombre de la columna de los scores sea "scores" y el de las clases "real".'),
        p("roc1.csv es el ejemplo del paper (las clases son p y n), roc2.csv es el ejemplo de piazza (las clases son p y n) y roc3.csv posee varias clases n, p, e y r. ")
        
      ),
      
      mainPanel(
        
        uiOutput("plots")
      )),
    
    server = function(input, output) {     
      dataset <- eventReactive(input$addButton, {
        
        
        inFile <- input$file1
        df <- read.csv(inFile$datapath)
        df$target <- input$target
        return(df)
        
      })
      
      output$plots <- renderUI({
        
        
        df <- dataset()
        n <- length(unique(df$real))-1
        
        plot_output_list <- lapply(1:n, function(i) {
          
          plotname <- paste("plot", i, sep="")
          plotOutput(plotname, height = 580, width = 550)
          
          
        })
        
      
        do.call(tagList, plot_output_list)
        
        
      })
      observe({
        
        
        
        df <- dataset()
        n <- length(unique(df$real))-1
        real = df$real
        scores =  df$scores
        
        target = df$target[1]
        clases <- unique(real)
        
        clases <- clases[!clases %in% target]
        for (i in 1:length(clases)) {
          local({ 
            #Se toma el valor positivo como uno solo, y los demas como negativos.
            df <- data.frame(scores,real)
            class1 <- df[df$real == target,]
            class2 <- df[df$real == clases[i],]
            df <- merge(x = class1, y = class2, all = TRUE)
            df <- df[order(df$scores, decreasing = TRUE),]
            
            r <- df$real
            s <- df$scores
            
            plotname <- paste("plot", i, sep="")
            
            output[[plotname]] <- renderPlot({
              
              re = r
              
              sco = s
              generate_ROC(sco, re,target)
              
            })
          })#endlocal
        }
        
      })
    }
  )
)