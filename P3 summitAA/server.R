#install.packages("Rtools")
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(tidyverse)
library(haven)
library(rgl)
#library(tree)
library(caret)
library(DT)
library (cluster)
library(dplyr)
options(dplyr.print_min = 5)
options(tibble.print_min = 5)
library(knitr)
opts_chunk$set(message = T, cache = F, warning = T)



# from https://wonder.cdc.gov/aids-v2002.html

HIV<-read_csv("HIV_DD.csv")

HIV<-filter(HIV, !is.na(Cases)&!is.na(Population))

#HIV<-HIV%>%select(-'Rate per 100000')
#HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)

HIV$Year<-factor(HIV$Year)
HIV$Indicator<-factor(HIV$Indicator)
HIV$Cases<-as.integer(HIV$Cases)
HIV$Population<-as.integer(HIV$Population)


HIV<-HIV %>% mutate(Rate_per_100000 = 100000*Cases/Population)
HIV<-filter(HIV, !is.na(Cases)&!is.na(Rate_per_100000))%>% select(-`Rate per 100000`)

HIV


HIV2<-HIV%>%group_by(Geography)%>%summarise(meanCase=mean(Cases,na.rm=T), meanPop=mean(Population,na.rm=T))


HIV$Survive <- ifelse((HIV$Indicator == "AIDS deaths" | HIV$Indicator == "HIV deaths"), 0 ,1)

set.seed(100)
index<-sample(1:nrow(HIV), 800)
index

HIV3<-HIV[index,]
HIV3<-HIV3
HIV3<-filter(HIV3, !is.na(Cases)&!is.na(Rate_per_100000)&!is.na(Survive))

write.csv(HIV3, file = "HIV3.csv",row.names=FALSE, na="")


HIV4<-read_csv("HIV3.csv")
#HIV4<-HIV4()

HIVPC<-reactive({HIV%>%select(Cases,Population,Rate_per_100000) })

# Define server logic required to draw the plots
shinyServer(function(input, output,section) {

  #create plot
  output$summary1<-renderPrint({

    HIVyear<-HIV %>% group_by(Year) %>%
      summarise(sum= sum(Cases),avg = mean(Cases), med = median(Cases), var = var(Cases))
    print (HIVyear%>% filter(Year == input$Year))

  })


  #get approximations and compare
  output$table1<-renderTable({
  head(HIV,n=20)
  })


  output$plotCluster<-renderPlot({
    hc <- hclust(dist(HIV2%>%select(input$var)), "ave")
    plot(hc, hang = -1)
 } )

  output$PCresult<-renderPrint({

    PCs <- prcomp(select(HIV,Cases,Population,Rate_per_100000), center = TRUE, scale = TRUE)
    PCs
  })


  output$plotPC <-renderPlot(
    {
      data<-HIVPC()
      dat <- princomp(data,cor=TRUE,score=TRUE)
      biplot(dat)
    })

 # output$plotPC <- renderPlot ({
 #   PCs <- prcomp(select(HIV,Cases,Population,Rate_per_100000), center = TRUE, scale = TRUE)
 #  biplot(PCs)

 # })

  set.seed(100)
  #split into training and test
  train <- sample(1:nrow(HIV3), size = nrow(HIV3)*0.8)
  test <- dplyr::setdiff(1:nrow(HIV3), train)
  HIVTrain <- HIV3[train, ]
  HIVTest <- HIV3[test, ]

  output$trainresult<-renderPrint({

    trainTree <- train(Survive ~ ., data = HIVTrain, method = input$treemodelmethod,
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                  preProcess = c("center", "scale"))
    trainTree
  })

  output$misclassRate<-renderText({
    predictTree <- table(data.frame(pred = predict(trainTree, HIVTest), true = HIVTest$Survive))
    #misclassificatoon rate
    misclassRate<-1- sum(diag(predictTree)/sum(predictTree))

    paste("the misclassificatoon rate of method of  " ,input$treemodelmethod, "is " , round(misclassRate, 2) )

  })


    set.seed(100)
  #split into training and test
  train <- sample(1:nrow(HIV3), size = nrow(HIV3)*0.8)
  test <- dplyr::setdiff(1:nrow(HIV3), train)
  HIVTrain <- HIV3[train, ]
  HIVTest <- HIV3[test, ]




  #HIV4<-reactive(read_csv("HIV3.csv"))
  #HIV4<-HIV()
  output$trainresult<-renderPrint({

    glmFit <- glm(Survive ~ input$varian, data = HIV4, family = "binomial")
    glmFit

  })
  #output$misclassRate<-renderText({


  #})

  output$trace_table <- renderDataTable({

    datatable(cbind(HIV450sample), options = list(paging = FALSE))

  })
  
  output$plot5 <- renderPlot({
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'Calculation in progress',
                 detail = 'This may take a while...')
    
    for (i in 1:15) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
    plot(HIV4$Population, HIV4$Cases)
  })
  
  
  #output$plot5 <- renderPlot({
   # plot(HIV4$Population, HIV4$Cases)
    
    
    
    
  #})
  
  output$info5 <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  



})
