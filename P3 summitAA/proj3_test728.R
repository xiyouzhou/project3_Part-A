## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(shiny)
library(shinydashboard)
library(plotly)
library(datasets)
library(tidyverse)
library(haven)
library(rgl)
library(tree)
library(caret)
library (cluster)
library(dplyr)
options(dplyr.print_min = 5)
options(tibble.print_min = 5)
library(knitr)
opts_chunk$set(message = F, cache = F, warning = F)


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



#view(HIV3)
view(HIV3,n=10)


#view(HIV)

#str(HIV)
#attributes(HIV)
#levels(HIV$Indicator)


#g <- ggplot(data = HIV, aes(x = Year, y=Cases))
#g + geom_bar()

# cases by indicators
HIV %>% group_by(Indicator) %>%
  summarise(avg = mean(Cases), med = median(Cases), var = var(Cases))

# cases by years

HIV %>% group_by(Year) %>%
  summarise(sum= sum(Cases),avg = mean(Cases), med = median(Cases), var = var(Cases))

HIV %>% group_by(Geography ) %>%
  summarise(avg = mean(Cases), med = median(Cases), var = var(Cases))




g <- ggplot(HIV, aes(x = Indicator, y = Cases))
g + geom_boxplot(fill = "green")

g <- ggplot(HIV, aes(x = Year, y = Cases))
g + geom_boxplot(fill = "green")

g <- ggplot(HIV, aes(x = Geography, y = Cases))
g + geom_boxplot(fill = "green")



g <- ggplot(HIV, aes(x = Year, y = Cases))
g + geom_point() # +
 # geom_smooth() +
  #geom_smooth(method = lm, col = "Red")


g <- ggplot(HIV, aes(x = Geography, y = Cases))
g + geom_point() # +

g <- ggplot(HIV, aes(x = Indicator, y = Cases))
g + geom_point() # +




# Three var.Cases,Population,Rate_per_100000


PCs <- prcomp(select(HIV,Cases,Population,Rate_per_100000), center = TRUE, scale = TRUE)
PCs
screeplot(PCs, type = "lines") #scree plot used for visual

plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')

#biplot(PCs, xlabs = subRegionData$region, cex = 1, xlim = c(-0.08, 0.08))
biplot(PCs)


# two var.Cases,Population

PCs <- prcomp(select(HIV,Cases,Population), center = TRUE, scale = TRUE)
PCs
screeplot(PCs, type = "lines") #scree plot used for visual

plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')

#biplot(PCs, xlabs = subRegionData$region, cex = 1, xlim = c(-0.08, 0.08))
biplot(PCs)




# two var.Cases,Rate_per_100000

PCs <- prcomp(select(HIV,Cases,Rate_per_100000), center = TRUE, scale = TRUE)
PCs
screeplot(PCs, type = "lines") #scree plot used for visual

plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component",
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')

#biplot(PCs, xlabs = subRegionData$region, cex = 1, xlim = c(-0.08, 0.08))
biplot(PCs)


library (cluster)

HIV2<-HIV%>%group_by(Geography)%>%summarise(meanCase=mean(Cases,na.rm=T), meanPop=mean(Population,na.rm=T))

hc <- hclust(dist(HIV2$meanPop), "ave")
plot(hc)
plot(hc, hang = -1)

hc <- hclust(dist(HIV2%>%select(meanCase)), "ave")
#plot(hc)
plot(hc, hang = -1)




#attributes(HIV2b)$row.names=HIV2b$Geography

HIV2b<-HIV2%>%select(Geography,meanPop)

HIV2b


hc <- hclust(dist(HIV2b), "ave")
plot(hc)
plot(hc, hang = -1)



#hcity.D2 <- hclust(UScitiesD, "ward.D2")


####################################################### bad
HIV2d<-data.frame(HIV2b, rol.name=HIV2b$Geography)

HIV2d<-HIV2d%>%select(-Geography)

hc <- hclust(dist(HIV2d), "ave")
plot(hc)
plot(hc, hang = -1)



#attributes(HIV2b)
#plot(hcity.D2, hang=-1)

############################################################# bad


HIV$Survive <- ifelse((HIV$Indicator == "AIDS deaths" | HIV$Indicator == "HIV deaths"), 0 ,1)

set.seed(100)
index<-sample(1:nrow(HIV), 800)
index

HIV3<-HIV[index,]
HIV3<-HIV3
HIV3<-filter(HIV3, !is.na(Cases)&!is.na(Rate_per_100000)&!is.na(Survive))

write.csv(HIV3, file = "HIV3.csv",row.names=FALSE, na="")

#view(HIV3)
view(HIV3,n=10)

set.seed(100)
#split into training and test
train <- sample(1:nrow(HIV3), size = nrow(HIV3)*0.8)
test <- dplyr::setdiff(1:nrow(HIV3), train)
HIVTrain <- HIV3[train, ]
HIVTest <- HIV3[test, ]
#fit models using repeated CV
rpartTree <- train(Survive ~ ., data = HIVTrain, method = "rpart",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                   preProcess = c("center", "scale"))
treebagdTree <- train(Survive ~ ., data = HIVTrain, method = "treebag",
                    trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                    preProcess = c("center", "scale"))
rfTree <- train(Survive ~ ., data = HIVTrain, method = "rf",
                trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                preProcess = c("center", "scale"))

gbmTree <- train(Survive ~ ., data = HIVTrain, method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                   preProcess = c("center", "scale"))


rpartTbl <- table(data.frame(pred = predict(rpartTree, HIVTest), true = HIVTest$Survive))

treebagTbl <- table(data.frame(pred = predict(treebagdTree, HIVTest), true = HIVTest$Survive))

rfTbl <- table(data.frame(pred = predict(rfTree, HIVTest), true = HIVTest$Survive))

gbmtTbl <- table(data.frame(pred = predict(gbmTree, HIVTest), true = HIVTest$Survive))

#misclassificatoon rate
1-c(rpart = sum(diag(rpartTbl)/sum(rpartTbl)), treebag = sum(diag(treebagTbl)/sum(treebagTbl)),
    rf = sum(diag(rfTbl)/sum(rfTbl)),
    gbm = sum(diag(gbmtTbl)/sum(gbmtTbl)))



##################################################################################
HIV4<-read_csv("HIV3.csv")
glmFit <- glm(Survive ~ Year, data = HIV4, family = "binomial")
glmFit

predict(glmFit, newdata = data.frame(Year = 2015),
        type = "response", se.fit = TRUE)
glmFit <- glm(Survive ~ . , data = HIV4, family = "binomial")
glmFit


glmFit2 <- glm(Survive ~ Population, data = HIV4, family = "binomial")
glmFit2



glmFit <- glm(Survive ~ Indicator , data = HIV4, family = "binomial")
glmFit

predict(glmFit, newdata = data.frame(Indicator = "HIV diagnoses"),
        type = "response", se.fit = TRUE)

temp <- summary(glmFit)
temp$coefficients
#deviance and its df
paste("Deviance = ", round(temp$deviance, 2), " DF = ", temp$df.residual, sep = "")

#predict probability for given Year
#predict(glmFit, newdata = data.frame(Year = 2015),
#       type = "response", se.fit = TRUE)
#





















