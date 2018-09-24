library(randomForest)
library(ggplot2)

#codigo de leitura e processamento foram baseados neste artigo
#https://www.r-bloggers.com/how-to-implement-random-forests-in-r/

leitor <- list(
  read_file = function(){
    data1 <- read.csv(file.choose(), header = TRUE)
    colnames(data1) <- c("Class","nroSp","Eccentricity","Aratio","Elongnation",
                         "Solidity","StocConv","Isofactor","MaxIdentation","Lobedness",
                         "Aintensity","Acontrast","Smoothness","ThirdM","Uniformity",
                         "Entropy")
    data1
  }
)

process <- list(
  GenerateTable = function(pred,validate){
    confusion_matrix <- as.data.frame(table(pred,validate))
    
    print(ggplot(data = confusion_matrix,
                 mapping = aes(x = pred,
                               y = validate)) + xlab("Valores previstos pelo modelo") +
            ylab("Valores testados") +
            geom_tile(aes(fill = Freq)) +
            geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
            scale_fill_gradient(low = "blue",
                                high = "red",
                                trans = "log"))
  },
  PredictModel = function(modelo){
    # Predicting on train set
    predTrain <- predict(modelo, TrainSet, type = "class")
    
    # Predicting on Validation set
    predValid <- predict(modelo, ValidSet, type = "class")
    # Checking classification accuracy
    predValid
  },
  ProcessarRF = function(){
    # Create a Random Forest model with default parameters
    models[[1]] <- randomForest(as.factor(Class) ~ ., data = TrainSet)
    pvalue[[1]] = process$PredictModel(models[[1]])
    cratio[[1]] = (mean(pvalue[[1]] == ValidSet$Class))*100
    
    # Fine tuning parameters of Random Forest model
    x = 500
    
    for (i in 2:10){
      x = x+2
      models[[i]] <- randomForest(as.factor(Class) ~., data = TrainSet, ntree = x, mtry = i+2)
      pvalue[[i]] <- process$PredictModel(models[[i]])
      
      # Checking classification accuracy
      cratio[[i]] <- (mean(pvalue[[i]] == ValidSet$Class))*100
    }
    
    print(plot(x=1:10,y=cratio,type='l',xlab="modelos",ylab="taxa de acerto %", main = "Precisão da Classificação"))
    
    for(i in 1:10) {
       if(cratio[[bestvalue]] < cratio[[i]]){
         bestvalue = i
       }
    }
    bestresult$x = pvalue[[bestvalue]]
    bestresult$y = ValidSet$Class
    bestresult
  }
)
data1 <- leitor$read_file()
#inicializando variáveis

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
models <- list()
cratio <- list()
pvalue <- list()
bestresult <- list(x=1,y=1)
bestvalue = 1

bestresult = process$ProcessarRF()
#process$GenerateTable(bestresult$x,bestresult$y)