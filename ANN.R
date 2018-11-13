library(neuralnet)
library(datasets)
library(tidyr)
library(dplyr)
library(readr)
library(StMoMo)
library(keras)
library(kerasR)
library(plotly)
library(tensorflow)

df <- read.csv(file = "mxt.csv")

df <- df[(df[,3]>0),]

min = apply(df , 2 , min)
max = apply(df , 2 , max)

#log mxt
df["Mxt"] <- log(df["Mxt"])

#standarization
df["Age"] <- (df["Age"] - min["Age"])/(max["Age"] - min["Age"])
df["Year"] <- (df["Year"] - min["Year"])/(max["Year"] - min["Year"])

p <- plot_ly(df, x = ~df$Age, y = ~df$Year, z = ~df$Mxt, type = "contour", 
             width = 600, height = 500)
chart_link = api_create(p, filename="contour-real-rate-mortality")
chart_link

p1 <- plot_ly(df, x = ~df$Age, y = ~df$Year, z = ~df$Mxt, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Age'),
                      yaxis = list(title = 'Year'),
                      zaxis = list(title = 'Mxt')))

chart_link = api_create(p1, filename="3d-scatter-plot-real-data")
chart_link

use_implementation("tensorflow")
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 512, activation = 'relu', input_shape = c(2), kernel_initializer='normal') %>% 
  layer_dense(units = 256, activation = 'relu', kernel_initializer='normal') %>%
  #layer_dense(units = 1, activation = "relu", kernel_initializer='normal') 
  layer_dense(units = 1) 

summary(model)

### Evaluate Model

model %>% compile(
  optimizer = optimizer_adam(0.001),
  loss = 'mse')

#select 2000 as year split
df_train = df[(df[,2]<0.87777778),]
df_test = df[(df[,2]>=0.87777778),]

train <- df_train[,1:2]
train_labels <- df_train[,3]

test <- df_test[,1:2]
test_labels <- df_test[,3]

#train.ind <- sample(1:nrow(train), 0.8*nrow(train))

#model %>% fit(as.matrix(train[train.ind,]), as.matrix(train_labels[train.ind]), epochs=200, batch_size=32)
model %>% fit(as.matrix(train), as.matrix(train_labels), epochs=200, batch_size=32)

score <- model %>% evaluate(as.matrix(test), test_labels, batch_size = 32)

sqrt(score) #rmse      
