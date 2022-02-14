
###### Importing the Data #########
# read excel file
library(readxl)
afdf <- read_excel("C:/Users/asuka/Desktop/Hult/2022 Spring/R/Air France Case study/data.xlsx")
# make soft copy
df <- afdf
# check the data
summary(df)


######## Massaging Data ###########
# publisher Name (Us - Non US)
# global = 0 , US = 1 
table(df$`Publisher Name`)

df$pn_2 <- gsub("Google - Global","0",df$`Publisher Name`)
df$pn_2 <- gsub("Google - US","1",df$pn_2)
df$pn_2 <- gsub("MSN - Global","0",df$pn_2)
df$pn_2 <- gsub("MSN - US","1",df$pn_2)
df$pn_2 <- gsub("Overture - Global","0",df$pn_2)
df$pn_2 <- gsub("Overture - US","1",df$pn_2)
df$pn_2 <- gsub("Yahoo - US","1",df$pn_2)
df$pn_2 <- as.numeric(df$pn_2)

table(df$pn_2)

# match type
# Advanced, Exact 0 / Standard 1 /  Broad 2 
# assume NA is standard
table(df$`Match Type`)

df$mt <- gsub("Exact","0",df$`Match Type`)
df$mt <- gsub("Advanced","0",df$mt)
df$mt <- gsub("Standard","1",df$mt)
df$mt <- gsub("Broad","2",df$mt)
df$mt <- gsub("N/A","1",df$mt)
df$mt <- as.numeric(df$mt)

table(df$mt)

# Status
# Deactivated 0 / Live 1 /  Paused 0 / Sent 1 / Unavailable 0
table(df$Status)
df$st <- gsub("Deactivated","0",df$Status)
df$st <- gsub("Live","1",df$st)
df$st <- gsub("Paused","0",df$st)
df$st <- gsub("Sent","1",df$st)
df$st <- gsub("Unavailable","0",df$st)
df$st <- as.numeric(df$st)

table(df$st)


#### Create Target Column #######
table(df$"Total Volume of Bookings")
# 0 value - 4142 / over 1 -368 

# for loop to divide business success or not 
# (success(booking > 1) = 1, fail = 0)
for (i in 1:nrow(df)){
  if(df$`Total Volume of Bookings`[i] == 0){
    df$tb[i] <- 0} else{df$tb[i] <- 1} # closing if 
  }# closing for loop

# check the result 
table(df$tb)


###### Normalize ##########
# creating a UDF to normalize with min-max rescaling
normalize <- function(var){
  my_norm <- (var - min(var)) / (max(var)-min(var))
  return(my_norm)
} # closing normalize function

df$mt_norm <- normalize(var = df$mt)

##### Sampling ##########
# make training index
training_idx <- sample(1:nrow(df), size = 0.8*nrow(df))

# making training set and testing set 
df_train <- df[training_idx, ]
df_test <- df[-training_idx, ]


###### Regression ########
## before normalization 
my_logit <- glm(tb ~ pn_2  # publisher (global=0 /  US = 1) 
                     + mt #match type (Advanced, Exact 0 / Standard 1 /  Broad 2)
                     +st #status (Live , Sent = 1 / Deactivated, Paused, Unavailable = 0)
                     ,data = df_train)
summary(my_logit)

## after normalization 
## check which variable is the most significant variables
my_logit_norm <- glm(tb ~ pn_2  # publisher (global=0 /  US = 1) 
                + mt_norm #match type (Advanced, Exact 0 / Standard 1 /  Broad 2)
                +st #status (Live , Sent = 1 / Deactivated, Paused, Unavailable = 0)
                ,data = df_train)
summary(my_logit_norm)


#### Confusion Matrix ####
# install.packages("caret")
library(caret)

# creating confusion matrix for training 
my_prediction_training <- predict(my_logit, df_train, type = "response")
confusionMatrix(data =  as.factor(as.numeric(my_prediction_training > 0.5)),
                reference = as.factor(as.numeric(df_train$tb)))

# creating confusion matrix for testing 
my_prediction_testing <- predict(my_logit, df_test, type = "response")
confusionMatrix(data =  as.factor(as.numeric(my_prediction_testing > 0.5)),
                reference = as.factor(df_test$tb))

# Creating an AUC ROC for my logit
# install.packages("ROCR")
library(ROCR)

pred_val_logit <- prediction(my_prediction_testing, df_test$tb)
perf_logit <- performance(pred_val_logit, "tpr", "fpr")

plot(perf_logit)

###### Gini Tree ########
# make a tree
library(rpart)
library(rpart.plot)
my_tree <- rpart(tb ~ pn_2+ mt+st,
                 data = df_train, method = "class", 
                 control=rpart.control(minsplit = 20,
                                       minbucket= 5,
                                       cp=0.0000000000000000001))
rpart.plot(my_tree, type = 1 , extra = 1)

# using this tree to predict on testing customers 
tree_predict <- predict(my_tree, df_test, type ="prob")
tree_prediction <- prediction(tree_predict[ ,2],
                                      df_test$tb)
my_tree_perf <- performance(tree_prediction,"tpr","fpr")
plot(perf_logit, col = "blue")
plot(my_tree_perf, col = "green4", add = TRUE)


### Common Keywords #####

#install.packages("wordcloud")
#install.packages("tm")
library(wordcloud)
library(RColorBrewer)
library(tm)

##slide only booked
booked <- df_train[which(df$tb == 1), ]

# divide global booked  / us booked
booked_global <- booked[which(booked$pn_2 == 0), ]
booked_us <- booked[which(booked$pn_2 == 1), ]

# keyword result (global booked)
wordcloud(booked_global$Keyword, scale = c(1,2), min.freq = 300, max.words =200, colors = rainbow(30))
# keyword result (US booked)
wordcloud(booked_us$Keyword, scale = c(1,2), min.freq = 300, max.words =200, colors = rainbow(30))

### slide only not booked
not_booked <- df_train[which(df$tb == 0), ]

# global not booked/us not booked
not_booked_global <- not_booked[which(not_booked$pn_2 == 0), ]
not_booked_us <- not_booked[which(not_booked$pn_2 == 1), ]

# keyword result (global not booked)
wordcloud(not_booked_global$Keyword, scale = c(1,2), min.freq = 50, max.words =200, colors = rainbow(30))
# keyword result (US not booked)
wordcloud(not_booked_us$Keyword, scale = c(1,2), min.freq = 200, max.words =200, colors = rainbow(30))
