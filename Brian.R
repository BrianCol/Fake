library(tidyverse)
library(DataExplorer)
library(lattice)
library(caret)
library(naniar)
library(textcat)


fake <- read_csv("CleanFakeNews.csv")

#fake$isFake <- as.factor(fake$isFake)

fake.train <- fake %>% drop_na() %>% filter(!is.na(isFake))
fake.test <- fake %>% filter(is.na(isFake))

sub <- createDataPartition(y = fake.train$isFake, p = .1, list = FALSE)
sub <- as.vector(sub)

new_data <- fake.train[sub, ]

#new_data$isFake <- as.factor(new_data$isFake)

xgbtree <- train(form=as.factor(isFake)~.,
                 data=new_data %>% select(-Id, -Set, -language.x),
                 method="xgbTree",
                 trControl=trainControl(method="repeatedcv",
                                        number=3, #Number of pieces of your data
                                        repeats=1) #repeats=1 = "cv"
)

gbm_model <- train(as.factor(isFake)~.,
                data=fake.train %>% select(-Id, -Set, -language.x),
                method="gbm",
                trControl=trainControl(method="repeatedcv",
                                       number=3, #Number of pieces of your data
                                       repeats=1) #repeats=1 = "cv"
)

xgbtree$results
xgbtree.preds <- data.frame(Id=fake.test$Id, label=predict(xgbtree, newdata=fake.test))

write_csv(x=xgbtree.preds, path="./xgbtree.csv")
