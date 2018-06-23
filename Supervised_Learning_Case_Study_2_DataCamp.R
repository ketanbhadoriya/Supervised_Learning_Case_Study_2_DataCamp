#Start of the Script

###Loading the required packages and the Data

library(dplyr)
library(tidyr)
library(ggplot2)
library(RCurl)
library(caret)
library(caTools)


url_data <- "https://assets.datacamp.com/production/course_6013/datasets/stackoverflow.csv"

x <- getURL(url_data)

stackoverflow <- read.csv(textConnection(x))

###Exploring the Data

head(stackoverflow)

str(stackoverflow)

str(stackoverflow$Remote)

#Number of Developers in Each Country

stackoverflow %>% 
  count(Country, sort = TRUE)

#Number of Developers falls under Remote Working or Non-Remote working style

stackoverflow %>% 
  count(Remote, sort = TRUE)

#Visualizing the relationship between coding experience among developers and their working style

ggplot(stackoverflow, aes(Remote, YearsCodedJob)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience") 

# #Comment : There is large the imbalance between remote and 
# non-remote workers in this dataset.

###Building a simple logistic regression model

simple_glm <- stackoverflow %>%
  select(-Respondent) %>%
  glm(Remote ~ .,
      family = "binomial",
      data = .)

#Summary of the model

summary(simple_glm)

# #Comment : From the summary of the aobove model we have an idea of which predictors have 
# larger effect sizes and which are significant or not significant.

###Splitting the data and then Training the Model using caret package

stack_select <- stackoverflow %>%
  select(-Respondent)

# Splitting the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(stack_select$Remote, p = .8, list = FALSE)
training <- stack_select[in_train,]
testing <- stack_select[-in_train,]

#Now Upsampling the data to handle imbalance in case of Remote and Non-remote Developers
up_train <- upSample(x = select(training, -Remote),
                     y = training$Remote,
                     yname = "Remote") %>%
                            as_tibble()

up_train %>%
  count(Remote)

#We can see in the results imbalancment has been removed

###Modeling Logistic Regression and Randmo Forest Model

test_train <- training
test_train$Remote<-gsub("Not remote","Not_Remote",test_train$Remote)
test_train %>%
    count(Remote)
test_train$Remote <-as.factor(test_train$Remote)
levels(test_train$Remote)
head(test_train$Remote)


sample_testing <- testing

sample_testing$Remote<-gsub("Not remote","Not_Remote",sample_testing$Remote)
sample_testing %>%
  count(Remote)
sample_testing$Remote <-as.factor(sample_testing$Remote)
levels(sample_testing$Remote)
head(sample_testing$Remote)

#Logistic Regression 

glm_model <- train(Remote ~ .,method="glm",family="binomial"(link = "logit"),
                   data=test_train,
                   trControl=trainControl(method = "cv",number=10,
                                          verboseIter = TRUE))

glm_model

p <- predict(glm_model,sample_testing,type="prob")
summary(p)

p_class <- ifelse(p>0.50,"Remote","Not_Remote")
table(p_class)
