# install libraries
install.packages('kernlab')
install.packages('class')
install.packages('dplyr')
install.packages('skimr')
install.packages('ggplot2')
install.packages("tidyverse")

# load used libraries
library(kernlab)
library(class)
library(dplyr)
library(skimr)
library(ggplot2)
library(tidyverse)

# load data
data(spam)

# check data 
str(spam)
names(spam)
summary(spam)

# set seed
set.seed(50) # make the random sampling reproducible

# separate data into those with type = spam and type = nonspam 
nonspam_m <- spam[spam$type == "nonspam",] 
spam_m <- spam[spam$type == "spam",] 

# randomly select 150 rows for each type, making the next step 
# of spliting into test and train data set not overlaping and easier
n = 150
nonspam_m <- nonspam_m[sample(nrow(nonspam_m), n), ]  # for nonspam
spam_m <- spam_m[sample(nrow(spam_m), n), ] # for spam

# random sampling to get indices of the training set
n = 100
index_ns=sample(which(nonspam_m$type=="nonspam"),n)
index_s=sample(which(spam_m$type=="spam"),n) 

# get training and test set
train_df = rbind(nonspam_m[index_ns,], spam_m[index_s, ]) 
test_df = rbind(nonspam_m[-index_ns,], spam_m[-index_s, ]) 

# select only features
train_df <- train_df[, -58]
test_df <- test_df[, -58]

# get class factor for training data
train_label= factor(c(rep("ns",n), rep("s",n)))
# get class factor for test data
test_label=factor(c(rep("ns",50), rep("s",50)))

## check duplicates by observation indices
# get train and test indices
train_indices = data.frame(as.integer(rownames(train_df)))
train_indices_check = train_indices[, 1]
test_indices = data.frame(as.integer(rownames(test_df)))
test_indeces_check = test_indices[, 1]

# to count cases if there are duplicates
count = 0 
# create a loop over test and train indices
# if there is a duplicate, count = count +1
for (i in train_indices_check) {
  for(j in test_indeces_check) {
    if (i == j){
      count = count +1
    }
  }
}

# if count == 0, there is no duplicate
if (count == 0){
  print('there is no duplicate')
  }else {
    print('there is a duplicate')
  }

############################ unstandardized features ############################

# populate k values and empthy accuracy to store values
k_values = c(1,9,25)
acc_trains_unscaled = c()
acc_tests_unscaled = c()

## classification using knn, with k=1
k = 1
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred
# we noticed all probabilities associated with each outcome
# equal to 1, as we take only one nearest 
# neighbor into the account

# get the accuracy when k = 1 
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_1_unscaled = mean(knn_pred == test_label)*100
acc_1_unscaled # 69% were correctly classified

# store accuracy when k = 1 
acc_tests_unscaled = append(acc_tests_unscaled, acc_1_unscaled)


## classification using knn, with k=9
k = 9
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred
# we noticed many probabilities associated with each outcome
# lower than 1, but it is not surprising as we set k = 9
# and surely there should be more than one type of label
# that have been taken into account

# get the accuracy when k = 9
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_9_unscaled = mean(knn_pred == test_label)*100
acc_9_unscaled # 70% were correctly classified

# store accuracy when k = 9 
acc_tests_unscaled = append(acc_tests_unscaled, acc_9_unscaled)


## classification using knn, with k=25
k = 25
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred
# we noticed many probabilities associated with each outcome
# lower than 1, but it is not surprising as we set k = 25
# and surely there should be more than one type of label
# that have been taken into account

# get the accuracy when k = 25
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_25_unscaled = mean(knn_pred == test_label) * 100
acc_25_unscaled # 75% were correctly classified

# store accuracy when k = 25
acc_tests_unscaled = append(acc_tests_unscaled, acc_25_unscaled)

## get the accuracy when tested with the train data set
## so that we can plot k vs. accuracy rates of results from 
## both test and train dataset, to analyse
## possible overfitting and underfitting issues

# k = 1
k = 1
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_1_unscaled = mean(knn_pred_train == train_label) *100
acc_train_1_unscaled # accuracy = 100%

# store accuracy when k = 1 
acc_trains_unscaled = append(acc_trains_unscaled, acc_train_1_unscaled)


# k = 9
k = 9
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_9_unscaled = mean(knn_pred_train == train_label) * 100
acc_train_9_unscaled # accuracy = 79.5%

# store accuracy when k = 9 
acc_trains_unscaled = append(acc_trains_unscaled, acc_train_9_unscaled)


# k = 25
k = 25
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_25_unscaled = mean(knn_pred_train == train_label) * 100
acc_train_25_unscaled # accuracy = 76%

# store accuracy when k = 25
acc_trains_unscaled = append(acc_trains_unscaled, acc_train_25_unscaled)


## plot accuracy rate when tested with train and test data set
# combine data into one dataframe
df_unscaled = data.frame( k_values, acc_trains_unscaled, acc_tests_unscaled) %>% 
  pivot_longer(cols = c(acc_trains_unscaled, acc_tests_unscaled),
               names_to = 'type', values_to = 'Accuracy_Rate')
# change specific row names to make reading easier
df_unscaled$type <- gsub("acc_trains_unscaled", "Train", df_unscaled$type)
df_unscaled$type <- gsub("acc_tests_unscaled", "Test", df_unscaled$type)

# visualize
ggplot(df_unscaled, aes(x = k_values, y= Accuracy_Rate, color= type )) + geom_line() + 
  geom_point() + ggtitle('Unscaled Features') +
  labs(y= "Accuracy Rate (%)", x = "Number of NN", color = "") 
   

############################ standardized features ################################
# check mean and sd of each feature
skim(train_df)
skim(test_df)

# as we have seen, some features (e.g. capitalTotal) have mean and standard deviation far away
# from the rest, hence we standardize all the features, to reduce disproportionate contribution
# to the calculated Euclidean distances in the knn
train_df = scale(train_df)
test_df = scale(test_df)

# populate k values and empthy accuracy to store values
k_values = c(1,9,25)
acc_trains_scaled = c()
acc_tests_scaled = c()

## classification using knn, with k=1
k = 1
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred 
# we noticed all probabilities associated with each outcome
# equal to 1, as we take only one nearest 
# neighbor into the account

# get the accuracy when k = 1
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_1_scaled = mean(knn_pred == test_label)*100
acc_1_scaled # 80% were correctly classified

# store accuracy when k = 1 
acc_tests_scaled = append(acc_tests_scaled, acc_1_scaled)


# classification using knn, with k=9
k = 9
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred
# we noticed many probabilities associated with each outcome
# lower than 1, but it is not surprising as we set k = 9
# and surely there should be more than one type of label
# that have been taken into account

# get the accuracy when k = 9
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_9_scaled = mean(knn_pred == test_label)*100
acc_9_scaled # 84% were correctly classified

# store accuracy when k = 9
acc_tests_scaled = append(acc_tests_scaled, acc_9_scaled)


# classification using knn, with k=25
k = 25
knn_pred=knn(train=train_df, test=test_df,cl=train_label, k=k, prob=TRUE) 
knn_pred

# get the accuracy when k = 25
table(knn_pred, test_label) # check true negative, positive and false negative, positive cases
acc_25_scaled = mean(knn_pred == test_label)*100
acc_25_scaled # 82% were correctly classified

# store accuracy when k = 25
acc_tests_scaled = append(acc_tests_scaled, acc_25_scaled)

## get the accuracy when tested with the train data set
## so that we can plot k vs. accuracy rates of results from 
## both test and train dataset, to analyse, to analyse
## possible overfitting and underfitting issues

# k = 1
k = 1
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_1_scaled = mean(knn_pred_train == train_label) * 100
acc_train_1_scaled
# accuracy = 100%

# store accuracy when k = 1 
acc_trains_scaled = append(acc_trains_scaled, acc_train_1_scaled)


# k = 9
k = 9
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_9_scaled = mean(knn_pred_train == train_label) * 100
acc_train_9_scaled
# accuracy = 91%

# store accuracy when k = 9
acc_trains_scaled = append(acc_trains_scaled, acc_train_9_scaled)


# k = 25
k = 25
knn_pred_train=knn(train=train_df, test=train_df,cl=train_label, k=k, prob=TRUE) 
acc_train_25_scaled = mean(knn_pred_train == train_label) * 100
acc_train_25_scaled
# accuracy = 86.5%

# store accuracy when k = 1 
acc_trains_scaled = append(acc_trains_scaled, acc_train_25_scaled)


## plot accuracy rate when tested with train and test data set
# combine data into one dataframe
df_scaled = data.frame( k_values, acc_trains_scaled, acc_tests_scaled) %>% 
  pivot_longer(cols = c(acc_trains_scaled, acc_tests_scaled),
               names_to = 'type', values_to = 'Accuracy_Rate')
# change specific row names to make reading easier
df_scaled$type <- gsub("acc_trains_scaled", "Train", df_scaled$type)
df_scaled$type <- gsub("acc_tests_scaled", "Test", df_scaled$type)

# visualize
ggplot(df_scaled, aes(x = k_values, y= Accuracy_Rate, color= type )) + geom_line() + 
  geom_point() + ggtitle('Scaled Features') +
  labs(y= "Accuracy Rate (%)", x = "Number of NN", color = "") 

