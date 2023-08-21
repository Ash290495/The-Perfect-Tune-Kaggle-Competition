setwd("C:/Users/naira_831868r/OneDrive/Desktop/Columbia/Fall '22/Frameworks & Mathods I/R_other")
songs = read.csv('analysisData.csv')
#Sample
model = lm(rating~ tempo+time_signature,songs)
pred_original_train=predict(model)
rmse=sqrt(mean((pred_original_train-songs$rating)^2))
rmse
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)
rmse_test=sqrt(mean((pred-scoringData$rating)^2))
rmse_test
submissionFile = data.frame(id = scoringData$id, rating = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#Studying the dataset
library(stringr)
library(skimr)
str(songs)
skim(songs)
head(songs)

#Counting distinct values
library(dplyr)
songs_check1<-songs %>%
  summarize(count=n_distinct(genre))
songs_check1 #No meaning as each line would still have a different combination of individual genres

songs_check2<-songs %>%
  summarize(count=n_distinct(performer))
songs_check2

songs_check3<-songs %>%
  summarize(count=n_distinct(song))
songs_check3

#Divide songs into train & test & run lm
library(caret)
library(ggplot2)
library(lattice)
set.seed(1031)
split = createDataPartition(y = songs$rating, p = 0.7, list = F,groups = 100)
train = songs[split,]
test = songs[-split,]
model2 =lm(rating~.,data=train) #Don't use (Long computation time probably on account of too many k-1 dummy variables)

#LM model without id, performer, song, genre
model3 = lm(rating~track_duration+as.factor(track_explicit)+danceability+energy+as.factor(key)+loudness+as.factor(mode)+speechiness+acousticness+instrumentalness+liveness+valence+tempo+as.factor(time_signature), 
             data = train)
pred3 = predict(model3)
rmse3 = sqrt(mean((pred3-train$rating)^2))
rmse3
pred3_test = predict(model3, newdata=test)
rmse3_test = sqrt(mean((pred3_test-test$rating)^2))
rmse3_test
#RMSE train = 15.67013
#RMSE test = 15.60728
#RMSE scoringData (Public: 15.53982, Private: 15.68619)

#GAM model without id, performer, song, genre
library(mgcv)
library(nlme)
model4 = gam(rating~s(track_duration)+as.factor(track_explicit)+s(danceability)+s(energy)+as.factor(key)+s(loudness)+as.factor(mode)+s(speechiness)+s(acousticness)+s(instrumentalness)+s(liveness)+s(valence)+s(tempo)+as.factor(time_signature),
             method = 'REML', 
             data = train)
pred4 = predict(model4)
rmse4 = sqrt(mean((pred4-train$rating)^2))
rmse4
pred4_test = predict(model4, newdata=test)
rmse4_test = sqrt(mean((pred4_test-test$rating)^2))
rmse4_test
#RMSE train = 15.49728
#RMSE test = 15.41666
#Better RMSEs than LM
#RMSE scoringData (Public: 15.39936, Private: 15.56732)


#Check statistical significance until now
summary(model)
summary(model3)
summary(model4)

#Exploratory data analysis
#1.track_duration vs. rating
library(dplyr)
songs2<-songs%>%
  mutate(track_duration2=track_duration/1000000)
library(ggplot2)
set.seed(1031)
split_trial = createDataPartition(y = songs2$rating, p = 0.7, list = F,groups = 100)
train_trial = songs2[split_trial,]
test_trial = songs2[-split_trial,]
cor(train_trial$track_duration2, train_trial$rating)
 #Checking polynomial relationship
library(gridExtra)
plot_poly = 
  ggplot(aes(x=track_duration2,y=rating),data=train_trial)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=track_duration2,y=rating),data=train_trial)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#2.track_explicit vs. rating
cor(train$track_explicit, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=track_explicit,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=track_explicit,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#3.danceability vs. rating
cor(train$danceability, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=danceability,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=danceability,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#4.energy vs. rating
cor(train$energy, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=energy,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=energy,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#5.key vs. rating
cor(train$key, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=key,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=key,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#6.loudness vs. rating
cor(train$loudness, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=loudness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=loudness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#7.speechiness vs. rating
cor(train$speechiness, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=speechiness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=speechiness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#8.acousticness vs. rating
cor(train$acousticness, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=acousticness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=acousticness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#9.instrumentalness vs. rating
cor(train$instrumentalness, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=instrumentalness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=instrumentalness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#10.liveness vs. rating
cor(train$liveness, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=liveness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=liveness,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#11.valence vs. rating
cor(train$valence, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=valence,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=valence,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#12.tempo vs. rating
cor(train$tempo, train$rating)
#Checking polynomial relationship
plot_poly = 
  ggplot(aes(x=tempo,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~poly(x,2),size=1.3,se=FALSE, color='tomato')+
  theme_bw()
plot_lm = 
  ggplot(aes(x=tempo,y=rating),data=train)+
  geom_point(color = 'darkgray')+
  geom_smooth(method="lm", formula=y~x,size=1.3,se=FALSE, color='cadetblue')+
  theme_bw()
grid.arrange(plot_poly, plot_lm, ncol=2)

#Reducing genre within genre2
library(stringr)
songs$genre2 = songs$genre
for (i in 1:length(songs$genre))
{songs$genre2[i] <- ifelse(str_detect(songs$genre[i],'a capella'),'a capella',
                           ifelse(str_detect(songs$genre[i],'acid house'),'acid house',
                                  ifelse(str_detect(songs$genre[i],'jazz'),'jazz',
                                         ifelse(str_detect(songs$genre[i],'pop'),'pop',
                                                ifelse(str_detect(songs$genre[i],'adult standards'),'adult standards',
                                                       ifelse(str_detect(songs$genre[i],'afrobeat'),'afrobeat',
                                                              ifelse(str_detect(songs$genre[i],'afropop'),'afropop',
                                                                     ifelse(str_detect(songs$genre[i],'rock'),'rock',
                                                                            ifelse(str_detect(songs$genre[i],'country'),'country',
                                                                                   ifelse(str_detect(songs$genre[i],'dance'),'dance',
                                                                                          ifelse(str_detect(songs$genre[i],'emo'),'emo',
                                                                                                 ifelse(str_detect(songs$genre[i],'hip hop'),'hip hop',
                                                                                                        ifelse(str_detect(songs$genre[i],'metal'),'metal',
                                                                                                               ifelse(str_detect(songs$genre[i],'r & b'),'r & b',
                                                                                                                      ifelse(str_detect(songs$genre[i],'folk'),'folk',
                                                                                                                             ifelse(str_detect(songs$genre[i],'worship'),'worship',
                                                                                                                                    ifelse(str_detect(songs$genre[i],'latin'),'latin',
                                                                                                                                           ifelse(str_detect(songs$genre[i],'rap'),'rap',
                                                                                                                                                  ifelse(str_detect(songs$genre[i],'beach music'),'beach music',
                                                                                                                                                         ifelse(str_detect(songs$genre[i],'edm'),'edm',
                                                                                
                                                                                                                                                             
                                                                                                                                                                ifelse(str_detect(songs$genre[i],'blues'),'blues',
                                                                                                                                                                       ifelse(str_detect(songs$genre[i],'classic'),'classic',
                                                                                                                                                                              ifelse(str_detect(songs$genre[i],'funk'),'funk',
                                                                                                                                                                                     ifelse(str_detect(songs$genre[i],'disco'),'disco',
                                                                                                                                                                                            ifelse(str_detect(songs$genre[i],'instrumental'),'instrumental','others')))))))))))))))))))))))))}

#Convert NAs in Genre to No Genre
songs$genre2[is.na(songs$genre2)]  <- "No Genre"
colSums(is.na(songs))

#New train & test
set.seed(1031)
split_new = createDataPartition(y = songs$rating, p = 0.7, list = F,groups = 100)
train_new = songs[split_new,]
test_new = songs[-split_new,]

#LM model using genre
model5 = lm(rating~as.factor(genre2)+track_duration+as.factor(track_explicit)+danceability+energy+as.factor(key)+loudness+as.factor(mode)+speechiness+acousticness+instrumentalness+liveness+valence+tempo+as.factor(time_signature), 
            data = train_new)
summary(model5)
pred5 = predict(model5)
rmse5 = sqrt(mean((pred5-train_new$rating)^2))
rmse5
pred5_test_new = predict(model5, newdata=test_new)
rmse5_test_new = sqrt(mean((pred5_test_new-test_new$rating)^2))
rmse5_test_new
#RMSE train = 15.19378
#RMSE test = 15.1437
#Better RMSEs than GAM without genre
#Submit model5
#Reducing scoringData
scoringData$genre2 = scoringData$genre
for (i in 1:length(scoringData$genre))
{scoringData$genre2[i] <- ifelse(str_detect(scoringData$genre[i],'a capella'),'a capella',
                           ifelse(str_detect(scoringData$genre[i],'acid house'),'acid house',
                                  ifelse(str_detect(scoringData$genre[i],'jazz'),'jazz',
                                         ifelse(str_detect(scoringData$genre[i],'pop'),'pop',
                                                ifelse(str_detect(scoringData$genre[i],'adult standards'),'adult standards',
                                                       ifelse(str_detect(scoringData$genre[i],'afrobeat'),'afrobeat',
                                                              ifelse(str_detect(scoringData$genre[i],'afropop'),'afropop',
                                                                     ifelse(str_detect(scoringData$genre[i],'rock'),'rock',
                                                                            ifelse(str_detect(scoringData$genre[i],'country'),'country',
                                                                                   ifelse(str_detect(scoringData$genre[i],'dance'),'dance',
                                                                                          ifelse(str_detect(scoringData$genre[i],'emo'),'emo',
                                                                                                 ifelse(str_detect(scoringData$genre[i],'hip hop'),'hip hop',
                                                                                                        ifelse(str_detect(scoringData$genre[i],'metal'),'metal',
                                                                                                               ifelse(str_detect(scoringData$genre[i],'r & b'),'r & b',
                                                                                                                      ifelse(str_detect(scoringData$genre[i],'folk'),'folk',
                                                                                                                             ifelse(str_detect(scoringData$genre[i],'worship'),'worship',
                                                                                                                                    ifelse(str_detect(scoringData$genre[i],'latin'),'latin',
                                                                                                                                           ifelse(str_detect(scoringData$genre[i],'rap'),'rap',
                                                                                                                                                  ifelse(str_detect(scoringData$genre[i],'beach music'),'beach music',
                                                                                                                                                         ifelse(str_detect(scoringData$genre[i],'edm'),'edm',
                                                                                                                                                                ifelse(str_detect(scoringData$genre[i],'blues'),'blues',
                                                                                                                                                                       ifelse(str_detect(scoringData$genre[i],'classic'),'classic',
                                                                                                                                                                              ifelse(str_detect(scoringData$genre[i],'funk'),'funk',
                                                                                                                                                                                     ifelse(str_detect(scoringData$genre[i],'disco'),'disco',
                                                                                                                                                                                            ifelse(str_detect(scoringData$genre[i],'instrumental'),'instrumental','others')))))))))))))))))))))))))}
scoringData$genre2[is.na(scoringData$genre2)]  <- "No Genre"
colSums(is.na(scoringData))
#For submission
pred5_scoringData = predict(model5,newdata=scoringData)
submissionFile4 = data.frame(id = scoringData$id, rating = pred5_scoringData)
write.csv(submissionFile4, 'submission4.csv',row.names = F)
#RMSE scoringData (Public: 15.21471, Private: 15.23895)


#GAM model using genre
model6 = gam(rating~as.factor(genre2)+s(track_duration)+as.factor(track_explicit)+s(danceability)+s(energy)+as.factor(key)+s(loudness)+as.factor(mode)+s(speechiness)+s(acousticness)+s(instrumentalness)+s(liveness)+s(valence)+s(tempo)+as.factor(time_signature),
             method = 'REML', 
             data = train_new)
summary(model6)
pred6 = predict(model6)
rmse6 = sqrt(mean((pred6-train_new$rating)^2))
rmse6
pred6_test_new = predict(model6, newdata=test_new)
rmse6_test_new = sqrt(mean((pred6_test_new-test_new$rating)^2))
rmse6_test_new
#RMSE train = 15.05243
#RMSE test = 15.01546
#Better RMSEs than LM with genre
pred6_scoringData = predict(model6,newdata=scoringData)
submissionFile5 = data.frame(id = scoringData$id, rating = pred6_scoringData)
write.csv(submissionFile5, 'submission5.csv',row.names = F)
#RMSE scoringData (Public: 15.10403, Private: 15.12758)


#Feature selection (summary & 1 upload)


#Bivariate filter 
library(ggcorrplot)
ggcorrplot(cor(train_new),
           method = 'square',
           type = 'lower',
           show.diag = F,
           colors = c('#e9a3c9', '#f7f7f7', '#a1d76a'))

#Multivariate filter 
library(car)
vif(model6)

data.frame(Predictor = names(vif(model6)), VIF = vif(model6)) %>%
  ggplot(aes(x=VIF, y = reorder(Predictor, VIF), fill=VIF))+
  geom_col()+
  geom_vline(xintercept=5, color = 'gray', size = 1.5)+
  geom_vline(xintercept = 10, color = 'red', size = 1.5)+
  scale_fill_gradient(low = '#fff7bc', high = '#d95f0e')+
  scale_y_discrete(name = "Predictor")+
  scale_x_continuous(breaks = seq(5,30,5))+
  theme_classic()

#as.factor for character variables
songs$genre2=as.factor(songs$genre2)
songs$track_explicit=as.factor(songs$track_explicit)
songs$key=as.factor(songs$key)
songs$mode=as.factor(songs$mode)
songs$time_signature=as.factor(songs$time_signature)

scoringData$genre2=as.factor(scoringData$genre2)
scoringData$track_explicit=as.factor(scoringData$track_explicit)
scoringData$key=as.factor(scoringData$key)
scoringData$mode=as.factor(scoringData$mode)
scoringData$time_signature=as.factor(scoringData$time_signature)

#Revised train & test
set.seed(1031)
split_latest = createDataPartition(y = songs$rating, p = 0.7, list = F,groups = 100)
train_latest = songs[split_latest,]
test_latest = songs[-split_latest,]

#Best Subset selection
library(leaps)
subsets = regsubsets(rating~.-id-performer-song-genre,
                     data=train_latest, 
                     nvmax=15)
summary(subsets)
coef(subsets,which.min(summary(subsets)$cp))

model7 = gam(rating~s(track_duration)+s(danceability)+s(energy)+s(loudness)+s(acousticness)+s(instrumentalness)+s(valence)+time_signature+genre2,
             method = 'REML', 
             data = train_latest)
summary(model7)
pred7 = predict(model7)
rmse7 = sqrt(mean((pred7-train_latest$rating)^2))
rmse7

pred7_test_latest = predict(model7, newdata=test_latest)
rmse7_test_latest = sqrt(mean((pred7_test_latest-test_latest$rating)^2))
rmse7_test_latest
#RMSE train = 15.08922
#RMSE test = 15.02434 
#RMSEs did not improve over regular GAM with genre
pred7_scoringData = predict(model7,newdata=scoringData)
submissionFile8 = data.frame(id = scoringData$id, rating = pred7_scoringData)
write.csv(submissionFile8, 'submission8.csv',row.names = F)
#RMSE scoringData (Public: 15.42317, Private: 15.31466)


#Forward selection
start_mod = lm(rating~1,data=train_latest)
empty_mod = lm(rating~1,data=train_latest)
full_mod = lm(rating~.-id-performer-song-genre,
               data=train_latest)
forwardStepwise = step(start_mod,
                       scope=list(upper=full_mod,lower=empty_mod),
                       direction='forward')
model8 = gam(rating~genre2 + s(acousticness) + s(track_duration) + s(loudness) + 
               s(danceability) + s(valence) + s(energy) + time_signature + key + 
               s(instrumentalness) + s(liveness) + s(tempo),
             method = 'REML', 
             data = train_latest)
summary(model8)
pred8 = predict(model8)
rmse8 = sqrt(mean((pred8-train_latest$rating)^2))
rmse8
pred8_test_latest = predict(model8, newdata=test_latest)
rmse8_test_latest = sqrt(mean((pred8_test_latest-test_latest$rating)^2))
rmse8_test_latest
#RMSE train = 15.05941
#RMSE test = 15.01694 
#RMSEs did not improve over regular GAM with genre although better than Best Subset


#Backward selection
start_mod2 = lm(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,data=train_latest)
empty_mod2 = lm(rating~1,data=train_latest)
full_mod2 = lm(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,data=train_latest)
backwardStepwise = step(start_mod2,
                       scope=list(upper=full_mod2,lower=empty_mod2),
                       direction='backward')
model9 = gam(rating~genre2 + s(acousticness) + s(track_duration) + s(loudness) + 
               s(danceability) + s(valence) + s(energy) + time_signature + key + 
               s(instrumentalness) + s(liveness) + s(tempo),
             method = 'REML', 
             data = train_latest)
summary(model9)
pred9 = predict(model9)
rmse9 = sqrt(mean((pred9-train_latest$rating)^2))
rmse9
pred9_test_latest = predict(model9, newdata=test_latest)
rmse9_test_latest = sqrt(mean((pred9_test_latest-test_latest$rating)^2))
rmse9_test_latest
#RMSE train = 15.05941
#RMSE test = 15.01694 
#RMSEs did not improve over regular GAM with genre although better than Best Subset (Similar to Forward selection)


#Stepwise selection
start_mod3 = lm(rating~1,data=train_latest)
empty_mod3 = lm(rating~1,data=train_latest)
full_mod3 = lm(rating~.-id-performer-song-genre,data=train_latest)
hybridStepwise = step(start_mod3,
                      scope=list(upper=full_mod3,lower=empty_mod3),
                      direction='both')
model10 = gam(rating~genre2 + s(acousticness) + s(track_duration) + s(loudness) + 
               s(danceability) + s(valence) + s(energy) + time_signature + key + 
               s(instrumentalness) + s(liveness) + s(tempo),
             method = 'REML', 
             data = train_latest)
summary(model10)
pred10 = predict(model10)
rmse10 = sqrt(mean((pred10-train_latest$rating)^2))
rmse10
pred10_test_latest = predict(model10, newdata=test_latest)
rmse10_test_latest = sqrt(mean((pred10_test_latest-test_latest$rating)^2))
rmse10_test_latest
#RMSE train = 15.05941
#RMSE test = 15.01694 
#RMSEs did not improve over regular GAM with genre although better than Best Subset (Similar to Forward selection)

pred10_scoringData = predict(model10,newdata=scoringData)
submissionFile6 = data.frame(id = scoringData$id, rating = pred10_scoringData)
write.csv(submissionFile6, 'submission6.csv',row.names = F)
#RMSE scoringData (Public: 15.1109, Private: 15.13924)

#Ridge regression (Did not use as summary results unreadable)
library(glmnet)
x = model.matrix(rating~.-id-performer-song,data=train_new)
y = train_new$rating
set.seed(617)
ridge = glmnet(x = x, 
               y = y, 
               alpha = 0)

#Lasso regression
x = model.matrix(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature-1,data=train_latest)
y = train_latest$rating
library(glmnet)
set.seed(617)
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')
coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)
model11 = gam(rating~genre2+s(track_duration)+track_explicit+s(danceability)+key+s(loudness)+s(acousticness)+s(instrumentalness)+s(liveness)+s(valence)+time_signature,
              method = 'REML', 
              data = train_latest)
summary(model11)
pred11 = predict(model11)
rmse11 = sqrt(mean((pred11-train_latest$rating)^2))
rmse11
pred11_test_latest = predict(model11, newdata=test_latest)
rmse11_test_latest = sqrt(mean((pred11_test_latest-test_latest$rating)^2))
rmse11_test_latest
#RMSE train = 15.09219
#RMSE test = 15.07019
#RMSEs worsened

pred11_scoringData = predict(model11,newdata=scoringData)
submissionFile10 = data.frame(id = scoringData$id, rating = pred10_scoringData)
write.csv(submissionFile10, 'submission10.csv',row.names = F)
#RMSE scoringData (Public: 15.1109, Private: 15.13924)

#Dimension reduction (Not performing as only components will be seen, no predictors can be obtained)
trainPredictors = train_new[,-17]
testPredictors = test_new[,-17]

pca = prcomp(trainPredictors,scale. = T)
train_components = data.frame(cbind(pca$x[,1:6], rating = train_new$rating))

train_model = lm(rating~.,train_components)
summary(train_model)

test_pca = predict(pca,newdata=testPredictors)
test_components = data.frame(cbind(test_pca[,1:6], rating = test_new$rating))

pred_pca = predict(train_model,newdata=test_components)
sse = sum((pred_pca-test_components$rating)^2)
sst = sum((mean(train_componentsrating) - test_components$rating)^2)
r2_test_pca = 1 - sse/sst
r2_test_pca

#Decision trees
#Trees do their own feature selection & my best feature selection method overfits the scoring data, therefore using all variables
#Using Regression tree as the outcome variables is numeric
library(rpart)
library(rpart.plot)
tree1 = rpart(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,data = train_latest, method = 'anova')
rpart.plot(tree1)
tree1$variable.importance
pred_tree1 = predict(tree1)
rmse_tree1 = sqrt(mean((pred_tree1 - train_latest$rating)^2))
rmse_tree1
pred_tree1_test = predict(tree1, newdata = test_latest)
rmse_test1_test = sqrt(mean((pred_tree1_test - test_latest$rating)^2))
rmse_test1_test
#RMSE train = 15.52529
#RMSE test = 15.42112
#RMSEs worsened

#Advanced Trees

#Tuned tree
tuneGrid = expand.grid(cp = seq(0,0.1,0.0001))
library(caret)
trControl = trainControl(method = 'cv',number = 5)
set.seed(1031)
tree_cv = train(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
                data = train_latest,
                method = 'rpart',
                trControl = trControl, 
                tuneGrid = tuneGrid)
tree_cv$bestTune

cvtree = rpart(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature, 
               data = train_latest, 
               method = 'anova', 
               cp = tree_cv$bestTune)
pred_tuned_train = predict(cvtree)
rmse_tuned_train = sqrt(mean((pred_tuned_train - train_latest$rating)^2))
rmse_tuned_train
pred_tuned_test = predict(cvtree, newdata = test_latest)
rmse_tuned_test = sqrt(mean((pred_tuned_test - test_latest$rating)^2))
rmse_tuned_test
#RMSE train = 15.18502
#RMSE test = 15.26486
#RMSEs did not improve

#Save latest songs & Scroring Data to recover if code aborts (call files & run latest train & test)
write.csv(songs, 'songs_changed.csv',row.names = F)
write.csv(scoringData, 'scoringData_changed.csv',row.names = F)

#Bag using ipred
library(ipred)
set.seed(1031) 
bag_ipred = bagging(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
              data = train_latest, 
              nbagg = 100)
pred_bag_train = predict(bag_ipred)
rmse_train_bag_ipred = sqrt(mean((pred_bag_train - train_latest$rating)^2))
rmse_train_bag_ipred
pred_bag_test = predict(bag_ipred, newdata = test_latest)
rmse_test_bag_ipred = sqrt(mean((pred_bag_test - test_latest$rating)^2))
rmse_test_bag_ipred
#RMSE train = 15.48575
#RMSE test = 15.34106
#RMSEs did not improve

#Bag using randomForest
library(randomForest)
set.seed(1031)
bag = randomForest(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature, 
                   data=train_latest, 
                   mtry = 15,
                   ntree = 100)
pred_bag_randomForest_train = predict(bag)
rmse_train_bag_randomforest = sqrt(mean((pred_bag_randomForest_train - train_latest$rating)^2))
rmse_train_bag_randomforest
pred_bag_randomForest_test = predict(bag, newdata = test_latest)
rmse_bag_randomforest_test = sqrt(mean((pred_bag_randomForest_test - test_latest$rating)^2))
rmse_bag_randomforest_test
#RMSE train = 15.26016
#RMSE test = 15.16662
#RMSEs did not improve

#Using randomForest library
library(randomForest)
set.seed(1031)
forest = randomForest(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature, 
                      data=train_latest, 
                      ntree = 100)
pred_rf_randomForest_train = predict(forest)
rmse_train_rf_randomForest = sqrt(mean((pred_rf_randomForest_train - train_latest$rating)^2))
rmse_train_rf_randomForest
pred_rf_randomForest_test = predict(forest, newdata= test_latest)
rmse_test_rf_randomForest = sqrt(mean((pred_rf_randomForest_test - test_latest$rating)^2))
rmse_test_rf_randomForest
#RMSE train = 15.1786
#RMSE test = 15.11212
#RMSEs did not improve

pred_rf_randomForest_scoringData = predict(forest,newdata=scoringData)
submissionFile9 = data.frame(id = scoringData$id, rating = pred_rf_randomForest_scoringData)
write.csv(submissionFile9, 'submission9.csv',row.names = F)
#RMSE scoringData (Public: 15.07688, Private: 15.10607)

#Tuned randomForest
trControl_tuned_rf = trainControl(method = 'cv', number = 5)
tuneGrid_tuned_rf = expand.grid(mtry = 1:15)
set.seed(1031)
forest_cv = train(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature, 
                  data = train_latest, 
                  method = 'rf', 
                  trControl = trControl_tuned_rf, 
                  tuneGrid = tuneGrid_tuned_rf, 
                  ntree = 100)
forest_cv$bestTune$mtry

set.seed(1031)
cvforest = randomForest(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature, 
                        data = train_latest, 
                        mtry = forest_cv$bestTune$mtry, 
                        ntree = 600)
pred_tuned_rf_train = predict(cvforest)
rmse_train_tuned_rf = sqrt(mean((pred_tuned_rf_train - train_latest$rating)^2))
rmse_train_tuned_rf
pred_tuned_rf_test = predict(cvforest, newdata= test_latest)
rmse_test_tuned_rf = sqrt(mean((pred_tuned_rf_test - test_latest$rating)^2))
rmse_test_tuned_rf
#RMSE Train: 15.01917
#RMSE Test: 15.01529
#RMSE improved

#Equating levels of songs & scoringData
levels(scoringData$time_signature) <- levels(songs$time_signature)
levels(scoringData$time_explicit) <- levels(songs$time_explicit)
levels(scoringData$key) <- levels(songs$key)
levels(scoringData$mode) <- levels(songs$mode)
levels(scoringData$genre2) <- levels(songs$genre2)

pred_tuned_rf_scoringData = predict(cvforest,newdata=scoringData)
submissionFile7 = data.frame(id = scoringData$id, rating = pred_tuned_rf_scoringData)
write.csv(submissionFile7, 'submission7.csv',row.names = F)
#RMSE scoringData (Public: 15.02365, Private: 15.04927)

#Using ranger (Overfits)
library(ranger)
set.seed(1031)
forest_ranger = ranger(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
                       data = train_latest, 
                       num.trees = 100)
pred_ranger_train = predict(forest_ranger, data = train_latest, num.trees = 100)
rmse_train_forest_ranger = sqrt(mean((pred_ranger_train$predictions - train_latest$rating)^2))
rmse_train_forest_ranger
pred_ranger_test = predict(forest_ranger, data = test_latest, num.trees = 100)
rmse_test_forest_ranger = sqrt(mean((pred_ranger_test$predictions - test_latest$rating)^2))
rmse_test_forest_ranger
#RMSE train = 6.678818
#RMSE test = 15.07838
#Overfitting

#Tuned ranger (Overfits)
trControl_tuned_ranger=trainControl(method="cv",number=5)
tuneGrid_tuned_ranger = expand.grid(mtry=1:5, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10))
set.seed(1031)
cvModel = train(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
                data=train_latest,
                method="ranger",
                num.trees=50,
                trControl=trControl_tuned_ranger,
                tuneGrid=tuneGrid_tuned_ranger)
cvModel$bestTune
set.seed(1031)
cv_forest_ranger = ranger(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
                          data=train_latest,
                          num.trees = 500, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule)

pred_tuned_ranger_train = predict(cv_forest_ranger, data = train_latest, num.trees = 500)
rmse_train_cv_forest_ranger_tuned = sqrt(mean((pred_tuned_ranger_train$predictions - train_latest$rating)^2)) 
rmse_train_cv_forest_ranger_tuned
pred_tuned_ranger_test = predict(cv_forest_ranger, data = test_latest, num.trees = 500)
rmse_test_cv_forest_ranger = sqrt(mean((pred_tuned_ranger_test$predictions - test_latest$rating)^2))
rmse_test_cv_forest_ranger
#RMSE train = 7.653822
#RMSE test = 15.01312
#Overfitting

#Boost
#gbm (overfits)
library(gbm)
set.seed(617)
boost = gbm(rating~genre2+track_duration+track_explicit+danceability+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+tempo+time_signature,
            data=train_latest,
            distribution="gaussian",
            n.trees = 500)
pred_boost_train = predict(boost, n.trees=500)
rmse_train_boost = sqrt(mean((pred_boost_train - train_latest$rating)^2))
rmse_train_boost
pred_boost_test = predict(boost, newdata = test_latest, n.trees = 500)
rmse_boost_test = sqrt(mean((pred_boost_test - test_latest$rating)^2))
rmse_boost_test
#RMSE train = 14.94594
#RMSE test = 15.05027
#Overfits

#Tuned gbm (Hold)
train_latest2 = train_latest[, c(1:18,20,19)]
test_latest2 = test_latest[, c(1:18,20,19)]

#xgboost
library(vtreat)
trt = designTreatmentsZ(dframe = train_latest,
                        varlist = names(train_latest2)[5:19])
newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train_latest2,
                      varRestriction = newvars)
test_input = prepare(treatmentplan = trt, 
                     dframe = test_latest2,
                     varRestriction = newvars)
library(xgboost)
library(caret)
set.seed(617)
tune_nrounds = xgb.cv(data=as.matrix(train_input), 
                      label = train_latest2$rating,
                      nrounds=100,
                      nfold = 5,
                      verbose = 0)

xgboost2= xgboost(data=as.matrix(train_input), 
                  label = train_latest2$rating,
                  nrounds=13,
                  verbose = 0)

pred_xgboost_train = predict(xgboost2, 
                       newdata=as.matrix(train_input))
rmse_xgboost_train = sqrt(mean((pred_xgboost_train - train_latest2$rating)^2))
rmse_xgboost_train
#rmse_xgboost = sqrt(mean((pred_xgboost - test_latest2$rating)^2))
#rmse_xgboost


pred_xgboost = predict(xgboost2, 
               newdata=as.matrix(test_input))
rmse_xgboost = sqrt(mean((pred_xgboost - test_latest2$rating)^2))
rmse_xgboost

which.min(tune_nrounds$evaluation_log$test_rmse_mean)
#RMSE Train = 13.94033
#RMSE Test = 15.11771
#Overfitting
