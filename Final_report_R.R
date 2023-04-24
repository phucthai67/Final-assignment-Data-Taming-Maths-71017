setwd("C:/Users/thain/Google Drive/Adelaide uni/Maths7107/Final report")
library(readr)
library(ggplot2)
library(tidymodels)
library(tidyverse)
library(RColorBrewer)
library(inspectdf)
library(discrim)
library(kknn)
library(vip)
library(forcats)
library(inspectdf)
###Importing and cleaning data
?Sys.Date()
spotify_songs<-read.csv("spotify_songs.csv")%>%
  dplyr::select(-X)

spotify_songs <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
View(spotify_songs)
song_data<-spotify_songs%>%
  dplyr::select(-track_id,-track_album_id,-playlist_id,
                -track_name,-track_album_name,
                -playlist_subgenre,-playlist_name)

View(song_data)
View(spotify_songs%>%
  group_by(track_artist,playlist_genre)%>%
  count())

View(spotify_songs%>%
       group_by(track_artist)%>%
       count()%>%
       filter(n>5))
View(song_data%>%
       group_by(playlist_name)%>%
       count()%>%
       filter(n>1))
ncol(song_data)
  song_data$track_artist
  

View(song_data%>%
       group_by(track_artist)%>%
       count()%>%
       filter(n>10)%>%
       ungroup())

song_artist<-song_data%>%
  add_count(track_artist)%>%
    filter(n>=10)



sum(artist_check$n)  
View(song_data)
View(spotify_songs)
song_data_cleaned<-song_data%>%
  mutate(release_year=factor(substr(track_album_release_date,
                                        1,4 )),                            
         genre=factor(playlist_genre))%>%
  dplyr::select(-track_album_release_date ,-playlist_genre)

song_artist<-song_data_cleaned%>%
  add_count(track_artist)%>%
  filter(n>=30)%>%
  select(-n)%>%
  mutate(track_artist=factor(track_artist))
nrow(song_artist)
song_artist%>%
  group_by(genre)%>%
  count()

View(song_artist)
View(spotify_songs%>%
  group_by(track_artist,playlist_genre)%>%
  summarize(count=n()))
nrow(song_data_cleaned)
View(spotify_songs)

## EDA

#Popularity vs genre

song_data_cleaned%>%
  ggplot(aes(x=genre,y=track_popularity,fill=genre))+
  geom_boxplot()
  

#Speechiness vs genre
song_data_cleaned%>%
  ggplot(aes(x=genre,y=speechiness,fill=genre))+
  geom_boxplot()
  

## Popularity over time

song_data_cleaned%>%
  ggplot(aes(y=track_popularity,x=as.factor(release_year)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = -90, hjust=0),
        legend.position="none")





song_data_cleaned%>%
            add_count(release_year,
              name='year_count')%>%
  filter(year_count>150)%>%
  ggplot(aes(x=as.factor(release_year),y=track_popularity,
             fill=as.numeric(release_year)))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = -90, hjust=0),
        legend.position="none")+
  xlab('release year')+
  scale_fill_gradient(low='lightblue',high='lightblue')
  


## And the other 2 predictors that was not asked for specifically

song_data_cleaned%>%
  ggplot(aes(y=tempo,x=genre,fill=genre))+
  geom_boxplot()

song_data_cleaned%>%
  ggplot(aes(y=danceability,x=genre,fill=genre))+
  geom_boxplot()

song_data_cleaned%>%
  add_count(release_year,
            name='year_count')%>%
  filter(year_count>50)%>%
  ggplot(aes(x = release_year, 
                                fill = genre)) + 
  geom_bar(position = "fill", col = "black") + 
  theme(axis.text.x = element_text(angle = -90, hjust=0))

View(spotify_songs)
summary(song_data_cleaned)


unique(song_data_cleaned$genre)

View(song_data_cleaned)

## Building models

# extracting the data for analysis
set.seed(1846505)
summary(song_data_cleaned)
song_data1<-song_data_cleaned%>%
  group_by(genre)%>%
  slice_sample(n=1000)
release_year_lumped=tibble(
  release_year_lumped=fct_lump_min(song_data1$release_year,15))
song_data1<-cbind(song_data1,release_year_lumped)
View(summary(song_data1$release_year))
View(summary(release_year_lumped$release_year_lumped))

# Splitting data
set.seed(1846505)
song_split<-initial_split(song_data1,strata=genre)
song_train<-training(song_split)
song_test<-testing(song_split)
View(song_train)


# Splitting artist
set.seed(1846505)
View(song_artist)
song_artist%>%
  mutate(track_artist=factor(track_artist))
artist_split<-initial_split(song_artist,strata=track_artist)
?view(initial_split)
artist_train<-training(artist_split)
artist_test<-testing(artist_split)



# Preprocessing data
colnames(song_train)
# train
song_recipe<-recipe(genre~.,data=song_train)%>%
  step_normalize(danceability,tempo,speechiness,track_popularity,
                 loudness,mode,energy,key,acousticness,
                 instrumentalness,liveness,valence,
                 duration_ms)%>%
  step_corr()%>%
  prep()

artist_recipe<-recipe(genre~.,data=artist_train)%>%
  step_normalize(danceability,tempo,speechiness,track_popularity,
                 loudness,mode,energy,key,acousticness,
                 instrumentalness,liveness,valence,
                 duration_ms)%>%
  step_corr()%>%
  prep()



song_train_processed<-juice(song_recipe)%>%
  mutate(release_year=factor(release_year),
         release_year_lumped=factor(release_year_lumped))
artist_train_processed<-juice(artist_recipe)%>%
  mutate(release_year=factor(release_year),
         track_artist=factor(track_artist))
summary(artist_train_processed)
#test
song_test_processed<-bake(song_recipe,song_test)%>%
  mutate(release_year=factor(release_year),
         release_year_lumped=factor(release_year_lumped))

artist_test_processed<-bake(artist_recipe,artist_test)%>%
  mutate(release_year=factor(release_year),
         track_artist=factor(track_artist))
         
summary(song_train_processed)

View(summary(song_test_processed$release_year))
View(summary(song_train_processed$release_year))
## Bootstrapping data for tuning
set.seed(1846505)
song_boots <- bootstraps( song_train_processed, times = 10,  strata = genre )

song_data1%>%
  group_by(release_year)%>%
  count()%>%
  filter(n<21)%>%
  ggplot(aes(x=release_year,y=n))+
  geom_col()+
  geom_hline(yintercept = 15,color='red')+
  theme(axis.text.x = element_text(angle = -90, hjust=0))

?geom_hline
summary(song_train$release_year_lumped)
summary(song_train$release_year)
##LDA
lda_spec<- discrim_linear( mode = "classification" ) %>% 
  set_engine( "MASS" )
?discrim_linear
##KNN
nrow(song_test)
knn_spec<-nearest_neighbor( mode = "classification", 
                                    neighbors = tune())%>%
  set_engine('kknn')

artist_knn_spec<-nearest_neighbor( mode = "classification", 
                            neighbors = 100)%>%
  set_engine('kknn')
neighbor_grid <- grid_regular( neighbors(range=c(1,100)),
                               levels = 20)
doParallel::registerDoParallel()
knn_tuned <- tune_grid( object = knn_spec,
                       preprocessor = recipe(genre ~ . ,
                                    data = song_train_processed%>%
                                      dplyr::select(-release_year)),
                       resamples = song_boots,
                       grid = neighbor_grid )

best_knn_auc <- select_best( knn_tuned, "roc_auc" )

final_knn_song <- finalize_model( knn_spec, best_knn_auc )



## Random forest
rf_spec <- rand_forest( mode = "classification",
                             trees = 100, 
                             mtry = tune(),
                             min_n = tune()) %>% 
  set_engine( "ranger", importance = "permutation")

artist_rf_spec <- rand_forest( mode = "classification",
                        trees = 100, 
                        mtry = 4,
                        min_n = 30) %>% 
  set_engine( "ranger", importance = "permutation")

rf_grid <- grid_regular( finalize( mtry(), song_train_processed %>% select( -genre ) ),
                             min_n(),
                             levels = 5)


doParallel::registerDoParallel() # This makes macs run a little faster 
rf_tuned <- tune_grid( object = rf_spec,
                       preprocessor = recipe(genre ~ . , data = song_train_processed%>%
                                               dplyr::select(-release_year)),
                       resamples = song_boots,
                       grid = rf_grid )
View(rf_tuned)  
best_rf_auc <- select_best( rf_tuned, "roc_auc" )
best_rf_auc
final_rf_song <- finalize_model( rf_spec, best_rf_auc )





set.seed(1846505)
song_train_cv<-vfold_cv(song_train_processed,v=5)



# Model selection



colnames(song_train_processed)
?lda

library(MASS)
View(iris)






summary(song_train_processed)





song_train_processed$release_year<-factor(song_train_processed$release_year)

summary(song_train_processed)

sth<-song_train_processed%>%
  dplyr::select(-release_year,-release_year_lumped)
lda_cv<-fit_resamples(object=lda_spec,
          preprocessor =recipe(genre~.,
                      data=song_train_processed%>%
                        dplyr::select(-release_year)),
                      resamples = song_train_cv,
          metrics=yardstick::metric_set(roc_auc, sensitivity, specificity)) 
criteria=metric_set(roc_auc, sensitivity, specificity) 
?fit_resamples

?metric_set

lda_cv%>%
  collect_metrics()

knn_cv<-fit_resamples(object=final_knn_song,
                      preprocessor =recipe(genre~.,
                                           data=song_train_processed %>%
                                           dplyr::select(-release_year)),
                      resamples = song_train_cv,
              metrics=criteria)
knn_cv%>%
  collect_metrics()%>%
  select(.metric,mean)
rf_cv<-fit_resamples(object=final_rf_song,
                      preprocessor =recipe(genre~.,
                                           data=song_train_processed%>%
                                             dplyr::select(-release_year)),
                      resamples = song_train_cv,
              metrics=criteria)


rf_result<-rf_cv%>%
  collect_metrics()%>%
  select(.metric,mean)
lda_result<-lda_cv%>%
  collect_metrics()%>%
  select(.metric,mean)
knn_result<-knn_cv%>%
  collect_metrics()%>%
  select(.metric,mean)
compare_table<-left_join(left_join(lda_result,knn_result,by=".metric"),rf_result,by=".metric")
colnames(compare_table)=c('Metric','LDA','KNN','Random Forest')
## Evaluating the winning model
final_model<-final_rf_song%>%
  fit(genre~.,data=song_train_processed%>%
        dplyr::select(-release_year_lumped))



final_model%>%
  vip()


prob_pred<-predict(final_model,song_test_processed,type='prob')
song_pred<-predict(final_model,song_test_processed)
song_pred<-bind_cols(true=song_test_processed$genre,
                     predicted=song_pred,
                     prob_pred)
View(song_pred)
sum(song_pred$.pred_class==song_pred$true)/nrow(song_pred)
categorical_metrics <- metric_set(sensitivity, specificity)
outcome<-song_pred%>%
  categorical_metrics(
    truth=true,
    estimate=.pred_class
  )
song_pred %>%
  roc_curve(
    truth = true,
    estimate = .pred_edm,.pred_latin,.pred_pop,`.pred_r&b`,
    .pred_rap,.pred_rock
  ) %>%
  autoplot()
outcome=rbind(outcome,song_pred %>%
  roc_auc(
    truth = true,
    estimate = .pred_edm,.pred_latin,.pred_pop,`.pred_r&b`,
    .pred_rap,.pred_rock
  ))

outcome=outcome[,c(1,3)]
colnames(outcome)=c('Metrics','Performance')





summary(artist_train)
colnames(artist_train_processed)
doParallel::registerDoParallel()
artist_model<-artist_rf_spec%>%
  fit(genre~track_artist+ track_popularity+danceability+
      energy+key+loudness+mode+speechiness+
        acousticness+instrumentalness+liveness
      +valence+tempo+duration_ms+release_year,data=artist_train_processed)
doParallel::registerDoParallel()


artist_knn_model<-artist_knn_spec%>%
  fit(genre~track_artist+ track_popularity+danceability+
        energy+key+loudness+mode+speechiness+
        acousticness+instrumentalness+liveness
      +valence+tempo+duration_ms+release_year,data=artist_train_processed)
artist_model%>%
  vip()
View(artist_train_processed)


artist_prob_pred<-predict(artist_model,artist_test_processed,type='prob')
artist_pred<-predict(artist_model,artist_test_processed)
artist_pred<-bind_cols(true=artist_test_processed$genre,
                     predicted=artist_pred,
                     artist_prob_pred)

artist_outcome<-artist_pred%>%
  categorical_metrics(
    truth=true,
    estimate=.pred_class
  )
nrow(artist_test_processed)

library(randomForest)
?randomForest
rf_artist=randomForest(genre~track_artist+ track_popularity+danceability+
                         energy+key+loudness+mode+speechiness+
                         acousticness+instrumentalness+liveness
                       +valence+tempo+duration_ms+release_year,
                       nodesize=30,mtry=4,ntree=100,
                       data=artist_train_processed)

#This links contain info about spotify playlist
# https://artists.spotify.com/en/blog/behind-the-playlists-your-questions-answered-by-our-playlist-editors