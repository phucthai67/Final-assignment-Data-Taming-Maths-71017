# Final-assignment-Data-Taming-Maths-71017


The goal is to build a classifier that can classify songs' music genre based on the songs characteristic (tempo, year released, accousticness, etc).The dataset is given, containing songs available in spotify.

In this project, I performed data cleaning, data exploration and visualization using tidyverse and ggplot2. I then build, tune and compare models using tidymodels (model building and tuning) and yardstick (comparing performance). The three types of models built was K nearest neighbor (tuning number of neighbor), Linear discriminant analysis (no hyperparameter to tune) and random forest (number of predictor for each tree, and minimum number of observation in a leaf node for continuing the separation). Note I did not use the whole dataset because it is quite computational heavy, hence I was allow to subset the data to build my models

an R code, report and rmarkdown file to generate the report is provided alongside the dataset.
