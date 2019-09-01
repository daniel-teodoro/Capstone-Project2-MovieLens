# Project 2 - MovieLens
# Daniel Teodoro

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# -----------------------------------------------------------------------------------------------
# 0 - Prepare
# -----------------------------------------------------------------------------------------------

#vPath <- "/Users/Daniel/_Pessoal/Treinamentos/Hardvard-Capstone-Data-Science/Project2-MovieLens-DanielTeodoro/"
vPath <- paste(getwd(), "/", sep="")
setwd(vPath)
vMovieLensFile <- paste(vPath, "ml-10M100K/movielens.csv", sep="")
vCached <- file.exists(vMovieLensFile)

# -----------------------------------------------------------------------------------------------
# 1 - Acquire Data
# -----------------------------------------------------------------------------------------------
if (!vCached)
{
  
  # Zip
  vZIPFile <- paste(vPath, "ml-10m.zip", sep="")
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", vZIPFile)
  unlink(paste(vPath, "ml-10M100K", sep=""), recursive = TRUE)
  
  # Ratings
  vRatingsFile <- "ml-10M100K/ratings.dat"
  unzip(vZIPFile, vRatingsFile)
  vRatingsDS <- readLines(vRatingsFile)
  ratings <- read.table(text = gsub("::", "\t", vRatingsDS), col.names = c("userId", "movieId", "rating", "timestamp"))
  rm(vRatingsDS)
  
  # Movies
  vMoviesFile <- "ml-10M100K/movies.dat"
  unzip(vZIPFile, vMoviesFile)
  vMoviesDS <- readLines(vMoviesFile)
  movies <- str_split_fixed(vMoviesDS, "\\::", 3)
  rm(vMoviesDS)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  #MovieLens
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Cache results
  write.csv(movielens, vMovieLensFile)  
  
  rm(vZIPFile, vRatingsFile, vMoviesFile)
} else {  
  movielens <- read.csv(vMovieLensFile, row.names = 1)
}
rm(vCached, vPath, vMovieLensFile)

# -----------------------------------------------------------------------------------------------
# 2 - Process
# -----------------------------------------------------------------------------------------------

# Validation set will be 10% of MovieLens data
set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings
validation <- validation %>% select(-rating)


# -----------------------------------------------------------------------------------------------
# 3 - Quiz
# -----------------------------------------------------------------------------------------------

# Q1) How many rows and columns are there in the edx dataset?
paste('The edx dataset has',nrow(edx),'rows and',ncol(edx),'columns.')

# Q2) How many zeros and threes were given in the edx dataset?
paste(sum(edx$rating == 0), 'ratings with 0 were given and',
      sum(edx$rating == 3),'ratings with 3')

# Q3) How many different movies are in the edx dataset?
edx %>% summarize(n_movies = n_distinct(movieId))

# Q4) How many different users are in the edx dataset?
edx %>% summarize(n_users = n_distinct(userId))

# Q5) How many movie ratings are in cear of the following genres in the edx dataset?
drama <- edx %>% filter(str_detect(genres,"Drama"))
paste('Drama has',nrow(drama),'movies')

comedy <- edx %>% filter(str_detect(genres,"Comedy"))
paste('Comedy has',nrow(comedy),'movies')

thriller <- edx %>% filter(str_detect(genres,"Thriller"))
paste('Thriller has',nrow(thriller),'movies')

romance <- edx %>% filter(str_detect(genres,"Romance"))
paste('Romance has',nrow(romance),'movies')

rm(drama, comedy, thriller, romance)

# Q6) Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarise(number = n()) %>% arrange(desc(number))

# Q7) What are the five most given ratings in order from most to least?
head(sort(-table(edx$rating)),5)

# Q8) True or False: 
# In general, half star ratings are less common than whole star ratings 
# (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4, etc.).
ratings35 <- table(edx$rating)["3.5"]
ratings3 <- table(edx$rating)["3"]
ratings4 <- table(edx$rating)["4"]
answer <- (ratings35 < ratings3 && ratings35 < ratings4)
print(answer)
rm(ratings35, ratings3, ratings4, answer)

# -----------------------------------------------------------------------------------------------
# 4 - Data Analysis
# -----------------------------------------------------------------------------------------------

# The movielens dataset has more than 10 million ratings. 
# Each rating comes with a userId, a movieId, the rating, 
# a timestamp and information about the movie like title and genre.
str(movielens)

# Ratings range from 0.5 to 5.0. 
# The difference in meadian an mean shows that the distribution is skewed towards higher ratings. 
# The chart shows that whole-number ratings are more common that 0.5 ratings.
hist(movielens$rating, col = "#000000")
summary(movielens$rating)

# More recent movies get more userratings. 
# Movies earlier than 1930 get few ratings, whereas newer movies, 
# especially in the 90s get far more ratings.
movielens$year <- as.numeric(substr(as.character(movielens$title),nchar(as.character(movielens$title))-4,nchar(as.character(movielens$title))-1))

plot(table(movielens$year), col = "#000000")


# Movies from earlier decades have more volatile ratings, which can be explained by the lower frequence of movieratings. 
# However, since the project is measured by the accuarcy, this volatility has to be taken into account.
avg_ratings <- movielens %>% group_by(year) %>% summarise(avg_rating = mean(rating))
plot(avg_ratings, col = "#000000")


# -----------------------------------------------------------------------------------------------
# 5 - Results
# -----------------------------------------------------------------------------------------------

#Root Mean Square Error Loss Function
RMSE <- function(true_ratings, predicted_ratings){
  # to do 
  # sqrt(mean((d$prediction-d$y)^2)) 
  # 
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

lambdas <- seq(0, 5, 0.25)

rmses <- sapply(lambdas,function(l){
  
  #Calculate the mean of ratings from the edx training set
  mu <- mean(edx$rating)
  
  #Adjust mean by movie effect and penalize low number on ratings
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #ajdust mean by user and movie effect and penalize low number of ratings
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #predict ratings in the training set to derive optimal penalty value 'lambda'
  predicted_ratings <- 
    edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, edx$rating))
})

plot(lambdas, rmses, col = "#000000")

lambda <- lambdas[which.min(rmses)]
paste('Optimal RMSE of',min(rmses),'is achieved with Lambda',lambda)


# -----------------------------------------------------------------------------------------------
# 6 - Apply Lambda on Validation set for Data-Export
# -----------------------------------------------------------------------------------------------

# Generated predictions for validation dataset.
pred_y_lse <- sapply(lambda,function(l){
  
  #Derive the mearn from the training set
  mu <- mean(edx$rating)
  
  #Calculate movie effect with optimal lambda
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #Calculate user effect with optimal lambda
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #Predict ratings on validation set
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred #validation
  
  return(predicted_ratings)
  
})


# -----------------------------------------------------------------------------------------------
# 7 - Export
# -----------------------------------------------------------------------------------------------

# Ratings will go into the CSV submission file below:
write.csv(validation %>% select(userId, movieId) %>% mutate(rating = pred_y_lse),
          "submission.csv", na = "", row.names=FALSE)


# -----------------------------------------------------------------------------------------------
# 8 - Conclusion
# -----------------------------------------------------------------------------------------------
# The aim of the project was to predict movieratings from a long list of rated movies. 
# The size of the dataset restricted the machinlearning algorithms my computer was able to perform on the dataset. 
# The penalized least squares approach was able to come up with ratings that are near the true ratings. 
# However, accuracy is measured as absolute difference between the predicted value and the acutal value. 
# The transformation from a continuous number to the actual rating did not result in a high overall accuarcy, 
# although the prediction in terms of real numbers makes sense.




