
source("load_movielens.R")

ml <- load_movielens(verbose=TRUE)
edx <- ml$edx
validation <- ml$validation

#=================
# Functions
#=================

# Computes the root mean square error for a set of predicted ratings
#  true_ratings - The actual ratings given
#  predicted_ratings - The ratings predicted by the algorithm in question
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



# Splits a movielens dataset into training and test sets
#  dataset - The dataset to split
#  seed - A random seed to set. Ignored if NULL
#  p - The fraction of data to use for the test set
split_movielens <- function(dataset, seed=NULL, p = 0.1) {
  # Set random seed if given
  if (!is.null(seed)) {
    set.seed(seed, sample.kind="Rounding")
  }
  test_index <- createDataPartition(y = dataset$rating, times = 1, p = p, list = FALSE)
  train_set <- dataset[-test_index,]
  temp <- dataset[test_index,]
  
  # Make sure userId and movieId in validation set are also in edx set
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from validation set back into edx set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  return (list(train_set = train_set, test_set = test_set))
}



#=====================
# Data Exploration
#=====================

# Count unique movies and users
length(unique(edx$userId))
length(unique(edx$movieId))

# Collect info on each movie in the dataset
movie_info <- edx %>% 
  select(movieId, title, genres) %>%
  distinct()

# One title has two movieIds assigned
length(unique(edx$movieId))
length(unique(edx$title))

duplicates <- edx %>% select(movieId, title) %>%
  distinct() %>%
  group_by(title) %>%
  filter(n() > 1)


# It may be a different cut or simply a mistake, but one has 
# only a few reviews, so it can be safely ignored.
edx %>% filter(movieId %in% duplicates$movieId) %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  left_join(movie_info, by = "movieId")

# Check for any na's 
edx %>% filter(is.na(movieId))
edx %>% filter(is.na(userId))
edx %>% filter(is.na(rating))
edx %>% filter(is.na(timestamp))
edx %>% filter(is.na(title))
edx %>% filter(is.na(genres))

# Split into training and test sets
ml_sets <- split_movielens(edx, 7)
train_set <- ml_sets$train_set
test_set <- ml_sets$test_set



## Test some different models previously used in the course

# Guess the average rating regardless of user or movie
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)

y_hat <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, y_hat)
rmse_results <- data_frame(method = "Overall average", RMSE = naive_rmse)


# Approximate per movie bias via average rating
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

length(setdiff(unique(train_set$movieId), unique(test_set$movieId)))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

y_hat <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(y_hat = mu + b_i) %>%
  .$y_hat


m1_rmse <- RMSE(test_set$rating, y_hat)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = m1_rmse ))

#rmse_results %>% knitr::kable()


# Regularized per movie bias
mu <- mean(train_set$rating)
lambda <- 1:100
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


