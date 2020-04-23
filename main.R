
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
  
  # Make sure userId and movieId in test set are also in train set
  test_set <- temp %>% 
    semi_join(train_set, by = "movieId") %>%
    semi_join(train_set, by = "userId")
  
  # Add rows removed from test set back into train set
  removed <- anti_join(temp, test_set)
  train_set <- rbind(train_set, removed)
  
  return (list(train_set = train_set, test_set = test_set))
}

# Generate bootstrap sets
bootstrap_movielens <- function(dataset, n = 10, seed = NULL, train_size = 0.5, test_size = 0.5) {
  # Set random seed if given
  if (!is.null(seed)) {
    set.seed(seed, sample.kind="Rounding")
  }
  
  train_sets <- sapply(1:n, simplify=FALSE, FUN=function(i) {
    sample_frac(train_set, size=train_size, replace=TRUE)
  })
  
  test_sets <- sapply(1:n, simplify=FALSE, FUN=function(i) {
    temp <- sample_frac(train_set, size=test_size, replace=TRUE)
    
    temp %>%
      semi_join(train_sets[[i]], by = "movieId") %>%
      semi_join(train_sets[[i]], by = "userId")
  })
  
  return (list(train_sets = train_sets, test_sets = test_sets))
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
  summarize(b_i = mean(rating - mu), n_i = n())

y_hat <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(y_hat = mu + b_i) %>%
  .$y_hat


m1_rmse <- RMSE(test_set$rating, y_hat)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = m1_rmse ))

#rmse_results %>% knitr::kable()


# L2 Regularized per movie bias

# Create 10 bootstrap sets for cross validation to choose lambda
lambdas <- seq(0,5,0.25)
n <- 10
cv <- bootstrap_movielens(train_set, n=20, train_size=0.1, test_size=0.1)

# Compute mean rmse for each lambda
lambda_rmses <- sapply(lambdas, function(l){
  print(l)
  mean(sapply(1:n, function(i) {
    mu <- mean(cv$train_sets[[i]]$rating)
    movie_avgs <- cv$train_sets[[i]] %>%
      group_by(movieId) %>%
      summarize(s = sum(rating - mu), n_i = n()) %>%
      mutate(b_i = s/(n_i+l))
      

    y_hat <- cv$test_sets[[i]] %>% 
      left_join(movie_avgs, by='movieId') %>%
      mutate(y_hat = mu + b_i) %>%
      .$y_hat
    RMSE(cv$test_sets[[i]]$rating, y_hat)
  }))
})

qplot(lambdas, lambda_rmses)
best_lambda <- lambdas[which.min(lambda_rmses)]

# Use the best lambda on the actual training set
mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n()) %>%
  mutate(b_i = s/(n_i+best_lambda))

movie_avgs %>% arrange(desc(b_i)) %>%
  left_join(movie_info) %>%
  slice(1:20)

y_hat <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(y_hat = mu + b_i) %>%
  .$y_hat

m2_rmse <- RMSE(test_set$rating, y_hat)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",
                                     RMSE = m2_rmse))
rmse_results %>% knitr::kable()
