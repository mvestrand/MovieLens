# Utility functions


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

# Generate n bootstrap sets
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

