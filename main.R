
source("load_movielens.R")
source("utility.R")

ml <- load_movielens(verbose=TRUE)
edx <- ml$edx
validation <- ml$validation

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


# Movie count with n ratings
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# Histogram of users with n ratings
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

# Not nearly as many movies have only 1 rating as did in the smaller movielens dataset, 
# suggesting regularizing movie effects will not help nearly as much



# ============================
# Comparing Methods
# ============================


# Split into training and test sets
ml_sets <- split_movielens(edx, 7)
train_set <- ml_sets$train_set
test_set <- ml_sets$test_set


# Test some different models previously used in the course
res <- create_results_table()

# ---------------------------------------
# Guess the average rating regardless of user or movie
# ---------------------------------------
mu <- mean(train_set$rating)
pred <- rep(mu, nrow(test_set))

res <- update_results_table(res, "Overall average", pred = pred, rmse = RMSE(test_set$rating, pred))



# ---------------------------------------
# Approximate per movie bias via average rating
# ---------------------------------------
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu), n_i = n())

pred <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

res <- update_results_table(res, "Movie Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))

# ---------------------------------------
# Approximate per movie and per user bias
# ---------------------------------------
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu), n_i = n())

user_avgs <- train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

pred <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

res <- update_results_table(res, "Movie + User Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))



# ---------------------------------------
# L2 Regularized per movie bias
# ---------------------------------------
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
      

    pred <- cv$test_sets[[i]] %>% 
      left_join(movie_avgs, by='movieId') %>%
      mutate(pred = mu + b_i) %>%
      .$pred
    RMSE(cv$test_sets[[i]]$rating, pred)
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

pred <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

res <- update_results_table(res, "Regularized Movie Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))





res$rmse %>% knitr::kable()

