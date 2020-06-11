
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
simple_average_predict <- function(train_set, test_set) {
  mu <- mean(train_set$rating)
  pred <- rep(mu, nrow(test_set))

  return(pred)  
}
# mu <- mean(train_set$rating)
# pred <- rep(mu, nrow(test_set))

pred <- simple_average_predict(train_set, test_set)
res <- update_results_table(res, "Overall average", pred = pred, rmse = RMSE(test_set$rating, pred))



# ---------------------------------------
# Approximate per movie bias via average rating
# ---------------------------------------
movie_bias_predict <- function(train_set, test_set) {
  mu <- mean(train_set$rating)
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu), n_i = n())
  
  pred <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  
  return(pred)  
}

pred <- movie_bias_predict(train_set, test_set)
res <- update_results_table(res, "Movie Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))

# ---------------------------------------
# Approximate per movie and per user bias
# ---------------------------------------
movie_user_bias_predict <- function(train_set, test_set) {
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
  return(pred)
}

pred <- movie_user_bias_predict(train_set, test_set)
res <- update_results_table(res, "Movie + User Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))



# ---------------------------------------
# L2 Regularized per movie bias
# ---------------------------------------
# Not nearly as many movies have only 1 rating as did in the smaller movielens dataset, 
# suggesting regularizing movie effects will not help nearly as much

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
reg_movie_bias_predict <- function(train_set, test_set, l) {
  
  mu <- mean(train_set$rating)
  movie_avgs <- train_set %>%
    group_by(movieId) %>%
    summarize(s = sum(rating - mu), n_i = n()) %>%
    mutate(b_i = s/(n_i+l))
  
  pred <- test_set %>% 
    left_join(movie_avgs, by='movieId') %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(pred)
}

pred <- reg_movie_bias_predict(train_set, test_set, best_lambda)

res <- update_results_table(res, "Regularized Movie Effect Model", pred = pred, rmse = RMSE(test_set$rating, pred))



# ----------------------
# User Genre Effect
# ----------------------


# Movie counts for combined genres
genres <- unique(edx$genres)
length(genres)

genre_counts <- movie_info %>%
  group_by(genres) %>%
  summarize(n=n())

genre_counts %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  ggtitle("Users")

genre_counts %>%
  top_n(30, n) %>%
  arrange(desc(n)) %>%
  knitr::kable()


# Predict movie ratings 
reg_genre_bias_predict <- function(train_set, test_set, l) {
  # Get the base genre strings
  base_genres <- unique(unlist(strsplit(genres, "\\|")))
  base_genres <- base_genres[base_genres!="(no genres listed)"]
  
  # Collect info on each movie in the dataset
  movie_info <- train_set %>% 
    select(movieId, title, genres) %>%
    distinct()
  
  # Create movie genre table with one hot encoding
  one_hot_genres <- movie_info
  i <- 0
  for (genre in base_genres) {
    i <- i+1
    one_hot_genres[[sprintf("g%02d", i)]] <- with(one_hot_genres, ifelse(str_detect(genres,genre), 1, 0))
  }
  
  one_hot_genres <- one_hot_genres %>% select(movieId, g01:g19)
  
  
  # Movie and user effect
  mu <- mean(train_set$rating)
  movie_avgs <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu), n_i = n())
  
  user_avgs <- train_set %>%
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))
  
  
  # Residual after removing movie and user effect
  resid <- train_set %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(r = rating - mu - b_i - b_u)
  
  
  user_genre_avgs <- resid %>%
    left_join(one_hot_genres, by='movieId') %>%
    group_by(userId) %>%
    mutate(r01=g01*r,
           r02=g02*r,
           r03=g03*r,
           r04=g04*r,
           r05=g05*r,
           r06=g06*r,
           r07=g07*r,
           r08=g08*r,
           r09=g09*r,
           r10=g10*r,
           r11=g11*r,
           r12=g12*r,
           r13=g13*r,
           r14=g14*r,
           r15=g15*r,
           r16=g16*r,
           r17=g17*r,
           r18=g18*r,
           r19=g19*r) %>%
    summarize(r01=sum(r01)/(sum(g01)+l),
              r02=sum(r02)/(sum(g02)+l),
              r03=sum(r03)/(sum(g03)+l),
              r04=sum(r04)/(sum(g04)+l),
              r05=sum(r05)/(sum(g05)+l),
              r06=sum(r06)/(sum(g06)+l),
              r07=sum(r07)/(sum(g07)+l),
              r08=sum(r08)/(sum(g08)+l),
              r09=sum(r09)/(sum(g09)+l),
              r10=sum(r10)/(sum(g10)+l),
              r11=sum(r11)/(sum(g11)+l),
              r12=sum(r12)/(sum(g12)+l),
              r13=sum(r13)/(sum(g13)+l),
              r14=sum(r14)/(sum(g14)+l),
              r15=sum(r15)/(sum(g15)+l),
              r16=sum(r16)/(sum(g16)+l),
              r17=sum(r17)/(sum(g17)+l),
              r18=sum(r18)/(sum(g18)+l),
              r19=sum(r19)/(sum(g19)+l)) %>%
    mutate(r01=ifelse(is.nan(r01), 0, r01),
           r02=ifelse(is.nan(r02), 0, r02),
           r03=ifelse(is.nan(r03), 0, r03),
           r04=ifelse(is.nan(r04), 0, r04),
           r05=ifelse(is.nan(r05), 0, r05),
           r06=ifelse(is.nan(r06), 0, r06),
           r07=ifelse(is.nan(r07), 0, r07),
           r08=ifelse(is.nan(r08), 0, r08),
           r09=ifelse(is.nan(r09), 0, r09),
           r10=ifelse(is.nan(r10), 0, r10),
           r11=ifelse(is.nan(r11), 0, r11),
           r12=ifelse(is.nan(r12), 0, r12),
           r13=ifelse(is.nan(r13), 0, r13),
           r14=ifelse(is.nan(r14), 0, r14),
           r15=ifelse(is.nan(r15), 0, r15),
           r16=ifelse(is.nan(r16), 0, r16),
           r17=ifelse(is.nan(r17), 0, r17),
           r18=ifelse(is.nan(r18), 0, r18),
           r19=ifelse(is.nan(r19), 0, r19)
    )
  
  pred <- test_set %>%
    left_join(user_genre_avgs, by='userId') %>%
    left_join(one_hot_genres, by='movieId') %>%
    mutate(
      b_g=(r01*g01 + r02*g02 + r03*g03 + r04*g04 + r05*g05 +
             r06*g06 + r07*g07 + r08*g08 + r09*g09 + r10*g10 +
             r11*g11 + r12*g12 + r13*g13 + r14*g14 + r15*g15 +
             r16*g16 + r17*g17 + r18*g18 + r19*g19)/
        (g01 + g02 + g03 + g04 + g05 + g06 + g07 + g08 + g09 + g10 +
           g11 + g12 + g13 + g14 + g15 + g16 + g17 + g18 + g19)) %>%
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  
  return(pred)
}  


# # Movie counts for base genres
# base_genres <- unique(unlist(strsplit(genres, "\\|")))
# base_genres <- base_genres[base_genres!="(no genres listed)"]
# length(base_genres)
# 
# 
# # Create movie genre table with one hot encoding
# one_hot_genres <- movie_info
# i <- 0
# for (genre in base_genres) {
#   i <- i+1
#   one_hot_genres[[sprintf("g%02d", i)]] <- with(one_hot_genres, ifelse(str_detect(genres,genre), 1, 0))
# }
# 
# one_hot_genres <- one_hot_genres %>% select(movieId, g01:g19)
# 
# 
# # Movie and user effect
# mu <- mean(train_set$rating)
# movie_avgs <- train_set %>% 
#   group_by(movieId) %>% 
#   summarize(b_i = mean(rating - mu), n_i = n())
# 
# user_avgs <- train_set %>%
#   left_join(movie_avgs, by='movieId') %>%
#   group_by(userId) %>%
#   summarize(b_u = mean(rating - mu - b_i))
# 
# 
# # Residual after removing movie and user effect
# resid <- train_set %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   mutate(r = rating - mu - b_i - b_u)
# 
# 
# user_genre_avgs <- resid %>%
#   left_join(one_hot_genres, by='movieId') %>%
#   group_by(userId) %>%
#   mutate(r01=g01*r,
#          r02=g02*r,
#          r03=g03*r,
#          r04=g04*r,
#          r05=g05*r,
#          r06=g06*r,
#          r07=g07*r,
#          r08=g08*r,
#          r09=g09*r,
#          r10=g10*r,
#          r11=g11*r,
#          r12=g12*r,
#          r13=g13*r,
#          r14=g14*r,
#          r15=g15*r,
#          r16=g16*r,
#          r17=g17*r,
#          r18=g18*r,
#          r19=g19*r) %>%
#   summarize(r01=sum(r01)/sum(g01),
#             r02=sum(r02)/sum(g02),
#             r03=sum(r03)/sum(g03),
#             r04=sum(r04)/sum(g04),
#             r05=sum(r05)/sum(g05),
#             r06=sum(r06)/sum(g06),
#             r07=sum(r07)/sum(g07),
#             r08=sum(r08)/sum(g08),
#             r09=sum(r09)/sum(g09),
#             r10=sum(r10)/sum(g10),
#             r11=sum(r11)/sum(g11),
#             r12=sum(r12)/sum(g12),
#             r13=sum(r13)/sum(g13),
#             r14=sum(r14)/sum(g14),
#             r15=sum(r15)/sum(g15),
#             r16=sum(r16)/sum(g16),
#             r17=sum(r17)/sum(g17),
#             r18=sum(r18)/sum(g18),
#             r19=sum(r19)/sum(g19)) %>%
#   mutate(r01=ifelse(is.nan(r01), 0, r01),
#          r02=ifelse(is.nan(r02), 0, r02),
#          r03=ifelse(is.nan(r03), 0, r03),
#          r04=ifelse(is.nan(r04), 0, r04),
#          r05=ifelse(is.nan(r05), 0, r05),
#          r06=ifelse(is.nan(r06), 0, r06),
#          r07=ifelse(is.nan(r07), 0, r07),
#          r08=ifelse(is.nan(r08), 0, r08),
#          r09=ifelse(is.nan(r09), 0, r09),
#          r10=ifelse(is.nan(r10), 0, r10),
#          r11=ifelse(is.nan(r11), 0, r11),
#          r12=ifelse(is.nan(r12), 0, r12),
#          r13=ifelse(is.nan(r13), 0, r13),
#          r14=ifelse(is.nan(r14), 0, r14),
#          r15=ifelse(is.nan(r15), 0, r15),
#          r16=ifelse(is.nan(r16), 0, r16),
#          r17=ifelse(is.nan(r17), 0, r17),
#          r18=ifelse(is.nan(r18), 0, r18),
#          r19=ifelse(is.nan(r19), 0, r19)
#   )
# 
# pred <- test_set %>%
#   left_join(user_genre_avgs, by='userId') %>%
#   left_join(one_hot_genres, by='movieId') %>%
#   mutate(
#       b_g=(r01*g01 + r02*g02 + r03*g03 + r04*g04 + r05*g05 +
#            r06*g06 + r07*g07 + r08*g08 + r09*g09 + r10*g10 +
#            r11*g11 + r12*g12 + r13*g13 + r14*g14 + r15*g15 +
#            r16*g16 + r17*g17 + r18*g18 + r19*g19)/
#         (g01 + g02 + g03 + g04 + g05 + g06 + g07 + g08 + g09 + g10 +
#          g11 + g12 + g13 + g14 + g15 + g16 + g17 + g18 + g19)) %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   mutate(pred = mu + b_i + b_u + b_g) %>%
#   .$pred

res <- update_results_table(res, "User Genre Bias Model", pred = pred, rmse = RMSE(test_set$rating, pred))


# ----------------------
# Regularized User Genre Effect
# ----------------------

# Histogram of users reviews for each genre
user_genre_counts <- train_set %>%
  left_join(one_hot_genres, by='movieId') %>%
  group_by(userId) %>%
  summarize(s01=sum(g01),
            s02=sum(g02), 
            s03=sum(g03), 
            s04=sum(g04), 
            s05=sum(g05), 
            s06=sum(g06), 
            s07=sum(g07), 
            s08=sum(g08), 
            s09=sum(g09), 
            s10=sum(g10), 
            s11=sum(g11), 
            s12=sum(g12), 
            s13=sum(g13), 
            s14=sum(g14), 
            s15=sum(g15), 
            s16=sum(g16), 
            s17=sum(g17), 
            s18=sum(g18), 
            s19=sum(g19)) %>%
  gather(s01:s19, key='genre', value='count')

user_genre_counts %>%
  filter(count != 0) %>%
  ggplot(aes(count)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_continuous(trans='log2') +
  ggtitle("Users")


# Regularization parameters
lambdas <- seq(0,5,0.25)

rmses <- lapply(lambdas, function(l) {
  
  pred <- reg_genre_bias_predict(train_set, test_set, l)

  print(l)
  return(RMSE(test_set$rating, pred))
})


l <- lambdas[which.min(rmses)]
plot(lambdas, rmses)

# user_genre_avgs <- resid %>%
#   left_join(one_hot_genres, by='movieId') %>%
#   group_by(userId) %>%
#   mutate(r01=g01*r,
#          r02=g02*r,
#          r03=g03*r,
#          r04=g04*r,
#          r05=g05*r,
#          r06=g06*r,
#          r07=g07*r,
#          r08=g08*r,
#          r09=g09*r,
#          r10=g10*r,
#          r11=g11*r,
#          r12=g12*r,
#          r13=g13*r,
#          r14=g14*r,
#          r15=g15*r,
#          r16=g16*r,
#          r17=g17*r,
#          r18=g18*r,
#          r19=g19*r) %>%
#   summarize(r01=sum(r01)/(sum(g01)+l),
#             r02=sum(r02)/(sum(g02)+l),
#             r03=sum(r03)/(sum(g03)+l),
#             r04=sum(r04)/(sum(g04)+l),
#             r05=sum(r05)/(sum(g05)+l),
#             r06=sum(r06)/(sum(g06)+l),
#             r07=sum(r07)/(sum(g07)+l),
#             r08=sum(r08)/(sum(g08)+l),
#             r09=sum(r09)/(sum(g09)+l),
#             r10=sum(r10)/(sum(g10)+l),
#             r11=sum(r11)/(sum(g11)+l),
#             r12=sum(r12)/(sum(g12)+l),
#             r13=sum(r13)/(sum(g13)+l),
#             r14=sum(r14)/(sum(g14)+l),
#             r15=sum(r15)/(sum(g15)+l),
#             r16=sum(r16)/(sum(g16)+l),
#             r17=sum(r17)/(sum(g17)+l),
#             r18=sum(r18)/(sum(g18)+l),
#             r19=sum(r19)/(sum(g19)+l)) %>%
#   mutate(r01=ifelse(is.nan(r01), 0, r01),
#          r02=ifelse(is.nan(r02), 0, r02),
#          r03=ifelse(is.nan(r03), 0, r03),
#          r04=ifelse(is.nan(r04), 0, r04),
#          r05=ifelse(is.nan(r05), 0, r05),
#          r06=ifelse(is.nan(r06), 0, r06),
#          r07=ifelse(is.nan(r07), 0, r07),
#          r08=ifelse(is.nan(r08), 0, r08),
#          r09=ifelse(is.nan(r09), 0, r09),
#          r10=ifelse(is.nan(r10), 0, r10),
#          r11=ifelse(is.nan(r11), 0, r11),
#          r12=ifelse(is.nan(r12), 0, r12),
#          r13=ifelse(is.nan(r13), 0, r13),
#          r14=ifelse(is.nan(r14), 0, r14),
#          r15=ifelse(is.nan(r15), 0, r15),
#          r16=ifelse(is.nan(r16), 0, r16),
#          r17=ifelse(is.nan(r17), 0, r17),
#          r18=ifelse(is.nan(r18), 0, r18),
#          r19=ifelse(is.nan(r19), 0, r19)
#   )
# 
# pred <- test_set %>%
#   left_join(user_genre_avgs, by='userId') %>%
#   left_join(one_hot_genres, by='movieId') %>%
#   mutate(
#     b_g=(r01*g01 + r02*g02 + r03*g03 + r04*g04 + r05*g05 +
#            r06*g06 + r07*g07 + r08*g08 + r09*g09 + r10*g10 +
#            r11*g11 + r12*g12 + r13*g13 + r14*g14 + r15*g15 +
#            r16*g16 + r17*g17 + r18*g18 + r19*g19)/
#       (g01 + g02 + g03 + g04 + g05 + g06 + g07 + g08 + g09 + g10 +
#          g11 + g12 + g13 + g14 + g15 + g16 + g17 + g18 + g19)) %>%
#   left_join(movie_avgs, by='movieId') %>%
#   left_join(user_avgs, by='userId') %>%
#   mutate(pred = mu + b_i + b_u + b_g) %>%
#   .$pred

res <- update_results_table(res, "Regularized User Genre Bias Model", pred = pred, rmse = RMSE(test_set$rating, pred))

res$rmse %>% knitr::kable()





