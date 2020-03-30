# Script for computing answers to the movielens dataset quiz. Assumes
# that the edx and validation sets are already loaded.

source("load_movielens.R")


# Q1
nrow(edx)
ncol(edx)

# Q2
edx %>%
  filter(rating == 0) %>%
  summarize(n = n()) %>% .$n

edx %>%
  filter(rating == 3) %>%
  summarize(n = n()) %>% .$n

# Q3
length(unique(edx$movieId))

# Q4
length(unique(edx$userId))

# Q5
# edx %>%
#   filter(genres %in% c("Drama", "Comedy", "Thriller", "Romance")) %>%
#   group_by(genres) %>%
#   summarize(n = n())
edx %>% filter(str_detect(genres,"Drama")) %>% summarize(n = n()) %>% .$n
edx %>% filter(str_detect(genres,"Comedy")) %>% summarize(n = n()) %>% .$n
edx %>% filter(str_detect(genres,"Thriller")) %>% summarize(n = n()) %>% .$n
edx %>% filter(str_detect(genres,"Romance")) %>% summarize(n = n()) %>% .$n

# Q6
movie_info <- edx %>% 
  select(movieId, title, genres) %>%
  distinct()
edx %>% group_by(movieId) %>%
  summarize(n = n()) %>%
  top_n(1, n) %>%
  left_join(movie_info, by = "movieId")

# Q7
edx %>% group_by(rating) %>%
  summarize(n = n()) %>%
  top_n(5, n) %>%
  arrange(desc(n))

# Q8
edx %>% 
  mutate(rem = rating %% 1) %>%
  group_by(rem) %>%
  summarize(n = n())



# One title has two movieIds assigned
length(unique(edx$movieId))
length(unique(edx$title))

duplicates <- edx %>% select(movieId, title) %>%
  distinct() %>%
  group_by(title) %>%
  filter(n() > 1)

edx %>% filter(movieId %in% duplicates$movieId) %>%
  group_by(movieId) %>%
  summarize(n = n()) %>%
  left_join(movie_info, by = "movieId")

# It may be a different cut or simply a mistake, but one has 
# only a few reviews, so it can be safely ignored. 