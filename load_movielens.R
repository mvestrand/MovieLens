# This script installs any necessary packages, downloads the movielens library,
# and creates the edx and validation sets. Code modified from instructions given
# in HarvardX's Data Science: Capstone course.

################################
# Create edx set, validation set
################################

load_movielens <- function(verbose=FALSE, forceDownload=FALSE) {
  
  # Note: this process could take a couple of minutes
  
  if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
  if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
  if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- "ml-10m.zip"
  if (forceDownload | !file.exists(dl)) {
    if(verbose) {print("Downloading MovieLens dataset...")}
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  }
  
  #dl <- tempfile()
  #download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  if(verbose) {print("Generating data tables...")}
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  # Validation set will be 10% of MovieLens data
  
  if(verbose) {print("Generating validation set...")}
  set.seed(1, sample.kind="Rounding")
  # if using R 3.5 or earlier, use `set.seed(1)` instead
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

  # rm(dl, ratings, movies, test_index, temp, movielens, removed)
  
  if(verbose) {print("MovieLens dataset loaded.")  }
  
  return (list(edx = edx, validation = validation))
}

