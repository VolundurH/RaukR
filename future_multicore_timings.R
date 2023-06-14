library(tidyverse)
library(future)
options(future.rng.onMisuse = "ignore")

# print("Without multicore:")
# tictoc::tic()
# sample(100,200000000,replace=T)
# tictoc::toc()
# 
# print("Split sample function into 4 multicore processes:")
# plan(multicore)
# tictoc::tic()
# a  %<-% {sample(100,200000000/4,replace=T)}
# b  %<-% {sample(100,200000000/4,replace=T)}
# c  %<-% {sample(100,200000000/4,replace=T)}
# d  %<-% {sample(100,200000000/4,replace=T)}
# tictoc::toc()
# 
# print("Append the 4 vectors together: ")
# tictoc::tic()
# unlist(append(c(1,2,3), list( c(1,2,3), c(1,2,3))))
# tictoc::toc()
# plan("default")


# tidyverse multicore mapping ---------------------------------------------

test_tibble <- tibble(ID = letters[1:15]) %>% 
  mutate(iterations = as.integer(200000000/15)) 

print("default map function:")
tictoc::tic()
test_tibble %>% 
  mutate(random_sample = map(iterations, ~sample(100, .x, replace = T)))
tictoc::toc()

print("furrr map_future function with multicore plan:")
plan(multicore)
tictoc::tic()
test_tibble %>% 
  mutate(random_sample = furrr::future_map(iterations, ~sample(100, .x, replace = T)))
tictoc::toc()
plan("default")

