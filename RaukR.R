setwd("~/Documents/RaukR/")
library(tidyverse)

# Day 1 -------------------------------------------------------------------


# Functions lab -----------------------------------------------------------

complement <- function(dna_string, reverse = F){
  # Create a character vector of complements
  complements <- c("A" = "T", "T" = "A", "C" = "G", "G" = "C")
  
  # Use the `chartr` function to replace each base with its complement
  dna_compl <- chartr(paste0(names(complements), collapse = ""), paste0(complements, collapse = ""), toupper(dna_string))
  
  if (reverse) {
    return(stringi::stri_reverse(dna_compl))
  }
  return(dna_compl)
}

complement("ATCGCGCGATT")
complement("ATCGCGCGATT", reverse = T)

complement("test sentence")

c(NA, 1, "a", Inf) %>% 
  complement()

complement()

# example: 
function_name <- function(param1, param2 = "Lucy"){
  if (param2 == "Lucy") {
    output <- paste("Lucy in the sky with ",param1,"\n",sep="")}
  else {
    output <- paste(param1," in the ocean with ",param2,"\n",sep="")}
  cat(output)
}

# questions: 

  #   What happens if you don’t assign a argument to the parameter that does not have a default?
    # The function returns an error

  #   Did I need to use the return() function in my example?
    # No, it automatically returns the last line in the function

  #   Why does my example perform the cat() function in the return() function?
    # So that the function can be used in std out

  #   Try assigning a variable to the return value of your function. variable_a <- function_name("Tom")
  #   What happens to the variable if the return value is the output of cat()?
    # It does not assign the strin to the variable, it prints it out and assigns a a NULL value.     
  
set.seed(123)
for (i in 1:10) {
  # generate a random DNA string
  string <- paste0(sample(c("A","T","G","C"), size = 10, replace = T), collapse = "")
  
  # call our function on that string
  print(complement(string))
}


data.frame(
  a = c(1,2,3), 
  b = c(3,2,1)
  )

my_df <- function(...){
  data.frame(...)
}

my_df(a = c(1,2,3), b = c(3,2,1))

####  3 . Passing and parsing arguments ###

parser <- optparse::OptionParser(formatter = optparse::IndentedHelpFormatter)
parser <-   optparse::add_option(parser, c("-i", "--input-file"), default = "test.txt", dest = "input", help = 'test')
parser <-   optparse::add_option(parser, c("-o","--output-file"), default = "out.txt", dest = "output")

optparse::print_help(parser)

?optparse::IndentedHelpFormatter()


# -------------------------------------------------------------------------

(vec <- seq(0.1, 0.9, by=0.1))
vec == 0.5
vec %>% near(0.7)
head(unlist(.Machine))


# tictoc::tic()
proc.time(runif(1e6))
tictoc::toc()


fun_fill_loop1 <- function(n = 10e6, f) {
  result <- NULL
  for (i in 1:n) {
    result <- c(result, eval(call(f, 1)))
  }
  return("result")
}

fun_fill_loop2 <- function(n = 10e6, f) {
  result <- vector(length = n)
  for (i in 1:n) {
    result[i] <- eval(call(f, 1))
  }
  return("result")
}

fun_fill_vec1 <- function(n = 10e6, f) {
  result <- NULL
  result <- eval(call(f, n))
  return("result")
}


fun_fill_vec2 <- function(n = 10e6, f) {
  result <- vector(length = n)

  result <- eval(call(f, n))
  return("result")
}


tictoc::tic()
fun_fill_loop1(n = 10e4,"runif")
tictoc::toc()

tictoc::tic()
fun_fill_loop2(n = 10e4,"runif")
tictoc::toc()

tictoc::tic()
fun_fill_vec1(n = 10e4,"runif")
tictoc::toc()

tictoc::tic()
fun_fill_vec2(n = 10e4,"runif")
tictoc::toc()

benchmark <- microbenchmark::microbenchmark(
  fun_fill_vec2(n = 1e6, f = "runif"),
  fun_fill_vec1(n = 1e6, f = "runif"),
  times = 10
  )
benchmark

autoplot(benchmark)


parallel::detectCores()

map(1:2, function(x){c(x, x**2, x**3)})

for (cnt in 1:20) {
  if (cnt > 12) {
    print("12+")
  } else {
    print("Not 12+")
  }
}

result <- logical(10)
input <- sample(1:10, size = 10, replace = T)
for (i in 1:length(input)) {
  if (input[i] >= 5) {
    result[i] <- TRUE
  }
}
result

vec <- round(seq(0.1, 0.9, by=0.1), digits = 4)
vec == 0.7
near(tol = 1e10)

vec

N <- 10e3 * 10e3

# By row
t1 <- proc.time()
M <- matrix(sample(1:42, size = N, replace = T), nrow = sqrt(N), byrow = T)
t2 <- proc.time()
(t2 - t1)

# By column
t1 <- proc.time()
M <- matrix(sample(1:42, size = N, replace = T), nrow = sqrt(N), byrow = F)
t2 <- proc.time()
(t2 - t1)

benchmark <-microbenchmark::microbenchmark(
  matrix(sample(1:42, size = N, replace = T), nrow = sqrt(N), byrow = F),
  matrix(sample(1:42, size = N, replace = T), nrow = sqrt(N), byrow = T),
  times = 3
)
benchmark
autoplot(benchmark)

system.time(rnorm(n = 10e6))

timing <- double(100)
for (i in 1:100) {
  st <- system.time(rnorm(n = 10e6))
  timing[i] <- st[3]
}
mean(timing)
var(timing)
st2 <- system.time(replicate(n = 100, rnorm(n = 10e6)))
st2 %>% mean()

microbenchmark::get_nanotime()
library(microbenchmark)
timing <- double(100)
for (i in 1:100) {
  nanotime_start <- get_nanotime()
  rnorm(n = 10e6)
  nanotime_stop <- get_nanotime()
  timing[i] <- nanotime_stop - nanotime_start
}
mean(timing)
var(timing)
boxplot(timing)

precision <- microbenchmark::microtiming_precision()
mean(precision)
var(precision)

mb <- microbenchmark(rnorm(n = 10e6))
autoplot(mb)
boxplot(mb)
summary(mb)
f <- function() {}
mb2 <- microbenchmark(f(), pi, 2+2)
summary(mb2)
autoplot(mb2)

fill_noloop <- function(N, bag, seed = 42) {
  set.seed(seed)
  values <- sample(bag, size = N^2, replace = T)
  M <- matrix(data = values, nrow = N, byrow = T)
  for (col_num in 1:N) {
    M[, col_num] <- M[, col_num] + col_num
  }
  return(M)
}

fill_noalloc <- function(N, bag, seed = 42) {
  set.seed(seed)
  values <- sample(bag, size = N^2, replace = T)
  M <- NULL
  cnt = 1
  for (row in 1:N) {
    row_tmp <- c()
    for (col in 1:N) {
      row_tmp <- c(row_tmp, values[cnt])
      cnt <- cnt + 1
    }
    M <- rbind(M, row_tmp)
  }
  for (col_num in 1:N) {
    M[, col_num] <- M[, col_num] + col_num
  }
  return(M)
}

fill_alloc <- function(N, bag, seed = 42, init = NA) {
  set.seed(seed)
  values <- sample(bag, size = N^2, replace = T)
  M <- matrix(rep(init, times=N^2), nrow = N, byrow = T)
  cnt = 1
  for (row in 1:N) {
    for (col in 1:N) {
      M[row, col] <- values[cnt]
      cnt <- cnt + 1
    }
  }
  for (col_num in 1:N) {
    M[, col_num] <- M[, col_num] + col_num
  }
  return(M)
}


summary <- summaryRprof('profiler_test_fillers.out', memory='both')
summary$by.self


# Day 2 -------------------------------------------------------------------


# Vectorization -----------------------------------------------------------

library(future)

system.time({sample(100,200000000,replace=T)})

plan(sequential)
system.time({sample(100,200000000,replace=T)})

mat <- matrix(1:100000, nrow = 10000, ncol = 10)
for.sum = vector()
tictoc::tic()
for (i in 1:nrow(mat)) {
  for.sum[i] <- sum(mat[i,])
}
tictoc::toc()
head(for.sum)

tictoc::tic()
app.sum = apply(mat, MARGIN = 1, FUN = sum)
tictoc::toc()
head(app.sum)

tictoc::tic()
all(rowSums(mat) == app.sum)
tictoc::toc()


fib_rec <- function(n) {
  if (n == 0 || n == 1) { 
    return(n) 
  } else {
    return(fib_rec(n - 1) + fib_rec(n - 2))
  }
}

tictoc::tic()
fib_rec(28)
tictoc::toc()

tictoc::tic()
sapply(28, fib_rec)
tictoc::toc()

vec_fib_rec <- Vectorize(fib_rec)
tic()
vec_fib_rec(28)
toc()

# non-recurive fib

fib_non_recursive <- function(n){
  vec <- c(0,1, rep(NA, n-2))
  if(n == 0 | n == 1){
    return(n)
  }
  
  for (i in 3:length(vec)) {
    vec[i] = vec[i-1] + vec[i-2]
  }
  return(vec[n])
}
fib_non_recursive_vec <- Vectorize(fib_non_recursive)

tic()
fib_rec(30)
toc()


a <- microbenchmark::microbenchmark(
  fib_rec(30),
  fib_non_recursive(50), 
  fib_non_recursive_vec(50), 
  times = 10)

autoplot(a)


c("1","2","3", "a", NULL, 2) %>% as.double()
a <-c("1","2","3", "a", NULL, 2) %>% parse_double() 
a

attributes(a)


"dasneajselkaj34lk45enl" %>% parse_number()
"dasneajselkaj34lk45enl" %>% str_extract_all("\\d+") %>% unlist() %>% as.numeric()

tic("outer")
tic("inner")
toc()
toc()


# OOP ---------------------------------------------------------------------

test <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)
class(test)
attributes(test)
str(test)
mtcars <- mtcars %>% 
  as_tibble()

class(mtcars)
typeof(mtcars)
str(mtcars)

setClass(Class = "meta_frame", 
  slots = c(
    data ="data.frame",
    descr = "character",
    creation = "character"
  )
)

obj <- new("meta_frame",
  data = mtcars, 
  descr = "Description of mtcars", 
  creation = "23-06-13"
  )

str(obj)

obj_list <- list(
  new("meta_frame",
    data = mtcars, 
    descr = "Description of mtcars", 
    creation = "23-06-13"
  ),
  new("meta_frame",
    data = iris,
    descr = "The Iris dataset",
    creation = "23-06-13")
)

obj_list %>% 
  extract2(1) %>% 
  attributes()

library(gt)
exibble %>% 
  gt(rowname_col = "row", 
    caption = "Caption") %>% 
  sub_missing()
?fmt_bins()


# Day 3 -------------------------------------------------------------------

library(magrittr)
?magrittr::equals()
. %>% sin %>% cos

mtcars %>% 
  as_tibble() %>% 
  select(1:4, 10) %>% 
  filter(disp > mean(disp)) %>% 
  colMeans()

summary(cars)
colSums(cars)

cars %T>% 
  {print(summarise(mean_speed = mean(speed), .data = ))} %>% 
  head()
  
  
  {print(summary(.))} %>% 
  colSums()

mtcars %$%
  cor(gear, mpg)

mtcars %>% cor()

dim_summary <- function(nrows, ncols) {
  print(
    paste0('Matrix M has: ', nrows, ' rows and ', ncols, ' columns.')
  )
}

M <- rnorm(16) %>% 
  matrix(ncol = 4) %T>% 
  plot() %>% 
  add(sample(.)) %T>% 
  {dim_summary(
    nrow(.), 
    ncol(.)
    )}

N <- rnorm(16) %>% 
  matrix(ncol = 4) %>% 
  `colnames<-`(letters[1:4]) %T>% 
  {print(summary(.))} %>% 
  add(0)

M %>% 
  `%x%`(t(N)) %T>% 
  heatmap() %>% 
  `colnames<-`(letters[1:dim(.)[2]]) %>% 
  as_tibble() %$% 
  cor(a, i)


?`%>%`


mat <- sample(1:100, 3000, replace = T) %>% 
  matrix(nrow = 100, ncol = 30) %>% 
  `colnames<-`(paste0("col_", 1:ncol(.)))
  
  
test <- microbenchmark::microbenchmark(
  as.data.frame(mat),
  as_data_frame(mat),
  as_tibble(mat)
)
test %>% autoplot()


tibble(x = sample(1:10, size = 10, replace = T), y = log10(x))

vehicles <- as_tibble(mtcars)
vehicles[[2]]
vehicles[2]
vehicles["cyl"]
vehicles$cyl

vehicles %>% 
  extract2(2)

vehicles %>% 
  extract2("cyl")

vehicles %>% 
  use_series(cyl)

tribble(
  ~id, ~event, ~date,
  1, 'success', '24-04-2017',
  2, 'failed', '25-04-2017',
  3, 'failed', '25-04-2017',
  4, 'success', '27-04-2017'
)

c(1, 7.2, 3.84, -5.23) %>% parse_double()
parse_vector("1,2,3", col_character())
?parse_vector
parse_guess()
parse_guess("1;2;3")

diamonds %>% 
  summarise(mean(depth), .by = cut) %>% 
  arrange(cut)

diamonds %>% 
  arrange(desc(cut))

tibble(
  ID = c("a", "b", "c"),
  values = c("1,2,3", "3,4,231", "4,3,2")
)


parse_guess(c("100;200;300", "80"))

diamonds %>% 
  count(cut)

cases %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "value")

storms %>% 
  separate(date, into = c("year", "month", "day"), sep = "-") %>% 
  unite(DMY, day, month, year, sep ="/") %>% 
  mutate()

test <- tibble(
  a = sample(1:10, 1000000, replace = T)
  )

library(microbenchmark)
benchmark <- microbenchmark(
  test %>% mutate(c = 3 <= a & a <=7 ),
  test %>% mutate(c = between(a, 3, 7))
)
benchmark %>% autoplot()

# Git ---------------------------------------------------------------------

usethis::git_sitrep()
usethis::create_github_token()
gitcreds::gitcreds_set("https://github.com/settings/tokens/1005595874")
usethis::use_git(message = "Initial commit")

usethis::use_github()
