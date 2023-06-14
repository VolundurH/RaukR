#!/usr/bin/env Rscript


# Args --------------------------------------------------------------------



# suppressPackageStartupMessages(require(optparse))
# 
# parser <- OptionParser(formatter = optparse::IndentedHelpFormatter)
# 
# option_list = list(
#   make_option(c("-m", "--mean"), default=0, )
# )
# options <- parse_args(OptionParser(option_list=option_list))
# 
# mydata <- rnorm(n = 1000, mean = options$mean)
# print(summary(mydata))


# stdIO -------------------------------------------------------------------


input_con  <- file("stdin")
open(input_con)
oneline <- readLines(con = input_con, n = 1, warn = FALSE)
close(input_con)
mean=as.numeric(oneline)
mydata=rnorm(1000,mean = mean)
print(summary(mydata))