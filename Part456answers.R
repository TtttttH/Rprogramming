##install.packages("tidyverse")
library(tidyverse)

## Part4

## change the green_prob from 0.05 to 0.95,
## and replicate game function 10000 times with each probability
## then calculate the mean of turns and compare the results

dist_of_num_turns_one <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.05, to_print = F))
mean(dist_of_num_turns_one)

dist_of_num_turns_two <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.20, to_print = F))
mean(dist_of_num_turns_two)

dist_of_num_turns_three <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.40, to_print = F))
mean(dist_of_num_turns_three)

dist_of_num_turns_four <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.60, to_print = F))
mean(dist_of_num_turns_four)

dist_of_num_turns_five <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.80, to_print = F))
mean(dist_of_num_turns_five)

dist_of_num_turns_six <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.95, to_print = F))
mean(dist_of_num_turns_six)
## Part5
dist_of_num_turns_seven <- replicate(10000, count_num_turns(lgrid, c(6,6), green_prob=0.05, to_print = F))
mean(dist_of_num_turns_seven)

## Part6