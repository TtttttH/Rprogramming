##install.packages("tidyverse")
##install.packages("devtools")
##install.packages("gridExtra")
library(tidyverse)

## Part4

## change the green_prob from 0.05 to 0.95,
## and replicate game function 10000 times with each probability
## then calculate the mean of turns and compare the results

mean_dist_of_num_turns <- function(replicate_time = 10000, square_position, green_prob) {
  dist_of_num_turns <- replicate(replicate_time, count_num_turns(lgrid, square_position, green_prob, to_print = F))
  return(mean(dist_of_num_turns))
}

mean_one <- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.05)
mean_two <- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.20)
mean_three<- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.40)
mean_four <- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.60)
mean_five <- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.80)
mean_six <- mean_dist_of_num_turns(square_position = c(4,4), green_prob = 0.95)

df_part4 <- data.frame(prob = c(0.05, 0.20, 0.40, 0.60, 0.80, 0.95),
                       mean_value = c(mean_one, mean_two, mean_three, mean_four, mean_five, mean_six))

ggplot(data = df_part4, aes(x = prob, y = mean_value)) +
  geom_point(size = 3, colour = "red") +
  geom_line(size = 1, colour = "blue") +
  labs(x = " probability p", y = "Mean moves ",
       title = "Mean moves to form a five-letter palindrome with different probability",
       subtitle = "The mean moves tends to decrease with p increasing")
## Part5
dist_of_num_turns_d4 <- replicate(10000, count_num_turns(lgrid, c(4,4), green_prob=0.95, to_print = F))
dist_of_num_turns_f6 <- replicate(10000, count_num_turns(lgrid, c(6,6), green_prob=0.05, to_print = F))

hist(dist_of_num_turns_d4, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )
hist(dist_of_num_turns_f6, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )

df_d4 <- as.data.frame(dist_of_num_turns_d4)
df_f6 <- as.data.frame(dist_of_num_turns_f6)

ggplot(df_d4, aes(x = dist_of_num_turns_d4)) + 
  geom_bar()

ggplot(df_f6, aes(x = dist_of_num_turns_f6)) + 
  geom_bar()

df_d4_count <- df_d4 %>% count(dist_of_num_turns_d4)
p_d4 <- ggplot(data = df_d4_count, aes(x = dist_of_num_turns_d4, y = n)) + 
  geom_point() +
  geom_smooth()
  
p_f6 <- ggplot(data = df_f6_count, aes(x = dist_of_num_turns_f6, y = n)) + 
  geom_point() +
  geom_smooth()

gridExtra::grid.arrange(p_d4, p_f6)

quantile(dist_of_num_turns_d4, seq(0.1, 1, 0.05))
quantile(dist_of_num_turns_f6, seq(0.1, 1, 0.05))

## Part6
A <- c(25, 13, 16, 24, 11, 12, 24, 26, 15, 19, 34)
B <- c(35, 41, 23, 26, 18, 15, 33, 42, 18, 47, 21, 26)

# Shapiro-Wilk Normality test
shapiro.test(A)
shapiro.test(B)

# Homogeneity of variance test
var.test(A, B)

# student-t test
t.test(A, B, var.equal = TRUE)

