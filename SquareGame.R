lgrid <- matrix(NA, nrow = 8, ncol = 8)
lgrid[1,] <- c("r", "l", "q", "s", "t", "z", "c", "a")
lgrid[2,] <- c("i", "v", "d", "z", "h", "l", "t", "p")
lgrid[3,] <- c("u", "r", "o", "y", "w", "c", "a", "c")
lgrid[4,] <- c("x", "r", "f", "n", "d", "p", "g", "v")
lgrid[5,] <- c("h", "j", "f", "f", "k", "h", "g", "m")
lgrid[6,] <- c("k", "y", "e", "x", "x", "g", "k", "i")
lgrid[7,] <- c("l", "q", "e", "q", "f", "u", "e", "b")
lgrid[8,] <- c("l", "s", "d", "h", "i", "k", "y", "n")

green_position <- c(14, 23, 42, 51)
white_direction <- c(-1, 1, -7, 7, -8, 8, -9, 9)
is_edge_square <- function(current) {
  is_edge <- (current <= 8 || current >= 57 || current %% 8 == 0 || current %% 8 == 1)
  return(is_edge)
}

is_green_square <- function(current) {
  is_green <- current %in% green_position
  return(is_green)
}

move_square <- function(current) {
  next_square = 0
  if (is_edge_square(current)) {
    next_square <- sample(1:64, size = 1)
  } else {
    next_square <- current + sample(white_direction, size = 1)
  }
  return(next_square)
}

## if current is a white square, 
## choose whether to add the letter to the collection
choose_whether_add <-function(current, collection) {
  collect_length <- length(collection)
  letter <- lgrid[current]
  if (collect_length < 3) {
    return(TRUE)
  } else if (collect_length == 3){
    return(letter %in% collection)
  } else if (collect_length == 4){
    return(is_palindrome(letter, collection))
  }
}


## if current is a green square
## the random events happen
green_events <- function(current, green_prob, collection) {
  ## random event happen according to the probability 
  mode <- sample(0:1, size = 1, prob=c(green_prob, 1-green_prob))
  if(mode == 0) {
    collection <- c("f", "f", "h", "k")
  } else {
    letter <- lgrid[current]
    remove_index <- which(collection == letter)
    if(length(remove_index) != 0) {
      collection <- collection[-remove_index]
    }
  }
  return(collection)
}

is_palindrome <- function(letter, collection) {
  collection <- sort(collection)
  
  ## like "abcc"
  if (collection[1] != collection[2] && collection[2] != collection[3]) {
    return((letter == collection[1] || letter == collection[2]))
  }
  
  ## like "aabc"
  if (collection[1] == collection[2] && collection[2] != collection[3] && collection[3] != collection[4]) {
    return((letter == collection[3] || letter == collection[4]))
  }
  
  ## like aabb aaaa
  if (collection[1] == collection[2] && collection[3] == collection[4]) {
    return(TRUE)
  }
  
  ## like abbc abbb aaab
  return(letter == collection[1] || letter == collection[4])
}

# start_position is a vector like(4, 4) means square D4
count_num_turns <- function(lgrid_matrix,start_position,green_prob, to_print) {
  num_turns <- 0
  current <- start_position[1] + (start_position[2] - 1) * 8
  collection <- vector()
  while(length(collection) < 5) {
    ## for green square
    if(is_green_square(current)) {
      if(to_print == T) cat ("current square = ", current, "it's a green square \n")
      collection <- green_events(current, green_prob, collection)
      current <- move_square(current)
    } else {
      ## for white square 
      ## choose whether to add current square letter to the collection
      if(to_print == T) cat ("current square = ", current, "it's an white square \n")
      if(choose_whether_add(current, collection)) {
        collection <- append(collection, lgrid[current])
      }
      
      ## Is current square an edge square?
      if(is_edge_square(current)) {
        if(to_print == T) cat ("it's an edge square!\n")
        current <- move_square(current)
      } else {
        current <- move_square(current)
      }
    }
    num_turns <- num_turns + 1
  }
  if(to_print == T) {
    cat ("the collection result:", collection, "\n")
    cat ("the number of turns:", num_turns, "\n")
  }
  return(num_turns)
}

