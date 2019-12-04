# My initial plan was to make a list of two lists containing all the points that the wires go over
# Then find the intersection of these lists and calculate the distances
# But then I was finding that hard to do in R and for some reason I thought it was a good idea
# to have a matrix of all the points... Yeah. It works, but it is slow and ridiculously memory intensive.
# Would not reccomend

library(tidyr)
library(dplyr)
library(purrr)

define_directions <- function(direction, value)
{
  if (direction == 'R'){
    movement <- c(0, value)
  }
  else if (direction == 'L'){
    movement <- c(0, -1*value)
  }
  else if (direction =='U'){
    movement <- c(value, 0)
  }
  else if (direction == 'D'){
    movement <- c(-1*value, 0)
  }
  else{
    print('Unknown direction')
  }
  movement
}

manhatten <- function(pos1, pos2)
{
  abs(pos1[1] - pos2[1]) + abs(pos1[2] - pos2[2])
}

# There is a warning message saying incomplete final line, but comparing the lengths it it fine
steps <- data.frame(t(read.table('day3.txt', sep=',',header=FALSE, row.name=c('wire1', 'wire2'))))
                    
steps <- steps %>% separate(wire1,
                   into=c('wrire1_dir', 'wire1_steps'),                                      
                   sep="(?<=[A-Z])(?=[0-9])",
                   convert=TRUE)

steps <- steps %>% separate(wire2,
                   into=c('wrire2_dir', 'wire2_steps'),                                      
                   sep="(?<=[A-Z])(?=[0-9])",
                   convert=TRUE)

current_pos = c(0,0)

#steps <- 
#cal <-  purrr::map2(steps$wire1_dir, steps$wire1_steps, define_dire

matrix_size <- 30000
#matrix_size <-20
wire1_path <- matrix(0L, nrow = matrix_size, ncol = matrix_size)
wire2_path <- matrix(0L, nrow = matrix_size, ncol = matrix_size)
centre_point <- c(matrix_size/2, matrix_size/2)

wire1_last_position <- centre_point
wire2_last_position <- centre_point

wire1_path[wire1_last_position] <- -1
wire2_path[wire2_last_position] <- -1

wire1_steps <- 0
wire2_steps <- 0

for(row in 1:nrow(steps))
{
  wire1_move <- wire1_last_position + define_directions(steps[row, 'wrire1_dir'], steps[row, 'wire1_steps'])
  wire2_move <- wire2_last_position + define_directions(steps[row, 'wrire2_dir'], steps[row, 'wire2_steps'])
  
  # Setting all the posisitions transversed in the last move to 1
  wire1_path[wire1_last_position[1]:wire1_move[1], wire1_last_position[2]:wire1_move[2]] <- pmin(wire1_steps + 0:steps[row, 'wire1_steps'])
  wire2_path[wire2_last_position[1]:wire2_move[1], wire2_last_position[2]:wire2_move[2]] <- pmin(wire2_steps + 0:steps[row, 'wire2_steps'])
  
  print(wire1_steps + 0:steps[row, 'wire1_steps'])
  
  wire1_steps <- wire1_steps + steps[row, 'wire1_steps']
  wire2_steps <- wire2_steps + steps[row, 'wire2_steps']
  
  
  wire1_last_position <- wire1_move
  wire2_last_position <- wire2_move
}


# 
min_dist_manhat = 10000000000
min_dist_steps = 10000000000
for(row in 1:matrix_size)
{
  for(col in 1:matrix_size)
  {
    if(wire1_path[row, col]>0 & wire2_path[row, col] >0 & row != centre_point[1] & col != centre_point[2])
    {
      dist <- manhatten(centre_point, c(row, col))
      min_dist_manhat <- min(c(dist, min_dist_manhat))
      
      dist <- wire1_path[row, col] + wire2_path[row, col]
      min_dist_steps <- min(c(dist, min_dist_steps))
    }
  }    
}
# Part 1
print(min_dist_manhat)
# Part 2
print(min_dist_steps)
