# The plan for this one is to make two lists containing all the points that the wires go over
# Then find the intersection of these lists

library(tidyr)

steps <- data.frame(t(read.table('day3.txt', sep=',',header=FALSE, row.name=c('wire1', 'wire2'))))
                    
steps <- steps %>% separate(wire1,
                   into=c('wrire1_dir', 'wire1_steps'),                                      
                   sep="(?<=[A-Z])(?=[0-9])")

steps <- steps %>% separate(wire2,
                   into=c('wrire2_dir', 'wire2_steps'),                                      
                   sep="(?<=[A-Z])(?=[0-9])")

current_pos = c(0,0)
