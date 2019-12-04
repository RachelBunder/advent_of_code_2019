library(dplyr)
library(stringr)
min_password <- 152085
max_password <- 670283

to_int_array <- function(num)
{
  num <- toString(num)
  num <- strsplit(num, split='')
  num <- lapply(num, as.numeric)[[1]]
  num
}

monotonically_increasing <- function(nums)
{
  all(nums == cummax(nums))
}

double_digit <- function(nums)
{
  lag1 <- paste(nums - lag(nums), collapse="")
  
  "0" %in% lag1
}

double_digit_no_triple <- function(nums)
{
  lag1 <- paste(nums - lag(nums), collapse="")
  
  lag1 %in% str_extract_all(lag1, "NA(([0-9]*[1-9])?0([1-9][0-9]*){1}|([0-9]*[1-9]){1}0([1-9][0-9]*)?)")
}


password_count_1 <- 0
password_count_2 <- 0
for(i in min_password:max_password)
{
  password <- to_int_array(i)
  
  if(length(password)==6&
     monotonically_increasing(password)&
     double_digit(password)
  )
  {
    password_count_1 <- password_count_1 + 1
  }
  if(length(password)==6&
     monotonically_increasing(password)&
     double_digit_no_triple(password)
  )
  {
    password_count_2 <- password_count_2 + 1
  }
}
print(password_count_1)
print(password_count_2)
