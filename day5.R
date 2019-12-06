library(dplyr)

run_ops_codes <- function(noun, verb)
{
  op_codes <- scan('day5_test.txt', sep=',', quiet=TRUE)
 # op_codes[2] <- noun
  #op_codes[3] <- verb
  
  i <- 1
  while(i <= length(op_codes))
  {
    
    instruction <- toString(op_codes[i]) %>% str_split("")
    instruction <- instruction[[1]]
    
    code <- instruction[length(instruction)-1]
    mode <- instruction[length(instruction)-2]
    parameters <- instruction[1:(length(instruction)-2)]

    # Adds
    if(code=='1')
    {
      op_codes[op_codes[i+3]+1] <- op_codes[op_codes[i+1]+1] + op_codes[op_codes[i+2]+1]
      step_size <- 4
    }
    # Multiplies
    else if (code=='2')
    {
      op_codes[op_codes[i+3]+1] <- op_codes[op_codes[i+1]+1] * op_codes[op_codes[i+2]+1]
      step_size <- 4
    }
    # Value to position
    else if (code == '3')
    {
      op_codes[op_codes[i+1]+1] <- readline(prompt='Input needed: ') %>% as.integer()
      step_size <- 2
    }
    # Print
    else if (code=='4')
    {
      print(op_codes[op_codes[i+1]+1])
      step_size <- 2
    }
    else if (op_codes[i]==99)
    {
      break
    }
    else
    {
      print("Didn't meet conditions, investigate")
    }
    i <- i + step_size
  }
  op_codes
}
run_ops_codes(2,3)

