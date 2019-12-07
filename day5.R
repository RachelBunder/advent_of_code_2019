library(dplyr)
library(stringr)


run_ops_codes <- function()
{
  # The expected number of parameters for the code
  number_params <- c('01'=3, '02'=3, '03'=1, '04'=1)
  
  op_codes <- scan('day5.txt', sep=',', quiet=TRUE)
  i <- 1
  while(i <= length(op_codes))
  {
    print(paste('iteration', i))
    instruction <- toString(op_codes[i]) %>% str_split("")
    instruction <- instruction[[1]]
    
    # Extracting ops code form instruction
    # If instuction is a single number, pad with 0
    if(length(instruction)==1)
    {
      code <- instruction
      code[2] <- 0
      code <- paste(rev(code), collapse='')
      
      # The additional parameters needed will be padded out later
      parameters <- c(0)
    }
    else
    {
      code <- paste(instruction[(length(instruction)-1):length(instruction)], collapse='')
      
      # Separate parameters and add in 0 for unspecified values
      parameters <- instruction[1:(length(instruction)-2)[1]] %>% rev()
    }

    if(code=='99')
    {
      return(op_codes)
    }
    
    print(paste('code', code))
    parameters <- parameters[1:number_params[code]]
    parameters[is.na(parameters)] <- '0'
 
    # Reassign the value of each parameter to be what is actually needed
    # I'm pretty sure this could be done in lapply, but I'm not sure how to get
    # p value in there as well
    p <- 1
    for(param in parameters)
    {
      # if in position mode, look at the value in the position
      # If writing, we ust want the location value, not the value in it
      if(p==3)
      {
        parameters[p] <- op_codes[i+p]
      }
    # Reading, always going to save to the right place
    else if (code == '03')
    {
      parameters[p] <- op_codes[i+p]
    }
      # if in relative mode, it is just itself. 
      else if(param == '1' | code =='03')
      {
        print(paste('parameter', p, parameters[p]))
        parameters[p] <- parameters[p]
      }
      else if(param == '0')
      {
          parameters[p] <- op_codes[op_codes[i+p]+1]
      }
      # if in relative mode, it is just itself,but if we are writing the next one
      else if(param=='1' & code=='04')
      {
        
      }
      else
      {
        print("Not known mode")
      }
      p <- p + 1
    }
    parameters <- strtoi(parameters)
    print(paste(i, code, instruction))
    # Adds
    if(code=='01')
    {
      op_codes[parameters[3]] <- parameters[1] + parameters[2]
      step_size <- 4
      print(paste('adds', parameters[1], parameters[2], 'saves to', parameters[3]))
    }
    # Multiplies
    else if (code=='02')
    {
      op_codes[parameters[3]] <-  parameters[1] * parameters[2]
      step_size <- 4
    }
    # Value to position
    else if (code == '03')
    {
      op_codes[parameters[1] + 1] <- 1# readline(prompt='Input needed: ') %>% as.integer()
      print(paste('writes',1, 'to', parameters[1]))
      step_size <- 2
    }
    # Print
    else if (code=='04')
    {
      print(op_codes[parameters[1]])
      step_size <- 2
      
      print(paste('in func', i))
      return(op_codes)
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
result <- run_ops_codes()


result
