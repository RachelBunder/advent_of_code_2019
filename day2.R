run_ops_codes <- function(noun, verb)
{
  op_codes <- scan('day2.txt', sep=',', quiet=TRUE)
  op_codes[2] <- noun
  op_codes[3] <- verb
  
  i <- 1
  while(i <= length(op_codes))
  {
    # Adds
    if(op_codes[i]==1)
    {
      op_codes[op_codes[i+3]+1] <- op_codes[op_codes[i+1]+1] + op_codes[op_codes[i+2]+1]
    }
    else if (op_codes[i]==2)
    {
      op_codes[op_codes[i+3]+1] <- op_codes[op_codes[i+1]+1] * op_codes[op_codes[i+2]+1]
    }
    else if (op_codes[i]==99)
    {
      break
    }
    else
    {
      print("Didn't meet conditions, investigate")
    }
    i <- i + 4
  }
  op_codes
}
# Part 1
part1_code <- run_ops_codes(1, 12)
print(part1_code[1])

# Part 2
target_value <- 19690720
found=FALSE
for(noun in 0:99)
{
  for(verb in 0:99)
  {
    result <- run_ops_codes(noun, verb)
    if(target_value == result[1])
    {
      print(sprintf('Found target, %s, %s', noun, verb))
      found=TRUE
      break
    }
  }
  if(found)
  {
    break
  }
  
}
print(100*noun + verb)?
  