library(purrr)
fuel_calc <- function(module_mass)
{
  module_mass  %/% 3 - 2 
}

fuel_calc_extra_fuel <- function(module_mass)
{
  module_fuel <- fuel_calc(module_mass) 
  total_module_fuel <- module_fuel
  fuel_for_fuel <- fuel_calc(module_fuel)
  while(fuel_for_fuel > 0)
  {
    total_module_fuel <- total_module_fuel + fuel_for_fuel
    fuel_for_fuel <- fuel_calc(fuel_for_fuel)
  }
  total_module_fuel
}

module_mass = read.table('day1.txt', header=FALSE, col.names=c('mass'))

# Part 1
module_mass$module_fuel <- purrr::map_dbl(module_mass$mass, fuel_calc)
print(sum(module_mass$module_fuel))

# Part 2
module_mass$module_fuel_extra <- purrr::map_dbl(module_mass$mass, fuel_calc_extra_fuel)
print(sum(module_mass$extra_fuel))
