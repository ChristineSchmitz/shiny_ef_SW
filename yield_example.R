#simple test monte carlo 

#Packages:#####
library(decisionSupport)
library(tidyverse)

orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")                      

apple_yield_function<-function(){
  
  yield<-trees_per_ha*apples_per_tree*g_per_apple/1000
  
  return(list(yield_at_harvest = yield))
}

apple_yield_mc_simulation <- mcSimulation(estimate = as.estimate(orchard_data),
                                                          model_function = apple_yield_function,
                                                          numberOfModelRuns = 10000,
                                                          functionSyntax = "plainNames")
plot_distributions(mcSimulation_object = apple_yield_mc_simulation, 
                   vars = c("yield_at_harvest"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)
