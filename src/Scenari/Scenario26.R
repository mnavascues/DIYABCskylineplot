# population size in number of individuals

demography <- function(generation){
  
  population_size <- array(NA,length(generation))
  
  population_size_0 <- 20000 #(in number of genes)

  # constant size
  for (gen in 1:length(generation)){
  	population_size[gen] <- population_size_0
  }

  return(population_size)
}
