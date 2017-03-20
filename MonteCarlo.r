rm(list = ls(all=T))

### define the function for bankruptcy
fnbankrupt <- function(simulations){
  
  ### number of customers per year for that insurance company is 700 to 900
  num_cust <- sample(700:900,simulations, replace = T)
  
  ### premium taken from each custumer is between 1500 to 2500 per month. so we can multiply this by 12 to get early premium
  cust_prem <- sample(18000:30000,simulations, replace = T)
  
  ### expected claims in a year are between 250 to 500
  exp_claim <- sample(250:500,simulations,replace = T)
  
  ### average amount per claim stands between 20000 to 150000
  avg_claim <- sample(20000:150000,simulations,replace = T)
  
  ### the amount company gets through premium will be premium per customers multiplied by the number of customers
  amt <- cust_prem * num_cust
  
  ### the claim amount to be given by the company will be expected claims per year multiplied by the average claim amount per claim
  claim_amt <- exp_claim * avg_claim
  
  ### a company will go bankrupt if the amount claimed by customers exceeds the amount earned by the company through premium
  bnkrpt <- sum(amt < claim_amt)
  
  ### we will find the probability where claim amount exceeds the amount earned in premium
  probability <- bnkrpt / simulations
  
  cat("probability is : ", probability, " for ",simulations, " simulations ","\n")
}

### define number of simulations, stop if two simulations give approximately same probability
simulations=c(10,100,1000,10000,100000,1000000)

### this will note the start time of simulations
start = Sys.time()


for (i in simulations) {
  probability=fnbankrupt(i)
}

end=Sys.time()-start
end
