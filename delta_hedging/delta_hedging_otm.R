df <- data.frame (
  week = seq(0,20),
  stock_price = c(49,49.75,52.00,50.00,48.38,48.25,48.75,49.63,48.25,48.25,51.12,51.50,49.88,49.88,48.75,47.50,48.00,46.25,48.13,46.63,48.12),
  delta = rep(0,21),
  shares_hold = rep(0,21),
  shares_purchase = rep(0,21),
  cost_of_shares = rep(0,21),
  #total_cost_of_shares = rep(0,21),
  total_cost_with_interest = rep(0,21),
  interest_cost = rep(0,21)
)

sold_C = 300000
n_shares = 100000
t = 20/52
s0 = 49
E = 50
r = 0.05
sigma = 0.2
mu = 0.13
BSM_C = 240000


calc_d1 <- function(s0, E, sigma,t)
{
  d1 = (log(s0/E) + (r+ 0.5*sigma^2)*t)/(sigma*sqrt(t))
  return(d1)
}
for (i in 1:nrow(df)){
  s0 = df[i,"stock_price"]
  t = (1/52)*(nrow(df) - 1 - i + 1)
  d1 = calc_d1(s0,E,sigma,t)
  delta = pnorm(d1)
  df[i,"delta"] = delta
}

for (i in 1:nrow(df)){
  if( i == 1)
  {
    delta = df[i,"delta"]
    df[i,"shares_hold"] = delta  * n_shares
    df[i,"shares_purchase"] = delta  * n_shares
  }
  else{
    delta = df[i,"delta"]
    shares_hold = delta*n_shares
    df[i,"shares_hold"] = shares_hold
    n_purchase = delta*n_shares - df[i-1,"shares_hold"]
    df[i,"shares_purchase"] = n_purchase
  }
}

for (i in 1:nrow(df)){
  cost_of_shares = -df[i,"shares_purchase"]*df[i,"stock_price"]
  df[i,"cost_of_shares"] = cost_of_shares
}
for (i in 1:nrow(df)){
  
  cost_of_shares = -df[i,"shares_purchase"]*df[i,"stock_price"]
  df[i,"cost_of_shares"] = cost_of_shares
}

for (i in 1:nrow(df)){
  if( i == 1){
    interest = -df[i,"cost_of_shares"] * (exp(r*1/52) - 1)
    df[i,"interest_cost"] = interest
    df[i,"total_cost_with_interest"] = -df[i,"cost_of_shares"]
  }
  else{
    total_cost_with_interest = df[i-1,"total_cost_with_interest"] + df[i - 1,"interest_cost"] - df[i,"cost_of_shares"]
    df[i,"total_cost_with_interest"] = total_cost_with_interest
    df[i,"interest_cost"] = df[i,"total_cost_with_interest"] * (exp(r*1/52) - 1)
  }
}