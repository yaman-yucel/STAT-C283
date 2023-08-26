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
d1 = calc_d1(s0,E,sigma,t) # delta

phi_d1 = pnorm(d1)
n_shares_must_purchase = n_shares * phi_d1

n_money_must_borrow = n_shares_must_purchase * s0

cost_week_1 = n_money_must_borrow
# Second week
weekly_interest = n_money_must_borrow * (exp(r*1/52) - 1)
# At the end of the new week stock price is 48.12

s1 = 48.12
t_remaining = t - (1/52)
d1_new = calc_d1(s1,E,sigma,t_remaining)
phi_d1_new = pnorm(d1_new)
n_shares_must_sell = n_shares_must_purchase - n_shares * phi_d1_new
n_shares_must_have= n_shares * phi_d1_new

cost_week_2 = n_money_must_borrow + weekly_interest - n_shares_must_sell*s1
