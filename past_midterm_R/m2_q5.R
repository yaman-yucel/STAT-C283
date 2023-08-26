#lower bound check
#call
# c > s0 - E/(1+r) = s0 - Ee^(-rt)

c = 5   # price of call
s0 = 10 # stock price at t = 0
E = 9   # strike price for call
r = 0.1 # interest rate for year
t = 1   # time to expiration
continous  = TRUE
rp = (1+r)^t - 1

left1 = c
if(continous){
  right1 = s0 - E*exp(-r*t)
}else{
  right1 = s0 - E/(1+rp)
}

if( left1 >right1){
  print("HOLDS LOWER CALL")
}

#put

# p >= E/(1+r) - s0 = Ee^(-rt) - s0

p = 2.5    # price of put
s0 = 47    # stock price at t = 0
E = 50     # strike price for call
r = 0.06   # interest rate for year
t = 1/12   # time to expiration in years
continous  = TRUE


rp = (1+r)^t - 1

left2 = p
if(continous){
  right2 =  E*exp(-r*t) - s0 
}else{
  right2 =  E/(1+rp) - s0
}

if( left2 > right2){
  print("HOLDS LOWER PUT")
}

#Borrow p + s0 
#Buy stock and put
debt = (p + s0)*exp(r*t)


#upper bound check 
#call

#put

#Put call-parity