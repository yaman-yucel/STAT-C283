  u = 1.2
  d = 1/u
  E = 60
  s0 = 50
  n = 10
  r = 0.1 # for each period, not continuous compounding
  
  k = ceiling(log(E/(d^n*s0))/log(u/d)) # 6
  
  p = ((1+r) - d)/(u-d)
  p_not = 1-p
  
  last_price_list = rep(0,n + 1)
  call_list = rep(0,n+1)
  
  for (j in 0:n){
    last_price_list[j+1] = u^(j) * d^(n-j) * s0
    call_list[j+1] = max(last_price_list[j+1] - E,0)
  }
  p_mark = p*u/(1+r)
  p_mark_not = 1-p_mark
  
  c = s0*pbinom(k-1,n,p_mark, lower.tail=FALSE) - (E/(1+r)^n)*pbinom(k-1, n, p, lower.tail=FALSE)
  
  get_comb <- function(n,k){ factorial(n)/(factorial(k)*factorial(n-k))}
  c_2 = 0
  for (i in k:n){
    c_2 = c_2 + get_comb(n,i) * p^i*(1-p)^(n-i)*call_list[i+1]
  }
  c_2 = c_2 / ((1+r)^n)
  
  dif = c - c_2
  
  print(k)
  print(p)
  print(p_not)
  print(last_price_list)
  print(call_list)
  print(p_mark)
  print(p_mark_not)
  print(c)
  print(c_2)
  print(dif)