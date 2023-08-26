#Q2
u  =1.06
d = 0.95
s0 = 50
r = 0.05
E = 51
months = 3
t1 = months/12
t2 = 2*months/12
sU = u*s0
sU2 = u*sU
sD = d*s0
sD2 = d*sD
sUD = u*d*s0

p = (exp(r*t1) - d)/(u-d)
p_not = 1-p

cu2 = max(sU2 - E,0) 
cud = max(sUD - E,0)
cd2 = max(sD2 - E,0)

C = (p^2*cu2 + 2*p*(p_not)*cud + p_not^2*cd2)*exp(-r*t2) 
cu = (p*cu2 + p_not*cud)*exp(-r*t1)
cd = (p*cud + p_not*cd2)*exp(-r*t1)
c_2 = (cu*p + cd *p_not)*exp(-r*t1)

print(sU)
print(sU2)
print(sD)
print(sD2)
print(sUD)
print(p)
print(p_not)
print(cu2)
print(cud)
print(cd2)
print(cu)
print(cd)
print(C)
print(c_2)


#Q3
u  =1.06
d = 0.95
s0 = 50
r = 0.05
E = 51
months = 3
t1 = months/12
t2 = 2*months/12
sU = u*s0
sU2 = u*sU
sD = d*s0
sD2 = d*sD
sUD = u*d*s0

p = (exp(r*t1) - d)/(u-d)
p_not = 1-p

pu2 = max(E - sU2,0) 
pud = max(E - sUD ,0)
pd2 = max(E - sD2,0)

P = (p^2*pu2 + 2*p*(p_not)*pud + p_not^2*pd2)*exp(-r*t2) 
pu = (p*pu2 + p_not*pud)*exp(-r*t1)
pd = (p*pud + p_not*pd2)*exp(-r*t1)
P_2 = (pu*p + pd *p_not)*exp(-r*t1)

parity_right = C + E*exp(-r*t2) 
parity_left = P + s0
dif = parity_right - parity_left

print(sU)
print(sU2)
print(sD)
print(sD2)
print(sUD)
print(p)
print(p_not)
print(pu2)
print(pud)
print(pd2)
print(pu)
print(pd)
print(P)
print(P_2)
print(dif)

