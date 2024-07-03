library(reshape2)
library(ggplot2)
library(ggpubr)

x_dp = c()
y_dp = c()
x_p0 = 0.3
y_p0 = 0.3
x_dp[1] = x_p0
y_dp[1] = y_p0

sx = 0.2
sy = 0.1

AA = 1
AB = 0
BA = 0
BB = 1

Q = matrix(data=c(AA,BA,AB,BB), nrow=2, ncol=2)
rownames(Q)=c("Ax", "Bx")
colnames(Q)=c("Ay", "By")

tmax=1000


for(t in 1:(tmax-1)){
  
  xp = x_dp[t]
  yp = y_dp[t]

  x_wA = 1 - sx*(Q[1,1]*yp + Q[1,2]*(1-yp)) 
  x_wB = 1 - sx*(Q[2,1]*yp + Q[2,2]*(1-yp))
  x_avg_w = xp*x_wA + (1-xp)*x_wB

  y_wA = 1 + sy*(Q[1,1]*xp + Q[2,1]*(1-xp)) 
  y_wB = 1 + sy*(Q[1,2]*xp + Q[2,2]*(1-xp))
  y_avg_w = yp*y_wA + (1-yp)*y_wB
  
  x_dp[t+1]=xp + ((xp*(1-xp)*(x_wA-x_wB))/x_avg_w)
  y_dp[t+1]=yp + ((yp*(1-yp)*(y_wA-y_wB))/y_avg_w)
  
}

r=data.frame(x_dp, y_dp, t=1:tmax)

ggplot()+
  geom_line(data=r, aes(x=t, y=x_dp), color="blue")+
  geom_line(data=r, aes(x=t, y=y_dp), color="red")+
  theme_pubr()
