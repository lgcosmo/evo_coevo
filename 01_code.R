library(ggplot2)
library(ggpubr)

a = 0.2
b = 0.2

p<-seq(0, 1, 0.001)
wa = 1 + a*p
wb = 1 + b*(1-p)
w_avg=p*wa + (1-p)*wb

df = data.frame(a, b, p, wa, wb, w_avg)

ggplot()+
  geom_point(data=df, aes(x=p, y=w_avg))+
  theme_pubr()

a = 0.2
b = 0.2
p0=0.6
pt=c()
pt[1]<-p0
tmax=100

for(t in 1:(tmax-1)){
  
  p<-pt[t]
  wa = 1 + a*p
  wb = 1 + b*(1-p)
  w_avg=p*wa + (1-p)*wb
  pt[t+1]= p + ((p*(1-p)*(wa-wb))/w_avg)
  
}

df=data.frame(t=1:100, p=pt)

ggplot()+
  geom_line(data=df, aes(x=t, y=p))+
  scale_y_continuous(limits=c(0,1))+
  theme_pubr()
