library(ggplot2)
library(ggpubr)

n_samples<-10000
b<-0.5
y<-0.25
g<-1
theta=3

z<-rnorm(n_samples, mean=5, sd=1)
e<-rnorm(n=n_samples, sd=0.05)
w<-1+b*z+e
w2<-exp(-0.2*((theta-z)^2+e))
w2<-1+b*z-y*z^2+e
w3<-1+log(b*z)+e

cov(w2,z)/mean(w2)

mean(w2)
cov(w2, (z-mean(z))^2)/mean(w2)
cov(w2, z)/var(z)

lm()

df<-data.frame(b=b,y=y,z=z, z2=(z-mean(z))^2, w=w, w2=w2, w3=w3)

ggplot(data=df, aes(x=z, y=w2))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_regline_equation()+
  scale_y_continuous(limits=c(0, 1.3))+
  theme_pubr()

ggplot(data=df, aes(x=z2, y=w2))+
  geom_point()+
  geom_smooth(method="lm")+
  stat_regline_equation(label.y.npc = "bottom")+
  theme_pubr()

lm(w2~z, data=df)

0.1001*var(z)

cov(w,z)/sd(z)

stat_regline
cor(w,z, method=c("pearson"))*var(z)

z<-seq(0, 10, 0.01)
w<-exp(-(5-z)^2)

df<-data.frame(z=z, w=w)

teste<-lapply(seq(0, 10, 0.01), FUN=function(x){z<-rnorm(n=10000, mean=x, sd=0.5); w<-exp(-(5-z)^2); return(data.frame(w=mean(w), z=mean(z)))})
teste<-do.call(rbind, teste)

ggplot(data=teste, aes(x=z, y=w))+
  geom_point()+
  theme_pubr()
