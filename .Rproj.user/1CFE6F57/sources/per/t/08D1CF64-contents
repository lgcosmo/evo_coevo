coevo<-function(sx, sy, a, h, v_zx, v_zy, tmax, z0_x, z0_y){ #Step (1)
  
  dzx = c() #Creating vector to store values of the mean trait value of the population of species 1
  dzy = c() #Creating vector to store values of the mean trait value of the population of species 2
  
  dzx[1] = z0_x #Setting initial mean trait value in the population of species 1
  dzy[1] = z0_y #Setting initial mean trait value in the population of species 1
  
  #For loop to iterate over the equation - here we will use Lande's equation
  for(t in 1:(tmax-1)){
    
    # Step (2)
    
    zx = dzx[t] #Setting the mean trait value at the current generation of the population of species 1
    zy = dzy[t] #Setting the mean trait value at the current generation of the population of species 2
    
    # Step (3)
    
    m1=sx*2*a*(exp(-a*((zx-zy)^2+v_zx+v_zy)))/(1-sx*exp(-a*((zx-zy)^2+v_zx+v_zy)))
    m2=sy*2*a*(exp(-a*((zx-zy)^2+v_zx+v_zy)))/(1+sy*exp(-a*((zx-zy)^2+v_zx+v_zy)))
    
    #m1=0.5
    #m2=0.7
    
    dlnw1 = m1*(zx-zy)
    dlnw2 = m2*(zx-zy)
    
    # Step (4)
    
    dzx[t+1]=zx + h*v_zx*dlnw1 #Using Lande's equation to compute mean trait value at the next generation of the population of species 1
    dzy[t+1]=zy + h*v_zy*dlnw2 #Using Lande's equation to compute mean trait value at the next generation of the population of species 2
    
  }
  
  r=data.frame(dzx, dzy, t=1:tmax) # Step (5), creating a data frame with frequencies over time
  
  return(r)
  
}

r=coevo(sx=0.1, sy=0.15, a=0.1, tmax=5000, z0_x=5, z0_y=1, h=0.5, v_zx=0.5, v_zy=0.5)

ggplot()+
  geom_line(data=r, aes(x=t, y=dzx), color="royalblue3")+ # Frequency of allele A for victims, in blue
  geom_line(data=r, aes(x=t, y=dzy), color="firebrick3")+ #Frequency of allele A for exploiters, in red
  xlab("Generations")+ylab("Frequency of allele A")+
  theme_pubr()

library(ggplot2)
library(ggpubr)
