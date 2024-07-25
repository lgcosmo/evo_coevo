a<-0.2

g<-D(expression(-a*(theta-z)^2), "z")


coevo_network<-function(net, z, theta, phi, rho, m, alpha, tmax){
  
  # Setting up matrix for simulations
  n_p <- nrow(net)
  n_a <- ncol(net)
  n_sp <- n_p + n_a
  # Changing labels
  rownames(net) <- paste("P", 1:n_p, sep = "")
  colnames(net) <- paste("A", 1:n_a, sep = "")
  net[net > 0] <- 1 # Making the matrix binary
  # Creating adjacency matrix
  A <- rbind(cbind(matrix(0, n_p, n_p), ),
             cbind(t(net), matrix(0, n_a, n_a)))
  
  dz=matrix(NA, nrow = tmax, ncol = n_sp)
  
  z0=runif(n_sp, min=0, max=10)
  theta=runif(n_sp, min=0, max=10)
  
  dz[1,]<-z0
  
  for(t in 1:(tmax-1)){
    
    z <- dz[t, ]
    z_dif <- t(A * z) - A * z
    
    Q <- A * (exp( - alpha * (z_dif^2)))
    Q <- Q / apply(Q, 1, sum)
    Q <- Q * m
    Q_dif <- Q * z_dif
    mut_sel <- apply(Q_dif, 1, sum)
    env_sel <- (1 - m) * (theta - z)
    
    dz[t+1, ] = z + phi *( mut_sel + env_sel)
    
  }
  
  dz_df<-as.data.frame(dz)
  dz_df$time<-1:tmax
  names(dz_df)[1:n_sp]<-paste0("SP", 1:n_sp)
  dz_df<-melt(dz_df, id.vars="time")
  
  return(dz_df)
  
}


