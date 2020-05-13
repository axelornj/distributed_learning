## Loss function

phi <- function(nu){
  kappa = c(3.1,1.3) #center of radial basis function
  sigma = 0.9
  Z = 2
  
  phi = 1/Z*exp(-norm(nu-kappa,type='2')/(2*sigma^2))
  #print('phi')
  #print(phi)
  return(matrix(phi))
}


y <- function(nu,t){
  print(phi(nu[t]))
  print(Theta[t])
  
  y = phi(nu[t])*Theta[t]
  
  return(matrix(y))
  
}

#lse = seq(0,0,length=num_it)

#for(k in 1:num_it){
  
#  lse[k] = norm(y(q_list[[1]][,k],k)-phi(q_list[[1]][,k])*Theta[k])
  
  
#}

env_y = seq(-5,5,length=100)
env_x = seq(-5,5,length=100)
env = matrix(0,100,100)
for(i in 1:100){
  for(j in 1:100){
    
    
    env[i,j] = phi(cbind(env_x[i],env_y[j]))
    
    
  }
}




