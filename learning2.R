
### Functions

N = 8

#### q ####
x_0 <- runif(N,min=0,max=1)
y_0 <- runif(N,min=0,max=1)

q <- rbind(x_0,y_0)

dim(q) <- c(1,2*N)


#### p #####
x_0_p <- runif(N,min=0,max=1)
y_0_p <- runif(N,min=0,max=1)

p <- rbind(x_0_p,y_0_p)

dim(p) <- c(1,2*N)

q_list = list()
p_list = list()

for(k in 1:N){
  q_list[[k]] = rbind(x_0[k],y_0[k])
  
  p_list[[k]] = rbind(x_0_p[k],y_0_p[k])
  
}




#q_list_func <- function(q_new,t){
#  q_list[[t]]=q_new
#  return(q_list)
#}




#p_list_func <- function(p_new,t){
#  p_list[[t]]=p_new
#  return(p_list)
#}



## grad_U

## grad_Psi

## gamma
num_it = 50
gamma = seq(0,0,length=num_it)
gamma[1] = 0.2






###################################
############# grad U ##############
###################################


grad_U <- function(q,t,j){
  ### q is a list, every list entry is measurement at time t for every agent
  d <-  0.6 #0.4
  d0 <- 0.848 #0.648
  d1 <- 2.4 #1.4
  
  alpha = 1.1
  r = matrix(0,N,N)
  grad_U = matrix(0,1,N)
  
  for( i in 1:N){
    #for(j in 1:N){
    
    r[i,j] = norm(q[[i]][,t]-q[[j]][,t],type='2')^2
    
    if(r[i,j] < d0^2){
      if (i !=j){
        grad_U[1,i]  = (r[i,j] - d^2)*(q[[i]][1,t]-q[[j]][1,t])/(alpha+r[i,j])^2
      }
    }else {
      if (i !=j){
        grad_U[1,i] = rho((sqrt(r[i,j])-d0)/(norm(d1-d0,type='2')))*(norm(d0^2-d^2,type='2')/(alpha+d0^2)^2)*(q[[i]][1,t]-q[[j]][1,t])
      }
    }
  }
  #}
  #print(grad_U)
  return(sum(grad_U))
}


rho <- function(z){
  h = 0.6
  if(z>=0 & z < h){
    rho = 1
  }else if(z>=h & z < 1){
    rho = 0.5*(1+cos(pi*(z-h)/(1-h)))
  }else{
    rho = 0
  }
  return(rho)
}



###################################
############ grad Psi #############
###################################

#p at time t

grad_Psi <- function(p,q,i,t){
  
  #print(N)
  
  temp = 0
  A = matrix(0,N,N) #adjacency matrix
  r = matrix(0,N,N)
  k3 = 0.1
  
  grad_psi = 0
  
  for(j in 1:N){
    
    r[i,j] = norm(q[[i]][,t]-q[[j]][,t],type='2')^2
    ### Adjency matrix
    
    
    if(r[i,j] < d0^2 & i!=j){
      
      A[i,j] = k3*1
      grad_psi = temp + A[i,j]*(p[[i]][,t]-p[[j]][,t])
      temp = grad_psi
    }
  }
  return(grad_psi)
}






###################################
######### phi environmet ##########
###################################


phi <- function(nu){
  kappa = c(3.1,3.3) #center of radial basis function
  sigma = 1.9
  Z = 2
  
  phi = 1/Z*exp(-norm(nu-kappa,type='2')^2/(2*sigma^2))
  #print('phi')
  #print(phi)
  return(matrix(phi))
}

###################################
############ grad phi #############
###################################

grad_phi <- function(nu){
  kappa = c(3.1,3.3) #center of radial basis function
  sigma = 1.3
  Z = 2
  grad_phi = (-norm(nu-kappa,type='2')/Z)*(sigma^2)*exp(-norm(nu-kappa,type='2')^2/(2*sigma^2))
  return(matrix(grad_phi))
}


# Goes from s to n. indexed by k

###################################
############## PHI ################
###################################

## add m for more centers of the radial basis function

Phi <- function(t,nu,num_it){
  
  n = num_it
  s = num_it - t
  Phi = matrix(0,1,t)
  for(i in 1:t){
    
    #print('Nu')
    #print(nu)
    #print(nu[,i])
    Phi[i] = phi(nu[,i])
    
  }
  #print(Phi)
  return(matrix(Phi))
}


############### y ################


y <- function(nu,t){
  
  y = phi(nu[t])*Theta[t]
  return(matrix(y))
  
}


###################################
################ Y ################
###################################


Y <-   function(t,nu,num_it){
  
  n = num_it
  s = num_it-t
  Y = matrix(0,n-s,1)
  for(i in 1:length(Y)){
    
    Y[i] = y(nu,t)
 
  }
  
  return(matrix(Y))
} # Changes for every t    



# Changes for every t

# k here is v_k or index of 

## P = (t(phi(nu[,k]))*phi(nu[,k]))^(-1)  # Changes for every t

### s is number of samples gathered from itself and its neighbors

### n - s is the total number of past measurements

# Y is the collection of cooperatively measured data

###################################
###### Initialize parameters ######
###################################

Theta = matrix(0,1,num_it) # Changes for every t
Theta[1] = 0

m_kappa = 1 # number of centers for radial basis function

lse_list = list()

P = matrix(0,N,num_it)

k_di = 0.1
k4 = 0.1

###################################
########### SIMULATION ############
###################################

for(t in 1:num_it){
  for(i in 1:N){
    
    #print('Start')

    s = num_it-t
    #print('Agent')
    #print(i)
    #print('Iteration nr')
    #print(t)
    
    ####### t + 1 update 

    q_i = q_list[[i]]
    m = 1
    
    P[i,t] = (t(Phi(t,q_i,num_it))%*%Phi(t,q_i,num_it))^(-1)
    
    
    #Theta_hat = P[i,t]*t(Phi(t,q_i,num_it))*Y(t,q_i,num_it)

    K = P[i,t]*t(Phi(t,q_i,num_it))%*%(diag(t)+Phi(t,q_i,num_it)%*%matrix(P[i,t])%*%t(Phi(t,q_i,num_it)))^(-1)
    
    P[i,t+1] = (diag(m) - K%*%Phi(t,q_i,num_it))%*%P[i,t]
    Theta[t+1] = Theta[t] + K%*%(Y(t,q_i,num_it)-matrix(Phi(t,q_i,num_it))%*%matrix(Theta[t]))
    
    #grad_mu[t,q_temp] = phi(q_temp)%*%Theta[t+1]
    new_q = q_list[[i]][,t] + 0.2*p_list[[i]][,t]
    ## the projected algorithm updates only if the updated value belongs to D otherwise it keeps the previous state.
    #print(new_q)
    if(new_q[2]>5 | new_q[2]<(-5) | new_q[1]>5 | new_q[1]<(-5)){
      q_list[[i]] = cbind(q_list[[i]],q_list[[i]][,t])
    }else{
      
    q_list[[i]] = cbind(q_list[[i]],q_list[[i]][,t] + 0.2*p_list[[i]][,t]) 

    }
    #print('update')
    p_list[[i]] = cbind(p_list[[i]],p_list[[i]][,t] + 0.2*(-grad_U(q_list,t,i)-k_di*p_list[[i]][,t]-grad_Psi(q_list,p_list,i,t))+k4*t(grad_phi(q_list[[i]][,t]))*Theta[t+1])
  
  }
}

