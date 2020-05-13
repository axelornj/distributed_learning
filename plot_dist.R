#### Plot and animate data

library(ggplot2)
library(plotly)



plot1 <- ggplot(data_frame,aes(x = , y =  )) 
+ geom_point(aes(frame = timesss))
+ labs(title = "Distributed search")

ggplotly(plot1)
  



######## Plot potential

d <-  0.4
d0 <- 0.648
d1 <- 1.4
r <- 1.6
N = 2

alpha = 0.1

r_agents <- seq(0.2,r,length=100)

potential = seq(0,0,length(r_agents))


for(kk in 1:length(r_agents)){

if(r_agents[kk] < d0^2){
  
  potential[kk] = 0.5*(log(alpha+r_agents[kk])+(alpha+d^2)/(alpha+r_agents[kk]))
  
}else{
  potential[kk] = 0.5*(log(alpha+d0^2)+(alpha+d^2)/(alpha+d0^2))
}
  
}

pot_df = data.frame(r_agents,potential)

pot <- ggplot(pot_df,aes(r_agents,potential))+ geom_line()+xlim(0.2,1.6) + ylim(-1,2)



potential = seq(0,0,length(r_agents))

for(kk in 1:length(r_agents)){


  if(r_agents[kk] < d0^2){
    #if (i !=j){
      
      temp  = (r_agents[kk]- d^2)*(r_agents[kk])/(alpha+r_agents[kk])^2
      potential[kk] = temp + potential[kk]  
      #print(grad_U[1,i])     
      print('Smaller')
    #}
  }else {
    #if (i !=j){
      
      temp2 = rho((sqrt(r_agents[kk])-d0)/(norm(d1-d0,type='2')))*(norm(d0^2-d^2,type='2')/(alpha+d0^2)^2)*(r_agents[kk])
      potential[kk] = temp2 + potential[kk]
      #print(grad_U[1,i])
    #}
      print('Else')
  }

}













