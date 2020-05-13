library(plotly)

num_it = 35
N = 8


x = seq(0,0,length=num_it)
y = seq(0,0,length=num_it)
agent = seq(0,0,length=num_it)
time_step = seq(0,0,length=num_it)
env_phi = seq(0,0,length=num_it)

x_temp = seq(0,0,length=num_it)
y_temp = seq(0,0,length=num_it)
agent_temp = seq(0,0,length=num_it)
time_step_temp = seq(0,0,length=num_it)
env_phi_temp = seq(0,0,length=num_it)

for(i in 1:N){
for(t in 1:num_it){

if(i!=1){
  
x_temp[t] = q_list[[i]][1,t]
y_temp[t] = q_list[[i]][2,t]
agent_temp[t] = i
time_step_temp[t] = t
env_phi_temp[t] = phi(cbind(x_temp[t],y_temp[t]))

}else{
  
x[t] = q_list[[i]][1,t]
y[t] = q_list[[i]][2,t]
agent[t] = i
time_step[t] = t
env_phi[t] = phi(cbind(x[t],y[t]))

  }
}
if(i!=1){
  
x = append(x,x_temp)
y = append(y,y_temp)
agent = append(agent,agent_temp)
time_step = append(time_step,time_step_temp)
env_phi = append(env_phi,env_phi_temp)
  }
}


df <- data.frame(
  x , 
  y, 
  agent,
  time_step,
  env_phi
)

fig2 <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    text = ~agent,
    #split = ~agent,
    frame = ~time_step,
    type = 'scatter',
    mode = 'markers', #'lines',#
    #line = list(simplyfy = F),
    showlegend = F
  )

env_y = seq(-5,5,length=100)
env_x = seq(-5,5,length=100)
env = matrix(0,100,100)
for(i in 1:100){
  for(j in 1:100){
    
    
    env[i,j] = phi(cbind(env_x[i],env_y[j]))
    
    
  }
}

#fig <- plot_ly(x=env_x,y=env_y,z=env,type='contour')

fig <- plot_ly(df,
               x = ~x,
               y = ~y,
               z = ~env_phi,
               #text = ~agent,
               split = ~agent,
               #frame = ~time_step,
               #type = 'scatter',
               #mode = 'markers', #'lines',#
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'),
               #line = list(simplyfy = F),
               showscale = T))
fig <- fig %>% add_markers()
#fig2 <- fig %>% add_trace(
#  x=env_x,y=env_y,z=env, type='contour')
#fig2 <- fig2%>% add_surface()
#fig2 <- fig2 %>% add_trace(x = ~x, y = ~y, split = ~agent, name = 'Trace', mode = 'lines')
fig <- fig %>%layout(scene = list(xaxis = list(title = ''),
                               yaxis = list(title = ''),
                               zaxis = list(title = '')),
                  annotations = list(
                    x = 1.13,
                    y = 1.05,
                    text = 'Height of potential',
                    xref = 'paper',
                    yref = 'paper',
                    showarrow = FALSE
                  ))

#fig <- fig %>% add_trace(
#  x=env_x,y=env_y,z=env, type='contour')
#z = ~env)
#fig <- fig %>% add_surface()
#fig <- fig%>% add_surface()

fig2 <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 1.5, end = 2, color = 'white'),
    z = list(show = TRUE, start = 0.5, end = 0.8)),
  x = ~x,
  y = ~y,
  z = ~env)