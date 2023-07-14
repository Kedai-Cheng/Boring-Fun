library(plotly)
## p is the proportion covered. 0<=p<=1 ##
## angle.func returns the angle of the target ball. ##
angle.func <- function(p){
  180/(pi/asin(((1-p)*2)/2))
}

p <- 0.6
angle.func(p)
###### Ploting ######
theta.vec <- seq(from=0,to=360,by=0.1)
target.x <- sin((theta.vec/180)*pi)
target.y <- cos((theta.vec/180)*pi)

dislocation.x <- 2*(1-p)
dislocation.y <- 2*sqrt(p*(2-p))

cue.x <- sin((theta.vec/180)*pi)-dislocation.x
cue.y <- cos((theta.vec/180)*pi)-dislocation.y

plot1 <- plot_ly()%>%
  add_trace(x=target.x , y=target.y, 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "blue"),
            name = "Target" , showlegend = TRUE) %>%
  add_trace(x=0 , y=0,
            type = "scatter", mode = "markers",
            marker = list(size=12 , color = "blue"),
            name = "Target" , showlegend = FALSE) %>%
  add_trace(x=cue.x , y=cue.y, 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "red"),
            name = "Cue" , showlegend = TRUE) %>%
  add_trace(x=-dislocation.x , y=-dislocation.y,
            type = "scatter", mode = "markers",
            marker = list(size=12 , color = "red"),
            name = "Cue" , showlegend = FALSE) %>%
  add_trace(x=c(-dislocation.x , sin((angle.func(p)/180)*pi)) , 
            y=c(-dislocation.y , cos((angle.func(p)/180)*pi)), 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "green"),
            name = "Trace" , showlegend = TRUE) %>%
  layout(
    xaxis = list(
      range = c(-3.5,3.5)
    ),
    yaxis = list(
      range = c(-3.5,3.5)
    )
  )

plot1  

#######################################
#######################################
## angle is the angle of the target ball. 0<=angle<=90 ##
## prop.func returns the proportion to cover. ##
prop.func <- function(angle){
  1-sin((angle*pi)/180)
}

angle <- 45
prop.func(angle)

###### Ploting ######
theta.vec <- seq(from=0,to=360,by=0.1)
target.x <- sin((theta.vec/180)*pi)
target.y <- cos((theta.vec/180)*pi)

dislocation.x <- 2*(1-prop.func(45))
dislocation.y <- 2*sqrt(prop.func(45)*(2-prop.func(45)))

cue.x <- sin((theta.vec/180)*pi)-dislocation.x
cue.y <- cos((theta.vec/180)*pi)-dislocation.y

plot2 <- plot_ly()%>%
  add_trace(x=target.x , y=target.y, 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "blue"),
            name = "Target" , showlegend = TRUE) %>%
  add_trace(x=0 , y=0,
            type = "scatter", mode = "markers",
            marker = list(size=12 , color = "blue"),
            name = "Target" , showlegend = FALSE) %>%
  add_trace(x=cue.x , y=cue.y, 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "red"),
            name = "Cue" , showlegend = TRUE) %>%
  add_trace(x=-dislocation.x , y=-dislocation.y,
            type = "scatter", mode = "markers",
            marker = list(size=12 , color = "red"),
            name = "Cue" , showlegend = FALSE) %>%
  add_trace(x=c(-dislocation.x , sin((angle/180)*pi)), 
            y=c(-dislocation.y , cos((angle/180)*pi)), 
            type = "scatter", mode = "lines",
            line = list(width = 5 , color = "green"),
            name = "Trace" , showlegend = TRUE) %>%
  layout(
    xaxis = list(
      range = c(-3.5,3.5)
    ),
    yaxis = list(
      range = c(-3.5,3.5)
    )
  )

plot2  





