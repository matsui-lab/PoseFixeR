distance = function(dat,a,b,time){
  x = dat$x[time,a] - dat$x[time,b]
  y = dat$y[time,a] - dat$y[time,b]
  dist = sqrt(x^2 + y^2) 
  return(dist)
}
