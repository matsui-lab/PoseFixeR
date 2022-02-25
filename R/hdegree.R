# hdegree270 = function(dat,a,b,c,time){
#   vec1x = dat$x[time,a]-dat$x[time,b]
#   vec1y = dat$y[time,a]-dat$y[time,b]
#   vec2x = dat$x[time,c]-dat$x[time,b]
#   vec2y = dat$y[time,c]-dat$y[time,b]
#   denom <- sqrt(vec1x^2+vec1y^2)*sqrt(vec2x^2+vec2y^2)
#   numer <- vec1x*vec2x+vec1y*vec2y
#   cos1 <- numer/denom
#   # if(dat$x[time,b]>0){
#     if((vec1y/vec1x)*dat$x[time,c] < dat$y[time,c]){
#       #theta = 180 - acos(cos1)*180/pi 
#       theta = acos(cos1)*180/pi - 180
#     }else{
#       #theta = acos(cos1)*180/pi - 180
#       theta = 180 - acos(cos1)*180/pi
#     }  
#   # }else{
#   #   if((vec1y/vec1x)*dat$x[time,c] < dat$y[time,c]){
#   #     theta = acos(cos1)*180/pi - 180
#   #     #theta = 180 - acos(cos1)*180/pi
#   #   }else{
#   #     theta = 180 - acos(cos1)*180/pi 
#   #     #theta = acos(cos1)*180/pi - 180
#   #   }
#   # }
#   return(theta)
# }
hdegree = function(dat,a,b,c,time){
  vec1x = dat$x[time,a]-dat$x[time,b]
  vec1y = dat$y[time,a]-dat$y[time,b]
  vec2x = dat$x[time,c]-dat$x[time,b]
  vec2y = dat$y[time,c]-dat$y[time,b]
  denom <- sqrt(vec1x^2+vec1y^2)*sqrt(vec2x^2+vec2y^2)
  numer <- vec1x*vec2x+vec1y*vec2y
  cos1 <- numer/denom
  aa = vec1y/vec1x
  bb = (-1)*dat$x[time,a]*aa+dat$y[time,a]
  if(dat$x[time,b]>=0){
    if(dat$y[time,c]>dat$x[time,c]*aa + bb){
    theta = acos(cos1)*180/pi - 180
  }else{
    #theta = acos(cos1)*180/pi - 180
    theta = 180 - acos(cos1)*180/pi
  }
  }else{
    if(aa*dat$x[time,c] + bb < dat$y[time,c]){
      theta = 180 - acos(cos1)*180/pi
      #theta = 180 - acos(cos1)*180/pi
    }else{
      theta = acos(cos1)*180/pi - 180
      #theta = acos(cos1)*180/pi - 180
    }
  }
  return(theta)
}






