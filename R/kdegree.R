# kdegree270 = function(dat,a,b,c,time){
#   vec1x = dat$x[time,a]-dat$x[time,b]
#   vec1y = dat$y[time,a]-dat$y[time,b]
#   vec2x = dat$x[time,c]-dat$x[time,b]
#   vec2y = dat$y[time,c]-dat$y[time,b]
#   denom <- sqrt(vec1x^2+vec1y^2)*sqrt(vec2x^2+vec2y^2)
#   numer <- vec1x*vec2x+vec1y*vec2y
#   cos1 <- numer/denom
#   if(dat$x[time,b]>0){
#     if((vec1y/vec1x)*dat$x[time,c] < dat$y[time,c]){
#       #theta = 180 - acos(cos1)*180/pi 
#       theta = acos(cos1)*180/pi - 180
#     }else{
#       #theta = acos(cos1)*180/pi - 180
#       theta = 180 - acos(cos1)*180/pi
#     }  
#   }else{
#     if((vec1y/vec1x)*dat$x[time,c] < dat$y[time,c]){
#       #theta = acos(cos1)*180/pi - 180
#       theta = 180 - acos(cos1)*180/pi
#     }else{
#       theta = 180 - acos(cos1)*180/pi 
#       #theta = acos(cos1)*180/pi - 180
#     }
#   }
#   return(theta)
# }
kdegree = function(dat,a,b,c,time){
  vec1x = dat$x[time,a]-dat$x[time,b]
  vec1y = dat$y[time,a]-dat$y[time,b]
  vec2x = dat$x[time,c]-dat$x[time,b]
  vec2y = dat$y[time,c]-dat$y[time,b]
  denom <- sqrt(vec1x^2+vec1y^2)*sqrt(vec2x^2+vec2y^2)
  numer <- vec1x*vec2x+vec1y*vec2y
  cos1 <- numer/denom
  aa = vec1y/vec1x
  bb = (-1)*dat$x[time,a]*aa+dat$y[time,a]
  # if(-180 < acos(cos1)*180/pi&acos(cos1)*180/pi <= 0){
  #   asdeg = (-1)*acos(cos1)*180/pi
  # }else if(0<acos(cos1)*180/pi & acos(cos1)*180/pi <= 180){
   asdeg = acos(cos1)*180/pi
  # }else if(acos(cos1)*180 <= -180){
  #   asdeg = acos(cos1)*180/pi+180
  # }else{
  #   asdeg = acos(cos1)*180/pi-180
  # }
  if(aa<0){
    if(aa*dat$x[time,c] + bb < dat$y[time,c]){
      theta = 180 - asdeg
    }else{
      #theta = acos(cos1)*180/pi - 180
      theta = asdeg - 180
    }
  }else{
    if(aa*dat$x[time,c] + bb < dat$y[time,c]){
      theta = 180 - asdeg
      #theta = 180 - acos(cos1)*180/pi
    }else{
      theta = 180 - asdeg
      #theta = acos(cos1)*180/pi - 180
    }
  }
  return(theta)
}









