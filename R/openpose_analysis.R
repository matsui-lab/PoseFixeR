# install.packages("imputeTS")
# install.packages("pracma")

openpose_analysis = function(dat,frame){
  for(i in 1){
    ##下肢誤検知
    L = na_interpolation(dat$x[,14], option = "linear")
    Lx = smooth.spline(x=seq_along(L),L,spar=smoothNum,cv=TRUE)$y#左足x座標
    R = na_interpolation(dat$x[,11], option = "linear")
    Rx = smooth.spline(x=seq_along(R),R,spar=smoothNum,cv=TRUE)$y#右足x座標
    fra_x = rep(0,frame)
    for(l in 1:(frame-2)){
      if(l==1|l==2){
        diffL1 = NA
      }else{
        diffL1 = diff(Lx[(l-2):(l-1)])
        diffL2 = diff(Lx[(l-1):l])
        diffL3 = diff(c(Lx[l-2],Lx[l]))
        diffL4 = diff(Lx[l:(l+1)])
        diffL5 = diff(Lx[(l+1):(l+2)])
        diffL6 = diff(c(Lx[l],Lx[l+2]))
        ste = sum(Lx-Rx > 0)/frame#サンプルの中に誤検知あるか
        if((ste > 0.7 | ste < 0.3) && -0.4 < Lx[l] && Lx[l] < 0.4 && 0 <= diffL1 &&
           0 <= diffL2 && 0 <= diffL3 && diffL4 < 0 && diffL5 < 0 && diffL6 < 0 |
           (ste > 0.7 | ste < 0.3) &&-0.4 < Lx[l] && Lx[l] < 0.4 && diffL1 < 0 &&
           diffL2 < 0 && diffL3 < 0 && 0 <= diffL4 && 0 <= diffL5 && 0 <= diffL6 ){
          fra_x[l] = TRUE
        }}}
    con = which(fra_x==1)
    if(length(con)==0){
    }else{
      for(l in 1:length(con)){
        if(!all(l/2 %% 1 == 0)){
          dat$x[con[l]:frame,14] = sbjList$x[con[l]:frame,11]#9:RH,10:RK,11:RA
          dat$y[con[l]:frame,14] = sbjList$y[con[l]:frame,11]#12:LH,13:LK,14:LA
          dat$x[con[l]:frame,11] = sbjList$x[con[l]:frame,14]
          dat$y[con[l]:frame,11] = sbjList$y[con[l]:frame,14]
          dat$x[con[l]:frame,13] = sbjList$x[con[l]:frame,10]
          dat$y[con[l]:frame,13] = sbjList$y[con[l]:frame,10]
          dat$x[con[l]:frame,10] = sbjList$x[con[l]:frame,13]
          dat$y[con[l]:frame,10] = sbjList$y[con[l]:frame,13]
          dat$x[con[l]:frame,12] = sbjList$x[con[l]:frame,9]
          dat$y[con[l]:frame,12] = sbjList$y[con[l]:frame,9]
          dat$x[con[l]:frame,9]  = sbjList$x[con[l]:frame,12]
          dat$y[con[l]:frame,9]  = sbjList$y[con[l]:frame,12]
        }else{
          dat$x[con[l]:frame,14] = sbjList$x[con[l]:frame,14]#9:RH,10:RK,11:RA
          dat$y[con[l]:frame,14] = sbjList$y[con[l]:frame,14]#12:LH,13:LK,14:LA
          dat$x[con[l]:frame,11] = sbjList$x[con[l]:frame,11]
          dat$y[con[l]:frame,11] = sbjList$y[con[l]:frame,11]
          dat$x[con[l]:frame,13] = sbjList$x[con[l]:frame,13]
          dat$y[con[l]:frame,13] = sbjList$y[con[l]:frame,13]
          dat$x[con[l]:frame,10] = sbjList$x[con[l]:frame,10]
          dat$y[con[l]:frame,10] = sbjList$y[con[l]:frame,10]
          dat$x[con[l]:frame,12] = sbjList$x[con[l]:frame,12]
          dat$y[con[l]:frame,12] = sbjList$y[con[l]:frame,12]
          dat$x[con[l]:frame,9] = sbjList$x[con[l]:frame,9]
          dat$y[con[l]:frame,9] = sbjList$y[con[l]:frame,9]
        }
      }
    }
    sb1 = dat
    ##overポイント
    #len_range
    overpoint = sort(unique(c(which(dat$x< (-Xover)) ,  which(Xover < dat$x) ,
                              which(dat$y < (-y_upper*2)) , which(y_under*rangeU < dat$y))))
    dat$x[overpoint] = dat$y[overpoint] = NA
    if(!(sum((frame*9<overpoint)&(overpoint<=14*frame))==0)){
    }
    ##時系列
    ##1/25sの間での変化
    r = sqrt(diff(dat$x)^2+diff(dat$y)^2)
    skip_num = which(r>speed)
    befs = afts = matrix(0,frame,18)
    ##2/25sの間での変化
    for(n in 1:frame){
      if(n==1|n==2){
      }else{
        befs[n,] = sqrt((sb1$x[n,]-sb1$x[n-2,])^2 +
                          (sb1$y[n,]-sb1$y[n-2,])^2)
      }
      if(n==(frame-1)|n==frame){
      }else{
        afts[n,] = sqrt((sb1$x[n,]-sb1$x[n+2,])^2 +
                          (sb1$y[n,]-sb1$y[n+2,])^2)
      }
    }
    befN = which(befs>speed*2)
    aftN = which(afts>speed*2)
    for(m in 1:18){
      if(any((frame-1)*(m-1) < skip_num & skip_num <= (frame-1)*m)){
        skipm = skip_num[which((frame-1)*(m-1) < skip_num & skip_num <= (frame-1)*m)]
        dat$x[skipm+m] = dat$y[skipm+m] = NA
      }
      if(any(frame*(m-1) < befN & befN <= frame*m)){
        skipbef = befN[which(frame*(m-1) < befN & befN <= frame*m)]
        dat$x[befN-2] = dat$y[befN-2] = NA
      }
      if(any(frame*(m-1) < aftN & aftN <= frame*m)){
        skipaft = aftN[which(frame*(m-1) < aftN & aftN <= frame*m)]
        dat$x[aftN+2] = dat$y[aftN+2] = NA
      }}
    #下腿距離、可動域
    sho_dis = Hip_dis = rep(NA,frame)
    for(j in 1:frame){
      if(is.na(dat$x[j,9]) | is.na(dat$x[j,10]) | is.na(dat$x[j,11])){
      }else if(dat$x[j,9] == dat$x[j,10] | dat$x[j,10] == dat$x[j,11] | dat$x[j,9] == dat$x[j,11]){
      }else{
        if(kdegree(dat,9,10,11,j) < degRk2.5|degRk97.5<kdegree(dat,9,10,11,j)){#右膝関節屈曲角×→右足関節位置座標×
          dat$x[j,"Rankle"] =　dat$y[j,"Rankle"] = NA
        }else if(distance(dat,10,11,j) > lower*len_range){#右下腿距離×→右足関節位置座標×
          dat$x[j,"Rankle"] =　dat$y[j,"Rankle"] = NA
        }}
      if(is.na(dat$x[j,2]) | is.na(dat$x[j,9]) | is.na(dat$x[j,10])){
      }else if(dat$x[j,2] == dat$x[j,9] | dat$x[j,9] == dat$x[j,10] | dat$x[j,10] == dat$x[j,2]){
      }else{
        if(hdegree(dat,2,9,10,j) > degR97.5 | hdegree(dat,2,9,10,j) < degR2.5){#右股関節屈曲角×→右膝関節位置座標×
          dat$x[j,"Rknee"] =　dat$y[j,"Rknee"] = NA
          dat$x[j,"Rankle"] =　dat$y[j,"Rankle"] = NA
        }else if(distance(dat,9,10,j) > thigh*len_range){#右大腿距離×→右膝関節位置座標×
          dat$x[j,"Rknee"] =　dat$y[j,"Rknee"] = NA
        }}
      if(is.na(dat$x[j,12]) | is.na(dat$x[j,13]) | is.na(dat$x[j,14])){
      }else if(dat$x[j,12] == dat$x[j,13] | dat$x[j,13] == dat$x[j,14] | dat$x[j,12] == dat$x[j,14]){
      }else{
        if(kdegree(dat,12,13,14,j) < degLk2.5|degLk97.5<kdegree(dat,12,13,14,j)){#左膝関節屈曲角×→左足関節位置座標×
          dat$x[j,"Lankle"] =　dat$y[j,"Lankle"] = NA
        }else if(distance(dat,13,14,j) > lower*len_range){#左下腿距離×→左足関節位置座標×
          dat$x[j,"Lankle"] =　dat$y[j,"Lankle"] = NA
        }}
      if(is.na(dat$x[j,2]) | is.na(dat$x[j,12]) | is.na(dat$x[j,13])){
      }else if(dat$x[j,2] == dat$x[j,12] | dat$x[j,12] == dat$x[j,13] | dat$x[j,2] == dat$x[j,13]){
      }else{
        if(hdegree(dat,2,12,13,j) > degL97.5 | hdegree(dat,2,12,13,j) < degL2.5){#左股関節屈曲角×→膝関節位置座標×
          dat$x[j,"Lknee"] =　dat$y[j,"Lknee"] = NA
          dat$x[j,"Lankle"] =　dat$y[j,"Lankle"] = NA
        }else if(distance(dat,12,13,j) > thigh*len_range){#左大腿距離×→左膝関節位置座標×
          dat$x[j,"Lknee"] =　dat$y[j,"Lknee"] = NA
        }}
      sho_dis[j] = distance(dat,"Rshoulder","Lshoulder",j)
      if(is.na(dat$x[j,"Rshoulder"]) | is.na(dat$x[j,"Lshoulder"])){#肩幅広いものは×
      }else if(distance(dat,"Rshoulder","Lshoulder",j) < Sh_lim3){
      }else if(distance(dat,"Rshoulder","Lshoulder",j) < Sh_lim4|
               Sh_lim1 <= distance(dat,"Rshoulder","Lshoulder",j)){
        dat$x[j,"Rshoulder"] =　dat$y[j,"Rshoulder"] = NA
        dat$x[j,"Lshoulder"] =　dat$y[j,"Lshoulder"] = NA
      }else{
        dat$x[j,] =　dat$y[j,] = NA
      }
      Hip_dis[j] = distance(dat,"Rhip","Lhip",j)
    }#1:len[i]のforループはここまで
    shold = sho_dis
    ##信頼度
    if(any(dat$p < p_rate)){#信頼度低いもの
      select_idx = which(dat$p < p_rate)
      dat$x[select_idx] = dat$y[select_idx] = NA
      if(!(sum((frame*8<select_idx)&(select_idx<=14*frame))==0)){
      }
    }
    ##足関節距離広過ぎるもの
    if(max(distance(sbjList,11,14,1:sum_file))<Fdist2.5){
      dat$x[,"Lankle"] =　dat$y[,"Lankle"] = NA
      dat$x[,"Rankle"] =　dat$y[,"Rankle"] = NA
    }
    ##
    ankle_mean = abs(sbjList$x[,"Rankle"]+sbjList$x[,"Lankle"])
    knee_mean = abs(sbjList$x[,"Rknee"]+sbjList$x[,"Lknee"])
    hip_mean = abs(sbjList$x[,"Rhip"]+sbjList$x[,"Lhip"])
    ##足関節平均が中心線近くにないもの
    if(any(ankle_mean >= abF,na.rm=T)){
      dat$x[which(ankle_mean>= abF),"Lankle"] =　dat$y[which(ankle_mean>= abF),"Lankle"] = NA
      dat$x[which(ankle_mean>= abF),"Rankle"] =　dat$y[which(ankle_mean>= abF),"Rankle"] = NA
    }
    ##膝関節平均が中心線近くにないもの
    if(any(knee_mean >= abK)){#膝関節平均が中心線近くにないもの
      dat$x[which(knee_mean >= abK),"Lknee"] =　dat$y[which(knee_mean >= abK),"Lknee"] = NA
      dat$x[which(knee_mean >= abK),"Rknee"] =　dat$y[which(knee_mean >= abK),"Rknee"] = NA
    }
    ##股関節平均が中心線近くにないもの
    if(any(hip_mean >= abH)){
      dat$x[which(hip_mean >= abH),"Lhip"] =　dat$y[which(hip_mean >= abH),"Lhip"] = NA
      dat$x[which(hip_mean >= abH),"Rhip"] =　dat$y[which(hip_mean >= abH),"Rhip"] = NA
    }
    ##足部の地面接地
    if(any(sbjList$y[,"Rankle"] > y_under*rangeU & sbjList$y[,"Lankle"] > y_under*rangeU|
           sbjList$y[,"Rankle"] < y_under*rangeD & sbjList$y[,"Lankle"] < y_under*rangeD,na.rm=T)){
      ankle_num = which(sbjList$y[,"Rankle"] > y_under*rangeU & sbjList$y[,"Lankle"] > y_under*rangeU|
                          sbjList$y[,"Rankle"] < y_under*rangeD & sbjList$y[,"Lankle"] < y_under*rangeD)
      dat$x[ankle_num,"Lankle"] =　dat$y[ankle_num,"Lankle"] = NA
      dat$x[ankle_num,"Rankle"] =　dat$y[ankle_num,"Rankle"] = NA
    }
    #片足NAはもう一方もNA
    if(any(is.na(dat))){
      dat = NA
    }else{
      Num_An = c(which(is.na(dat$x[,"Rankle"])),which(is.na(dat$x[,"Lankle"])))
      an_num = sort(Num_An[!duplicated(Num_An)])
      dat$x[an_num,"Lankle"] =　dat$y[an_num,"Lankle"] = NA
      dat$x[an_num,"Rankle"] =　dat$y[an_num,"Rankle"] = NA
      Num_Kn = c(which(is.na(dat$x[,"Rknee"])),which(is.na(dat$x[,"Lknee"])))
      kn_num = sort(Num_Kn[!duplicated(Num_Kn)])
      dat$x[kn_num,10:11] =　dat$y[kn_num,10:11] = NA
      dat$x[kn_num,13:14] =　dat$y[kn_num,13:14] = NA
      Num_Hi = c(which(is.na(dat$x[,"Rhip"])),which(is.na(dat$x[,"Lhip"])))
      hi_num = sort(Num_Hi[!duplicated(Num_Hi)])
      dat$x[hi_num,9:14] =　dat$y[hi_num,9:14] = NA
    }
    ##補正
    ##足の長さ調節
    if(any(is.na(dat)) == TRUE){
      dat = NA
    }else{
      num1 = which(!hazureti(na.omit(distance(dat,12,13,1:frame)))$caselist)
      mLleg = mean(na.omit(distance(dat,12,13,num1)))
      num2 = which(!hazureti(na.omit(distance(dat,9,10,1:frame)))$caselist)
      mRleg = mean(na.omit(distance(dat,9,10,num2)))
      Lleg = distance(dat,12,13,1:frame)
      Rleg = distance(dat,9,10,1:frame)
      dat$x[,10] = dat$x[,9]+(dat$x[,10]-dat$x[,9])*mRleg/Rleg#Rknee
      dat$y[,10] = dat$y[,9]+(dat$y[,10]-dat$y[,9])*mRleg/Rleg
      dat$x[,13] = dat$x[,12]+(dat$x[,13]-dat$x[,12])*mLleg/Lleg#Lknee
      dat$y[,13] = dat$y[,12]+(dat$y[,13]-dat$y[,12])*mLleg/Lleg
      ##
      num3 = which(!hazureti(na.omit(distance(dat,13,14,1:frame)))$caselist)
      mLLow = mean(na.omit(distance(dat,13,14,num3)))
      num4 = which(!hazureti(na.omit(distance(dat,10,11,1:frame)))$caselist)
      mRLow = mean(na.omit(distance(dat,13,14,num4)))
      LLow = distance(dat,13,14,1:frame)
      RLow = distance(dat,10,11,1:frame)
      dat$x[,11] = dat$x[,10]+(dat$x[,11]-dat$x[,10])*mRLow/RLow#Rankle
      dat$y[,11] = dat$y[,10]+(dat$y[,11]-dat$y[,10])*mRLow/RLow
      dat$x[,14] = dat$x[,13]+(dat$x[,14]-dat$x[,13])*mLLow/LLow#Lankle
      dat$y[,14] = dat$y[,13]+(dat$y[,14]-dat$y[,13])*mLLow/LLow
      Num1 = which(is.na(dat$x[,9]))
      Num2 = which(is.na(dat$x[,12]))
      Num3 = which(is.na(dat$x[,11]))
      Num4 = which(is.na(dat$x[,14]))
      Num5 = which(is.na(dat$x[,10]))
      Num6 = which(is.na(dat$x[,13]))
      if(any(is.na(dat$x[,9])) | any(is.na(dat$x[,12]))){
        dat$x[Num1,12] =　dat$y[Num1,12] = NA
        dat$x[Num2,9] =　dat$y[Num2,9] = NA
      }
      if(any(is.na(dat$x[,11])) | any(is.na(dat$x[,14]))){
        dat$x[c(Num3,Num5,Num6),14] =　dat$y[c(Num3,Num5,Num6),14] = NA
        dat$x[c(Num4,Num5,Num6),11] =　dat$y[c(Num4,Num5,Num6),11] = NA
      }
      if(any(is.na(dat$x[,10])) | any(is.na(dat$x[,13]))){
        dat$x[c(Num3,Num4,Num5),13] =　dat$y[c(Num3,Num4,Num5),13] = NA
        dat$x[c(Num3,Num4,Num6),10] =　dat$y[c(Num3,Num4,Num6),10] = NA
      }
    }
    ##スムージング
    sbjList_na = dat
    xseq2 = yseq2 = pseq2 = matrix(NA,nrow=frame,ncol = 18)
    Na = FALSE
    for(l in 1:18){
      if(any(is.na(dat)) == TRUE){
        mat = matrix(NA,frame,18)
        dat = NA
      }else if(sum(!is.na(dat$x[,l]))/frame > NArateNum){
        xxx = na_interpolation(dat$x[,l], option = "linear")
        xseq2[,l] = smooth.spline(x=seq_along(xxx),xxx,spar=smoothspar,cv=TRUE)$y#tskernel
        yyy = na_interpolation(dat$y[,l], option = "linear")
        yseq2[,l] = smooth.spline(x=seq_along(yyy),yyy,spar=smoothspar,cv=TRUE)$y
      }else{
        xseq2[,l] = rep(NA,frame)
        yseq2[,l] = rep(NA,frame)
      }
      na7 = is.na(dat$x[,l])
      gap3 = rle(diff(which(na7)))
      Num_gap = 0
      if(any(gap3$values==1)&(9 <= l & l <= 14)){
        Num_gap = max(gap3$lengths[gap3$values==1])
        if(Num_gap>7){
          Na = c(Na,TRUE)
        }
      }
    }
    if(any(is.na(dat)) == TRUE){
      mat = matrix(NA,frame)
      dat =  NA
    }else if(sum(xseq2[,11] - xseq2[,14] > 0)/length(xseq2[,11]) > 1-LRlegrate |
             sum(xseq2[,11] - xseq2[,14] > 0)/length(xseq2[,11]) < LRlegrate |
             any(is.na(xseq2[,2])) | any(is.na(xseq2[,9:14])) | any(Na)){
      mat = matrix(NA,frame,18)
      dat = NA
      xseq2 = matrix(NA,frame,18)
      yseq2 = matrix(NA,frame,18)
    }
    sm_sbj_90 = list(x = xseq2,y=yseq2,p=sbjList$p)
    colnames(sm_sbj_90$x) = colnames(sm_sbj_90$y) =colnames(sm_sbj_90$p) = c("nose","neck","Rshoulder","Relbow","Rwrist","Lshoulder",
                                                                             "Lelbow","Lwrist","Rhip","Rknee","Rankle","Lhip",                        
                                                                             "Lknee","Lankle","Reye","Leye","Rear","Lear")
    
  }
  return(sm_sbj_90)
}
