options(rgl.useNULL=TRUE)
options(shiny.maxRequestSize=30*1024^2)
#######################################################################
#
#last updated on 11/16/2015 @ 22:48
#
#Created by: Clayton Dorrity
#
#Contact: 
#
#email:clayton.dorrity@gmail.com
#
#phone: (806) 729-9519
#
#######################################################################
#library(plotly)

raw.data.file.11 <- data.frame(read.table("csvfiles/2011_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
raw.data.file.12 <- data.frame(read.table("csvfiles/2012_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
raw.data.file.14 <- data.frame(read.table("csvfiles/2014_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
raw.data.file.15 <- data.frame(read.table("csvfiles/2015_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
time.plotly <- data.frame(read.table("csvfiles/time.csv", sep=",",header = TRUE, as.is = TRUE))

enviro.var <- c('AT','Dew_Point','RH_Percent','VPD', 'ppt_mm', 'ppt_mm_sum', 
                'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
ct.var <- c("p1","p2","p3","p4","p5","p6","p7") 
treat.var <- c('.00','.15','.30','.60')
yr.list <- c('2003','2004','2005','2006','2007','2008','2009',
             '2010','2011','2012','2013','2014','2015')
loc.list <- c('lbk', 'CSIRO', 'FL')
tod.min <- rep(seq(0,45,by=15),24)
tod.hr <- rep(seq(0,23),each=4)
tod <-paste(tod.hr,tod.min,sep=":")
tod[1:40] <- paste("0",tod[1:40],sep="")
tod[seq(1,96,by=4)] <- paste(tod[seq(1,96,by=4)],"0",sep="")
tod.seq <- rep(tod,365)
tod.seq.hr <- tod.seq[seq(1,35040,by=4)]
midnight <- grep("00:00",tod.seq.hr)
###############################################################################
#Side by Side View
side.rgl.3d <- function(num.raw.col,x,y,z,ext.x,ext.y,ext.z,x.offset,x.offset.2,y.offset,y.offset.2,x.vector,treatment.array,
                        treat.range,session,data.type.matchs,r.col.names,mask.check, yr.choice,yr.ui,label.marker,
                        width.offset,side.heigth.offset,col.ui,mask.l.num,mask.check2,mask.h.num, mask.colorlut.b,mask.colorlut.a,
                        col.ui.ct.2,col.ui.ct.3,yr.ui.ct,file.type,col.ui.ct,col.ui.cust) 
{
  progress <- shiny::Progress$new(session, min=1,max=length(r.col.names))
  on.exit(progress$close())
  y.2 <- y
  x.2 <- x
  x.range <- range(x.2)
  y.range <- range(y.2)
  label.offset.treat <- 1
  yr.temp.ct <- length(yr.ui.ct)
  yr.temp <- length(yr.ui)
  if(length(yr.ui.ct) > 1)col.ui.ct <- rep(col.ui.ct,length(yr.ui.ct))
  ##############################################
  for(d in 1:length(r.col.names))
  {
    #progress$set(message = paste("Building Time Surface #",d," out of ",length(r.col.names),sep=""))
    withProgress(message = paste("Building Time Surface #",d," out of ",length(r.col.names),sep=""),value = 0,{
    if(file.type == 'Enviromental')colorlut <- create.temp.colorlut(r.col.names[d],1)#ext.z)
    if(file.type == 'Canopy Temperatures')colorlut <- create.temp.colorlut(col.ui.ct[d],1)#ext.z)
    if(file.type == 'Custom Input File')colorlut <- create.temp.colorlut.ceta(col.ui.cust[d],1)#ext.z)
    ##############################################
    #NEED TO USE A SWITCH
    if(as.logical(mask.check2) == TRUE ){ 
      if(mask.l.num > as.numeric(0))
      {
        #Mask all temperatures under 30 Celsius
        mask.l.temp <- (as.numeric(mask.l.num) + 1)   
        colorlut[1:mask.l.temp] = mask.colorlut.b#colorlut[1]#"#FFFFFFFF"#"grey" 
      }
      
      if(mask.h.num > as.numeric(0))
      {
        #Mask all temperatures under 30 Celsius
        mask.h.temp <- (as.numeric(mask.h.num) + 1)  
        colorlut[mask.h.temp:max(68)] = mask.colorlut.a#"#FFFFFFFF" #"grey" #"FFFFFFFF" 
      }
    }
    #####################################
    #progress$set(value = d)
    incProgress(1/length(r.col.names))
    Sys.sleep(0.25)
    ##############################################
    z <- ext.z * treatment.array[,,d]
    z.2 <- as.matrix(z)
    ##############################################
    if(as.logical(mask.check) == TRUE){
      for(t in 1:length(z))if(z[t] == 9999) z[t] <- as.numeric(length(colorlut)-1)
      colorlut[1] <- mask.colorlut.b
      colorlut[length(colorlut)] <- mask.colorlut.a
      
    }
    #####################################
    col <- colorlut[z - as.numeric(0) + 1]
    col.2 <- col
    ##############################################
    rgl.surface(x, y, z , color=col, alpha=1, front="fill", back="fill")
    #############################################
#     if(x.offset.2 == 0){
#       ##############################################
#       rgl.surface(x, y, z , color=col, alpha=1, front="fill", back="fill")
#       #############################################
#       show("x.offset.2 = 0")
#       y.offset.2 <- y.offset.2 + 1
#       #x.offset.2 <- x.offset.2 + 1
#       if(label.offset.treat == 1){
#         if(label.marker == 1){
#           text3d(color = "black",
#                  x =  (max(x.2) + (max(x.2)) + side.heigth.offset),
#                  y = 0,
#                  z = (max(y)/2) + (width.offset * (d-1)),# + (ext.y * treat.range),
#                  text = col.ui.ct.3[d])
#         }
#       }
#       y <- y + (ext.y * treat.range) + width.offset
#       ######################
#       if(y.offset == y.offset.2) 
#       {
#         if(label.marker == 1){
#           text3d(color = "black",
#                  x = (min(x) - min(x.2) / 2),
#                  y = 0,
#                  z = width.offset * -2,
#                  text = col.ui.ct.2[label.offset.treat])
#         }
#         x.offset <- x.offset + (ext.x * side.heigth.offset)
#         y.offset.2 <- as.numeric(0)
#         y <- y.2
#       }
#       ######################
#       if(y.offset.2 == 0){
#         x <- x.2 - x.offset
#         if(yr.choice == 1 && label.offset.treat == yr.temp )label.offset.treat <- 1
#         if(yr.choice == 2 && label.offset.treat == yr.temp.ct )label.offset.treat <- 1
#         if(label.offset.treat <= length(col.ui.ct.2)){
#           label.offset.treat <- label.offset.treat + 1
#         }else {label.offset.treat <- 1}
#       }
#       
#     }
#     if(x.offset.2 > 0){
#       x <- x.2 - x.offset
#       ##############################################
#       rgl.surface(x, y, z , color=col, alpha=1, front="fill", back="fill")
#       #############################################
#       if(label.offset.treat == 1){
#         if(label.marker == 1){
#           text3d(color = "black",
#                  x =  (max(x.2) + (max(x.2)) + side.heigth.offset),
#                  y = 0,
#                  z = (max(y)/2) + (width.offset * (d-1)),# + (ext.y * treat.range),
#                  text = col.ui.ct.3[d])
#         }
#       }
#       y.offset.2 <- y.offset.2 + 1
#       #x <- x + x.2 - x.offset
#       x.offset <- x.offset + (ext.x * side.heigth.offset)
#       ######################
#       if(x.offset.2 == y.offset.2) 
#       {
#         if(label.marker == 1){
#           text3d(color = "black",
#                  x = (min(x) - min(x.2) / 2),
#                  y = 0,
#                  z = width.offset * -2,
#                  text = col.ui.ct.2[label.offset.treat])
#         }
#          y.offset.2 <- as.numeric(0)
#          x.offset <- as.numeric(0)
#          y <- y + (ext.y * treat.range) + width.offset
#       }
#       ######################
#       if(y.offset.2 == 0){
#         if(yr.choice == 1 && label.offset.treat == yr.temp )label.offset.treat <- 1
#         if(yr.choice == 2 && label.offset.treat == yr.temp.ct )label.offset.treat <- 1
#         if(label.offset.treat <= length(col.ui.ct.2)){
#           label.offset.treat <- label.offset.treat + 1
#         }else {label.offset.treat <- 1}
#       }
#       #y.offset.2 <- y.offset.2 + 1
#     }
    if(yr.choice == 2){
      y.offset.2 <- y.offset.2 + 1
      x.offset.2 <- x.offset.2 + 1
      if(label.offset.treat == 1){
        if(label.marker == 1){
          text3d(color = "black",
                 x =  (max(x.2) + (max(x.2)) + side.heigth.offset),
                 y = 0,
                 z = (max(y)/2) + (width.offset * (d-1)),# + (ext.y * treat.range),
                 text = col.ui.ct.3[d])
        }
      }
      y <- y + (ext.y * treat.range) + width.offset
      ######################
      if(y.offset == y.offset.2) 
      {
        if(label.marker == 1){
          text3d(color = "black",
                 x = (min(x) - min(x.2) / 2),
                 y = 0,
                 z = width.offset * -2,
                 text = col.ui.ct.2[label.offset.treat])
        }
        x.offset <- x.offset + (ext.x * side.heigth.offset)
        y.offset.2 <- as.numeric(0)
        y <- y.2
      }
      ######################
      if(y.offset.2 == 0){
        x <- x.2 - x.offset
        if(yr.choice == 1 && label.offset.treat == yr.temp )label.offset.treat <- 1
        if(yr.choice == 2 && label.offset.treat == yr.temp.ct )label.offset.treat <- 1
        if(label.offset.treat <= length(col.ui.ct.2)){
          label.offset.treat <- label.offset.treat + 1
        }else {label.offset.treat <- 1}
      }
      
    }#End of for loop of rgl scene
    })
  }
  #############################################
  #'2' = 
  return(Sys.time())
  #y.offset.2 <- as.numeric(0)
}

################################################################################################################################################################################################################################
create.temp.colorlut <- function(r.col.name, ext.z)
{
  colorlut <- data.frame(v=1,ch=c("colorlut"))
  
  #####################################################################################
  if(r.col.name == "Solar_WM2"){  
    col.solar <- read.csv(file = "TempSymbology/Solar_WM2.csv", header = TRUE)
    #zlim.s <- (c(0,14)) * ext.z
    colorlut.solar <- seq(1,length(col.solar[,1]))
    for (i in 1:length(colorlut.solar)){
      colorlut.solar[i] <- rgb(col.solar[i,2],col.solar[i,3],col.solar[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.s
    colorlut <- colorlut.solar
    
  }else if(r.col.name == "Dew_Point"){  
    col.dpt <- read.csv(file = "TempSymbology/Dew_Point.csv", header = TRUE)
    #zlim.dpt <- (c(0,26)) * ext.z
    colorlut.dpt <- seq(1,length(col.dpt[,1]))
    for (i in 1:length(colorlut.dpt)){
      colorlut.dpt[i] <- rgb(col.dpt[i,2],col.dpt[i,3],col.dpt[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.dpt
    colorlut <- colorlut.dpt
    
  }else if(r.col.name == "RH_Percent"){  
    col.rh <- read.csv(file = "TempSymbology/RH_Percent.csv", header = TRUE)
    #zlim.rh <- (c(0,1)) * ext.z
    colorlut.rh <- seq(1,length(col.rh[,1]))
    for (i in 1:length(colorlut.rh)){
      colorlut.rh[i] <- rgb(col.rh[i,2],col.rh[i,3],col.rh[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.rh
    colorlut <- colorlut.rh
    
  }else if(r.col.name == "ppt_mm"){  
    col.ppt <- read.csv(file = "TempSymbology/ppt_mm.csv", header = TRUE)
    #zlim.ppt <- (c(0,40)) * ext.z
    colorlut.ppt <- seq(1,length(col.ppt[,1]))
    for (i in 1:length(colorlut.ppt)){
      colorlut.ppt[i] <- rgb(col.ppt[i,2],col.ppt[i,3],col.ppt[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.ppt
    colorlut <- colorlut.ppt
    
  }else if(r.col.name == "ppt_mm_sum"){  
    col.ppt.sum <- read.csv(file = "TempSymbology/ppt_mm_sum.csv", header = TRUE)
    #zlim.ppt.sum <- (c(0,100)) * ext.z
    colorlut.ppt.sum <- seq(1,length(col.ppt.sum[,1]))
    for (i in 1:length(colorlut.ppt.sum)){
      colorlut.ppt.sum[i] <- rgb(col.ppt.sum[i,2],col.ppt.sum[i,3],col.ppt.sum[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.ppt.sum
    colorlut <- colorlut.ppt.sum
    
  }else if(r.col.name == "ws2m_ms"){  
    col.ws <- read.csv(file = "TempSymbology/ws2m_ms.csv", header = TRUE)
    #zlim.ws <- (c(0,33)) * ext.z
    colorlut.ws <- seq(1,length(col.ws[,1]))
    for (i in 1:length(colorlut.ws)){
      colorlut.ws[i] <- rgb(col.ws[i,2],col.ws[i,3],col.ws[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.ws
    colorlut<- colorlut.ws
  }else if(r.col.name == "ws10m_ms"){  
    col.ws <- read.csv(file = "TempSymbology/ws10m_ms.csv", header = TRUE)
    zlim.ws <- (c(0,33)) * ext.z
    colorlut.ws <- seq(1,length(col.ws[,1]))
    for (i in 1:length(colorlut.ws)){
      colorlut.ws[i] <- rgb(col.ws[i,2],col.ws[i,3],col.ws[i,4],maxColorValue=255)#max)
    }
    # zlim <- zlim.ws
    colorlut <- colorlut.ws
    
  }else if(r.col.name == "VPD"){  
    col.vpd <- read.csv(file = "TempSymbology/VPD.csv", header = TRUE)
    # zlim.vpd <- (c(0,10)) * ext.z
    colorlut.vpd <- seq(1,length(col.vpd[,1]))
    for (i in 1:length(colorlut.vpd)){
      colorlut.vpd[i] <- rgb(col.vpd[i,2],col.vpd[i,3],col.vpd[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.vpd
    colorlut <- colorlut.vpd
    
  }else if(r.col.name == "AT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
    }else if(r.col.name == "X2.AirTEMPIN"){
      col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
      #zlim.t <- (c(0,67)) * ext.z
      colorlut.t <- seq(1,length(col.t[,1]))
      for (i in 1:length(colorlut.t)){
        colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
      }
      #zlim <- zlim.t
      colorlut <- colorlut.t
    }else {
    #col.t <- read.csv(file = "TempSymbology/default.csv", header = TRUE)
    col.t <- read.csv(file = "TempSymbology/default.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }
  
  return(colorlut)
}
##############################################################################
#side.rgl.3d.plot <- cmpfun(side.rgl.3d)

##############################################################################
create.treatment.array.cust <- function(num.meas.days,tod.meas.days, treat.range,rawdata,
                                        treatment.col1,midnight,y.temp,r.col.names,
                                        mask.check, mask.rows.l,mask.rows.h, tod.start, tod.end, 
                                        dap.view,dap.treatment.col1,file.type,turn.2.hr)
  
{
  
  treatment.array <- array(dim = c(num.meas.days, treat.range, length(r.col.names)))
  treatment.array.hr <- array(dim = c(24, treat.range, length(r.col.names)))
  
  if(dap.view == 1)col.temp <- treatment.col1
  if(dap.view == 2)col.temp <- dap.treatment.col1 + (treatment.col1 - 1)
  temp.col <- length(r.col.names)
  for(t in 1:temp.col)
  {
    if(dap.view == 1)temp.start <- midnight[col.temp]
    if(dap.view == 2)temp.start <- midnight[col.temp[t]]
    temp.end <- temp.start + (num.meas.days - 1)
    #for(j in 1:treat.range)
    j<-as.numeric(1)
    while(j <= treat.range)
    {
      #if(is.na(temp.start)==TRUE)break()
      if(mask.check == TRUE){
        rawdata[mask.rows.l,y.temp] <- as.numeric(-9999)
        rawdata[mask.rows.h,y.temp] <- as.numeric(9999)
      }
      treatment.array[,j,t] <- cbind(rawdata[temp.start:temp.end, y.temp])
      
      if(tod.start[1] != 1)treatment.array[1:tod.start[1],j,t] <- as.numeric(-9999)
      
      temp.start <- temp.end + 1
      temp.end <- temp.start + (num.meas.days - 1)
      
      j <- j + 1
      
    }
    y.temp <- y.temp + 1
    
  }
  
  #####Check for canopy temperature outliners > 150####################################
  treatment.array[treatment.array > 1000] <- c(0)
  treatment.array[treatment.array < 0] <- c(0)
  treatment.array[is.na(treatment.array)] <- c(0)
  
 #treatment.array <- treatment.array/100
  
  if(turn.2.hr == "TRUE"){
    for(b in 1:length(r.col.names)){
      for(a in 1:treat.range){
        st <- 1
        end <- 4
        for(r in 1:24){
          treatment.array.hr[r,a,b] <- mean(treatment.array[st:end,a,b])
          st <- end + 1
          end <- st + 3
        }
      }
    }
    
    treatment.array <- treatment.array.hr
  }
  
  
  return(treatment.array)
}

create.rawdata <- function(input.type,yr.ui.input,col.ui.input,loc)
{
  if(input.type == 'Enviromental'){
    for(h in 1:length(loc)){
      for(j in 1:length(yr.ui.input)){
        if(j == 1){
          file <- paste(getwd(),'/csvfiles/',yr.list[yr.ui.input[j]],'_',loc.list[h],'_','hr.csv',sep = '')
          rawdata.0 <- data.frame(read.table(file, sep=",",header = TRUE, as.is = TRUE))
          if(length(col.ui.input) == 1)rawdata.0 <- rawdata.0[col.ui.input[1]]
          if(length(col.ui.input) > 1)rawdata.0 <- rawdata.0[c(col.ui.input[1:length(col.ui.input)])]
        }
        if(j > 1){
          file <- paste(getwd(),'/csvfiles/',yr.list[yr.ui.input[j]],'_',loc.list[h],'_','hr.csv',sep = '')
          rawdata.1 <- data.frame(read.table(file, sep=",",header = TRUE, as.is = TRUE))
          if(length(col.ui.input) == 1)rawdata.1 <- rawdata.1[col.ui.input[1]]
          if(length(col.ui.input) > 1)rawdata.1 <- rawdata.1[c(col.ui.input[1:length(col.ui.input)])]
          rawdata.0 <- cbind(rawdata.0,rawdata.1)
        }
      }
      if(h == 1)rawdata <- rawdata.0
      if(h > 1)rawdata <- cbind(rawdata,rawdata.0)
    }
    
    ##############################################################
  }
  if(input.type == 'Canopy Temperatures'){
    for(j in 1:length(yr.ui.input)){ 
      if(yr.ui.input[j] == 1)rawdata.0 <- raw.data.file.11
      if(yr.ui.input[j] == 2)rawdata.0 <- raw.data.file.12
      if(yr.ui.input[j] == 3)rawdata.0 <- raw.data.file.14
      if(yr.ui.input[j] == 4)rawdata.0 <- raw.data.file.15
      
      if(length(col.ui.input) == 1)rawdata.0 <- rawdata.0[col.ui.input[1]]
      if(length(col.ui.input) > 1)rawdata.0 <- rawdata.0[c(col.ui.input[1:length(col.ui.input)])]
      if(j == 1)rawdata <- rawdata.0
      if(j > 1)rawdata <- cbind(rawdata,rawdata.0)
    }
  }
  ##############################################################
#   if(input.type == 'Custom Input File'){
#     rawdata.0 <- rawdata.cust
#     if(length(col.ui.cust) == 1)rawdata.0 <- rawdata.0[col.ui.cust[1]]
#     if(length(col.ui.cust) > 1)rawdata.0 <- rawdata.0[c(col.ui.cust[1:length(col.ui.cust)])]
#     rawdata <- rawdata.0
#   }
  ##############################################################
  return(rawdata)
}
#install_github("rgl", "trestletech", "js-class")
################################################################################################################################################################################################################################
create.temp.colorlut.ceta <- function(r.col.name, ext.z)
{
  colorlut <- data.frame(v=1,ch=c("colorlut"))
  
if(r.col.name == "X2.AirTempIN"){
  col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
  #zlim.t <- (c(0,67)) * ext.z
  colorlut.t <- seq(1,length(col.t[,1]))
  for (i in 1:length(colorlut.t)){
    colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
  }
  #zlim <- zlim.t
  colorlut <- colorlut.t
  }else if(r.col.name == "X3.AirTempIN"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.AirTempIN"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.AirTempIN"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.AirTempIN"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.AirTempIN"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.AirTempOUT"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.IRTC_Avg"){
    col.t <- read.csv(file = "TempSymbology/AT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.AvgH2OIn"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.AvgH2OOut"){
    col.t <- read.csv(file = "TempSymbology/default_h2oin_out.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  
  }else if(r.col.name == "X2.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.DeltaH20"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.DeltaT"){
    col.t <- read.csv(file = "TempSymbology/default_deltaT.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X2.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X3.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X4.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X5.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X6.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
  }else if(r.col.name == "X7.DeltaCO2"){
    col.t <- read.csv(file = "TempSymbology/default_deltaCO2.csv", header = TRUE)
    #zlim.t <- (c(0,67)) * ext.z
    colorlut.t <- seq(1,length(col.t[,1]))
    for (i in 1:length(colorlut.t)){
      colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
    }
    #zlim <- zlim.t
    colorlut <- colorlut.t
}else {
  #col.t <- read.csv(file = "TempSymbology/default.csv", header = TRUE)
  col.t <- read.csv(file = "TempSymbology/default_co2in_out.csv", header = TRUE)
  #zlim.t <- (c(0,67)) * ext.z
  colorlut.t <- seq(1,length(col.t[,1]))
  for (i in 1:length(colorlut.t)){
    colorlut.t[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
  }
  #zlim <- zlim.t
  colorlut <- colorlut.t
}

return(colorlut)
}
# ##############################################################################
#   
  