options(rgl.useNULL=TRUE)
options(shiny.maxRequestSize=300*1024^2)
#options(host="127.0.0.1:4340")
#######################################################################
#
#last updated on 11/21/2015 @ 12:06
#
#Created by: Clayton Dorrity
#
#Contact: 
#email:clayton.dorrity@gmail.com
#
#phone: (806) 729-9519
#
#######################################################################
#options(shiny.maxRequestSize=30*1024^2)
#options( device = "browser")#"quartz" )
#http://www.whatismyscreenresolution.com/

rm(list=ls())
library(shiny)
library(shinyRGL)
library(rgl)

source("shinyfunctions.R")
################################################
# input <- 'http://www.lbk.ars.usda.gov/WEWC/pswc_met/dataport/pswc_1hr_15.txt'
# 
# page <- url('http://www.lbk.ars.usda.gov/WEWC/pswc_met/dataport/pswc_1hr_15.txt')
# input<- readLines(page)
# 
# input <- download.file('http://www.lbk.ars.usda.gov/WEWC/pswc_met/dataport/pswc_1hr_15.txt', 'myfile.txt')
# 
# raw <- data.frame(read.table("myfile.txt", sep=",",header = FALSE, as.is = TRUE, skip = 3))
# write.table(y, file = "/Users/mahanlab1/test.csv", sep = ",", row.names = FALSE, col.names = TRUE)
################################################
# raw.data.file.11 <- data.frame(read.table("csvfiles/2011_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
# raw.data.file.12 <- data.frame(read.table("csvfiles/2012_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
# raw.data.file.14 <- data.frame(read.table("csvfiles/2014_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
# raw.data.file.15 <- data.frame(read.table("csvfiles/2015_15min_MASTER.csv", sep=",",header = TRUE, as.is = TRUE))
time.plotly <- data.frame(read.table("csvfiles/time.csv", sep=",",header = TRUE, as.is = TRUE))

tod.min <- rep(seq(0,45,by=15),24)
tod.hr <- rep(seq(0,23),each=4)
tod <-paste(tod.hr,tod.min,sep=":")
tod[1:40] <- paste("0",tod[1:40],sep="")
tod[seq(1,96,by=4)] <- paste(tod[seq(1,96,by=4)],"0",sep="")
tod.seq <- rep(tod,365)
tod.seq.hr <- tod.seq[seq(1,35040,by=4)]
enviro.var <- c('AT','Dew_Point','RH_Percent','VPD', 'ppt_mm', 'ppt_mm_sum', 
                'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
ct.var <- c("p1","p2","p3","p4","p5","p6","p7") 
treat.var <- c('.00','.15','.30','.60')
mask.var <- c(enviro.var,ct.var)
yr.list <- c('2003','2004','2005','2006','2007','2008','2009',
             '2010','2011','2012','2013','2014','2015')
loc.list <- c('lbk', 'CSIRO', 'FL')
dap.11 <- c(102,119,133,145,160,173,192)
dap.12 <- c(95,108,122,138,150,164,178)
dap.14 <- c(93,105,119,133,154,167,182)
dap.15 <- c(93,105,119,133,154,167,182)
dap.all <- cbind(dap.11,dap.12,dap.14,dap.15)
rownames(dap.all) <- ct.var
cat("\014")#Clears the console screen in R Studio
###########################################################################################################################
shinyServer(function(input, output, session) {
  observeEvent(input$do,{
  output$sctPlot.cust <- renderWebGL({
      start.time <- Sys.time()
    #helpText(paste("Time Surface Generation began at: ",start.time,sep=""))
    #})
    make2d.3d <- "TRUE"#input$rgl.refresh
    dap.view <- as.numeric(input$doy.dap.view)
    ########################################################################################################################## 
    if(as.numeric(input$labels.cust) == 1)label.marker <- as.numeric(1)
    if(as.numeric(input$labels.cust) != 1)label.marker <- as.numeric(0)
    ############################################################################
    if(input$mask.color.cust.b == 1) mask.colorlut.b = "#A9A9A9"#FFFFFFFF"#"grey"
    if(input$mask.color.cust.b == 2) mask.colorlut.b = "#FFFFFFFF"#"white"
    if(input$mask.color.cust.b == 3) mask.colorlut.b = "#000000"#FFFFFFFF"#"black
    #################################
    if(input$mask.color.cust.a == 1) mask.colorlut.a = "#A9A9A9"#FFFFFFFF"#"grey"
    if(input$mask.color.cust.a == 2) mask.colorlut.a = "#FFFFFFFF"#"white"
    if(input$mask.color.cust.a == 3) mask.colorlut.a = "#000000"#FFFFFFFF"#"black
    ############################################################################
    ext.z <- as.numeric(1)
    yr.ui.ct <- c(as.numeric(input$yr.dates.ct))
    col.ui.ct.1 <- as.character(input$plantings.cust)
    col.ui.ct.2 <- as.character(input$plantings.cust2)
    col.ui.ct.3 <- as.character(input$treatments.cust)
    col.ui.ct <- paste(rep(col.ui.ct.2,each=length(col.ui.ct.3)),col.ui.ct.3,sep='')
    col.ui.ct.plants <- col.ui.ct
    if(length(col.ui.ct.1) >= 1){
      if(length(col.ui.ct.2) == 1)col.ui.ct <- c(col.ui.ct.1,col.ui.ct)
      if(length(col.ui.ct.2) > 1){
        col.ui.ct <- c(col.ui.ct.1,col.ui.ct[1:length(col.ui.ct.3)])
        temp.inc <- length(col.ui.ct.3)
        st <- length(col.ui.ct.3) + 1
        end <- length(col.ui.ct.3) * 2
        for(u in 2:length(col.ui.ct.2)){
          col.ui.ct.temp <- c(col.ui.ct.1,col.ui.ct.plants[st:end])
          st <- end + 1
          end <- end + length(col.ui.ct.3)
          col.ui.ct <- c(col.ui.ct,col.ui.ct.temp)
        }
      }
    }else {col.ui.ct <- c(col.ui.ct.1,col.ui.ct)}
    ##############################################################
    yr.ui <- c(as.numeric(input$yr.dates.cust))
    loc <- as.numeric(input$location.view.radio2)
    col.ui <- as.character(input$plant.dates.cust)
    ##############################################################
    col.ui.cust <- as.character(input$plant.dates.cust3)
    tod.st.input <- as.character(input$tod.start.cust)
    tod.end.input <- as.character(input$tod.end.cust)
    ##############################################################
    yr.choice <- as.numeric(2)#input$yr.col.row)
    if(as.numeric(input$doy.dap.view) == 1)dap.view <- as.numeric(1)
    if(dap.view == 1){
      treatment.col1 <- as.numeric(input$n.start.cust)
      treatment.col2 <- as.numeric(input$n.end.cust)
      treat.range <- (treatment.col2 - treatment.col1) + 1
    }else if(dap.view == 2){
      dap.temp <- 1
      dap.treatment.col1 <- vector('logical', (((length(col.ui.ct.3) * length(col.ui.ct.2)) + length(col.ui.ct.1)) * length(yr.ui.ct)))
      for(q in 1:length(yr.ui.ct)){
        for(p in 1:length(col.ui.ct.2)){
          dap.row <- which(col.ui.ct.2[p] == rownames(dap.all))
          dap.treatment.col1[dap.temp] <- dap.all[dap.row,yr.ui.ct[q]]
          dap.temp <- dap.temp + 1
        }
      }
      dap.treatment.col2 <- dap.treatment.col1[c(which(dap.treatment.col1 != 0))]
      
      if(length(col.ui.ct.1) >= 1){
        if(length(col.ui.ct.2) >= 1)dap.treatment.col1 <- rep(dap.treatment.col2,each = (length(col.ui.ct.1)+length(col.ui.ct.3)))
      }else {dap.treatment.col1 <- rep(dap.treatment.col2,each = length(col.ui.ct.3))}
      treatment.col1 <- as.numeric(input$n.start.cust)
      treatment.col2 <- as.numeric(input$n.end.cust)
      treat.range <- (treatment.col2 - treatment.col1) + 1
    }else {
      treatment.col1 <- as.numeric(input$n.start.cust)
      treatment.col2 <- as.numeric(input$n.end.cust)
      treat.range <- (treatment.col2 - treatment.col1) + 1
    }
    #########################################################################################################
    file.type <- as.character(input$input_type)
    if(file.type == 'Enviromental'){
      turn.2.hr <- 'FALSE'
      updateRadioButtons(session, 'doy.dap.view',choices = list('DOY VIEW' = 1),
                         selected = input$doy.dap.view)
      rawdata <- create.rawdata(file.type,yr.ui,col.ui,loc)
      col.ui.ct.2 <- yr.list[yr.ui]
      col.ui.ct.3 <- col.ui
      num.meas.days <- as.numeric(24)
      midnight <- grep("00:00",tod.seq.hr)
      tod.start <- grep(tod.st.input,tod.seq.hr)
      tod.end <- grep(tod.end.input,tod.seq.hr)
      #depth.plots <- length(col.ui)
      dap.treatment.col1 <- 0
      depth.plots <- length(col.ui)
      if(as.numeric(input$auto.plot.depth) == 1){
        depth.plots <- length(col.ui)
        #depth.plots <- as.numeric(input$plot.depth)
        row.depth.plots <- as.numeric(0)
      }else {
        depth.plots <- as.numeric(input$plot.depth)
        #row.depth.plots <- as.numeric(input$plot.depth)
      }
      updateSelectInput(session,"select",choices = names(rawdata))
      ##############################################################
    }else if(file.type == 'Canopy Temperatures'){
      turn.2.hr <- as.character(input$turn.2.hour)
      updateRadioButtons(session, 'doy.dap.view',choices = list('DOY VIEW' = 1,'DAP VIEW' = 2),
                         selected = input$doy.dap.view)
#       validate(
#         need(input$yr.dates.ct != '', "Click Refresh")
#       )
      rawdata <- create.rawdata(file.type,yr.ui.ct,col.ui.ct,loc)
      num.meas.days <- as.numeric(96)
      midnight <- grep("00:00",tod.seq)
      tod.start <- grep(tod.st.input,tod.seq)
      tod.end <- grep(tod.end.input,tod.seq)
      #depth.plots <- as.numeric(input$plot.depth)
#       depth.plots <- length(col.ui.ct.3)
#       if(length(col.ui.ct.1) >= 1)depth.plots <- (length(col.ui.ct.3) + length(col.ui.ct.1)) 
      if(as.numeric(input$auto.plot.depth) == 1){
        depth.plots <- length(col.ui.ct.3)
        if(length(col.ui.ct.1) >= 1)depth.plots <- (length(col.ui.ct.3) + length(col.ui.ct.1)) 
#         depth.plots <- as.numeric(input$plot.depth)
#         row.depth.plots <- as.numeric(0)
      }else {
        depth.plots <- as.numeric(input$plot.depth)
        row.depth.plots <- as.numeric(0)
        #row.depth.plots <- as.numeric(input$plot.depth)
      }
      ##############################################################
    }else if(file.type == 'Custom Input File'){
      num.meas.days <- as.numeric(96)

      turn.2.hr <- as.character(input$turn.2.hour)
      validate(
        need(input$file1 != "", "Please Select a .csv File & Press APPLY in the Left Margin")
      )
      inFile <- input$file1
      if(is.null(inFile))
        return(NULL)    
      rawdata.cust <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
      updateSelectizeInput(session, "plant.dates.cust3",choices = names(rawdata.cust), selected = input$plant.dates.cust3)     
      
      rawdata.0 <- rawdata.cust
      if(length(col.ui.cust) == 1)rawdata.0 <- rawdata.0[col.ui.cust[1]]
      if(length(col.ui.cust) > 1)rawdata.0 <- rawdata.0[c(col.ui.cust[1:length(col.ui.cust)])]
      rawdata <- rawdata.0
      if(num.meas.days == 96)midnight <- grep("00:00",tod.seq)
      if(num.meas.days == 24)midnight <- grep("00:00",tod.seq.hr)
      tod.start <- grep(tod.st.input,tod.seq)
      tod.end <- grep(tod.end.input,tod.seq)
      #depth.plots <- as.numeric(input$plot.depth)
      if(as.numeric(input$auto.plot.depth) == 1){
        depth.plots <- as.numeric(input$plot.depth)
        #depth.plots <- length(col.ui.ct.3)
        if(length(col.ui.ct.1) >= 1)depth.plots <- (length(col.ui.ct.3) + length(col.ui.ct.1)) 
      }else {depth.plots <- as.numeric(input$plot.depth)}
    }
    ##############################################################
    r.col.names <- colnames(rawdata)
    output$data.sum <- renderPrint({
      summary(rawdata)
    })
    ##############################################################
    mask.check <- input$mask.all.cols
    mask.check2 <- input$mask.all.cols2
    
    if(is.null(mask.check) == TRUE)mask.check <- FALSE
    if(is.null(mask.check2) == TRUE)mask.check2 <- FALSE
    
    if(mask.check2 == TRUE){
      updateCheckboxInput(session,"mask.all.cols",value = FALSE)
      mask.check <- FALSE
      if(as.numeric(input$mask.l.num.cust) == 0){mask.l <- NULL#as.numeric(0)
      }else {mask.l <- as.numeric(input$mask.l.num.cust)}
      if(as.numeric(input$mask.h.num.cust) == 0){mask.h <- NULL#as.numeric(0)
      }else {mask.h <- as.numeric(input$mask.h.num.cust)}
    }else{
      mask.check2 <- FALSE
    }
    
    if(mask.check == TRUE){
      updateCheckboxInput(session,"mask.all.cols2",value = FALSE)
      mask.check2 <- FALSE
      mask.column <- as.character(input$select)
      #mask.column.num <- grep(mask.column, as.character(enviro.var))
      mask.column.num <- grep(mask.column, names(rawdata))
      #show(mask.column.num)
      if(is.null(input$mask.l.num.cust) == FALSE){
        if(as.numeric(input$mask.l.num.cust) == 0){mask.l <- NULL#as.numeric(0)
        }else {mask.l <- as.numeric(input$mask.l.num.cust)}
        if(as.numeric(input$mask.h.num.cust) == 0){mask.h <- NULL#as.numeric(0)
        }else {mask.h <- as.numeric(input$mask.h.num.cust)}
        #mask.l <- as.numeric(input$mask.l.num.cust)
        if(mask.column == 'Solar_WM2')mask.l <- (mask.l / 100) 
        mask.rows.l <- which(rawdata[,mask.column.num] < mask.l)
      }else mask.l <- NULL
      if(is.null(input$mask.h.num.cust) == FALSE){
        #mask.h <- as.numeric(input$mask.h.num.cust)
        if(mask.column == 'Solar_WM2')mask.h <- (mask.h / 100) 
        mask.rows.h <- which(rawdata[,mask.column.num] > mask.h)
      }else mask.h <- NULL
    }else{
      mask.check <- FALSE
    }
    ###########################################################################################################################
    treatment.array <- create.treatment.array.cust(num.meas.days,as.numeric(tod.end[1]-tod.start[1] + 1),
                                                   treat.range,rawdata,treatment.col1,midnight,1,r.col.names,
                                                   mask.check, mask.rows.l,mask.rows.h, tod.start, tod.end,dap.view, dap.treatment.col1,file.type,turn.2.hr)
    if(turn.2.hr == "TRUE")num.meas.days <- as.numeric(24)
    ###########################################################################################################################   
    bg3d(as.character(input$back.color.cust))
    phi.angle.f <- as.numeric(90)#input$pov.angle.cust.f) 
    phi.angle.s <- as.numeric(-90)#input$pov.angle.cust.s) 
    #####################################################################################
    if(input$back.color.cust == 1)bg3d(paste('#',"808080",sep=""))#"grey")
    if(input$back.color.cust == 2)bg3d("white")
    if(input$back.color.cust == 3)bg3d("black")
    clear3d(type=("lights"))
    zoom.f <- as.numeric(.7)
    if(as.character(input$show.axis) == 'TRUE')axes3d()
    #zoom.f <- as.numeric(input$zoom.plot)/10
    view3d( theta=phi.angle.s, phi=phi.angle.f, zoom = zoom.f)
    #####Assign variables from treatment matrix to variables used in rgl.surface
    ext.x <- as.numeric(input$ext.x.cust)
    ext.y <- as.numeric(input$ext.y.cust)
    if(as.numeric(input$view2d.3d) == 1)ext.x <- ext.x * 1500
    if(as.numeric(input$view2d.3d) == 1)ext.y <- ext.y * 700
    x <- ext.x * (1:num.meas.days) #TOD
    y <- ext.y *(1:treat.range) #DOY
    z.vector <- array(dim = c(num.meas.days, treat.range,length(r.col.names)))
    x.vector <- z.vector
    #if(file.type == 'Enviromental')width.offset <- as.numeric(input$y.offset.cust) + 24
    width.offset <- as.numeric(input$y.offset.cust) + num.meas.days
    if(file.type == 'Canopy Temperatures')if(as.numeric(input$view2d.3d) == 1){
      width.offset <- as.numeric(input$y.offset.cust) * 10000
    }else {width.offset <- as.numeric(input$y.offset.cust)*10}
   # if(file.type == 'Enviromental')side.heigth.offset <- as.numeric(input$x.offset.cust) + 25 
    #if(file.type == 'Canopy Temperatures')
      side.heigth.offset <- as.numeric(input$x.offset.cust) + num.meas.days + 1
    #####################################################################################
      updateTabsetPanel(session,"inTabset",selected = "EXPLORE")
      progress <- shiny::Progress$new(session,"working")
    on.exit(progress$close())
    progress$set(message = paste("Time Surface Generation began at: ",start.time,sep=""))
    progress$set(message = 'Processing...')
    end.time <- side.rgl.3d(length(col.ui),(x*-1),y,z,ext.x,ext.y,as.numeric(input$ext.z.cust),0,0,depth.plots,0,x.vector,treatment.array, 
                            treat.range,session,col.ui,r.col.names, 
                            mask.check, yr.choice,yr.ui,label.marker,width.offset,side.heigth.offset,length(col.ui),mask.l,mask.check2,mask.h,
                            mask.colorlut.b,mask.colorlut.a,col.ui.ct.2,col.ui.ct.3,yr.ui.ct,file.type,col.ui.ct,col.ui.cust)
   
    if(as.character(input$plot_dim) =='4K TV (3200x1600)'){
             webgl.wid <- as.numeric(input$html.width.1)
             webgl.hei <- as.numeric(input$html.height.1)
    }
    if(as.character(input$plot_dim) =='PC Monitor (1920x1200)'){
      webgl.wid <- as.numeric(input$html.width.2)
      webgl.hei <- as.numeric(input$html.height.2)
    }
    if(as.character(input$plot_dim) =='Ipad screen (1024x768)'){
      webgl.wid <- as.numeric(input$html.width.3)
      webgl.hei <- as.numeric(input$html.height.3)
    }
    if(make2d.3d == "TRUE"){
      writeWebGL(dir = 'www',snapshot = FALSE,
                 width = webgl.wid  ,height = webgl.hei)
      #show('****3D PLOT GENERATED**********')
    }
    validate(
      need(input$data != "", "THIS PAGE WILL REFRESH WHEN TIME SURFACES ARE FINISHED BEING CREATED")
    )
    
  }, height = 100)
  })#End of Observe
  
  output$table.11 <- renderDataTable({
    cbind(time.plotly, raw.data.file.11)
  },options = list(orderClasses = TRUE,lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')), pageLength = 10))
  
  output$table.12 <- renderDataTable({
    cbind(time.plotly, raw.data.file.12)
  },options = list(orderClasses = TRUE,lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')), pageLength = 10))
  
  output$table.14 <- renderDataTable({
    cbind(time.plotly, raw.data.file.14)
  },options = list(orderClasses = TRUE,lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')), pageLength = 10))
  
  
  output$ui <- renderUI({
    
    switch(as.character(input$input_type),
           'Enviromental' = wellPanel(#"Select",
             selectizeInput("location.view.radio2",label = "Location:",
                            choices = list("Lubbock, Texas" = 1,  "CSIRO-Australia" = 2, "Citra, Florida" = 3), 
                            selected = 1, multiple = TRUE),
             selectizeInput("yr.dates.cust", "Years:", 
                            choices = list('2003' = 1,'2004'= 2, '2005' = 3,'2006'= 4, '2007' = 5,'2008'= 6,
                                           '2009' = 7,'2010'= 8, '2011' = 9,'2012'= 10, '2013'= 11,'2014'= 12,
                                           '2015'= 13),
                            selected = 13, multiple = TRUE),
             selectizeInput("plant.dates.cust","Enviroment",
                            choices = enviro.var,
                            selected = 'AT', multiple = TRUE)#,
             # submitButton("Apply", icon("refresh"))#,width='100%')
           ),
           'Canopy Temperatures' = wellPanel(#"Select",
             
             selectizeInput("yr.dates.ct", label = "Years", 
                            choices = list('2011' = 1,'2012'= 2,'2014' = 3,'2015' = 4),
                            selected = 4, multiple = TRUE),
             selectizeInput("plantings.cust", label = "Enviroment",
                            choices = enviro.var,
                            selected = NULL, multiple = TRUE),
             
             selectizeInput("plantings.cust2", label = "Planting dates:",
                            choices = ct.var,
                            selected = 'p1', multiple = TRUE),
             
             selectizeInput("treatments.cust", label = 'Water treatments:',
                            choices = treat.var,
                            selected = '.00', multiple = TRUE),
             checkboxInput("turn.2.hour","Convert to hourly dataset",value = TRUE)#,
             # submitButton("Apply", icon("refresh"))#,width='100%')
             
           ),
           'Custom Input File' =  wellPanel(#"Select",
             checkboxInput("turn.2.hour","Convert 15min to hourly dataset",value = FALSE),
             selectizeInput("plant.dates.cust3", "Columns:", 
                            choices = list('TEMPORARY COLUMN 1' = 1,'TEMPORARY COLUMN 2'= 2),
                            selected = NULL, multiple = TRUE),
             numericInput("n.meas.days",label = "Input number of measurements/day", value = 96),
             numericInput("n.start.day",label = h5("DOY dataset begins with on line 1"), value = 1),
             fileInput('file1', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
             tags$hr(),
             checkboxInput('header', 'Header', TRUE),
             radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')#,
             #submitButton("Apply", icon("refresh"))#,width='100%')
           )
    )
    
  })
  
  output$datatable.view <- renderUI({
    switch(as.character(input$input.table),
           "Lubbock USDA 2011" = dataTableOutput("table.11"),
           "Lubbock USDA 2012" = dataTableOutput("table.12"),
           "Lubbock USDA 2014" = dataTableOutput("table.14"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      "index.html"
      #paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      #write.csv(
      datasetInput()#, file)
    }
  )
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Current time: ", Sys.time())
  })
  
  
  output$html.output <- renderUI({
    #     tempDir <- tempfile()
    #     dir.create(tempDir)
    #     htmlFile <- file.path(tempDir, "index.html")
    #     # (code to write some content to the file)
    viewer <- getOption("viewer")
    viewer('www/3D Web Page/index.html')
    show('RGL SCENE RAN')
    #updateNavlistPanel(session,"input.topnav", selected = "Time Surface Inputs")
    #viewer('PPT SUM 3D 03-15/index.html')
    #viewer('PPT 3D 03-15/index.html')
  })
  
  # Parse the GET query string
  output$queryText <- renderText({
    query <- parseQueryString(session$clientData$url_search)
    
    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
  
})

