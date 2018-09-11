options(rgl.useNULL=TRUE)
options(shiny.maxRequestSize=300*1024^2)
#######################################################################
#
#last updated on 11/16/2015 @ 22:48
#
#Created by: Clayton Dorrity
#
#Contact: 
#email:clayton.dorrity@gmail.com
#
#phone: (806) 729-9519
#
#######################################################################

library(shiny)
library(rgl)
library(shinyRGL)
library(shinythemes)



enviro.var <- c('AT','Dew_Point','RH_Percent','VPD', 'ppt_mm', 'ppt_mm_sum', 
                'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
cust.var <- c("p1.00"   ,  
              "p1.15" ,     "p1.30"  ,    "p1.60"   ,   "p2.00"   ,   "p2.15"  ,
              "p2.30"  ,    "p2.60"    ,  "p3.00"   ,   "p3.15"   ,   "p3.30"  ,
              "p3.60" ,     "p4.00",      "p4.15"  ,    "p4.30"  ,    "p4.60"  ,
              "p5.00"   ,   "p5.15"   ,   "p5.30"   ,   "p5.60"   ,   "p6.00" ,
              "p6.15"    ,  "p6.30"  ,    "p6.60"   ,   "p7.00"  ,    "p7.15"  ,    
              "p7.30"    ,  "p7.60" )
ct.var <- c("p1","p2","p3","p4","p5","p6","p7") 

treat.var <- c('.00','.15','.30','.60')

temp <- c(enviro.var,ct.var)

shinyUI(
  fluidPage(
    tags$head(
      theme = shinytheme("flatly"),
      img(src ="dmvd_cow2.png",width="450px")
      ),

    tabsetPanel(id = "inTabset",
                type = "tabs", 
                #type = "pills",
                #tabPanel('DEMO'),
                tabPanel('CREATE',
                         #"Time Surface Inputs",
                         #tabsetPanel(
                         #helpText(h3("If this window turns grey, click the REFRESH button on your browser")),
                           navlistPanel(
                             tabPanel("Select Input Assignments",
                                      inputPanel(
                           wellPanel(
                             selectInput("input_type", "Choose visualization type:",
                                         c('Enviromental', 'Canopy Temperatures','Custom Input File')),
                                       radioButtons('view2d.3d',NULL,
                                                    choices = list('2D ' = 1,'3D ' = 2 ),
                                                    selected = 1),
                                       radioButtons("labels.cust",NULL,
                                                    choices = list('Labels ON' = 1, 'Labels OFF' = 2),
                                                    selected = 2),
                             checkboxInput("show.axis","Show plot axis",FALSE),

                                       conditionalPanel(
                                         condition = "input.input_type == 'Canopy Temperatures'",
                                         radioButtons("doy.dap.view",NULL,
                                                      choices = list("DOY View" = 1, "DAP View" = 2),
                                                      selected = 1))
                                     ),
                                     uiOutput("ui"),
                                     wellPanel(
                                       numericInput("n.start.cust","DOY-DAP range:", value = 1),
                                       numericInput("n.end.cust",NULL, value = 365),
                                     conditionalPanel(
                                       condition = "input.input_type == 'Enviromental'",
                                       textInput("tod.start.cust","TOD range:", value = '00:00'),
                                       textInput("tod.end.cust",NULL, value = '23:00')
                                     ),
                                     conditionalPanel(
                                       condition = "input.input_type == 'Canopy Temperatures'",
                                       textInput("tod.start.cust","TOD range:", value = '00:00'),
                                       textInput("tod.end.cust",NULL, value = '23:45')
                                     ))
                                   ),
                                   inputPanel(
                                     wellPanel(
#                                        selectInput("plot_dim",'Supported Screen Resolutions (width x height)',
#                                                     choices = list('4K TV (3200x1600)', 'PC Monitor (1920x1200)',
#                                                                    'Ipad screen (768x1024)'),
#                                                     selected = 1),
                                       radioButtons("plot_dim",'Supported Screen Resolutions (width x height)',
                                                    choices = list('4K TV (3200x1600)', 'PC Monitor (1920x1200)',
                                                                   'Ipad screen (1024x768)'),
                                                    selected = '4K TV (3200x1600)'),
                                       conditionalPanel(
                                         condition = "input.plot_dim == '4K TV (3200x1600)'",
                                     numericInput("html.width.1","width", value = 3000),
                                     numericInput("html.height.1","height", value = 1500)
                                       ),
                                     conditionalPanel(
                                       condition = "input.plot_dim == 'PC Monitor (1920x1200)'",
                                       numericInput("html.width.2","width", value = 1800),
                                       numericInput("html.height.2","height", value = 1000)
                                     ),
                                     conditionalPanel(
                                       condition = "input.plot_dim == 'Ipad screen (1024x768)'",
                                       numericInput("html.width.3","width", value = 900),
                                       numericInput("html.height.3","height", value = 600)
                                     )
                                     ),
                           wellPanel("To find your screen resolution, ",
                                     tags$a(href="http://www.whatismyscreenresolution.com", 
                                            "CLICK HERE"),
                                     hr(),
                                     "ATTENTION",
                                     helpText("It is recommended to open the above link in a new tab or window")
                                     )
                             ),
                                     
#),
                                   verbatimTextOutput("data.sum")

),
                          tabPanel("Assign Masking Filter",
                                   inputPanel(
                                     wellPanel(
                                       checkboxInput("mask.all.cols2","Apply filter by a specific range of values",FALSE)
                                     ),
                                     wellPanel(
                                       checkboxInput("mask.all.cols", "Apply filter by a specific column", FALSE),
                                       selectInput("select", NULL, 
                                                   choices = temp,
                                                   selected = NULL)
                                     ),
                                     wellPanel(
                                       numericInput("mask.l.num.cust", "Values below :", value = 0),      
                                       numericInput("mask.h.num.cust", "Values above :", value = 0)
                                     ),
                                     wellPanel(
                                       selectInput("mask.color.cust.b", "Color for values below", 
                                                   choices = list("grey" = 1, "white" = 2, "black" = 3),
                                                   #choices = list("WHITE" = 2), 
                                                   selected = 2),
                                       selectInput("mask.color.cust.a", "Color for values above", 
                                                   choices = list("grey" = 1, "white" = 2, "black" = 3), 
                                                   #choices = list("GREY" = 1), 
                                                   selected = 1)#,
                                       #submitButton("Apply", icon("refresh"))#,width='100%')
                                     )
                                   )
                          ),
                          tabPanel("Adjust Settings",
                                   inputPanel(
                                     wellPanel(
                                       selectInput("back.color.cust", "Plot background Color", 
                                                   choices = list("grey" = 1, "white" = 2, "black" = 3), 
                                                   selected = 1)
                                     ),
                                     wellPanel(
                                       radioButtons("auto.plot.depth",NULL,
                                                    choices = list('Default # of plot columns' = 1, 'Custom # of plot columns' = 2),
                                                    selected = 1),
                                       numericInput("plot.depth",NULL,value=3)
                                     ),
                                     wellPanel("Plot axis multipliers",
                                               numericInput("ext.x.cust", "TOD",value = 1),#1500),
                                               numericInput("ext.y.cust", "DOY",value = 1),#700),
                                               numericInput("ext.z.cust", "Surface",value = 1)
                                     ),
                                     wellPanel("Offsets between plots",
                                               numericInput("y.offset.cust", "Left/Right",value = 1),
                                               numericInput("x.offset.cust", "Up/Down",value = 1)
                                     )
                                   )
                          ),
tabPanel(actionButton("do","Apply",icon("refresh"),width='100%'),
webGLOutput("sctPlot.cust")),#height = "100%")
# tabPanel("Process Time Surface Image",
#          webGLOutput("sctPlot.cust")#height = "100%")
# ),
#submitButton("Apply", icon("refresh")),#,width='100%')
#actionButton("process","Process Custom Time Surface Image"),
widths = c(2,10))
),
tabPanel("EXPLORE",
#          singleton(
#            tags$head(tags$script(src = "message-handler.js"))
#          ),
         tabsetPanel(type = "pills",#selected =  "PC",
                     tabPanel("Ipad screen (768x1024)",
                              tags$embed(src = "index.html",
                                         height = 700, width = 1000)
                     ),
                     tabPanel("PC Monitor (1920x1200)",id='PC',
                              tags$embed(src = "index.html",
                                         height = 1200, width = 1920)
                              ),
                     tabPanel("4K TV (3200x1600)",
                              tags$embed(src = "index.html",
                                         height = 1600, width = 3200)
                     ))
),
tabPanel("CATALOG"))
))#shiny and fluid page              