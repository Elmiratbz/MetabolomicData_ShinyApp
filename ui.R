
# starting August 6 2019
# An app to draw different plots using the metabolomic data 
# input <- axises and conditional variable
# Allow title, axis, color changes



packages <- c('shiny', 'DT', 'RCurl', 'openxlsx', 'dplyr', 'tidyr', 
                   'shinydashboard', 'ggplot2', 'plotly')
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})

library(shiny)
library(DT)
library(RCurl)
# library(openxlsx)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(xlsx)


# reading the data
setwd("/Users/elmira/Box/Projects/Nataliia Kovalchuk") 
raw.data <- xlsx::read.xlsx("Data 060319 Copy of NCCU160_Gyamfi-Mouse-Metabolomics-Results_2019-05-29.xlsx",
                      sheetIndex = 1,rowIndex=4:184)

t.data <-data.frame(t(raw.data)) 
rownames(t.data) <- NULL
colnames(t.data) <- data.frame(lapply(t.data[1,], as.character), stringsAsFactors=FALSE)

wide.dt <- t.data %>% slice(-1)
colnames(wide.dt)[4] <- "Sample Name"
colnames(wide.dt) <- gsub("-","_",colnames(wide.dt))

wide.dt <- data.frame(lapply(wide.dt, droplevels))
wide.dt <- data.frame(lapply(wide.dt,as.character),stringsAsFactors=FALSE)
wide.dt <- cbind(wide.dt[,1:4], data.frame(lapply(wide.dt[,5:176], as.numeric)))


##### UI #####


ui <- dashboardPage(
  
  dashboardHeader(title=span("Metabolomic Data Plot Constructor",style = "font-size: 18px"),
                  titleWidth = 325,
                  tags$li(a(img(src = 'StatLab RGB_CMYK_Alt.png',
                                title = "Logo", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown")),
  
  dashboardSidebar(width = 325,
    
    selectInput(inputId = "yvar", 
                label = "Y-Axis Variable:",
                choices = colnames(wide.dt)[5:176],
                multiple = FALSE),
    
    # radioButtons(inputId = "yvar.log",
    #              label = "Log tranformed Y-Axis Variable",
    #              choices = c("Yes", "No"),
    #              selected = "No"),
    
    selectInput(inputId = "xvar", 
                label = "X-Axis Variable:",
                choices = c("Mouse.Type","Drug","Gender"),
                selected = "Drug",
                multiple = FALSE),
    
    selectInput(inputId = "catvar", 
                label = "Categorized by:",
                choices = c("","Mouse.Type","Drug","Gender"),
                # selected = "Mouse.Type",
                multiple = FALSE), 
    
    selectInput(inputId = "facetvar", 
                label = "Faceting variable:",
                choices = c("","Mouse.Type","Drug","Gender"),
                # selected = "Gender",
                multiple = FALSE), 
    
    selectInput(inputId = "plot_parameters", 
                label = "Plot Parameters:",
                choices = c("Title", 
                            "Axis Text Size",
                            "Axis Title Size",
                            "Title Size"),
                multiple = FALSE),
    
    conditionalPanel(
      condition = "input.plot_parameters == 'Title'",
      textInput(inputId = "plot_title",
                label = "Plot Title:")),

    conditionalPanel(
      condition = "input.plot_parameters == 'Title Size'",
      textInput(inputId = "title_size",
                label = "Title Size",
                value = "20")),
    conditionalPanel(
      condition = "input.plot_parameters == 'Axis Text Size'",
      textInput(inputId = "axis_text_size",
                label = "Axis Text Size",
                value = "12")),
    conditionalPanel(
      condition = "input.plot_parameters == 'Axis Title Size'",
      textInput(inputId = "axis_label_size",
                label = "Axis Title Size",
                value = "12")),
    
    br(),
    div(style="display:inline-block;width:90%;text-align: center;",
        downloadButton("downloadPlot", label = "Download Plot")), # allows me to place download button closer to center of sidebar
    tags$style(".skin-blue .sidebar a { color: #444; }") # Makes the download button not grayed out
    
  ),
  
  
  dashboardBody(
    
    tags$head(tags$style(HTML('
                                /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #0C234B;
                              }
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #AB0520;
                              }
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #0C234B;
                              }
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #AB0520;
                              }
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #AB0520;
                              }
                              /* toggle button when hovered  */
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #AB0520;
                              }'))),
    
    
    fluidRow(
      box(title = "Plot Information:", 
          width = 12,length=12,
          textOutput("id_message"),
          helpText(paste0("Notes:  The boxplot below shows the distribution of the requested Y variable in different levels of the requested X factor. ",
                          "In order to create grouped boxplot using another factor, you can choose a categorizing factor from the menu on the left. "  ,
                          "The faceting variable, gives you an option to have separated plots for each level of the chosen facet variable. " ,
                          "You can customize some aspects of this plot by using the Plot Parameters menu. "
          )))),
    fluidRow(
      box(title = "Boxplot of compounds for different factors",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          plotOutput("plot", height = 300))))
)




##################### SERVER ######################
server <- function(input, output) {
  
  # data.plot <- reactive({
  #   data <- HIV.Aging.dt
  #   if(input$yvar.log == "Yes"){
  #     n.zero <- length(data[data[,input$yvar]==0,input$yvar])
  #     if(n.zero >0){
  #       data[,input$yvar] = log(data[,input$yvar]+0.0001)
  #     }else{data[,input$yvar]=log(data[,input$yvar])
  #     }
  #   }
  #   data
  # })
  
  
  plot <- reactive({
    
    # dt <- data.plot()
    dt <- wide.dt
  
    if(input$catvar!=""){

      # if(input$ylab==""){ 
      #   if(input$yvar.log == "Yes"){p.ylab=paste0("log (",as.character(input$yvar),")")}
      #   else{p.ylab=as.character(input$yvar)}}
      # else{p.ylab=input$ylab}
      
      p <- ggplot(aes_string(y = input$yvar, x = input$xvar, 
                             fill = input$catvar), data = wide.dt) +
        geom_boxplot() + geom_point(aes_string(shape=input$catvar, colour=input$catvar))
      
      if(input$facetvar !=""){
        p <- p + facet_wrap(~as.factor(eval(parse(text=input$facetvar))))
      }
      
      p <- p + 
        ggtitle(input$plot_title) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, size=input$title_size),
              axis.text = element_text(size = input$axis_text_size),
              axis.title = element_text(size = input$axis_label_size),
              # panel.border = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.text=element_text(size=10))
      
      }else{
      
        p <- ggplot(aes_string(y = input$yvar, x = input$xvar, color=input$xvar), data = wide.dt) +
          geom_boxplot() + geom_point()
        
        if(input$facetvar !=""){
          p <- p + facet_wrap(~as.factor(eval(parse(text=input$facetvar))))
        }
        
        p <- p + 
          ggtitle(input$plot_title) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size=input$title_size),
                axis.text = element_text(size = input$axis_text_size),
                axis.title = element_text(size = input$axis_label_size),
                # panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.text=element_text(size=10))
      }
    p
    })
        
        
        
    output$plot <- renderPlot(plot())
    
    
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste0('boxplot_', format(Sys.time(), '%Y-%m-%d_%H%M%S'), '.png') },
      content = function(file) {
        ggsave(file, plot = plot(), device = "png",
               width = 5, height = 4, units = "in",
               dpi = 500)
      }
    )
    

}

shinyApp(ui, server)