library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(data.table)
library(readr)
library(tidyverse)
library(shinyWidgets)
library(gridExtra)
library(shinythemes)


dubai_house_transaction <- read.csv("Transactions.csv")
dubai_house_transaction <- dubai_house_transaction[,c(1,3:5,7,9,11:19,21)]
dubai_house_transaction$instance_date <- as.Date(dubai_house_transaction$instance_date, "%d-%m-%Y")
dubai_house_transaction$month_yr <- format(dubai_house_transaction$instance_date, "%m-%Y")
dubai_house_transaction <- subset(dubai_house_transaction , procedure_area <= 1000000)


areas_dubai <- dubai_house_transaction %>%
    select(area_name_en) %>%
    unique()

property_type <- dubai_house_transaction %>%
    select(property_type_en) %>%
    unique()
property_type <- c(property_type$property_type_en)

property_subtype <- dubai_house_transaction %>%
    select(property_sub_type_en) %>%
    unique()
property_subtype <- c(property_subtype$property_sub_type_en)

procedure_name <- dubai_house_transaction %>%
    select(procedure_name_en) %>%
    unique()

reg_type <- dubai_house_transaction %>%
    select(reg_type_en) %>%
    unique()


require(data.table)
setDT(dubai_house_transaction)
require(magrittr)
colnames(dubai_house_transaction) =  colnames(dubai_house_transaction) %>% 
    gsub("X","",.) %>%                
    gsub("\\.","/",.)            

ui <- fluidPage(
    theme = shinytheme("superhero"),

    titlePanel("Dubai Real Estate Transactions - Buy a house in the city of your dreams!", windowTitle = "Dubai Real Estate"),
    sidebarLayout( 
        
        column(2, wellPanel(
        selectInput("area", 
                    label = "Choose an area in Dubai:",
                    choices = areas_dubai,
                    selected = "Al Thanayah Fourth"
        ),
        br(),
        checkboxGroupInput("property_type", 
                           label = "Choose a property type:", 
                           choices = property_type,
                           selected = property_type
        ),
        br(),
        checkboxGroupInput("property_subtype", 
                           label = "Choose a property subtype:", 
                           choices = property_subtype,
                           selected = property_subtype
        ),
        br(),
        radioButtons("procedure_name", 
                           label = "Choose a procedure:", 
                           choices = procedure_name$procedure_name_en,
                           selected = "Sales"
        ),
        br(),
        radioButtons("reg_type", 
                     label = "Choose a type:", 
                     choices = reg_type$reg_type_en,
                     selected = "Existing Properties"
        ),
        br(),
        dateRangeInput("daterange",
                       label = 'Date range input: yyyy-mm-dd',
                       start = as.Date(min(dubai_house_transaction$instance_date, na.rm = TRUE), format = "%m/%d/%Y"),
                       end = as.Date(max(dubai_house_transaction$instance_date, na.rm = TRUE), format = "%m/%d/%Y"), 
                       min = as.Date(min(dubai_house_transaction$instance_date, na.rm = TRUE), format = "%m/%d/%Y"),
                       max = as.Date(max(dubai_house_transaction$instance_date, na.rm = TRUE), format = "%m/%d/%Y")
        ),
        br(),
        sliderInput("meter2range",
                       label = 'Meter square',
                       min = min(dubai_house_transaction$procedure_area, na.rm = TRUE),
                       max = max(dubai_house_transaction$procedure_area, na.rm = TRUE),
                       value = c(500, 10000),   
                       step = 100    
        ),
        )),
        
        mainPanel(
                  h3("You have selected these parameters:"),
                  fluidRow(
                  column(4,
                  textOutput("area"),
                  textOutput("property_type"),
                  textOutput("property_subtype"),
                  textOutput("procedure_name"),
                  textOutput("reg_type"),
                  textOutput("daterange"),
                  textOutput("meter2range"),
                  textOutput("transactions"),
                  span(textOutput("warning"),style="color:red")),
                  column(8,
                         img(src = "dubai.png", width=650, align = "right")),
                  ),
                  br(),
                  br(),
                  plotOutput("graph")
                  )
    )
)

server <- function(input, output) {
    
    output$area <- renderText({ 
        paste("Neighbourhood:", input$area)
    })
    output$property_type <- renderText({ 
        paste(c("Property types:", (input$property_type)), collapse="\t")
    })
    output$property_subtype <- renderText({ 
        paste(c("Sub property types:", (input$property_subtype)), collapse="\t")
    })
    output$procedure_name <- renderText({ 
        paste("Procedure:", input$procedure_name)
    })
    output$reg_type <- renderText({ 
        paste("Type:", input$reg_type)
    })
    output$daterange <- renderText({ 
        paste("Period from:", input$daterange[1], "to", input$daterange[2])
    })
    output$meter2range <- renderText({ 
        paste("Surface from:", input$meter2range[1], "m^2 to", input$meter2range[2], "m^2")
    })
    output$graph <- renderPlot({
        dubai_plot <- subset(dubai_house_transaction, 
                           instance_date >= input$daterange[1] & instance_date <= input$daterange[2]
                         & procedure_area >= input$meter2range[1] & procedure_area <= input$meter2range[2]
                         & area_name_en ==input$area
                         & procedure_name_en == input$procedure_name
                         & reg_type_en == input$reg_type
                         & property_type_en %in% input$property_type
                         & property_sub_type_en %in% input$property_subtype
                         )
                        transactions <- nrow(dubai_plot)
                        p1 <- ggplot(dubai_plot,
                                aes(x= instance_date, y = actual_worth)) +
                                geom_line(data = dubai_plot[`area_name_en`==input$area], color = 'red', size = 1) +
                                ggtitle("Amount sold each day in Dubai in euros") +
                                xlab("Year") +
                                ylab("Price") +
                                theme_bw() +
                                theme(plot.title = element_text(lineheight=.8, face="bold", color='red', hjust=0.5))
                        p2 <- ggplot(dubai_plot,
                               aes(x= instance_date, y = meter_sale_price)) +
                               geom_line(data = dubai_plot[`area_name_en`==input$area], color = 'blue', size = 1) +
                               ggtitle("Amount per meter squared sold each day in Dubai in euros") +
                               xlab("Year") +
                               ylab("Price per m^2") +
                               theme_bw() +
                               theme(plot.title = element_text(lineheight=.8, face="bold", color='blue', hjust=0.5))
                        grid.arrange(p1,p2, nrow=2)
                        
    }, height=650, width=1200)
    output$transactions <- renderText({ 
        dubai_plot <- subset(dubai_house_transaction, 
                             instance_date >= input$daterange[1] & instance_date <= input$daterange[2]
                             & procedure_area >= input$meter2range[1] & procedure_area <= input$meter2range[2]
                             & area_name_en ==input$area
                             & procedure_name_en == input$procedure_name
                             & reg_type_en == input$reg_type
                             & property_type_en %in% input$property_type
                             & property_sub_type_en %in% input$property_subtype
        )
        paste("Number of transactions considered:", nrow(dubai_plot))
    })
    output$warning <- renderText({ 
        dubai_plot <- subset(dubai_house_transaction, 
                             instance_date >= input$daterange[1] & instance_date <= input$daterange[2]
                             & procedure_area >= input$meter2range[1] & procedure_area <= input$meter2range[2]
                             & area_name_en ==input$area
                             & procedure_name_en == input$procedure_name
                             & reg_type_en == input$reg_type
                             & property_type_en %in% input$property_type
                             & property_sub_type_en %in% input$property_subtype
        )
        if(nrow(dubai_plot)==0){print("Warning! Your research doesn't correspond to any result")}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
