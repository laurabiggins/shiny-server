source("R/utilities.r")
library(shiny)
library(tidyverse)
library(plotly)

tt_processed <- readRDS("data/tt_processed.rds")
# this code only needs to be run once
all_unique_names <- tt_processed %>%
                        select(Name) %>%
                        distinct() %>%
                        pull()

ui <- fluidPage(

    tags$style("#highlight_checkbox {
                font-size:20px
                }"),
    
    tags$style("input[type=checkbox] {
                    transform: scale(2);
                    margin-top:10px
                }"),
    
    tags$style("input[type=radio] {
                    transform: scale(2);
                }"),

    # Application title
    titlePanel("TT"),

    sidebarLayout(
        sidebarPanel(
            br(),
            br(),
            selectInput(inputId = "select_name", label = "Select name", 
                        choices = all_unique_names, selected = "Roald Dahl", multiple = FALSE),
            br(),
            br(),
            htmlOutput("advice")
        ),

        mainPanel(

            plotlyOutput("scatterplot", height = 500),
            fluidRow(
                column(1),
                div(id="highlight_checkbox",
                    column(5,
                       checkboxInput(inputId = "highlight", label = " highlight chosen character", value = FALSE)
                    )   
                ),
                column(3, 
                       radioButtons(inputId = "scatter_value", label = "", choices = c("actual value", "percentage"))
                )
            ),
            br(),
            br(),
            br(),
            plotlyOutput("barplot", height = 300),
            radioButtons(inputId = "bar_value", label = "", choices = c("actual value", "percentage"))
        )
    )
)

server <- function(input, output) {

    observeEvent(input$select_name, {
        rv$advice = get_advice(input$select_name, tt_processed)
        rv$dataset <- mutate(rv$dataset, colour_col = if_else(Name == input$select_name, 1, 2))
    })
    
    output$advice <- renderText(rv$advice)
    
    rv <- reactiveValues(
        scatter_y = "value", 
        advice = "", 
        bar_value = "value", 
        dataset = tt_processed, 
        colour_option = "characteristic"
    )
    
    observeEvent(input$highlight, {
        
        if(input$highlight){
            rv$colour_option <- "colour_col"
            
        } else{
            rv$colour_option = "characteristic"
        }    
    })
    
    observeEvent(input$scatter_value, {
        
        type <- input$scatter_value
        
        if(type == "actual value"){
            rv$scatter_y <- "value"
            print("changing to value\n")
        } else if (type == "percentage"){
            rv$scatter_y <- "percentage_rank"
            cat("changing to percentage\n")
        } 
    })
    
    observeEvent(input$bar_value, {
        
        type <- input$bar_value
        
        if(type == "actual value"){
            rv$bar_value <- "value"
        } else if (type == "percentage"){
            rv$bar_value <- "percentage_rank"
        } 
    })
    
    # update_selected_character <- reactive({
    #     rv$dataset <- mutate(rv$dataset, colour_col = if_else(Name == input$select_name, 1, 2))
    # })
    
    output$scatterplot <- renderPlotly({
     
        set.seed(1)
        
        scatter_plot_all_data <- rv$dataset %>%
            ggplot(aes(x = characteristic, y = get(rv$scatter_y), text = Name, color = get(rv$colour_option))) +
            geom_jitter(width = 0.2, height = 0, size = 2.5) +
            xlab("") +
            ylab("") +
            theme(legend.position = "none")
        
        ggplotly(scatter_plot_all_data, tooltip = "text", width = 500, height = 500)
    })
    
    output$barplot <- renderPlotly({
        
        bar_type <- rv$bar_value
        
        p <- tt_processed %>%
            filter(Name == input$select_name) %>%
            ggplot(aes(x = reorder(characteristic, get(bar_type)), y = get(bar_type), text = get(bar_type))) +
            geom_bar(stat = "identity") +
            xlab("") +
            coord_flip() +
            theme(axis.text = element_text(size = 14))
        
        ggplotly(p, tooltip = "text", width = 500, height = 300)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


