
library(shiny)
library(shinydashboard)
library(leaflet)

{
    setwd("Rfunctions")
    for (file in list.files()){
        source(file)
    }
    setwd("../rds")
    for (file in list.files()){
        load(file)
    }
    setwd("../")
}

strat_choices <- c("Health Center","Age","Sex","Race","Ethnicity","Language","Insurance Class","Alcohol Use Disorder","Depression Diagnosis","Tobacco Use")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "APCA Data Portal"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "tab1"),
            menuItem("Covid-19 Infections", tabName = "tab2"),
            menuItem("Covid-19 Vaccinations", tabName = "tab3"),
            menuItem("Telehealth Utilization", tabName = "tab4")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "tab1",
                h3("Welcome to the Alaska Primary Care Associations Data Portal"),
                m <- leaflet() %>%
                    addTiles() %>%
                    addMarkers(data = mapData, lng = ~lon, lat = ~lat, popup = ~center),
                url_1 <- a("APCA Website", href="https://alaskapca.org/"),
                url_2 <- a("APCA GitHub Page", href="https://github.com/AlaskaPrimaryCareStatistics")
            ),
            tabItem(
                tabName = "tab2",
                div(
                    style="padding: 10px; font-size:20px;",
                    strong("About:")
                ),
                div(
                    style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                    div(
                        style="font-size:20px; padding: 0px; ",
                        strong("Stratify By:")
                    ),
                    splitLayout(
                        radioButtons(
                            inputId = "str_cov",
                            label = NULL,
                            choices = strat_choices,
                            inline = T
                        )
                    )
                ),
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("30%", "39.3%", "30%"),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Y Axis Limits")
                        ),
                        div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput(
                                inputId = "y_cov",
                                min = 0, 
                                max = 1,
                                value = c(0, 0.1),
                                label = NULL,
                                width="100%"
                            )
                        )
                    ),
                    tags$div(
                        style="text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Date Range:")
                        ),
                        tags$div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput("dr_cov",
                                        label=NULL,
                                        min = min(data_covid$date),
                                        max = max(data_covid$date),
                                        value = c(min(data_covid$date), max(data_covid$date)),
                                        timeFormat = "%m/%y",
                                        width="100%")
                        ),
                    ),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color:rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 15px;",
                            strong("Estimate Type:")
                        ),
                        div(
                            radioButtons(
                                inputId = "ci_cov",
                                label = NULL,
                                choices = c("95% Confidence Interval", "Point Estimate"),
                                inline = F,
                                width = "100%"
                            )
                        )
                    )
                ),
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p21"
                    )
                ),
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p22"
                    )
                )
            ),
            tabItem(
                tabName = "tab3",
                div(
                    style="padding: 10px; font-size:20px;",
                    strong("About:")
                ),
                div(
                    style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                    div(
                        style="font-size:20px; padding: 0px; ",
                        strong("Stratify By:")
                    ),
                    splitLayout(
                        radioButtons(
                            inputId = "str_vac",
                            label = NULL,
                            choices = strat_choices,
                            inline = T
                        )
                    )
                ),
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("30%", "39.3%", "30%"),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Y Axis Limits")
                        ),
                        div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput(
                                inputId = "y_vac",
                                min = 0, 
                                max = 1,
                                value = c(0, 0.4),
                                label = NULL,
                                width="100%"
                            )
                        )
                    ),
                    tags$div(
                        style="text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Date Range:")
                        ),
                        tags$div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput("dr_vac",
                                        label=NULL,
                                        min = min(data_vaccination$date),
                                        max = max(data_vaccination$date),
                                        value = c(min(data_vaccination$date), max(data_vaccination$date)),
                                        timeFormat = "%m/%y",
                                        width="100%")
                        ),
                    ),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color:rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 15px;",
                            strong("Estimate Type:")
                        ),
                        div(
                            radioButtons(
                                inputId = "ci_vac",
                                label = NULL,
                                choices = c("95% Confidence Interval", "Point Estimate"),
                                inline = F,
                                width = "100%"
                            )
                        )
                    )
                ),
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p31"
                    )
                ),
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p32"
                    )
                )
            ),
            tabItem(
                tabName = "tab4",
                div(
                    style="padding: 10px; font-size:20px;",
                    strong("About:")
                ),
                div(
                    style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                    div(
                        style="font-size:20px; padding: 0px; ",
                        strong("Stratify By:")
                    ),
                    splitLayout(
                        radioButtons(
                            inputId = "str_tel",
                            label = NULL,
                            choices = strat_choices,
                            inline = T
                        )
                    )
                ),
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("30%", "39.3%", "30%"),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Y Axis Limits")
                        ),
                        div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput(
                                inputId = "y_tel",
                                min = 0, 
                                max = 1,
                                value = c(0, 0.8),
                                label = NULL,
                                width="100%"
                            )
                        )
                    ),
                    tags$div(
                        style="text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Date Range:")
                        ),
                        tags$div(
                            style="padding-left: 10px; padding-right: 10px;",
                            sliderInput("dr_tel",
                                        label=NULL,
                                        min = min(data_telehealth$date),
                                        max = max(data_telehealth$date),
                                        value = c(min(data_telehealth$date), max(data_telehealth$date)),
                                        timeFormat = "%m/%y",
                                        width="100%")
                        ),
                    ),
                    tags$div(
                        style = "text-align: center; border: 1px solid grey; background-color:rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 15px;",
                            strong("Estimate Type:")
                        ),
                        div(
                            radioButtons(
                                inputId = "ci_tel",
                                label = NULL,
                                choices = c("95% Confidence Interval", "Point Estimate"),
                                inline = F,
                                width = "100%"
                            )
                        )
                    )
                ),
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p41"
                    )
                )
            )
        )    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$p21 <- renderPlot(
        {
            see_prop(
                data = data_covid,
                xvar = "time", 
                yvar = "covid",
                ovar = input$str_cov,
                main = "Percentage of Patients Diagnosed with New Case Covid-19 During Encounter",
                xlab = NA,
                ylab = "Percentage",
                dr = input$dr_cov,
                legend = "N",
                lx = 15,
                ly = 1,
                ci = input$ci_cov,
                ylim = input$y_cov
            )
        }
    )
    output$p22 <- renderPlot(
        {
            plot(1:10, 1:10)
        }
    )
    output$p31 <- renderPlot(
        {
            see_prop(
                data = data_vaccination,
                xvar = "time", 
                yvar = "vaccinated",
                ovar = input$str_vac,
                main = "Percentage of Encounters where Patient was Vaccinated by CHC",
                xlab = NA,
                ylab = "Percentage",
                dr = input$dr_vac,
                legend = "N",
                lx = 15,
                ly = 1,
                ci = input$ci_vac,
                ylim = input$y_vac
            )
        }
    )
    output$p32 <- renderPlot(
        {
            plot(1:10, 1:10)
        }
    )
    output$p41 <- renderPlot(
        {
            see_prop(
                data = data_telehealth,
                xvar = "time", 
                yvar = "telehealth",
                ovar = input$str_tel,
                main = "Percentage of Patient Encounters Occurring by Telehealth",
                xlab = NA,
                ylab = "Percentage",
                dr = input$dr_tel,
                legend = "N",
                lx = 15,
                ly = 1,
                ci = input$ci_tel,
                ylim = input$y_tel
            )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
