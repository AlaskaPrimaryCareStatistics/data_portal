
#### Load Libraries and Helper Functions #####
{
    library(shiny)
    library(shinydashboard)
    library(leaflet)
    library(dplyr)
    library(mailtoR)
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

##### Global Variables #####
{
    strat_choices <- c("Age","Sex","Race","Ethnicity","Language","Insurance Class","Alcohol Use Disorder","Depression Diagnosis","Tobacco Use")
    org_choices <- c("All", Organizations$ORGANIZATION_NAME)
}

##### Set Up User Interface #####
ui <- dashboardPage(
    
    ### Header ###
    dashboardHeader(title = "APCA Data Portal"),
    
    ### Side Bar ###
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "tab1"),
            menuItem("Covid-19 Infections", tabName = "tab2"),
            menuItem("Covid-19 Vaccinations", tabName = "tab3"),
            menuItem("Telehealth Utilization", tabName = "tab4")
        )
    ),
    
    ### Body ###
    dashboardBody(
        tabItems(
            
            ### Tab 1 --> Landing Page ###
            tabItem(
              
                tabName = "tab1",
                div(
                  style = "text-align: center; margin-bottom:25px;",
                  img(src="apca_logo.png")
                ),
                div(
                  "Since its founding over twenty-five years ago, the APCA has grown to serve twenty-nine Federally Qualified Health Centers and Look Alikes in addition to saftey net providers, and stakeholders across the State of Alaska. Our growning network consists of more that 200 community health centers spread across the State of Alaska. The purpose of APCA's data portal is to provide policy makers, public health officials, stakeholders, and the general public with timely population health statistics that can be used to inform the future of healthcare within the State of Alaska."
                ),
                div(
                  style = "text-align: center; margin-bottom:25px;",
                  h2("Community Health Centers in Our Network:")
                ),
                div(
                  style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                  div(
                    style="font-size:20px; padding: 0px; ",
                    strong("Organization:")
                  ),
                  splitLayout(
                    radioButtons(
                      inputId = "org_choice",
                      label = NULL,
                      choices = org_choices,
                      inline = T
                    )
                  )
                ),
                div(
                  style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                  leafletOutput(
                    outputId = "p1"
                  )
                ),
                splitLayout(
                  style = "padding: 0px; margin-top: 25px; background-color: white;",
                  cellWidths = c("33%", "33%", "33%"),
                  tags$div(
                    style = "text-align: left;",
                    url_1 <- a("APCA Website", href="https://alaskapca.org/")
                  ),
                  tags$div(
                    style = "text-align: center;",
                    url_2 <- a("APCA GitHub Page", href="https://github.com/AlaskaPrimaryCareStatistics")
                  ),
                  tags$div(
                    style = "text-align: right;",
                    mailtoR(email = "jordanB@alaskapca.org",
                            text = "Contact")
                  )
                )
            ),
            ### Tab 2 --> Covid-19 Infections ###
            tabItem(
                tabName = "tab2",
                # Big Button 1#
                div(
                  
                  style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                  div(
                    style="font-size:20px; padding: 0px; ",
                    strong("Visualize:")
                  ),
                  splitLayout(
                    radioButtons(
                      inputId = "surv_cov",
                      label = NULL,
                      choices = c(
                        "Percent of Patients Diagnosed with New Case of Covid-19 During Encounter", 
                        "Percent of Patients without Positive Covid-19 Diagnosis on Record"
                        ),
                      inline = F,
                      width = "100%"
                    )
                  )
                ),
                
                # Big Button 2#
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
                
                # Button Row 2 Panel 1 #
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("49.75%", "49.75%"),
                    tags$div(
                        style="text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Date Range:")
                        ),
                        tags$div(
                            style="padding-left: 10px; padding-right: 40px;",
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
                
                # Plot pannel 1 #
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p2"
                    )
                ),
                tags$a("Click here to download the full report", href="report.pdf")
            ),
            
            ### Tab 3 --> Covid-19 Vaccinations ###
            tabItem(
                tabName = "tab3",
                # Big Button 1#
                div(
                  style = "text-align: center; border: 1px solid grey; padding: 10px; background-color: rgb(211,211,211);",
                  div(
                    style="font-size:20px; padding: 0px; ",
                    strong("Visualize:")
                  ),
                  splitLayout(
                    radioButtons(
                      inputId = "surv_vac",
                      label = NULL,
                      choices = c(
                        "Percent of Patients Receiving Covid-19 Vaccination During Encounter", 
                        "Percent of Patients without CHC Administered Covid-19 Vaccine "
                      ),
                      inline = F,
                      width = "100%"
                    )
                  )
                ),
                
                # Big Button Panel 1#
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
                
                # Button Row 2 Panel 1#
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("49.75%", "49.75%"),
                    tags$div(
                        style="text-align: center; border: 1px solid grey; background-color: rgb(211,211,211);",
                        div(
                            style="font-size:20px;  padding-top: 10px; padding-bottom: 0px;",
                            strong("Date Range:")
                        ),
                        tags$div(
                            style="padding-left: 10px; padding-right: 40px;",
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
                # Plotting output #
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p3"
                    )
                )
                ,tags$a("Click here to download the full report", href="report.pdf")
            ),
            ### Tab 4 --> Covid-19 Vaccinations ###
            tabItem(
                tabName = "tab4",
                # Big Button #
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
                # Button Row 2 #
                style = "padding: 0px;",
                splitLayout(
                    style = "padding: 0px;",
                    cellWidths = c("30%", "39.2%", "30%"),
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
                                max = 100,
                                value = c(0, 80),
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
                
                # Plotting Output #
                div(
                    style = "border: 1px solid grey; padding:20px; background-color: rgb(255,255,255);",
                    plotOutput(
                        outputId = "p4"
                    )
                )
                ,
                tags$a("Click here to download the full report", href="report.pdf")
            )
        )    
    )
)

##### SERVER LOGIC #####
server <- function(input, output) {
  
    # Landing page map #
    output$p1 <- renderLeaflet(
      {
        if (input$org_choice == "All"){
          mapData <- Sites
        }else{
          org_id <- Organizations$ORGANIZATION_ID[Organizations$ORGANIZATION_NAME==input$org_choice]
          mapData <- Sites[Sites$ORGANIZATION_ID==org_id,]
        }
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = mapData, lng = ~LONGITUDE, lat = ~LATITUDE, popup = ~SITE_NAME)
      }
    )
    
    
    # tab 2 --> Covid --> plot 1 #
    output$p2 <- renderPlot(
        {
          if (input$surv_cov=="Percent of Patients Diagnosed with New Case of Covid-19 During Encounter"){
            see_prop(
              data = data_covid,
              xvar = "time", 
              yvar = "covid",
              ovar = input$str_cov,
              main = input$surv_cov,
              xlab = NA,
              ylab = "Percentage",
              dr = input$dr_cov,
              legend = "N",
              lx = 15,
              ly = 1,
              ci = input$ci_cov,
              ylim = c(0,0.1)*100
            )
          }else{
            
            survival_plot(
              data = data_covid,
              xvar = "time",
              yvar = "covid",
              ovar = input$str_cov,
              dr = input$dr_cov, 
              ylim = c(0.9,1)*100,
              input$ci_cov,
              main = input$surv_cov
            )
          }
        }
    )
    
    # tab 3 --> Vaccination --> plot 1 #
    output$p3 <- renderPlot(
        {
          if (input$surv_vac=="Percent of Patients Receiving Covid-19 Vaccination During Encounter"){
            see_prop(
                data = data_vaccination,
                xvar = "time", 
                yvar = "vaccinated",
                ovar = input$str_vac,
                main = input$surv_vac,
                xlab = NA,
                ylab = "Percentage",
                dr = input$dr_vac,
                legend = "N",
                lx = 15,
                ly = 1,
                ci = input$ci_vac,
                ylim = c(0,0.3)*100
            )
          }else{
              survival_plot(
                data = data_vaccination,
                xvar = "time",
                yvar = "vaccinated",
                ovar = input$str_vac,
                dr = input$dr_vac, 
                ylim = c(0.6,1)*100,
                ci = input$ci_vac,
                main = input$surv_vac
              )
          }
        }
    )
    
    # tab 4 --> Telehealth --> plot 1 #
    output$p4 <- renderPlot(
        {
            see_prop(
                data = data_telehealth,
                xvar = "time", 
                yvar = "telehealth",
                ovar = input$str_tel,
                main = "Percentage of Patient Encounters in Occurring by Telehealth",
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
