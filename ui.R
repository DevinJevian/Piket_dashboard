# ui
ui <- dashboardPage(title = "Piket Dashboard",skin = "black", 
                    dashboardHeader(title = "Piket Dashboard"),
                    dashboardSidebar(
                        sidebarMenu(id = "tabs",
                                    menuItem("Overview",tabName = "sum"),
                                    menuItem("Timeline",tabName = "time"),
                                    menuItem("Clasroom",tabName = "classr"))
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML(".box.box-info {border-top-color: #d63e2d;}"),
                                           HTML("#member_act li {width: 12%;}"))),
                        tabItems(
                            # Menu Overview
                            tabItem(tabName = "sum",
                                    fluidPage(
                                      fluidRow(uiOutput("member_act")),
                                      div(align = "center",
                                          fluidRow(dateRangeInput("filterer",label = "Select Observation Range",
                                                                  start = datep[1],end = datep[2],width = "80%"))),
                                      fluidRow(
                                          box(width = 6,title = "Mentoring Request per Class",status = "info",
                                              plotlyOutput("plot1")),
                                          box(width = 6,title = "Count of Material",status = "info",
                                              plotlyOutput("plot3"))
                                        ),
                                        fluidRow(
                                          box(title = "Mentoring Request per Mentor",status = "info",width = 12,
                                              plotlyOutput("plot2"))
                                        ),
                                      fluidRow(
                                          box(width = 12,title = "Count of Mentoring by Day",status = "info",
                                              dateRangeInput("date_4",label = "Select date",start = datep[1],end = datep[2]),
                                              #selectInput(inputId = "sel_4",label = "Select Periode",choices = unique(mentoring$Period),selected = prd),
                                              plotlyOutput("plot4"))
                                        ),
                                      fluidRow(
                                        box(width = 12,title = "Student Request",status = "info",
                                            dataTableOutput("dt1"))
                                      )
                                    )),
                            # Menu Timeline
                            tabItem(tabName = "time",
                                    fluidPage(
                                      box(width = 12,title = "Team Work Schedule",status = "info",
                                            dateInput("date_sch",label = "Select Date",width = "25%"),
                                            radioButtons("team_sch",label = "Select Team",choices = c("A","B"),inline = T),
                                          plotlyOutput("plot5")
                                          )
                                    )),
                            # Menu Clasroom
                            tabItem(tabName = "classr",
                                    fluidPage(
                                      div(align="center",tags$img(src="https://images-na.ssl-images-amazon.com/images/I/71UgwaC4DjL._SL1500_.jpg",
                                                                  height = 500, width = 1000))
                                    ))
                        )
                        )
                    
                    )


