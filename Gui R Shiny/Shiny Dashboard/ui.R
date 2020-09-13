library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Gui R Syukron"),
    dashboardSidebar(
      sliderInput("bins","Number of Breaks",1,100,50),
      sidebarMenu(
        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
        menuSubItem("Dashboard Finance",tabName = "finance"),
        menuSubItem("Dashboard Sales",tabName = "sales"),
      menuItem("Detailed Analysis"),
      menuItem("Raw Data")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(plotOutput("histogram"))
                )),
        tabItem(tabName = "finance",
                h1("Finance Dashboard")
                ),
        tabItem(tabName = "sales",
                h2("Sales dashboard")
                )
        )
      )
    )
  )
