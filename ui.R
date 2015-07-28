## This App is developed by Yan Li for requested project (CRI-BIO-300), last update on August, 2015

header <- dashboardHeader(
  title = "NGG Hit Exploration App"
)

sidebar <- dashboardSidebar()

body <- dashboardBody()

ui <- dashboardPage(header, sidebar, body, skin = "red")
