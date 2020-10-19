library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(googlesheets4)
library(ggplot2)
library(plotly)
library(padr)
library(lubridate)
library(DT)


#gs4_auth(token = "appkey.rds")
gs4_auth(cache = ".secrets",email = "joe@algorit.ma")
piket_id <- "1OMT9kAU_ceoku4M44VZWqiA1al9tKpJzpN6rfY5hLzY"

mentoring <- read_sheet(piket_id,sheet="Mentoring")
mentoring <- mentoring %>% 
  mutate(`Date Created` = dmy_hm(`Date Created`))
timeline <- read_sheet(piket_id,sheet="Timeline")
member <- read_sheet(piket_id,sheet="Active_Mentor")
member <- member %>%
  filter(is_active == "y") %>% 
  mutate(desc = ifelse(is_lead == "y","Team Leader",""))


prd <- mentoring %>% 
  filter(`Date Created` == max(`Date Created`)) %>% 
  pull(Period)
datep <- mentoring %>% 
  filter(Period == prd)
datep <- range(date(datep$`Date Created`))

tim_a <- c("Arga","Fafil","Handoyo","Ina","Nabilla","Devin")
tim_b <- c("Joe","Wulan","Tomy","Sitta","Iqbal","Lita","Devin")



# Run App
source("ui.R")
source("server.R")
shinyApp(ui,server)