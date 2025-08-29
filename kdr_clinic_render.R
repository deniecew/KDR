
library(quarto)
library(tidyverse)
library(stringr)

load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/alldata_fy25.Rdata")

service_filter <- "OU" #set service line
cutoff_date <- as.Date("2024-12-31")


if (service_filter =='OU'){
  var_filter <- "E4" # Procedural Areas
  } else if (service_filter =='MD') {
  var_filter <- "O4" # Virtual Health
  } else  {
    var_filter <- "O3" # Outpatient Oncology & Inpatient
  } 


##Running kdr reports for those clinics who meet the specified criteria

filtered_data<-alldata%>%
  mutate(unit2 = case_when(
    clinic == "Blood and Marrow Treatment Center" ~ "BMTT",
    clinic == "Cardiology" | clinic == "Pulmonary" ~ "CARDPULM",
    TRUE ~ unit)) %>%
  mutate(clinic2 = case_when(
    clinic == "Cardiology" | clinic == "Pulmonary" ~ "Cardiopulmonary",
    TRUE ~ clinic)) %>%
  mutate(clinic2 = str_replace_all(clinic2, "/", "_"))%>%
  filter(date > cutoff_date) %>% 
  filter(clinic2 != "NA") %>%
  filter(unit2 != "NA") %>%
  filter(service == service_filter) %>%
  filter(varname == var_filter ) %>%
  group_by(clinic2, unit2) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n()) 
  




runners <- filtered_data %>%
  filter(tbscore<100 )

t<- nrow(runners)

units <- runners %>%
  pull(unit2) %>%
  as.character()

clinics <- runners %>%
  pull(clinic2) %>%
  as.character()
  

reports<-
  tibble(
    input="kdr_clinic.qmd",
    output_file = str_glue("{clinics} ({units}).html"),
    execute_params=map(units,~list(unit=.))
  )

reports<-reports%>%
  slice(1:t)

pwalk(reports,quarto_render)


library(quarto)
library(tidyverse)

load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/data_072024-052025.Rdata")

x <- "ON" #set service line

if (x =='ON'){
  y <- "O3" #Outpatient Oncology
  } else {
  y <- "O4" # Virtual Health
  } 


##Running kdr reports for those clinics who meet the specified criteria

allresults <- data %>%
  filter(service == x ) %>%
  filter(varname == y ) %>%
  group_by(clinic, unit) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n())


runners <- data %>%
  filter(service == x ) %>%
  filter(varname == y ) %>%
  group_by(clinic, unit) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n()) %>%
  filter(tbscore<100 )

t<- nrow(runners)

units <- runners %>%
  pull(unit) %>%
  as.character()

clinics <- runners %>%
  pull(clinic) %>%
  as.character()

reports<-
  tibble(
    input="kdr_clinic.qmd",
    output_file = str_glue("{clinics}.html"),
    execute_params=map(units,~list(unit=.))
  )

reports<-reports%>%
  slice(1:t)

pwalk(reports,quarto_render)


