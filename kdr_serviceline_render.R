library(quarto)
library(tidyverse)

load("C:/Users/4477078/OneDrive - Moffitt Cancer Center/Key Drivers//kdr_serviceline/data/op_data.Rdata")

# x <- "ON" #set service line
# 
# # y <- "O7"
# if (x =='ON'){
#   y <- "O3" #Outpatient Oncology
# } else {
#   y <- "O4" # Virtual Health
# } 


##Running kdr reports for those providers who meet the specified criteria

surveys <- op_data %>%
  filter(varname == "O3") %>%
  group_by(survey_type) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n())

print(surveys)
  
t<- nrow(surveys)

reasons <- surveys %>%
  pull(survey_type) %>%
  as.character()

reports<-
  tibble(
    input="kdr_serviceline.qmd",
    output_file = str_glue("{reasons}.html"),
    execute_params=map(reasons,~list(survey_type=.))
  )

reports<-reports%>%
  slice(1:t)

pwalk(reports,quarto_render)