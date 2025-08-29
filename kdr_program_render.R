library(quarto)
library(tidyverse)

load("G:/Press Ganey II/Reports/Ad Hoc/DEEP DIVE/Key Driver Reports/data/data_072024-052025.Rdata.Rdata")

# x <- "APP"

y <- "O3"

##Running kdr reports for those providers who meet the specified criteria
runners <- data %>%
  filter(service == "ON") %>%
  #filter(type == x ) %>%
  filter(varname == y ) %>%
  group_by(program) %>%
  summarise(tbscore=sum(top_box)/n()*100,n=n())

t<- nrow(runners)

programs <- runners %>%
  pull(program) %>%
  as.character()

reports_01<-
  tibble(
    input="kdr_program.qmd",
    output_file = str_glue("{programs}.html"),
    execute_params=map(programs,~list(program=.))
  )

reports_01<-reports_01%>%
  slice(1:t)

pwalk(reports_01,quarto_render)

