library(shiny)
library(tidyverse)


census_pop_est_2018<-read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into=c("dot","state"), extra="merge") %>% 
  select(-dot)

recentcovid<-covid19 %>% 
  mutate(state_name=str_to_lower(state)) %>% 
  left_join(census_pop_est_2018,
            by=c("state")) %>% 
  mutate(cases_per_1000=(cases/est_pop_2018)*10000)

ui<-fluidPage(
  sliderInput(inputId = "date",
              label="Date",
              min=as.Date("2020-01-21"),
              max=as.Date("2022-03-30"),
              value=c(as.Date("2020-01-21"),
                      as.Date("2022-03-30"))),
  selectInput(inputId = "date",
              label="Choose your state",
              choices=covid19 %>% 
                arrange(state) %>% 
                distinct(state) %>% 
                pull(state),
              multiple=TRUE),
  submitButton(text="View states"),
  plotOutput(outputId = "timeplot"))

server<-function(input,output){
  output$timeplot<-renderPlot({
    recentcovid %>% 
      filter(state %in% input$state) %>% 
      ggplot()+
      geom_line(aes(x=date,
                    y=cases_per_1000,
                    color=state))+
      scale_x_date(limits=input$date)
  })
}
shinyApp(ui=ui, server=server)