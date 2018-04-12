library(shiny)
library(xml2)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(DT)
organisation_list<-c("ASD001","ASD002","ASD003","ASD004","ASD005","ASD006","ASD007","ASD008","ASD009","ASD010","ASD011","ASD012","ASD013","ASD014","ASD015","ASD016","ASD017","ASD018","ASD019")

website<-"https://procxed.scotxed.net/procxedwebservice/ProcXedDataReturn.asmx/GetForthcomingPublications?OrganisationIDList="

#slow loop - can be improved

df <- data.frame(
  publication=character(),
                 date = character(),
                 location = character(),
                 theme = character()
                 )

for (organisation in organisation_list) {
  xml_data <- read_xml(paste0(website,organisation))
  xml_series <- xml_find_all(xml_data, ".//Series")
  publication<-xml_text(xml_find_first(xml_series, ".//SeriesName"))
  
  
  for (i in 1:length(xml_series)) {
    #  print(publication[i])
    editions<-xml_find_all(xml_series[[i]], ".//Editions")
    location<-xml_text(xml_find_all(editions, ".//UrlAddress"))
    theme<-xml_text(xml_find_all(editions, ".//Theme"))
    publication_dates<-xml_text(xml_find_all(editions, ".//PublicationFullDate"))
    
    for (x in 1:length(editions)){
      if (publication_dates[x] != "") {
        row = data.frame(publication[i],publication_dates[x],location[x],theme[x],stringsAsFactors=FALSE)
        df<-rbind(df,row)
      }
      if (as.character(Sys.Date()) == publication_dates[x]) {
        print(
          paste(publication[i],"is published today:", location[x])
        )
      } else {
        #do something else
        #      print(
        #        paste(publication[i],"is published today:", location[x])
        #      )
      }
    } 
  }
}
df2<- df%>%
  rename(publication = publication.i.,
         date=publication_dates.x.,
         theme=theme.x.,
         location=location.x.
  ) %>%
  mutate(new_date = ymd(date)) %>%
  mutate(new_date2= if_else(is.na(new_date),
                            dmy(paste0("01",date)),
                            new_date)) %>%
  mutate(location = paste0("<a href=",location,">link</a>"))%>%
  arrange(new_date2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
   
   # Application title
  dashboardHeader(title="Upcoming Publications"),
   
   # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    disable = TRUE,
    collapsed = TRUE,
    sidebarMenu(
      disable = TRUE,
      collapsed = TRUE
#         sliderInput("time",
#                     "Date of release",
#                     min = Sys.Date(),
#                     max = Sys.Date()+365,
#                     value = Sys.Date()+365)
      )
)
,
      
      # Show a plot of the generated distribution
  dashboardBody(
         dataTableOutput("table")
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$table <- renderDataTable(df2 %>%
                                 filter(new_date2 >= Sys.Date()) %>%
                                 select(publication,
                                        theme,
                                        date,
                                        location),
                              escape = FALSE
                               )
}

# Run the application 
shinyApp(ui = ui, server = server)
