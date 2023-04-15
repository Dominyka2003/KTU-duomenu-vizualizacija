library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "467300: Medienos, statybinių medžiagų ir sanitarinių įrenginių didmeninė prekyba"),
                    
                    dashboardSidebar( 
                      
                      selectizeInput(inputId = "imones_pavadinimas", label = "Imones pavadinimas", choices = NULL),
                      
                      sidebarMenu(
                        menuItem("Pagrindine informacija", icon = icon("chart-line"), tabName = "Pagrindine_informacija"),
                        menuItem("Lentele", icon = icon("list-ul"), tabName = "Lentele")
                      )
                      
                    ),
                    
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName = "Pagrindine_informacija", h2("Pagrindine informacija"),
                                fluidRow(
                                  valueBoxOutput("mokesciai", width = 4),
                                  valueBoxOutput("atlyginimas", width = 4),
                                  valueBoxOutput("kodas", width = 4),
                                  box(title = "Apdraustu darbuotoju skaicius", width = 12, solidHeader = TRUE, collapsible = TRUE, plotOutput("plot", height = 350)),
                                  box(title = "Vidutinio atlyginimo dinamika", width = 12, solidHeader = TRUE, collapsible = TRUE, plotOutput("plot1", height = 350))
                                )
                        ),
                        
                        tabItem(tabName = "Lentele", h2("Lentele"),
                                box(title = "Visi duomenys", width = 12, solidHeader = TRUE, collapsible = TRUE, tableOutput("table"))
                        )
                      )
                      
                    )
)

server <- function(input, output, session) {
  
  #Read data
  data <- read_csv("https://raw.githubusercontent.com/Dominyka2003/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")
  # data filtering by ecoActCode and creating month column
  data <- data %>% filter(ecoActCode == 467300) %>%  mutate("month1" = as.numeric(substr(month, 5, 6)))
  
  updateSelectizeInput(session, "imones_pavadinimas", choices = data$name, server = TRUE)
  
  # data atrenkama pagal imones pavadinima, ismetamas menesio stulpelis, pervadinami kiti stulpeliai
  output$table <- renderTable(
    data %>% filter(name == input$imones_pavadinimas) %>% select(-"month1") %>% rename(Kodas = code, Pavadinimas = name, Savivaldybe = municipality, Men = month, EkoVeikPav = ecoActName, EkoVeikKod = ecoActCode, Atlyginimas = avgWage, Apdrausti = numInsured, Mokesciai = tax), digits = 0
  )
  
  #Apdraustuju skaiciaus kitimo vaizdavimas
  output$plot <- renderPlot(
    data %>% filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month1, y = numInsured))+
      geom_col(fill = alpha("orange", 0.4))+
      geom_text(aes(label = numInsured), vjust = -0.5)+
      scale_x_continuous(breaks = 1:12)+
      scale_y_continuous(breaks = pretty_breaks())+
      labs(x = 'Menesiai', y = 'Apdraustuju skaicius')+
      theme_classic()
  )
  
  #Atlyginimu kitimo vaizdavimas
  output$plot1 <- renderPlot(
    data %>% filter(name == input$imones_pavadinimas) %>%
      ggplot(aes(x = month1, y = avgWage))+
      geom_line(color = "blue4", size = 1.1)+
      geom_text(aes(label = avgWage), vjust = -0.5)+
      scale_x_continuous(breaks = 1:12)+
      scale_y_continuous(breaks = pretty_breaks())+
      labs(x = 'Menesiai', y = 'Atlyginimas')+
      theme_classic()
    
  )
  
  #Lenteles, rodancios sumoketus mokescius, kurimas
  output$mokesciai <- renderValueBox({
    a <- data %>%
      filter(name == input$imones_pavadinimas) %>%
      summarise(suma = sum(tax, na.rm = TRUE)) %>%
      round() %>%
      prettyNum(big.mark = ",")
    b <- "Info nera"
    valueBox(
      value = ifelse(a == 0, b, paste(a, "eur")),
      subtitle = "Sumoketi mokesciai",
      color = "olive"
    )
  })
  
  #Lenteles, rodancios vidutini uzmokesti, kurimas
  output$atlyginimas <- renderValueBox({
    c <- data %>%
      filter(name == input$imones_pavadinimas) %>%
      summarise(vid = mean(sum(avgWage, na.rm = TRUE))) %>%
      round() %>%
      prettyNum(big.mark = ",")
    d <- "Info nera"
    valueBox(
      value = ifelse(c == 0, d, paste(c, "eur")),
      subtitle = "Vidutinis atlyginimas",
      color = "fuchsia"
    )
  })
  
  #Lenteles, rodancios imones koda, kurimas
  output$kodas <- renderValueBox({
    data %>%
      filter(name == input$imones_pavadinimas) %>%
      select(code) %>% unique() %>% 
      valueBox(
        subtitle = "Įmones kodas",
        color = "teal"
      )
  })
  
}

shinyApp(ui, server)