setwd("C:/Users/userPC/Documents/GeneralInsurance_Class/Data")
dt_KPI <- read.csv("lesson2_KPI.csv")
dt_KPI_new <- dt_KPI %>% filter_all(all_vars(!is.na(.)))

fluidPage(    
  
  # Give the page a title
  titlePanel("Premium vs. Expenses"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("volba", "Choose", 
                  choices=colnames(dt_KPI_new[,1:5]),
                  selected = 1),
      hr(),
      helpText("KPI data")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("Plot")  
    )
    
  )
)