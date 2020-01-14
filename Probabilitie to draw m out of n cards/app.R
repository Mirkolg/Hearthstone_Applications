library(shiny)
library(extraDistr)
library(ggplot2)
library(scales)

# Define UI for app that Draws a histogram ----
ui <- fluidPage(
  
  headerPanel("Draw Probabilities"),
  
  sidebarPanel("The graph shows the probabilty to Draw at least one of the desired cards for the next 10 Draws",
               
               sliderInput("Decksize","Plese select the number of cards in your deck",
                           min=1, max=30, value=27, step=1),
               sliderInput("Cards","Plese select the number of cards you want to Draw in your deck",
                           min=1, max=30, value=3, step=1),
               
               
               
  ),
  
  mainPanel(
    plotOutput("myPlot"),
    tableOutput("myTable")
    
  )
  
)



# Define server logic required to Draw a histogram ----
server <-   function(input,output,session){
  
  output$myPlot<-renderPlot({
    
    deckSize<-input$Decksize
    cards<-input$Cards
    
    
    Probability=numeric(10)
    Draw=c(1:10)
    
    for (i in 1:10) {Probability[i]=1-phyper(0,cards,deckSize-cards,i)

    
    vec=data.frame(Draw,Probability)}
    
    
    p<-ggplot(vec,aes(Draw,Probability))  
    p  +scale_x_continuous(breaks = seq(0,10,1))  + ylab("Probability")+geom_point(size=5,aes(colour=Probability))+ scale_colour_gradient(low = "red", high = "green",limits=c(0,1))+labs(fill="Probability")
    
  }
  )
  
  output$myTable<-renderTable({
    deckSize<-input$Decksize
    cards<-input$Cards
    
    
    Probability=numeric(10)
    Draw=c(1:10)
    
    for (i in 1:10) {Probability[i]=1-phyper(0,cards,deckSize-cards,i)}
    
    Probability=percent(as.numeric(Probability))
    vec=rbind(Draw,Probability)
    #vec[,1]=round(vec[,1],0)
    vec
  },colnames = FALSE)
  
  }


# Create Shiny app ----
shinyApp(ui = ui, server = server)