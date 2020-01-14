library(shiny)
library(extraDistr)
library(ggplot2)
library(scales)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  headerPanel("Probabilities for no duplicates:"),
  

  
  sidebarPanel(
    selectInput("Number", "Please select the number of duplicate Blocks",
                choices=c("1","2","3","4","5")),
    sliderInput("Cards","Please select the number of cards in your deck",
                min=1, max=50, value=27, step=1),
    
    conditionalPanel(condition="input.Number=='1'",
      textInput("number1","Please select the size of block 1",2)),
    
    
    conditionalPanel(condition="input.Number=='2'",
                     textInput("number2","Please select the size of block 1",2),
                     textInput("number3","Please select the size of block 2",2)),
    
    conditionalPanel(condition="input.Number=='3'",
                     textInput("number4","Please select the size of block 1",2),
                     textInput("number5","Please select the size of block 2",2),
                     textInput("number6","Please select the size of block 3",2)),
    
    
    conditionalPanel(condition="input.Number=='4'",
                     textInput("number7","Please select the size of block 1",2),
                     textInput("number8","Please select the size of block 2",2),
                     textInput("number9","Please select the size of block 3",2),
                     textInput("number10","Please select the size of block 4",2)
              
                     
                     ),
    conditionalPanel(condition="input.Number=='5'",
                     textInput("number11","Please select the size of block 1",2),
                     textInput("number12","Please select the size of block 2",2),
                     textInput("number13","Please select the size of block 3",2),
                     textInput("number14","Please select the size of block 4",2),
                     textInput("number15","Please select the size of block 5",2)
                     
    ),
    
    
  ),
  
  mainPanel(
    h5("The aim of this application is to display the probability of having no duplicates in the deck for the following draws. This is primarily important for the arena play mode, since you cant guarante a deck with no duplicates there. Knowing the exact probability might help with th descision, if a card needs to be played immedately or if it is legit to wait till there arent any duplicates left."),
    plotOutput("myPlot"),
    tableOutput("myTable")
  )
)

# Define server logic required to draw a histogram ----
server <- function(input,output,session){
  
  output$myPlot<-renderPlot({
    
    
    deckSize<-input$Cards
    blocks<-as.numeric(input$Number)
    prob=numeric(10)
    
    if(blocks==1){
      
      number=as.numeric(input$number1)
      
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number-1,number)) {prob[i]=prob[i]+dmvhyper(x=c(s,l),n=c(number,deckSize-number),k=i)
          
          }
          
        }
        
      }
      
      
    }
    
    
    if(blocks==2){
      
      number2=as.numeric(input$number2)
      number3=as.numeric(input$number3)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number2-1,number2)) {
            
            for (t in c(number3-1,number3)) {
              
              prob[i]=prob[i]+dmvhyper(x=c(s,t,l),n=c(number2,number3,deckSize-number2-number3),k=i)
              
            }
          }
        }
      }
    }
    
    if(blocks==3){
      
      number4=as.numeric(input$number4)
      number5=as.numeric(input$number5)
      number6=as.numeric(input$number6)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number4-1,number4)) {
            
            for (t in c(number5-1,number5)) {
              
              for (q in c(number6-1,number6)) {
                
              
              
              prob[i]=prob[i]+dmvhyper(x=c(s,t,q,l),n=c(number4,number5,number6,deckSize-number4-number5-number6),k=i)
             
              } 
            }
          }
        }
      }
    }
    
    if(blocks==4){
      
      number7=as.numeric(input$number7)
      number8=as.numeric(input$number8)
      number9=as.numeric(input$number9)
      number10=as.numeric(input$number10)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number7-1,number7)) {
            
            for (t in c(number8-1,number8)) {
              
              for (q in c(number9-1,number9)) {
                
                for (w in c(number10-1,number10)) {
                  
                
                
                
                prob[i]=prob[i]+dmvhyper(x=c(s,t,q,w,l),n=c(number7,number8,number9,number10,deckSize-number7-number8-number9-number10),k=i)
                
                }
              } 
            }
          }
        }
      }
    }
    if(blocks==5){
      
      number11=as.numeric(input$number11)
      number12=as.numeric(input$number12)
      number13=as.numeric(input$number13)
      number14=as.numeric(input$number14)
      number15=as.numeric(input$number15)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number11-1,number11)) {
            
            for (t in c(number12-1,number12)) {
              
              for (q in c(number13-1,number13)) {
                
                for (w in c(number14-1,number14)) {
                  
                
                  for (r in c(number15-1,number15)) {
                
                
                prob[i]=prob[i]+dmvhyper(x=c(s,t,q,w,r,l),n=c(number11,number12,number13,number14,number15,deckSize-number11-number12-number13-number14-number15),k=i)
                
                  }  
                }
              } 
            }
          }
        }
      }
    }
    
    Draw=c(1:10)
    vec=data.frame(Draw,prob)

    p<-ggplot(vec,aes(Draw,prob))  
    p +scale_x_continuous(breaks = seq(0,10,1))  + ylab("Probability") + scale_y_continuous(labels = scales::percent)+geom_point(size=5,aes(colour=prob))+ scale_colour_gradient(low = "red", high = "green",limits=c(0,1))+labs(fill="Probability")
    })  
  
  
  output$myTable<-renderTable({
    deckSize<-input$Cards
    blocks<-as.numeric(input$Number)
    prob=numeric(10)
    
    if(blocks==1){
      
      number=as.numeric(input$number1)
      
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number-1,number)) {prob[i]=prob[i]+dmvhyper(x=c(s,l),n=c(number,deckSize-number),k=i)
          
          }
          
        }
        
      }

    }
    
    else if(blocks==2){
      
      number2=as.numeric(input$number2)
      number3=as.numeric(input$number3)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number2-1,number2)) {
            
            for (t in c(number3-1,number3)) {
              
              prob[i]=prob[i]+dmvhyper(x=c(s,t,l),n=c(number2,number3,deckSize-number2-number3),k=i)
              
            }
          }
        }
      }

    }
    
    else if(blocks==3){
      
      number4=as.numeric(input$number4)
      number5=as.numeric(input$number5)
      number6=as.numeric(input$number6)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number4-1,number4)) {
            
            for (t in c(number5-1,number5)) {
              
              for (q in c(number6-1,number6)) {
                
                
                
                prob[i]=prob[i]+dmvhyper(x=c(s,t,q,l),n=c(number4,number5,number6,deckSize-number4-number5-number6),k=i)
                
              } 
            }
          }
        }
      }
    }
    
    else if(blocks==4){
      
      number7=as.numeric(input$number7)
      number8=as.numeric(input$number8)
      number9=as.numeric(input$number9)
      number10=as.numeric(input$number10)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number7-1,number7)) {
            
            for (t in c(number8-1,number8)) {
              
              for (q in c(number9-1,number9)) {
                
                for (w in c(number10-1,number10)) {
                  
                  
                  
                  
                  prob[i]=prob[i]+dmvhyper(x=c(s,t,q,w,l),n=c(number7,number8,number9,number10,deckSize-number7-number8-number9-number10),k=i)
                  
                }
              } 
            }
          }
        }
      }
    }
    else if(blocks==5){
      
      number11=as.numeric(input$number11)
      number12=as.numeric(input$number12)
      number13=as.numeric(input$number13)
      number14=as.numeric(input$number14)
      number15=as.numeric(input$number15)
      
      for (i in 1:10) {
        
        for (l in 0:i) {
          
          for (s in c(number11-1,number11)) {
            
            for (t in c(number12-1,number12)) {
              
              for (q in c(number13-1,number13)) {
                
                for (w in c(number14-1,number14)) {
                  
                  
                  for (r in c(number15-1,number15)) {
                    
                    
                    prob[i]=prob[i]+dmvhyper(x=c(s,t,q,w,r,l),n=c(number11,number12,number13,number14,number15,deckSize-number11-number12-number13-number14-number15),k=i)
                    
                  }  
                }
              } 
            }
          }
        }
      }
    }
    
    
    Probability=percent(prob,accuracy = 0.01)
    #Probability=t(Probability)
    #Probability=as.data.frame(Probability)
    #colnames(Probability)=c(1:10)
    Draw=c(1:10)
    prob=rbind(Draw,Probability)  
    
    },colnames = FALSE ) }


# Create Shiny app ----
shinyApp(ui = ui, server = server)
