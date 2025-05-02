
library(shiny)                                         #Calls the "shiny" library

source("M:\\Desktop\\AssuranceCode\\assuranceServer.R")              #Calls the serversheet R-script, with all the functions and plots the app use


ui <- fluidPage(                                       #Creates an user-interface
  headerPanel( "Assurance Calculator"),        #Creates a headline of the app
  column(12,
         wellPanel(
           helpText("The assurance app calculates and plots assurance, power and prior, for your own inputs"))),   #Creates a description of the app
  column(4,
         wellPanel(                                                               # Creates an inputbox, where the user can pick the following values by them self.
           headerPanel(" Input  \n "),                                         # Makes a header of the inputbox.
           selectInput("test_type", "Type of test", c("one_sided", "two_sided"), selected = "two_sided"),
           numericInput("n1", "Sample size for treatment 1", value = 53, min = 1, max = 100),
           numericInput("n2", "Sample size for treatment 2", value = 53, min = 1, max = 100),
           numericInput("sd1", "Standard deviation of the outcome for treatment 1", value = 2.8, min = 0.01, max = 100),
           numericInput("sd2", "Standard deviation of the outcome for treatment 2", value = 3.7, min = 0.01, max = 100),
           numericInput("alpha", "Significance level", value = 0.05, min = 0.01, max = 0.99),
           numericInput("m", "Mean of prior for the effect", value = 1.89, min = -100, max = 100),
           numericInput("v", "Variance of prior for the effect", value = 0.42, min = 0.01, max = 100),
           numericInput("maxsize", "Maximum sample size in plot", value = 100, min = 10, max = 1000),
           numericInput("es", "Target treatment effect for power calculation", value = 1.89, min = 0, max = 40),
           numericInput("pwr", "Target for probability of success", value = .8, min = 0, max = 1))
  ),
  column(8,
         wellPanel(                                             # Creates the assurance plot
           plotOutput("assuranceplot", click = "plot_click"),   # Makes the plot interactive by setting up a click-function in the plot
           verbatimTextOutput("n1")                             # Makes the under-box, where you can see the output of your click 
         )),
  column(8,
         wellPanel(                                       # Creates the description-box of the assurance plot, which output is HTML-text.
           htmlOutput("discription")
         )),
  
  column(12,
         wellPanel(                                       # Creates the calculation box, which output is both HTML-text and the assurance and critical value calculations.
           htmlOutput("gammaresults")     
         )),
  fluidRow(                                             # Creates a new row in the user interface
    column(8,
           wellPanel(                                       # Creates the prior plot
             plotOutput("normplot")
           )),
    column(4,
           wellPanel(                                       # creates the description-box of the prior plot
             htmlOutput("priorplotdis")
           ))))



server <- function(input, output) {        # Create a server, which can run all the function by calling the server sheet "assuranceServer.R", 
  # which was sources earlier in this sheet
  output$assuranceplot <- renderPlot({     # Plots the assuranceplot in the "assuranceplot-box" by calling the function "assuranceplot" in the server sheet,     
    assuranceplot(
      ratio_ss= input$n2 / input$n1,      # Defining all of the variables the function from the serversheet use, such that it's set up to the value picked in the inputbox
      alpha=input$alpha,
      sd1=input$sd1,
      sd2=input$sd2,
      m=input$m,
      v=input$v,
      maxsize=input$maxsize,
      test_type=ifelse(input$test_type%in%"two_sided","two_sided","one_sided"),
      es=input$es,
      pwr=input$pwr,
      q=input$n1)
  })
  
  output$n1 <- renderText( {
    paste0("n1 = ", input$plot_click$x, "\nProbability of success = ", input$plot_click$y) # prints the output of the click in the assuranceplot
  } )
  
  output$discription <- renderText({            # Creates the output to the "discription-box" created above for the Assurance plot. 
    ifelse(input$test_type == "one_sided",      #  By an if/else statement, the description-text is changing such that is is always describes exactly the showed plot 
           yes = " <header> <h2> Description of the plot for the one-sided test </h2> </header> <br/> <br/>
           The red line represents the assurance of rejecting the null hypothesis, depending on the selected input values.<br/> <br/>
           The grey line represents the calculated power, depending on the selected input values.<br/> <br/>
           The pink line showes the power target, chosen in the inputbox.<br/> <br/>
           The purple line showes the chosen sample size for treatment 1 (n1).<br/> <br/>
           If you click on the plot, you can see which values the assurance and the n1 take, at the specifict place in the plot, in the box below the plot.<br/> <br/>", 
           
           no = " <header> <h2> Description of the plot for the two-sided test </h2> </header> <br/> <br/>
                 The red line (The overall assurance) represents the overall assurance of rejecting the null hypothesis,
                 depending on the selected input values.<br/> <br/>
                 This only says something about if there is one treatment which is superiority against/over the other, but not which one it is.<br/> <br/>
                 The green line (Assurance < 1) represents the assurance of rejecting the null hypothesis with data favoring treatment 1, 
                 depending on the selected input values. <br/> <br/>
                 The blue line (Assurance < 2) represents the assurance of rejecting the null hypothesis with data favoring treatment 2, 
                 depending on the selected values in the box to your left. <br/> <br/>
                 The grey line represents the calculated power, depending on the selected input values. <br/> <br/>
                 The pink line showes the power target, chosen in the box to your left. <br/> <br/>
                 The purple line showes the chosen sample size for treatment 1 (n1). <br/> <br/>
                 If you click on the plot, you can see which values the assurance and the n1 take, at the specifict place in the plot, 
                 in the box below the plot. <br/> <br/>")
  })
  
  output$gammaresults <- renderText({
    
    gammaresults <- gamma_OneAndTwo(      # Creates the output to the calculating-box, by using the function "gamma_OneAndTwo" from the server sheet
      alpha = input$alpha,                # Defining the values from the sever sheet-function to the input-values
      sd1 = input$sd1,
      sd2 = input$sd2,
      n1 = input$n1,
      n2 = input$n2,
      m = input$m,
      v = input$v,
      test_type = input$test_type)
    cresults <- critical_value(          # insert the critical value, by using the function "critical_value" from the sever sheet
      alpha = input$alpha,
      sd1 = input$sd1,
      sd2 = input$sd2,
      n1 = input$n1,
      n2 = input$n2,
      es = input$es,
      test_type = input$test_type
    )
    ifelse(input$test_type == "one_sided",
           yes = paste("In this box, you can see the calculated assurance and the critical value for the chosen input values. <br/> <br/> Assurance = ", round(gammaresults, digits = 3), 
                       "<br/> Values above this value will result in a significant p-value = ",round(cresults, digits = 3)), 
           no = paste0("In this box, you can see the calculated assurance and the critical values for the chosen input values. <br/> <br/> The overall assurance with no data favoring  =  ", round(gammaresults[[1]], digits = 3), 
                       "<br/> Assurance with data favoring for treatment 1 =  ", round(gammaresults[[2]], digits = 3), 
                       "<br/> Assurance with data favoring for treatment 2 =  ", round(gammaresults[[3]], digits = 3), 
                       "<br/> Values below this value will result in a significant p-value = - ", round(cresults, digits = 3),
                       "<br/> Values above this value will result in a significant p-value = ",round(cresults, digits = 3)))
    
  })
  output$normplot <- renderPlot({
    normplot <- normplot(                           # Plots the prior plot by using th function "normplot" from the server sheet
      m = input$m,                                  # Defining the values from the sever sheet-function to the selected values from the inputbox
      v = input$v,
      alpha = input$alpha,
      sd1 = input$sd1,
      sd2 = input$sd2,
      n1 = input$n1,
      n2 = input$n2,
      test_type = input$test_type,
      es = input$es
    )})
  
  
  output$priorplotdis <- renderText({             # Makes the output to the description-box for the prior plot.
    
    " <header> <h2> Description of Prior plot distribution </h2> </header> <br/> <br/> 
    The red area represents the probability of not rejecting the null hypothesis, based on current knowledge only. It's also represent the effect that will result in rejection of the null hypothesis. <br/> <br/>
    The green area represents the probability of success, based on current knowledge only. <br/> <br/>
    The vertical line(s), represent(s) the critical value(s) for the drug, depending on the picked input values."
  })
}


shinyApp(ui = ui, server = server) #Runs the app









