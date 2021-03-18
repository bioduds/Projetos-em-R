library(shiny)
library(shinyjs)
library(ggplot2)

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  title = "Probability of Normal Distribution",
  fluidRow(
    wellPanel(style="background-color:skyblue;", width = "100%", height = "5%",
      h1("Probability of Normal Distribution", style = "text-align:center;font-weight:bold;color:white;")
    )
  ),
  fluidRow(
    column(3,
           wellPanel(style="background-color:skyblue;border:none;color:white;",
                     h3("Usage", style="font-weight:bold;"),
                     p(style = "color:white;text-align:justify;",
                       HTML('specify the parameter values (\\(\\mu, \\sigma\\)), press the setup (gears icon) button and the lower and upper limits.
                       $$P(L \\lt X \\lt U) = \\int_{L}^{U}\\frac{1}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}dx$$
                       <br/>
                            <br/>
                            If you want to find the z-value for a significant value \\(\\alpha\\) (alpha) then enter the alpha value on the <strong>"Calculate Z-value Table"</strong> section. 
                            The Z-value will be calculated for \\(\\frac{\\alpha}{2}\\). \\(1 - P(X \\lt x)\\).'))
                     )
           ),
    column(width = 3,
           h5("Normal Distribution Parameter"),
           div(style="display:inline-block;vertical-align:top;width:42%;font-size:120%;", numericInput("mu", expression("\u03BC"), value = 0)),
           div(style="display:inline-block;vertical-align:top;width:42%;font-size:120%;", numericInput("sigma", expression("\u03C3"), value = 1, min = 0.1, step = 0.1)),
           div(style="display:inline-block;vertical-align:top;width:10%;margin-top:28px;", actionButton("updParam", NULL, icon = icon("cogs"))),
           hr(),
           helpText("Cumulative value between lower and upper limits"),
           conditionalPanel(condition = "input.nilai2 < input.nilai1",
                            p("The Upper Limit value cannot be smaller than the Lower Limit value.", style = "color:red;font-size:90%;")
           ),
           uiOutput("prob"),
           hr(),
           h5("Calculate Z-value Table"),
           div(style="display:inline-block;vertical-align:top;width:32%;font-size:120%;", numericInput("alpha", expression("\u03B1"), value = 0.05, step = 0.001, min = 0.00001, max = 0.99999)),
           div(style="display:inline-block;vertical-align:top;width:52%;font-size:120%;", numericInput("zv", paste0("Z-value (", expression("\u03B1"), "/2)"), value = 1.96)),
           div(style="display:inline-block;vertical-align:top;width:10%;margin-top:28px;", actionButton("calcZv", NULL, icon = icon("calculator"))),
           hr(),
           h5("Standardized Value"),
           div(style="display:inline-block;vertical-align:top;width:32%;font-size:120%;", numericInput("xval", "X", value = 0)),
           div(style="display:inline-block;vertical-align:top;width:52%;font-size:120%;", textInput("std", paste0("Z = (X - ", expression("\u03BC"), ")/", expression("\u03C3")), value = 0)),
           div(style="display:inline-block;vertical-align:top;width:10%;margin-top:28px;", actionButton("calcStd", NULL, icon = icon("calculator")))
    ),
    column(width = 6,
           # uiOutput("slider"),
           sliderInput("sliderrange", label = h4("Lower and Upper limits"), min = -4.5, max = 4.5, value = c(-1.96, 1.96), step = 0.01, width = "100%"),
           uiOutput("plotTitle"),
           plotOutput("plotPeluang")
    )
  )
                          
)

server <- function(input, output, session){
  # Menu Menghitung Peluang Normal
  
  observe({
    disable("zv")
    disable("std")    
  })
  
  param <- eventReactive(input$updParam, {
    mu = as.numeric(input$mu)
    sigma = as.numeric(input$sigma)
    list(mu = mu, sigma = sigma)
  })
  
  output$plotTitle <- renderUI({
    param <- param()
    h4(style = "text-align:center;", paste0("Normal Distribution with ", expression("\u03BC = "), param$mu, expression(" dan \u03C3 = "), param$sigma))
  })
  
  observeEvent(input$calcZv,{
    req(input$alpha)
    isolate(input$alpha)
    updateNumericInput(session, "zv", paste0("Z-value (", expression("\u03B1"), "/2)"), value = round(qnorm(1 - as.numeric(input$alpha)/2), 2))
  })
  
 observeEvent(input$calcStd, {
   req(input$xval)
   req(input$mu)
   req(input$sigma)
   isolate(input$xval)
   # param <- param()
   z <- (as.numeric(input$xval) - input$mu)/input$sigma
   updateTextInput(session, "std", paste0("Z = (X - ", expression("\u03BC"), ")/", expression("\u03C3")), value = round(z, 2))
 })
  
  observeEvent(input$updParam, {
    param <- param()
    mu <- param$mu
    sigma <- param$sigma
    # a = as.numeric(input$sliderrange[1])
    # b = as.numeric(input$sliderrange[2])
    if(mu == "" | sigma == ""
       | mu == "-" | sigma == "-"
       | is.null(mu) | is.null(sigma))
    {
      return(NULL)
    }
    
    bawah.x = mu - 4.5*sigma
    atas.x = mu + 4.5*sigma
    # output$slider <- renderUI({
      updateSliderInput(session, "sliderrange", label = "Lower and Upper limits", min = bawah.x, max = atas.x, step = 0.01)
    # })
  })

 
  probs <- reactive({
    param <- param()
    mu  <- as.numeric(param$mu)
    sigma  <- as.numeric(param$sigma)
    
    a = as.numeric(input$sliderrange[1])
    b = as.numeric(input$sliderrange[2])
    if(mu == "" | sigma == ""
       | mu == "-" | sigma == "-"
       | is.null(mu) | is.null(sigma))
    {
      return(NULL)
    }
    
    bawah.x = mu - 4.5*sigma
    atas.x = mu + 4.5*sigma
    # posisi = b + 0.7
    
    peluang1 = pnorm(a, mu, sigma)
    if(b <= mu) {
      peluang2 = pnorm(b, mu, sigma) - peluang1
    } else {
      peluang2 = pnorm(mu, mu, sigma) - peluang1
    }
    if(a >= mu) {
      peluang3 = pnorm(b, mu, sigma) - pnorm(a, mu, sigma)
    } else {
      peluang3 = pnorm(b, mu, sigma) - pnorm(mu, mu, sigma)
    }
    peluang4 = 1 - pnorm(b, mu, sigma)
    
    if(b <= bawah.x) {
      bawah.x <- b
      # posisi <- b * 1.1 
      # peluang = 1 - peluang
    }
    if(a >= atas.x) {
      atas.x <- a
      # posisi <- a * 1.1
      # peluang = 1 - peluang
    }
    
    
    df <- data.frame(poly.x = c(a, seq(a, b, 0.01), b),
                     poly.y = c(0, dnorm(seq(a, b, 0.01), mu, sigma), 0)
    )
    df_norm <- data.frame(curve.x = c(bawah.x, seq(bawah.x, atas.x, 0.01), atas.x),
                          curve.y = c(0, dnorm(seq(bawah.x, atas.x, 0.01), mu, sigma), 0)
    )
    
    
    if(peluang2 >= 0 & peluang3 >= 0) {
      probtot1 <- peluang2
      probtot2 <- peluang3
    } else if(peluang2 < 0 & peluang3 >= 0) {
      probtot1 <- 0
      probtot2 <- peluang3
    } else if(peluang2 >= 0 & peluang3 < 0) {
      probtot1 <- peluang2
      probtot2 <- 0
    } else {
      probtot1 <- 0
      probtot2 <- 0
    }
    
    list(df_norm = df_norm, 
         df = df, 
         bawah.x = bawah.x, 
         atas.x = atas.x, 
         peluang1 = peluang1,
         peluang2 = peluang2,
         peluang3 = peluang3,
         peluang4 = peluang4,
         probtot1 = probtot1, 
         probtot2 = probtot2)
    
  })
  
  # ip <- reactive({
  #   mu = as.numeric(input$mu)
  #   sigma = as.numeric(input$sigma)
  #   mu*sigma
  # })
  # 
  # observeEvent(ip(), {
  #   updateSliderInput(session, "sliderrange", label = h3("Lower and Upper limits"), min = probs()$bawah.x, max = probs()$atas.x, value = c(-1.96, 1.96), step = 0.01)
  # })
  
  # observeEvent(input$sliderrange, {
  #   updateNumericInput(session, "nilai1", label = "Lower", value = input$sliderrange[1])
  #   updateNumericInput(session, "nilai2", label = "Upper", value = input$sliderrange[2])
  #   hideElement("nilai1")
  #   hideElement("nilai2")
  # })
  
  output$prob <- renderUI({
    probs <- probs()
    p(sprintf("P(%s < X < %s) = %s%%", input$sliderrange[1], input$sliderrange[2], round((probs$probtot1 + probs$probtot2)*100, 1)), style="text-align:center;font-weight:bold;color:black;font-size:150%;")
  })
  
  # observeEvent(input$sliderrange, {
    output$plotPeluang <- renderPlot({
      param <- param()
      mu  <- as.numeric(param$mu)
      sigma <- as.numeric(param$sigma)
      
      a = as.numeric(input$sliderrange[1])
      b = as.numeric(input$sliderrange[2])
      if(mu == "" | sigma == ""
         | mu == "-" | sigma == "-"
         | is.null(mu) | is.null(sigma))
      {
        return(NULL)
      }
      
      bawah.x = mu - 4.5*sigma
      atas.x = mu + 4.5*sigma
      # posisi = b + 0.7
      # 

      probs <- probs()
      
      p9 <- ggplot(data = probs$df_norm, aes(x = curve.x, y = curve.y)) +
        geom_area(fill = "lightblue", colour = "skyblue", size = 1.2) +
        geom_polygon(data = probs$df, aes(x = poly.x, y = poly.y), fill = "#71A9CA") +
        geom_vline(xintercept = a, linetype = "dotted", colour = "#F24343") +
        geom_vline(xintercept = b, linetype = "dotted", colour = "#F24343") +
        geom_vline(xintercept = mu, linetype = "dotted", colour = "black") 
      if(probs$peluang1*100 >= 1) {
        p9 <- p9 + annotate("text",
                            label = paste0(round(probs$peluang1*100, 1), "%"),
                            x = a - 0.4, #posisi,
                            y = max(probs$df_norm$curve.y) * 1.2,
                            size = 5, colour = "lightblue")
      }
      x2 <- ifelse(b < mu, b - 0.4, mu - 0.4)

      # anotasi upper.tail
      x3 <- ifelse(a > mu, a + 0.4, mu + 0.4)
      if(probs$peluang4*100 >= 1) {
        p9 <- p9 + annotate("text",
                            label = paste0(round(probs$peluang4*100, 1), "%"),
                            x = b + 0.4, #posisi,
                            y = max(probs$df_norm$curve.y) * 1.2,
                            size = 5, colour = "lightblue")
      }
      
      p9 <- p9 +
        xlim(probs$bawah.x, probs$atas.x) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, max(probs$df_norm$curve.y) * 1.3)) +
        labs(title = paste0("P(", a, " < X < ", b, ") = ", round(probs$probtot1*100, 1), "% + ", round(probs$probtot2*100, 1), "%\n"),
             x = "X") +
        theme_classic() +
        theme(
          plot.title = element_text(hjust = 0.5, color = "#71A9CA", size = 14, face = "bold"),
          axis.line.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(size = 14, face = "bold"),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_blank()
        )
      p9
    }, height = 350)
  # })

}

shinyApp(ui, server)
