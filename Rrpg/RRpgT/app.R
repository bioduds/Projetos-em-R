### Eduardo Capanema (bioduds@gmail.com)
### Projeto RRpg - Criando um RPG estatisticamente coerente

library( shiny )
library( shinyjs )
library( ggplot2 )

##########
### UI ###
ui <- fluidPage(
        useShinyjs(),
        withMathJax(),
        title = "Sala de Controle do DM",
        fluidRow( wellPanel( style="background-color:#494949;", width="100%", height="5%", h1( "Sala de Controle do DM", style="text-align:center;font-weight:bold;color:white;" ) )
    ),
    fluidRow(
        column( 4,
               h2( "Parametros da Distribuicao do Evento" ),
               div( style="display:inline-block;vertical-align:top;width:35%;font-size:120%;", 
                    selectInput( "selectDifficulty", "Selecao de Dificuldade", 
                                 c( "Impossivel"="impossible", "Muito Dificil"="very_hard", "Dificil"="hard", "Medio"="medium", "Facil"="easy", "Muito Facil"="very_easy", "Trivial"="trivial" ),
                                 selected = "medium"
                    )
                  ),
               div( style="display:inline-block;vertical-align:top;width:49%;font-size:120%;", numericInput( "mu", "Regular Dificuldade", value=50 ) ),
               div( style="display:inline-block;vertical-align:top;width:35%;font-size:120%;", 
                    selectInput( "selectVariance", "Variancia do Efeito", 
                                 c( "Caotico"="chaotic", "Erratico"="erratic", "Medio"="medium", "Esperavel"="expected", "Previsivel"="predictable" ),
                                 selected = "medium"
                    )
               ),
               div( style="display:inline-block;vertical-align:top;width:49%;font-size:120%;", numericInput( "sigma", "Regular Variancia", value=10, min=0.1, step=0.1 ) ),
               actionButton( "rodarAcao", "Rodar Acao", NULL, "100%" ),
               hr(),
               helpText( "Valor acumulado entre limites superior e inferior (critico e extremo)" ),
               conditionalPanel( 
                   condition="input.nilai2 < input.nilai1", p( "O valor superior nao pode ser menor que o superior.", style="color:red;font-size:90%;"
                   )
               ),
               uiOutput( "prob" ),
               hr(),
               div( style="display:inline-block;vertical-align:top;width:44%;font-size:120%;visibility:hidden", numericInput( "roll", expression( "\u03C4" ), value=0.00001 ) )
               
        ),
        column( width = 6,
                # uiOutput( "slider" ),
                sliderInput( "sliderrange", label=h4("Critico e Extremo"), min=0, max=100, value=c( 10.0, 90.0 ), step=0.00001, width="100%" ),
                uiOutput( "plotTitle" ),
                plotOutput("plotPeluang" )
        ),
        column( width = 2,
                # uiOutput( "slider" ),
                wellPanel( style="background-color:#494949;border:1px dotted red;color:white;",
                           h3( "Parametros", style="font-weight:bold;" ),
                           p( style = "color:white;text-align:justify;",
                              HTML( 'Incluir condicao')
                           ),
                           selectInput( "selectCondition", "Selecao de condicao", 
                                        c( "Chuva"="rain", "Gelo"="ice", "Neblina"="fog" ) 
                                      )
                           
                )
                
                
        )
        
    ) ## end fluidRow
    
) ## end fluidPage




############
## SERVER ##
server <- function( input, output, session ) {
    # Menu Menghitung Peluang Normal
    
    roll <- runif( 1, 0.0, 100.0 ) ## roda um d100 com 13 casas decimais 10^13 - evento mitologico
    #updateNumericInput( session, "roll", value = roll )

    observe({
        disable( "zv" )
        disable( "std" )
        click( "rodarAcao" )
    })

    param <- eventReactive( input$updParam, {
        print( "updattttteeeeee" )
        updateGraph();
    })
    
    observeEvent( input$mu, {
        print( "updattttteeeeee Muuuu" )
        updateGraph();    })

    observeEvent( input$sigma, {
        print( "updattttteeeeee Sigma" )
        updateGraph();
    })
    
    param <- eventReactive( input$rodarAcao, {
        mu = as.numeric( input$mu )
        sigma = as.numeric( input$sigma )
        roll =  as.numeric( input$roll )
        list( mu=mu, sigma=sigma, roll=roll )
    })
    
    output$plotTitle <- renderUI({
        param <- param()
        h4( style="text-align:center;", paste0( "Modelagem do Evento ", expression( "\u03BC (dificuldade) = " ), param$mu, expression( " (variancia) \u03C3 = " ), param$sigma ) )
    })
    
    observeEvent( input$calcZv,{
        req( input$alpha )
        isolate( input$alpha )
        updateNumericInput( session, "zv", paste0( "Z-value (", expression("\u03B1"), "/2)"), value=round( qnorm( 1 - as.numeric( input$alpha )/2 ), 2 ) )
    })
    
    observeEvent( input$calcStd, {
        req( input$xval )
        req( input$mu )
        req( input$sigma )
        req( input$roll )
        #isolate( input$xval )
        z <- ( as.numeric( input$xval ) - input$mu )/input$sigma
        updateTextInput( session, "std", paste0( "Z = (X - ", expression( "\u03BC" ), ")/", expression( "\u03C3" ) ), value=round( z, 2 ) )
    })
    
    observeEvent( input$selectDifficulty, {
        nDiff = 50
        if( input$selectDifficulty == "impossible" ) { nDiff = 97 }
        else if( input$selectDifficulty == "very_hard" ) { nDiff = 85 }
        else if( input$selectDifficulty == "hard" ) { nDiff = 70 }
        else if( input$selectDifficulty == "medium" ) { nDiff = 50 }
        else if( input$selectDifficulty == "easy" ) { nDiff = 30 }
        else if( input$selectDifficulty == "very_easy" ) { nDiff = 15 }
        else if( input$selectDifficulty == "trivial" ) { nDiff = 3 }
        else { nDiff = 50 }
        updateNumericInput( session, "mu", value=nDiff )
    })

    observeEvent( input$selectVariance, {
        nVar = 10
        if( input$selectVariance == "chaotic" ) { nVar = 20 }
        else if( input$selectVariance == "erratic" ) { nVar = 15 }
        else if( input$selectVariance == "medium" ) { nVar = 10 }
        else if( input$selectVariance == "expected" ) { nVar = 5 }
        else if( input$selectVariance == "predictable" ) { nVar = 1 }
        else { nVar = 10 }
        updateNumericInput( session, "sigma", value=nVar )
    })
    
    
    observeEvent( input$updParam, {
        param <- param()
        mu <- param$mu
        sigma <- param$sigma
        if( mu == "" | sigma == "" | mu == "-" | sigma == "-" | is.null( mu ) | is.null( sigma ) ) { return( NULL ) }
        bawah.x = mu - ( 4.5 * sigma )
        atas.x = mu + ( 4.5 * sigma )
        updateSliderInput( session, "sliderrange", label="Critico e Extremo", min=bawah.x, max=atas.x, step=0.001 )
    })
    
    observeEvent( input$rodarAcao, {
        param <- param()
        roll <- runif( 1, 0.0, 100.0 ) ## roda um d100 com 13 casas decimais 10^13 - evento mitologico
        print( paste( "ROLL: ", roll ) )
        # classificacao da rodagem
        if( roll > 10 && roll < 90 ) { print( "Normal Roll" ) }
        if( roll <= 10 && roll > 1 ) { print( "Critico: Normal Roll" ) }
        if( roll >=90 && roll < 99 ) { print( "Extremo: Normal Roll" ) }
        updateNumericInput( session, "roll", value = roll )
    })
        
    probs <- reactive({
        param <- param()
        mu  <- as.numeric( param$mu )
        sigma  <- as.numeric( param$sigma )
        a = as.numeric( input$sliderrange[1] )
        b = as.numeric( input$sliderrange[2] )
        roll <- as.numeric( param$roll )
        if( mu == "" | sigma == "" | mu == "-" | sigma == "-" | is.null( mu ) | is.null( sigma ) ) { return(NULL) }
        bawah.x = mu - ( 4.5 * sigma )
        atas.x = mu + ( 4.5 * sigma )
        peluang1 = pnorm( a, mu, sigma )
        if( b <= mu ) { peluang2 = pnorm( b, mu, sigma ) - peluang1 } 
        else { peluang2 = pnorm( mu, mu, sigma ) - peluang1 }
        if( a >= mu ) { peluang3 = pnorm( b, mu, sigma ) - pnorm( a, mu, sigma ) } 
        else { peluang3 = pnorm( b, mu, sigma ) - pnorm( mu, mu, sigma ) }
        peluang4 = 1 - pnorm( b, mu, sigma )
        if( b <= bawah.x ) { bawah.x <- b }
        if( a >= atas.x ) { atas.x <- a }
        df <- data.frame( poly.x = c( a, seq( a, b, 0.01 ), b ), poly.y = c( 0, pnorm( seq( a, b, 0.01 ), mu, sigma ), 0 ) )
        df_norm <- data.frame( curve.x = c( bawah.x, seq( bawah.x, atas.x, 0.01 ), atas.x ), curve.y = c( 0, pnorm( seq( bawah.x, atas.x, 0.01 ), mu, sigma ), 0 ) )
        if( peluang2 >= 0 & peluang3 >= 0 ) {
            probtot1 <- peluang2
            probtot2 <- peluang3
        } else if( peluang2 < 0 & peluang3 >= 0 ) {
            probtot1 <- 0
            probtot2 <- peluang3
        } else if( peluang2 >= 0 & peluang3 < 0 ) {
            probtot1 <- peluang2
            probtot2 <- 0
        } else {
            probtot1 <- 0
            probtot2 <- 0
        }
        list( df_norm = df_norm, 
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
    
    output$prob <- renderUI({
        probs <- probs()
        p( sprintf( "P(%s < X < %s) = %s%%", input$sliderrange[1], input$sliderrange[2], ( probs$probtot1 + probs$probtot2 ) * 100, 5 ), style="text-align:center;font-weight:bold;color:black;font-size:150%;" )
    })
    
    output$plotPeluang <- renderPlot({
        param <- param()
        mu  <- as.numeric( param$mu )
        sigma <- as.numeric( param$sigma )
        a = as.numeric( input$sliderrange[1] )
        b = as.numeric( input$sliderrange[2] )
        roll <- as.numeric( param$roll )
        if( mu == "" | sigma == "" | mu == "-" | sigma == "-" | is.null(mu) | is.null( sigma ) ) { return( NULL ) }
        bawah.x = mu - 4.5 * sigma
        atas.x = mu + 4.5 * sigma
        probs <- probs()
        p9 <- ggplot( data=probs$df_norm, aes( x=curve.x, y=curve.y ) ) +
            geom_area( fill="lightblue", colour="skyblue", size=1.2 ) +
            geom_polygon( data=probs$df, aes( x=poly.x, y=poly.y ), fill="#71A9CA") +
            geom_vline( xintercept=a, linetype="dotted", colour="#F24343" ) +
            geom_vline( xintercept=b, linetype="dotted", colour="#180f82" ) +
             #geom_vline( xintercept=mu, linetype="dotted", colour="#292929" ) + ### NAO MOSTRAR Mu
            geom_vline( xintercept=roll, linetype="solid", colour="#797979" )
        
        ### CRÍTICO ###
        p9 <- p9 + annotate( "text",
                             label = paste0( "Critico: ", round( a, 9 ), "%" ),
                             x = a + 6.4,
                             y = max( probs$df_norm$curve.y ) * 0.2,
                             size = 5, colour = "#610808")
            
        x2 <- ifelse( b < mu, b - 0.4, mu - 0.4 )
        x3 <- ifelse( a > mu, a + 0.4, mu + 0.4 )
        
        ### EXTREMO ###
        p9 <- p9 + annotate( "text",
                             label = paste0( "Extremo: ", round( b, 9 ), "%" ),
                             x = b - 6.9,
                             y = max( probs$df_norm$curve.y ) * 0.2,
                             size = 5, colour = "#180f82")
        
        ### ROLL ###
        p9 <- p9 + annotate( "text",
                             label = paste0( "Roll: ", roll ),
                             x = roll + 1.4,
                             y = max( probs$df_norm$curve.y ) * 1.2,
                             size = 6, colour = "#292929")

        p9 <- p9 +
            xlim( 0, 100 ) +
            scale_y_continuous( expand = c( 0, 0 ), limits=c( 0, max( probs$df_norm$curve.y ) * 1.3 ) ) +
            labs( title=paste0( "Subtitulo [var] ", a ) ) + theme_classic() +
            theme(
                plot.title = element_text( hjust=0.5, color="#71A9CA", size=14, face="bold" ),
                #axis.line.y = element_blank(),
                axis.title.y = element_text(  size=14, face="bold" ),
                axis.text.y = element_text( "Efeito" ),
                #axis.ticks.y = element_blank(),
                axis.title.x = element_text( size=14, face="bold" ),
                panel.background = element_blank(),
                panel.grid.minor = element_blank(),
                #panel.grid.major = element_blank(),
                plot.background = element_blank()
            )
        p9
    }, height = 350)

    ## FUNCTIONS
    updateGraph <- function() {
        mu = as.numeric( input$mu )
        sigma = as.numeric( input$sigma )
        roll =  as.numeric( input$roll )
        list( mu=mu, sigma=sigma, roll=roll )
        click( "rodarAcao" )
    }
    
}

shinyApp( ui, server )
