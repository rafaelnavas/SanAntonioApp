
library(abind)
library(shiny)
library(plotly)
library(shinyWidgets)
library(png)
library(raster)

dec = readRDS("dec.RDS")
flow = readRDS("flow.RDS")
utm = readRDS("utm.RDS")
xy_sp_dec = readRDS("xy_sp_dec.RDS")
xy_sp_utm = readRDS("xy_sp_utm.RDS")
xy_utm = readRDS("xy_utm.RDS")
base = readRDS("base.RDS")
logo = readRDS("logo.RDS")
qtl = readRDS("qtl.RDS")
xy_dec = readRDS("xy_dec.RDS")

meses = c("enero","febrero",
          "marzo","abril",
          "mayo","junio",
          "julio","agosto",
          "septiembre","octubre",
          "noviembre","diciembre")


ui <- navbarPage("San Antonio App",
                 
                 tabPanel("Inicio",
                          mainPanel(
                              h3("Esta aplicación muestra las curvas de duración de caudales a lo largo del cauce del arroyo San Antonio - Salto (Uruguay). \n\n"),
                              h3("Han sido desarrolladas mediante modelación hidrológica distribuída WFLOW-HBV en el período 1991-2020. \n\n"),
                              h3("Es parte del proyecto Hacia una Gestión Integrada de los Recursos Hídricos en Sistemas Hidrológicos Altamente Antropizados: Arroyo San Antonio - Acuífero Salto/Arapey. \n\n"),
                              h3("Financiamiento: Agencia Nacional de Investigación e Innovación, Fondo María Viñas 2017."),
                              h3("Contacto: Rafael Navas (rafaelnavas23@gmail.com)"),
                              plotOutput("logoplot1")

                          )
                 ),
                 
                 tabPanel("Modo de uso",
                          mainPanel(
                              h3("Instrucciones: \n\n"),
                              h4("Ingrese el mes de consulta (menu desplegable panel izquierdo superior). \n\n"),
                              h4("Ingrese las coordenadas haciendo clic con el botón izquierdo del ratón en el mapa. \n\n"),
                              h3("Interpretación: \n\n"),
                              h4("El cuadro del panel izquierdo le muestra las coordenadas de los puntos A y B así como la distancia A-B. \n\n"),
                              h4("La curva de duración de caudales (CDC) indica la probabilidad de que un caudal dado sea igualado o excedido. \n\n"),
                              h4("En azul la CDC para el caudal diario en el período 1991-2020. \n\n"),
                              h4("En rojo la CDC para el caudal diario del mes de consulta (definido en el paso 3). \n\n"),
                              h4("Si la curva roja ese ubica por arriba de la curva azul, la probabilidad de excedencia de un caudal dado en el mes de consulta será mayor que en el período. \n\n"),
                              h4("Casos interesantes son: enero y agosto (más probable caudales bajos), octubre (más probable caudales altos), abril (mes muy variable, más probable altos y bajos). \n\n")
                          )
                 ),
                 
                 tabPanel("Aplicación",
                         sidebarPanel(

                              fluidRow(selectInput(inputId = "mes",
                                                   label=NULL,
                                                   choices = meses,
                                                   selected = "enero")),

                              fluidRow(plotOutput("out2",click = "plot_click")),
                              fluidRow(verbatimTextOutput("info"))
                          ),
                         
                         mainPanel(
                             fluidRow(plotlyOutput('out3')),
                             fluidRow(plotOutput('logoplot2'))
                         )
                )
)

server <- function(input, output) {

    output$logoplot1 <- renderPlot({
        par(mar=c(2,0,0,0))
        plot(c(1,dim(logo)[2]),c(1,dim(logo)[1]), type="n",
             xaxt="n",xlab="", yaxt="n",ylab="",bty="n")
        rasterImage(logo,1,1,dim(logo)[2],dim(logo)[1])
    })

    output$logoplot2 <- renderPlot({
        par(mar=c(2,0,0,0))
        plot(c(1,dim(logo)[2]),c(1,dim(logo)[1]), type="n",
             xaxt="n",xlab="", yaxt="n",ylab="",bty="n")
        rasterImage(logo,1,1,dim(logo)[2],dim(logo)[1])
        #plot(1:100)
    })    
    
    val <- reactiveValues(clickx = -57.8, clicky = -31.3)
    
    observe({
        input$plot_click
        isolate({
            val$clickx = c(val$clickx, input$plot_click$x)
            val$clicky = c(val$clicky, input$plot_click$y)     
        })
    }) #adding clicks to list
 

    output$out2 <- renderPlot({
        x = val$clickx[length(val$clickx)]
        y = val$clicky[length(val$clickx)]
        xu = 5932.2215 +  95.2997*x
        yu = 9977.3713 + 109.9222*y
        dd = sqrt((xu - xy_utm[,1])^2 + (yu - xy_utm[,2])^2)
        mm = order(dd)[1]

        par(mar=c(5,5,1,0))
        plot(xy_sp_dec,pch=".",cex=2,col="blue",las=2,axes=T, ylim=c(-31.4,-31.25))
        rasterImage(base, -58.028,-31.46,-57.63,-31.216, interpolate = F)
        plot(xy_sp_dec,pch=15,cex=0.5,col="blue",add=T)

        isolate({
            points(val$clickx[length(val$clickx)], val$clicky[length(val$clickx)],pch=19,col=2,cex=2)
            points(xy_dec[mm,1],xy_dec[mm,2],pch=19,col=2,cex=2)
            lines(c(val$clickx[length(val$clickx)],xy_dec[mm,1]),
                 c(val$clicky[length(val$clickx)],xy_dec[mm,2]),
                 col=2,lty=2)
            text(val$clickx[length(val$clickx)], val$clicky[length(val$clickx)], "A",pos=4)
            text(xy_dec[mm,1],xy_dec[mm,2],"B",pos=2)
        })

    })
    
    output$info <- renderText({
        x = val$clickx[length(val$clickx)]
        y = val$clicky[length(val$clickx)]
        xu = 5932.2215 +  95.2997*x
        yu = 9977.3713 + 109.9222*y
        dd = sqrt((xu - xy_utm[,1])^2 + (yu - xy_utm[,2])^2)
        mm = order(dd)[1]

        paste0("Coordenadas A: ", round(val$clickx[length(val$clickx)],3), "ºW ",round(val$clicky[length(val$clickx)],3),"ºS", "\n",
               "Coordenadas B: ", round(xy_dec[mm,1],3), "ºW ",round(xy_dec[mm,2],3),"ºS", "\n",
               "Distancia A-B: ", round(dd[mm],1)," km")
        
    })
    
    
    output$out3 <- renderPlotly({
        x = val$clickx[length(val$clickx)]
        y = val$clicky[length(val$clickx)]
        xu = 5932.2215 +  95.2997*x
        yu = 9977.3713 + 109.9222*y
        dd = sqrt((xu - xy_utm[,1])^2 + (yu - xy_utm[,2])^2)
        mm = order(dd)[1]

        fig = plotly_empty()

        mesN = match(input$mes,meses)

        fig <- fig %>% add_lines(x = 1000*flow[-1,mm], y = 100:1,
                                 type = 'scatter',
                                 mode = 'line',
                                 line = list(color = 'blue', width = 2),
                                 name="Período 1991-2020")

        fig <- fig %>% add_lines(x = qtl[-1,mm,mesN], y = 100:1,
                                 type = 'scatter',
                                 mode = 'line',
                                 line = list(color = 'red', width = 2),
                                 name=paste(meses[mesN]))

        fig <- fig %>% layout(yaxis = list(title="Probabilidad de Excedencia (%)",
                                           showticklabels = TRUE,
                                           ticks="outside",
                                           showline=TRUE,
                                           showgrid=TRUE,
                                           type="norm"),

                              xaxis = list(title="Caudal (lps)",
                                           showticklabels = TRUE,
                                           ticks="outside",
                                           showline=TRUE,
                                           showgrid=TRUE,
                                           #range=c(0.01,0.6),
                                           type="log")
        )
        
        fig
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


