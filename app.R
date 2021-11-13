#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library('rsconnect')
library('shinythemes')
library('shinyWidgets')
library('ggplot2')
library('dplyr')
library('shiny')
library('stringr')
library('DT')
library('plotly')
library('emo')
library('shinydashboard')


ui <- fluidPage( 
    theme = shinytheme("flatly"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "pers.css")),
    
    navbarPage(title = paste0("App Master Ciencia de Datos ", ji('computer')),
                 
                 header = tagList(
                     useShinydashboard()
                 ),
                 
                 tabPanel(paste("Inicio",ji('home')),
                          br(),br(),
                          box( width = 12, status = "warning", solidHeader = T, collapsible = T,collapsed = F,
                               h3('Pablo Hernández Cámara')),
                          box(width = 12, status = "warning", solidHeader = T, collapsible = T,collapsed = F,
                                includeHTML("www/inicio.html"))
                          
                          
                 ),
                 
                 tabPanel(title = paste0("Selección de máquina ", ji('open_file_folder')), 
                          br(),br(),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(h1('MÁQUINA'),
                                                     fileInput(inputId = "inputdata",
                                                               label = "Selecciona un fichero",
                                                               accept = NULL),
                                                     uiOutput(outputId = 'selectbox')),
                                        mainPanel(h3('Probabilidad de orden'),
                                                  plotlyOutput(outputId = "graficoporden"),
                                                  textOutput(outputId = 'activealarms')
                                                  ))),
                 
                 navbarMenu(title = paste0("Estado de máquina ", ji('chart')),
                            tabPanel(title = paste0("Evolución temporal alarmas ", ji('calendar')),
                                     sidebarLayout(position = 'left',
                                                   sidebarPanel(h1('ALARMAS radiobuttons'),
                                                                uiOutput(outputId = 'radiobuttonsalarm')),
                                                   mainPanel(h3('Evolución temporal alarmas'),
                                                             plotlyOutput(outputId = "graficoevolucion"),
                                                             textOutput(outputId = 'activealarms_2')
                                                             ))),
                            
                            tabPanel(title = paste0("Registros de la máquina ", ji('page_facing_up')),
                                     sidebarLayout(position = 'left',
                                                   sidebarPanel(h1('ALARMAS checkbox'),
                                                                uiOutput(outputId = "alarmcheckbox")),
                                                   mainPanel(h3('Registros de la máquina seleccionada'),
                                                             dataTableOutput(outputId = 'tabla'))))),
                 
                 tabPanel(title = paste0("Estadísticas Globales Temporales ", ji('chart_with_upwards_trend')),
                          sidebarLayout(position = 'left',
                                        sidebarPanel(h1('PERIODO Y ESTADÍSTICAS'),
                                                     h2('INTERVALO DE FECHAS'),
                                                     uiOutput(outputId = 'seleccionfechas'),
                                                     h2('GRÁFICO'),
                                                     radioButtons(inputId = 'selecthistboxplot',
                                                                  label = NULL,
                                                                  choices = c("Boxplot" = "box", "Histograma" = "hist")),
                                                     h2('ALARMA'),
                                                     uiOutput(outputId = 'selectbox_2'),                             
                                                     h2('HISTOGRAMA'),
                                                     uiOutput(outputId = 'slider'),
                                                     h2('BOXPLOT'),
                                                     checkboxInput(inputId = 'boxplottodas',
                                                                   label = 'Todas las máquinas',
                                                                   value = FALSE)),
                                        mainPanel(h3('Aqui seleccionaremos el grafico elegido de la alarma seleccionada'),
                                                  plotOutput(outputId = "histboxplot"),)))
))


server <- function(input, output) {
    
    maquinas_data <- reactive({
        req(input$inputdata)
        a <- input$inputdata
        nombre_variable <- load(a$datapath)   
        data <- eval(parse(text = nombre_variable))
        return(data)
    })
    
    output$selectbox <- renderUI({
        data <- maquinas_data()
        selectInput(inputId = 'seleccionmaquinas',
                    label = 'Selecciona la máquina',
                    choices = data$matricula,
                    selected = 'a')
    })
    
    noactivealarms <- reactive({
        data <- maquinas_data()
        matricula <- input$seleccionmaquinas
        data <- data[data$matricula == matricula,]
        alarmas <- colnames(data)[str_detect(colnames(data), '^a')]
        data <- na.omit(data)
        noactivealarms <- c()
        i <- 1
        for (alarm in alarmas){
            col <- (data[,alarm] != 0)
            if(length(col[col == TRUE]) == 0){
                noactivealarms[i] <- alarm
                i <- i + 1
            }
        }
        return(noactivealarms)
    })
    
    output$activealarms <- renderText({
        data <- maquinas_data()
        alarmas <- colnames(data)[str_detect(colnames(data), '^a')]
        paste('Número alarmas activadas para esta máquina: ', (length(alarmas)-length(noactivealarms())), 
              'Número de alarmas no activadas para esta máquina: ', length(noactivealarms()))
    })
    
    output$activealarms_2 <- renderText({
        data <- maquinas_data()
        alarmas <- colnames(data)[str_detect(colnames(data), '^a')]
        paste('Número alarmas activadas para esta máquina: ', (length(alarmas)-length(noactivealarms())), 
              'Número de alarmas no activadas para esta máquina: ', length(noactivealarms()))
    })
    
    output$graficoporden <- renderPlotly({
        data <- maquinas_data()
        matricula <- input$seleccionmaquinas
        data <- data[data$matricula == matricula,]
        ggplot(data = data, aes(dia, p_orden)) +
            geom_line() +
            geom_point(aes(colour = p_orden)) +
            scale_color_gradient(low = "blue", high = "red")
    })
    
    output$radiobuttonsalarm <- renderUI({
        data <- maquinas_data()
        radioButtons(inputId = 'radiobuttons',
                     label = 'Selecciona la alarma a visualizar',
                     choices = colnames(data)[str_detect(colnames(data), '^a')],
                     selected = 'a1')
    })
    
    output$graficoevolucion <- renderPlotly({
        data <- maquinas_data()
        alarma <- input$radiobuttons
        matricula <- input$seleccionmaquinas
        data <- data[data$matricula == matricula,]
        datos_alarma <- data[,alarma]
        ggplot(data, aes(dia, datos_alarma)) +
            geom_line() +
            geom_point(aes(colour = datos_alarma)) +
            scale_colour_gradient(low = "blue", high = "red") +
            ylab(alarma)
    })
    
    output$alarmcheckbox <- renderUI({
        data <- maquinas_data()
        checkboxGroupInput(inputId = 'checkbox',
                           label = 'Selecciona las alarmas para ver en la tabla',
                           choices = colnames(data)[str_detect(colnames(data), '^a')],
                           inline = FALSE,
                           selected = 'a1')
    })
    
    maquinas_tabla <- reactive({
        data <- maquinas_data()
        matricula <- input$seleccionmaquinas
        data <- data[data$matricula == matricula,]
        alarma <- input$checkbox
        columnas <- paste(c('matricula', 'dia', alarma, 'p_orden'))
        data <- select(data, columnas)
        data <- na.omit(data)
        return(data)
    })
    
    output$tabla <- renderDataTable(
        datatable(maquinas_tabla(),
                  rownames = FALSE,
                  class = 'cell-border stripe',
                  options = list(searchHighlight = TRUE,
                                 initComplete = JS("function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': 'Coral', 'color': '#fff'});",  #000
                                                   "}"))) %>% formatStyle(names(maquinas_tabla()),
                                                                          target = 'row',
                                                                          color = 'black',
                                                                          backgroundColor = 'white')
    )
    
    output$seleccionfechas <- renderUI({
        data <- maquinas_data()
        alarma <- input$radiobuttons
        matricula <- input$seleccionmaquinas
        data <- data[data$matricula == matricula,]
        last_date <- tail(data$dia, n = 1)
        first_date <- head(data$dia, n = 1)
        dateRangeInput(inputId = 'daterange',
                       label = 'Selecciona el periodo', 
                       start = '2016-01-02',
                       end = '2016-12-14',
                       min = first_date, 
                       max = last_date,
                       weekstart = 1,
                       language = "es",
                       separator = "a")
    })
    
    output$selectbox_2 <- renderUI({
        data <- maquinas_data()
        selectInput(inputId = 'selectalarma',
                    label = 'Alarma',
                    choices = colnames(data)[str_detect(colnames(data), '^a')],
                    selected = 'a1')
    })
    
    output$slider <- renderUI({
        sliderInput(inputId = 'selectbins',
                    label = "Ancho del bin del histograma",
                    min = 1,
                    max = 50,
                    value = 1,
                    step = 1)
    })
    
    output$histboxplot <- renderPlot({
        if (input$selecthistboxplot == 'hist') {
            data <- maquinas_data()
            alarma <- input$selectalarma
            matricula <- input$seleccionmaquinas
            data <- data[data$matricula == matricula,]
            rangofechas <- input$daterange
            fecha_min <- rangofechas[1]
            fecha_max <- rangofechas[2]
            data <- filter(data, dia >= fecha_min, dia <= fecha_max)
            y <- data[,alarma]
            anchobins <- input$selectbins
            ggplot(data, aes(y)) +
                geom_histogram(binwidth = anchobins, color = "black", fill = 'coral') +
                ylab('count') + 
                xlab(alarma)
        }
        else {
            data <- maquinas_data()
            todas <- input$boxplottodas
            alarma <- input$selectalarma
            rangofechas <- input$daterange
            fecha_min <- rangofechas[1]
            fecha_max <- rangofechas[2]
            data <- filter(data, dia >= fecha_min, dia <= fecha_max)
            if (todas == FALSE){
                matricula <- input$seleccionmaquinas
                data <- data[data$matricula == matricula,]
                y <- data[,alarma]
                ggplot(data, aes(x = matricula, y = y)) +
                    geom_boxplot(color = "black", fill = 'coral', alpha = 0.8) +
                    xlab(alarma) +
                    ylab('eval(ALA)')
            }
            else {
                y <- data[,alarma]
                ggplot(data, aes(x = matricula, y = y)) +
                    geom_boxplot(color = "black", fill = 'coral', alpha = 0.8) +
                    ylab('eval(ALA)') +
                    theme(axis.text.x = element_text(angle = 90)) +
                    xlab(alarma)
            }
        }
    })
    
    
}






# Run the application 
shinyApp(ui = ui, server = server)
