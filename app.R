library(shiny)
library(tidyverse)
library(bslib)

ui <- page_navbar(
  theme = bs_theme(
    bg = "#e5e5e5", fg = "#0d0c0c", primary = "#dd2020",
    base_font = font_google("Press Start 2P"),
    code_font = font_google("Press Start 2P"),
    "font-size-base" = "0.75rem", "enable-rounded" = FALSE
  ) %>%
    bs_add_rules(
      list(
        sass::sass_file("nes.min.css"),
        sass::sass_file("custom.scss"),
        "body { background-color: $body-bg; }"
      )
    ),
  nav_panel(
    title = "Recomendame algo amigue",
      column(8,
             accordion(
               multiple = T,
               accordion_panel(value = "1",
                               title = "Prólogo",
                               p(""),
                               p("Sólo te pido el mail para evitar recomendarte algo que vos mismx pusiste")
               )),
            card(max_height = "20%", min_height = "100px",
                 card_body(textInput("email", "Pasame tu mail", ""),fill = T)
              ),
            accordion(
              multiple = T,
              accordion_panel(value = "1",
                              title = "1era recomendación",
                              textInput("libro1",
                                        label = "Título y autor del cuento",
                                        value = "", width = '100%'),
                              textAreaInput("explicacion1",
                                        label = "¿Por qué lo recomendás?",
                                        value = "", width = '100%')
                              ),
              accordion_panel(value = "2",
                              title = "2da recomendación",
                              textInput("libro2",
                                        label = "Título y autor del cuento",
                                        value = "", width = '100%'),
                              textAreaInput("explicacion2",
                                        label = "¿Por qué lo recomendás?",
                                        value = "", width = '100%')
              ),
              accordion_panel(value = "3",
                              title = "3era recomendación",
                              textInput("libro3",
                                        label = "Título y autor del cuento",
                                        value = "", width = '100%'),
                              textAreaInput("explicacion3",
                                        label = "¿Por qué lo recomendás?",
                                        value = "", width = '100%')
              ),
              accordion_panel(value = "4",
                              title = "4ta recomendación",
                              textInput("libro4",
                                        label = "Título y autor del cuento",
                                        value = "", width = '100%'),
                              textAreaInput("explicacion4",
                                        label = "¿Por qué lo recomendás?",
                                        value = "", width = '100%')
              ),
              accordion_panel(value = "5",
                              title = "5ta recomendación",
                              textInput("libro5",
                                        label = "Título y autor del cuento",
                                        value = "", width = '100%'),
                              textAreaInput("explicacion5",
                                        label = "¿Por qué lo recomendás?",
                                        value = "", width = '100%')
              ),
            ),
           actionButton("submit", "Guardar recomendaciones", width = '100%'),
      offset = 2
      ),
    column(2)
    ),
    nav_panel(
      title = "¿Te comparto unas recomendaciones?",
      column(8,
             card(max_height = "20%", min_height = "100px",
                  card_body(textInput("email2", "Decime tu email", ""),fill = T), 
             ),
             br(),
             actionButton("askRecomendation", "Pedir recomendaciones", width = '100%'),
             card(card_body(uiOutput("recomendaciones"))),
             offset = 2
      ),
      column(2)
    )
  )

server <- function(input, output, session) {

  observeEvent(input$submit, {
    respuestas <- reactive({
      x <- reactiveValuesToList(input)
      x <- x[grep("email|libro|explicacion",names(x))]
      x <- lapply(x, function(x) {if(is.null(x)) {""} else {x}})
      x
      
    })

    df <- as_tibble(respuestas())
    
    df <- df %>% 
      mutate(across(everything(), as.character))

    readr::write_csv(df, glue::glue("data/{session$token}-user-data-strftime(Sys.time(), '-%Y%m%d-%H%M%S%z')}.csv"),
                     quote = "all", eol = "\n")
    
    showModal(modalDialog(
      title = NULL, size = "s",
      div(p("Recomendaciones guardadas.", br(), p("¡Gracias por compartir!")),
          modalButton("Cerrar")),
      easyClose = TRUE,
      footer = NULL
    ))    
    
  })
  
  observeEvent(input$askRecomendation, {

    
    archivos <- list.files("data", full.names = T)
    
    rptas <- map(archivos, read_csv)
    
    rptas <- bind_rows(rptas)
    
    # filtrar las respuestas del propio usuario y generar subset
    
    rptas <- rptas %>% filter(! email %in% input$email2)
    
    rptas <- rptas %>% 
      pivot_longer(cols = -email) %>% 
      mutate(id = gsub("[^\\d]", "",name, perl = T),
             name = gsub("\\d", "", name, perl = T))
    
    rptas <- left_join(rptas[rptas$name == "libro",],
              rptas[rptas$name == "explicacion",], by = c("email", "id")) 
      
    rptas <- rptas[!is.na(rptas$value.x),]
    
    rptas <- rptas %>% 
      slice_sample(n = 5, replace = F)
    
    items <- map2(rptas$value.x, rptas$value.y,
                  .f =  function(x,y) {
                    accordion_panel(title = tags$b(x), y, value = x)
                  })
    
    # gurdar csv con la combinacion de recomendaciones y el mail de a quien se le recomendó eso
    
    # generar salida descarble
    
    output$recomendaciones <- renderUI({
      
      
      accordion(!!!items)
    })
    
    
  })
  

}

shinyApp(ui = ui, server = server)
