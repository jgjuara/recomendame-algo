library(shiny)
library(dplyr)
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
      card(max_height = "20%", min_height = "20%",
           card_body(textInput("email", "Enter your email", ""),fill = T)
        ),
        accordion(
          multiple = T,
          accordion_panel(value = "1",title = "Book Recommendation 1:",textInput("book1",label = "", value = "")),
          accordion_panel(value = "2",title = "Book Recommendation 2:",textInput("book2",label = "", value = "")),
          accordion_panel(value = "3",title = "Book Recommendation 3:",textInput("book3",label = "", value = "")),
          accordion_panel(value = "4",title = "Book Recommendation 4:",textInput("book4",label = "", value = "")),
          accordion_panel(value = "5",title = "Book Recommendation 5:",textInput("book5",label = "", value = ""))
        ),
      actionButton("submit", "Submit")
    ),
    nav_panel(
      title = "¿Te recomiendo algo yo?",
      card("Te recomiendo esto")
    )
  )

server <- function(input, output, session) {
  user_data <- reactiveValues(data = if(file.exists("user_data.rds")) readRDS("user_data.rds") else data.frame(email = character(), book1 = character(), book2 = character(), book3 = character(), book4 = character(), book5 = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$submit, {
    new_entry <- data.frame(email = input$email, book1 = input$book1, book2 = input$book2, book3 = input$book3, book4 = input$book4, book5 = input$book5, stringsAsFactors = FALSE)
    user_data$data <- rbind(user_data$data, new_entry)
    
    saveRDS(user_data$data, "user_data.rds")
    
    output$thankYou <- renderText("Thank you for your recommendations!")
    
    all_books <- unlist(user_data$data[ ,2:6])
    all_books <- all_books[all_books != ""]
    
    user_books <- unlist(new_entry[ ,2:6])
    
    recommended_books <- sample(setdiff(all_books, user_books), 5)
    
    output$feedback <- renderText(paste("Here are 5 book recommendations for you: ", paste(recommended_books, collapse = ", ")))
  })
}

shinyApp(ui = ui, server = server)
