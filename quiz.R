# quiz.R
quizUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("quiz_page")),
    br(),
    fluidRow(
      column(4, actionButton(ns("prev_btn"), "Previous", class = "btn btn-secondary")),
      column(4, align = "center", actionButton(ns("submit"), "Submit", class = "btn btn-primary")),
      column(4, align = "right", actionButton(ns("next_btn"), "Next", class = "btn btn-secondary"))
    ),
    tags$style(HTML("
      .btn { margin-top: 10px; width: 100px; }
      .shiny-modal .modal-content { border-radius: 10px; }
      .modal-header { background-color: #f0f8ff; }
    "))
  )
}

quizServer <- function(id, questions_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shuffled_questions <- reactiveVal(sample(1:nrow(questions_df)))
    current_index <- reactiveVal(0)
    user_answers <- reactiveValues()
    
    observeEvent(input$next_btn, {
      if (current_index() < nrow(questions_df)) {
        current_index(current_index() + 1)
      }
    })
    
    observeEvent(input$prev_btn, {
      if (current_index() > 0) {
        current_index(current_index() - 1)
      }
    })
    
    output$quiz_page <- renderUI({
      idx <- current_index()
      
      if (idx == 0) {
        return(
          tagList(
            h2("Welcome to the Light and Health Quiz!"),
            p("This quiz tests your knowledge about the nonvisual effects of ocular light."),
            p("Use the 'Previous' and 'Next' buttons to navigate."),
            p("Choose an answer and click 'Submit' to check."),
            p("Good luck!")
          )
        )
      }
      
      if (idx > length(shuffled_questions())) {
        return(
          tagList(
            h2("Quiz Complete!"),
            p("Well done! You've answered all the questions."),
            icon("trophy", class = "fa-3x text-success")
          )
        )
      }
      
      qn_idx <- shuffled_questions()[idx]
      question <- questions_df[qn_idx, ]
      input_id <- paste0("q", qn_idx)
      correct_ans <- unlist(strsplit(question$correct, ";"))
      options <- unlist(strsplit(question$options, ";"))
      is_multi <- length(correct_ans) > 1
      
      ans_ui <- if (is_multi) {
        checkboxGroupInput(ns(input_id), label = question$question, choices = options, selected = user_answers[[input_id]])
      } else {
        radioButtons(ns(input_id), label = question$question, choices = options, selected = user_answers[[input_id]])
      }
      
      tagList(ans_ui)
    })
    
    observeEvent(input$submit, {
      idx <- current_index()
      if (idx == 0 || idx > length(shuffled_questions())) return()
      
      qn_idx <- shuffled_questions()[idx]
      input_id <- paste0("q", qn_idx)
      user_input <- input[[input_id]]
      user_answers[[input_id]] <- user_input
      
      correct_ans <- unlist(strsplit(questions_df[qn_idx, ]$correct, ";"))
      feedback_right <- questions_df[qn_idx, ]$feedback_correct
      feedback_wrong <- questions_df[qn_idx, ]$feedback_wrong
      
      is_correct <- setequal(sort(user_input), sort(correct_ans))
      title <- if (is_correct) "Correct!" else "Try Again!"
      msg <- if (is_correct) feedback_right else feedback_wrong
      
      showModal(modalDialog(
        title = span(icon(if (is_correct) "check-circle" else "times-circle"), " ", title),
        p(msg),
        footer = tagList(
          actionButton(ns("retry"), "Retry", class = "btn btn-warning"),
          actionButton(ns("continue"), "Continue", class = "btn btn-success")
        ),
        easyClose = FALSE
      ))
    })
    
    observeEvent(input$retry, {
      removeModal()
      qn_idx <- shuffled_questions()[current_index()]
      input_id <- paste0("q", qn_idx)
      user_answers[[input_id]] <- NULL
    })
    
    observeEvent(input$continue, {
      removeModal()
      if (current_index() < nrow(questions_df)) {
        current_index(current_index() + 1)
      }
    })
  })
}