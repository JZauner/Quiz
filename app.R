# app.R
library(shiny)
library(readxl)
library(bslib)

source("quiz.R")

questions_df <- read_excel("www/quiz_questions_multichoice.xlsx", col_types = rep("text", 5))
colnames(questions_df) <- c("question", "options", "correct", "feedback_correct", "feedback_wrong")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel("Light and Human Biology Quiz"),
  quizUI("quiz1")
)

server <- function(input, output, session) {
  quizServer("quiz1", questions_df)
}

shinyApp(ui, server)