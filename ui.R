library(shiny)
library(reshape2)
library(shinythemes)


shinyUI(navbarPage(title = "AmyLoad Statistics",
                   theme = shinytheme("cerulean"),
                   id = "navbar", windowTitle = "AmyLoad Statistics", collapsible=TRUE,
                   tabPanel("n-gram analysis",
                            includeMarkdown("./ngram_analysis/ngram_analysis1.md"),
                            fluidRow(
                              column(3, sliderInput("seq_length", label = h4("Length range of sequences:"), 
                                                    min = 4, max = 83, value = c(5, 30), step = 1)),
                              column(4, sliderInput("ngram_length", label = h4("Number of elements in the n-gram:"), 
                                                    min = 1, max = 3, value = 1, step = 1))
                            ),
                            DT::dataTableOutput("freq_table"),
                            includeMarkdown("./ngram_analysis/ngram_analysis2.md"),
                            downloadButton("freq_plot_download_button", 
                                           "Save chart (.svg)"),
                            plotOutput("freq_plot")
                   )
))