library(shiny)
library(reshape2)
library(shinythemes)


shinyUI(navbarPage(title = "AmyLoad Statistics",
                   theme = shinytheme("cerulean"),
                   id = "navbar", windowTitle = "AmyLoad Statistics", collapsible=TRUE,
                   tabPanel("Peptide length",
                            includeMarkdown("./density_analysis/density1.md"),
                            fluidRow(
                              column(3, selectInput("dens_or_hist", label = h4("Select chart type:"), 
                                                    choices = c("histogram" = FALSE, "density" = TRUE)))
                            ),
                            plotOutput("density_plot"),
                            downloadButton("density_plot_download_button", 
                                           "Save chart (.svg)"),
                            DT::dataTableOutput("density_table"),
                            includeMarkdown("./density_analysis/density2.md"),
                            fluidRow(
                              column(3, selectInput("log_count", label = h4("Logarithm transformation of y-axis:"), 
                                                    choices = c("no" = FALSE, "yes" = TRUE))),
                              column(3, sliderInput("count_seq_length", label = h4("Length range of sequences:"), 
                                                    min = 4, max = 83, value = c(5, 30), step = 1))
                            ),
                            plotOutput("density_barplot", height = 460),
                            downloadButton("density_barplot_download_button", 
                                           "Save chart (.svg)")
                   ),
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