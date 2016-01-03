library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
load("./data/amyloid_sequences.RData")



my_theme <- theme(plot.background=element_rect(fill = "transparent",
                                               colour = "transparent"),
                  panel.grid.major = element_line(colour="grey", linetype = "dashed", size = 0.5),
                  panel.grid.major = element_line(colour="lightgrey", linetype = "dashed", size = 0.5),
                  panel.background = element_rect(fill = "transparent",colour = "black"),
                  legend.background = element_rect(fill = "NA"),
                  legend.position = "bottom",
                  strip.background = element_rect(fill = "NA", colour = "NA"))

shinyServer(function(input, output, session) {

  chosen_ngram_data <- reactive({
    n <- input[["ngram_length"]]
    d <- 0

    id_ngram <- which(sapply(ngram_id[["n"]], function(i) i == n) & sapply(ngram_id[["d"]], function(i) all(i == d)))
    ngrams[[id_ngram]]
  })
  
  
  freq_data <-reactive({
    max_length <- max(input[["seq_length"]])
    min_length <- min(input[["seq_length"]])
    
    chosen_ngrams <- levels(chosen_ngram_data()[["ngram"]])[1L:20] #DT
    filter(chosen_ngram_data(), ngram %in% chosen_ngrams, len >= min_length & len <= max_length) %>%
      group_by(et, ngram) %>%
      summarize(freq = mean(freq) * 100)
    })
  
  
  output[["freq_table"]] <- DT::renderDataTable({ 
    #     datatable(freq_data(), escape = FALSE, extensions = 'TableTools', 
    #               filter = "top", options = list(
    #                 dom = 'T<"clear">lfrtip',
    #                 tableTools = list(sSwfPath = copySWF('www'))))
    dat <- freq_data()
    colnames(dat) <- c("Status", "n-gram", "Frequency (%)")
    datatable(dat, extensions = 'TableTools', filter = "top", 
              options = list(dom = 'T<"clear">lfrtip', tableTools = list(sSwfPath = copySWF('www'))))
  })
  
  
  freq_plot <- reactive({
    dat <- freq_data()
    selected_rows <- as.numeric(input[["freq_table_rows_selected"]])
    
    if(length(selected_rows)) {
      selected_ngram <- unique(as.character(unlist(dat[selected_rows, "ngram"])))
      plot_dat <- dat[dat[["ngram"]] %in% selected_ngram, ]
      p <- ggplot(plot_dat, aes(x = et, y = freq, fill = et)) +
        geom_bar(stat = "identity") + 
        scale_x_discrete("") +
        scale_y_continuous("Frequency") + 
        scale_fill_discrete("") +
        ggtitle("n-gram frequency") +
        my_theme
      if(length(selected_ngram) > 1)
        p <- p + facet_wrap(~ ngram) 
    } else {
      p <- ggplot(data.frame()) + geom_blank() + 
        theme(panel.background = element_rect(fill = "transparent",colour = "transparent")) + 
        ggtitle("No n-grams selected")
    }
    p
  })
  
  
  output[["freq_plot_download_button"]] <- downloadHandler("freq.svg",
                                                           content = function(file) {
                                                             ggsave(file, freq_plot(), device = svg, height = 210, width = 297,
                                                                    units = "mm")
                                                           })
  
  output[["freq_plot"]] <- renderPlot({
    freq_plot()
  })

  
})