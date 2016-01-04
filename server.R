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
  
  density_dat <- reactive({
    data.frame(len = lens, et = ets)
  })
  
  output[["density_table"]] <- DT::renderDataTable({ 
    dat <- group_by(density_dat(), et) %>% 
      summarize(n = length(len), mean = mean(len), sd = sd(len), median = median(len), mad = mad(len))
    colnames(dat) <- c("Status", "Number of peptides", "Mean", "SD", "Median", "MAD")
    datatable(dat)
  })
  
  
  
  density_plot <- reactive({
    p <- ggplot(density_dat(), aes(x = len, fill = et)) +
      facet_wrap(~ et) +
      scale_x_continuous("Number of amino acids") +
      scale_fill_discrete("") +
      my_theme
    
    if(input[["dens_or_hist"]]) {
      p + geom_density() +
        scale_y_continuous("Density") +
        ggtitle("Peptide length density chart")
    } else {
      p + geom_histogram() +
        scale_y_continuous("Number of peptides") +
        ggtitle("Peptide length histogram")
    }
  })
  
  output[["density_plot"]] <- renderPlot({
    density_plot()
  })
  
  output[["density_plot_download_button"]] <- downloadHandler("length.svg",
                                                              content = function(file) {
                                                                ggsave(file, density_plot(), device = svg, height = 210, width = 297,
                                                                       units = "mm")
                                                              })
  
  density_barplot <- reactive({
    dat <- density_dat()
    
    max_length <- max(input[["count_seq_length"]])
    min_length <- min(input[["count_seq_length"]])
    
    plot_dat <- filter(dat, len >= min_length & len <= max_length) %>%
      group_by(len, et) %>% summarise(count = length(len))
    
    p <- ggplot(plot_dat, aes(x = len, fill = et, y = count, label = count)) +
      geom_bar(stat="identity") +
      facet_wrap(~ et, ncol = 1) +
      scale_x_continuous("Number of amino acids") +
      scale_fill_discrete("") +
      scale_y_continuous("Count") + 
      geom_text(vjust = 0) +
      my_theme
    
    if(input[["log_count"]]) {
      p <- p + scale_y_log10("Log(Count)") 
    }
      
    p
  })
  
  output[["density_barplot"]] <- renderPlot({
    density_barplot()
  })
  
  output[["density_barplot_download_button"]] <- downloadHandler("counts.svg",
                                                              content = function(file) {
                                                                ggsave(file, density_barplot(), device = svg, height = 210, width = 297,
                                                                       units = "mm")
                                                              })
  
  
  chosen_ngram_data <- reactive({
    n <- input[["ngram_length"]]
    d <- 0
    
    id_ngram <- which(sapply(ngram_id[["n"]], function(i) i == n) & sapply(ngram_id[["d"]], function(i) all(i == d)))
    ngrams[[id_ngram]]
  })
  
  
  freq_data <-reactive({
    max_length <- max(input[["seq_length"]])
    min_length <- min(input[["seq_length"]])
    
    filter(chosen_ngram_data(), len >= min_length & len <= max_length) %>%
      group_by(et, ngram) %>%
      summarize(freq = mean(freq) * 100)
  })
  
  
  output[["freq_table"]] <- DT::renderDataTable({ 
    dat <- freq_data()
    colnames(dat) <- c("Status", "n-gram", "Frequency (%)")
    datatable(dat, extensions = 'TableTools', filter = "top", 
              options = list(dom = 'T<"clear">lfrtip', tableTools = list(sSwfPath = copySWF('www'))))
  })
  
  
  freq_plot <- reactive({
    dat <- freq_data()
    selected_rows <- as.numeric(input[["freq_table_rows_selected"]])

    if(length(selected_rows) > 0) {
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
      p <- ggplot(dat, aes(x = et, y = freq, fill = et)) + geom_blank() + 
        ggtitle("No n-grams selected") +
        scale_x_discrete("") +
        scale_y_continuous("Frequency") + 
        scale_fill_discrete("") +
        my_theme
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