require(ggplot2)
require(dplyr)
require(reshape2)


load("./data/amyloid_sequences.RData")


# density plot
ggplot(data.frame(len = lens, et = ets), aes(x = len)) +
  geom_density() +
  facet_wrap(~ et) +
  scale_x_continuous("Number of amino acids") +
  scale_y_continuous("Density") + 
  ggtitle("Peptide length density")


# amino acid frequency
max_length <- 7
min_length <- 5
n <- 3
d <- c(0, 1)

id_ngram <- which(sapply(ngram_id[["n"]], function(i) i == n) & sapply(ngram_id[["d"]], function(i) all(i == d)))

chosen_ngram_data <- ngrams[[id_ngram]]
chosen_ngrams <- levels(chosen_ngram_data[["ngram"]])[1L:20]

plot_dat <- filter(chosen_ngram_data, ngram %in% chosen_ngrams, len > min_length & len < max_length) %>%
  group_by(et, len, ngram) %>%
  summarize(freq = mean(freq))


ggplot(plot_dat, aes(x = et, y = freq, fill = et)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ngram) +
  scale_x_discrete("") +
  scale_y_continuous("Frequency") + 
  ggtitle("n-gram frequency")
