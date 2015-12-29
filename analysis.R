require(seqinr)
require(ggplot2)
require(dplyr)
require(reshape2)

source("./functions/read_AmyLoad.R")

seqs <- read_AmyLoad("./data")
ets <- sapply(seqs, function(i) attr(i, "et"))
lens <- lengths(seqs)

# density plot
ggplot(data.frame(len = lens, et = ets), aes(x = len)) +
  geom_density() +
  facet_wrap(~ et)


# amino acid frequency
max_length <- 7
min_length <- 5
aa_freq <- data.frame(t(sapply(seqs[lens > min_length & lens < max_length], function(single_seq) 
  as.vector(table(factor(single_seq, levels = a()[-1])))/length(single_seq))))
colnames(aa_freq) <- a()[-1]
aa_freq[["et"]] <- ets[lens > min_length & lens < max_length]

maa_freq <- melt(aa_freq, variable.name = "aa", value.name = "freq")

agg_freq <- group_by(maa_freq, et, aa) %>%
  summarise(mfreq = mean(freq))

ggplot(agg_freq, aes(x = et, y = mfreq, fill = et)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ aa)
