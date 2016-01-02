require(seqinr)
require(biogram)

source("./functions/read_AmyLoad.R")

seqs <- read_AmyLoad("./raw_data")
ets <- sapply(seqs, function(i) attr(i, "et"))
lens <- lengths(seqs)

aa <- a()[-1]

calc_freqs <- function(seq, n, u, d, lens) {
  as.matrix(count_ngrams(seq = seq, n = n, u = u, d = d, pos = FALSE, scale = FALSE, threshold = 0))/(lens - n - sum(d) + 1)
}

seq_mat <- list2matrix(seqs)

ns <- c(1, 
        2, 2, 2, 2, 
        3, 3, 3)
ds <- list(0, 
           0, 1, 2, 3, 
           c(0, 0), c(1, 0), c(0, 1))

ngrams <- pblapply(1L:8, function(i)
  calc_freqs(seq_mat, ns[i], aa, ds[[i]], lens)
)



save(aa, #names of amino acids
  seqs, # raw sequences
  ngrams = ngram,
  file = "./data/amyloid_sequences.RData")
