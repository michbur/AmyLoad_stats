require(seqinr)

source("./functions/read_AmyLoad.R")

seqs <- read_AmyLoad("./raw_data")
aa <- a()[-1]
save(aa, #names of amino acids
  seqs, # raw sequences
  file = "./data/amyloid_sequences.RData")
