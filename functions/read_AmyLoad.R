#add etiquette
# et \code{logical}, if \code{TRUE} amyloidogenic, if not \code{FALSE}
add_et <- function(x, et) 
  lapply(x, function(i) {
    attr(i, "et") <- et
    i
  })

read_AmyLoad <- function(data_path) {
  seq_neg <- add_et(read.fasta(paste0(data_path, "/amyloid_neg_full.fasta"), seqtype = "AA"), FALSE)
  seq_pos <- add_et(read.fasta(paste0(data_path, "/amyloid_pos_full.fasta"), seqtype = "AA"), TRUE)
  c(seq_pos, seq_neg)
}


