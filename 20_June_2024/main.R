
library(Biostrings)
library(ape)
library(msa)

#sequences <- c("ATGCTAGCTAG", "ATGCGATGCA", "ATGCCGTA")
fasta_file <- "sequences_small.fasta"
sequences <- read.dna("sequences_small.fasta","fasta")
dna_sequences <- readDNAStringSet(fasta_file)
#dna_sequences <- DNAStringSet(sequences)
dna_sequences

# SNP
alignment <- msa(inputSeqs=dna_sequences)
alignment_matrix <- as.matrix(alignment)

ref_seq <- alignment_matrix[1,]
for (row_index in 1:2) {
  dist <- 0
  for (col_index in 1:29000) {
    is_snp <- ref_seq[col_index] != alignment_matrix[row_index,col_index]
    if (is_snp) {
      dist <- dist + 1
    }
  }
  print(dist)
}