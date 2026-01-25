install.packages("rentrez")
library(rentrez)

search_results <- entrez_search(db = "nucleotide", term = "SARS-CoV-2[ORGN] AND South Carolina[Location]", retmax = 5)

genbank_ids <- search_results$ids

sequences <- entrez_fetch(db = "nucleotide", id = genbank_ids, rettype = "fasta")

cat(sequences,file="sequences.fasta")

library(Biostrings)
seqs <- readDNAStringSet("sequences.fasta")

seqs
library(msa)
msa(seqs,method="Muscle")

# Print aligned sequences
print(alignment, show="alignment")

library(ape)
alignment <- readDNAStringSet("alignment.aln")
alignment <- read.dna("alignment.aln", format="fasta")
alignment
# Convert to msa format
msa_alignment <- msa(alignment)

# Print alignment
print(msa_alignment, show="alignment")

# Convert to matrix
alignment_matrix <- as.matrix(as.character(alignment))

# View the alignment matrix
print(alignment_matrix)
