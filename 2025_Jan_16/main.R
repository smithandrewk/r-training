print("hello")

# Variants coming from a sample, like from the PHL
# load the csv file
variants = c("XEC","XEC","XEC","XEC","XEC","KP.3.1.1") # n=6 in our sample
# 5/6 are XEC
# 1/6 are KP.3.1.1

# Is our sample of variants from this LTCF
# the same as the population distribution (South Carolina's),
# or is it statistically, systematically, significantly,
# different _for some reason_.

# Maybe there was an outbreak.
population_counts = c("A","A","B","B","C","D","E")
population_distribution = table(population_counts) / length(population_counts)
barplot(population_distribution,xlab="Variant",ylab="Proportion",names.arg=unique(population_counts))

sample_counts <- data.frame(
  Variant = sample(c("B","C", "D"), size = 100, replace = TRUE)
)

sampling_distribution = table(sample_counts) / length(sample_counts)
barplot(sampling_distribution,xlab="Variant",ylab="Proportion",names.arg=unique(sample_counts))

t <- table(population_counts)
t['A'] <- 0
t['B'] <- table(sample_counts)['B']
t['C'] <- table(sample_counts)['C']
t['D'] <- table(sample_counts)['D']
t['E'] <- 0

t

for (variant in unique(population_counts)) {
  print(variant %in% unique(sample_counts))
}

"B" == as.vector(unique(sample_counts))


chi_squared_test <- chisq.test(t, p = population_distribution)
chi_squared_test$p.value

if (chi_square_test$p.value < 0.05) {
  print("The sampling distribution and the population are different")
} else {
  print("They are the same")
}
