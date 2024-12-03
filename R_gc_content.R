# Function to calculate GC content
calculate_gc_content <- function(sequence) {
  g_count <- sum(strsplit(sequence, "")[[1]] == "G")
  c_count <- sum(strsplit(sequence, "")[[1]] == "C")
  gc_content <- (g_count + c_count) / nchar(sequence) * 100
  return(gc_content)
}

# Function to read FASTA file and concatenate all sequences
read_fasta <- function(filename) {
  fasta_lines <- readLines(filename)
  sequences <- fasta_lines[!grepl("^>", fasta_lines)]  # Filter out header lines
  full_sequence <- paste(sequences, collapse = "")
  return(full_sequence)
}

# Main function
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) != 1) {
    stop("Usage: Rscript gc_content.R <fasta_file>")
  }
  
  fasta_file <- args[1]
  full_sequence <- read_fasta(fasta_file)
  gc_content <- calculate_gc_content(full_sequence)
  
  cat(sprintf("Overall GC Content: %.2f%%\n", gc_content))
}

# Call main function if script is run
if (interactive() == FALSE) {
  main()
}
