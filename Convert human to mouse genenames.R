# Convert between mouse and human gene lists

library(tidyverse)

# function to count the number of entries in a tibble
count_entries <- function(input_frame, col = 1){
  input_frame <- input_frame[order(input_frame[,col][[1]]),]
  input_frame.len <- length(input_frame[,col][[1]])
  counted_homologs <- tibble(name = c(input_frame[1,col][[1]]), count = c(col))
  j = 1
  for(i in 2:input_frame.len){
    if(input_frame[i,col][[1]] == input_frame[i-1,col][[1]]){
      counted_homologs$count[j] <- counted_homologs$count[j] + 1
    }else{
      counted_homologs <- add_row(counted_homologs, name = input_frame[i,col][[1]], count = 1)
      j = j + 1
    }
  }
  return(counted_homologs)
}

# Function to take human homologs and output mouse versions
# Default uses the list provided by 
# JAX at http://www.informatics.jax.org/homology.shtml
# you can use the HGNC IDs directly  in the format <HGNC:#>, as it is in the 
# JAX list, and you will need to adjust the index_input_key input to 7
human_to_mouse_homologs <- function(gene_index, gene_name_list, index_input_key = 4, index_DB_key = 1, index_organism_col = 2, input_organism = "human", output_organism = "mouse, laboratory"){
  output_tibble <- gene_index[0,]
  input_length <- length(gene_name_list)
  input_index <- gene_index[which(gene_index[,index_organism_col][[1]] == input_organism),]
  output_index <- gene_index[which(gene_index[,index_organism_col][[1]] == output_organism),]
  for(i in 1:input_length){
    key_name <- which(input_index[,index_input_key][[1]] == gene_name_list[i])
    key_name <- input_index[key_name,index_DB_key][[1]]
    for(j in 1:length(key_name)){
      output_tibble <- add_row(output_tibble, output_index[which(output_index[,index_DB_key] == key_name[j]),])
    }
  }
  return(output_tibble)
}

gene_index <- read_tsv("HOM_MouseHumanSequence.rpt")
head(gene_index)
names(gene_index)

counted_homologs <- count_entries(gene_index)
counted_homologs <- count_entries(gene_index, 4)
length(counted_homologs[,1][[1]])
max(counted_homologs[,2][[1]])
counted_homologs[which(counted_homologs$count > 20),]
ggplot(counted_homologs, aes(x = name, y = count)) +
  geom_point()

gene_list <- read_csv("Senescence gene list.csv")
human_only_genes <- gene_list[which(!is.na(gene_list$Human_Gene_Symbol)),]
human_only_genes_list <- unique(human_only_genes$Human_Gene_Symbol)
mouse_homologs <- human_to_mouse_homologs(gene_index = gene_index, gene_name_list = human_only_genes_list)
db_counts <- count_entries(mouse_homologs)
max(db_counts[,2])
view(mouse_homologs[which(mouse_homologs[,1] == db_counts[which(db_counts[,2] == 3),1][[1]]),])

