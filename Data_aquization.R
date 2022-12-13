# import libraries
library(xlsx)
library(dplyr)
library(ggplot2)

# function to read excel file
read_xlsx <- function(i, b) {
  # i is sheetindex of the file you wanna read
  # b is a logical variable if the file is header, b = TRUE then b = FALSE
  xlsx_file <- read.xlsx(file = file.choose(),sheetIndex = i, header = b) # nolint
}


# function to get the final result of data aquization
aquization <- function(xlsx_file) {
  if (!is.data.frame(xlsx_file)) {
    uniprot.cytokines <- as.data.frame(xlsx_file) # nolint
  }else {
    uniprot.cytokines <- xlsx_file # nolint
  }
  uniprot.cytokines <- as.data.frame(cbind(ID = uniprot.cytokines$Entry, # nolint
                                          Name = uniprot.cytokines$Protein.name,
                                          Specie = uniprot.cytokines$Organism,
                                          Sequence = uniprot.cytokines$Sequence)) # nolint
  View(uniprot.cytokines)
  return(uniprot.cytokines)
}


xlsx_file <- read_xlsx(i = 1, b = TRUE)
data_aquisated <- as.data.frame(xlsx_file)
View(data_aquisated)

aquization <- aquization(xlsx_file)
str(aquization)
