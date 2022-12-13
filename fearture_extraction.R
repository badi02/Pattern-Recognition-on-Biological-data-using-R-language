library(stringi)
library(stringr)
ID <- c(aquization$ID) # nolint
# Create vector contains the AA characters

vector <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", "N", "P", "Q", "R", "S", "T", "V", "W","Y") # nolint
vector_combinasion <- vector("character", length = 400)
j <- 1
while (j < 400) {
  for(k in 1:length(vector)){ # nolint
    for (l in 1:length(vector)) { # nolint
      vector_combinasion[j] <- str_c(vector[k], vector[l])
      j <- j + 1
    }
  }
}
vector_combinasion

AAChars <- c(vector, vector_combinasion)  # nolint
#AAChars
FE <- NULL # nolint
for(i in 1:length(AAChars)){ # nolint
  vec <- str_count(aquization$Sequence, AAChars[i])
  FE <- c(FE, vec) # nolint
}
dimnames <- list(ID, AAChars)
result_matrix <- matrix(data = FE, nrow = 22332, ncol = 420, byrow = FALSE, dimnames = dimnames) # nolint
 # nolint
