#installing packages
install.packages("rjson",  dependencies = T)
library(rjson)

# Loading JSNOL data

# Open the gzipped file connection

conn <- gzfile("/Users/sp/Desktop/R_Project_Hypatia/Hypatia_JSONL.gz", "r")

# Creating an empty list to store the parsed JSON objects
json_data <- list()

# Read each line and parse it as JSON
i <- 1
while (length(line <- readLines(conn, n = 1)) > 0) {
  json_data[[i]] <- fromJSON(line)
  i <- i + 1
}

# Close the connection
close(conn)


# Chunking data
chunks <- split(json_data, ceiling(seq_along(json_data)/100))

# Exploring data
element_names <- names(chunks[[1]][[1]])

# Printing elements names
# for (name in element_names) {
#  cat(name, "\n")
# }

