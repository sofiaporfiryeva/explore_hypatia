library(textmineR)
library(dendextend)

# extracting beta matrix

beta_matrix <- exp(lda_model@beta) 

dist_mx <- CalcHellingerDist(beta_matrix)
dclust <- hclust(as.dist(dist_mx), method = "ward.D")

ddend <- as.dendrogram(dclust)

# customizing the graph

library(RColorBrewer)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
 # from  https://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes

ddend %>% 
  set("branches_k_color", k = 15, value = c25) %>% 
  set("labels_col", k = 15, value = c25) %>% 
  set("branches_lwd", 2) %>% 
  plot(horiz = F, main = "Topic Clusters")
