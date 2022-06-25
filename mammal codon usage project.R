#Load the factoextra library (nbclust library features will be enabled too)
library(factoextra)

#Make the data object cdata
cdata=codon_usage

#Eplore the dataset characteristics
str(cdata)
head(cdata)

#Use grep function to create the new dataframe with mammalia kingdom only
mamdat <- cdata[grep(c("mam"), cdata$Kingdom), ]

#Using !grepl rows containing "mitochondrion" are removed
mamdat<- mamdat[!grepl("mitochondrion", mamdat$SpeciesName),]

#Making sure Rownames are Species
rownames(mamdat)<-mamdat$SpeciesName

#Observe the structure of the finalized  mammal codon dataframe
str(mamdat)

#Before performing the actual clustering, the ideal number of cluster is determined
fviz_nbclust(mamdat[,6:16], hcut, method = "silhouette", k.max = 20) +
  labs(subtitle = "Find optimal number of clusters")

#Ideal number of clusters can be better found by having domain knowledge too

  
#Create a distance matrix object (euclidean) for first 10 codons
dmatrix=dist(mamdat[,6:16], method = "euclidean")
  
#Perform hierachical clustering using the ward.D2 linkage
hc1=hclust(dmatrix, method = "ward.D2")



#Create the dendrogram with 4 clusters based on ward.D2 linkage (with 4 different colors)
fviz_dend(hc1, cex = 0.7, k = 4,
          k_colors = c("blue", "green3", "red","purple", "orange"),
          rect = TRUE, lower_rect = -0.05, rect_lty = 5 )

#Perform hierachical clustering using the complete linkage
hcl2=hclust(dmatrix, method = "complete")

#Create the dendrogram with 4 clusters based on complete linkage  (with 4 different colors)
fviz_dend(hcl2, cex = 0.7, k = 4,
          k_colors = c("blue", "green3", "red","purple", "orange"),
          rect = TRUE, lower_rect = -0.05, rect_lty = 5 )




  
 