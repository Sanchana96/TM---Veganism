library(dplyr)
library(ggplot2)
library(tm)
library(stringr)
library(wordcloud)
library(SnowballC)
library(caTools)
library(textstem)
library(factoextra)
library(geosphere)
library(ggdendro)

# Load the data
#data <- read.csv("C:/Users/Sanchu/Desktop/MS/ML/TextMining/Vegan_News.csv",header = FALSE,sep =",")
data <- read.csv("C:/Users/Sanchu/Downloads/cleanedveganism.csv")
data <- na.omit(data)
colnames(data) <- c("Label","Tweet")

# Extract the tweet columns
data_headline <- as.list(data$Tweet)

# create a document term matrix
data_DTM <- DocumentTermMatrix(data_headline,
                          control = list(
                            stopwords = TRUE, ## remove normal stopwords
                            wordLengths=c(2, 25), ## get rid of words of len 2 or smaller or larger than 15
                            removePunctuation = TRUE,
                            removeNumbers = TRUE,
                            tolower=TRUE
                            #stemming = TRUE,
                          ))

inspect(data_DTM)

#Create matrix and dataframe form the dtm
Headline_DF_DT <- as.data.frame(as.matrix(data_DTM))
Headline_DF_DT <-na.omit(Headline_DF_DT)
Headline_DTM_mat <- as.matrix(data_DTM)                 

# Compute the distance matrix
dist_C_dataheadline1 <- dist(distCosine(Headline_DF_DT))
dist_C_dataheadline1
dist_C_dataheadline <- as.dist(distance(as.matrix(scale(t(Headline_DF_DT))), method="cosine",use.row.names = TRUE))


# Apply hierarchical clustering
HClust_Ward_CosSim <- hclust(dist_C_dataheadline, method="ward.D2")


# Plot dendrogram
plot(HClust_Ward_CosSim, cex=.6, hang=-1,main = "Cosine Sim")
rect.hclust(HClust_Ward_CosSim, k=5)

plot(HClust_Ward_CosSim)
rect.hclust(HClust_Ward_CosSim, 2, border="red") 
rect.hclust(HClust_Ward_CosSim, 4, border="blue")
rect.hclust(HClust_Ward_CosSim, 5, border="darkgreen")
