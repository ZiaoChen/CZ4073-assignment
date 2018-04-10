# ======================
# Assignment 3
# Name: Chen Ziao
# Matric No.: U1420681G
# ======================

# ==========
# Problem 1
# ==========

# 0.Activate Required Packages
# install.packages("jsonlite")    # read JSON file in R
# install.packages("tm")          # text mining in R
# install.packages("factoextra")  # advanced clustering result analysis
# install.packages("irlba")       # efficient pca
# install.packages("fifer")       # stratified sampling
library(fifer)
library(factoextra)
library(jsonlite)
library(tm)
library(irlba) # Use this library instead of r's orginial pca because it is more efficient
library(mclust)
library(cluster)

# 1 Read and Preprocess Data
# 1.1 Load the dataset from JSON format
cuisData <- read_json("assign3_CuisineData.json", simplifyVector = TRUE)
str(cuisData)
head(cuisData)

# 1.2 Remove the c() wrapper from the list of ingredients
cuisData$ingredients <- substring(cuisData$ingredients, 3, nchar(cuisData$ingredients)-1)

# 1.3 Remove anything present in brackets (mostly explanations and amounts) 
cuisData$ingredients <- gsub("\\s*\\([^\\)]+\\)","",cuisData$ingredients)

# 1.4 Remove the space following the comma between ingredients
cuisData$ingredients <- gsub(', ',',',cuisData$ingredients)

# 1.5 Remove the space between two words of the same ingredient
cuisData$ingredients <- gsub(' ','',cuisData$ingredients)

# 1.6 Remove any hyphen present within the same ingredient
cuisData$ingredients <- gsub('-','',cuisData$ingredients)
head(cuisData)

# 1.7 Create the corpus of terms (dictionary) for ingredients
cuisCorpus <- Corpus(VectorSource(cuisData$ingredients))
inspect(cuisCorpus[1:5])

# 1.8 Translate all letters to lower case
cuisCorpus <- tm_map(cuisCorpus, tolower)

# 1.9 Create the DTM from the Corpus
cuisDTM <- DocumentTermMatrix(cuisCorpus)
inspect(cuisDTM[1:5,])

# 1.10 Create DataFrame from the DTM
cuisDF <- as.data.frame(as.matrix(cuisDTM))

# 2.Exploratory Data Analysis
# 2.1 Column Names of DTM
names(cuisDF)  # Dictonary of tokens

# 2.2 Structure of DTM
str(cuisDF)

# 2.3 Summary of DTM
# Too big to show output
# summary(cuisDF)

# 2.4 Statistics on Cuisine Ingredients
hist(rowSums(cuisDF)) 
# From the plot, we can see that most cuisines have around 5 to 15 ingredients
# Only very few cuisines (<5000) has less than 5 ingredients
# Alo very few cuisines (<1000) has more than 20 ingredients
# Since the DTM is high dimensional data, it is hard to identify outliers.
# Hence, no instance is dropped.


# 2.5 DTM Dimensions (Sparse Matrix)
nrow(cuisDF)
ncol(cuisDF)

# 2.6 Principle Component Analysis
# The DTM data frame is too large to perform clustering
# Dimensionality reduction is performed on the DTM data frame
# Since PCA takes long time to reduce to high dimension, only first 200 principle componenets are computed and kept
# The dimensionality is reduced from (39774,6704) to (39774, 300)
start <- Sys.time()
set.seed(4073)
PCA <- prcomp_irlba(cuisDF, n=300) # This statement takes around 20 minutes to finish
cuisDFPCA <- as.data.frame(PCA$x)
print(Sys.time() - start)

# 3.Model Building
# 3.1 K-means Clustering
kMin <- 1
kMax <- 20
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)
set.seed(4073)

for (K in kMin:kMax) {
  print(K)
  kMeansFit <- kmeans(cuisDFPCA, centers = K, nstart = 10)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}

plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, betweenSS, pch=19, type="b", col="green")
# Elbow point: 2, 6, 8, 10

# Since the dataset is still too large to be processed by Hierarchical Clustering and EM Clustering
# Stratified Random Sampling is performed based on K-means clustering using 10 as number of clusters
# 3.3 Stratified Random Sampling
# 3.3.1 10 clusters K-means Clustering
kMeansFit <- kmeans(cuisDFPCA, centers = 10, nstart = 10)

# 3.3.2 Append clustering result as a new column
cuisDFPCA$cluster <- kMeansFit$cluster

# 3.3.3 20% stratified Sampling using clustering result
set.seed(4073)
cuisDFPCASS <- stratified(cuisDFPCA, "cluster", 0.1)

# 3.3.4 Try k-means again with stratified sampled data
fviz_nbclust(cuisDFPCASS, kmeans, method = "wss", k.max=20)
# Elbow Point: 2, 3, 8, 11

set.seed(4073)
fviz_nbclust(cuisDFPCASS, kmeans, method = "silhouette", k.max=20)
# Elbow Point: 4, 7, 11

# 3.4 Hierarchical Clustering
# Due to time complexity of hierarchical clustering, the sample needs to be smaller
# 5% stratified random sampling is applied
set.seed(4073)
cuisDFPCASS <- stratified(cuisDFPCA, "cluster", 0.05)
fviz_nbclust(cuisDFPCASS, hcut, method = "wss", k.max=20)
# Elbow points: 2, 3, 10

set.seed(4073)
fviz_nbclust(cuisDFPCASS, hcut, method = "silhouette", k.max=20)
# Elbow points:  2, 12

# 4 Conclusion
# According to the result from 3.1, 3.3.4 and 3.4, the optimal number of clusters is 2

# ==========
# Problem 2
# ==========

# 1.Read and Preprocess Data
surveyData <- read.csv("responses.csv", header=TRUE)

# 2.Exploratory Data Analysis
# 2.1 Check Data Dimension
nrow(surveyData)
ncol(surveyData)

# 2.2 Column Names of Survey Data
names(surveyData)  # Dictonary of tokens

# 2.3 Structure of Survey Data
str(surveyData)

# 2.4 Summary of Survey Data
summary(surveyData)

# 2.5 Inpute Missing Values
# 2.5.1 Check columns with missing values
for (i in names(surveyData)){
  print(i)
  print(sum(is.na(surveyData[[i]])))
}
# For each column, the number of missing values ranging from 0 to 20, which is quite small
# Therefore, no columns are dropped

# 2.5.2 Impute Missing value use median
impute <- function(x){
  x<-as.numeric(x) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  return(x)
}
for (i in names(surveyData)){
  surveyData[[i]] <- impute(surveyData[[i]])
}

# 3.Model Building
# 3.1 K-means Clustering
kMin <- 1
kMax <- 30
withinSS <- double(kMax - kMin + 1)
betweenSS <- double(kMax - kMin + 1)
set.seed(4073)
for (K in kMin:kMax) {
  print(K)
  kMeansFit <- kmeans(surveyData, centers = K, nstart = 10)
  withinSS[K] <- sum(kMeansFit$withinss)
  betweenSS[K] <- kMeansFit$betweenss
}
plot(kMin:kMax, withinSS, pch=19, type="b", col="red",
     xlab = "Value of K", ylab = "Sum of Squares (Within and Between)")
points(kMin:kMax, betweenSS, pch=19, type="b", col="green")
# Elbow points: 2, 3, 7, 9

# Use factoextra library method
fviz_nbclust(surveyData, kmeans, method = "wss", k.max=30)
# Elbow points: 4, 6, 10, 13, 16

set.seed(4073)
fviz_nbclust(surveyData, kmeans, method = "silhouette", k.max=30)
# Elbow points: 5, 7, 10, 14

# 3.2 Hierarchical Clustering
fviz_nbclust(surveyData, hcut, method = "wss", k.max=30)
# Elbow points: 2, 6

set.seed(4073)
fviz_nbclust(surveyData, hcut, method = "silhouette", k.max=30)
# Elbow points: 5, 6, 7, 12

# 3.3 EM Clustering
d_clust <- Mclust(surveyData, G=1:30, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

emFit <- Mclust(surveyData)
summary(emFit)
plot(emFit, what = "classification")
emFit$parameters
# Top 3 model based on BIC is cluster 1, which does not make sense

# 3.4 PAM Clustering
set.seed(4073)
fviz_nbclust(surveyData, pam, method = "wss", k.max=30)
# Elbow points: 3, 5, 7, 9, 11

set.seed(4073)
fviz_nbclust(surveyData, pam, method = "silhouette", k.max=30)
# Elbow points: 5, 7, 9, 11

# 4. Conclusion
# Based on all elbow points in 3.1, 3.2 and 3.4, the optimal number of clusters should be 6 or 7

# 5. Find the strongest variable
# 5.1 Check whether Gender plays a important role
kMeansFit <- kmeans(surveyData, centers = 7, nstart = 10)
surveyData$cluster <- kMeansFit$cluster
plot(surveyData[surveyData$cluster == 1,]$Gender)
plot(surveyData[surveyData$cluster == 2,]$Gender)
plot(surveyData[surveyData$cluster == 3,]$Gender)
plot(surveyData[surveyData$cluster == 4,]$Gender)
plot(surveyData[surveyData$cluster == 5,]$Gender)
plot(surveyData[surveyData$cluster == 6,]$Gender)
plot(surveyData[surveyData$cluster == 7,]$Gender)

# From the plot, it seems that gender plays an important role in clustering because
# for different clusters, the distribution of gender is quite different

# 5.2 Find important variables
# For each variable, calculate the average coefficient of variation (CV) among all 7 clusters
# The higher the CV is, the less concentrated of the data is
# If after clustering, the CV for a variable decreases a lot
# Then it means that this variable plays an important role in clustering
cvDif <- 0
importantVariable <- "cluster"
for(i in names(surveyData)){
  cvAfterClustering <- 0
  for (j in 1:7){
    m <- mean(surveyData[surveyData$cluster == 1,][[i]])
    s <- sd(surveyData[surveyData$cluster == 1,][[i]])
    cvAfterClustering <- cvAfterClustering + s/m
  }
  
  cvAfterClustering <- cvAfterClustering / 7
  cvBeforeClustering <- sd(surveyData[[i]]) / mean(surveyData[[i]])
  cvDifTemp <- (cvBeforeClustering - cvAfterClustering) / cvBeforeClustering
  if(cvDifTemp> cvDif && i != "cluster"){
    cvDif <- cvDifTemp
    importantVariable <- i
  }
}

print(cat("The most important variable which has highest percentage decrement in CV is", importantVariable))
# Variable "WeightNULL" is the most important variable in clustering

