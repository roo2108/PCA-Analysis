setwd("")
data<-read.csv("insurance_risk_data.csv",header=T)

#STRUCTURE OF THE DATA
str(data)
summary(data)

#BOXPLOT FOR OUTLIER DETECTION
boxplot(data$Risk.Score,main = "Boxplot of Risk Score Index for Outlier Detection",horizontal=TRUE,frame.plot=F,pch=16, col = "white",ylim=c(0,100))

#IDENTIFYING CASES WITH MISSINGNESS
sum(is.na(data))

#STANDARDISING THE DATA
Data1<-scale(data)

#APPLYING PCA
results<-princomp(Data1)

#SUMMARY OF ANALYSIS
summary(results,loadings=TRUE,digits=2)

#EIGENVALUES
results$sdev^2

#CUMULATIVE VARIANCE EXPLAINED
cumulative.var<- cumsum(results$sdev^2 / sum(results$sdev^2))
plot(cumulative.var, type = "b", pch = 19, col = "#4472C4",xlab = "Principal Components", ylab = "Cumulative Variance Explained",main = "Cumulative Variance Explained by Principal Components")
abline(h = 0.7, col = "firebrick2", lty = 2)

#SCREE PLOT OF VARIANCE
library(factoextra)
library(ggplot2)
SP<-fviz_eig(results,addlabels = TRUE, ylim = c(0,50), main = "Scree Plot - Variance Explained by Principal Components",barfill = "#4472C4", linecolor = "firebrick2") + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
SP + ggtitle("Scree Plot - Variance Explained by Principal Components") + theme(plot.title = element_text(face = "bold", hjust = 0.5))

#BIPLOT
B<-fviz_pca_biplot(results, 
                          label = "var",
                          col.var = "contrib",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                          repel = TRUE) +
  labs(title = "PCA Biplot: PC1 vs PC2") +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
B +  ggtitle("Biplot: PC1 vs PC2") + theme(plot.title = element_text(face = "bold", hjust = 0.5))

#PREPARATION FOR A HEATMAP
loadings<-results$loadings
loadings_df <- as.data.frame(loadings[, 1:5])  
loadings_df$Variable <- rownames(loadings_df)  

#RESHAPING THE DATA FOR A GGPLOT(long format)
library(reshape2)
loadings_long <- melt(loadings_df, id.vars = "Variable", variable.name = "Component", value.name = "Loading")

#HEATMAP OF LOADINGS
GP<-ggplot(loadings_long, aes(x = Component, y = Variable, fill = Loading)) + geom_tile(color="white") +  scale_fill_gradient2(low = "steelblue4", mid = "white", high = "firebrick3", midpoint = 0) + labs(title = "Heatmap of PCA Loadings",x = "Principal Components",y = "Variables",fill = "Loading Value") +theme_minimal() +  theme(axis.text.x = element_text(angle = 45, hjust = 1))
GP + theme(plot.title = element_text(face = "bold", hjust = 0.5))


