#THE MOST APPROPRIATE CLUSTER SOLUTION SEEMS TO BE WITH UNSCALED RESULTS, but report scaled results using the cluster allocation (testCluster)
#cols4PCA
#cols : variables for clustering
#cols4Scaling

#testCluster from dataL - logs for press, no logs for SVI
#testClusterR from dataR - no logs for press, no logs for SVI
#testClusterScaled from dataL -> testCluster - scaled 
#testClusterRScaled from dataR->testClusterR->scaled


## do K-Means cluster analysis on combinations of visibility variables
#check ClusterMeans from 2015-01-01 - no zeros???
summary(dataL[,c("PressAllCluster", "PressPapersCluster")])
#LIBRARIES ARE IN MODULE 1


elbowMethod<-function (x) {#function to calculate elbow method
  
  wss <- (nrow(x)-1)*sum(apply(x,2,var))
  for(i in 1:25){wss[i] <- sum(kmeans(x, centers=i)$withinss)}
  plot(1:25, wss, type="b", xlab="No. of Clusters", ylab="wss")  #plotting the curve
  return (wss)
}

elbowMethodBSS<-function (x) {#function to calculate elbow method
  
  bss <- (nrow(x)-1)*sum(apply(x,2,var))
  for(i in 1:25){bss[i] <- sum(kmeans(x, centers=i)$betweenss)}
  plot(1:25, bss, type="b", xlab="No. of Clusters", ylab="bss")  #plotting the curve
  return (bss)
}



#testCluster - is a copy of dataL for clustering (to save memory)
#res - the results of NbClust (algorithm to determine the optimal number of clusters)


#Principal component analysis: needs to be log transformed and scaled first

#1. cross-section clustering

#1.0 Kmeans - how to select variables for clustering

#1.1. Clustering pooled  


#- Elbow method
#fpc - cluster assessment
#- NbClust - model based clustering

#1.1.1.Remove  observations with all 0s into a separate cluster
#- NbClust
#- Elbow method
#start with a minimum number of variables and then extend: 
#statistics to report: sse; cluster centres
#visualisation of clusters



#1.2. Clustering per month
#______________________________________
#1.1.1.Remove  observations with all 0s  into a separate cluster
#- NbClust
#- Elbow method


#2. model based evaluation of clustering  



#2. time series clustering

#trying using NbCLust- package to determine the best number of clusters

#1. pooled clustering
#clustering variables assignmnent
#cols<-colnames(dataL[,c("PressAll", 

#                             "SVI_NEW"), with=F])

cols<-colnames(dataL[,c(
  
  "PressPapers", 
  "PressWires", 
  "PressPapersMajor", 
  "PressWiresMajor", 
  "SVI_NEW", 
  "SVI_AU", 
  "SVI_ASX"
), with=F]) 

#The below set is just for PCA - it can be omitted if not PCA is done
cols4PCA<-colnames(dataL[,c("date",  "code",
                            
                            "PressAll", #PC1
                            "PressPapers", #PC2
                            "PressWires", #PC3
                            "PressPapersMajor", #PC4
                            "PressWiresMajor", #PC5
                            
                            "SVI_NEW",#PC8
                            "SVI_AU", #PC7
                            "SVI_ASX", #PC6
                            
                            "EPS1NE"), with=F]) #PC9


cols4Scaling<-colnames(dataL[,c(
  "SVI_ASX", 
  "SVI_AU", 
  "SVI_NEW",
  "PressAll", 
  "PressPapers", 
  "PressWires", 
  "PressPapersMajor", 
  "PressWiresMajor", 
  
  "EPS1NE"), with=F]) 

rm(testCluster, testClusterR, testClusterScaled, testClusterRScaled)
testCluster<-(dataL[, .SD, .SDcols=cols]) #copy observations so it is more manageable re size
testClusterR<-(dataR[, .SD, .SDcols=cols]) #copy observations so it is more manageable re size
testClusterScaled<-as.data.table(scale(testCluster)) #SCALING old way with no codes or dates, Scaling makes the results blurred
testClusterRScaled<-as.data.table(scale(testClusterR))
#GO TO CLUSTERING ASSIGNMENT

#testClusterScaled<-testCluster[, (cols4Scaling):=scale(.SD), .SDcols=cols4Scaling]
#basicStats(testCluster[, c(-1, -2)])



#PCA
#PCA with scaling
results <- prcomp(testCluster[, c(-1, -2)],
                  center = TRUE,
                  scale. = TRUE) 
print(results)
plot(results, type = "l")
summary(results)

#PCA without scaling
results <- prcomp(testCluster[, c(-1, -2)],
                  center = TRUE,
                  scale. = FALSE)  #Note that scale = TRUE cannot be used if there are zero or constant (for center = TRUE) variables.
print(results)
plot(results, type = "l")
summary(results)
par(mar = rep(2, 4))
plot(results) #plot results differently to show the main component



#__________________ varimax rotation


fit <- principal(testClusterScaled[, c(-1, -2)], nfactors=2, rotate="oblimin")
fit <- principal(testCluster[, c(-1, -2)], nfactors=2, rotate="oblimin") #Scaled and notScaled produce the same solution
print(fit)

#_______________________ exploratory factor analysis

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit <- factanal(testCluster[, c(-1, -2)], 2, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(testCluster),cex=.7) # add variable names

#____________________________CLUSTERING

#move on to clustering
##only those variables are kept which showed greater importance in PCA and above
##factor1 (>0.80), factor 2 (>0.8)
#THE SET BELOW IS FOR CLUSTERING!



#clusterCenter <- attr(testCluster, "scaled:center")  
#clusterScale <- attr(testCluster, "scaled:scale")

#scaling with dates and codes
#testCluster[, -c("date","code")]<-scale(testCluster[, -c("date","code"), with=FALSE]) #SCALING!!!!

#using the Elbow method
elbowMethod (testCluster)
elbowMethodBSS (testCluster)

elbowMethod (testClusterR) # suggested 4 (?)
elbowMethodBSS (testClusterR)


elbowMethod (testClusterScaled)
elbowMethodBSS (testClusterScaled)

elbowMethod (testClusterRScaled) #suggested 5 (?!)
elbowMethodBSS (testClusterRScaled)

#____________ CLUSTER ASSIGNMENT____________
dataC<-dataL
setkey(dataC, code, date)

#testCluster - cluster solution 1 
# number 3 was proposed to be the best number of clusters
##clustering with log press, not log SVI
fit.km <- kmeans(testCluster, 3, nstart=25)   #Cluster 1: middle, Cluster 2: high, Cluster 3: low
fit.km$centers
fit.km$size
export<-fit.km$cluster
testCluster[,"ClusterKMeansLog"]<-export
dataC[,"ClusterKMeansLog"]<-export
testCluster[, .(mean(PressPapers), median (PressPapers), mean (SVI_NEW), median (SVI_NEW)), by=ClusterKMeansLog]
#ClusterKMeansLog        V1        V2       V3 V4
#1:                3 1.1673528 0.6931472 40.09454 39
#2:                1 0.7195102 0.0000000 11.84647 12
#3:                2 1.7310110 1.6094379 66.86209 69

#testClusterR - cluster solution 2 
##clustering with no logs for press or SVI
#4 cluster solution
fit.km <- kmeans(testClusterR, 4, nstart=25)   #Cluster 1: middle, Cluster 2: high, Cluster 3: low
fit.km$centers
fit.km$size
export<-fit.km$cluster
testClusterR[,"ClusterKMeansR4"]<-export
testCluster[,"ClusterKMeansR4"]<-export #to add all cluster allocations to one dataset for comparison
dataC[,"ClusterKMeansR4"]<-export
testClusterR[, .(mean(PressPapers), median (PressPapers), mean (SVI_NEW), median (SVI_NEW)), by=ClusterKMeansR4]
#ClusterKMeansR4        V1 V2       V3 V4
#1:               4  3.714529  1 39.08322 38
#2:               1  2.748133  0 11.42488 11
#3:               2  4.333843  2 65.81187 67
#4:               3 41.454454 47 56.48323 56

#testClusterR - cluster solution 3 
##clustering with no logs for press or SVI
#3 cluster solution
fit.km <- kmeans(testClusterR, 3, nstart=25)   #Cluster 1: middle, Cluster 2: high, Cluster 3: low
fit.km$centers
fit.km$size
export<-fit.km$cluster
testClusterR[,"ClusterKMeansR3"]<-export
testCluster[,"ClusterKMeansR3"]<-export #to add all cluster allocations to one dataset for comparison
dataC[,"ClusterKMeansR3"]<-export
testClusterR[, .(mean(PressPapers), median (PressPapers), mean (SVI_NEW), median (SVI_NEW)), by=ClusterKMeansR3]
#PressPapers PressWires PressPapersMajor PressWiresMajor    SVI_NEW     SVI_AU     SVI_ASX
#1  -0.4514143 -0.4384850       -0.4447696      -0.3758464 -0.7447522 -0.7449290 -0.23646205
#2  -0.2272141 -0.2733215       -0.3037235      -0.3409058  0.8259324  0.7481243  0.02872357
#3   1.5052030  1.5468849        1.6117376       1.4977494  0.5481404  0.6739800  0.55036199

#testClusterScaled- cluster solution 4
##clustering with log press, not log SVI - scaled
##clustering with all scaled variables 
fit.km <- kmeans(testClusterScaled, 3, nstart=25)   #Cluster 1: middle, Cluster 2: high, Cluster 3: low
fit.km$centers
fit.km$size
export<-fit.km$cluster
testClusterScaled$ClusterKMeansScaled<-export
testCluster[,"ClusterKMeansScaled"]<-export #to add all cluster allocations to one dataset for comparison
dataC[,"ClusterKMeansScaled"]<-export
testClusterR[,"ClusterKMeansScaled"]<-export #to add all cluster allocations to one dataset for comparison  - so I can get unscaled means/medians
testClusterScaled[, .(mean(PressPapers), median (PressPapers), mean (SVI_NEW), median (SVI_NEW)), by=ClusterKMeansScaled]
#ClusterKMeansScaled         V1         V2         V3         V4
#1:                   2 -0.2272141 -0.3381452  0.8259324  0.7664825
#2:                   1 -0.4514143 -0.8995078 -0.7447522 -0.8153727
#3:                   3  1.5052030  1.6038502  0.5481404  0.5167159

#testClusterRScaled- cluster solution 5
##clustering with no logs for press or SVI - scaled
##clustering with all scaled variables 
fit.km <- kmeans(testClusterRScaled, 3, nstart=25)   #Cluster 1: middle, Cluster 2: high, Cluster 3: low
fit.km$centers
fit.km$size
export<-fit.km$cluster
testClusterRScaled$ClusterKMeansRScaled<-export
testCluster[,"ClusterKMeansRScaled"]<-export #to add all cluster allocations to one dataset for comparison
dataC[,"ClusterKMeansRScaled"]<-export
testClusterRScaled[,"ClusterKMeansRScaled"]<-export #to add all cluster allocations to one dataset for comparison  - so I can get unscaled means/medians
testClusterRScaled[, .(mean(PressPapers), median (PressPapers), mean (SVI_NEW), median (SVI_NEW)), by=ClusterKMeansRScaled]
#testClusterRScaled         V1         V2         V3         V4
#1:                  3 -0.1768495 -0.4475506  0.8494510  0.8081102
#2:                  2 -0.3575976 -0.5290464 -0.7115257 -0.7737449
#3:                  1  2.3484804  2.9752729  0.6746855  0.6415992

write.csv(dataC, file="testing\\CopyMainDatasets\\dataC.csv", row.names=FALSE)
write.csv(testCluster, file="testing\\CopyMainDatasets\\testCluster.csv", row.names=FALSE)
write.csv(testClusterR, file="testing\\CopyMainDatasets\\testClusterR.csv", row.names=FALSE)
write.csv(testClusterScaled, file="testing\\CopyMainDatasets\\testClusterScaled.csv", row.names=FALSE)
write.csv(testClusterRScaled, file="testing\\CopyMainDatasets\\testClusterRScaled.csv", row.names=FALSE)

#distribution of clusters across months
clusterDistribution<-dataC[,.N, by=c("date", "ClusterKMeansLog")]
write.csv(clusterDistribution, file="testing\\clusterDistributionLog.csv")

clusterDistribution<-dataC[,.N, by=c("date", "ClusterKMeansR4")]
write.csv(clusterDistribution, file="testing\\clusterDistributionR4.csv")

clusterDistribution<-dataC[,.N, by=c("date", "ClusterKMeansR3")]
write.csv(clusterDistribution, file="testing\\clusterDistributionR3.csv")

clusterDistribution<-dataC[,.N, by=c("date", "ClusterKMeansScaled")]
write.csv(clusterDistribution, file="testing\\clusterDistributionScaled.csv")

clusterDistribution<-dataC[,.N, by=c("date", "ClusterKMeansRScaled")]
write.csv(clusterDistribution, file="testing\\clusterDistributionScaledR.csv")


#transition matrix for each cluster #you read the matrix from left to right (NOT from top to bottom)
trans.matrix_full(dataC, "ClusterKMeansLog", TRUE) #transition matrix for clusters 

trans.matrix_full(dataC, "ClusterKMeansR4", TRUE) #transition matrix for clusters 

trans.matrix_full(dataC, "ClusterKMeansR3", TRUE) #transition matrix for clusters 

trans.matrix_full(dataC, "ClusterKMeansScaled", TRUE) #transition matrix for clusters 

trans.matrix_full(dataC, "ClusterKMeansRScaled", TRUE) #transition matrix for clusters 

#mean and median for clusters in ClusterKMeansLog 
ClusterKMeansLog.Mean<-testCluster[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansLog] #means for clusters (same as centres)
ClusterKMeansLog.Median<-testCluster[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansLog] #median for clusters
ClusterKMeansLog.SD<-testCluster[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansLog] #sd for clusters
testCluster[, .N, by=ClusterKMeansLog] #number of observations in a cluster
##scaled mean and median for clusters in ClusterKMeansLog
testCluster[, (cols):=lapply(.SD, scale), .SDcols=cols][,lapply(.SD, mean), by=ClusterKMeansLog]

#mean and median for clusters in ClusterKMeansR4
ClusterKMeansR4.Mean<-testClusterR[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansR4] #means for clusters (same as centres)
ClusterKMeansR4.Median<-testClusterR[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansR4] #median for clusters
ClusterKMeansR4.SD<-testClusterR[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansR4] #sd for clusters
ClusterKMeansR4[, .N, by=ClusterKMeansR4] #number of observations in a cluster

#mean and median for clusters in ClusterKMeansR3
ClusterKMeansR3.Mean<-testClusterR[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansR3] #means for clusters (same as centres)
ClusterKMeansR3.Median<-testClusterR[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansR3] #median for clusters
ClusterKMeansR3.SD<-testClusterR[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansR3]#sd for clusters
ClusterKMeansR3[, .N, by=ClusterKMeansR3] #number of observations in a cluster


#mean and median scaled
ClusterKMeansScaled.Mean<-testClusterScaled[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansScaled] #means for clusters (same as centres)
ClusterKMeansScaled.Median<-testClusterScaled[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansScaled] #median for clusters 
ClusterKMeansScaled.SD<-testClusterScaled[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansScaled] #sd for clusters (same as centres)
#mean and median unscaled
ClusterKMeansScaled2.Mean<-testClusterR[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansScaled] #means for clusters (same as centres)
ClusterKMeansScaled2.Median<-testClusterR[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansScaled] #median for clusters 
ClusterKMeansScaled2.SD<-testClusterR[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansScaled] #median for clusters 

ClusterKMeansScaled2[, .N, by=ClusterKMeansScaled] #number of observations in a cluster

#mean and median for clusters in ClusterKMeansRScaled
ClusterKMeansRScaled.Mean<-testClusterRScaled[, lapply(.SD, mean), .SDcols=cols, by=ClusterKMeansRScaled] #means for clusters (same as centres)
ClusterKMeansRScaled.Median<-testClusterRScaled[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansRScaled] #median for clusters
ClusterKMeansRScaled.SD<-testClusterRScaled[, lapply(.SD, sd), .SDcols=cols, by=ClusterKMeansRScaled]#sd for clusters
ClusterKMeansRScaled[, .N, by=ClusterKMeansRScaled] #number of observations in a cluster

#mean and median for the whole sample
testClusterScaled[, lapply(.SD, mean), .SDcols=cols] #means for the whole sample
testClusterScaled[, lapply(.SD, median), .SDcols=cols, by=ClusterKMeansScaled] #median for the whole sample

rm(ClusterDistribution, testCluster, testClusterR, testClusterScaled, visibilityT)
rm(ClusterKMeansLog.Mean, ClusterKMeansLog.Median, ClusterKMeansLog.Mean, ClusterKMeansR4.Median, ClusterKMeansR3.Mean, ClusterKMeansR3.Median, ClusterKMeansScaled.Mean, ClusterKMeansScaled.Median )

#ALTERNATIVE VERSION
# append cluster assignment
#dataL$Cluster <- fit$cluster
#alternative assignment using data.table
#test<-dataL[, kmeans(as.matrix(scale(cbind(.SD))), 4, nstart=20), .SDcols=cols]
#test$centers
#dataC<-data.frame(dataL, test$centers)

#____________ CLUSTER ASSIGNMENT END____________


## Visualize the clusters - needs to be DONE TODAY!!!!
#fit.km$cluster <- as.factor(fit.km$cluster)
#testCluster<-as.data.frame(testCluster)
#ggplot(testCluster, aes(PressAll, SVI_NEW, color = fit.km$cluster)) + geom_point()

#-_____ pamk from fpc Package

#PAM is similar to K-Means, but it is more robust with outliers. It can be implemented using pam() from cluster package.
library(cluster)
results<-pam(testCluster, 4)
summary(results)
export<-results$cluster
dataC[,"ClusterPAM"]<-export

##calls the function pam or clara to perform a partitioning around medoids clustering with 
##the number of clusters estimated by optimum average silhouette width (see pam.object) or Calinski-Harabasz index (calinhara).
results<-pamk(testCluster, krange=1:10, criterion="multiasw", usepam=FALSE, critout=TRUE)
summary(results)
str(results)
results$nc

##the number of clusters estimated by optimum average silhouette width (see pam.object) or Calinski-Harabasz index (calinhara).
results<-pamk(testCluster, krange=1:15, criterion="ch", usepam=FALSE, critout=TRUE)
summary(results)
str(results)
results$nc



#___________________ hierarchical clustering
# Ward Hierarchical Clustering
d <- dist(testCluster, method = "euclidean") # distance matrix
fit.hc <- hclust(d, method="ward.D2") 
plot(fit.hc) # display dendogram
groups <- cutree(fit.hc, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(fit.hc, k=4, border="red")

#---------------FPC-------------
#using clusterboot()

# set the desired number of clusters                               
kbest.p<-5       

#   Run clusterboot() with hclust 
#   ('clustermethod=hclustCBI') using Ward's method 
#   ('method="ward"') and kbest.p clusters 
#   ('k=kbest.p'). Return the results in an object 
#   called cboot.hclust.
testCluster<-data.table(testCluster)


#   The results of the clustering are in 
#   cboot.hclust$result. The output of the hclust() 
#   function is in cboot.hclust$result$result. 
#
#   cboot.hclust$result$partition returns a 
#   vector of clusterlabels. 

results<- clusterboot(testCluster,B=3,bootmethod=
                        c("boot","noise","jitter"),clustermethod=kmeansCBI,
                      krange=4,seed=15555)
print(results)
plot (results)


#model based clustering NbClust
res <- NbClust(testCluster, min.nc=2, max.nc=12, method="kmeans") # determining the number of clusters and proposes to user the best clustering scheme
res$All.index

barplot(table(res$Best.n[1,]), #shows the best number of clusters to use
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

table(res$Best.n[1,])


colsCluster<-colnames(dataL[,c(
  "PressPapersMajor",  
  "SVI_NEW"), with=F])

#experimenting with clustering
##clustering in one month - the results should not be different if scaling is done on the full dataset. 
##what if the scaling is done in one month only????
testC<-dataL[date==("2014-11-01"), .SD, .SDcols=colsCluster]
testC<-scale(testC) # SCALING

#stopped here!!!!!!!!!!!!!!!!!!!!!__________________________________
#needs to check this part!!!!!!!!!!!!!!

x0 <- dataL[,data.table(kmeans(cbind(.SD),centers=2)$centers), .SDcols=colsCluster,by=date] #clustering in each month
x <- dataL[(PressPapersMajor>0),data.table(kmeans(cbind(.SD),centers=2)$centers), .SDcols=colsCluster,by=date] #clustering in each month

#solution with all press/svi variables
test2Scale<-subset(dataR, select =c("wPressPapers", "wPressPapersMajor", "wPressWires", "wPressWiresMajor", "SVI_NEW", "SVI_AU", "SVI_ASX")) 
test2Scale<-data.table(scale(test2Scale))
dataR$VisibilityCluster4<-visibilityCluster$cluster
visibilityCluster$centers


#THIS CODE NEEDS TO BE CHECKED AND POSSIBLY DELETED - OLD CODE

library(ggplot2)
ggplot(testC, aes(PressPapersMajor, SVI_NEW))+geom_point()
##ggplot(testC, aes(AnnOwn, SVI, color=ASX100))+geom_point()

visibilityCluster<-kmeans(dataR[, c("zPressPapersCluster", "SVI_NEWCluster")], 3, nstart=20)
visibilityCluster<-kmeans(dataR[, c("lPressPapers", "lSVI_ASX")], 4, nstart=20)#best so far
visibilityCluster<-kmeans(dataR[, c("lPressWires", "lSVI_ASX")], 3, nstart=20)
visibilityCluster<-kmeans(dataR[, c("nPressPapers", "nSVI_ASX")], 3, nstart=20)
visibilityCluster<-kmeans(dataR[, c("lPressPapers", "lSVI_ASX", "lAnnOwn", "lEPS1NE")], 4, nstart=20)#best so far

visibilityCluster<-kmeans(dataR[, c("lPressPapers", "lSVI_NEW", "lEPS1NE")], 3, nstart=20)
visibilityCluster$centers
visibilityCluster$size
visibilityCluster$withinss
visibilityCluster$betweenss