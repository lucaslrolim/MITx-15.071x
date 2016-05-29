airlines = read.csv("./data/AirlinesCluster.csv")
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
distance = dist(airlinesNorm,method="euclidian")
clusterAirL = hclust(distance,method="ward.D")
ALClusters = cutree(clusterAirL,k=5)
table(ALClusters)
avgValues <- function(nCluster){
  for(clusterNumber in 1:nCluster){
    print(paste("Cluster número:", clusterNumber))
    if(tapply(airlines$Balance, ALClusters, mean)[clusterNumber] == max(tapply(airlines$Balance, ALClusters, mean))){print("Balance")}
    if(tapply(airlines$QualMiles, ALClusters, mean)[clusterNumber] == max(tapply(airlines$QualMiles, ALClusters, mean))){print("QualMiles")}
    if(tapply(airlines$BonusMiles, ALClusters, mean)[clusterNumber] == max(tapply(airlines$BonusMiles, ALClusters, mean))){print("BonusMiles")}
    if(tapply(airlines$BonusTrans, ALClusters, mean)[clusterNumber] == max(tapply(airlines$BonusTrans, ALClusters, mean))){print("BonusTrans")}
    if(tapply(airlines$FlightMiles, ALClusters, mean)[clusterNumber] == max(tapply(airlines$FlightMiles, ALClusters, mean))){print("FlightMiles")}
    if(tapply(airlines$FlightTrans, ALClusters, mean)[clusterNumber] == max(tapply(airlines$FlightTrans, ALClusters, mean))){print("FlightTrans")}
    if(tapply(airlines$DaysSinceEnroll, ALClusters, mean)[clusterNumber] == max(tapply(airlines$DaysSinceEnroll, ALClusters, mean))){print("DaysSinceEnroll")}
  }
}
k = 5
set.seed(88)
KMC = kmeans(airlinesNorm,centers = k,iter.max = 1000)
table(KMC$cluster)
KMC$centers