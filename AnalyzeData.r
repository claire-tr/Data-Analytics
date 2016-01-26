install.packages("taRifx")
install.packages("network")
install.packages("fpc")
install.packages("arules")
install.packages("psych")
install.packages("survival")
install.packages("doBy")
install.packages("ggplot2")
install.packages("cluster") 
install.packages("igraph")
install.packages("caret")
install.packages("pROC")
install.packages("rpart")
install.packages("klaR")
library(taRifx)
library(network)
library(fpc)
library(arules)
library(psych)
library("survival")
library("doBy")
library("ggplot2")
library("cluster") 
library(igraph)
library(caret)
library(pROC)
library("rpart")
library(klaR)
########################################################################################################################
# MovingData Clean
########################################################################################################################
MBS <- read.csv(file.choose(),stringsAsFactors = F,header = T)
# destring all of cols 
for (i in 1:ncol(MBS)){
  MBS[,i] <- destring(MBS[,i])}
# remvoe na in population and percapita income
MBS <- MBS[!is.na(MBS$Population|MBS$Per.capita.income),]
#  replace na with sampling
for (i in 1:ncol(MBS)){
  na <- is.na(MBS[,i])
  MBS[na,i] <- sample(MBS[!na,i],length(na[na==T]), replace = T)}

MDC <- read.csv(file.choose(),stringsAsFactors = F,header = T)
# destring all of cols 
for (i in 1:ncol(MDC)){
  MDC[,i] <- destring(MDC[,i])}
# remvoe na in population and percapita income
MDC <- MDC[!is.na(MDC$Population|MDC$Per.capita.income),]
#  replace na with sampling
for (i in 1:ncol(MDC)){
  na <- is.na(MDC[,i])
  MDC[na,i] <- sample(MDC[!na,i],length(na[na==T]), replace = T)}

MNYC <- read.csv(file.choose(),stringsAsFactors = F,header = T)
# destring all of cols 
for (i in 1:ncol(MNYC)){
  MNYC[,i] <- destring(MNYC[,i])}
# remvoe na in population and percapita income
MNYC <- MNYC[!is.na(MNYC$Population|MNYC$Per.capita.income),]
# replace na with sampling
for (i in 1:ncol(MNYC)){
  na <- is.na(MNYC[,i])
  if (length(na[na==T])==0){break}
  else{
    MNYC[na,i] <- sample(MNYC[!na,i],length(na[na==T]), replace = T)}}
# delete some cols
MNYC <- MNYC[,-49:-100]
MNYC <- MNYC[,-49:-52]

MSF <- read.csv(file.choose(),stringsAsFactors = F,header = T)
# destring all of cols 
for (i in 1:ncol(MSF)){
  MSF[,i] <- destring(MSF[,i])}
# remvoe na in population and percapita income
MSF <- MSF[!is.na(MSF$Population|MSF$Per.capita.income),]
#  replace na with sampling
for (i in 1:ncol(MSF)){
  na <- is.na(MSF[,i])
  MSF[na,i] <- sample(MSF[!na,i],length(na[na==T]), replace = T)}

MDA <- read.csv(file.choose(),stringsAsFactors = F,header = T)
# destring all of cols 
for (i in 1:ncol(MDA)){
  MDA[,i] <- destring(MDA[,i])}
# remvoe na in population and percapita income
MDA <- MDA[!is.na(MDA$Population|MDA$Per.capita.income),]
#  replace na with sampling
for (i in 1:ncol(MDA)){
  na <- is.na(MDA[,i])
  MDA[na,i] <- sample(MDA[!na,i],length(na[na==T]), replace = T)}

# combine 5 cities into one dataset
TotalMoving <- rbind(MBS,MDA,MDC,MNYC,MSF)
# select useful attributes
TotalMoving <- TotalMoving[,c(-5:-11,-20:-26,-34:-36,-41:-47)]


###############################################################################
##yelp cleaning
###########################################################################################

##########################
# moving new featrues clean
###########################
new_features_test <- read.csv(file.choose())
for(i in 1:4){
  new_features_test=rbind(new_features_test,read.csv(file.choose()))}
# destring all of cols 
new_features_test[,4] <- destring(new_features_test[,4])
# remvoe na in population and percapita income
new_features_test <- new_features_test[!is.na(new_features_test$Higher.Degree.percentage),]
#  replace na with samplings
for (i in 1:nrow(new_features_test)){
  if(new_features_test$Higher.Degree.percentage[i]<0 | new_features_test$Higher.Degree.percentage[i]>1){
    new_features_test<-new_features_test[-i,]}}

###################### 
#  yelp file clear
######################
yelp_test <- read.csv(file.choose())
for(i in 1:4){
  yelp_test=rbind(yelp_test,read.csv(file.choose()))
}
# destring all of cols 
for (i in 6:7){
  yelp_test[,i] <- destring(yelp_test[,i])
}
# remvoe na in population and percapita income
yelp_test <- yelp_test[!is.na(yelp_test$Zip.Code),]

###########################
# yelp price clear
############################
yelp_price_test <- read.csv(file.choose())
for(i in 1:4){
  yelp_price_test=rbind(yelp_price_test,read.csv(file.choose()))
}
# destring all of cols 

for (i in 7:8){
  yelp_price_test[,i] <- destring(yelp_price_test[,i])
}
# remvoe na in population and percapita income
yelp_price_test <- yelp_price_test[!is.na(yelp_price_test$average_price_in_._sign),]
yelp_price_test <- yelp_price_test[!is.na(yelp_price_test$average_rating),]
write.csv(new_features_test,file="new_features_test.csv")
write.csv(yelp_price_test,file="yelp_price_test.csv")
write.csv(yelp_test,file="yelp_test.csv")

# MERGE
names(new_features_test)[2]="Zip"
names(yelp_price_test)[1] <- "Zip"
names(yelp_price_test)[7]="AverPrice"
names(yelp_price_test)[8]="AverRate"
Yelp=yelp_price_test[,c(1,6,7,8)]
NewFeature=new_features_test[,2:4]
DataMerge1 <- merge(TotalMoving,Yelp,by = c("Zip")) 
DataMerge1 <- merge(DataMerge1,NewFeature,by = c("Zip"))
write.csv(DataMerge,file="DataMerge.csv")
DataMerge<-read.csv(file.choose(),header=T)
DataMerge <- DataMerge[,-1]

###############################################################################
# Exploratory Analysis
###########################################################################################
# Basic Statistical Analysis
###########################################################################################
#  the min and max values for the average prices
min(yelp_price_test$AverPrice)
yelp_price_test[which.min(yelp_price_test$AverPrice),1]
max(yelp_price_test$AverPrice)
yelp_price_test[which.max(yelp_price_test$AverPrice),1]
median(yelp_price_test$AverPrice)
hist(yelp_price_test$AverPrice)

# the min and max values for the average rating
min(yelp_price_test$AverRate)
yelp_price_test[which.min(yelp_price_test$AverRate),1]
max(yelp_price_test$AverRate)
yelp_price_test[which.max(yelp_price_test$AverRate),1]
hist(yelp_price_test$AverRate)

# the min and max values for degree percentage

min(new_features_test$Higher.Degree.percentage)
new_features_test[which.min(new_features_test$Higher.Degree.percentage),2]
max(new_features_test$Higher.Degree.percentage)
new_features_test[which.max(new_features_test$Higher.Degree.percentage),2]
mean(new_features_test$Higher.Degree.percentage)
sd(new_features_test$Higher.Degree.percentage)
median(new_features_test$Higher.Degree.percentage)

# the mean and sd for  review count
# the count of yelp price and rating
table(yelp_test$Price)
table(yelp_test$Rating)
mean(yelp_test$Review.Count)
sd(yelp_test$Review.Count)
min(yelp_test$Review.Count)
max(yelp_test$Review.Count)
median(yelp_test$Review.Count)
hist(yelp_test$Rating)

################################
#moving data statistica analysis
describe(TotalMoving)

# binning equal depth
ED <- cut(DataMerge$Population,breaks = quantile(DataMerge$Population,prob = seq(0,1,1/4)))

##########################################################################################
# Histograms and Correlations
#
##########################################################################################

###############################
### HISTGRAPH
TotalMoving <- read.csv(file.choose(),header = T)
attach(TotalMoving)
hist(Per.capita.income)
hist(Average.household.Income)
qqnorm(Per.capita.income)
hist(Median.Disposable.Income)
hist(Population.Desity)
hist(Total.Crime.Risk)
hist(Average.Home.Sale.Price)
####correlation
corr <-cor(DataMerge[,c(2:23,25,26,28)])
zdf <- as.data.frame(as.table(corr))
zdf
# get all the correlations which is larger than .9
sub <- subset(zdf, abs(Freq) > 0.9)
sub

##############################################################################################################
# Cluster
##############################################################################################################
clusterAttr1 <- c(20,2,9,10,11,25,28,6)
clusterData <- DataMerge[,clusterAttr1]
myCleanData <- scale(clusterData)
fitKmean <- kmeans(myCleanData, 5) 
clusplot(myCleanData, fitKmean$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
fitKmean$tot.withinss

# calculate the SSE GAP
CG = clusGap(myCleanData,kmeans, 10)
plot(CG$Tab[,2], type="l",ylim=c(4.5,6.0))
lines(CG$Tab[,1], type="l", col="red")
plot(CG$Tab[,3], type="l")
        
##  hierarchical #############
data.dist <- dist(myCleanData)
data.hclust <- hclust(data.dist)
plot(data.hclust)
plot(data.hclust, labels = data.hclust$class)
groups <- cutree(data.hclust,3)
groups
cbind(data.hclust$class,groups)
table(groups)
# create function to plot different groups to see which is better
clus.size <- function (ncl){
table(cutree(data.hclust,ncl))}
counts <- sapply(2:6,clus.size)
names(counts) <- 2:6
counts

#  dbscan??????????????????????????
ds <- dbscan(myCleanData,1,MinPts = 5,scale = F,)
plot(myCleanData,col=ds$cluster)

##############################################################################################################
# association rule
##############################################################################################################

# assocation rule

#choose the subset
AR_index=c(5,11,12,26,29)-1
subset_AR=DataMerge1[,AR_index]

#bining yelp the attributes 
subset_AR$Population.Desity = cut(subset_AR$Population.Desity,breaks=quantile(subset_AR$Population.Desity, probs=seq(0,1,1/4)))
subset_AR$Per.capita.income = cut(subset_AR$Per.capita.income,breaks=quantile(subset_AR$Per.capita.income, probs=seq(0,1,1/4)))
subset_AR$Median.Disposable.Income = cut(subset_AR$Median.Disposable.Income,breaks=quantile(subset_AR$Median.Disposable.Income, probs=seq(0,1,1/4)))
subset_AR$AverPrice = cut(subset_AR$AverPrice,breaks=quantile(subset_AR$AverPrice, probs=seq(0,1,1/2)))
subset_AR$Higher.Degree.percentage = cut(subset_AR$Higher.Degree.percentage,breaks=quantile(subset_AR$Higher.Degree.percentage, probs=seq(0,1,1/4)))

# try three different support level to make the assocation rule
rules_0.2 <- apriori(subset_AR,parameter = list(minlen=2, supp=0.2))
inspect(rules_0.2)
rules_0.15 <- apriori(subset_AR,parameter = list(minlen=2, supp=0.15))
inspect(rules_0.15)
rules_0.1 <- apriori(subset_AR,parameter = list(minlen=2, supp=0.1))
inspect(rules_0.1)

##############################################################################################################
# net work
##############################################################################################################

#select the subset
network_index=c(3:24,26,27,29)-1
nt_data=DataMerge1[,network_index]
num_col=ncol(nt_data)
network_data=c()

# get the nodes edges
for(i in 1:num_col){
  for(j in 1:num_col){
    if(i==j | abs(cor(nt_data[,i],nt_data[,j]))<0.5){
      edge=0
    }
    else{
      edge=1
    }
    network_data=c(network_data,edge)
  }
}
edge_data=matrix(network_data,num_col,num_col)

# get the netwok
g <- graph.adjacency(edge_data, weighted=T,mode = "undirected")
V(g)$label=names(nt_data)
V(g)$degree=degree(g)
plot.igraph(g)

# caluate the values

#degree
Degree <- degree(g, mode = "out")
Degree
#betweenness
Betweenness <- betweenness(g, directed = FALSE)
Betweenness
# clustering coefficient
Clustering.Coefficient <- transitivity(g, type="weighted")
Clustering.Coefficient

# get the average of degree, betweenness, Clustering coefficient
mean_degree=mean(Degree)
mean_degree
mean_Betweenness=mean(Betweenness)
mean_Betweenness
mean_clustering_coefficient=mean(Clustering.Coefficient,na.rm=T)
mean_clustering_coefficient

#degree distribution
hist(Degree)

#Density
graph.density(g)
#Components
summary(components(g))
# largest k-core
max(graph.coreness(g))
#diameter
diameter(g)

#caluate Edge-Betweenness and Modularity
eb_cluster=edge.betweenness.community(g)
eb_cluster

Mod_cluster<-fastgreedy.community(g,merges=TRUE, modularity=TRUE)
Mod_cluster
Mod_cluster$modularity


#plot cluster by edge-betweeness and modularity
par(mfrow=c(1,2))
V(g)$color <- eb_cluster$membership+1 
g <- set.graph.attribute(g,"layout",layout.kamada.kawai(g))
plot(g,vertex.label=NA,main="Cluster by Edge-Betweeness") 

V(g)$color <- Mod_cluster$membership+1 
g <- set.graph.attribute(g,"layout",layout.kamada.kawai(g))
plot(g,vertex.label=NA,main="Cluster by Modularity")
#######################################################


############################################################################################################### 
# ANOVA
##############################################################################################################
# get data ready for ANOVA test
YBS <- read.csv(file.choose(),header = T)
YDA<- read.csv(file.choose(),header = T)
YDC<- read.csv(file.choose(),header = T)
YNYC<- read.csv(file.choose(),header = T)
YSF<- read.csv(file.choose(),header = T)
names(YBS)[7] <- "AverPrice"
names(YDA)[7] <- "AverPrice"
names(YDC)[7] <- "AverPrice"
names(YNYC)[7] <- "AverPrice"
names(YSF)[7] <- "AverPrice"
YBS$City <- "BS" 
YDA$City <- "DA"
YDC$City <- "DC"
YNYC$City <- "NYC"
YSF$City<- "SF"

TotalYPrice <- rbind(YBS,YDA,YDC,YNYC,YSF)
TotalYPrice <- TotalYPrice[,c(1,7,8,9)]
boxplot(TotalYPrice$AverPrice~TotalYPrice$City)
# H0: average yelp price for all city is the same
ANOVA <- aov(TotalYPrice$AverPrice~TotalYPrice$City)
summary(ANOVA)
attributes(ANOVA)
TukeyHSD(ANOVA)
plot(TukeyHSD(ANOVA))

# linear regression
Model1 <- lm(DataMerge$AverPrice~DataMerge$Population+DataMerge$Average.household.Income+DataMerge$AverRate+DataMerge$Per.capita.income)
summary(Model1)
cor(DataMerge$AverPrice,DataMerge$AverRate,method="pearson")
confint(Model1,conf.level = .95)
par(mfrow=c(2,2))
plot(Model1)# diagnosis plot to test assumptions 

################################# ##################################################################  
#Predictive Analysis 
##################################################################### ################################# 
# data for Predictive Analysis 
DataMerge <- read.csv(file.choose(),header =T)
names(DataMerge)
decisionTreeAttr <- c(20,2,9,10,11,25,28,6)
treeData <- DataMerge[,decisionTreeAttr]
High <- ifelse(treeData$AverPrice>1.5,"YES","NO")
treeData <- data.frame(treeData,High)
treeData <- treeData[,-6]
################################# 
#DECISION TREE
####################################
fit <-rpart(High~.,data = treeData,method = "class",control = rpart.control(xval = 10))
names(fit)
plot(fit)
text(fit)
fit2 <- predict(fit, newdata = treeData,type = "class")
confusionMatrix(fit2,treeData$High)
nbr <- as.numeric(fit2)
roc <- roc(treeData$High, nbr,plot=TRUE, add=TRUE)
plot(roc)

########################################################
# Naive Bayes
###########################################################
trainControl <- trainControl(method = "cv",number =10)
fitNB <- train(treeData$High~.,method = "nb",data  =treeData,trControl= trainControl)
fitNBpre <-predict(fitNB,newdata = treeData)
confusionMatrix(fitNBpre,treeData$High)
NBNbr <- as.numeric(fitNBpre)
rocNB <- roc(treeData$High, NBNbr,plot=TRUE, add=TRUE)
plot(rocNB)
################################################################################
# Lazy Learner
####################################################################################################
fitKnn <- train(treeData$High~.,method = "knn",data  =treeData,trControl= trainControl)
fitKnnpre <-predict(fitKnn,newdata = treeData)
confusionMatrix(fitKnnpre,treeData$High)
knnNbr <- as.numeric(fitKnnpre)
rocKnn <- roc(treeData$High, knnNbr,plot=TRUE, add=TRUE)
plot(rocKnn)
