###############################################################
install.packages("AlgDesign")
library(AlgDesign)

set.seed(123)
levels.design= c(2,2,3,4); #set the number of levels for each variable
f.design <- gen.factorial(levels.design,factors="all"); #construct full orthogonal design
fract.design <- optFederov(frml=~X1+X2+X3+X4,data=f.design,nTrials=12,approximate=FALSE); #select the best design given main effects are all that is to be estimated

###############################################################
##Regression Analysis for getting part-worth estimates
setwd("~/Google Drive/UofR/Winter Quarter/Marketing Research/Week 8")
filenm = "Session 06 ClassExampleConjointData"
load(paste(filenm,".Rdata",sep=""))

##list the objects and make sense of them
ls()

##regressions
##aggregate
summary(lm(ratings~desmat))

##by apriori segment age
summary(lm(ratings~desmat*ageD))
summary(lm(ratings~desmat*genderD)); ##run the regression with interactions for segment dummies
summary(lm(ratings~desmat,subset=ageD==1))
summary(lm(ratings~desmat,subset=ageD==0))

##by individual
desmatf = cbind(rep(1,nrow(desmat)),desmat); ##add column for constant
partworths = matrix(nrow=sampsize,ncol=ncol(desmatf))
for(i in 1:sampsize){ #for each individual run the regression
  partworths[i,]=lm(ratings~desmat,subset=ID==i)$coef
}
colnames(partworths) = c("Intercept","Price","Size","Motion","Style")

##segmenting individuals - see code from last week for details
library(cluster)
library(fpc)
set.seed(123456)   # set random number seed before doing cluster analysis


toclust = partworths;
pm1 = pamk(toclust,scaling=TRUE)

wss <- (nrow(toclust)-1)*sum(apply(toclust,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(toclust,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

km1 = kmeans(toclust,3,iter.max = 20, nstart=2)
km2 = kmeans(toclust,2,iter.max = 20, nstart=2)
percsize = paste(1:2," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)

clusplot(toclust, km2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0); #plot clusters against principal components

plotcluster(toclust, km2$cluster); #plot against discriminant functions ()

plotClust = function(km,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toclust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toclust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}



##predicting missing cells (preparing for market simulation)
##repeat individual level partworths for multiplication
partworths.full = matrix(rep(partworths,each=16),ncol=5)
pratings = rowSums(desmatf*partworths.full)
finalratings = ifelse(is.na(ratings),pratings,ratings); #combining actual when available and predicted ratings

##market simulation
##a scenario is a set of products, each with a set of levels.
## create a vector with the indexes for the product profiles from the
##status quo
#Our competitor offers 1 ($139, 18", Bouncing, Race Horse)
#We offer 9, ($139, 18", Bouncing, Glamour Horse)
scen0 = c(1,9)
##Adding with product 3, a $139, 26", Bouncing, Glamour Horse
scen1 = c(1,9,3)
##What if our competitor responds by lowering price?
scen2 = c(2,9,3)

##market simulations
#tranform final ratings into matrix
simDecInput = matrix(finalratings,nrow=nprofiles); ##this has 16 rows for profiles and sampsize columns

##inputmat is the ratings matrix with rows as profiles and cols as ratings
##secn is the list of products in the market for the scenario (these are rows in inputmat)
simDec = function(inputmat,scen){
  inmkt = inputmat[scen,]
  max = apply(inmkt,2,max)
  firstChoices = (inmkt>rep(max,each=length(scen))-.000000000001)
  shares = firstChoices/rep(colSums(firstChoices),each=length(scen))
  rowMeans(shares)
}
simDec0 = simDec(simDecInput,scen0)
simDec1 = simDec(simDecInput,scen1)
simDec2 = simDec(simDecInput,scen2)

##inputmat and scen is as above. myprods are indicators of which prods are the firms,
## prices are the prices for all products, vcosts are the variable costs
## fcosts are the fixed costs for the firm (need to calculate in already the number of products)
simProfit = function(inputmat,scen, myProds, prices, vcosts,fcosts,mktsize=1){
  mktshr = simDec(inputmat,scen);
  vprofit = mktshr * (prices-vcosts)*mktsize;
  sum(vprofit[myProds])-fcosts
}
simProf0 = simProfit(simDecInput,scen0,c(2),c(139,139),c(99,99),20000,2000)
simProf1 = simProfit(simDecInput,scen1,c(2,3),c(139,139,139),c(89,89,99),40000,2000)
simProf2 = simProfit(simDecInput,scen2,c(2,3),c(119,139,139),c(89,89,99),40000,2000)

