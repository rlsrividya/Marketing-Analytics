library("bayesm")
data_load <- read.csv(file = '/Users/amritangshumukherjee/Downloads/Marketing Analytics/Group Project/clean_df.csv')
data_load_temp <- data_load
data_load_temp$X <- NULL
X=data.matrix(data_load_temp[1:2])

#data_load_temp
#rhierMnlRwMixture
#rhierMnlRwMixture(Data, Prior, Mcmc)
p= 4 # num of choice alterns
R=20000
nlgt = 385  #number of observations

#Z = matrix(runif(nz*nlgt),ncol=nz)
#Z = t(t(Z) - apply(Z,2,mean))        # demean Z
ncomp = 1 # num of mixture components

myList <- list()
dataset_new = NULL
k=0
j=0
m=0
n=0

process=data_load_temp[c(2,3,4,5)]
process$c <- process$Alternatives * process$Choice
outpp=unique(process[c(1,2,5)])
outpp2=outpp[c(1,3)]
outpp3=subset(outpp2, c != 0)


for(i in 1:385) 
{ # for-loop over rows
  temp<-subset(data_load_temp, Respondent == i)
  X=as.matrix(temp[6:31])
  
  temp2<-subset(outpp3, Respondent == i)
  Y=as.matrix(temp2[2])
  
  dataset_new[[i]]=list(y=Y, X=X)
  
}


## set parms for priors and Z
Prior1 = list(ncomp=ncomp)
keep = 5000
Mcmc1 = list(R=R, keep=keep)
Data1 = list(p=p, lgtdata=dataset_new)

## fit model without sign constraints
out1 = rhierMnlRwMixture(Data=Data1, Prior=Prior1, Mcmc=Mcmc1)

plot(out1$betadraw)
plot(out1$nmix)
out1$Deltadraw
out1$nmix
summary(out1$betadraw)

if(0) {
  plot(out1$betadraw)
  plot(out1$nmix)
}

temps<-out1$betadraw
#library(reshape2)
#new<-melt(temps)

data <-out1$betadraw
df<- data.frame(out1$betadraw)

df<-data.frame(matrix(out1$betadraw, nrow=dim(out1$betadraw)[2],ncol = dim(out1$betadraw)[1] , byrow=TRUE))
#df <- data.frame(matrix(unlist(out1$betadraw), nrow=length(out1$betadraw), byrow=TRUE))
write.csv(df,"/Users/amritangshumukherjee/Downloads/Marketing Analytics/Group Project/File Name.csv", row.names = FALSE)

