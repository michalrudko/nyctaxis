library(dplyr)

# Read in a training data set
inp1<-read.table("train.csv",sep=",",header=T,stringsAsFactors = F)


# Read in enreached data set (by L. Ciszak)
inpProcessed<-read.table("train_processed.txt",sep="|",header=T,stringsAsFactors = F)