# Read metadata from biomart export; includes coordinates (starting-ending b.p.), chromosome name, gene-name (symbolic), ENSG id
normalGeneInfo <- read.csv("/labs/mpsnyder/neekonsu/ensembl/normal.txt", header=TRUE, sep='\t')
abnormalGeneInfo <- read.csv("/labs/mpsnyder/neekonsu/ensembl/abnormal.txt", header=TRUE, sep='\t')

# counts data stored in data.frame

# function for computing average metric to be placed in .bed file
# the function isolates the counts of relevant ENSGs (column values), 
# computes mean for each ENSG, and replaces each column with its single mean value
computeAverage <- function (ID_ARRAY) {
  #remove version tags from colnames of count data
  unVersionedENSG <- gsub("[.].*$", "", colnames(data.frame))
  colnames(data.frame) <- unVersionedENSG
  #isolate relevant columns
  countsIsolate <- data.frame[,colnames(data.frame)%in%ID_ARRAY]
  #replace columns with mean values
  for(i in 1:ncol(countsIsolate)){
    countsIsolate[,i] <- mean(countsIsolate[,i], na.rm = TRUE)
  }
  return(countsIsolate[1,])
}

# define array of ENSGs to pass into computeAverage() by reading normal/abnormalGeneInfo
normalENSG <- as.vector(normalGeneInfo$Gene.stable.ID)
abnormalENSG <- as.vector(abnormalGeneInfo$Gene.stable.ID)

# compute averag values of each gene and replace raw counts with single average-metrics
avgNorm <- computeAverage(normalENSG)
avgAbnorm <- computeAverage(abnormalENSG)

# add average metric for all genes that mutually exist in both sets (averages and ENSEMBL export)

# function spliceAverages populates "Average_Counts" columns of raw ENSEMBL export with avgNorm/Abnorm
spliceAverages <- function (ID_AVG_COUNTS_DF, ENSEMBL_DF) {
    # handle bad typing
    if (!is.data.frame(ID_AVG_COUNTS_DF)) {
        ID_AVG_COUNTS_DF <- as.data.frame(ID_AVG_COUNTS_DF)
    }
    if (!is.data.frame(ENSEMBL_DF)) {
        ENSEMBL_DF <- as.data.frame(ENSEMBL_DF)
    }
    for (i in range(1:nrow(ENSEMBL_DF))) {
        ENSEMBL_DF[i,"Average_Counts"] <- ID_AVG_COUNTS_DF[1,ENSEMBL_DF[i,"Gene.stable.ID"]]
    }
    return(ENSEMBL_DF)
}

# write dataframes that contain all requisite info to completeMetadata(Norm/Abnorm)
# TODO: format below variables to .bed conventions and send to Akshay (3/10/19)
completeMetadataNorm <- spliceAverages(avgNorm, normalGeneInfo)
completeMetadataAbnorm <- spliceAverages(avgAbnorm, abnormalGeneInfo)