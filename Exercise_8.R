setwd("c:/R/DBDA2Eprograms/DBDA2Eprograms")
getwd()

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

install.packages("rjags")
install.packages("runjags")
library(rjags)
library(runjags)

##Exercise 8.1

y = c( rep(1,9),rep(0,3) , rep(1,45),rep(0,15) , rep(1,3),rep(0,9) )
s = c( rep("A",12) , rep("B",60) , rep("C",12) )
write.csv( data.frame(y=y,s=s) , file="Exercise.08.1.csv" , row.names=FALSE )

# Below is just the essential lines of Jags-Ydich-XnomSsubj-MbernBeta-Example.R
# with the data file changed:
graphics.off()
rm(list=ls(all=TRUE))
fileNameRoot="Exercise.08.1" # for output filenames
source("DBDA2E-utilities.R")
# Load The data from the file: myData = read.csv("Exercise.08.1.csv")
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbernBeta.R")
# Generate the MCMC chain:
myData <- read.csv("Exercise.08.1.csv")
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName )
}
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL , #rope=c(0.45,0.55) ,
                        compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , #rope=c(0.45,0.55) ,
          compValDiff=0.0 , #ropeDiff = c(-0.05,0.05)
)


##Exercise 8.2

summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) )


##Exercise 8.3

fileNameRoot = "Jags-Ydich-XnomSsubj-MbernBeta-" graphFileType = "eps"
The first line above specifies the beginning of the filenames for saved information, and the second line above specifies the graphics format for saved graphs.
# Generate the MCMC chain: mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )
The MCMC chain is saved in a file named
Jags-Ydich-XnomSsubj-MbernBeta-Mcmc.Rdata
Notice the name is the fileNameRoot with Mcmc appended.
It is in compressed Rdata format.
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , saveName=fileNameRoot , saveType=graphFileType )
}
The diagnostic graphs are saved in files named
Jags-Ydich-XnomSsubj-MbernBeta-Diagtheta[1].eps and
Jags-Ydich-XnomSsubj-MbernBeta-Diagtheta[2].eps
Notice the names are the fileNameRoot with Diag<parameter> appended.
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=0.5 , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) , saveName=fileNameRoot )
The summary information is saved in a file named
Jags-Ydich-XnomSsubj-MbernBeta-SummaryInfo.csv
which is in comma-separated-value format.
Notice the name is the fileNameRoot with SummaryInfo appended.
# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , #rope=c(0.45,0.55) ,
          compValDiff=0.0 , #ropeDiff = c(-0.05,0.05) , saveName=fileNameRoot , saveType=graphFileType )
          The graph of the posterior distribution is saved in a file named
          Jags-Ydich-XnomSsubj-MbernBeta-Post.eps
          Notice the name is the fileNameRoot with Post appended

          
##Exercise 8.4
          