setwd("c:/R/DBDA2Eprograms/DBDA2Eprograms")
getwd()

graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

install.packages("rjags")
install.packages("runjags")
library(rjags)
library(runjags)
source("DBDA2E-utilities.R")

myData = read.csv("z15N50.csv") # Read data file; must be in curr. work. dir.
y = myData$y        # The y values are in the column named y.
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  y = y ,
  Ntotal = Ntotal 
)

# Define the model:
modelString = "
model {
for ( i in 1:Ntotal ) {
y[i] ~ dbern( theta )
}
theta ~ dbeta( 1 , 1 )
}
" # close quote for modelString
writeLines( modelString , con="TEMPmodel.txt" )

# Initialize the chains based on MLE of data.
# Option: Use single initial value for all chains:
#  thetaInit = sum(y)/length(y)
#  initsList = list( theta=thetaInit )
# Option: Use function that generates random values for each chain:
initsList = function() {
  resampledY = sample( y , replace=TRUE )
  thetaInit = sum(resampledY)/length(resampledY)
  thetaInit = 0.001+0.998*thetaInit # keep away from 0,1
  return( list( theta=thetaInit ) )
}

# Run the chains:
jagsModel = jags.model( file="TEMPmodel.txt" , data=dataList , inits=initsList , 
                        n.chains=3 , n.adapt=500 )
update( jagsModel , n.iter=500 )
codaSamples = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )
diagMCMC( codaObject=codaSamples , parName="theta" )
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) )
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) ,
          cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90)
plotPost( codaSamples[,"theta"] , main="theta" , xlab=bquote(theta) ,
          cenTend="median" , compVal=0.5 , ROPE=c(0.45,0.55) , credMass=0.90,
          showCurve = TRUE)
