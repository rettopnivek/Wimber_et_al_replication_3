#--------------------#
# Missing data       #
# Kevin Potter       #
# Updated 10/03/2017 #
#--------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if a pdf should be saved
savePlot = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Data extraction (Replication)
# Lookup - 03:  Data extraction (Original)
# Lookup - 04:  Analysis of missing data

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# For geting github packages
# install.packages( 'devtools' )
# library(devtools)

# Miscellanous functions for modeling/plotting
# install_github("rettopnivek/utilityf")
library(utilityf)

# Define some useful functions
source('F0_Useful_functions.R')

# Load in original data
load( 'Data/Original_all_data.RData' )
# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

# Create pdf or sample figure
if ( savePlot ) {
  setwd( 'Figures' )
  pdf( 'Missing_data.pdf' )
  setwd( orig_dir )
} else {
  x11()
}

# Specify layout of figure panels
lyt = cbind( 1 )
layout( lyt )

# Plotting characteristics
ptSz = 1.3
lnSz = 2
txtSz = 1.35

###
### Data extraction (Replication)
###
# Lookup - 02

# For easy manipulation
od = allData
colnames( od ) = c( 'S', 'Tr', 'Ph', 'IN', 'CN', 'Co', 
                    'Ch', 'Ac', 'RT', 'CR', 'IT', 'B', 
                    'Cat', 'Bl', 'ID' )

N = 24 # Number of subjects

# Define dummy-coded variable for selective-retrieval
od$SR = 1 - od$B

# Create index of missing responses
od$MR = 0; od$MR[ is.na( od$RT ) ] = 1

# Extract performance during initial training
pd = od[ od$Ph > 1 & od$Ph < 5, ]
# Define variable for repeat training
pd$Pract = 0; pd$Pract[ pd$Ph == 3 ] = 1

# Extract performance for selective-retrieval phase
sr = Extract_SR( allData[ od$Ph == 5, ] )

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

# Compute total amount of missing data
total_rep = sum( fr$MR == 1 )/nrow( fr )

###
### Data extraction (Original)
###
# Lookup - 03

# For easy manipulation
od = OriginalAllData
colnames( od ) = c( 'S', 'Tr', 'Ph', 'IN', 'Co', 
                    'Ch', 'RT', 'Ac', 'IT', 'B', 
                    'Bl', 'Cat', 'CR', 'fName' )

N = 24 # Number of subjects

# Define dummy-coded variable for selective-retrieval
od$SR = 1 - od$B

# Create index of missing responses
od$MR = 0; od$MR[ is.na( od$RT ) ] = 1

# Extract performance during initial training
pd = od[ od$Ph > 1 & od$Ph < 5, ]
# Define variable for repeat training
pd$Pract = 0; pd$Pract[ pd$Ph == 3 ] = 1

# Extract performance for selective-retrieval phase
sr = Extract_SR( OriginalAllData[ od$Ph == 5, ] )

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

# Compute total amount of missing data
total_orig = sum( fr$MR == 1 )/nrow( fr )


###
### Analysis of missing data
###
# Lookup - 03

# Determine proportion of missing data per subject
md = aggregate( fr$MR, list( fr$S ), mean )
colnames( md ) = c( 'S', 'P' )

# Extract response times for errors
RT_err = fr[ fr$Ac == 0, c('S','RT') ]

# Exclude missing data
RT_err = RT_err[ !is.na( RT_err$RT ), ]

# We'll exclude overly fast responses as well
RT_err = RT_err[ RT_err$RT > .3, ]

# Take log-transform of errors (to correct for skewed distribution)
RT_err$lRT = log( RT_err$RT )

# Compute standard deviation and average for each subject
check = aggregate( RT_err$lRT, list( RT_err$S ), 
                   function(x) c( mean(x), sd(x) ) )
colnames( check ) = c( 'S', 'TS' )
check$TS = as.data.frame( check$TS )
colnames( check$TS ) = c( 'Avg', 'SD' )

# Take log-transform of deadline
cutoff = log( 3.5 )

# Predictions based on CDF of normal distribution
pred = pnorm( cutoff, check$TS$Avg, check$TS$SD, lower.tail = FALSE )

# Track total number of missing responses (Original)
total_orig = round( 100 * sum( fr$MR )/nrow( fr ), 1 )

# Track total number of missing responses (Replication)
total_rep = round( 100 * sum( allData$Cond == 6 & allData$RT > 3.5 ) / 
                     sum( allData$Cond == 6 ), 1 )

# Plot observed against predicted
x = pred * 100
y = md$P * 100

# Plotting characteristics
txtSz = 1.5
lnSz = 2
ptSz = 2

par( mar = c( 4, 4, 1, .5 ) )
blankPlot( c(-1,20), c(-1,20) )
abline( h = -1, lwd = lnSz )
abline( v = -1, lwd = lnSz )

axis( 1, seq( 0, 20, 10 ), 
      paste( seq( 0, 20, 10 ), '%', sep = '' ), 
      tick = F, cex.axis = txtSz, line = -.5 )
axis( 2, seq( 0, 20, 10 ), 
      paste( seq( 0, 20, 10 ), '%', sep = '' ), 
      tick = F, cex.axis = txtSz, line = -.5 )

mtext( 'Predicted missing', side = 1, line = 2, cex = txtSz )
mtext( 'Observed missing', side = 2, line = 2, cex = txtSz )

segments( -1, -1, 30, 30, lty = 2, lwd = lnSz )
points( x, y, pch = 19, cex = ptSz )

legend( 10, 3,
        c( paste( 'Total (original): ', total_orig, '%', sep = '' ),
           paste( 'Total (replication): ', total_rep, '%', sep = '' )
        ),
        bty = 'n', cex = txtSz * .8 )

if ( savePlot ) dev.off()
setwd( orig_dir )