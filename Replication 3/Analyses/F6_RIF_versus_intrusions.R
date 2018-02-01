#------------------------------#
# Forgetting versus intrusions #
# Kevin Potter                 #
# Updated 10/11/2017           #
#------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if a pdf should be saved
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Data extraction (Original)
# Lookup - 03:  Create figure

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

# Create pdf or sample figure
if ( savePlot ) {
  setwd( 'Figures' )
  pdf( 'RIF_versus_intrusions.pdf' ) 
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
### Data extraction (Original)
###
# Lookup - 02

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
### Create figure
###
# Lookup - 03

# Determine proportion of missing data per subject
md = aggregate( fr$MR, list( fr$S ), mean )
colnames( md ) = c( 'S', 'P' )

# Proportion of intrusions
pInt = aggregate( sr$Choice == 2, list( sr$Subject ), mean )
colnames( pInt ) = c( 'S', 'P' )
# Convert to percentage
pInt$P = pInt$P * 100;

# Forgetting effect for competitors
fr$Ac[ fr$MR == 1 ] = 0 # Set missing responses equal to errors

tmp = aggregate( fr$Ac, list( fr$IT, fr$SR, fr$S ), mean )
colnames( tmp ) = c( 'IT', 'SR', 'S', 'P' )
pFC = data.frame( S = tmp$S[ tmp$IT == 2 & tmp$SR == 1 ],
                  P = tmp$P[ tmp$IT == 2 & tmp$SR == 1 ] - 
                    tmp$P[ tmp$IT == 2 & tmp$SR == 0 ] )
# Flip so that positive values denote more forgetting
pFC$P = -pFC$P
# Convert to percentage
pFC$P = pFC$P * 100

# Plot observed against predicted
x = scale( pInt$P )
y = scale( pFC$P )

# Plotting characteristics
txtSz = 1.5
lnSz = 2
ptSz = 2

par( mar = c( 4, 4, 1, .5 ) )
pl = c( -3.2, 3.2 )
blankPlot( pl, pl )
abline( h = pl[1], lwd = lnSz )
abline( v = pl[1], lwd = lnSz )

xl = range( x )
yl = range( y )

axis( 1, seq( xl[1], xl[2], length = 3 ),
      paste( round( seq( xl[1], xl[2], length = 3 ) * 
                      sd( pInt$P ) + mean( pInt$P ) ),
             '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.5 )
axis( 2, seq( yl[1], yl[2], length = 3 ),
      paste( round( seq( yl[1], yl[2], length = 3 ) * 
                      sd( pFC$P ) + mean( pFC$P ) ),
             '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.5 )

mtext( 'Intrusions', side = 1, line = 2, cex = txtSz )
mtext( 'Estimated RIF', 
       side = 2, line = 2, cex = txtSz )

# Plot points
points( x, y, pch = 19, cex = ptSz )

# Add lines for regression
R = numeric( 2 )
R[1] = cor( x, y, method = 'spearman' )

segments( pl[1], pl[1] * R[1],
          pl[2], pl[2] * R[1],
          lwd = lnSz, col = 'red', lty = 2 )

# Exclude subject with most missing data
exclude = md$S[ which( md$P == max( md$P ) ) ]
points( x[ exclude ], y[ exclude ],
        pch = 21, col = 'red', bg = NA, cex = ptSz * 1.5 )

R[2] = cor( x[-exclude], y[-exclude], method = 'spearman' )

segments( pl[1], pl[1] * R[2],
          pl[2], pl[2] * R[2],
          lwd = lnSz )

legend( 'topright',
        c( paste( 'R = ', round( R[1], 2 ), sep = '' ),
           paste( 'R = ', round( R[2], 2 ), '*', sep = '' ) ),
        bty = 'n', text.col = c( 'red', 'black' ),
        cex = txtSz * .8 )

legend( pl[1], pl[1] + .15*(pl[2] - pl[1]),
        '17% missing', fill = 'red', bty = 'n',
        cex = txtSz * .8 )

if ( savePlot ) dev.off()