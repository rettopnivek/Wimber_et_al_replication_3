#-------------------------#
# A priori power analysis #
# Kevin Potter            #
# Updated 10/11/2017      #
#-------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if a pdf should be saved
savePlot = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Create figure

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
load( 'Data/Power_1000_rep.RData' )

# Create pdf or sample figure
if ( savePlot ) {
  setwd( 'Figures' )
  pdf( 'Power_analysis.pdf' ) 
  setwd( orig_dir )
} else {
  x11()
}

###
### Create figure
###
# Lookup - 02

lnSz = 2
txtSz = 1.4
ptSz = 1.5

# Create blank plot
xl = c( .5, 5.5 ); yl = c( 0, 1 );
blankPlot( xl, yl )
segments( rep( xl[1], 4 ), c( .2, .4, .6, .8 ),
          rep( xl[2], 4 ), c( .2, .4, .6, .8 ),
          lwd = lnSz, col = 'grey80' )
segments( xl[1], .05, xl[2], .05, lwd = lnSz, lty = 2, col = 'grey80' )
customAxes( xl, yl )

# Add labels
axis( 2, seq(0,1,.2), tick = F, 
      cex.axis = txtSz, line = -1.2 )
mtext( 'Power', side = 2, line = 2, cex = txtSz )

axis( 1, 1:5, paste( 4:0, '%', sep = '' ), tick = F, 
      cex.axis = txtSz, line = -1.2 )
mtext( 'Magnitude of forgetting effect', side = 1, line = 2, cex = txtSz )

ltp = c(2,1)
for ( i in 1:2 ) {
  lines( 1:5, power[i,], lty = ltp[i], lwd = lnSz )
  points( 1:5, power[i,], pch = 19, cex = ptSz )
}

legend( 'topright', c( 'N = 24', 'N = 48' ),
        lty = 2:1, lwd = lnSz, cex = txtSz,
        bty = 'n' )


if ( savePlot ) dev.off()
setwd( orig_dir )
