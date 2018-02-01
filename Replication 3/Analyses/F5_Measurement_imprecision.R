#-------------------------#
# Measurement imprecision #
# Kevin Potter            #
# Updated 10/11/2017      #
#-------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if a pdf should be saved
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Data extraction (Replication)
# Lookup - 03:  Panel A
# Lookup - 04:  Data extraction (Original)
# Lookup - 05:  Panel B

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

# Load in packages for mixed effects modeling (MLE)
# install.packages( 'lme4' )
library( lme4 )

# Define some useful functions
source('F0_Useful_functions.R')

# Load in original data
load( 'Data/Original_all_data.RData' )
# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

# Define additional functions
ds_perf = function( dat, sel, IV ) {
  # Purpose:
  # Computes difference scores between conditions for 
  # performance/accuracy data (Ac).
  # Arguments:
  # dat - A data frame with a vector of accuracy values (Ac) 
  #       and a set of condition indices
  # sel - A matrix with 2 rows:
  #       Row 1 = A vector of the levels of the variables used 
  #               to select the first group of scores
  #       Row 1 = A vector of the levels of the variables used 
  #               to select the second group of scores
  # IV  - A character vector, giving the names of the variables to 
  #       aggregate performance over
  # Returns:
  # A vector of difference scores.
  
  cvrt = c()
  for ( i in 1:length(IV) ) {
    cvrt = c( cvrt, list( dat[,IV[i]] ) )
  }
  names( cvrt ) = IV
  
  perf = aggregate( dat$Ac, cvrt, mean )
  colnames( perf ) = c( IV, 'P' )
  
  tmp = matrix( NA, nrow( perf ), ncol( sel ) )
  for ( i in 1:ncol(sel)) {
    tmp[,i] = as.numeric( perf[,IV[i]] == sel[1,i] )
  }
  sel1 = rowSums( tmp ) == ncol( sel )
  
  for ( i in 1:ncol(sel)) {
    tmp[,i] = as.numeric( perf[,IV[i]] == sel[2,i] )
  }
  sel2 = rowSums( tmp ) == ncol( sel )
  
  out = perf$P[ sel1 ] - perf$P[ sel2 ]
  
  return( out )
}

sim_diff_scores = function( V, N = c( 18, 54 ), nRep = 50000) {
  # Purpose:
  # A function to generate a Monte Carlo sample of difference 
  # scores computed by simulating a multinomial process model.
  # The model assumes that subjects correctly recall an item 
  # with probability V, otherwise they will correctly guess the 
  # item 50% of the time with probability (1-V). Difference 
  # scores are then computed between proportion correct for 
  # the baseline and selective-retrieval conditions, which only 
  # differ in the number of trials.
  # Arguments:
  # V    - The probability of recalling the correct image
  # N    - The number of trials for the baseline and selective-retrieval
  #        conditions
  # nRep - The number of Monte Carlo samples to generate
  # Returns:
  # A vector of difference scores.
  
  # Baseline
  recall = rbinom( nRep, N[1], V )
  guess = rbinom( nRep, N[1] - recall, .5 )
  observed = recall + guess
  prop_correct_B = observed/N[1]
  
  # Selective-retrieval
  recall = rbinom( nRep, N[2], V )
  guess = rbinom( nRep, N[2] - recall, .5 )
  observed = recall + guess
  prop_correct_SR = observed/N[2]
  
  # Estimate of forgetting effect
  diff_score = prop_correct_B - prop_correct_SR
  
  return( diff_score )  
}

est_ui = function(v, alpha) {
  # Purpose:
  # Given a value for P(Recall), simulates difference scores 
  # between simulated conditions from a multinomial processing 
  # model, then estimates a desired confidence interval.
  # Arguments:
  # v     - value for P(Recall)
  # alpha - Width of confidence interval
  # Returns:
  # The upper and lower bounds for the confidence interval 
  # around the difference scores.
  
  interval = numeric(2)
  interval[1] = ( 1 - alpha )/2
  interval[2] = interval[1] + alpha
  
  diff_scores = sim_diff_scores( v )
  out = quantile( diff_scores, prob = interval )
  
  return( out )
}

# Create pdf or sample figure
if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'Measurement_imprecision.pdf', width = 12 ) else 
    pdf( 'Measurement_imprecision_no_label.pdf', width = 12 )
  setwd( orig_dir )
} else {
  x11( width = 12 )
}

# Specify layout of figure panels
lyt = cbind( 2, 1 )
layout( lyt )

# Plotting characteristics
ptSz = 1.3
lnSz = 2
txtSz = 1.35

# Determine uncertainty intervals for multinomial
# processing model

# Recall corrected for guessing
V = seq( 0, 1, length = 100 )
I = V + .5*( 1 - V )
I = I * 100
ui = sapply( V, est_ui, alpha = .95 )
# Convert to percentages
ui = ui * 100

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
### Panel B
###
# Lookup - 03

# Extract initial performance
trn = aggregate( pd$Ac, list( pd$S, pd$IT ), mean )
colnames( trn ) = c( 'S', 'IT', 'P' )
# Convert to percentages
trn$P = trn$P * 100
# Separate targets/competitors
T_trn = trn$P[ trn$IT == 1 ]
C_trn = trn$P[ trn$IT == 2 ]

# Selective-retrieval versus baseline (Missing = trimmed)
cd = fr[ fr$MR == 0, ]
T_SRmB = ds_perf( cd, rbind( c(1,1), c(1,0) ), 
                  c( 'IT', 'SR', 'S' ) )
C_SRmB = ds_perf( cd, rbind( c(2,1), c(2,0) ), 
                  c( 'IT', 'SR', 'S' ) )
# Convert to percentages
T_SRmB = -T_SRmB * 100 # Positive values indicate more forgetting
C_SRmB = -C_SRmB * 100

# Define variables for scatter plot
x = c( T_trn, C_trn )
y = c( T_SRmB, C_SRmB )
# Seperate targets and competitors
clr = rep( c('black','blue'), each = 24 )

# Standardize variables
xs = scale( x )
ys = scale( y )

# Plotting scales
pxl = ( c( 20, 100 ) - mean( x ) )/sd( x )
pyl = ( c( -30, 30 ) - mean( y ) )/sd( y )

# Create blank plot
par( mar = c( 3, 4.5, 2, 1 ) )
pl = pyl
blankPlot( pxl, pyl )

# Add uncertainty based on multinomial processing tree
Is = ( I - mean( x ) )/sd( x )
uis = ui
uis[1,] = ( ui[1,] - mean( y ) )/sd( y )
uis[2,] = ( ui[2,] - mean( y ) )/sd( y )

# Stretch grey region to 20%
Is = c( ( 20.5 - mean( x ) )/ sd( x ), Is )
uis = cbind( uis[,1], uis )

polygon( c( Is, rev(Is) ),
         c( uis[1,], rev( uis[2,] ) ),
         col = 'grey',
         border = NA )

# Denote zero point
segments( pxl[1], ( 0 - mean(y) )/sd(y), 
          pxl[2], ( 0 - mean(y) )/sd(y), lty = 2, lwd = lnSz )

# Denote credible interval for chance performance
ub = qbeta( .975, 1 + 72, 1 + 72 ) * 100
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 2 )

abline( h = pyl[1], lwd = lnSz )
abline( v = pxl[1], lwd = lnSz )

mtext( 'Observed initial performance', side = 1, line = 2.1, cex = txtSz )
mtext( 'Final performance difference from baseline', 
       side = 2, line = 2, cex = txtSz )

xl = seq( 20, 100, 20 )
yl = seq( -30, 30, 10 )

axis( 1, ( ub - mean( x ) )/sd( x ), 'Chance', tick = F, 
      cex = txtSz * .8, line = -1.4 )
axis( 1, ( xl - mean( x ) )/sd( x ), 
      paste( xl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.25 )
axis( 2, ( yl - mean( y ) )/sd( y ), 
      paste( yl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.5 )

points( xs, ys, pch = 19, col = clr, cex = ptSz )

# Add legend
par( xpd = NA )
legend( pxl[1] + .5, pyl[2] + .75, 
        'Targets', fill = unique( clr )[1], bty = 'n',
        cex = txtSz * .8 )

legend( pxl[1] + 2.5, pyl[2] + .75, 
        'Competitors', fill = unique( clr )[2], bty = 'n',
        cex = txtSz * .8 )
par( xpd = F )

# Add legend
par( xpd = NA )
legend( pxl[1] + .5, pyl[2] + .5, 
        'Multinomial model predictions', 
        fill = 'grey', bty = 'n',
        cex = txtSz * .8 )
par( xpd = F )

# Add panel label
if ( panelYes ) legend( 'topleft', '  B', bty = 'n', cex = txtSz )

###
### Data extraction (Original)
###
# Lookup - 04

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
### Panel A
###
# Lookup - 05

# Initial performance
trn = aggregate( pd$Ac, list( pd$S, pd$IT ), mean )
colnames( trn ) = c( 'S', 'IT', 'P' )
# Convert to percentages
trn$P = trn$P * 100
# Separate targets/competitors
T_trn = trn$P[ trn$IT == 1 ]
C_trn = trn$P[ trn$IT == 2 ]

# Selective-retrieval versus baseline (Missing = trimmed)
cd = fr[ fr$MR == 0, ]
T_SRmB = ds_perf( cd, rbind( c(1,1), c(1,0) ), 
                  c( 'IT', 'SR', 'S' ) )
T_SRmB = -T_SRmB * 100
C_SRmB = ds_perf( cd, rbind( c(2,1), c(2,0) ), 
                  c( 'IT', 'SR', 'S' ) )
C_SRmB = -C_SRmB * 100

x = c( T_trn, C_trn )
y = c( T_SRmB, C_SRmB )
clr = rep( c('black','blue'), each = 24 )

xs = scale( x )
ys = scale( y )

pxl = ( c( 20, 100 ) - mean( x ) )/sd( x )
pyl = ( c( -30, 30 ) - mean( y ) )/sd( y )

# Create blank plot
par( mar = c( 3, 4.5, 2, 1 ) )
pl = pyl
blankPlot( pxl, pyl )

Is = ( I - mean( x ) )/sd( x )
uis = ui
uis[1,] = ( ui[1,] - mean( y ) )/sd( y )
uis[2,] = ( ui[2,] - mean( y ) )/sd( y )

# Stretch grey region to 20%
Is = c( ( 22.5 - mean( x ) )/ sd( x ), Is )
uis = cbind( uis[,1], uis )

polygon( c( Is, rev(Is) ),
         c( uis[1,], rev( uis[2,] ) ),
         col = 'grey',
         border = NA )

segments( pxl[1], ( 0 - mean(y) )/sd(y), 
          pxl[2], ( 0 - mean(y) )/sd(y), lty = 2, lwd = lnSz )

ub = qbeta( .975, 1 + 72, 1 + 72 ) * 100
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 2 )

abline( h = pl[1], lwd = lnSz )
abline( v = pl[1], lwd = lnSz )

mtext( 'Observed initial performance', side = 1, line = 2.1, cex = txtSz )
mtext( 'Final performance difference from baseline', 
       side = 2, line = 2, cex = txtSz )
# 'Final performance difference from baseline'
xl = seq( 20, 100, 20 )
yl = seq( -30, 30, 10 )

axis( 1, ( ub - mean( x ) )/sd( x ), 'Chance', tick = F, 
      cex = txtSz * .8, line = -1.4 )
axis( 1, ( xl - mean( x ) )/sd( x ), 
      paste( xl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.25 )
axis( 2, ( yl - mean( y ) )/sd( y ), 
      paste( yl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.5 )

points( xs, ys, pch = 19, col = clr, cex = ptSz )

points( xs[ c(15,N+15) ], ys[ c(15,N+15) ], pch = 21, col = 'red', 
        cex = ptSz * 1.5, lwd = 2 )

# Add legend
par( xpd = NA )
legend( pxl[1] + .5, pyl[2] + .8, 
        'Subject 15', fill = 'Red', bty = 'n',
        cex = txtSz * .8 )

# Add panel label
if ( panelYes ) legend( 'topleft', '  A', bty = 'n', cex = txtSz )

if ( savePlot ) dev.off()
setwd( orig_dir )