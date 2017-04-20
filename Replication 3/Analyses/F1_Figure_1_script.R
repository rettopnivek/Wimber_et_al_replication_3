#--------------------#
# Figure 1           #
# Kevin Potter       #
# Updated 04/19/2017 #
#--------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate whether a pdf should be generated
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Estimate uncertainty
# Lookup - 03:  Create base figure 
# Lookup - 04:  Panel A
# Lookup - 05:  Panel B (Final recogntion memory)
# Lookup - 06:  Panel C (Final recogntion memory)
# Lookup - 07:  Panel C (Initial performance)
# Lookup - 08:  Panel B (Initial performance)

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# Load in package for recreating jpeg image files
# install.packages( 'jpeg')
library(jpeg)

# Define some useful functions
source('F0_Useful_functions.R')

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )
# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Estimate uncertainty
###
# Lookup - 02

### Extract data for final recognition test ###

# Replication
sel = allData$Cond == 6
rd = allData[ sel, ]

# Original
sel = OriginalAllData$Cond == 6
od = OriginalAllData[ sel, ]

### Define useful covariates ###

# Dependent variable
rd$Y = rd$Accuracy; od$Y = od$Accuracy
# Set missing data to be 0
rd$Y[ is.na( rd$RT ) ] = 0
od$Y[ is.na( od$RT ) ] = 0

# Define specific intercepts for each condition

# Targets (Selective-retrieval)
rd$TSR = 0; rd$TSR[ rd$ImageType == 1 & rd$Baseline == 0 ] = 1
od$TSR = 0; od$TSR[ od$ImageType == 1 & od$Baseline == 0 ] = 1
# Targets (Baseline)
rd$TB = 0; rd$TB[ rd$ImageType == 1 & rd$Baseline == 1 ] = 1
od$TB = 0; od$TB[ od$ImageType == 1 & od$Baseline == 1 ] = 1
# Competitors (Selective-retrieval)
rd$CSR = 0; rd$CSR[ rd$ImageType == 2 & rd$Baseline == 0 ] = 1
od$CSR = 0; od$CSR[ od$ImageType == 2 & od$Baseline == 0 ] = 1
# Competitors (Baseline)
rd$CB = 0; rd$CB[ rd$ImageType == 2 & rd$Baseline == 1 ] = 1
od$CB = 0; od$CB[ od$ImageType == 2 & od$Baseline == 1 ] = 1

# Define covariates for condition, subjects, and images

# Image type (1 = target, 2 = competitor)
rd$IT = rd$ImageType; od$IT = od$ImageType;
# Selective retrieval ( 1 = yes, 0 = no )
rd$SR = 1 - rd$Baseline; od$SR = 1 - od$Baseline;
rd$S = as.factor( rd$Subject ); od$S = as.factor( od$Subject );
rd$I = as.factor( rd$ImageNum ); od$I = as.factor( od$ImageNum )

# Define priors
prior_vals = normal( 
  c( 1.775, 1.775, 1.475, 1.775 ), # Means
  rep( .3, 4 ) # Standard deviations
)

modelFit = F
if ( modelFit ) {
  
  # Fit the model to the original data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + # Fixed effects
                      (1|S) + (1|I), # Random effects
                    data = od, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 1828,
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Generate posterior simulations
  nd = od[,c('Y','TSR','TB','CSR','CB','S','I','IT','SR')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  setwd( 'Reanalysis results' )
  save( fit, post, nd, sim, prc, file = 'Figure_1_posteriors.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Reanalysis results' )
  load( 'Figure_1_posteriors.RData' )
  setwd( orig_dir )
}

# Define priors
prior_vals = normal( 
  c( 1.540, 1.629, 1.317, 1.780 ), # Means
  rep( .3, 4 ) # Standard deviations
)

modelFit = F
if ( modelFit ) {
  
  # Fit the model to the replication data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + # Fixed effects
                      (1|S) + (1|I), # Random effects
                    data = rd, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 4998,
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post_r = as.matrix( fit )
  
  # Generate posterior simulations
  nd = rd[,c('Y','TSR','TB','CSR','CB','S','I','IT','SR')]; nd$Y = 0;
  sim_r = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc_r = apply( sim_r, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  setwd( 'Replication results' )
  save( fit, post_r, nd, sim_r, prc_r, 
        file = 'Figure_1_posteriors.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Replication results' )
  load( 'Figure_1_posteriors.RData' )
  setwd( orig_dir )
}

###
### Create base figure 
###
# Lookup - 03

if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'Figure_1.pdf' ) else 
    pdf( 'Figure_1_no_label.pdf' )
  setwd( orig_dir )
} else x11()

# Define plot characteristics
ptSz = 2
lnSz = 2
txtSz = 1.5
posL1 = c( .5, .95 )
posL2 = c( .5, .95 )
uiSz = .05

# Define layout for figure
lyt = matrix( 6, 12, 12 )
lyt[ 1:6, ] = 1
lyt[ 7:9, 1:6 ] = 2
lyt[ 10:12, 1:6 ] = 3
lyt[ 7:9, 7:12 ] = 5
lyt[ 10:12, 7:12 ] = 4
layout( lyt )

###
### Panel A
###
# Lookup - 04

# Experimental design

par( mar = c(0,0,2,0) )
blankPlot()
shft = 0
library(jpeg)
setwd( 'Figures' )
x = readJPEG("Procedure.jpg")
rasterImage(x, shft/2, 0, 1 - shft/2, 1)
setwd( orig_dir )

# Add label for row
if ( panelYes ) title( '              a', adj = 0, cex.main = txtSz )

###
### Panel B (Final recognition memory)
###
# Lookup - 05

# Determine condition means for each set of data
old = aggregate( od$Y, list( od$IT, od$SR ), mean )
new = aggregate( rd$Y, list( rd$IT, rd$SR ), mean )
colnames( old ) = c( 'IT', 'SR', 'P' )
colnames( new ) = c( 'IT', 'SR', 'P' )

# Determine x-axis positions
xa = c( 1.1, 0.9, 1.9, 2.1 )
ord = c( 1, 2, 3, 4 )

# Create a blank plot
par( mar = c( 3, 5, 3, 2 ) )
blankPlot( c( .5, 2.5 ), c( .7, .9 ) )

abline( h = .7, col = 'black', lwd = lnSz )
abline( v = .5, col = 'black', lwd = lnSz )
axis( 2, c( .7, .8, .9 ),
      c( '70', '80', '90' ),
      tick = F, cex.axis = txtSz, line = -.5 )
axis( 1, 1:2, c( 'Baseline', 'Selective-retrieval' ),
      tick = F, cex.axis = txtSz )

par( xpd = T )
mtext( 'Percent correct', side = 2, cex = txtSz*.8, line = 3,
       adj = 3 )
par( xpd = F )

# Add label for row
if ( panelYes ) title( 'b           ', adj = 0, cex.main = txtSz,
                       line = 1.5 )

# Determine uncertainty intervals for original data
ui = apply( prc, 1, quantile, prob = c( .025, .975 ) )
arrows( xa, ui[1,ord], xa, ui[2,ord], code = 3, length = uiSz,
        angle = 90, col = 'red', lwd = lnSz )

# Plot original data
segments( xa[ c(1,2) ], old$P[ ord[ c(1,2)] ],
          xa[ c(3,4) ], old$P[ ord[ c(3,4)] ],
          lwd = lnSz, lty = 1:2, col = 'red' )
points( xa, old$P[ord], pch = c( 21, 22, 21, 22 ),
        bg = rep( c('red','white'), 2 ),
        col = 'red', 
        cex = ptSz )

par( xpd = T )
legend( posL2[1], posL2[2], c('Targets'),
        pch = c( 21 ), pt.bg = c('red'), col = 'red', 
        cex = txtSz, bty = 'n', horiz = T )
legend( posL2[1]+.7, posL2[2], c('Competitors'),
        pch = c( 22 ), pt.bg = c('white'), col = 'red', 
        cex = txtSz, bty = 'n', horiz = T )
par( xpd = F )

###
### Panel C (Final recognition memory)
###
# Lookup - 06

# Create a blank plot
par( mar = c( 3, 5, 3, 2 ) )
blankPlot( c( .5, 2.5 ), c( .7, .9 ) )

abline( h = .7, col = 'black', lwd = lnSz )
abline( v = .5, col = 'black', lwd = lnSz )
axis( 1, 1:2, c( 'Baseline', 'Selective-retrieval' ),
     tick = F, cex.axis = txtSz )
axis( 2, c( .7, .8, .9 ),
      c( '70', '80', '90' ),
      tick = F, cex.axis = txtSz, line = -.5 )

# Add label for row
if ( panelYes ) title( 'c             ', adj = 0, cex.main = txtSz,
                       line = 1.5 )

# Determine uncertainty intervals for replication data
ui = apply( prc_r, 1, quantile, prob = c( .025, .975 ) )
arrows( xa, ui[1,ord], xa, ui[2,ord], code = 3, length = uiSz,
        angle = 90, col = 'black', lwd = lnSz )

# Plot original data
segments( xa[ c(1,2) ], new$P[ ord[ c(1,2)] ],
          xa[ c(3,4) ], new$P[ ord[ c(3,4)] ],
          lwd = lnSz, lty = 1:2, col = 'black' )
points( xa, new$P[ord], pch = c( 21, 22, 21, 22 ),
        bg = rep( c('black','white'), 2 ),
        col = 'black', 
        cex = ptSz )

par( xpd = T )
legend( posL2[1], posL2[2], c('Targets'),
        pch = c( 21 ), pt.bg = c('black'),
        cex = txtSz, bty = 'n', horiz = T )

legend( posL2[1]+.7, posL2[2], c('Competitors'),
        pch = c( 22 ), pt.bg = c('white'),
        cex = txtSz, bty = 'n', horiz = T )
par( xpd = F )

###
### Panel C (Initial performance)
###
# Lookup - 07

# Determine uncertainty intervals for multinomial
# processing model

# Recall corrected for guessing
V = seq( 0, 1, length = 100 )
I = V + .5*( 1 - V )
I = I * 100
ui = sapply( V, est_ui, alpha = .95 )
# Convert to percentages
ui = ui * 100

### Extract data for 3rd replication ###

# For easy manipulation
od = allData
colnames( od ) = c( 'S', 'Tr', 'Ph', 'IN', 'CN', 'Co', 
                    'Ch', 'Ac', 'RT', 'CR', 'IT', 'B', 
                    'Cat', 'Bl', 'ID' )

N = 48 # Number of subjects

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
pts = rep( c( 19, 22 ), each = 24 )

# Standardize variables
xs = scale( x )
ys = scale( y )

# Plotting scales
pxl = ( c( 20, 100 ) - mean( x ) )/sd( x )
pyl = ( c( -30, 30 ) - mean( y ) )/sd( y )

### Plot results ###

# Create blank plot
par( mar = c( 4, 4.5, 2, 2 ) )
pl = pyl
blankPlot( pxl, pyl )

# Add uncertainty based on multinomial processing tree
Is = ( I - mean( x ) )/sd( x )
uis = ui
uis[1,] = ( ui[1,] - mean( y ) )/sd( y )
uis[2,] = ( ui[2,] - mean( y ) )/sd( y )

polygon( c( Is, rev(Is) ),
         c( uis[1,], rev( uis[2,] ) ),
         col = 'grey',
         border = NA )

# Denote zero point
segments( pxl[1], ( 0 - mean(y) )/sd(y), 
          pxl[2], ( 0 - mean(y) )/sd(y), lty = 2, lwd = lnSz )

# Denote credible interval for chance performance
# ub = qbeta( .975, 1 + 72, 1 + 72 ) * 100
ub = 50
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 2 )
# Denote credible interval for chance performance
ub = qbeta( .975, 1 + 72, 1 + 72 ) * 100
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 3, col = 'grey50' )

abline( h = pyl[1], lwd = lnSz )
abline( v = pxl[1], lwd = lnSz )

mtext( 'Practice test', side = 1, line = 2.1, cex = txtSz*.7 )

par( xpd = T )
mtext( 'Percent forgetting (B - SR)', 
       side = 2, cex = txtSz*.8, line = 2.5,
       adj = -.1 )
par( xpd = F )

xl = seq( 20, 100, 20 )
yl = c( -30, 0, 30 )

axis( 3, ( ub - mean( x ) )/sd( x ), 'Chance', tick = F, 
      cex = txtSz * .9, line = -1.25 )
axis( 1, ( xl - mean( x ) )/sd( x ), 
      paste( xl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.25 )
axis( 2, ( yl - mean( y ) )/sd( y ), 
      c( '-30', '0', '30' ),
      tick = F, cex.axis = txtSz, line = -.5 )

points( xs, ys, pch = pts, bg = 'white', cex = ptSz*.8 )

###
### Panel B (Initial performance)
###
# Lookup - 08

### Extract data for original study ###

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

# Initial performance
trn = aggregate( pd$Ac, list( pd$S, pd$IT ), mean )
colnames( trn ) = c( 'S', 'IT', 'P' )
# Convert to percentages
trn$P = trn$P * 100
# Separate targets/competitors
T_trn = trn$P[ trn$IT == 1 ]
C_trn = trn$P[ trn$IT == 2 ]

# Selective-retrieval versus baseline (Missing = error)
cd = fr; cd$Ac[ fr$MR == 1 ] = 0
T_SRmB = ds_perf( cd, rbind( c(1,1), c(1,0) ), 
                  c( 'IT', 'SR', 'S' ) )
T_SRmB = -T_SRmB * 100
C_SRmB = ds_perf( cd, rbind( c(2,1), c(2,0) ), 
                  c( 'IT', 'SR', 'S' ) )
C_SRmB = -C_SRmB * 100

# Define variables for scatter plot
x = c( T_trn, C_trn )
y = c( T_SRmB, C_SRmB )
# Seperate targets and competitors
pts = rep( c( 19, 22 ), each = 24 )

# Standardize variables
xs = scale( x )
ys = scale( y )

# Plotting scales
pxl = ( c( 20, 100 ) - mean( x ) )/sd( x )
pyl = ( c( -30, 30 ) - mean( y ) )/sd( y )

### Plot results ###

# Create blank plot
par( mar = c( 4, 4.5, 2, 2 ) )
pl = pyl
blankPlot( pxl, pyl )

# Add uncertainty based on multinomial processing tree
Is = ( I - mean( x ) )/sd( x )
uis = ui
uis[1,] = ( ui[1,] - mean( y ) )/sd( y )
uis[2,] = ( ui[2,] - mean( y ) )/sd( y )

polygon( c( Is, rev(Is) ),
         c( uis[1,], rev( uis[2,] ) ),
         col = 'grey',
         border = NA )

# Denote zero point
segments( pxl[1], ( 0 - mean(y) )/sd(y), 
          pxl[2], ( 0 - mean(y) )/sd(y), lty = 2, lwd = lnSz )

# Denote 50% percent mark
ub = 50
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 2 )
# Denote credible interval for chance performance
ub = qbeta( .975, 1 + 72, 1 + 72 ) * 100
segments( ( ub - mean( x ) )/sd( x ), pl[1], 
          ( ub - mean( x ) )/sd( x ), pl[2], 
          lwd = lnSz, lty = 3, col = 'grey50' )

abline( h = pyl[1], lwd = lnSz )
abline( v = pxl[1], lwd = lnSz )

mtext( 'Practice test', side = 1, line = 2.1, cex = txtSz*.7 )

xl = seq( 20, 100, 20 )
yl = c( -30, 0, 30 )

axis( 3, ( ub - mean( x ) )/sd( x ), 'Chance', tick = F, 
      cex = txtSz * .9, line = -1.25 )
axis( 1, ( xl - mean( x ) )/sd( x ), 
      paste( xl, '%', sep = '' ),
      tick = F, cex.axis = txtSz, line = -.25 )
axis( 2, ( yl - mean( y ) )/sd( y ), 
      c('-30','0','30'),
      tick = F, cex.axis = txtSz, line = -.5 )

points( xs, ys, pch = pts, col = 'red', bg = 'white', cex = ptSz*.8 )

if (savePlot) dev.off()
setwd( orig_dir )