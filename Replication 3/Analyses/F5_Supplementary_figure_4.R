#------------------------#
# Supplementary figure 4 #
# Kevin Potter           #
# Updated 04/19/2017     #
#------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if models should be fitted
modelFit = F

# Indicate if a pdf should be saved
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Define functions to fit multinomial processing model
# Lookup - 03:  Fit hierarchical multinomial processing model
# Lookup - 04:  Create figure base
# Lookup - 05:  Data extraction (Replication)
# Lookup - 06:  Data extraction (Original)
# Lookup - 07:  Panel A
# Lookup - 08:  Panel B
# Lookup - 09:  Panel C

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# For geting github packages
# install.packages( 'devtools' )
# library(devtools)

# Define some useful functions
source('F0_Useful_functions.R')

# Load in original data
load( 'Data/Original_all_data.RData' )
# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

###
### Fit hierarchical multinomial processing model
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

# Create dummy variables for each condition
od$TSR = 0; od$TSR[ od$IT == 1 & od$SR == 1 ] = 1
od$TB = 0; od$TB[ od$IT == 1 & od$SR == 0 ] = 1
od$CSR = 0; od$CSR[ od$IT == 2 & od$SR == 1 ] = 1
od$CB = 0; od$CB[ od$IT == 2 & od$SR == 0 ] = 1

# Effects coding
od$ITef = -1; od$ITef[ od$IT == 1 ] = 1;
od$SRef = -1; od$SRef[ od$B == 0 ] = 1
od$ITxSR = od$ITef * od$SRef

# Intercept
od$Int = 1

# Extract performance during initial training
pd = od[ od$Ph > 1 & od$Ph < 5, ]
# Define variable for repeat training
pd$Pract = 0; pd$Pract[ pd$Ph == 3 ] = 1

# Calculate estimate of P(Recall) from multinomial 
# process model for initial performance ( 2nd training 
# phase of targets and competitors
init_perf = aggregate( pd$Ac, list( pd$S, pd$IT, pd$Ph ), sum )
colnames( init_perf ) = c( 'S', 'IT', 'Ph', 'Y' )
init_perf$N = aggregate( pd$Int, list( pd$S, pd$IT, pd$Ph ), sum )$x
# Round counts less then N/2 to N/2 + 1/2
init_perf$Y[ init_perf$Y < init_perf$N/2 ] = 
  init_perf$N[ init_perf$Y < init_perf$N/2 ]/2 + .5
# Adjust for subjects with exactly at-chance performance
init_perf$P = init_perf$Y/init_perf$N
# Exclude initial training for targets
init_perf = init_perf[ init_perf$Ph > 2, ]
# Compute P(Recall)
init_perf$eta = 2 * init_perf$P - 1
# Compute logit of P(Recall)
init_perf$eta_logit = logit( init_perf$eta )

# Extract performance for selective-retrieval phase
sr = Extract_SR( OriginalAllData[ od$Ph == 5, ] )

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

# Planned comparison
fr$PC = 0
fr$PC[ fr$IT == 2 & fr$SR == 1 ] = 1
fr$PC[ fr$IT == 2 & fr$SR == 0 ] = -1

# Incorporate estimates for the logit of P(Recall) 
# for past performance
fr$PP = 0
for ( s in 1:N ) {
  sel = fr$S == s & fr$IT == 1
  fr$PP[ sel ] = 
    init_perf$eta_logit[ init_perf$S == s & init_perf$IT == 1 ]
  sel = fr$S == s & fr$IT == 2
  fr$PP[ sel ] = 
    init_perf$eta_logit[ init_perf$S == s & init_perf$IT == 2 ]
}

fr$Cnd = 0
fr$Cnd[ fr$IT == 2 & fr$SR == 0 ] = 1
fr$Cnd[ fr$IT == 1 & fr$SR == 0 ] = 2
fr$Cnd[ fr$IT == 1 & fr$SR == 1 ] = 3
fr$Cnd[ fr$IT == 2 & fr$SR == 1 ] = 4

# Exclude missing data and subject w/ highest amount of missing
dtbf = fr[ fr$MR == 0, ]

# Define regression coefficents

# Int - Overall learning/forgetting (+ = learning)
# Itef - Adjustment based on image type (+ = targets)
# SRef - Adjustment based on manipulation type (+ = selective retrieval)
# ItxSR - Interaction
IV1 = c( 'Int', 'ITef', 'SRef', 'ITxSR' )

# CB  - Simple effect for competitors (Baseline)
# TB  - Simple effect for targets (Baseline)
# TSR - Simple effect for targets (Selective retrieval)
# CSR - Simple effect for competitors (Selective retrieval)
IV2 = c( 'CB', 'TB', 'TSR', 'CSR' )

if ( modelFit ) {
  m1 = model_fit( dtbf, IV1, beta_offsets = dtbf$PP )
  m2 = model_fit( dtbf, IV2, beta_offsets = dtbf$PP )
  
  # Save results
  setwd( 'Reanalysis results' )
  save( m1, m2, file = 'MPM_posteriors.RData' )
  setwd( orig_dir )
} else {
  # Load results
  setwd( 'Reanalysis results' )
  load( file = 'MPM_posteriors.RData' )
  setwd( orig_dir )
}

###
### Create figure base
###
# Lookup - 03

# Create pdf or sample figure
if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'Supplementary_figure_4.pdf' ) else 
    pdf( 'Supplementary_figure_4_no_label.pdf' )
  setwd( orig_dir )
} else {
  x11()
}

# Specify layout of figure panels
lyt = matrix( c( 1, 2, 3, 3 ), 2, 2, byrow = T )
layout( lyt )

###
### Data extraction (Replication)
###
# Lookup - 04

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
# Lookup - 05

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
# Lookup - 06

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

legend( 4, 4,
        c( paste( 'Total (original): ', total_orig, '%', sep = '' ),
           paste( 'Total (replication): ', total_rep, '%', sep = '' )
        ),
        bty = 'n', cex = txtSz * .8 )

# Add panel label
if ( panelYes ) legend( 'topleft', '    a', bty = 'n', cex = txtSz )

###
### Panel B
###
# Lookup - 07

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

mtext( 'Competitor intrusions', side = 1, line = 2, cex = txtSz )
mtext( 'Estimated RIF', 
       side = 2, line = 2, cex = txtSz )

# Plot points
points( x, y, pch = 19, cex = ptSz )

# Add lines for regression
R = numeric( 2 )
R[1] = cor( x, y, method = 'spearman' )
print( cor.test( x, y, method = 'spearman' ) )

segments( pl[1], pl[1] * R[1],
          pl[2], pl[2] * R[1],
          lwd = lnSz, col = 'red', lty = 2 )

# Exclude subject with most missing data
exclude = md$S[ which( md$P == max( md$P ) ) ]
points( x[ exclude ], y[ exclude ],
        pch = 21, col = 'red', bg = NA, cex = ptSz * 1.5 )

R[2] = cor( x[-exclude], y[-exclude], method = 'spearman' )
print( cor.test( x[-exclude], y[-exclude], method = 'spearman' ) )

segments( pl[1], pl[1] * R[2],
          pl[2], pl[2] * R[2],
          lwd = lnSz )


text( 2.8, 3, substitute(paste(rho, " = ", r1, ' '), list(r1 = round(R[1],2))), pos = 2,
      col = 'red', cex = txtSz * .8 )
text( 2.8, 2.4, substitute(paste(rho, " = ", r2, '*'), list(r2 = round(R[2],2))), pos = 2,
      col = 'black', cex = txtSz * .8 )

legend( pl[1], pl[1] + .15*(pl[2] - pl[1]),
        '17% missing', fill = 'red', bty = 'n',
        cex = txtSz * .8 )

# Add panel label
if ( panelYes ) legend( 'topleft', 'b', bty = 'n', cex = txtSz )

###
### Panel C
###
# Lookup - 08

v1 = apply( m1$post$beta, 2, post_prob, greater = T )
v2 = apply( m1$post$beta, 2, post_prob, greater = F )
pval = pmin( v1, v2 )
pval = c( '0.065', '0.000', '0.015', '0.076' )

mds = apply( m2$post$beta, 2, findMode )
ui = round( apply( m2$post$beta, 2, quick_ui ), 2 )

v1 = apply( m2$post$beta, 2, post_prob, greater = T )
v2 = apply( m2$post$beta, 2, post_prob, greater = F )
pval = pmin( v1, v2 )
pval = c( '0.000', '0.087', '0.013', '0.000' )

xl = c(.5,2.5)
yl = lowerUpper( .25, as.vector( ui ) )
par( mar = c( 3, 5, 1, 5 ) )
blankPlot( xl, yl )
abline( h = yl[1], lwd = lnSz )
abline( v = xl[1], lwd = lnSz )
segments( xl[1],0,xl[2],0, 
          lty = 2, lwd = lnSz, col = 'grey' )

mtext( 'Change from practice test (logits)', side = 2, 
       cex = txtSz * .85, line = 2 )
axis( 2, c( -1, 0, 1, 2 ), 
      tick = F, line = -1, cex.axis = txtSz )

axis( 1, c(1,2), c( 'Baseline', 'Selective retrieval' ),
      tick = F, line = -.5, cex.axis = txtSz )

xa = c( .9, 1.1, 1.9, 2.1 )
arrows( xa, ui[1,], xa, ui[2,], code = 3, length = .05,
        angle = 90, lwd = lnSz )
segments( xa[1:2], mds[1:2],
          xa[4:3], mds[4:3],
          lwd = lnSz, lty = 1:2 )
points( xa, mds, pch = c( 22, 19, 19, 22 ),
        bg = 'white', cex = ptSz )

legend( 'topright', c( 'Targets', 'Competitors' ),
        pch = c( 19, 22 ), pt.bg = 'white',
        cex = txtSz * .9, bty = 'n' )

par( xpd = T )
arrows( rep( xl[2] + .01, 2 ), c( -.25, .25 ), 
        rep( xl[2] + .01, 2 ), c( -1, 1 ), 
        lwd = lnSz, code = 2:1 )
par( xpd = F )
axis( 4, c( -.7, .7 ), c( 'Forgetting', 'Learning' ), 
      cex.axis = txtSz * .85, line = -.5, tick = F )

# Add panel label
if ( panelYes ) legend( xl[1] - .07, yl[2] + .2, 
                        'c', bty = 'n', cex = txtSz )

if ( savePlot ) dev.off()