#-----------------------------------#
# Miscellaneous statistical tests   #
# Kevin Potter                      #
# Updated 02/01/2018                #
#-----------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate which code segments to run
runCode = c( T, T, T, F, F )

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Extract data
# Lookup - 03:  BF test for chance performance (Subject 15)
# Lookup - 04:  Examination of missing data
# Lookup - 05:  ANOVA with recoded missing data

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

# Load in package for Bayes' factor
# install.packages('BayesFactor')
library(BayesFactor)

# Define some useful functions
source( 'F0_Useful_functions.R' )

# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Extract data
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

# Extract performance during initial training
pd = od[ od$Ph > 1 & od$Ph < 5, ]
# Define variable for repeat training
pd$Pract = 0; pd$Pract[ pd$Ph == 3 ] = 1

# Extract performance for selective-retrieval phase
sr = Extract_SR( OriginalAllData[ od$Ph == 5, ] )

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

# Planned comparison
fr$PC = 0
fr$PC[ fr$IT == 2 & fr$SR == 1 ] = 1
# fr$PC[ fr$SR == 0 ] = -1

###
### BF test for chance performance (Subject 15)
###
# Lookup - 03

if ( runCode[1] ) {
  
  chance_perf = list(
    train = NULL,
    sr = NULL,
    final = NULL )
  
  sbj = 15
  
  # Training stage
  # Null = .5, Alternative = p > .5, 1st option
  sel = pd$S == sbj # Subject to evaluate
  bf_train = proportionBF( sum( pd$Ac[ sel ] ), sum( sel ), .5,
                           nullInterval = c( .5, 1 ) )
  chance_perf$train = as.vector( 1/bf_train[1] )
  
  # Selective-retrieval stage
  # Null = 1/3, Alternative = p > 1/3, 2nd option
  sel = sr$Subject == sbj # Subject to evaluate
  bf_sr = proportionBF( sum( sr$Choice[sel] == 1 ), sum( sel ), 1/3,
                        nullInterval = c( 1/3, 1) )
  chance_perf$sr = as.vector( 1/bf_sr[1] )
  
  # Final test stage
  cd = fr; cd$Ac[ fr$MR == 1 ] = 0
  # Null = .5, Alternative = p > .5, 2nd option
  sel = cd$S == sbj # Subject to evaluate
  bf_final = proportionBF( sum( cd$Ac[ sel ] ), sum( sel ), .5,
                           nullInterval = c( .5, 1 ) )
  chance_perf$final = as.vector( 1/bf_final[1] )
  
}

###
### Examination of missing data
###
# Lookup - 04

if ( runCode[2] ) {
  
  # Create meaningful labels
  fr$SRL = 'Selective retrieval'
  fr$SRL[ fr$B == 1 ] = 'Baseline'
  fr$ITL = 'Target'
  fr$ITL[ fr$IT == 2 ] = 'Competitor'
  
  # Proportion of missing data over conditions
  prp = aggregate( is.na( fr$Ac ), list( fr$ITL, fr$SRL ), mean )
  colnames( prp ) = c( 'IT', 'SR', 'P' )
  
  ### Logistic regression ###
  
  # Dependent variable = 1 if missing, 0 otherwise
  fr$md = as.numeric( is.na( fr$Ac ) )
  
  # Intercept is P(Missing) for competitors undergoing SR
  
  # Covariate 1 is for baseline competitors
  fr$C1 = 0; fr$C1[ fr$B == 1 & fr$IT == 2 ] = 1
  # Covariate 2 2s for targets undergoing SR
  fr$C2 = 0; fr$C2[ fr$B == 0 & fr$IT == 1 ] = 1
  # Covariate 3 2s for baseline targets
  fr$C3 = 0; fr$C3[ fr$B == 1 & fr$IT == 1 ] = 1
  
  fit = glmer( md ~ C1 + C2 + C3 + 
                 (1|S) + (1|IN), 
               family = 'binomial',
               data = fr )
}

###
### ANOVA with recoded missing data
###
# Lookup - 05

if ( runCode[3] ) {
  
  # Create meaningful labels
  fr$SRL = 'Selective retrieval'
  fr$SRL[ fr$B == 1 ] = 'Baseline'
  fr$ITL = 'Target'
  fr$ITL[ fr$IT == 2 ] = 'Competitor'
  
  # Code missing as 0.5 instead of 0 or 1
  fr$Ac[ is.na( fr$Ac ) ] = .5
  
  # Proportion of missing data over conditions
  prp = aggregate( fr$Ac, list( fr$ITL, fr$SRL, fr$S ), mean )
  colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
  
  out = my_RM_ANOVA( prp )
}

setwd( orig_dir )