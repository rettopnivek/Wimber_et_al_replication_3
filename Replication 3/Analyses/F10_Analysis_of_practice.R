#-------------------------#
# Measurement imprecision #
# Kevin Potter            #
# Updated 10/04/2017      #
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

# Store data
rep_dat = list(
  pd = pd, sr = sr, fr = fr
)

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

# Store data
orig_dat = list(
  pd = pd, sr = sr, fr = fr
)


###
### Fit planned contrast for practice performance (Original)
###
# Lookup - 04

dtbf = orig_dat$pd[,c('Ac','S','IN')]
# Create planned contrast
dtbf$Cmp = 0; dtbf$Cmp[ orig_dat$pd$IT == 2 ] = 1;
dtbf$T2 = 0; dtbf$T2[ orig_dat$pd$Pract == 1 ] = 1;

dtbf$S = as.factor( dtbf$S ); dtbf$IN = as.factor( dtbf$IN )

res = glmer( Ac ~ 1 + Cmp + T2 + (1|S) + (1|IN), family = 'binomial', data = dtbf )

###
### Fit planned contrast for practice performance (Replications)
###
# Lookup - 05

dtbf = rep_dat$pd[,c('Ac','S','IN')]
# Create planned contrast
dtbf$Cmp = 0; dtbf$Cmp[ rep_dat$pd$IT == 2 ] = 1;
dtbf$T2 = 0; dtbf$T2[ rep_dat$pd$Pract == 1 ] = 1;

dtbf$S = as.factor( dtbf$S ); dtbf$IN = as.factor( dtbf$IN )

res2 = glmer( Ac ~ 1 + Cmp + T2 + (1|S) + (1|IN), family = 'binomial', data = dtbf )

setwd( orig_dir )