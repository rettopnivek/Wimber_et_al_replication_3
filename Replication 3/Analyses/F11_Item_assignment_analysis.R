#--------------------#
# Item assignment    #
# Kevin Potter       #
# Updated 10/16/2017 #
#--------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if a pdf should be saved
savePlot = T

# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Data extraction (Original)
# Lookup - 03:  Analysis of item assignment
# Lookup - 04:  Impact of item difficulty

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

# Load in package for Bayes' factor
# install.packages('BayesFactor')
library(BayesFactor)

# Define some useful functions
source('F0_Useful_functions.R')

# Load in original data
load( 'Data/Original_all_data.RData' )

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

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

N = 24 # Number of subjects

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

###
### Analysis of item assignment
###
# Lookup - 03

# Extract labels for items
item = unique( fr$IN )
item = sort( item )

# For summing over occurences
count = rep( 1, nrow( fr ) )

# Function to compute number of times a picture 
# was assigned to a given condition over subjects
n_subject = function( val, cnd ) {
  
  out = sum( count[ cnd & fr$IN == val ] )
  
  return( out )
}

# Initialize output
ia = matrix( NA, length( item ), 5 )
ia[,1] = item

# Loop over conditions
cnd = cbind( c( 1, 2, 1, 2 ), c( 1, 1, 0, 0 ) )
for ( i in 1:4 ) {
  
  sel = fr$IT == cnd[i,1] & fr$SR == cnd[i,2]
  
  ia[,1+i] = sapply( item, function( x ) n_subject( x, cnd = sel ) )
  
}

# Label columns
colnames( ia ) = c( 'item', 'T-SR', 'C-SR', 'T-B', 'C-B' )
ia = as.data.frame( ia )

# Range of items assigned to a given condition over the 24 subjects
print( range( ia[,-1] ) )

###
### Impact of item difficulty
###
# Lookup - 04

# Load in mixed effects model results
setwd( 'Reanalysis results' )
load( 'Figure_1_posteriors.RData' )
setwd( orig_dir )

# Extract estimates for item difficulties
item_col = grep( 'I:', colnames( post ) )
item_diff = apply( post[,item_col], 2, findMode )
names( item_diff ) = paste( 'I', item, sep = '_' )
ia$diff = item_diff

# Match categories to items
ia$Cat = aggregate( fr$Cat, list( fr$IN ), unique )$x

# Effects coding for regression
c2 = rep( 0, nrow( ia ) ); c2[ ia$Cat == 2 ] = 1
c3 = rep( 0, nrow( ia ) ); c3[ ia$Cat == 3 ] = 1
c2[ ia$Cat == 1 ] = -1; c3[ ia$Cat == 1 ] = -1

df = data.frame( y = ia$diff, c2 = c2, c3 = c3 )
# 
m1 = lmBF( y ~ c2, data = df )
m2 = lmBF( y ~ c2 + c3, data = df )
print( m2/m1 ) # Faces, objects, and scenes all differ

# Sample from posterior
m2_post = posterior( m2, iterations = 10000 )

sm = function(x) {
  out = c(
    xbar = mean(x),
    med = median(x),
    md = findMode(x),
    L = quantile( x, prob = 0.025 ),
    U = quantile( x, prob = 0.975 )
  )
  return( out )
}

print( round( apply( m2_post, 2, sm ), 2 ) )

# Objects appear to be significantly easier, and 
# scenes significantly more difficult, compared 
# to faces

# Plot results
x11( width = 12 );
layout( cbind( 1, 2 ) )

xl = c(.5,144.5); yl = c(-2,2);
blankPlot( xl, yl ); customAxes( xl, yl )

mtext( 'Pictures', side = 1, line = 1.5 )
mtext( 'Difficulty', side = 2, line = 2 )
axis( 2, c(-.75, 0, .75 ), c( 'Hard', 'Medium', 'Easy' ),
      line = -.75, tick = F )

ord = order( ia$diff )
clr = rep( 'black', 144 ) # Faces
clr[ ia$Cat == 2 ] = 'red' # Objects
clr[ ia$Cat == 3 ] = 'blue' # Scenes

points( 1:144, ia$diff[ord], col = clr[ord], pch = 19 )

legend( xl[1], yl[2], c( 'Faces', 'Objects', 'Scenes' ),
        fill = c( 'black', 'red', 'blue' ), bty = 'n' )

# Objects seem easier than faces/scenes

tmp = matrix( 0, 144, 4 )
for ( i in 1:144 ) {
  tmp[i,] = as.numeric( ia$diff[i] * ia[i,1:4+1] )
}

xl = c( .5, 4.5 ); yl = c( -1, 1 );
blankPlot( xl, yl ); customAxes( xl, yl )
points( 1:4, colMeans( tmp ), 
        pch = c( 19, 22, 19, 22 ),
        bg = 'white', cex = 1.4 )

axis( 1, c( 1.5, 3.5 ), c( 'Selective retrieval', 'Baseline' ),
      line = -.75, tick = F )
axis( 2, seq( -.5, .5, .25 ), line = -.75, tick = F )
mtext( 'Average item difficulty', side = 2, line = 2 )

legend( 'top', c( 'Targets', 'Competitors' ),
        pch = c( 19, 22 ), pt.bg = c( 'black', 'white' ),
         bty = 'n' )

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

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

###
### Analysis of item assignment
###
# Lookup - 03

# Extract labels for items
item = unique( fr$IN )
item = sort( item )

# For summing over occurences
count = rep( 1, nrow( fr ) )

# Function to compute number of times a picture 
# was assigned to a given condition over subjects
n_subject = function( val, cnd ) {
  
  out = sum( count[ cnd & fr$IN == val ] )
  
  return( out )
}

# Initialize output
ia = matrix( NA, length( item ), 5 )
ia[,1] = item

# Loop over conditions
cnd = cbind( c( 1, 2, 1, 2 ), c( 1, 1, 0, 0 ) )
for ( i in 1:4 ) {
  
  sel = fr$IT == cnd[i,1] & fr$SR == cnd[i,2]
  
  ia[,1+i] = sapply( item, function( x ) n_subject( x, cnd = sel ) )
  
}

# Label columns
colnames( ia ) = c( 'item', 'T-SR', 'C-SR', 'T-B', 'C-B' )
ia = as.data.frame( ia )

# Range of items assigned to a given condition over the 24 subjects
print( range( ia[,-1] ) )

###
### Impact of item difficulty
###
# Lookup - 04

# Load in mixed effects model results
setwd( 'Replication results' )
load( file = 'RM_results.RData' )
setwd( orig_dir )

# Extract estimates for item difficulties
item_col = grep( 'I:', colnames( post ) )
item_diff = apply( post[,item_col], 2, findMode )
names( item_diff ) = paste( 'I', item, sep = '_' )
ia$diff = item_diff

# Match categories to items
ia$Cat = aggregate( fr$Cat, list( fr$IN ), unique )$x

# Effects coding for regression
c2 = rep( 0, nrow( ia ) ); c2[ ia$Cat == 2 ] = 1
c3 = rep( 0, nrow( ia ) ); c3[ ia$Cat == 3 ] = 1
c2[ ia$Cat == 1 ] = -1; c3[ ia$Cat == 1 ] = -1

df = data.frame( y = ia$diff, c2 = c2, c3 = c3 )
# 
m1 = lmBF( y ~ c2, data = df )
m2 = lmBF( y ~ c2 + c3, data = df )
print( m2/m1 ) # Faces, objects, and scenes all differ

# Sample from posterior
m2_post = posterior( m2, iterations = 10000 )

sm = function(x) {
  out = c(
    xbar = mean(x),
    med = median(x),
    md = findMode(x),
    L = quantile( x, prob = 0.025 ),
    U = quantile( x, prob = 0.975 )
  )
  return( out )
}

print( round( apply( m2_post, 2, sm ), 2 ) )

# Objects appear to be significantly easier, and 
# scenes significantly more difficult, compared 
# to faces

# Plot results
x11( width = 12 );
layout( cbind( 1, 2 ) )

xl = c(.5,144.5); yl = c(-2,2);
blankPlot( xl, yl ); customAxes( xl, yl )

mtext( 'Pictures', side = 1, line = 1.5 )
mtext( 'Difficulty', side = 2, line = 2 )
axis( 2, c(-.75, 0, .75 ), c( 'Hard', 'Medium', 'Easy' ),
      line = -.75, tick = F )

ord = order( ia$diff )
clr = rep( 'black', 144 ) # Faces
clr[ ia$Cat == 2 ] = 'red' # Objects
clr[ ia$Cat == 3 ] = 'blue' # Scenes

points( 1:144, ia$diff[ord], col = clr[ord], pch = 19 )

legend( xl[1], yl[2], c( 'Faces', 'Objects', 'Scenes' ),
        fill = c( 'black', 'red', 'blue' ), bty = 'n' )

# Objects seem easier than faces/scenes

tmp = matrix( 0, 144, 4 )
for ( i in 1:144 ) {
  tmp[i,] = as.numeric( ia$diff[i] * ia[i,1:4+1] )
}

xl = c( .5, 4.5 ); yl = c( -1, 1 );
blankPlot( xl, yl ); customAxes( xl, yl )
points( 1:4, colMeans( tmp ), 
        pch = c( 19, 22, 19, 22 ),
        bg = 'white', cex = 1.4 )

axis( 1, c( 1.5, 3.5 ), c( 'Selective retrieval', 'Baseline' ),
      line = -.75, tick = F )
axis( 2, seq( -.5, .5, .25 ), line = -.75, tick = F )
mtext( 'Average item difficulty', side = 2, line = 2 )

legend( 'top', c( 'Targets', 'Competitors' ),
        pch = c( 19, 22 ), pt.bg = c( 'black', 'white' ),
        bty = 'n' )

