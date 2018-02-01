#-----------------------------------------------#
# Add additional subjects to results for Exp. 2 #
# Kevin Potter                                  #
# Updated 01/16/2018                            #
#-----------------------------------------------#

# Index
# Lookup - 01:  Load in data from additional subjects
# Lookup - 02:  Apply data pre-processing protocols
# Lookup - 03:  Collate with full data set

###
### Load in data from additional subjects
###
# Lookup - 01

# Clear the workspace
rm(list=ls())

# Save current working directory
orig_dir = getwd()

# Extract training results
setwd('Data/Additional_subjects_exp_2')

# Determine sample size
N_additional = length( grep( 'test.csv', dir() ) )

### Extract training data ###

allTrainData = c()

fNames = dir()
train_files = grep( 'train.csv', fNames )

for (n in 1:N_additional) {
  
  tmp = read.csv( file = fNames[ train_files[n] ], header = T )
  
  # Add subject identifier
  tmp = cbind( rep( n, nrow(tmp)), tmp )
  colnames(tmp)[1] = 'Subject'
  tmp$ID = strsplit( fNames[train_files[n]], split = '_' )[[1]][2]
  
  # Collate data
  allTrainData = rbind( allTrainData, tmp )
}

### Extract test data ###

allTestData = c()

fNames = dir()
test_files = grep( 'test.csv', fNames )

for (n in 1:N_additional) {
  
  tmp = read.csv( file = fNames[ test_files[n] ], header = T )
  
  # Add subject identifier
  tmp = cbind( rep( n, nrow(tmp)), tmp )
  colnames(tmp)[1] = 'Subject'
  tmp$ID = strsplit( fNames[train_files[n]], split = '_' )[[1]][2]
  
  # Collate data
  allTestData = rbind( allTestData, tmp )
}

# Clean up the workspace
rm(fNames,n,tmp,test_files,train_files)
setwd( orig_dir )

# Combine training and test data sets
newExp2Data = rbind( allTrainData, allTestData )

# Add additional variables
newExp2Data$Exp = 2
newExp2Data$BadInstruct = 0

# Make sure image type is labeled properly in final task
# Also make sure subjects who had flipped instructions 
# are labeled
for ( s in 1:N_additional ) {
  
  sel = newExp2Data$Subject == s & newExp2Data$Cond == 6
  
  check = length( unique( newExp2Data$ImageType[sel] ) )
  if ( check == 1 ) {
    
    sel = newExp2Data$Subject == s & newExp2Data$Cond == 2
    pT = newExp2Data[sel,]
    Targets = pT$ImageNum
    
    sel = newExp2Data$Subject == s & newExp2Data$Cond == 4
    pC = newExp2Data[sel,]
    Competitors = pC$ImageNum
    
    sel = newExp2Data$Subject == s & newExp2Data$Cond == 6
    fr = newExp2Data[sel,]
    
    ImageType = rep( 1, nrow( fr ) )
    ImageType[ fr$ImageNum %in% as.numeric( Competitors ) ] = 2
    
    sel = newExp2Data$Subject == s & newExp2Data$Cond == 6
    newExp2Data$ImageType[sel] = ImageType
    
  }
  
  id = unique( newExp2Data$ID[sel] )
  if ( id %in% as.character( 2:13 ) ) {
    sel = newExp2Data$Subject == s
    newExp2Data$BadInstruct[sel] = 1
  }
  
}

# Clean up workspace
rm( s, sel, Targets, Competitors, ImageType, pT, pC, fr )

###
### Apply data pre-processing protocols
###
# Lookup - 02

# Accuracy
sel = newExp2Data$Cond %in% c( 2, 3, 4 )
ac = aggregate( newExp2Data$Accuracy[sel],
                list( newExp2Data$Subject[sel] ), 
                function(x) {
                  out = c( sum(x), length(x), mean(x) );
                  return(out) } )
colnames( ac ) = c( 'S', 'x' )
ac$Y = ac$x[,1]; ac$N = ac$x[,2]; ac$P = ac$x[,3];
ac$x = NULL

BetaPriors = matrix( 1, nrow( ac ), 2  )
BetaPosteriors = BetaPriors + cbind( ac$Y, ac$N - ac$Y )
colnames(BetaPosteriors) = c('alpha','beta')
# Check whether credible intervals include .5
ui = apply( BetaPosteriors, 1, 
            function(x) qbeta( c(.005, .995 ), x[1], x[2] ) )
exclude = which( ui[1,] < .5 )

# Check time out responses
sel = newExp2Data$Cond %in% c( 2, 3, 4, 6 )
to = aggregate( newExp2Data$RT[sel], 
                list( newExp2Data$Subject[sel] ), 
                function(x) ( sum( is.na( x ) ) + sum( x > 3.5 ) )/length(x) )
colnames( to ) = c( 'S', 'TO' )
exclude = c( exclude, which( to$TO >= .25 ) )

sel = newExp2Data$Cond == 5
sr = aggregate( newExp2Data$Accuracy[sel], list(
  newExp2Data$ID[sel], newExp2Data$Subject[sel], newExp2Data$BadInstruct[sel] ),
  mean )
exclude = c( exclude, sr[ which( sr$x <= 1/3 ), 2 ] )
exclude = unique( exclude )

# Remove time out responses and 
# subjects

if ( length( exclude ) > 0 ) {
  
  sel = !(newExp2Data$Subject %in% exclude)
  newExp2Data = newExp2Data[sel,]
  
}

sel = !is.na( newExp2Data$RT ) & !(newExp2Data$RT > 3.5 )
newExp2Data = newExp2Data[sel,]

N_additional = N_additional - length( exclude )

###
### Collate with full data set
###
# Lookup - 03

# Load in full set of data for Exp. 1 and 2
setwd( orig_dir )
load( 'Data/Combined_replication_data.RData' )

# Adjust subjects
newExp2Data$Subject = createIncrement( newExp2Data$Subject )

# Add to full data set
prevRepData$ID = prevRepData$Subject
prevRepData = rbind( prevRepData[ prevRepData$Exp == 1, ], newExp2Data )

# Save the results
setwd(orig_dir)
save(N,N_additional,prevRepData,data_preprocessing,
     file = 'Data/Combined_replication_data.RData')
