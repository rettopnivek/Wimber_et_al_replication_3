#-------------------------------------------------#
# Script to generate tables and statistical tests #
# Kevin Potter                                    #
# Updated 04/17/2017                              #
#-------------------------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate which code segments to run
runCode = c( F, F, F, F, F, F )

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Extract data
# Lookup - 03:  Final recognition memory test
# Lookup - 04:  BF test for chance performance (Subject 15)
# Lookup - 06:  Forgetting effect for subject 15 versus the average
# Lookup - 06:  Relation between visualization and average 
#               performance
# Lookup - 07:  Planned comparison for competitors/targets 
#               during training
# Lookup - 08:  Demographics

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# Load in packages for mixed effects modeling (MLE)
# install.packages( 'lme4' )
library( lme4 )

# Load in package for Bayes' factor
# install.packages('BayesFactor')
library( BayesFactor )

# Define some useful functions
source( 'F0_Useful_functions.R' )

# Load in original data
load( 'Data/Original_all_data.RData' )

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

list_extract = function( lst, val = 1, prec = NULL ) {
  # Purpose:
  # Convenience function to extract numeric vectors from 
  # a list and round them to a desired precision.
  # Arguments:
  # lst  - A list with a specific hierarchy:
  #        lst ->
  #               N additional lists -> 
  #                                     lists of numeric vectors 
  #                                     to index
  # val  - Index for internal list
  # prec - Number of decimal places to round up to (Optional)
  # Returns:
  # A matrix with N rows containing the extracted numeric vectors.
  
  # Default value for list index
  if ( length( val ) == 0 ) val = 1
  
  N = length( lst )
  
  if ( is.vector( lst[[1]][[ val ]] ) ) {
    
    L = length( lst[[1]][[ val ]] ) # Length of vector
    out = matrix( NA, N, L ) # Define output
    
    for ( n in 1:N ) {
      out[ n, ] = lst[[ n ]][[ val ]]
    }
    
    if ( length( prec ) > 0 ) out = round( out, prec )
    
  } else stop( 'Not a numeric vector' )
  
  return( out )
}

prop_change = function( fit, dat, IV ) {
  # Purpose:
  # Computes the change in probability over the levels of an 
  # independent variable.
  # Arguments
  # fit - An 'lmer' fit object
  # dat - The data frame with the dependent and independent variables
  # IV  - The independent variable to examine
  # Returns:
  # The estimated probabilities for each level of the independent 
  # variable.
  
  lvls = sort( unique( dat[,IV] ) )
  
  est = fixef( fit )
  
  # Extract intercept
  int = est['(Intercept)']
  
  base_prob = logistic( int )
  new_prob = logistic( int + lvls * est[ IV ] )
  
  out = new_prob - base_prob
  names( out ) = lvls
  
  return( out )
}

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

# Create index for levels of conditions
od$Cnd = 1; od$Cnd[ od$TB == 1 ] = 2;
od$Cnd[ od$TSR == 1 ] = 3; od$Cnd[ od$CSR == 1 ] = 4;

# Effects coding
od$ITef = -1; od$ITef[ od$IT == 1 ] = 1;
od$SRef = -1; od$SRef[ od$B == 0 ] = 1
od$ITxSR = od$ITef * od$SRef

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

###
### Final recognition memory test
###
# Lookup - 03

# For the final recognition memory test (image type by 
# manipulation type), the tests of interest are:
# 1) A standard repeated-measures ANOVA on proportion correct
# 2) A hierarchical logistic regression on accuracy with subject effects
# 3) A hierarchical logistic regression on accuracy with subject and 
#    item effects
# 4) A hierarchical multinomial process model on accuracy with 
#    subject and item effects
# These will be done to 3 data sets:
# 1) 24 SS (Missing = error)
# 2) 24 SS (Missing = trimmed)
# 3) 24 SS (Missing = trimmed) *adjust for past performance*
# The effects of interest are:
# 1) The main effect of manipulation type (selective-retrieval versus 
#    baseline)
# 2) The interaction between image type and manipulation type

if ( runCode[1] ) {
  
  # Create list to contain results for statistical tests
  all_res = list(
    std_aov = NULL,
    logistic_s = NULL,
    logistic_s_i = NULL,
    mpm_s_i = NULL
  )
  # Create internal lists for results of each data set
  for ( i in 1:4 ) {
    
    for ( j in 1:3 ) {
      all_res[[i]] = c( all_res[[i]], list(NULL) )
    }
    
  }
  
  # Create the data-sets of interest
  all_dtbf = c()
  for ( i in 1:3 ){
    all_dtbf = c( all_dtbf, list(NULL) )
  }
  names( all_dtbf ) = c( 'D1', 'D2', 'D3' )
  
  cd = fr;
  cd$Ac[ fr$MR == 1 ] = 0
  all_dtbf$D1 = cd
  sel = fr$MR == 0
  all_dtbf$D2 = fr[ sel, ]
  all_dtbf$D3 = fr[ sel, ]
  
  ### Standard repeated-measures ANOVA ###
  # Lookup - 03a
  
  for ( i in 1:3 ) {
    
    cd = all_dtbf[[i]]
    dtbf = aggregate( cd$Ac, list( cd$S, cd$IT, cd$SR ), mean )
    
    # If original data, round proportions to match Wimber et al. results
    if ( i == 1 ) dtbf$x = round( dtbf$x, 2 )
    # To adjust for past performance, use difference scores
    if ( i == 3 ) {
      sel = pd$Ph == 3 | pd$Ph == 4
      tmp = aggregate( pd$Ac[sel], list( 
        pd$S[sel], pd$IT[sel] ), mean )
      colnames( tmp ) = c( 'S', 'IT', 'P' )
      tmp = rbind( tmp, tmp )
      dtbf$x = dtbf$x - tmp$P
    }
    
    colnames( dtbf ) = c( 'Subject', 'Group.1', 'Group.2', 'x' )
    
    all_res$std_aov[[i]] = my_RM_ANOVA( dtbf, display = F )
  }
  names( all_res$std_aov ) = names( all_dtbf )
  
  ### Hierarchical logistic w/ subject effects ###
  # Lookup - 03c
  
  for ( i in 1:3 ) {
    
    cd = all_dtbf[[i]]
    
    # Fit a hierarchical logistic regression with standard ANOVA 
    # coefficients and a random intercept for subjects
    if ( i < 3 ) {
      fit = glmer( Ac ~ ITef + SRef + ITxSR + # Fixed effects
                     (1|S), # Random effects
                   family = binomial('logit'),
                   data = cd )
      res = summary(fit)
    } else {
      fit = glmer( Ac ~ ITef + SRef + ITxSR + # Fixed effects
                     (1|S), # Random effects
                   offset = PP, # Constants for past performance
                   family = binomial('logit'),
                   data = cd )
      res = summary(fit)
    }
    
    all_res$logistic_s[[i]] = list(
      fit = fit,
      res = res,
      est = fixef( fit ),
      pval = res$coefficients[-1,4]
    )
  }
  names( all_res$logistic_s ) = names( all_dtbf )
  
  ### Hierarchical logistic w/ subject and item effects ###
  # Lookup - 03d
  
  for ( i in 1:3 ) {
    
    cd = all_dtbf[[i]]
    
    # Fit a hierarchical logistic regression with standard ANOVA 
    # coefficients and random intercepts for subjects and items
    if ( i < 3 ) {
      fit = glmer( Ac ~ ITef + SRef + ITxSR + # Fixed effects
                     (1|S) + (1|IN), # Random effects
                   family = binomial('logit'),
                   data = cd )
      res = summary(fit)
    } else {
      fit = glmer( Ac ~ ITef + SRef + ITxSR + # Fixed effects
                     (1|S) + (1|IN), # Random effects
                   offset = PP, # Constants for past performance
                   family = binomial('logit'),
                   data = cd )
      res = summary(fit)
    }

    all_res$logistic_s_i[[i]] = list(
      fit = fit,
      res = res,
      est = fixef( fit ),
      pval = res$coefficients[-1,4]
    )
  }
  names( all_res$logistic_s_i ) = names( all_dtbf )
  
  ### Hierarchical multinomial process model ###
  # Lookup - 03e
  
  for ( i in 1:3 ) {
    
    cd = all_dtbf[[i]]
    
    # Fit a hierarchical multinomial process model with standard 
    # ANOVA coefficients, random intercepts for subjects and items
    if ( i < 3 ) {
      fit = model_fit( cd, c( 'Int', 'ITef', 'SRef', 'ITxSR' ) )
    } else {
      fit = model_fit( cd, c( 'Int', 'ITef', 'SRef', 'ITxSR' ), 
                       beta_offsets = cd$PP )
    }
    
    v1 = apply( fit$post$beta, 2, post_prob, greater = T )
    v2 = apply( fit$post$beta, 2, post_prob, greater = F )
    pval = pmin( v1, v2 )
    res = apply( fit$post$beta, 2, S )
    ui = apply( fit$post$beta, 2, quick_ui )
    
    all_res$mpm_s_i[[i]] = list(
      res = res,
      ui = ui,
      est = as.vector( apply( fit$post$beta, 2, findMode ) ),
      pval = pval
    )
    
    rm( fit, pval, v1, v2, res, ui )
  }
  names( all_res$mpm_s_i ) = names( all_dtbf )
  
  
}

###
### BF test for chance performance (Subject 15)
###
# Lookup - 04

if ( runCode[2] ) {
  
  chance_perf = list(
    train = NULL,
    sr = NULL )
  
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
  
  # Linear trends
  dtbf = data.frame(
    U = as.numeric( sr$Choice[sel] == 4 ),
    H = as.numeric( sr$Choice[sel] == 1 ),
    CR = od$CR[ od$Ph == 5 ][sel] )
  
  # Generalized linear model for picking 'unknown'
  fit = glm( U ~ CR, data = dtbf )
  res = summary( fit )
  pval = numeric(2)
  pval[1] = res$coefficients[2,4]
  
  # Generalized linear model for picking target
  fit = glm( H ~ CR, data = dtbf )
  res = summary( fit )
  pval[2] = res$coefficients[2,4]
  
  names( pval ) = c( 'Unknown', 'Hits' )
  
}

###
### Forgetting effect for subject 15 versus the average
###
# Lookup - 05

if ( runCode[3] ) {
  
  forgetting = matrix( NA, 2, 2 )
  colnames( forgetting ) = c( 'S15', 'Avg' )
  
  # Compute difference scores for forgetting w/ competitors
  cd = fr; cd$Ac[ cd$MR == 1 ] = 0 # Missing = error
  C_BmSR = ds_perf( cd, 
                    rbind( c( 2, 0 ), c( 2, 1 ) ), 
                    c( 'IT', 'SR', 'S' ) )
  forgetting[1,] = c( round( 100 * C_BmSR[15] ),
                      round( 100 * mean( C_BmSR ) ) )
  
  # Compute difference scores for forgetting w/ competitors
  cd = fr[ fr$MR == 0, ] # Missing = trimmed
  C_BmSR = ds_perf( cd, 
                    rbind( c( 2, 0 ), c( 2, 1 ) ), 
                    c( 'IT', 'SR', 'S' ) )
  forgetting[2,] = c( round( 100 * C_BmSR[15] ),
                      round( 100 * mean( C_BmSR ) ) )
}

###
### Relation between visualization and average performance
###
# Lookup - 06

if ( runCode[4] ) {
  
  # Extract reported degree of visualization
  V = allLogs[ !is.na( allLogs$Subject ), 
               c('Subject','Visualization') ]
  V = V[ order(V[,1]), ]
  V = as.data.frame( V )
  colnames( V ) = c('S','V')
  
  # Extract performance for selective-retrieval phase
  sr_r = Extract_SR( allData[ allData$Cond == 5, ] )
  SR = aggregate( sr_r$Choice == 1, list( sr_r$Subject ), mean )
  colnames( SR ) = c( 'S', 'H' )
  # Relation between performance
  cor.test( SR$H, V$V, method )
  
  # Extract performance for final test
  fr_r = allData[ allData$Cond == 6, ]
  FR = aggregate( fr_r$Accuracy, list( fr_r$Subject ), mean )
  colnames( FR ) = c( 'S', 'P' )
  
  cor.test( FR$P, V$V )
  
  colnames( fr_r ) = c( 'S', 'Tr', 'Ph', 'IN', 'CN',
                        'Co', 'Ch', 'Ac', 'RT', 'CR',
                        'IT', 'B', 'Cat', 'Bl', 'ID' )
  C_BmSR = ds_perf( fr_r, rbind( c(2,1), c(2,0) ), c('IT', 'B', 'S' ) )
  cor.test( C_BmSR, V$V )
}

###
### Planned comparison for competitors/targets during training
###
# Lookup - 07

if ( runCode[5] ) {
  
  # Descriptive statistics
  tst = aggregate( pd$Ac, list( pd$Ph, pd$S ), mean )
  colnames( tst ) = c( 'Ph', 'S', 'P' )
  ds = aggregate( tst$P, list( tst$Ph ), S )
  
  # Extract data to be fitted
  dtbf = pd
  dtbf$PC = 0;
  dtbf$PC[ pd$Ph == 3 ] = 1; dtbf$PC[ pd$Ph == 4 ] = -1;
  
  fit = glmer( Ac ~ PC + # Fixed effects
                 (1|S) + (1|IN), # Random effects
               family = binomial('logit'),
               data = dtbf )
  res = summary( fit )
  
}

###
### Demographics
###
# Lookup - 08

if ( runCode[6] ) {
  
  print( 'Sex' )
  print( table( allDemographics$Sex[ 
    !is.na( allDemographics$Subject ) ] ) )
  
  print( 'Mean age' )
  print( round( mean( as.numeric( allDemographics$Age[ 
    !is.na( allDemographics$Subject ) ] ) ), 2 ) )
  
  print( 'SD age' )
  print( round( sd( as.numeric( allDemographics$Age[ 
    !is.na( allDemographics$Subject ) ] ) ), 2 ) )
  
  print( 'Range age' )
  print( range( as.numeric( allDemographics$Age[ 
    !is.na( allDemographics$Subject ) ] ) ) )
  
}

###
### Time-out responses
###
# Lookup - 09

if ( runCode[7] ) {
  
  # Extract selective retrieval data
  sr = Extract_SR( allData[ allData$Cond == 5, ] )
  Int = aggregate( sr$Choice == 1, list( sr$Subject ), mean )
  colnames( Int ) = c( 'S', 'P' )
  
  # Compute time-out responses for final task
  fr = allData[ allData$Cond == 6, ]
  n_timeout = aggregate( fr$RT, 
                   list( fr$Subject ), 
                   function(x) sum( x < .3 )/length(x) )
  colnames( n_timeout ) = c('S','P')
  
  print( round( n_timeout$P[ which( n_timeout$P > .25 ) ], 2 ) )
  
  Int$P[ which( n_timeout$P > .25 ) ]
  
}

setwd( orig_dir )