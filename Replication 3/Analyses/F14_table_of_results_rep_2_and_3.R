#-----------------------------------------------#
# Progressively more powerful tables of results #
# Kevin Potter                                  #
# Updated 01/03/2017                            #
#-----------------------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Extract data

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
source( 'F0_Useful_functions.R' )

# Load in previous replication data
load( 'Data/Combined_replication_data.RData' )

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

# Define function to extract data for final 
# recognition memory task
extract_current_data = function( df, type = 3 ) {
  
  # Rename columns for easy manipulation
  if ( type == 3 ) { # Replication 3
    colnames( df ) = c( 'S', 'Tr', 'Ph', 'IN', 'CN', 'Co', 
                        'Ch', 'Ac', 'RT', 'CR', 'IT', 'B', 
                        'Cat', 'Bl', 'ID' )
    
    # Extract performance for final recognition memory test
    fr = df[ df$Ph == 6, ]
    
  }
  if ( type == 2 ) { # Replication 2
    
    colnames( df ) = c(
      'S', 'Tr', 'Ph', 'IN', 'Cn', 'Bl', 'Co',
      'Ch', 'RT', 'Ac', 'CR', 'fName', 'IT', 'B',
      'Cat', 'Exp', 'BI' )
    
    # Extract data for final recognition test
    sel = df$Ph == 6 & df$Exp == 2 & df$BI == 0
    fr = df[ sel, ]
    
  }
  
  # Track missing data
  fr$md = F
  fr$md[ fr$RT > 3.5 ] = T
  
  # Define meaningful label for conditions
  fr$SRL = 'Selective retrieval'
  fr$SRL[ fr$B == 1 ] = 'Baseline'
  
  # Define meaningful label for image types
  fr$ITL = 'Target'
  fr$ITL[ fr$IT == 2 ] = 'Competitor'
  
  # Recode missing as errors
  fr$Acm = fr$Ac
  fr$Acm[ fr$md ] = 0
  
  # Create factors for standard ANOVA analysis
  fr$FIT = 1 # Main effect of image type
  fr$FIT[ fr$ITL == 'Competitor' ] = -1
  fr$FSR = 1 # Main effect of condition
  fr$FSR[ fr$SRL == 'Baseline' ] = -1
  fr$ITxSR = fr$FIT * fr$FSR # Interaction
  
  return( fr )
}

# Function to create an easily interpretable display of the results
display_pretty_results = function( pv, cf = NULL, alpha = .05 ) {
  
  print( 'IT = Image type (Target vs. competitor' )
  print( 'Cnd = Condition (Selective retrieval vs. baseline)' )
  print( 'I = Interaction between factors' )
  print( ' ' )
  
  # Extract numerical values
  p_values = pv[,-1]
  
  # Intitialize output
  output = pv
  
  for ( c in 1:ncol( p_values ) ) {
    for ( r in 1:nrow( p_values ) ) {
      if ( p_values[r,c] < alpha ) {
        output[r,c+1] = paste( round( p_values[r,c], 3 ), '*', sep = '' )
      } else {
        output[r,c+1] = paste( round( p_values[r,c], 3 ), ' ', sep = '' )
      }
    }
  }
  
  print( 'p-values' )
  print( output )
  
  if ( !is.null( cf ) ) {
    
    # Extract numerical values
    est = cf[,-1]
    
    # Intitialize output
    output = cf
    
    for ( c in 1:ncol( est ) ) {
      for ( r in 1:nrow( est ) ) {
        if ( p_values[r,c] < alpha ) {
          output[r,c+1] = paste( round( est[r,c], 2 ), '*', sep = '' )
        } else {
          output[r,c+1] = paste( round( est[r,c], 2 ), ' ', sep = '' )
        }
      }
    }
    
    print( 'Coefficients' )
    print( 'IT:  + = Better performance for targets' )
    print( 'Cnd: + = Better performance for selective retrieval' )
    print( 'I:   + = Better performance for T-SR and C-B' )
    print( output )
   
    #  1  1  1 = T-SR
    #  1 -1 -1 = T-B
    # -1  1 -1 = C-SR
    # -1 -1  1 = C-B
     
  }
  
  print( ' ' )
  string = paste( 'Significant when p <', alpha )
  print( string )
}

###
### Extract data for 3rd replication
###
# Lookup - 02

fr = extract_current_data( allData )

# Create table to store p-value results
table_of_results_r3 = data.frame( 
  Test = c( 'AOV', 'LR w/ S', 'LR w/ S & I', 'MPM' )
)

# Add additional columns
table_of_results_r3$IT_MaE = NA
table_of_results_r3$Cnd_MaE = NA
table_of_results_r3$I_MaE = NA
table_of_results_r3$IT_MR = NA
table_of_results_r3$Cnd_MR = NA
table_of_results_r3$I_MR = NA

# Create table to store p-value results
table_of_coef_r3 = data.frame( 
  Test = c( 'AOV', 'LR w/ S', 'LR w/ S & I', 'MPM' )
)

# Add additional columns
table_of_coef_r3$IT_MaE = NA
table_of_coef_r3$Cnd_MaE = NA
table_of_coef_r3$I_MaE = NA
table_of_coef_r3$IT_MR = NA
table_of_coef_r3$Cnd_MR = NA
table_of_coef_r3$I_MR = NA

# Create table to store coefficients for 

incMaE = 1
incMR = 1

###
### ANOVA
### (Missing as error )
###
# Lookup - 03

prp = aggregate( fr$Acm, list( fr$ITL, fr$SRL, fr$S ), mean )
colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
out = my_RM_ANOVA( prp, display = F )

# Store results
inc = incMaE
message( inc )
table_of_results_r3[inc,1:3+1] = out$p
incMaE = incMaE + 1

###
### ANOVA
### (Missing removed )
###
# Lookup - 04

sel = fr$RT < 3.5
prp = aggregate( fr$Ac[sel], list( fr$ITL[sel], fr$SRL[sel], fr$S[sel] ), 
                 function(x) mean(x,na.rm=T) ) 
colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
out = my_RM_ANOVA( prp, display = F )

# Store results
inc = incMR
message( inc )
message( inc )
table_of_results_r3[inc,1:3+4] = out$p
incMR = incMR + 1

###
### Logistic regression with random effect of subjects
### (Missing as error)
###
# Lookup - 05

dtbf = data.frame( Y = fr$Acm, IT = fr$FIT, SR = fr$FSR, ITxSR = fr$ITxSR, 
                   S = as.factor( fr$S ), I = as.factor( fr$IN ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+1] = tmp$coefficients[-1,4]
table_of_coef_r3[inc,1:3+1] = tmp$coefficients[-1,1]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects
### (Missing responses removed )
###
# Lookup - 06

sel = fr$RT < 3.5
dtbf = data.frame( Y = fr$Acm[sel], IT = fr$FIT[sel], SR = fr$FSR[sel], ITxSR = fr$ITxSR[sel], 
                   S = as.factor( fr$S[sel] ), I = as.factor( fr$IN[sel] ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+4] = tmp$coefficients[-1,4]
table_of_coef_r3[inc,1:3+4] = tmp$coefficients[-1,1]
incMR = incMR + 1

###
### Logistic regression with random effect of subjects & items
### (Missing as error)
###
# Lookup - 07

dtbf = data.frame( Y = fr$Acm, IT = fr$FIT, SR = fr$FSR, ITxSR = fr$ITxSR, 
                   S = as.factor( fr$S ), I = as.factor( fr$IN ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+1] = tmp$coefficients[-1,4]
table_of_coef_r3[inc,1:3+1] = tmp$coefficients[-1,1]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects & items
### (Missing responses removed )
###
# Lookup - 08

sel = fr$RT < 3.5
dtbf = data.frame( Y = fr$Acm[sel], IT = fr$FIT[sel], SR = fr$FSR[sel], ITxSR = fr$ITxSR[sel], 
                   S = as.factor( fr$S[sel] ), I = as.factor( fr$IN[sel] ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+4] = tmp$coefficients[-1,4]
table_of_coef_r3[inc,1:3+4] = tmp$coefficients[-1,1]
incMR = incMR + 1

###
### MPM results
###
# Lookup - 09

# Load in results from MPM analyses
setwd( 'Data' )
load('MPM_rep3_results.RData')
setwd( orig_dir )

# Store results (Missing as error)
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+1] = mpm_rep3[[1]]$pval
table_of_coef_r3[inc,1:3+1] = mpm_rep3[[1]]$value
incMaE = incMaE + 1

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r3[inc,1:3+4] = mpm_rep3[[2]]$pval
table_of_coef_r3[inc,1:3+4] = mpm_rep3[[2]]$value
incMR = incMR + 1

###
### Extract data for 2nd replication
###
# Lookup - 10

fr = extract_current_data( prevRepData, type = 2 )

# Create table to store p-value results
table_of_results_r2 = data.frame( 
  Test = c( 'AOV', 'LR w/ S', 'LR w/ S & I', 'MPM' )
)

# Add additional columns
table_of_results_r2$IT_MaE = NA
table_of_results_r2$Cnd_MaE = NA
table_of_results_r2$I_MaE = NA
table_of_results_r2$IT_MR = NA
table_of_results_r2$Cnd_MR = NA
table_of_results_r2$I_MR = NA

# Create table to store p-value results
table_of_coef_r2 = data.frame( 
  Test = c( 'AOV', 'LR w/ S', 'LR w/ S & I', 'MPM' )
)

# Add additional columns
table_of_coef_r2$IT_MaE = NA
table_of_coef_r2$Cnd_MaE = NA
table_of_coef_r2$I_MaE = NA
table_of_coef_r2$IT_MR = NA
table_of_coef_r2$Cnd_MR = NA
table_of_coef_r2$I_MR = NA

# Create table to store coefficients for 

incMaE = 1
incMR = 1

###
### ANOVA
### (Missing as error )
###
# Lookup - 11

prp = aggregate( fr$Acm, list( fr$ITL, fr$SRL, fr$S ), mean )
colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
out = my_RM_ANOVA( prp, display = F )

# Store results
inc = incMaE
message( inc )
table_of_results_r2[inc,1:3+1] = out$p
incMaE = incMaE + 1

###
### ANOVA
### (Missing and overly fast responses removed )
###
# Lookup - 12

sel = fr$RT > .2 & fr$RT < 3.5
prp = aggregate( fr$Ac[sel], list( fr$ITL[sel], fr$SRL[sel], fr$S[sel] ), 
                 function(x) mean(x,na.rm=T) ) 
colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
out = my_RM_ANOVA( prp, display = F )

# Store results
inc = incMR
message( inc )
message( inc )
table_of_results_r2[inc,1:3+4] = out$p
incMR = incMR + 1

###
### Logistic regression with random effect of subjects
### (Missing as error)
###
# Lookup - 13

dtbf = data.frame( Y = fr$Acm, IT = fr$FIT, SR = fr$FSR, ITxSR = fr$ITxSR, 
                   S = as.factor( fr$S ), I = as.factor( fr$IN ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+1] = tmp$coefficients[-1,4]
table_of_coef_r2[inc,1:3+1] = tmp$coefficients[-1,1]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects
### (Missing responses removed )
###
# Lookup - 14

sel = fr$RT < 3.5
dtbf = data.frame( Y = fr$Acm[sel], IT = fr$FIT[sel], SR = fr$FSR[sel], ITxSR = fr$ITxSR[sel], 
                   S = as.factor( fr$S[sel] ), I = as.factor( fr$IN[sel] ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+4] = tmp$coefficients[-1,4]
table_of_coef_r2[inc,1:3+4] = tmp$coefficients[-1,1]
incMR = incMR + 1

###
### Logistic regression with random effect of subjects & items
### (Missing as error)
###
# Lookup - 15

dtbf = data.frame( Y = fr$Acm, IT = fr$FIT, SR = fr$FSR, ITxSR = fr$ITxSR, 
                   S = as.factor( fr$S ), I = as.factor( fr$IN ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+1] = tmp$coefficients[-1,4]
table_of_coef_r2[inc,1:3+1] = tmp$coefficients[-1,1]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects & items
### (Missing responses removed )
###
# Lookup - 16

sel = fr$RT < 3.5
dtbf = data.frame( Y = fr$Acm[sel], IT = fr$FIT[sel], SR = fr$FSR[sel], ITxSR = fr$ITxSR[sel], 
                   S = as.factor( fr$S[sel] ), I = as.factor( fr$IN[sel] ) )

tst = glmer( Y ~ IT + SR + ITxSR + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+4] = tmp$coefficients[-1,4]
table_of_coef_r2[inc,1:3+4] = tmp$coefficients[-1,1]
incMR = incMR + 1

###
### MPM results
###
# Lookup - 17

# Load in results from MPM analyses
setwd( 'Data' )
load('MPM_rep2_results.RData')
setwd( orig_dir )

# Store results (Missing as error)
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+1] = mpm_rep2[[1]]$pval
table_of_coef_r2[inc,1:3+1] = mpm_rep2[[1]]$value
incMaE = incMaE + 1

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results_r2[inc,1:3+4] = mpm_rep2[[2]]$pval
table_of_coef_r2[inc,1:3+4] = mpm_rep2[[2]]$value
incMR = incMR + 1

setwd( orig_dir )