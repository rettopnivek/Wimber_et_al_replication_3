#-----------------------------------------------------#
# Robustness of simple competitor:baseline comparison #
# Kevin Potter                                        #
# Updated 12/09/2017                                  #
#-----------------------------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Extract data
# Lookup - 03:  Simple comparison of Competitor:Baseline
#               (Missing as error)
# Lookup - 04:  Simple comparison of Competitor:Baseline
#               (Missing removed)
# Lookup - 05:  Logistic regression with random effect of subjects
#               (Missing as error)
# Lookup - 06:  Logistic regression with random effect of subjects
#               (Missing removed)
# Lookup - 07:  Logistic regression with random effect of subjects & items
#               (Missing as error)
# Lookup - 08:  Logistic regression with random effect of subjects & items
#               (Missing removed)
# Lookup - 09:  Display results

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

# Extract performance for final recognition memory test
fr = od[ od$Ph == 6, ]

# Track missing data
fr$md = F
fr$md[ is.na( fr$Ac ) ] = T

# Define meaningful label for conditions
fr$SRL = 'Selective retrieval'
fr$SRL[ fr$B == 1 ] = 'Baseline'

# Define meaningful label for image types
fr$ITL = 'Target'
fr$ITL[ fr$IT == 2 ] = 'Competitor'

# Recode missing as errors
fr$Acm = fr$Ac
fr$Acm[ fr$md ] = 0

# Create table to store results
table_of_results = data.frame( 
  Test = c( 't-test', 'LR w/ S', 'LR w/ S & I', 'MPM' )
)

Value_MaE = rep( NA, nrow( table_of_results ) )
Size_MaE = rep( NA, nrow( table_of_results ) )
pval_MaE = rep( NA, nrow( table_of_results ) )
Value_MR = rep( NA, nrow( table_of_results ) )
Size_MR = rep( NA, nrow( table_of_results ) )
pval_MR = rep( NA, nrow( table_of_results ) )

incMaE = 1
incMR = 1

###
### Simple comparison of Competitor:Baseline
### (Missing as error )
###
# Lookup - 03

prp = aggregate( fr$Acm, list( fr$ITL, fr$SRL, fr$S ), mean ) 
colnames( prp ) = c( 'IT', 'Cnd', 'S', 'P' )

sel1 = prp$IT == 'Competitor' & prp$Cnd == 'Selective retrieval'
sel2 = prp$IT == 'Competitor' & prp$Cnd == 'Baseline'
# Round to two decimal places in order 
# to match Wimber et al.'s original result
prp$P = round( prp$P, 2 )
tst = t.test( prp$P[sel2] - prp$P[sel1] )

# Store results
inc = incMaE
message( inc )
table_of_results$Value_MaE[inc] = tst$statistic
table_of_results$Size_MaE[inc] = tst$estimate
table_of_results$pval_MaE[inc] = tst$p.value
incMaE = incMaE + 1

###
### Simple comparison of Competitor:Baseline
### (Missing removed )
###
# Lookup - 04

prp = aggregate( fr$Ac, list( fr$ITL, fr$SRL, fr$S ), 
                 function(x) mean(x,na.rm=T) ) 
colnames( prp ) = c( 'IT', 'Cnd', 'S', 'P' )

sel1 = prp$IT == 'Competitor' & prp$Cnd == 'Selective retrieval'
sel2 = prp$IT == 'Competitor' & prp$Cnd == 'Baseline'
tst = t.test( prp$P[sel2] - prp$P[sel1] )

# Store results
inc = incMR
message( inc )
table_of_results$Value_MR[inc] = tst$statistic
table_of_results$Size_MR[inc] = tst$estimate
table_of_results$pval_MR[inc] = tst$p.value
incMR = incMR + 1

###
### Logistic regression with random effect of subjects
### (Missing as error)
###
# Lookup - 05

dtbf = data.frame( Y = fr$Acm, S = as.factor( fr$S ), I = as.factor( fr$IN ) )

# Create planned contrasts

# Intercepts
dtbf$CI = 1; dtbf$CI[ fr$ITL == 'Target' ] = 0;
dtbf$TI = 1; dtbf$TI[ fr$ITL == 'Competitor' ] = 0;

# Contrasts
dtbf$C_SRvB = 0;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Selective retrieval' ] = -1;
dtbf$T_SRvB = 0;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Selective retrieval' ] = -1;

tst = glmer( Y ~ -1 + CI + C_SRvB + TI + T_SRvB + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results$Value_MaE[inc] = tmp$coefficients[2,1]
table_of_results$Size_MaE[inc] = 
  logistic( tmp$coefficients[1,1] + tmp$coefficients[2,1] ) - 
  logistic( tmp$coefficients[1,1] - tmp$coefficients[2,1] )
table_of_results$pval_MaE[inc] = tmp$coefficients[2,4]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects
### (Missing removed)
###
# Lookup - 06

dtbf = data.frame( Y = fr$Ac, S = as.factor( fr$S ), I = as.factor( fr$IN ) )

# Create planned contrasts

# Intercepts
dtbf$CI = 1; dtbf$CI[ fr$ITL == 'Target' ] = 0;
dtbf$TI = 1; dtbf$TI[ fr$ITL == 'Competitor' ] = 0;

# Contrasts
dtbf$C_SRvB = 0;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Selective retrieval' ] = -1;
dtbf$T_SRvB = 0;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Selective retrieval' ] = -1;

# Remove missing data
dtbf = dtbf[ !fr$md == 1, ]

tst = glmer( Y ~ -1 + CI + C_SRvB + TI + T_SRvB + 
               (1|S),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results$Value_MR[inc] = tmp$coefficients[2,1]
table_of_results$Size_MR[inc] = 
  logistic( tmp$coefficients[1,1] + tmp$coefficients[2,1] ) - 
  logistic( tmp$coefficients[1,1] - tmp$coefficients[2,1] )
table_of_results$pval_MR[inc] = tmp$coefficients[2,4]
incMR = incMR + 1


###
### Logistic regression with random effect of subjects & items
### (Missing as error)
###
# Lookup - 07

dtbf = data.frame( Y = fr$Acm, S = as.factor( fr$S ), I = as.factor( fr$IN ) )

# Create planned contrasts

# Intercepts
dtbf$CI = 1; dtbf$CI[ fr$ITL == 'Target' ] = 0;
dtbf$TI = 1; dtbf$TI[ fr$ITL == 'Competitor' ] = 0;

# Contrasts
dtbf$C_SRvB = 0;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Selective retrieval' ] = -1;
dtbf$T_SRvB = 0;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Selective retrieval' ] = -1;

tst = glmer( Y ~ -1 + CI + C_SRvB + TI + T_SRvB + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMaE
message( inc )
tmp = summary( tst )
table_of_results$Value_MaE[inc] = tmp$coefficients[2,1]
table_of_results$Size_MaE[inc] = 
  logistic( tmp$coefficients[1,1] + tmp$coefficients[2,1] ) - 
  logistic( tmp$coefficients[1,1] - tmp$coefficients[2,1] )
table_of_results$pval_MaE[inc] = tmp$coefficients[2,4]
incMaE = incMaE + 1

###
### Logistic regression with random effect of subjects & items
### (Missing removed)
###
# Lookup - 08

dtbf = data.frame( Y = fr$Ac, S = as.factor( fr$S ), I = as.factor( fr$IN ) )

# Create planned contrasts

# Intercepts
dtbf$CI = 1; dtbf$CI[ fr$ITL == 'Target' ] = 0;
dtbf$TI = 1; dtbf$TI[ fr$ITL == 'Competitor' ] = 0;

# Contrasts
dtbf$C_SRvB = 0;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$C_SRvB[ dtbf$CI == 1 & fr$SRL == 'Selective retrieval' ] = -1;
dtbf$T_SRvB = 0;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Baseline' ] = 1;
dtbf$T_SRvB[ dtbf$TI == 1 & fr$SRL == 'Selective retrieval' ] = -1;

# Remove missing data
dtbf = dtbf[ !fr$md == 1, ]

tst = glmer( Y ~ -1 + CI + C_SRvB + TI + T_SRvB + 
               (1|S) + (1|I),
             family = binomial('logit'), data = dtbf )

# Store results
inc = incMR
message( inc )
tmp = summary( tst )
table_of_results$Value_MR[inc] = tmp$coefficients[2,1]
table_of_results$Size_MR[inc] = 
  logistic( tmp$coefficients[1,1] + tmp$coefficients[2,1] ) - 
  logistic( tmp$coefficients[1,1] - tmp$coefficients[2,1] )
table_of_results$pval_MR[inc] = tmp$coefficients[2,4]
incMR = incMR + 1

###
### Load in results for multinomial processing model
###
# Lookup - 10

setwd( orig_dir )
setwd( 'Data' )
load( 'MPM_robust_results.RData' )
setwd( orig_dir )

# Store results
inc = incMaE
message( inc )
table_of_results$Value_MaE[inc] = mpm_robust$MaE$value
table_of_results$Size_MaE[inc] = mpm_robust$MaE$estimate
table_of_results$pval_MaE[inc] = mpm_robust$MaE$pval
incMaE = incMaE + 1

# Store results
inc = incMR
message( inc )
table_of_results$Value_MR[inc] = mpm_robust$MR$value
table_of_results$Size_MR[inc] = mpm_robust$MR$estimate
table_of_results$pval_MR[inc] = mpm_robust$MR$pval
incMR = incMR + 1

###
### Display results
###
# Lookup - 09

pretty_display = function( table_of_results ) {
  
  R = nrow( table_of_results )
  
  for ( r in 1:R ) {
    message( table_of_results$Test[r] )
    message( 'Missing as error:' )
    pval = quick_pval( table_of_results$pval_MaE[r] )
    string = paste( 
      'Value = ', 
      round( table_of_results$Value_MaE[r], 3 ),
      ', Estimate = ',
      round( table_of_results$Size_MaE[r], 2 ),
      ', ', pval, sep = ''
      
    )
    message( string )
    message( 'Missing removed:' )
    pval = quick_pval( table_of_results$pval_MR[r] )
    string = paste( 
      'Value = ', 
      round( table_of_results$Value_MR[r], 3 ),
      ', Estimate = ',
      round( table_of_results$Size_MR[r], 2 ),
      ', ', pval, sep = ''
    )
    message( string )
    message( ' ' )
  }
  
}

pretty_display( table_of_results )

setwd( orig_dir )