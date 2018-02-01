#----------------------------------------------#
# Original analyses as applied to replications #
# Kevin Potter                                 #
# Updated 12/08/2017                           #
#----------------------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Index
# Lookup - 01:  Load in useful packages and functions
# Lookup - 02:  Replication 2
# Lookup - 03:  Replication 3
# Lookup - 04:  Replication 3 (Low intrusions)
# Lookup - 05:  Replication 3 (High visualization)
# Lookup - 06:  Check of weird p-value for Competitor:baseline test
# Lookup - 06:  Replicaiton 1

###
### Load in useful packages and functions
###
# Lookup - 01

# For geting github packages
# install.packages( 'devtools' )
# library(devtools)

# Miscellanous functions for modeling/plotting
# install_github("rettopnivek/utilityf")
library(utilityf)

# Define some useful functions
source( 'F0_Useful_functions.R' )

# Function to replicate analyses used by Wimber et al.
# for the selective retrieval stage
replicate_Wimber_et_al_analyses_SR = function(cD) {
  
  # Intialize list for outputs
  out = list( I_v_E_SR = NULL )
  
  ### Analysis of selective retrieval stage ###
  
  # Extract data
  SR = Extract_SR( cD )
  
  # Test if proportion of intrusions differs from proportion of errors
  i_m = aggregate( SR$Choice == 2, list( SR$Subject ), mean )
  colnames( i_m ) = c( 'S', 'P' )
  e_m = aggregate( SR$Choice == 3, list( SR$Subject ), mean )
  colnames( e_m ) = c( 'S', 'P' )
  out$I_v_E_SR = t.test( i_m$P - e_m$P )
  
  # Test if proportion of intrusions vary by cueRep
  i_by_c = aggregate( SR$Choice == 2, list( cD$CueRep - 2.5, SR$Subject ), mean )
  colnames( i_by_c ) = c( 'Group.1', 'Subject', 'x' )
  out$AOV_I_SR_table = my_RM_ANOVA( i_by_c, display = F, all = T )
  
  # Test linear trend
  n = length( unique( i_by_c$Subject ) )
  D = matrix( NA, n, 4 )
  for ( i in 1:4 ) {
    sel = i_by_c$Group.1 == sort( unique( i_by_c$Group.1 ) )[i]
    D[,i] = i_by_c$x[sel]
  }
  colnames( D ) = c( 'c-3','c-1','c1','c3' )
  cont = D %*% contrasts( as.factor( i_by_c$Group.1 ) )
  mod = lm( cont ~ 1 )
  tst = anova( mod, test = 'Spherical' )
  out$AOV_I_LT_SR_table = list(
    df = tst$Df,
    F = tst$F[1],
    p = pf( tst$F[1], tst$Df[1], tst$Df[2], lower.tail = F )
  )
  
  return( out )
}

# Function to replicate analyses used by Wimber et al. 
# for the final recognition memory task
replicate_Wimber_et_al_analyses_FR = function(cD) {
  
  # Helpful labels
  cD$ITL = 'Target';
  cD$ITL[ cD$ImageType == 2 ] = 'Competitor'
  cD$CL = 'Selective retrieval'
  cD$CL[ cD$Baseline == 1 ] = 'Baseline'
  
  out = list()
  
  # Omnibus ANOVA
  tot = aggregate( rep(1,nrow(cD)), list( cD$ITL, cD$CL, cD$Subject ), sum ) 
  prp = aggregate( cD$Accuracy, list( cD$ITL, cD$CL, cD$Subject ), sum ) 
  colnames( prp ) = c( 'Group.1', 'Group.2', 'Subject', 'x' )
  prp$x = prp$x / tot$x
  out$AOV_FR_table = my_RM_ANOVA( prp, display = F, all = T )
  
  sel1 = prp$Group.1 == 'Competitor' & prp$Group.2 == 'Selective retrieval'
  sel2 = prp$Group.1 == 'Competitor' & prp$Group.2 == 'Baseline'
  out$TT_CSR_v_CB = t.test( prp$x[sel2] - prp$x[sel1] )
  
  sel1 = prp$Group.1 == 'Target' & prp$Group.2 == 'Selective retrieval'
  sel2 = prp$Group.1 == 'Target' & prp$Group.2 == 'Baseline'
  out$TT_TSR_v_TB = t.test( prp$x[sel2] - prp$x[sel1] )
  
  return( out )
}

# Function to report results in tidy form 
pretty_results = function( lst, type ) {
  
  if ( type == 'SR' ) {
    
    string = 'Difference between P(Intrusions) & P(Error)'
    message( string )
    pval = quick_pval( lst$I_v_E_SR$p.value )
    string = paste( 't(', lst$I_v_E_SR$parameter, ') = ',
                    round( lst$I_v_E_SR$statistic, 3 ),
                    ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Intrusions over four repetitions'
    message( string )
    pval = quick_pval( lst$AOV_I_SR_table$p[1] )
    string = paste(
      'F(', lst$AOV_I_SR_table$df[1], ',', 
      lst$AOV_I_SR_table$df[3], ') = ',
      round( lst$AOV_I_SR_table$F[1], 3 ),
      ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Linear trend'
    message( string )
    pval = quick_pval( lst$AOV_I_LT_SR_table$p[1] )
    string = paste(
      'F(', lst$AOV_I_LT_SR_table$df[1], ',', 
      lst$AOV_I_LT_SR_table$df[2], ') = ',
      round( lst$AOV_I_LT_SR_table$F[1], 3 ),
      ', ', pval, sep = '' )
    message( string )
    
  }
  
  if ( type == 'FR' ) {
    
    string = 'Main effect of associate'
    message( string )
    pval = quick_pval( lst$AOV_FR_table$p['A'] )
    string = paste(
      'F(', lst$AOV_FR_table$df['A'], ',', 
      lst$AOV_FR_table$df['AS'], ') = ',
      round( lst$AOV_FR_table$F['A'], 3 ),
      ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Main effect of condition'
    message( string )
    pval = quick_pval( lst$AOV_FR_table$p['B'] )
    string = paste(
      'F(', lst$AOV_FR_table$df['B'], ',', 
      lst$AOV_FR_table$df['BS'], ') = ',
      round( lst$AOV_FR_table$F['B'], 3 ),
      ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Interaction'
    message( string )
    pval = quick_pval( lst$AOV_FR_table$p['AB'] )
    string = paste(
      'F(', lst$AOV_FR_table$df['AB'], ',', 
      lst$AOV_FR_table$df['ABS'], ') = ',
      round( lst$AOV_FR_table$F['AB'], 3 ),
      ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Comparison of Competitor:Baseline'
    message( string )
    pval = quick_pval( lst$TT_CSR_v_CB$p.value )
    string = paste( 't(', lst$TT_CSR_v_CB$parameter, ') = ',
                    round( lst$TT_CSR_v_CB$statistic, 3 ),
                    ', ', pval, sep = '' )
    message( string )
    
    message( ' ' )
    
    string = 'Comparison of Target:Baseline'
    message( string )
    pval = quick_pval( lst$TT_TSR_v_TB$p.value )
    string = paste( 't(', lst$TT_TSR_v_TB$parameter, ') = ',
                    round( lst$TT_TSR_v_TB$statistic, 3 ),
                    ', ', pval, sep = '' )
    message( string )
    
  }
  
}

###
### Replication 2
###
# Lookup - 02

# Load in previous replication data
load( 'Data/Combined_replication_data.RData' )

# Extract data for selective retrieval stage
sel = prevRepData$Cond == 5 & prevRepData$Exp == 2 & 
  prevRepData$BadInstruct == 0
cD = prevRepData[sel,]

R2_SR = replicate_Wimber_et_al_analyses_SR( cD )

# Extract data for final recognition test
sel = prevRepData$Cond == 6 & prevRepData$Exp == 2 & 
  prevRepData$BadInstruct == 0
cD = prevRepData[ sel, ]

R2_FR = replicate_Wimber_et_al_analyses_FR( cD )

message( 'Replication 2' )
pretty_results( R2_SR, 'SR' )
pretty_results( R2_FR, 'FR' )


###
### Replication 3
###
# Lookup - 03

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )

# Extract data for selective retrieval stage
sel = allData$Cond == 5
cD = allData[sel,]

R3_SR = replicate_Wimber_et_al_analyses_SR( cD )

# Extract data for final recognition test
sel = allData$Cond == 6
cD = allData[ sel, ]

R3_FR = replicate_Wimber_et_al_analyses_FR( cD )

message( 'Replication 3' )
pretty_results( R3_SR, 'SR' )
pretty_results( R3_FR, 'FR' )

###
### Replication 3 (Low intrusions)
###
# Lookup - 04

# Extract data for selective retrieval stage
sel = allData$Cond == 5
cD = allData[sel,]

SR = Extract_SR( cD )

# Determine proportion of intrusions for each subject
P_RC = aggregate( SR$Choice == 2, list( SR$Subject ), mean )
colnames( P_RC ) = c( 'S', 'P' )

ord = order( P_RC$P )
# Determine subjects with low number of intrusions
sel = 1:24
subgroup = P_RC$S[ ord ][sel]
Nc = length( subgroup )

# Extract data for selective retrieval stage
sel = allData$Cond == 5 & allData$Subject %in% subgroup
cD = allData[sel,]

R3_LI_SR = replicate_Wimber_et_al_analyses_SR( cD )

# Extract data for final recognition test
sel = allData$Cond == 6 & allData$Subject %in% subgroup
cD = allData[ sel, ]

R3_LI_FR = replicate_Wimber_et_al_analyses_FR( cD )

message( 'Replication 3 (Low intrusions)' )
pretty_results( R3_LI_SR, 'SR' )
pretty_results( R3_LI_FR, 'FR' )

###
### Replication 3 (High visualization)
###
# Lookup - 05

# Select subjects who said that they could 
# typically or almost always visualize images in 
# the selective retrieval phase
sel = !is.na( allLogs$Subject )
visRank = cbind( S = allLogs$Subject[ sel ],
                 V = allLogs$Visualization[ sel ] )
visRank = as.data.frame( visRank )
sel = visRank$V > 3
subgroup = visRank$S[ sel ]
Nc = length( subgroup )

# Extract data for selective retrieval stage
sel = allData$Cond == 5 & allData$Subject %in% subgroup
cD = allData[sel,]

R3_HV_SR = replicate_Wimber_et_al_analyses_SR( cD )

# Extract data for final recognition test
sel = allData$Cond == 6 & allData$Subject %in% subgroup
cD = allData[ sel, ]

R3_HV_FR = replicate_Wimber_et_al_analyses_FR( cD )

message( 'Replication 3 (High visualization)' )
pretty_results( R3_HV_SR, 'SR' )
pretty_results( R3_HV_FR, 'FR' )

###
### Check of weird p-value for Competitor:baseline test
###
# Lookup - 06

# Helpful labels
cD$ITL = 'Target';
cD$ITL[ cD$ImageType == 2 ] = 'Competitor'
cD$CL = 'Selective retrieval'
cD$CL[ cD$Baseline == 1 ] = 'Baseline'

# Omnibus ANOVA
tot = aggregate( rep(1,nrow(cD)), list( cD$ITL, cD$CL, cD$Subject ), sum ) 
prp = aggregate( cD$Accuracy, list( cD$ITL, cD$CL, cD$Subject ), sum ) 
colnames( prp ) = c( 'IT', 'Cnd', 'S', 'Y' )
prp$P = prp$Y / tot$x
prp$N = tot$x
rm( tot )

sel1 = prp$IT == 'Competitor' & prp$Cnd == 'Selective retrieval'
sel2 = prp$IT == 'Competitor' & prp$Cnd == 'Baseline'
check = numeric(3)
check[1] = cor( prp$P[sel1], prp$P[sel2] )
check[2] = cor( prp$P[sel1], prp$P[sel2],
                method = 'kendall' )
check[3] = cor( prp$P[sel1], prp$P[sel2],
                method = 'spearman' )
print( round( check, 2 ) ) # Correlation is high but not 1, so seems fine

###
### Replication 1
###
# Lookup - 07

# Load in previous replication data
load( 'Data/Combined_replication_data.RData' )

# Extract data for selective retrieval stage
sel = prevRepData$Cond == 5 & prevRepData$Exp == 1
cD = prevRepData[sel,]

R1_SR = replicate_Wimber_et_al_analyses_SR( cD )

# Extract data for final recognition test
sel = prevRepData$Cond == 6 & prevRepData$Exp == 1
cD = prevRepData[ sel, ]

R1_FR = replicate_Wimber_et_al_analyses_FR( cD )

message( 'Replication 1' )
pretty_results( R1_SR, 'SR' )
pretty_results( R1_FR, 'FR' )

setwd( orig_dir )