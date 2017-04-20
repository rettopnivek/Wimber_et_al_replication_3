#------------------------#
# Supplementary figure 3 #
# Kevin Potter           #
# Updated 04/19/2017     #
#------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if model uncertainty should be estimated
modelFit = F

# Indicate if a pdf should be generated
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Selective-retrieval stage (Low intrusions)
# Lookup - 03:  Final recognition memory (Low intrusions)
# Lookup - 04:  Selective-retrieval stage (High visualization)
# Lookup - 05:  Final recognition memory (High visualization)
# Lookup - 06:  Create figure

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# Define some useful functions
source('F0_Useful_functions.R')

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )
# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Selective-retrieval stage (Low intrusions)
###
# Lookup - 02

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

SR = Extract_SR( cD )

# Determine group means
obs_li = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_li ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

if ( modelFit ) {
  
  # Define priors based on previous results of Wimber et al. (2015)
  Priors = cbind( c( 1.700, -.409, -1.700 ),
                  c( .7, .7, .7 ),
                  c( 2, 2, 2 ),
                  c( 8, 8, 8 ) )
  
  # Define a list of input for Stan
  stan_dat = list(
    No = length( SR$Choice ),
    Ns = N,
    K = 4,
    Y = SR$Choice,
    indS = SR$Subject,
    Priors = Priors
  )
  
  warm = 750 # Iterations of warm-up
  niter = 1250*3 # Number of samples to approximate posterior per chain
  chains = 8 # Number of chains to run in parallel
  
  startTime = Sys.time() # To assess run-time
  
  # Compile the model
  sm = stan_model(stanc_ret = stanc_builder(
    "Stan_scripts/SR_categories.stan"))
  
  # Draw samples
  fit = sampling( sm, data = stan_dat, 
                  warmup = warm,
                  iter = warm + niter, 
                  chains = chains,
                  thin = 3, 
                  seed = 777, # For reproducibility
                  control = list(
                    adapt_delta = .95 )
  )
  
  post = extract(fit)
  # Report run time
  runTime = Sys.time() - startTime
  print( runTime )
  
  # Carry out posterior retrodictive checks
  # Define a list of input for Stan
  stan_dat_prc = list(
    No = length( SR$Choice ),
    Ns = N,
    K = 4,
    Ni = nrow( post$mu_beta ),
    indS = SR$Subject,
    mu_beta = post$mu_beta,
    sigma_beta = post$sigma_beta,
    beta_raw = post$beta_raw
  )
  
  # Compile the model
  sm = stan_model(stanc_ret = stanc_builder(
    "Stan_scripts/SR_categories_sim.stan"))
  
  # Draw samples
  check = sampling( sm, data = stan_dat_prc, 
                    warmup = 0,
                    iter = 1, 
                    chains = 1,
                    seed = 778, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_li = stan_dat
  fit_li = fit
  post_li = post
  prc_li = prc
  
  setwd( 'Replication results' )
  save( stan_dat_li, fit_li, post_li, prc_li, 
        file = 'SR_LI_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Replication results' )
  load( 'SR_LI_post.RData' )
  setwd( orig_dir )
}

###
### Final recognition memory (Low intrusions)
###
# Lookup - 03

# Extract data for final recognition test
sel = allData$Cond == 6 & allData$Subject %in% subgroup
cD = allData[ sel, ]
cD$Subject = createIncrement( cD$Subject )

# Create a data frame for data to be fitted
d = cD
# Dependent variable
d$Y = d$Accuracy
# Image type (1 = target, 2 = competitor)
d$IT = as.factor( d$ImageType )
# Selective retrieval ( 1 = yes, 0 = no )
d$SR = as.factor( 1 - d$Baseline )
d$S = as.factor( d$Subject )
d$I = as.factor( d$ImageNum )
# Dummy coded variable for RIF
d$TSR = 0; d$TSR[ d$IT == 1 & d$SR == 1 ] = 1
d$TB = 0; d$TB[ d$IT == 1 & d$SR == 0 ] = 1
d$CSR = 0; d$CSR[ d$IT == 2 & d$SR == 1 ] = 1
d$CB = 0; d$CB[ d$IT == 2 & d$SR == 0 ] = 1

# Define priors
prior_vals = normal( 
  c( 1.540, 1.629, 1.317, 1.780 ), # Means
  rep( .3, 4 ) # Standard deviations
)

if ( modelFit ) {
  
  # Fit the model to the replication data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + 
                      (1|S) + (1|I), 
                    data = d, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 309, 
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Posterior retrodictive checks
  nd = d[,c('Y','Trial','S','I','IT','SR',
            'TSR','TB','CSR','CB')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  # Calculate the observed accuracy
  frm = aggregate( d$Y, list( d$IT, d$SR ), mean )
  colnames( frm ) = c( 'IT', 'SR', 'P' )
  
  est_frm_li = list(
    post = post,
    prc = prc,
    frm = frm )
  
  setwd( 'Replication results' )
  save( est_frm_li, 
        file = 'RM_LI_results.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Replication results' )
  load( file = 'RM_LI_results.RData' )
  setwd( orig_dir )
}

###
### Selective-retrieval stage (High visualization)
###
# Lookup - 04

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

SR = Extract_SR( cD )

# Determine group means
obs_hv = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_hv ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

if ( modelFit ) {
  
  # Define priors based on previous results of Wimber et al. (2015)
  Priors = cbind( c( 1.700, -.409, -1.700 ),
                  c( .7, .7, .7 ),
                  c( 2, 2, 2 ),
                  c( 8, 8, 8 ) )
  
  # Define a list of input for Stan
  stan_dat = list(
    No = length( SR$Choice ),
    Ns = N,
    K = 4,
    Y = SR$Choice,
    indS = SR$Subject,
    Priors = Priors
  )
  
  warm = 750 # Iterations of warm-up
  niter = 1250*3 # Number of samples to approximate posterior per chain
  chains = 8 # Number of chains to run in parallel
  
  startTime = Sys.time() # To assess run-time
  
  # Compile the model
  sm = stan_model(stanc_ret = stanc_builder(
    "Stan_scripts/SR_categories.stan"))
  
  # Draw samples
  fit = sampling( sm, data = stan_dat, 
                  warmup = warm,
                  iter = warm + niter, 
                  chains = chains,
                  thin = 3, 
                  seed = 1001, # For reproducibility
                  control = list(
                    adapt_delta = .95 )
  )
  
  post = extract(fit)
  # Report run time
  runTime = Sys.time() - startTime
  print( runTime )
  
  # Carry out posterior retrodictive checks
  # Define a list of input for Stan
  stan_dat_prc = list(
    No = length( SR$Choice ),
    Ns = N,
    K = 4,
    Ni = nrow( post$mu_beta ),
    indS = SR$Subject,
    mu_beta = post$mu_beta,
    sigma_beta = post$sigma_beta,
    beta_raw = post$beta_raw
  )
  
  # Compile the model
  sm = stan_model(stanc_ret = stanc_builder(
    "Stan_scripts/SR_categories_sim.stan"))
  
  # Draw samples
  check = sampling( sm, data = stan_dat_prc, 
                    warmup = 0,
                    iter = 1, 
                    chains = 1,
                    seed = 800, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_hv = stan_dat
  fit_hv = fit
  post_hv = post
  prc_hv = prc
  
  setwd( 'Replication results' )
  save( stan_dat_hv, fit_hv, post_hv, prc_hv, 
        file = 'SR_HV_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Replication results' )
  load( 'SR_HV_post.RData' )
  setwd( orig_dir )
}

###
### Final recognition memory (High visualization)
###
# Lookup - 05


# Extract data for final recognition test
sel = allData$Cond == 6 & allData$Subject %in% subgroup
cD = allData[ sel, ]
cD$Subject = createIncrement( cD$Subject )

# Create a data frame for data to be fitted
d = cD
# Dependent variable
d$Y = d$Accuracy
# Image type (1 = target, 2 = competitor)
d$IT = as.factor( d$ImageType )
# Selective retrieval ( 1 = yes, 0 = no )
d$SR = as.factor( 1 - d$Baseline )
d$S = as.factor( d$Subject )
d$I = as.factor( d$ImageNum )
# Dummy coded variable for RIF
d$TSR = 0; d$TSR[ d$IT == 1 & d$SR == 1 ] = 1
d$TB = 0; d$TB[ d$IT == 1 & d$SR == 0 ] = 1
d$CSR = 0; d$CSR[ d$IT == 2 & d$SR == 1 ] = 1
d$CB = 0; d$CB[ d$IT == 2 & d$SR == 0 ] = 1

# Define priors
prior_vals = normal( 
  c( 1.540, 1.629, 1.317, 1.780 ), # Means
  rep( .3, 4 ) # Standard deviations
)

if ( modelFit ) {
  
  # Fit the model to the replication data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + 
                      (1|S) + (1|I), 
                    data = d, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 309,
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Posterior retrodictive checks
  nd = d[,c('Y','Trial','S','I','IT','SR',
            'TSR','TB','CSR','CB')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  # Calculate the observed accuracy
  frm = aggregate( d$Y, list( d$IT, d$SR ), mean )
  colnames( frm ) = c( 'IT', 'SR', 'P' )
  
  est_frm_hv = list(
    post = post,
    prc = prc,
    frm = frm )
  
  setwd( 'Replication results' )
  save( est_frm_hv, 
        file = 'RM_HV_results.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Replication results' )
  load( file = 'RM_HV_results.RData' )
  setwd( orig_dir )
}

###
### Create figure 
###
# Lookup - 06

ptSz = 1.5
lnSz = 2
uiWd = .05
txtSz = 1.5

# Extract original data for selective retrieval stage
sel = OriginalAllData$Cond == 5
cD = OriginalAllData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_od = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_od ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

# Extract replication data for selective retrieval stage
sel = allData$Cond == 5
cD = allData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_rd = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_rd ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

# Load in reanalysis data
setwd( 'Reanalysis results' )
load( file = 'SR_post.RData' )
setwd( orig_dir )

# Load in replication data
setwd( 'Replication results' )
load( file = 'SR_post.RData' )
setwd( orig_dir )

if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'Supplementary_figure_3.pdf' ) else 
    pdf( 'Supplementary_figure_3_no_label.pdf' )
  setwd( orig_dir )
} else x11()

lyt = matrix( 5, 6, 6 )
lyt[ 1:3, 1:3 ] = 1
lyt[ 1:3, 4:6 ] = 2
lyt[ 4:6, 1:3 ] = 3
lyt[ 4:6, 4:6 ] = 4
layout( lyt )

### Selective retrieval stage

par( mar = c( 3, 5, 1, 1 ) )
# Create a blank plot
blankPlot( c(.5,4.5), c(0,1) )
abline( h = 0, lwd = lnSz )
abline( v = .5, lwd = lnSz )
# segments( rep( .5, 4 ), seq( .25, 1, .25 ), 
#           rep (.45, 4 ), seq( .25, 1, .25 ), lwd = lnSz )
axis( 1, 1:4, c( 'Target', 'Competitor', 'Error', "Don't know" ),
      tick = F, cex.axis = txtSz*.71, line = -.75 )
axis( 2, seq( 0, 1, .25 ), 
      seq( 0, 100, 25 ),
      tick = F, cex.axis = txtSz*.71, line = -.75 )
mtext( 'Percent chosen', side = 2, line = 2, cex = txtSz*.8 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topleft', 'a', bty = 'n', cex = 1.7 )
par( font = 1 )

xa = 1:4

shft = c( -.25, 0, .25 )
clr = c( 'red', 'blue', 'purple' )

for ( i in 1:4 ) {
  
  if ( i == 1 ) {
    sm = prc_od$P[1,,]
    x = obs_od
  }
  if ( i == 2 ) {
    sm = prc_li$P[1,,]
    x = obs_li
  }
  if ( i == 3 ) {
    sm = prc_hv$P[1,,]
    x = obs_hv
  }
  
  # Plot uncertainty and data points
  ui = apply( sm, 2, quantile, prob = c(.025,.975) )
  arrows( xa + shft[i], ui[1,], xa + shft[i], ui[2,],
          col = clr[i], code = 3, length = uiWd, angle = 90,
          lwd = lnSz )
  points( xa + shft[i], x, pch = 19, col = clr[i], cex = ptSz )
  
}

legend( 'topright', c( 'Original', 'Low intrusions', 
                      'High visualization' ),
        fill = clr, cex = txtSz*.9, bty = 'n' )

### Final recognition memory results

# Define plot characteristics
ptSz = 2
lnSz = 2
txtSz = 1.5
posL1 = c( .5, .75 )
posL2 = c( .5, .92 )
uiSz = .05

setwd( 'Reanalysis results' )
load( 'Figure_1_posteriors.RData' )
setwd( orig_dir )

# Determine condition means for original data
sel = OriginalAllData$Cond == 6
od = OriginalAllData[ sel, ]
od$Y = od$Accuracy
od$Y[ is.na( od$RT ) ] = 0
od$IT = od$ImageType
od$SR = 1 - od$Baseline

# Determine condition means for each set of data
old = aggregate( od$Y, list( od$IT, od$SR ), mean )
colnames( old ) = c( 'IT', 'SR', 'P' )

for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    new = est_frm_li$frm
    prc_r = est_frm_li$prc
    clr = 'blue'
  } else {
    new = est_frm_hv$frm
    prc_r = est_frm_hv$prc
    clr = 'purple'
  }
  
  # Determine x-axis positions
  xa = c( 1.1, 0.9, 1.9, 2.1 )
  ord = c( 1, 2, 3, 4 )
  
  if ( i == 1 ) {
    
    # Create a blank plot
    par( mar = c( 4, 5, .5, .5 ) )
    blankPlot( c( .5, 2.5 ), c( .7, .95 ) )
    
    abline( h = .7, col = 'black', lwd = lnSz )
    abline( v = .5, col = 'black', lwd = lnSz )
    axis( 2, c( .7, .8, .9 ),
          c( '70', '80', '90' ),
          tick = F, cex.axis = txtSz*.9, line = -.5 )
    mtext( 'Percent correct', side = 2, cex = txtSz*.8, line = 3 )
    axis( 1, 1:2, c( 'Baseline', 'Selective-retrieval' ),
          tick = F, cex.axis = txtSz )
    
    # Add label
    par( font = 2 )
    if ( panelYes ) legend( 'topleft', 'b', bty = 'n', cex = 1.7 )
    par( font = 1 )
    
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
    
  }
  
  # Create a blank plot
  par( mar = c( 4, 5, .5, .5 ) )
  blankPlot( c( .5, 2.5 ), c( .7, .95 ) )
  
  # Add label
  if ( panelYes ) {
    par( font = 2 )
    if ( i == 1 ) legend( 'topleft', 'c', bty = 'n', cex = 1.7 )
    if ( i == 2 ) legend( 'topleft', 'd', bty = 'n', cex = 1.7 )
    par( font = 1 )
  }
  
  abline( h = .7, col = 'black', lwd = lnSz )
  abline( v = .5, col = 'black', lwd = lnSz )
  axis( 1, 1:2, c( 'Baseline', 'Selective-retrieval' ),
        tick = F, cex.axis = txtSz )
  axis( 2, c( .7, .8, .9 ),
        c( '70', '80', '90' ),
        tick = F, cex.axis = txtSz*.9, line = -.5 )
  mtext( 'Percent correct', side = 2, cex = txtSz*.8, line = 3 )
  
  # Determine uncertainty intervals for replication data
  ui = apply( prc_r, 1, quantile, prob = c( .025, .975 ) )
  arrows( xa, ui[1,ord], xa, ui[2,ord], code = 3, length = uiSz,
          angle = 90, col = clr, lwd = lnSz )
  
  # Plot original data
  segments( xa[ c(1,2) ], new$P[ ord[ c(1,2)] ],
            xa[ c(3,4) ], new$P[ ord[ c(3,4)] ],
            lwd = lnSz, lty = 1:2, col = clr )
  points( xa, new$P[ord], pch = c( 21, 22, 21, 22 ),
          bg = rep( c(clr,'white'), 2 ),
          col = clr, 
          cex = ptSz )
  
  if ( i == 2 ) {
    
    legend( 'topright', c('Targets','Competitors'),
            pch = c( 21, 22 ), pt.bg = c('purple','white'),
            cex = txtSz*.9, bty = 'n', col = 'purple' )
    
  }
  
}

# blankPlot()

if (savePlot) dev.off()