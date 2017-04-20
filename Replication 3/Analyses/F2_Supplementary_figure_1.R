#------------------------#
# Supplementary figure 1 #
# Kevin Potter           #
# Updated 04/19/2017     #
#------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if model uncertainty should be estimated
modelFit = F

# Indicate if a pdf should be saved
savePlot = T

# Indicate whether panel labels should be included
panelYes = T

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Selective-retrieval stage (Replication 1)
# Lookup - 03:  Trend analysis (Replication 1)
# Lookup - 04:  Final recognition test (Replication 1)
# Lookup - 05:  Selective-retrieval stage (Replication 2)
# Lookup - 06:  Trend analysis (Replication 2)
# Lookup - 07:  Final recognition test (Replication 2)
# Lookup - 08:  Create figure

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# Define some useful functions
source('F0_Useful_functions.R')

# Load in previous replication data
load( 'Data/Combined_replication_data.RData' )
# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Selective-retrieval stage (Replication 1)
###
# Lookup - 02

# Extract data for selective retrieval stage
sel = prevRepData$Cond == 5 & prevRepData$Exp == 1
cD = prevRepData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_r1 = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_r1 ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

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
                  seed = 6001, # For reproducibility
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
                    seed = 1400, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_r1 = stan_dat
  fit_r1 = fit
  post_r1 = post
  prc_r1 = prc
  
  setwd( 'Previous replication results' )
  save( stan_dat_r1, fit_r1, post_r1, prc_r1, 
        file = 'SR_R1_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Previous replication results' )
  load( 'SR_R1_post.RData' )
  setwd( orig_dir )
}

###
### Trend analysis (Replication 1)
###
# Lookup - 03

# Determine the frequencies for a given response type
Total = aggregate( rep(1,nrow(cD)), list(
  cD$CueRep - 2.5, cD$Subject ), sum )
Targets_rep = aggregate( SR$Choice == 1, list(
  cD$CueRep - 2.5, cD$Subject ), sum )
Competitors_rep = aggregate( SR$Choice == 2, list(
  cD$CueRep - 2.5, cD$Subject ), sum )

if ( modelFit ) {
  
  # Create data frame
  d = Targets_rep
  colnames( d ) = c('R','S','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model, collapsing over responses to give 
  # targets vs. all others
  fit = stan_glmer( cbind( Y, N - Y ) ~ R + (1|S), 
                    data = d, family = binomial("logit"), 
                    prior_intercept = normal(1.39,.3), 
                    prior = normal( .3, .3 ), 
                    chains = 8, cores = 8, seed = 7700,
                    iter = 1250,
                    warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/18, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_tar_R1 = list( post = post, prc = prc, ui = ui,
                    obs = obs )
  
  # Create data frame
  d = Competitors_rep
  colnames( d ) = c('R','S','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model, collapsing over responses to give 
  # targets vs. all others
  fit = stan_glmer(cbind(Y, N - Y) ~ R + (1|S), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(-1.76,.3), 
                   prior = normal( -.3, .3 ), 
                   chains = 8, cores = 8, seed = 3300,
                   iter = 1250,
                   warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/18, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_int_R1 = list( post = post, prc = prc, ui = ui,
                    obs = obs )
  
  setwd( 'Previous replication results' )
  save( trend_tar_R1, trend_int_R1, 
        file = 'SR_R1_trend_analysis.RData' )
  setwd( orig_dir )
  
} else {
  
  setwd( 'Previous replication results' )
  load( 'SR_R1_trend_analysis.RData' )
  setwd( orig_dir )
  
}

###
### Final recognition test (Replication 1)
###
# Lookup - 04

# Extract data for final recognition test
sel = prevRepData$Cond == 6 & prevRepData$Exp == 1
cD = prevRepData[ sel, ]

### Define useful covariates ###

# Dependent variable
cD$Y = cD$Accuracy;

# Define specific intercepts for each condition

# Targets (Selective-retrieval)
cD$TSR = 0; cD$TSR[ cD$ImageType == 1 & cD$Baseline == 0 ] = 1
# Targets (Baseline)
cD$TB = 0; cD$TB[ cD$ImageType == 1 & cD$Baseline == 1 ] = 1
# Competitors (Selective-retrieval)
cD$CSR = 0; cD$CSR[ cD$ImageType == 2 & cD$Baseline == 0 ] = 1
# Competitors (Baseline)
cD$CB = 0; cD$CB[ cD$ImageType == 2 & cD$Baseline == 1 ] = 1

# Define covariates for condition, subjects, and images

# Image type (1 = target, 2 = competitor)
cD$IT = cD$ImageType
# Selective retrieval ( 1 = yes, 0 = no )
cD$SR = 1 - cD$Baseline;
cD$S = as.factor( cD$Subject )
cD$I = as.factor( cD$ImageNum )

# Define priors
prior_vals = normal( 
  c( 1.775, 1.775, 1.475, 1.775 ), # Means
  rep( .3, 4 ) # Standard deviations
)

if ( modelFit ) {
  
  # Fit the model to the original data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + # Fixed effects
                      (1|S) + (1|I), # Random effects
                    data = cD, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 4545,
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Generate posterior simulations
  nd = cD[,c('Y','TSR','TB','CSR','CB','S','I','IT','SR')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  est_frm_R1 = list( fit = fit, 
                     post = post, 
                     nd = nd, 
                     sim = sim, 
                     prc = prc )
  
  setwd( 'Previous replication results' )
  save( est_frm_R1, file = 'FRM_R1_post.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Previous replication results' )
  load( 'FRM_R1_post.RData' )
  setwd( orig_dir )
}

###
### Selective-retrieval stage (Replication 2)
###
# Lookup - 04

# Extract data for selective retrieval stage
sel = prevRepData$Cond == 5 & prevRepData$Exp == 2
cD = prevRepData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_r2 = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_r2 ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

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
                  seed = 6002, # For reproducibility
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
                    seed = 1401, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_r2 = stan_dat
  fit_r2 = fit
  post_r2 = post
  prc_r2 = prc
  
  setwd( 'Previous replication results' )
  save( stan_dat_r2, fit_r2, post_r2, prc_r2, 
        file = 'SR_R2_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Previous replication results' )
  load( 'SR_R2_post.RData' )
  setwd( orig_dir )
}

###
### Trend analysis (Replication 2)
###
# Lookup - 05

# Determine the frequencies for a given response type
Total = aggregate( rep(1,nrow(cD)), list(
  cD$CueRep - 2.5, cD$Subject ), sum )
Targets_rep = aggregate( SR$Choice == 1, list(
  cD$CueRep - 2.5, cD$Subject ), sum )
Competitors_rep = aggregate( SR$Choice == 2, list(
  cD$CueRep - 2.5, cD$Subject ), sum )

if ( modelFit ) {
  
  # Create data frame
  d = Targets_rep
  colnames( d ) = c('R','S','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model, collapsing over responses to give 
  # targets vs. all others
  fit = stan_glmer( cbind( Y, N - Y ) ~ R + (1|S), 
                    data = d, family = binomial("logit"), 
                    prior_intercept = normal(1.39,.3), 
                    prior = normal( .3, .3 ), 
                    chains = 8, cores = 8, seed = 7701,
                    iter = 1250,
                    warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/36, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_tar_R2 = list( post = post, prc = prc, ui = ui,
                       obs = obs )
  
  # Create data frame
  d = Competitors_rep
  colnames( d ) = c('R','S','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model, collapsing over responses to give 
  # targets vs. all others
  fit = stan_glmer(cbind(Y, N - Y) ~ R + (1|S), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(-1.76,.3), 
                   prior = normal( -.3, .3 ), 
                   chains = 8, cores = 8, seed = 3301,
                   iter = 1250,
                   warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/36, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_int_R2 = list( post = post, prc = prc, ui = ui,
                       obs = obs )
  
  setwd( 'Previous replication results' )
  save( trend_tar_R2, trend_int_R2, 
        file = 'SR_R2_trend_analysis.RData' )
  setwd( orig_dir )
  
} else {
  
  setwd( 'Previous replication results' )
  load( 'SR_R2_trend_analysis.RData' )
  setwd( orig_dir )
  
}

###
### Final recognition test (Replication 2)
###
# Lookup - 07

# Extract data for final recognition test
sel = prevRepData$Cond == 6 & prevRepData$Exp == 2
cD = prevRepData[ sel, ]

### Define useful covariates ###

# Dependent variable
cD$Y = cD$Accuracy;

# Define specific intercepts for each condition

# Targets (Selective-retrieval)
cD$TSR = 0; cD$TSR[ cD$ImageType == 1 & cD$Baseline == 0 ] = 1
# Targets (Baseline)
cD$TB = 0; cD$TB[ cD$ImageType == 1 & cD$Baseline == 1 ] = 1
# Competitors (Selective-retrieval)
cD$CSR = 0; cD$CSR[ cD$ImageType == 2 & cD$Baseline == 0 ] = 1
# Competitors (Baseline)
cD$CB = 0; cD$CB[ cD$ImageType == 2 & cD$Baseline == 1 ] = 1

# Define covariates for condition, subjects, and images

# Image type (1 = target, 2 = competitor)
cD$IT = cD$ImageType
# Selective retrieval ( 1 = yes, 0 = no )
cD$SR = 1 - cD$Baseline;
cD$S = as.factor( cD$Subject )
cD$I = as.factor( cD$ImageNum )

# Define priors
prior_vals = normal( 
  c( 1.775, 1.775, 1.475, 1.775 ), # Means
  rep( .3, 4 ) # Standard deviations
)

if ( modelFit ) {
  
  # Fit the model to the original data
  fit = stan_glmer( Y ~ -1 + TSR + TB + CSR + CB + # Fixed effects
                      (1|S) + (1|I), # Random effects
                    data = cD, family = binomial("logit"), 
                    prior = prior_vals, 
                    chains = 8, cores = 8, seed = 4546,
                    iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Generate posterior simulations
  nd = cD[,c('Y','TSR','TB','CSR','CB','S','I','IT','SR')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  est_frm_R2 = list( fit = fit, 
                     post = post, 
                     nd = nd, 
                     sim = sim, 
                     prc = prc )
  
  setwd( 'Previous replication results' )
  save( est_frm_R2, file = 'FRM_R2_post.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Previous replication results' )
  load( 'FRM_R2_post.RData' )
  setwd( orig_dir )
}

###
### Create figure
###
# Lookup - 08

ptSz = 1.5
lnSz = 2
uiWd = .05
txtSz = 1.5
uiSz = .05
posL1 = c( .5, .9 )
posL2 = c( .5, .9 )

if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'Supplementary_figure_1.pdf' ) else 
    pdf( 'Supplementary_figure_1_no_label.pdf' )
  setwd( orig_dir )
} else x11()

lyt = matrix( 1, 8, 8 )
lyt[ 1:2, 5:7] = 2;
lyt[ 3:4, 5:7] = 4;
lyt[ 5:8, 1:4] = 6;
lyt[ 5:8, 5:8] = 7;
lyt[ 1:4, 8 ] = rep( c(3,5), each = 2 )

layout( lyt )

### Selective retrieval stage

# Create a blank plot
par( mar = c( 3, 5, 1, 1 ) )
blankPlot( c(.5,4.5), c(0,1) )
abline( h = 0, lwd = lnSz )
abline( v = .5, lwd = lnSz )
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

shft = c( -.2, .2 )
clr = c( 'grey30', 'grey60' )

for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    sm = prc_r1$P[1,,]
    x = obs_r1
  }
  if ( i == 2 ) {
    sm = prc_r2$P[1,,]
    x = obs_r2
  }
  
  # Plot uncertainty and data points
  ui = apply( sm, 2, quantile, prob = c(.025,.975) )
  arrows( xa + shft[i], ui[1,], xa + shft[i], ui[2,],
          col = clr[i], code = 3, length = uiWd, angle = 90,
          lwd = lnSz )
  points( xa + shft[i], x, pch = 19, col = clr[i], cex = ptSz )
  
}

legend( 'topright', c( 'First replication', 'Second replication' ),
        fill = clr, cex = txtSz*.9, bty = 'n' )

### Trend analyses

# Targets
par( mar = c( 4, 5, 1, .5 ) )
blankPlot( c( .5, 4.5 ), c(.4, 1 ) )
abline( h = .4, lwd = lnSz )
abline( v = .5, lwd = lnSz )

axis( 1, 1:4, c('1st','2nd','3rd','4th'), tick = F, 
      cex.axis = txtSz*.8, line = -.5 )
axis( 2, c(.5,.75,1), c('50','75','100'), tick = F,
      cex.axis = txtSz*.8, line = -.5 )
mtext( 'Percent hits', side = 2, line = 2,
       cex = txtSz*.7 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topleft', 'b', bty = 'n', cex = 1.5 )
par( font = 1 )

xa = 1:4
shft = c( -.2, .2 )
clr = c( 'grey30', 'grey60' )

# Uncertainty intervals
for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    ui = trend_tar_R1$ui[c(1,3),]
    ya = list( trend_tar_R1$ui[2,], trend_tar_R1$obs$P )
  }
  if ( i == 2 ) {
    ui = trend_tar_R2$ui[c(1,3),]
    ya = list( trend_tar_R2$ui[2,], trend_tar_R2$obs$P )
  }
  
  arrows( xa + shft[i], ui[1,], xa + shft[i], ui[2,],
          col = clr[i], code = 3, length = uiWd, angle = 90,
          lwd = lnSz )
  
  lines( xa + shft[i], ya[[1]], col = clr[i], lwd = lnSz )
  points( xa + shft[i], ya[[2]], col = clr[i], pch = 19, cex = ptSz )
  
}

# Credible intervals for slope
par( mar = c( 4, .5, .5, 3 ) )
blankPlot( c(0,3), c(0,.4) )

axis( 4, seq( 0, .4, .2 ), tick = F, cex.axis = txtSz*.71,
      line = -1 )
abline( v = 3, lwd = lnSz )
mtext( 'Slope', side = 4, cex = txtSz*.8, line = 1.7 )

xa = c( .8, 2.2 )
for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    ui = quantile( trend_tar_R1$post[,2], c( .025, .975 ) )
    pts = mean( trend_tar_R1$post[,2] )
  }
  if ( i == 2 ) {
    ui = quantile( trend_tar_R2$post[,2], c( .025, .975 ) )
    pts = mean( trend_tar_R2$post[,2] )
  }
  
  arrows( xa[i], ui[1], xa[i], ui[2], length = uiWd, lwd = lnSz,
          col = clr[i], code = 3, angle = 90 )
  points( xa[i], pts, pch = 19, col = clr[i], 
          cex = ptSz )
  
}

# Intrusions
par( mar = c( 4, 5, 1, .5 ) )
blankPlot( c( .5, 4.5 ), c(.4, 0 ) )
abline( h = 0, lwd = lnSz )
abline( v = .5, lwd = lnSz )

axis( 1, 1:4, c('1st','2nd','3rd','4th'), tick = F, 
      cex.axis = txtSz*.8, line = -.5 )
axis( 2, c(0,.2,.4), c('0','20','40'), tick = F,
      cex.axis = txtSz*.8, line = -.5 )
mtext( 'Percent intrusions', side = 2, line = 2,
       cex = txtSz*.7 )

xa = 1:4
shft = c( -.2, .2 )
clr = c( 'grey30', 'grey60' )

# Uncertainty intervals
for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    ui = trend_int_R1$ui[c(1,3),]
    ya = list( trend_int_R1$ui[2,], trend_int_R1$obs$P )
  }
  if ( i == 2 ) {
    ui = trend_int_R2$ui[c(1,3),]
    ya = list( trend_int_R2$ui[2,], trend_int_R2$obs$P )
  }
  
  arrows( xa + shft[i], ui[1,], xa + shft[i], ui[2,],
          col = clr[i], code = 3, length = uiWd, angle = 90,
          lwd = lnSz )
  
  lines( xa + shft[i], ya[[1]], col = clr[i], lwd = lnSz )
  points( xa + shft[i], ya[[2]], col = clr[i], pch = 19, cex = ptSz )
  
}

# Credible intervals for slope
par( mar = c( 4, .5, .5, 3 ) )
blankPlot( c(0,3), c(-.4,0) )

axis( 4, seq( -.4, 0, .2 ), tick = F, cex.axis = txtSz*.71,
      line = -1 )
abline( v = 3, lwd = lnSz )
mtext( 'Slope', side = 4, cex = txtSz*.8, line = 1.7 )

xa = c( .8, 2.2 )
for ( i in 1:2 ) {
  
  if ( i == 1 ) {
    ui = quantile( trend_int_R1$post[,2], c( .025, .975 ) )
    pts = mean( trend_int_R1$post[,2] )
  }
  if ( i == 2 ) {
    ui = quantile( trend_int_R2$post[,2], c( .025, .975 ) )
    pts = mean( trend_int_R2$post[,2] )
  }
  
  arrows( xa[i], ui[1], xa[i], ui[2], length = uiWd, lwd = lnSz,
          col = clr[i], code = 3, angle = 90 )
  points( xa[i], pts, pch = 19, col = clr[i], 
          cex = ptSz )
  
}

### Results for final recognition task

clr = c( 'grey30', 'grey60' )
ptSz = 2
for ( i in 1:2 ) {

  # Create a blank plot
  par( mar = c( 3, 5, 3, 2 ) )
  blankPlot( c( .5, 2.5 ), c( .7, .9 ) )
  
  if ( i == 1 ) {
    sel = prevRepData$Cond == 6 & prevRepData$Exp == 1
    prc = est_frm_R1$prc
    
    # Add label for row
    if ( panelYes ) title( 'c        ', adj = 0, cex.main = 1.5,
                           line = 1.5 )
  }
  if ( i == 2 ) {
    sel = prevRepData$Cond == 6 & prevRepData$Exp == 2
    prc = est_frm_R2$prc
    
    # Add label for row
    if ( panelYes ) title( 'd        ', adj = 0, cex.main = 1.5,
                           line = 1.5 )
  }
  
  # Add legend
  legend( posL2[1], posL2[2], c('Targets'),
          pch = c( 21 ), pt.bg = c(clr[i]), col = clr[i], 
          cex = txtSz*.9, bty = 'n', horiz = T )
  
  legend( posL2[1]+.65, posL2[2], c('Competitors'),
          pch = c( 22 ), pt.bg = c('white'), col = clr[i],
          cex = txtSz*.9, bty = 'n', horiz = T )
  
  # Extract observed data
  cD = prevRepData[ sel, ]
  new = aggregate( cD$Accuracy, list( cD$ImageType, 
                                      1 - cD$Baseline ), mean )
  colnames( new ) = c( 'IT', 'SR', 'P' )
  
  # Determine x-axis positions
  xa = c( 1.1, 0.9, 1.9, 2.1 )
  ord = c( 1, 2, 3, 4 )
  
  abline( h = .7, col = 'black', lwd = lnSz )
  abline( v = .5, col = 'black', lwd = lnSz )
  axis( 2, c( .7, .8, .9 ),
        c( '70', '80', '90' ),
        tick = F, cex.axis = txtSz*.9, line = -.5 )
  mtext( 'Percent correct', side = 2, cex = txtSz*.8, line = 3 )
  axis( 1, 1:2, c( 'Baseline', 'Selective-retrieval' ),
        tick = F, cex.axis = txtSz )
  
  # Determine uncertainty intervals for original data
  ui = apply( prc, 1, quantile, prob = c( .025, .975 ) )
  arrows( xa, ui[1,ord], xa, ui[2,ord], code = 3, length = uiSz,
          angle = 90, col = clr[i], lwd = lnSz )
  
  # Plot data
  segments( xa[ c(1,2) ], new$P[ ord[ c(1,2)] ],
            xa[ c(3,4) ], new$P[ ord[ c(3,4)] ],
            lwd = lnSz, lty = 1:2, col = clr[i] )
  points( xa, new$P[ord], pch = c( 21, 22, 21, 22 ),
          bg = rep( c(clr[i],'white'), 2 ),
          col = clr[i], 
          cex = ptSz )
  
  print( '1st replication' )
  
  # Forgetting effect
  print( round( diff_pst( est_frm_R1$post, 
                          ind = c(4,3), greater = F ), 3 ) )
  # Interaction
  print( round( diff_of_diff( est_frm_R1$post, 
                              ind = list( c(2,1), c(4,3) ), 
                              greater = T ), 3 ) )
  
  print( '2nd replication' )
  
  # Forgetting effect
  print( round( diff_pst( est_frm_R2$post, 
                          ind = c(4,3), greater = F ), 3 ) )
  # Interaction
  print( round( diff_of_diff( est_frm_R2$post, 
                              ind = list( c(2,1), c(4,3) ), 
                              greater = T ), 3 ) )
}

if (savePlot) dev.off()