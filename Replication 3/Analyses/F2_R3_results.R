#-----------------------------#
# Results for 3rd replication #
# Kevin Potter                #
# Updated 10/03/2017          #
#-----------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate if model uncertainty should be estimated
modelFit = F

# Indicate if a pdf should be saved
savePlot = F

# Indicate whether panel labels should be included
panelYes = T

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Selective-retrieval stage (Original)
# Lookup - 03:  Trend analysis (Original)
# Lookup - 04:  Selective-retrieval stage (Replication)
# Lookup - 05:  Trend analysis (Replication)
# Lookup - 06:  Posterior predictive check
# Lookup - 07:  Posterior retrodictive check
# Lookup - 08:  Create figure

###
### Load in useful packages, functions, and data
###
# Lookup - 01

# For geting github packages
# install.packages(devtools)
# library(devtools)

# Miscellanous functions for modeling/plotting
# install_github("rettopnivek/utilityf")
library(utilityf)

# Load in packages for Bayesian estimation
# install.packages('rstan')
library(rstan)
# For parallel processing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# install.packages( 'rstanarm' )
library( rstanarm )

# Define some useful functions
source('F0_Useful_functions.R')

# Load in replication data
load( 'Data/Wimber_rep_3.RData' )
# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Selective-retrieval stage (Original)
###
# Lookup - 02

# Extract data for selective retrieval stage
sel = OriginalAllData$Cond == 5
cD = OriginalAllData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_od = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_od ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

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
                  seed = 5001, # For reproducibility
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
                    seed = 1984, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_od = stan_dat
  fit_od = fit
  post_od = post
  prc_od = prc
  
  setwd( 'Reanalysis results' )
  save( stan_dat_od, fit_od, post_od, prc_od, 
        file = 'SR_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Reanalysis results' )
  load( 'SR_post.RData' )
  setwd( orig_dir )
}

###
### Trend analysis (Original)
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
                    chains = 8, cores = 8, seed = 8432,
                    iter = 1250,
                    warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/54, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_tar = list( post = post, prc = prc, ui = ui,
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
                   chains = 8, cores = 8, seed = 1784,
                   iter = 1250,
                   warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/54, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_int = list( post = post, prc = prc, ui = ui,
                    obs = obs )
  
  setwd( 'Reanalysis results' )
  save( trend_tar, trend_int, 
        file = 'SR_trend_analysis.RData' )
  setwd( orig_dir )
  
} else {
  
  setwd( 'Reanalysis results' )
  load( 'SR_trend_analysis.RData' )
  setwd( orig_dir )
  
}

###
### Selective-retrieval stage (Replication)
###
# Lookup - 04

# Extract data for selective retrieval stage
sel = allData$Cond == 5
cD = allData[sel,]

SR = Extract_SR( cD )

# Determine group means
obs_rd = as.numeric( table( SR$Choice ) )/length( SR$Choice )
names( obs_rd ) = c( 'Targets', 'Intrusions', 'Errors', 'Unknown' )

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
                  seed = 5001, # For reproducibility
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
                    seed = 1847, # For reproducibility
                    algorithm = 'Fixed_param'
  )
  
  # Extract simulated means
  prc = extract( check )
  
  # Re-label objects
  stan_dat_rd = stan_dat
  fit_rd = fit
  post_rd = post
  prc_rd = prc
  
  setwd( 'Replication results' )
  save( stan_dat_rd, fit_rd, post_rd, prc_rd, 
        file = 'SR_post.RData' )
  setwd( orig_dir )
  
} else {
  setwd( 'Replication results' )
  load( 'SR_post.RData' )
  setwd( orig_dir )
}

###
### Trend analysis (Replication)
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
                    chains = 8, cores = 8, seed = 8432,
                    iter = 1250,
                    warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/54, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_tar = list( post = post, prc = prc, ui = ui,
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
                   chains = 8, cores = 8, seed = 1784,
                   iter = 1250,
                   warmup = 500)
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  prc = apply( Sim, 1, function(x) aggregate(x/54, list(d$R), mean )$x )
  ui = apply( prc, 1, quantile, prob = c( .025, .5, .975 ) )
  
  obs = aggregate( d$Y/d$N, list( d$R ), mean )
  colnames( obs ) = c( 'R', 'P' )
  
  trend_int = list( post = post, prc = prc, ui = ui,
                    obs = obs )
  
  setwd( 'Replication results' )
  save( trend_tar, trend_int, 
        file = 'SR_trend_analysis.RData' )
  setwd( orig_dir )
  
} else {
  
  setwd( 'Replication results' )
  load( 'SR_trend_analysis.RData' )
  setwd( orig_dir )
  
}

###
### Posterior predictive check
###
# Lookup - 06

# Extract data for final recognition test
sel = OriginalAllData$Cond == 6
cD = OriginalAllData[ sel, ]

# Create a data frame for data to be fitted
d = cD
# Dependent variable
d$Y = d$Accuracy
# Set missing data to be incorrect
d$Y[ is.na( d$RT ) ] = 0
# Image type (1 = target, 2 = competitor)
d$IT = as.factor( d$ImageType )
# Selective retrieval ( 1 = yes, 0 = no )
d$SR = as.factor( 1 - d$Baseline )
d$S = as.factor( d$Subject )
d$I = as.factor( d$ImageNum )
# Dummy coded variable for RIF
d$RIF = 0; d$RIF[ d$IT == 2 & d$SR == 1 ] = 1

frm = aggregate(
  d$Y, list( d$IT, d$SR ), mean )

if ( modelFit ) {
  
  # Fit the model to the original data
  fit = stan_glmer( Y ~ RIF + (1|S) + (1|I), 
                    data = d, family = binomial("logit"), 
                    prior_intercept = normal(1.775,.3), 
                    prior = normal( c(-.3), c(.3) ), 
                    chains = 8, cores = 8, 
                    seed = 3872, iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Generate posterior simulations
  nd = d[,c('Y','RIF','S','I','IT','SR')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  est_frm_od = list(
    post = post,
    prc = prc,
    frm = frm )
  
  setwd( 'Reanalysis results' )
  save( est_frm_od, 
        file = 'RM_results.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Reanalysis results' )
  load( file = 'RM_results.RData' )
  setwd( orig_dir )
}

###
### Posterior retrodictive check
###
# Lookup - 07

# Extract data for final recognition test
sel = allData$Cond == 6
cD = allData[ sel, ]

# Create a data frame for data to be fitted
d = cD
# Dependent variable
d$Y = d$Accuracy
# Set missing data to be incorrect
d$Y[ is.na( d$RT ) ] = 0
# Image type (1 = target, 2 = competitor)
d$IT = as.factor( d$ImageType )
# Selective retrieval ( 1 = yes, 0 = no )
d$SR = as.factor( 1 - d$Baseline )
d$S = as.factor( d$Subject )
d$I = as.factor( d$ImageNum )
# Dummy coded variable for RIF
d$RIF = 0; d$RIF[ d$IT == 2 & d$SR == 1 ] = 1

frm = aggregate(
  d$Y, list( d$IT, d$SR ), mean )

if ( modelFit ) {
  
  # Fit the model to the replication data
  fit = stan_glmer( Y ~ RIF + (1|S) + (1|I), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(1.57,.12), 
                   prior = normal( -.28, .08 ), 
                   chains = 8, cores = 8, seed = 3874,
                   iter = 1250, warmup = 500 )
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Generate posterior simulations
  nd = d[,c('Y','RIF','S','I','IT','SR')]; nd$Y = 0;
  sim = posterior_predict( fit, newdata = nd )
  
  # Calculate avg. accuracy over conditions of interest
  prc = apply( sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  
  est_frm_rd = list(
    post = post,
    prc = prc,
    frm = frm )
  
  setwd( 'Replication results' )
  save( est_frm_rd, 
        file = 'RM_results.RData' )
  setwd( orig_dir )
} else {
  setwd( 'Replication results' )
  load( file = 'RM_results.RData' )
  setwd( orig_dir )
}

###
### Create figure
###
# Lookup - 08

ptSz = 2
lnSz = 2
uiWd = .05
txtSz = 1.5
posL1 = c( .7, .9 )

if ( savePlot ) {
  setwd( 'Figures' )
  if ( panelYes ) pdf( 'R3_results.pdf' ) else 
    pdf( 'R3_results_no_label.pdf' )
  setwd( orig_dir )
} else x11()

lyt = matrix( 1, 8, 8 )
lyt[ 1:2, 5:7] = 2;
lyt[ 3:4, 5:7] = 4;
lyt[ 5:8, 1:4] = 6;
lyt[ 5:8, 5:8] = 7;
lyt[ 1:4, 8 ] = rep( c(3,5), each = 2 )

layout( lyt )

### Proportions for response categories

par( mar = c( 4, 5, 1, .5 ) )
blankPlot( c(.5,4.5), c(0,1) )
abline( h = 0, lwd = lnSz )
abline( v = .5, lwd = lnSz )
axis( 1, 1:4, c( 'Target', 'Competitor', 'Error', "Don't know" ),
      tick = F, cex.axis = txtSz*.71, line = -.5 )
axis( 2, seq(0,1,.25), paste( seq(0,1,.25)*100, '%', sep = '' ),
      cex.axis = txtSz*.71, tick = F, line = -.5 )
mtext( 'Percent chosen', side = 2, line = 2, cex = txtSz*.7 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topleft', 'A', bty = 'n', cex = 1.5 )
par( font = 1 )

# Original data
ui = apply( prc_od$P[1,,], 2, quantile, prob = c( .025, .975 ) )
arrows( 1:4 - .2, ui[1,], 1:4 - .2, ui[2,], 
        col = 'red', lwd = lnSz, angle = 90,
        length = uiWd, code = 3 )
points( 1:4 - .2, obs_od, pch = 19, col = 'red', 
        cex = ptSz )

# Replication data
ui = apply( prc_rd$P[1,,], 2, quantile, prob = c( .025, .975 ) )
arrows( 1:4 + .2, ui[1,], 1:4 + .2, ui[2,], 
        col = 'black', lwd = lnSz, angle = 90,
        length = uiWd, code = 3 )
points( 1:4 + .2, obs_rd, pch = 19, col = 'black', 
        cex = ptSz )

### Trend analyses

par( mar = c( 4, 5, 1, .5 ) )
blankPlot( c( 1, 4 ), c(.5, 1 ) )
abline( h = .5, lwd = lnSz )
abline( v = .9, lwd = lnSz )

axis( 1, 1:4, c('1st','2nd','3rd','4th'), tick = F, 
      cex.axis = txtSz*.71, line = -.5 )
axis( 2, c(.5,.75,1), c('50%','75%','100%'), tick = F,
      cex.axis = txtSz*.71, line = -.5 )
mtext( 'Percent hits', side = 2, line = 2,
       cex = txtSz*.7 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topleft', 'B', bty = 'n', cex = 1.5 )
par( font = 1 )

ui = apply( trend_tar$prc, 1, quantile, prob = c( .025, .5, .975 ) )

polygon( c( 1:4, 4:1 ), c( ui[1,1:4], ui[3,4:1] ),
         col = 'grey', border = NA )
lines( 1:4, ui[2,], col = 'blue', lwd = lnSz )
points( 1:4, trend_tar$obs$P, pch = 19, cex = ptSz )

# Credible intervals for slope
par( mar = c( 4, .5, .5, 3 ) )
blankPlot( c(0,2), c(0,.4) )

axis( 4, seq( 0, .4, .2 ), tick = F, cex.axis = txtSz*.71,
      line = -1 )
abline( v = 1.5, lwd = lnSz )
mtext( 'Slope', side = 4, cex = txtSz*.8, line = 1.7 )

ui = quantile( trend_tar$post[,2], c( .025, .975 ) )

arrows( .5, ui[1], .5, ui[2], length = uiWd, lwd = lnSz,
        col = 'black', code = 3, angle = 90 )
points( .5, mean( trend_tar$post[,2] ), pch = 21, 
        cex = ptSz, bg = 'black' )

# Intrutions
par( mar = c( 4, 5, 1, .5 ) )
blankPlot( c( 1, 4 ), c(0, .5 ) )
abline( h = 0, lwd = lnSz )
abline( v = .9, lwd = lnSz )

axis( 1, 1:4, c('1st','2nd','3rd','4th'), tick = F, 
      cex.axis = txtSz*.71, line = -.5 )
axis( 2, c(0,.25,.5), c('0%','25%','50%'), tick = F,
      cex.axis = txtSz*.71, line = -.5 )
mtext( 'Percent intrusions', side = 2, line = 2,
       cex = txtSz*.7 )
mtext( 'Cue repetitions', side = 1, line = 2,
       cex = txtSz*.7 )

ui = apply( trend_int$prc, 1, quantile, prob = c( .025, .5, .975 ) )

polygon( c( 1:4, 4:1 ), c( ui[1,1:4], ui[3,4:1] ),
         col = 'grey', border = NA )
lines( 1:4, ui[2,], col = 'blue', lwd = lnSz )
points( 1:4, trend_int$obs$P, pch = 19, cex = ptSz )

# Credible intervals for slope
par( mar = c( 4, .5, .5, 3 ) )
blankPlot( c(0,2), c(-.4,0) )

axis( 4, seq( -.4, 0, .2 ), tick = F, cex.axis = txtSz*.71,
      line = -1 )
abline( v = 1.5, lwd = lnSz )
mtext( 'Slope', side = 4, cex = txtSz*.8, line = 1.7 )

ui = quantile( trend_int$post[,2], c( .025, .975 ) )

arrows( .5, ui[1], .5, ui[2], length = uiWd, lwd = lnSz,
        col = 'black', code = 3, angle = 90 )
points( .5, mean( trend_int$post[,2] ), pch = 21, 
        cex = ptSz, bg = 'black' )

### Posterior predictive checks

par( mar = c( 4, 5, 1, 1 ) )
blankPlot( c(.6,2.4), c(.7,.9) )
abline( h = .7, lwd = lnSz )
abline( v = .6, lwd = lnSz )
axis( 1, 1:2, c('Targets', 'Competitors'),
      tick = F, cex.axis = txtSz*.71, line = -.5 )
axis( 2, seq(.7,.9,.1),
      paste( seq(.7,.9,.1)*100, '%', sep = '' ),
      tick = F, line = -.5, cex.axis = txtSz*.71 )
mtext( 'Percent correct', side = 2, line = 2, cex = txtSz*.7 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topright', 'C', bty = 'n', cex = 1.5 )
par( font = 1 )

xao = c( .8, 1.8, 1.2, 2.2 )
xar = c( .8, 1.8, 1.2, 2.2 )

# Add in posterior predictive checks
ui = apply( est_frm_od$prc, 1, quantile, prob = c( .025, .975 ) )
for ( i in 1:4 ) {
  polygon( xao[i] + c( -.1, -.1, .1, .1 ),
           ui[ c( 1, 2, 2, 1 ), i ], col = 'pink', 
           border = NA )
}
points( xao, est_frm_od$frm$x, pch = c( 21, 21, 22, 22 ),
        col = 'red', bg = c('red','red','white','white'),
        cex = ptSz )
points( xar, est_frm_rd$frm$x, pch = c( 21, 21, 22, 22 ),
        col = 'black', bg = c('black','black','white','white'),
        cex = ptSz )

legend( posL1[1], posL1[2],
        c( 'Original', 'Replication' ),
        pch = 19, col = c('red','black'),
        cex = txtSz*.8, bty = 'n' )

### Posterior retrodictive checks

par( mar = c( 4, 5, 1, 1 ) )
blankPlot( c(.5,2.5), c(.7,.9) )
abline( h = .7, lwd = lnSz )
abline( v = .5, lwd = lnSz )
axis( 1, 1:2, c('Targets', 'Competitors'),
      tick = F, cex.axis = txtSz*.71, line = -.5 )
axis( 2, seq(.7,.9,.1),
      paste( seq(.7,.9,.1)*100, '%', sep = '' ),
      tick = F, line = -.5, cex.axis = txtSz*.71 )
mtext( 'Percent correct', side = 2, line = 2, cex = txtSz*.7 )

# Add panel label
par( font = 2 )
if ( panelYes ) legend( 'topright', 'D', bty = 'n', cex = 1.5 )
par( font = 1 )

xao = c( .8, 1.8, 1.2, 2.2 )
xar = c( .8, 1.8, 1.2, 2.2 )

# Add in posterior retrodictive checks
ui = apply( est_frm_rd$prc, 1, quantile, prob = c( .025, .975 ) )
for ( i in 1:4 ) {
  polygon( xar[i] + c( -.1, -.1, .1, .1 ),
           ui[ c( 1, 2, 2, 1 ), i ], col = 'grey', 
           border = NA )
}
points( xao, est_frm_od$frm$x, pch = c( 21, 21, 22, 22 ),
        col = 'red', bg = c('red','red','white','white'),
        cex = ptSz )
points( xar, est_frm_rd$frm$x, pch = c( 21, 21, 22, 22 ),
        col = 'black', bg = c('black','black','white','white'),
        cex = ptSz )

legend( posL1[1], posL1[2],
        c( 'Baseline', 'Selective-retrieval' ),
        pch = c( 21, 22 ), col = 'black',
        pt.bg = c( 'black', 'white' ),
        cex = txtSz*.8, bty = 'n' )

if (savePlot) dev.off()