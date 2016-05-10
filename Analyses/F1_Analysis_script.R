#---------------------------------#
# Analysis script for replication #
# Kevin Potter                    #
# Updated 05/10/2016              #
#---------------------------------#

# Clear workspace
rm(list = ls())

# Save current directory
orig_dir = getwd()

# Load in data
# load( "All_subject_data.RData" )
load( "All_subject_data_Sim.RData" )

# Load in useful functions
library( utilityf )
library( rstan )
library( rstanarm )
options (mc.cores=parallel::detectCores ()) # Run on multiple cores

# Define additional useful functions
S = function(x) {
  # Purpose:
  # Calculates descriptive statistics for a sample of values
  # Arguments:
  # x - A vector of values
  # Returns:
  # The mean, standard error of the mean, the inter-quartile range,
  # and the min/max of the sample.
  
  out = c( mean(x), sem(x), diff( quantile(x,prob=c(.25,.75)) ), min(x), max(x) )
  
  return( out )
}

logistic = function(x) {
  # Purpose:
  # A basic logistic (or sigmoidal) function
  # Arguments:
  # x - A value (or vector) bounded by -Infinity and Infinity
  # Returns:
  # A value (or vector) bounded between 0 and 1
  
  return( 1/(1+exp(-x)) )
}

logit = function(p) {
  # Purpose:
  # A basic logit function
  # Arguments:
  # p - A value (or vector) bounded by 0 and 1
  # Returns:
  # A value (or vector) bounded by -Infinity and Infinity
  
  return( log( p/(1-p) ) )
}

violinPlot = function( x, pos, crit = NULL, scaleH = .5, 
                       type = 'greater', ... ) {
  # Purpose:
  # Forthcoming
  # Arguments:
  # Forthcoming
  # Returns:
  # Forthcoming
  
  den = density( x )
  
  if ( length(crit) > 0 ) {
    if (type=='greater') {
      sel = den$x > crit
      PostProb = sum( x > crit )/length(x)
    }
    if (type=='less') {
      sel = den$x < crit
      PostProb = sum( x < crit )/length(x)
    }
  } else {
    sel = rep( T, length( den$x ) )
    PostProb = 1
  }
  
  den$y = den$y/max(den$y); den$y = den$y*scaleH;
  xa = c( -den$y[sel], rev(den$y[sel]) ) + pos
  ya = c( den$x[sel], rev(den$x[sel]) )
  polygon( xa, ya, ... )
  
  return( PostProb )
}

runCode = c( T, T, F, F )

#---------------------------#
# Initial training accuracy #
#---------------------------#

if (runCode[1]) {
  # Extract data for initial training sesssions
  sel = allData$Cond == 2 | allData$Cond == 3 | allData$Cond == 4
  curData = allData[ sel, ]
  
  # Extract proportion correct by subject and condition
  tmp = aggregate( curData$Accuracy, list(
    curData$Cond, curData$Subject ), mean )
  
  # Determine the descriptive statistics across conditions
  InitialTraining = aggregate( tmp$x, list( tmp[,1] ), S )
  InitialTraining = cbind( c(1,1,2), c(1,2,1), InitialTraining$x )
  colnames( InitialTraining ) = c('Associate','Repetition','Mean',
                                  'SEM','IQR','Min','Max')
  print( round( InitialTraining, 2 ) )
}

#---------------------#
# Selective retrieval #
#---------------------#

if ( runCode[2] | runCode[3] ) {
  
  # Extract data for selective retrieval stage
  sel = allData$Cond == 5
  curData = allData[sel,]
  
  # Category for target images
  Targets = floor(curData$Category)
  # Category for competitor images
  Competitors = round( 10*(curData$Category - Targets) )
  # The category of the unrelated error
  Errors = apply( cbind( Targets, Competitors ), 1, 
                  function(x) { tar = 1:3 %in% x[1]; com = 1:3 %in% x[2];
                  err = 1 - (tar+com); (1:3)[err==1] } )
  # If unknown, subjects could pick a 4th response category
  Unknown = rep( 4, nrow( curData ) )
  
  # Determine a subject's choice based on categories
  Choice = rep( NA, nrow( curData ) )
  Choice[ curData$Resp == Targets ] = 1
  Choice[ curData$Resp == Competitors ] = 2
  Choice[ curData$Resp == Errors ] = 3
  Choice[ curData$Resp == Unknown ] = 4
  
}


### Estimate uncertainty around category performance ###
if (runCode[2]) {
  
  # Descriptive statistics for group-level performance
  propCat = aggregate( Choice, list( curData$Subject ), 
                       function(x) table(x)/length(x) )
  colnames( propCat$x ) = c('Target','Competitor','Error','Unknown')
  colnames( propCat ) = c('S','Category')
  
  DescStat = aggregate( propCat$Category, list( rep(1,nrow(propCat) ) ), S )
  DescStat = matrix( unlist( DescStat[1,-1] ), 4, 5, byrow = T )
  colnames( DescStat ) = c('Mean','SEM','IQR','Min','Max')
  rownames( DescStat ) = c('Target','Competitor','Error','Unknown')
  print( round( DescStat, 2 ) )
  
  # Model script for fitting categorical distribution to set of 
  # 4 response categories (i.e. Target, Competitor, Error, Don't know).
  model_script = "
  data {
    int<lower=1> No; // Total number of observations
    int<lower=1> Ns; // Total number of subjects
    int<lower=2> K; // Number of response categories
    int<lower=1,upper=K> Y[No]; // Observed responses
    int indS[No]; // Subject index per trial
    matrix[K-1,4] Priors; // Matrix of parameter values for priors
  }
  parameters{
    matrix[ Ns, K-1 ] beta_raw; // Subject-level coefficients
    real mu_beta[K-1]; // Group-level means
    real<lower=0> sigma_beta[K-1]; // Group-level standard deviations
  }
  transformed parameters {
    vector[Ns] zeros;
    matrix[ Ns, K ] beta; // Coefficients for all categories
    simplex[ K ] theta[ Ns ]; // Category probabilities
    
    zeros <- rep_vector(0, Ns);
    beta <- append_col( beta_raw, zeros ); // Fix 'unknown' category to 0
    for (ns in 1:Ns ) theta[ ns ] <- softmax( to_vector( beta[ ns ] ) );
  }
  model {
    
    // Priors
    mu_beta ~ normal( col( Priors, 1 ), col( Priors, 2 ) );
    sigma_beta ~ gamma( col( Priors, 3 ), col( Priors, 4 ) );
    
    // Hierarchy
    for (i in 1:3) {
      col( beta_raw, i ) ~ normal( mu_beta[i], sigma_beta[i] );
    }
    
    // Likelihood
    for ( no in 1:No ) {
      Y[no] ~ categorical( theta[ indS[no] ] );
    }
    
  }
  "
  
  # Save as .stan file
  setwd('Stan_scripts')
  writeChar( model_script, "SR_categories.stan" )
  
  indS = numeric( nrow( curData ) )
  inc = 1
  for ( i in unique( curData$Subject ) ) {
    indS[ curData$Subject == i ] = inc
    inc = inc + 1
  }
  
  # Define priors based on previous results of Wimber et al. (2015)
  Priors = cbind( c( 1.700, -.409, -1.700 ),
                  c( .3, .3, .3 ),
                  c( 2, 2, 2 ),
                  c( 8, 8, 8 ) )
  
  stan_dat = list(
    No = length( Choice ),
    Ns = N,
    K = 4,
    Y = Choice,
    indS = indS,
    Priors = Priors
  )
  
  burn = 500 # Burn-in
  niter = 1250 # Number of samples to approximate posterior
  
  startTime = Sys.time() # To assess run-time
  fit = stan(file = 'SR_categories.stan', data = stan_dat, 
             warmup = burn, iter = burn+niter, 
             chains = 8 )
  
  post = extract(fit)
  # Report run time
  runTime = Sys.time() - startTime
  print( runTime )
  rm( startTime )
  
  # Return to current directory
  setwd(orig_dir)
}

### Trend analysis ###
if (runCode[3]) {
  
  Total = aggregate( rep(1,nrow(curData)), list(
    curData$CueRep - 1.5, curData$Subject ), sum )
  Targets_rep = aggregate( Choice == Targets, list(
    curData$CueRep - 1.5, curData$Subject ), sum )
  Competitors_rep = aggregate( Choice == Competitors, list(
    curData$CueRep - 1.5, curData$Subject ), sum )
  
  # For targets...
  # Create data frame
  d = Targets_rep
  colnames( d ) = c('S','R','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model to the original data
  fit = stan_glmer(cbind(Y, N) ~ R + (1|S), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(1.39,.3), 
                   prior = normal( .3, .3 ), 
                   chains = 8, cores = 8, seed = 5019,
                   iter=1250,warmup=500)
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  x11()
  layout( cbind( 1, 2 ) )
  plot( c(0,1), c(-.5,.5), type = 'n' )
  violinPlot( post[,2], pos = .5, scaleH = .4, crit = 0,
              border = NA, col = 'grey' )
  violinPlot( post[,2], pos = .5, scaleH = .4 )
  
  # For targets...
  # Create data frame
  d = Competitors_rep
  colnames( d ) = c('S','R','Y')
  d$S = as.factor( d$S )
  d$N = Total$x
  
  # Fit the model to the original data
  fit = stan_glmer(cbind(Y, N) ~ R + (1|S), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(1.39,.3), 
                   prior = normal( -.3, .3 ), 
                   chains = 8, cores = 8, seed = 7439,
                   iter=1250,warmup=500)
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  plot( c(0,1), c(-.5,.5), type = 'n' )
  violinPlot( post[,2], pos = .5, scaleH = .4, crit = 0,
              border = NA, col = 'grey' )
  violinPlot( post[,2], pos = .5, scaleH = .4 )
}

#------------------------#
# Final recognition test #
#------------------------#

if (runCode[4]) {
  
  # Extract data for final recognition test
  sel = allData$Cond == 6
  curData = allData[ sel, ]
  
  d = curData # Create a data frame for data to be fitted
  d$Y = d$Accuracy # Dependent variable
  d$IT = as.factor( d$ImageType ) # Image type (1 = target, 2 = competitor)
  d$SR = as.factor( 1 - d$Baseline ) # Selective retrieval ( 1 = yes, 0 = no )
  d$S = as.factor( d$Subject )
  d$I = as.factor( d$ImageNum )
  d$Trial = rep(1,nrow(d)) # Nuisance parameter for bernoulli distribution
  d$RIF = 0; d$RIF[ d$IT == 2 & d$SR == 1 ] = 1 # Dummy coded variable for RIF
  
  # Fit the model to the original data
  fit = stan_glmer(cbind(Y, Trial) ~ RIF + (1|S) + (1|I), 
                   data = d, family = binomial("logit"), 
                   prior_intercept = normal(1.76,.11), 
                   prior = normal( -.29, .09 ), 
                   chains = 8, cores = 8, seed = 3874,
                   iter=1250,warmup=500)
  
  # Extract the posterior estimates
  post = as.matrix( fit )
  
  # Simulate data from the posterior estimates
  nd = d; nd$Y = 0;
  Sim = posterior_predict( fit, newdata = nd )
  
  ### Group-level performance ###
  
  # Calculate avg. accuracy over conditions of interest
  tmp = apply( Sim, 1, function(x) 
    aggregate( x, list( nd$IT, nd$SR ), mean )$x )
  obs = aggregate( d$Y, list( d$IT, d$SR ), mean )$x
  # Create a plot of the posterior predictive check
  x11()
  plot( c(1,4), c(.5,1), type='n', bty='l',
        xlab = 'Condition', xaxt='n', yaxt='n',
        ylab = 'P(Correct)' )
  abline( h = c(.85,.8,.75), col = 'grey' )
  axis( 1, 1:4, c('T-BS','C-BS','T-SR','C-SR'), tick=F )
  axis( 2, seq(.5,1,.1) )
  tmp2 = apply( tmp, 1, quantile, prob=c(.025,.25,.5,.75,.975) )
  segments( 1:4, tmp2[1,], 1:4, tmp2[5,] )
  segments( 1:4, tmp2[2,], 1:4, tmp2[4,], lwd = 3 )
  points( 1:4, tmp2[3,], pch = 21, bg='white', cex = 2 )
  points( 1:4, obs, pch = 19, col = 'blue' )
  
}