#---------------------------------#
# Multinomial processing model w/ # 
# subject and item effects        #
# Kevin Potter                    #
# Updated 12/09/2017              #
#---------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Indicate which code segments to run
runCode = c( F, F, F, F, T, T )

# Index
# Lookup - 01:  Load in useful packages, functions, and data
# Lookup - 02:  Define additional functions
# Lookup - 03:  Extract data
# Lookup - 04:  Simulation and parameter recovery
# Lookup - 05:  Main effects and interactions over data sets
# Lookup - 06:  Approaches for incorporating past performance
# Lookup - 07:  Robustness check of simple comparisons
# Lookup - 08:  Replication 3
# Lookup - 09:  Replication 2

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

# Load in package for R to C++ interface
library(Rcpp)

# Load in packages for Bayesian estimation
# install.packages( 'rstan' )
library( rstan )
# For parallel processing
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Define some useful functions
source( 'F0_Useful_functions.R' )

# Load in original data
load( 'Data/Original_all_data.RData' )

###
### Define additional functions
###
# Lookup - 02

#
# Function to fit a multinomial process model with 
# item and subject effects
#

model_fit = function( dat, IV, 
                      Priors = NULL,
                      beta_offsets = NULL,
                      control = list(
                        warm = 250,
                        niter = 1250,
                        chains = 8,
                        seed = runif(1) * 1000
                        ) ) {
  
  # Extract design matrix
  X = as.matrix( dat[, IV ] )
  
  # Determine indices for subjects/items
  subjIndex = createIncrement( dat$S )
  itemIndex = createIncrement( dat$IN )
  
  # Check for matrix of prior values
  if ( length( Priors ) == 0 ){
    Priors = matrix( 0, ncol( X ) + 2, 2 )
    # Default priors for random effects' standard deviations
    Priors[ ncol( X ) + 1, ] = c( 2, 4 )
    Priors[ ncol( X ) + 2, ] = c( 2, 4 )
    prior_type = 2
  } else {
    prior_type = 1
  }
  
  # Check for offsets to apply to fixed effects
  if ( length( beta_offsets ) == 0 ) {
    beta_offsets = rep( 0, nrow( X ) )
  }
  
  # Define input for Stan
  stanDat = list(
    No = nrow( X ),
    Ns = max( subjIndex ),
    Ni = max( itemIndex ),
    K = ncol( X ),
    subjIndex = subjIndex,
    itemIndex = itemIndex,
    Y = dat$Ac,
    X = X,
    beta_offsets = beta_offsets, 
    prior_type = prior_type,
    Priors = Priors
  )
  
  startTime = Sys.time() # To assess run-time
  
  # Compile model
  sm = stan_model(stanc_ret = 
                    stanc_builder("Stan_scripts/MPM_mixed.stan"))
  
  # Draw samples
  fit = sampling( sm, data = stanDat, 
                  warmup = control$warm, 
                  iter = control$warm+control$niter, 
                  chains = control$chains,
                  seed = control$seed )
  
  # Extract posterior samples
  post = extract(fit)
  
  # Extract convergence diagnostics
  Rhat = summary(fit)$summary[,"Rhat"]
  n_eff = summary(fit)$summary[,"n_eff"]
  totSampSize = length(extract(fit, pars = "lp__")[[1]])
  # We're only interested in a subset of parameters
  par_name = names( Rhat )[ 
        which( names( Rhat ) == "sigma_i" ) ]
  sel = 1:which( names(Rhat) == par_name )
  Rhat = Rhat[sel]; n_eff = n_eff[sel];
  
  # Determine run-time
  runTime = Sys.time() - startTime
  print( runTime ); rm( startTime )
  
  # Return output
  return( list(
    post = post,
    stanDat = stanDat,
    Rhat = Rhat,
    n_eff = n_eff,
    totSampSize = totSampSize ) )
}

#
# C++ function to generate posterior simulations
#

cpp_code_1 = '
IntegerMatrix post_sim( Rcpp::List post, 
                        Rcpp::List stanDat ) {
  
  int Ns = stanDat["Ns"]; // Number of subjects
  int Ni = stanDat["Ni"]; // Number of items
  // Index for subjects
  IntegerVector subjIndex = stanDat["subjIndex"];
  // Index for items
  IntegerVector itemIndex = stanDat["itemIndex"];
  
  // Extract matrix of posterior samples for beta
  arma::mat post_samp_beta = post["beta"];
  // Extract standard deviation for subject effects
  arma::vec sigma_s = post["sigma_s"];
  // Extract standard deviation for item effects
  arma::vec sigma_i = post["sigma_i"];
  
  // Number of posterior samples
  int S = post_samp_beta.n_rows;
  
  // Extract design matrix
  arma::mat X = stanDat[ "X" ];
  int K = X.n_cols; // Number of predictors
  int N = X.n_rows; // Number of observations to simulate
  
  // Extract beta offsets
  Rcpp::NumericVector beta_offsets = stanDat["beta_offsets"];
  
  // Initialize output
  Rcpp::IntegerMatrix out( S, N );
  
  // Nuisance variables
  arma::mat beta( K, 1 );
  arma::mat alpha ( N, 1 );
  double eta;
  double theta;
  double mu;
  Rcpp::NumericVector alpha_s_raw(Ns);
  Rcpp::NumericVector alpha_i_raw(Ni);
  
  // Loop over posterior samples
  for ( int s = 0; s < S; s++ ) {
    
    // Generate subject/item effects
    for ( int ns = 0; ns < Ns; ns++ ) {
      alpha_s_raw(ns) = R::rnorm( 0.0, 1.0 );
    }
    for ( int ni = 0; ni < Ni; ni++ ) {
      alpha_i_raw(ni) = R::rnorm( 0.0, 1.0 );
    }
    
    // Calculate predicted logit of P(Recall)
    for ( int k = 0; k < K; k++ ) {
      beta( k, 0 ) = post_samp_beta( s, k );
    }
    alpha = X * beta;
    
    // Loop over observations
    for ( int n = 0; n < N; n++ ) {
      // Incorporate subject/item effects
      mu = alpha(n,0) + alpha_s_raw( subjIndex(n) - 1 ) * sigma_s(s) + 
        alpha_i_raw( itemIndex(n) - 1 ) * sigma_i(s) + 
        beta_offsets(n);
      eta = 1.0/( 1.0 + exp( -mu ) );
      theta = eta + 0.5 * ( 1.0 - eta );
      out( s, n ) = R::rbinom( 1, theta );
    }
    
  }
  
  return( out );
}
'

# Compile code
cppFunction( cpp_code_1, depends = 'RcppArmadillo' )

#
# Define function for computing proportion correct 
# over unique combinations of levels for different factors
#

cpp_code_1 = '
NumericMatrix mean_over_combos( NumericMatrix Y, 
                                IntegerVector UniqueCombos,
                                int r ) {
  
  // Determine number of columns in Y
  int C = Y.ncol();
  // Determine total number of combinations
  int K = max( UniqueCombos );
  
  NumericVector totals(K); // Track number of observations
  NumericVector sums(K); // Track running sums
  int sel; // Index for current combination of conditions
  
  // Define output
  NumericMatrix out(K,1);
  
  for ( int c = 0; c < C; c++ ) {
    sel = UniqueCombos(c) - 1;
    sums( sel ) = sums( sel ) + Y(r,c);
    totals( sel ) = totals( sel ) + 1.0;
  }
  
  out(_,0) = sums/totals;
  
  // Return means
  return out;
}
'

cpp_code_2 = '
NumericMatrix quick_agg( NumericMatrix Y, 
                         IntegerVector UniqueCombos ) {
  
  int R = Y.nrow(); // Extract number of rows
  int C = Y.ncol(); // Extract number of columns
  int K = max( UniqueCombos ); // Maximum number of combinations
  
  // Define matrix for output
  NumericMatrix out(K,R);
  
  // Loop over rows
  for ( int r = 0; r < R; r++ ) {
    
    out(_,r) = mean_over_combos( Y, UniqueCombos, r );
    
  }
  
  return out;
}
'

cppFunction( cpp_code_2, includes = cpp_code_1 )
rm( cpp_code_2, cpp_code_1 )

#
# Convenience function for computing uncertainty intervals
#

quick_ui = function( x, alpha = .95 ) {
  
  interval = numeric(2)
  interval[1] = ( 1 - alpha )/2
  interval[2] = interval[1] + alpha
  
  return( quantile( x, interval ) )
}

#
# Convenience function for computing posterior probabilities
#

post_prob = function( x, greater = T, crit = 0 ) {
  
  if ( greater ) 
    out = mean( x > crit ) else
      out = mean( x < crit )
  
  return( out )
}

#
# Function to plot posterior retrodictive checks
#

post_check = function( output, UniqueCombos, new = T,
                       opt = list(
                         lnSz = 2,
                         txtSz = 1.1 ), ... ) {
  
  # Number of levels
  L = length( unique( UniqueCombos ) )
  
  # Observed data
  obs = aggregate( output$stanDat$Y, 
                   list( UniqueCombos ), mean )
  colnames( obs ) = c( 'Cnd', 'P' )
  
  # Posterior retrodictive checks
  sim = post_sim( output$post, output$stanDat )
  pred = quick_agg( sim, UniqueCombos )
  
  # Uncertainty intervals
  ui_95 = apply( pred, 1, quick_ui )
  ui_68 = apply( pred, 1, quick_ui, alpha = .68 )
  ui_50 = apply( pred, 1, quick_ui, alpha = .5 )
  # Mode
  md = apply( pred, 1, findMode )
  
  # y-axis boundaries
  yl = lowerUpper( .2, ui_95 )
  yl[1] = max( 0.0, yl[1] )
  yl[2] = min( 1.0, yl[2] )
  
  # x-axis boundaries
  xl = c( .5, L + .5 )
  
  if ( new ) x11();
  
  blankPlot( xl, yl )
  abline( v = xl[1], lwd = opt$lnSz )
  abline( h = yl[1], lwd = opt$lnSz )
  axis( 2, seq( yl[1], yl[2], .2 ),
        paste( round( 100 * seq( yl[1], yl[2], .2 ) ), '%', sep = '' ),
        tick = F, line = -.5, cex.axis = opt$txtSz )
  mtext( 'Percent correct', side = 2, cex = opt$txtSz,
         line = 2 )
  
  # Loop over observations
  wdth = c( .1, .3 )
  for ( l in 1:L ) {
    
    arrows( l, ui_95[1,l], l, ui_95[2,l],
            code = 3, col = 'grey', angle = 90,
            length = .05, lwd = opt$lnSz )
    
    for ( i in 1:2 ) {
      
      if ( i == 1 ) ui = ui_68[,l];
      if ( i == 2 ) ui = ui_50[,l];
      
      polygon( l + c( wdth[i], wdth[i], -wdth[i], -wdth[i] ),
               ui[ c( 1, 2, 2, 1 ) ],
               col = 'grey', border = NA )
      
    }
  }
  segments( 1:L - wdth[2], md, 1:L + wdth[2], md,
            col = 'grey60', lty = 2, lwd = opt$lnSz )
  
  # Add actual observations
  points( 1:L, obs$P, ... )
  
}

#
# Function to compute posterior p-values for retrodictive checks
#

retro_check = function( output, UniqueCombos ) {
  
  # Number of levels
  L = length( unique( UniqueCombos ) )
  
  # Observed data
  obs = aggregate( output$stanDat$Y, 
                   list( UniqueCombos ), mean )
  colnames( obs ) = c( 'Cnd', 'P' )
  
  # Posterior retrodictive checks
  sim = post_sim( output$post, output$stanDat )
  pred = quick_agg( sim, UniqueCombos )
  
  p_more_crit = numeric( L )
  p_less_crit = numeric( L )
  
  for ( i in 1:L ) {
    p_more_crit[i] = post_prob( pred, greater = T, crit = obs$P[i] )
    p_less_crit[i] = post_prob( pred, greater = F, crit = obs$P[i] )
  }
  
  out = pmin( p_more_crit, p_less_crit )
  return( out )
}

#
# Function to plot the posterior distributions for the fixed effects
#

plot_fixef_dist = function( post, alpha = c( .95, .68, .5), 
                            prec = .5,
                            opt = list(
                              lnSz = 2,
                              txtSz = 1.1 ),
                            new = T ) {
  
  # Sort from highest to lowest
  alpha = alpha[ order( alpha, decreasing = T ) ]
  
  # Number of intervals
  Ni = length( alpha )
  # Lower and upper boundaries of intervals
  interval = matrix( NA, Ni, 2 )
  interval[,1] = (1 - alpha)/2
  interval[,2] = interval[,1] + alpha
  
  # Number of fixed effects
  K = ncol( post$beta )
  
  # Uncertainty intervals for largest width
  ui = apply( post$beta, 2, quantile, prob = interval[1,] )
  yl = lowerUpper( prec, as.vector( ui ) )
  
  if ( new ) x11( width = 12 )
  
  # Create blank plot
  blankPlot( c( .5, K + .5 ), yl )
  abline( h = opt$yl[1], lwd = opt$lnSz )
  abline( v = .5, lwd = opt$lnSz )
  
  axis( 2, seq( yl[1], yl[2], length = 5 ),
        round( seq( yl[1], yl[2], length = 5 ), 2 ),
        tick = F, cex.axis = opt$txtSz, line = -.5 )
  mtext( 'Additive effects for logit of P(Recall)', 
         side = 2, line = 2, cex = opt$txtSz )
  mtext( 'Fixed effects', side = 1, line = 2.25, cex = opt$txtSz )
  
  for ( i in 1:Ni ) {
    
    if ( i == 1 ) {
      
      arrows( 1:K, ui[1,], 1:K, ui[2,],
              col = 'grey', lwd = opt$lnSz,
              code = 3, angle = 90, length = .1 )
      
    } else {
      for ( k in 1:K ) {
        ui = apply( post$beta, 2, quantile, prob = interval[i,] )
        scl = .1 + .1*(i - 1)
        polygon( k + c( -scl, -scl, scl, scl ),
                 ui[ c( 1, 2, 2, 1 ), k ],
                 col = 'grey', border = NA )
      }
    }
    
  }
  xbar = colMeans( post$beta )
  segments( 1:K - scl, xbar,
            1:K + scl, xbar,
            lwd = opt$lnSz, col = 'grey60', lty = 2 )
  
  segments( .5, 0, K + .5, 0, lty = 2, lwd = opt$lnSz )
  
}

#
# Function to compute Akaike weights
#

akaike_weights = function( AIC_val ) {
  
  delta_AIC = AIC_val - min(AIC_val) # Compute difference scores
  rel_L = exp( -.5 * delta_AIC ) # Determine relative likelihoods
  w = rel_L/sum( rel_L ) # Compute normalized weights
  
  return( list( w = w, rel_L = rel_L ) )
}

#
# Function to compute the sum of the log-likelihoods based 
# on the posterior mode
#

compute_log_likelihood = function( output ) {
  
  Y = output$stanDat$Y
  k = output$stanDat$beta_offsets
  X = output$stanDat$X
  
  # Find the posterior modes
  beta = apply( output$post$beta, 2, findMode )
  alpha_s_raw = apply( output$post$alpha_s_raw, 
                       2, findMode )
  alpha_i_raw = apply( output$post$alpha_i_raw, 
                       2, findMode )
  sigma_s = findMode( output$post$sigma_s )
  sigma_i = findMode( output$post$sigma_i )
  
  alpha_s = alpha_s_raw * sigma_s
  alpha_i = alpha_i_raw * sigma_i
  
  subjIndex = output$stanDat$subjIndex
  itemIndex = output$stanDat$itemIndex
  
  # Compute P(Correct)
  alpha = k + X %*% beta + alpha_s[subjIndex] + alpha_i[itemIndex];
  eta = logistic( alpha )
  theta = eta + .5 * ( 1 - eta )
  
  out = sum( dbinom( Y, 1, theta, log = T ) )
  
  return( out )
}

#
# Function to extract data for final 
# recognition memory task for replications 2
# and 3
#

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

###
### Extract data
###
# Lookup - 03

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

# Effects coding
od$ITef = -1; od$ITef[ od$IT == 1 ] = 1;
od$SRef = -1; od$SRef[ od$B == 0 ] = 1
od$ITxSR = od$ITef * od$SRef

# Intercept
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

# Define variable for unique combinations of levels
fr$Cnd = 0
fr$Cnd[ fr$IT == 2 & fr$SR == 0 ] = 1
fr$Cnd[ fr$IT == 1 & fr$SR == 0 ] = 2
fr$Cnd[ fr$IT == 1 & fr$SR == 1 ] = 3
fr$Cnd[ fr$IT == 2 & fr$SR == 1 ] = 4

# Define predictor based on number of times subject got an 
# item correct during training
tmp = aggregate( pd$Ac, list( pd$IN, pd$S ), sum )
colnames( tmp ) = c( 'IN', 'S', 'X' )
fr$IC = 0
for ( i in 1:nrow(fr) ) {
  fr$IC[i] = tmp$X[ tmp$IN == fr$IN[i] & tmp$S == fr$S[i] ]
}
# Standardize
fr$IC = scale( fr$IC )


###
### Simulation and parameter recovery
###
# Lookup - 04

if ( runCode[1] ) {
  
  # Indicate which model to run
  modelNum = 2
  
  ### Model 1 ###
  
  if ( modelNum == 1 ) {
    # Fixed effects (logit units)
    beta = cbind(
      rnorm( 4, c( 0, # Intercept
                   .00, # Image types
                   -.3, # Manupilation type
                   .20  # Interaction
      ),
      rep( .5, 4 ) )
    )
    sigma_s = .5 # Standard deviation for subject effect
    sigma_i = .5 # Standard deviation for item effects
    
    Ns = 24 # Number of subjects
    Ni = 144 # Number of items
    
    # Generate subject/item level effects
    alpha_s_raw = rnorm( Ns )
    alpha_i_raw = rnorm( Ni )
    
    # Re-scale base on standard deviations
    alpha_s = alpha_s_raw * sigma_s
    alpha_i = alpha_i_raw * sigma_i
    
    # Base simulated data on observed struture
    simDat = fr[ fr$MR == 0, ]
    No = nrow( simDat ) # Number of observations
    # Extract design matrix
    IV = c( 'Int', 'ITef', 'SRef','ITxSR' )
    X = as.matrix( simDat[,IV] )
    # Re-label item indices
    simDat$IN = createIncrement( simDat$IN )
    # Simulate data
    alpha = X %*% beta + alpha_s[ simDat$S ] + alpha_i[ simDat$IN ];
    eta = logistic( alpha )
    theta = eta + .5 * ( 1 - eta )
    simDat$Ac = rbinom( No, 1, theta )
    
    # Fit model
    output = model_fit( simDat, c( 'Int', 'ITef', 'SRef','ITxSR' ) );
  }
  
  ### Model 2 ###
  if ( modelNum == 2 ) {
    
    # Fixed effects (logit units)
    beta = cbind(
      rnorm( 2, c( 0, # Intercept
                   -.3 # Planned comparison
      ),
      rep( .5, 2 ) )
    )
    sigma_s = .1 # Standard deviation for subject effect
    sigma_i = .5 # Standard deviation for item effects
    
    Ns = 24 # Number of subjects
    Ni = 144 # Number of items
    
    # Generate subject/item level effects
    alpha_s_raw = rnorm( Ns )
    alpha_i_raw = rnorm( Ni )
    
    # Re-scale base on standard deviations
    alpha_s = alpha_s_raw * sigma_s
    alpha_i = alpha_i_raw * sigma_i
    
    # Base simulated data on observed struture
    simDat = fr[ fr$MR == 0, ]
    No = nrow( simDat ) # Number of observations
    # Extract design matrix
    IV = c( 'Int', 'PC' )
    X = as.matrix( simDat[,IV] )
    # Re-label item indices
    simDat$IN = createIncrement( simDat$IN )
    # Simulate data (with offsets)
    alpha = simDat$PP + X %*% beta + 
      alpha_s[ simDat$S ] + alpha_i[ simDat$IN ];
    eta = logistic( alpha )
    theta = eta + .5 * ( 1 - eta )
    simDat$Ac = rbinom( No, 1, theta )
    
    # Fit model
    output = model_fit( simDat, c( 'Int', 'PC' ),
                        beta_offsets = simDat$PP );
    post_check( output, simDat$Cnd, pch = 19, cex = 1.5 )
    
  }
  
}

###
### Main effects and interactions over data sets
###
# Lookup - 05

if ( runCode[2] ) {
  
  # Create the data-sets of interest
  all_dtbf = c()
  for ( i in 1:8 ){
    all_dtbf = c( all_dtbf, list(NULL) )
  }
  names( all_dtbf ) = c( 'D1', 'D2', 'D3', 'D4',
                         'D5', 'D6', 'D7', 'D8' )
  
  cd = fr;
  cd$Ac[ fr$MR == 1 ] = 0
  all_dtbf$D1 = cd
  all_dtbf$D2 = cd[ fr$S != 12, ]
  all_dtbf$D3 = cd[ fr$S != 15, ]
  all_dtbf$D4 = cd[ fr$S != 12 & fr$S != 15, ]
  sel = fr$MR == 0
  all_dtbf$D5 = fr[ sel, ]
  all_dtbf$D6 = fr[ fr$S != 12 & sel, ]
  all_dtbf$D7 = fr[ fr$S != 15 & sel, ]
  all_dtbf$D8 = fr[ fr$S != 12 & fr$S != 15 & sel, ]
  
  # Store posterior p-values for 
  mpm_aov = c()
  for ( j in 1:8 ) {
    mpm_aov = c( mpm_aov, list( list( pv = NULL, prc = NULL) ) )
  }
  names( mpm_aov ) = names( all_dtbf )
  
  # Design matrix
  IV = c( 'Int', 'IT', 'SR', 'ITxSR' )
  
  # Loop over data sets
  for ( i in 1:8 ) {
    
    output = model_fit( all_dtbf[[i]], IV );
    
    p_more_zero = apply( output$post$beta, 2, post_prob, greater = T )
    p_less_zero = apply( output$post$beta, 2, post_prob, greater = F )
    
    mpm_aov[[i]]$pv = pmin( p_more_zero, p_less_zero )
    mpm_aov[[i]]$prc = retro_check( output, all_dtbf[[i]]$Cnd )
    
  }
}

quick_list_extract = function( lst, var ) {
  out = matrix( NA, length( lst[[1]][[var]] ), length( lst ) )
  for ( i in 1:length( lst ) ) {
    out[,i] = round( lst[[i]][[var]], 3 )
  }
  
  return( out )
}

###
### Approaches for incorporating past performance
###
# Lookup - 06

if ( runCode[3] ) {
  
  # Exclude missing responses and subject 15
  dtbf = fr[ fr$MR == 0 & fr$S != 15, ]
  
  # Fit model w/ simple effects
  IV = c( 'CB', 'TB', 'TSR', 'CSR' )
  
  # Fit default model
  m1 = model_fit( dtbf, IV );
  round( retro_check( m1, dtbf$Cnd ), 3 )
  
  # Fit model with pre-defined offsets
  m2 = model_fit( dtbf, IV, beta_offsets = dtbf$PP )
  round( retro_check( m2, dtbf$Cnd ), 3 )
  
  # Fit model with additional predictor
  m3 = model_fit( dtbf, c( IV, 'IC' ) )
  round( retro_check( m3, dtbf$Cnd ), 3 )
  
  # Fit model with simple effect of C-B
  m4 = model_fit( dtbf, c( 'Int', 'CB', 'IC' ) )
  round( retro_check( m4, dtbf$Cnd ), 3 )
  
  # Fit model with main effects only
  m5 = model_fit( dtbf, c( 'Int', 'SRef', 'IC' ) )
  round( retro_check( m5, dtbf$Cnd ), 3 )
  
  # Fit default model
  m6 = model_fit( dtbf, c( 'Int', 'PC' ) );
  round( retro_check( m6, dtbf$Cnd ), 3 )
  
  # Fit default model
  m7 = model_fit( dtbf, c( 'Int', 'PC', 'IC' ) );
  round( retro_check( m7, dtbf$Cnd ), 3 )
  
  loo_m1 = loo( m1$post$logLik )
  loo_m2 = loo( m2$post$logLik )
  loo_m3 = loo( m3$post$logLik )
  loo_m4 = loo( m4$post$logLik )
  loo_m5 = loo( m5$post$logLik )
  loo_m6 = loo( m6$post$logLik )
  loo_m7 = loo( m7$post$logLik )
  
  lw = c( loo_m1$looic,
          loo_m2$looic,
          loo_m3$looic,
          loo_m4$looic,
          loo_m5$looic,
          loo_m6$looic,
          loo_m7$looic )
  
  x11()
  blankPlot( c(.8, length(lw) + .2 ),
             c( 0, 1 ) )
  abline( h = 0, lwd = 2 )
  abline( v = .8, lwd = 2 )
  
  points( 1:7, akaike_weights( lw )$w, pch = 19, cex = 1.5 )
  axis( 1, 1:7, cex.axis = 1.2, line = -.5, tick = F )
  axis( 2, seq(0,1,.25), cex.axis = 1.2, line = -.5, tick = F )
  mtext( 'Akaike weights', side = 2, line = 2 )
  mtext( 'Models', side = 1, line = 2 )
  
  legend( 'topright', c( '1 = AOV', 
                         '2 = AOV w/ constant', 
                         '3 = AOV w/ past',
                         '4 = CSR w/ past', 
                         '5 = SR w/ past', 
                         '6 = Forgetting (C)',
                         '7 = Forgetting (C) w/ past' ),
          bty = 'n' )
  
}

###
### Robustness check of simple comparisons
###
# Lookup - 07

if ( runCode[4] ) {
  
  # Create the data-sets of interest
  all_dtbf = c()
  for ( i in 1:2 ){
    all_dtbf = c( all_dtbf, list(NULL) )
  }
  names( all_dtbf ) = c( 'MaE', 'MR' )
  
  # Define meaningful label for conditions
  fr$SRL = 'Selective retrieval'
  fr$SRL[ fr$B == 1 ] = 'Baseline'
  
  # Define meaningful label for image types
  fr$ITL = 'Target'
  fr$ITL[ fr$IT == 2 ] = 'Competitor'
  
  # Accuracy
  fr$Y = fr$Ac
  # Recode missing as errors
  fr$Ym = fr$Y; fr$Ym[ fr$MR == 1 ] = 0
  dtbf = fr;
  
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
  
  # Create data sets
  all_dtbf$MaE = dtbf
  all_dtbf$MaE$Ac = dtbf$Ym
  all_dtbf$MR = dtbf
  all_dtbf$MR = all_dtbf$MR[ dtbf$MR == 0, ]
  
  
  # Save results
  mpm_robust = c()
  for ( j in 1:2 ) {
    mpm_robust = c( mpm_robust, 
                    list( list( value = NA, 
                                estimate = NA,
                                pval = NA ) ) )
  }
  names( mpm_robust ) = names( all_dtbf )
  
  # Design matrix
  IV = c( 'CI', 'C_SRvB', 'TI', 'T_SRvB' )
  
  # Loop over data sets
  for ( i in 1:2 ) {
    
    output = model_fit( all_dtbf[[i]], IV );
    
    p_more_zero = apply( output$post$beta, 2, post_prob, greater = T )
    p_less_zero = apply( output$post$beta, 2, post_prob, greater = F )
    
    mpm_robust[[i]]$value = findMode( output$post$beta[,2] )
    sel1 = all_dtbf[[i]]$ITL == 'Competitor' & 
      all_dtbf[[i]]$SRL == 'Selective retrieval'
    sel2 = all_dtbf[[i]]$ITL == 'Competitor' & 
      all_dtbf[[i]]$SRL == 'Baseline'
    mpm_robust[[i]]$estimate = mean(output$post$theta[,sel2]) - 
      mean(output$post$theta[,sel1] )
    mpm_robust[[i]]$pval = pmin( p_more_zero[2], p_less_zero[2] )
    
  }
  
  setwd( 'Data' )
  save( mpm_robust, file = 'MPM_robust_results.RData' )
  setwd( orig_dir )
  
}

###
###
###
# Lookup - 08

###
### Multinomial process model for 3rd replication
###
# Lookup - 08

if ( runCode[5] ) {
  
  # Load in replication data
  load( 'Data/Wimber_rep_3.RData' )
  fr = extract_current_data( allData )
  # Create intercept term
  fr$Int = 1
  
  # Create the data-sets of interest
  all_dtbf = c()
  for ( i in 1:2 ){
    all_dtbf = c( all_dtbf, list(NULL) )
  }
  names( all_dtbf ) = c( 'MaE', 'MR' )
  
  # Create data sets
  all_dtbf$MaE = fr
  all_dtbf$MaE$Ac = fr$Acm
  sel = fr$RT < 3.5
  all_dtbf$MR = fr[sel,]
  
  # Save results
  mpm_rep3 = c()
  for ( j in 1:2 ) {
    mpm_rep3 = c( mpm_rep3, 
                    list( list( value = rep( NA, 3 ), 
                                pval = rep( NA, 3 ) ) ) )
  }
  names( mpm_rep3 ) = names( all_dtbf )
  
  # Design matrix
  IV = c( 'Int', 'FIT', 'FSR', 'ITxSR' )
  
  # Loop over data sets
  for ( i in 1:2 ) {
    
    output = model_fit( all_dtbf[[i]], IV );
    
    p_more_zero = apply( output$post$beta, 2, post_prob, greater = T )
    p_less_zero = apply( output$post$beta, 2, post_prob, greater = F )
    
    mpm_rep3[[i]]$value = apply( output$post$beta, 2, findMode )[-1]
    mpm_rep3[[i]]$pval = pmin( p_more_zero, p_less_zero )[-1]
    
  }
  
  setwd( 'Data' )
  save( mpm_rep3, file = 'MPM_rep3_results.RData' )
  setwd( orig_dir )
  
}

###
### Multinomial process model for 2nd replication
###
# Lookup - 08

if ( runCode[6] ) {
  
  # Load in previous replication data
  load( 'Data/Combined_replication_data.RData' )
  fr = extract_current_data( prevRepData, type = 2 )
  # Create intercept term
  fr$Int = 1
  
  # Create the data-sets of interest
  all_dtbf = c()
  for ( i in 1:2 ){
    all_dtbf = c( all_dtbf, list(NULL) )
  }
  names( all_dtbf ) = c( 'MaE', 'MR' )
  
  # Create data sets
  all_dtbf$MaE = fr
  all_dtbf$MaE$Ac = fr$Acm
  sel = fr$RT < 3.5
  all_dtbf$MR = fr[sel,]
  
  # Save results
  mpm_rep2 = c()
  for ( j in 1:2 ) {
    mpm_rep2 = c( mpm_rep2, 
                  list( list( value = rep( NA, 3 ), 
                              pval = rep( NA, 3 ) ) ) )
  }
  names( mpm_rep2 ) = names( all_dtbf )
  
  # Design matrix
  IV = c( 'Int', 'FIT', 'FSR', 'ITxSR' )
  
  # Loop over data sets
  for ( i in 1:2 ) {
    
    output = model_fit( all_dtbf[[i]], IV );
    
    p_more_zero = apply( output$post$beta, 2, post_prob, greater = T )
    p_less_zero = apply( output$post$beta, 2, post_prob, greater = F )
    
    mpm_rep2[[i]]$value = apply( output$post$beta, 2, findMode )[-1]
    mpm_rep2[[i]]$pval = pmin( p_more_zero, p_less_zero )[-1]
    
  }
  
  setwd( 'Data' )
  save( mpm_rep2, file = 'MPM_rep2_results.RData' )
  setwd( orig_dir )
  
}

setwd( orig_dir )