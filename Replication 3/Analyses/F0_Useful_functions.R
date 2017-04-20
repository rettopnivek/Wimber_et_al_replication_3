#--------------------#
# Useful functions   #
# Kevin Potter       #
# Updated 04/17/2017 #
#--------------------#

# Load in useful packages

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
# install.packages( 'rstanarm' )
library( rstanarm )

# Lookup - 01:  S
# Lookup - 02:  violinPlot
# Lookup - 03:  my_RM_ANOVA
# Lookup - 04:  resp_cat
# Lookup - 05:  Extract_SR
# Lookup - 06:  ds_perf
# Lookup - 07:  sim_diff_scores
# Lookup - 08:  est_ui
# Lookup - 09:  quick_ui
# Lookup - 10:  post_prob
# Lookup - 11:  model_fit
# Lookup - 12:  post_sim
# Lookup - 13:  quick_agg
# Lookup - 14:  diff_pst
# Lookup - 15:  diff_of_diff
# Lookup - 16:  main_eff_pst

# Lookup - 01
S = function(x) {
  # Purpose:
  # Calculates descriptive statistics for a sample of values
  # Arguments:
  # x - A vector of values
  # Returns:
  # The mean, standard error of the mean, the inter-quartile range,
  # and the min/max of the sample.
  
  out = c( mean(x), sem(x), diff( quantile(x,prob=c(.25,.75)) ), 
           min(x), max(x) )
  
  return( out )
}

# Lookup - 02
violinPlot = function( x, pos, crit = NULL, scaleH = .5, 
                       type = 'greater', ... ) {
  # Purpose:
  # Adds a violin distribution to an already existing 
  # plot.
  # Arguments:
  # x      - A vector of numerical values
  # pos    - The position on the x-axis to draw the violin
  #          distribution
  # crit   - The value past which to calculate the posterior 
  #          probability.
  # scaleH - The width of the distribution
  # type   - Whether to calculate the probability 'greater' than 
  #          or 'less' than the critical value
  
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

my_RM_ANOVA = function(data, display = T ) {
  # Purpose:
  # Calculates a repeated measures ANOVA (1 or 2 factors) 
  # given a data frame of a specific format
  # Arguments:
  # data - A data frame that meets the following specifications:
  #          1) Has 3 or 4 columns ( for 1 or 2 factors respectively)
  #          2) Columns are labeled as 'Subject', 'Group.1', 
  #             'Group.2', and 'x'
  # display - A logical value, indicating if the ANOVA table should 
  #           be displayed
  # Output:
  # Returns the F-values for the main effects and interaction,
  # and the associated p-values as a list.
  # Also prints the standard table for reporting ANOVAs
  
  # Check if data is in format of a data-frame
  if ( !is.data.frame(data) ) 
    stop( 'Data must be formatted as a data-frame' )
  
  # Determine if there are 1 or 2 factors
  Nvar = ncol( data )
  # If there are too many or too few columns
  if ( Nvar != 3 & Nvar != 4 ) 
    stop( 'Structure of data is inappropriate for this function \n Only 1 or 2 within-subject factors are admissable ' )
  
  # One factor repeated measures ANOVA
  if ( Nvar == 3 ) {
    
    # Sample size
    Ns = length( unique( data$Subject ) ) # Number of subjects
    Na = length( unique( data$Group.1 ) ) # Number of levels
    No = length( data$x ) # Total number of observations
    
    # Calculate the means
    grand.mean = mean( data$x )
    xbar.A = aggregate( data$x, list( data$Group.1 ), mean )$x
    xbar.S = aggregate( data$x, list( data$Subject ), mean )$x
    
    # Calculate the sums of squares
    SS.A = Ns * sum( ( xbar.A - grand.mean )^2 )
    SS.S = Na * sum( ( xbar.S - grand.mean )^2 )
    SS.T = sum( ( data$x - grand.mean )^2 )
    SS.E = SS.T - SS.A - SS.S
    
    # Calculate the degrees of freedom
    df.A = Na - 1
    df.S = Ns - 1
    df.T = No - 1
    df.E = df.T - df.A - df.S
    
    f.value = ( SS.A/df.A ) / ( SS.E / df.E )
    p.value = pf( f.value, df.A, df.E, lower.tail = F )
    
    # Determine string length of output
    strlength = c(
      length( strsplit( as.character( round(SS.A,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.E,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( df.A ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( df.E ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(f.value,2) ), 
                        split = '' )[[1]] )
    )
    
    sep = c()
    for (i in 1:length( strlength ) ) {
      if ( strlength[i] > 7 ) sep = c(sep, 2) else sep = c( sep, 1 )
    }
    
    # Create and print an output table
    string = paste('Source SS', paste( rep('\t',
                                           max(sep[1:2])),
                                       collapse='' ),
                   'df', paste( rep('\t',
                                    max(sep[3:4])),
                                collapse='' ),
                   'F', paste( rep('\t',sep[5]),
                               collapse='' ),
                   'p-value\n',sep='')
    cat(string)
    string = paste('A\t', round(SS.A,2),
                   paste( rep('\t',
                              sep[1]),
                          collapse='' ),
                   round(df.A,2), paste( rep('\t',
                                             sep[3]),
                                         collapse='' ),
                   round(f.value,2), paste( rep('\t',sep[5]),
                                            collapse='' ),
                   round(p.value,2),'\n',sep='')
    cat(string)
    string = paste('E\t', round(SS.E,2),
                   paste( rep('\t',
                              sep[2]),
                          collapse='' ),
                   round(df.E,2), '\n',sep='')
    cat(string)
    
    # Output
    out = list( F = f.value, p = p.value )
  }
  
  # Two factor repeated measures ANOVA
  if ( Nvar == 4 ) {
    
    # Sample size
    Ns = length( unique( data$Subject ) ) # Number of subjects
    Na = length( unique( data$Group.1 ) ) # Number of levels for 1st variable
    Nb = length( unique( data$Group.2 ) ) # Number of levels for 2nd variable
    No = length( data$x ) # Total number of observations
    
    # Calculate the means
    grand.mean = mean( data$x )
    xbar.A = aggregate( data$x, list( data$Group.1 ), mean )$x
    xbar.B = aggregate( data$x, list( data$Group.2 ), mean )$x
    xbar.S = aggregate( data$x, list( data$Subject ), mean )$x
    xbar.AB = aggregate( data$x, list( data$Group.2, data$Group.1 ), mean )$x
    xbar.AS = aggregate( data$x, list( data$Subject, data$Group.1 ), mean )$x
    xbar.BS = aggregate( data$x, list( data$Subject, data$Group.2 ), mean )$x
    
    # Calculate the sums of squares
    SS.T = sum( ( data$x - grand.mean )^2 )
    SS.A = Ns * Nb * sum( ( xbar.A - grand.mean )^2 )
    SS.B = Ns * Na * sum( ( xbar.B - grand.mean )^2 )
    SS.S = Na * Nb * sum( ( xbar.S - grand.mean )^2 )
    
    SS.AB = numeric( Na*Nb )
    inc = 1
    for (i in 1:Na) {
      for (j in 1:Nb) {
        SS.AB[ inc ] = ( xbar.AB[inc] - xbar.A[i] - xbar.B[j] + grand.mean )^2
        inc = inc + 1
      }
    }
    SS.AB = Ns * sum( SS.AB )
    
    SS.AS = numeric( Na*Ns )
    inc = 1
    for (i in 1:Na) {
      for (j in 1:Ns) {
        SS.AS[ inc ] = ( xbar.AS[inc] - xbar.A[i] - xbar.S[j] + grand.mean )^2
        inc = inc + 1
      }
    }
    SS.AS = Nb * sum( SS.AS )
    
    SS.BS = numeric( Nb*Ns )
    inc = 1
    for (i in 1:Nb) {
      for (j in 1:Ns) {
        SS.BS[ inc ] = ( xbar.BS[inc] - xbar.B[i] - xbar.S[j] + grand.mean )^2
        inc = inc + 1
      }
    }
    SS.BS = Na * sum( SS.BS )
    
    # tmp = aggregate( data$x, list( data$Group.1, data$Group.2 ), mean )$x
    # tmp = rep( tmp, Ns )
    # SS.ABS = sum( ( data$x - tmp )^2 )
    
    SS.ABS = SS.T - SS.S - SS.A - SS.B - SS.AB - SS.AS - SS.BS
    
    df.total = No - 1
    df.A = Na - 1
    df.B = Nb - 1
    df.S = Ns - 1
    df.AB = df.A*df.B
    df.AS = df.A*df.S
    df.BS = df.B*df.S
    df.ABS = df.A*df.B*df.S
    
    f.value.A = ( SS.A / df.A ) / ( SS.AS / df.AS )
    p.value.A = pf( f.value.A, df.A, df.AS, lower.tail = F )
    
    f.value.B = ( SS.B / df.B ) / ( SS.BS / df.BS )
    p.value.B = pf( f.value.B, df.B, df.BS, lower.tail = F )
    
    f.value.AB = ( SS.AB / df.AB ) / ( SS.ABS / df.ABS )
    p.value.AB = pf( f.value.AB, df.AB, df.ABS, lower.tail = F )
    
    out = list( F = c( A = f.value.A,
                       B = f.value.B,
                       AB = f.value.AB ),
                p = c( A = p.value.A,
                       B = p.value.B,
                       AB = p.value.AB ) )
    
    # Determine string length of output
    strlength = c(
      length( strsplit( as.character( round(SS.A,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.AS,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.B,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.BS,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.AB,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(SS.ABS,2) ), 
                        split = '' )[[1]] ), # 1 - 6
      length( strsplit( as.character( df.A ), 
                        split = '' )[[1]] ), 
      length( strsplit( as.character( df.AS ), 
                        split = '' )[[1]] ), 
      length( strsplit( as.character( df.B ), 
                        split = '' )[[1]] ), 
      length( strsplit( as.character( df.BS ), 
                        split = '' )[[1]] ), 
      length( strsplit( as.character( df.AB ), 
                        split = '' )[[1]] ), 
      length( strsplit( as.character( df.ABS ), 
                        split = '' )[[1]] ), # 7 - 12
      length( strsplit( as.character( round(f.value.A,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(f.value.B,2) ), 
                        split = '' )[[1]] ),
      length( strsplit( as.character( round(f.value.AB,2) ), 
                        split = '' )[[1]] ) # 13 - 15
    )
    
    sep = c()
    for (i in 1:length( strlength ) ) {
      if ( strlength[i] > 7 ) sep = c(sep, 2) else sep = c( sep, 1 )
    }
    
    # Create and print an output table
    if ( display ) {
      
      string = paste('Source SS', paste( rep('\t',
                                             max(sep[1:6])),
                                         collapse='' ),
                     'df', paste( rep('\t',
                                      max(sep[7:12])),
                                  collapse='' ),
                     'F', paste( rep('\t',max(sep[13:15])),
                                 collapse='' ),
                     'p-value\n',sep='')
      cat(string)
      
      str1 = c('A\t','AS\t',
               'B\t','BS\t',
               'AB\t','ABS\t')
      str2 = round(
        c( SS.A, SS.AS, SS.B, SS.BS, SS.AB, SS.ABS ),
        2 )
      str3 = c( df.A, df.AS, df.B, df.BS, df.AB, df.ABS )
      str4 = round( c( f.value.A, f.value.B, f.value.AB ), 2 )
      str4 = as.character( str4 )
      str4 = c( str4[1], ' ', str4[2], ' ', str4[3], ' ' )
      str5 = c( paste( rep('\t',sep[13]),collapse='' ),
                ' ', paste( rep('\t',sep[14]),collapse='' ),
                ' ',paste( rep('\t',sep[15]),collapse='' ),
                ' ' )
      str6 = c( as.character( round( p.value.A, 2 ) ),
                ' ',
                as.character( round( p.value.B, 2 ) ),
                ' ',
                as.character( round( p.value.AB, 2 ) ),
                ' ' )
      
      for (i in 1:6) {
        
        string = paste(str1[i], str2[i],
                       paste( rep('\t',
                                  sep[i]),
                              collapse='' ),
                       str3[i], paste( rep('\t',
                                           sep[i+6]),
                                       collapse='' ),
                       str4[i], str5[i],str6[i],'\n',sep='')
        cat(string)
        
      }
      
      
    }
    
  }
  
  # Return the results
  return( out )
}

# Lookup - 04
resp_cat = function(x) {
  # Purpose:
  # A function to categorize responses in selective-retrieval 
  # phase.
  # Arguments:
  # x - A vector of categorical responses (either 1, 2, 3, or 4)
  # Returns:
  # The proportion of times each category was selected.
  
  out = c(
    sum( x == 1 ),
    sum( x == 2 ),
    sum( x == 3 ),
    sum( x == 4 ) )
  out = out/length(x)
  
  return( out )
}

# Lookup - 05
Extract_SR = function( cD ) {
  # Purpose:
  # Categorizes a subject's responses as correctly identifying 
  # targets, mistakenly choosing competitors, committing an 
  # error, or selecting 'unknown'.
  # Arguments:
  # cD = The data frame specific to the selective-retrieval phase
  # Returns:
  # A list with the picture catories for targets, competitors, 
  # errors, and unknowns, and the choice subjects actually 
  # made.
  
  # Category for target images
  Targets = floor(cD$Category)
  # Category for competitor images
  Competitors = round( 10*(cD$Category - Targets) )
  # The category of the unrelated error
  Errors = apply( cbind( Targets, Competitors ), 1, 
                  function(x) {
                    tar = 1:3 %in% x[1];
                    com = 1:3 %in% x[2];
                    err = 1 - (tar+com);
                    return( (1:3)[err==1] ) } )
  # If unknown, subjects could pick a 4th response category
  Unknown = rep( 4, nrow( cD ) )
  
  # Determine a subject's choice based on categories
  Choice = rep( NA, nrow( cD ) )
  Choice[ cD$Resp == Targets ] = 1
  Choice[ cD$Resp == Competitors ] = 2
  Choice[ cD$Resp == Errors ] = 3
  Choice[ cD$Resp == Unknown ] = 4
  # Set missing data to be 'Unknown' as well'
  Choice[ cD$Resp == 0 ] = 4
  
  out = list(
    Targets = Targets,
    Competitors = Competitors,
    Errors = Errors,
    Unknown = Unknown,
    Choice = Choice,
    Subject = cD$Subject )
  
  return( out )
}

# Lookup - 06
ds_perf = function( dat, sel, IV ) {
  # Purpose:
  # Computes difference scores between conditions for 
  # performance/accuracy data (Ac).
  # Arguments:
  # dat - A data frame with a vector of accuracy values (Ac) 
  #       and a set of condition indices
  # sel - A matrix with 2 rows:
  #       Row 1 = A vector of the levels of the variables used 
  #               to select the first group of scores
  #       Row 1 = A vector of the levels of the variables used 
  #               to select the second group of scores
  # IV  - A character vector, giving the names of the variables to 
  #       aggregate performance over
  # Returns:
  # A vector of difference scores.
  
  cvrt = c()
  for ( i in 1:length(IV) ) {
    cvrt = c( cvrt, list( dat[,IV[i]] ) )
  }
  names( cvrt ) = IV
  
  perf = aggregate( dat$Ac, cvrt, mean )
  colnames( perf ) = c( IV, 'P' )
  
  tmp = matrix( NA, nrow( perf ), ncol( sel ) )
  for ( i in 1:ncol(sel)) {
    tmp[,i] = as.numeric( perf[,IV[i]] == sel[1,i] )
  }
  sel1 = rowSums( tmp ) == ncol( sel )
  
  for ( i in 1:ncol(sel)) {
    tmp[,i] = as.numeric( perf[,IV[i]] == sel[2,i] )
  }
  sel2 = rowSums( tmp ) == ncol( sel )
  
  out = perf$P[ sel1 ] - perf$P[ sel2 ]
  
  return( out )
}

# Lookup - 07
sim_diff_scores = function( V, N = c( 18, 54 ), nRep = 50000) {
  # Purpose:
  # A function to generate a Monte Carlo sample of difference 
  # scores computed by simulating a multinomial process model.
  # The model assumes that subjects correctly recall an item 
  # with probability V, otherwise they will correctly guess the 
  # item 50% of the time with probability (1-V). Difference 
  # scores are then computed between proportion correct for 
  # the baseline and selective-retrieval conditions, which only 
  # differ in the number of trials.
  # Arguments:
  # V    - The probability of recalling the correct image
  # N    - The number of trials for the baseline and selective-retrieval
  #        conditions
  # nRep - The number of Monte Carlo samples to generate
  # Returns:
  # A vector of difference scores.
  
  # Baseline
  recall = rbinom( nRep, N[1], V )
  guess = rbinom( nRep, N[1] - recall, .5 )
  observed = recall + guess
  prop_correct_B = observed/N[1]
  
  # Selective-retrieval
  recall = rbinom( nRep, N[2], V )
  guess = rbinom( nRep, N[2] - recall, .5 )
  observed = recall + guess
  prop_correct_SR = observed/N[2]
  
  # Estimate of forgetting effect
  diff_score = prop_correct_B - prop_correct_SR
  
  return( diff_score )  
}

# Lookup - 08
est_ui = function(v, alpha) {
  # Purpose:
  # Given a value for P(Recall), simulates difference scores 
  # between simulated conditions from a multinomial processing 
  # model, then estimates a desired confidence interval.
  # Arguments:
  # v     - value for P(Recall)
  # alpha - Width of confidence interval
  # Returns:
  # The upper and lower bounds for the confidence interval 
  # around the difference scores.
  
  interval = numeric(2)
  interval[1] = ( 1 - alpha )/2
  interval[2] = interval[1] + alpha
  
  diff_scores = sim_diff_scores( v )
  out = quantile( diff_scores, prob = interval )
  
  return( out )
}

# Lookup - 09
quick_ui = function( x, alpha = .95 ) {
  # Purpose:
  # A convenience function for computing uncertainty intervals.
  # Arguments:
  # x     - A vector of data
  # alpha - The width of the uncertainty interval
  # Returns:
  # The lower and upper quantiles for the specified width.
  
  interval = numeric(2)
  interval[1] = ( 1 - alpha )/2
  interval[2] = interval[1] + alpha
  
  return( quantile( x, interval ) )
}

# Lookup - 10
post_prob = function( x, greater = T, crit = 0 ) {
  # Purpose:
  # A convenience function for computing posterior probabilities.
  # Arguments:
  # x       - A vector of data
  # greater - Logical; if true, computes the proportion that falls 
  #           above the critical value
  # crit    - The value used to compute how much data falls above 
  #           or below
  # Returns:
  # The proportion of data more extreme than the critical value.
  
  if ( greater ) 
    out = mean( x > crit ) else
      out = mean( x < crit )
    
    return( out )
}

# Lookup - 11
model_fit = function( dat, IV, 
                      Priors = NULL,
                      beta_offsets = NULL,
                      control = list(
                        warm = 250,
                        niter = 1250,
                        chains = 8,
                        seed = runif(1) * 1000
                      ) ) {
  # Purpose:
  # Function to fit a multinomial process model with 
  # item and subject effects
  # Arguments:
  # dat          - a data frame with the dependent and independent 
  #                variables (requires a variable 'S' for subjects,
  #                a variable 'IN' for items, and a variable 'Ac' 
  #                for the dependent variable)
  # IV           - A character vector indicating which independent 
  #                variables should be included as fixed effects
  # Priors       - An optional matrix for the prior values (if
  #                NULL, improper priors for the fixed effects 
  #                are used)
  # beta_offsets - An optional vector of pre-defined constants to 
  #                apply to the fixed effects
  # control      - A named list of control variables for Stan 
  #                estimation, with...
  #                warm = number of warmup iterations
  #                niter = number of samples to draw
  #                chain = number of chains to run
  #                seed = RNG seed (for reproducibility)
  # Returns:
  # A named list consisting of...
  #   post = a list of the posterior samples;
  #   stanDat = the input to Stan;
  #   Rhat = the Gelman-Rubin convergence statistics;
  #   n_eff = the effective number of samples per parameter;
  #   totSampSize = the total number of samples drawn from 
  #                 the posterior.
  
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

# Lookup - 12
# Purpose:
# C++ function to generate posterior simulations.
# Arguments:
# post    - A list of samples from the posterior for every parameter
#           (sub-output from the 'model_fit' function)
# stanDat - The input for Stan (sub-output from the 'model_fit' 
#           function)
# Returns:
# A matrix where each row is a set of simulated observations for 
# each joint posterior sample
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

# Lookup - 13
# Purpose:
# C++ function for computing proportion correct over unique 
# combinations of levels for different factors.
# Arguments:
# Y            - A matrix of simulated data per row (0 = error, 
#                1 = correct)
# UniqueCombos - An index of which unique combination of levels to 
#                which each observation belongs
# Returns:
# A matrix given the proportion correct per combination of levels 
# for every row.
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

# Lookup - 14
diff_pst = function( post, ind = c(1,2), ... ) {
  # Purpose:
  # Computes the difference between two vectors of posterior 
  # samples given a matrix of values.
  # Arguments:
  # post    - A matrix of posterior samples
  # ind     - The two columns over which to compute the difference
  #           (first minus second)
  # ...     - Additional arguments for the 'post_prob' function
  # Returns:
  # The mean of the difference and the associated posterior 
  # p-value.
  
  est = post[,ind[1]] - post[,ind[2]]
  return( c( mean(est), post_prob( est, ... ) ) )
}

# Lookup - 15
diff_of_diff = function( post, 
                         ind = list( c(1,2), c(3,4) ),
                         ... ) {
  # Purpose:
  # Computes the difference of differences for a set of four 
  # posteriors samples given a matrix of values.
  # Arguments:
  # post    - A matrix of posterior samples
  # ind     - A list giving the pairs of columns over which to 
  #           compute the differences ( (1st - 2nd) - (3rd - 4th) )
  # ...     - Additional arguments for the 'post_prob' function
  # Returns:
  # The mean of the difference of differences and the 
  # associated posterior p-value.
  
  est = ( post[,ind[[1]][1]] - post[,ind[[1]][2]] ) - 
    ( post[,ind[[2]][1]] - post[,ind[[2]][2]] )
  
  return( c( mean(est), post_prob( est, ... ) ) )
}

# Lookup - 16
main_eff_pst = function( post, 
                         ind = list( c(1,2), c(3,4) ),
                         ... ) {
  # Purpose:
  # Computes the difference of the averages for a set of four 
  # posteriors samples given a matrix of values.
  # Arguments:
  # post    - A matrix of posterior samples
  # ind     - A list giving the pairs of columns over which to 
  #           compute the differences ( avg(1st,2nd) - avg(3rd,4th) )
  # ...     - Additional arguments for the 'post_prob' function
  # Returns:
  # The mean of the difference of averages and the 
  # associated posterior p-value.
  
  
  est = ( post[,ind[[1]][1]] + post[,ind[[1]][2]] )/2 - 
    ( post[,ind[[2]][1]] + post[,ind[[2]][2]] )/2
  
  return( c( mean(est), post_prob( est, ... ) ) )
}