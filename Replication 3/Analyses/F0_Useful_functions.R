#--------------------#
# Useful functions   #
# Kevin Potter       #
# Updated 12/08/2017 #
#--------------------#

# Lookup - 01:  S
# Lookup - 02:  violinPlot
# Lookup - 03:  my_RM_ANOVA
# Lookup - 04:  resp_cat
# Lookup - 05:  Extract_SR
# Lookup - 06:  quick_pval

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

my_RM_ANOVA = function(data, display = T, all = F ) {
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
  # all     - Outputs the full table.
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
    
    if ( display ) {
      
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
    }
    
    # Output
    if ( !all ) out = list( F = f.value, p = p.value ) else 
      out = list( SS = c( A = SS.A, S = SS.S, E = SS.E ),
                  df = c( A = df.A, S = df.S, E = df.E ),
                  F = f.value, p = p.value )
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
    
    if ( !all ) {
    out = list( F = c( A = f.value.A,
                       B = f.value.B,
                       AB = f.value.AB ),
                p = c( A = p.value.A,
                       B = p.value.B,
                       AB = p.value.AB ) ) 
    } else {
      out = list( SS = c( A = SS.A, B = SS.B, S = SS.S,
                          AB = SS.AB, AS = SS.AS, BS = SS.BS,
                          ABS = SS.ABS ),
                  df = c( A = df.A, B = df.B, S = df.S,
                          AB = df.AB, AS = df.AS, BS = df.BS,
                          ABS = df.ABS ),
                  F = c( A = f.value.A,
                         B = f.value.B,
                         AB = f.value.AB ),
                  p = c( A = p.value.A,
                         B = p.value.B,
                         AB = p.value.AB ) )
    }
    
    
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
quick_pval = function(p) {
  # Purpose:
  # # Function to generate pretty reporting of p-values.
  # Arguments:
  # p - A p-value
  # Returns:
  # A string that gives a cleaned up report of the p-value.
  
  if ( p < .0001 ) {
    pval = 'p < 0.0001'
  } else {
    pval = paste( 'p =', round( p, 3 ) )
  }
  
  return( pval )
}
