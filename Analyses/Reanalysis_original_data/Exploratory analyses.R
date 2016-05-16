# Clear the workspace
rm(list=ls())

orig_dir = getwd()

runCode = c(T,F)

# Define a basic logistic (or sigmoidal) function
logistic = function(x) 1/(1+exp(-x))
# Define a basic logit function
logit = function(p) log( p/(1-p) )

# Load in data
load( 'Original_all_data.RData' )
OriginalRecMem = OriginalAllData[ OriginalAllData$Cond == 6, ]

# Determine the sample size
N = length( unique( OriginalRecMem$Subject ) )

# data and covariates
d = OriginalRecMem # Create a data frame for data to be fitted
d$Y = d$Accuracy # Dependent variable
d$Y[ is.na(d$Y) ] = 0 # Set missing responses to incorrects
d$IT = as.factor( d$ImageType ) # Image type (1 = target, 2 = competitor)
d$SR = as.factor( 1 - d$Baseline ) # Selective retrieval ( 1 = yes, 0 = no )
d$S = as.factor( d$Subject )
d$I = as.factor( d$ImageNum )
d$Trial = rep(1,nrow(d)) # Nuisance parameter for bernoulli distribution
# RIF effect
d$RIF = 0; d$RIF[ d$IT == 2 & d$SR == 1 ] = 1

# Covariate for practice accuracy
sel = OriginalAllData$Cond == 2 | OriginalAllData$Cond == 3 | 
  OriginalAllData$Cond == 4
dT = OriginalAllData[ sel, ]

tmp = aggregate( dT$Accuracy, list( dT$ImageNum, dT$Subject ), 
                 function(x) { 
                   out = mean(x);
                   out = out - .5;
                   # out = sum(x)
                   # if (length(x)==1) out=out-.5;
                   # if (length(x)==2) out=out-1;
                   return( out*2 ) }  )

cv = numeric( nrow( tmp ) )
for ( n in 1:N ) {
  sel = d$Subject == n
  img = d$ImageNum[sel]
  ord = order( img )
  cv[sel][ord] = tmp$x[sel]
}
d$PT = cv # Save covariate for practice performance

# Clean up workspace
rm( sel, img, ord, n, tmp, cv, dT )

# Covariate for selective retrieval
sel = OriginalAllData$Cond == 5
dSR = OriginalAllData[ sel, ]

# Category for target images
Targets = floor(dSR$Category)
# Category for competitor images
Competitors = round( (dSR$Category - Targets)*10 )
# The category of the unrelated error
Errors = apply( cbind( Targets, Competitors ), 1, 
                function(x) { tar = 1:3 %in% x[1]; com = 1:3 %in% x[2];
                err = 1 - (tar+com); (1:3)[err==1] } )
# If unknown, subjects could pick a 4th response category
Unknown = rep( 4, nrow( dSR ) )

# Determine a subject's choice based on categories
dSR$Choice = 0
dSR$Choice[ dSR$Resp == Targets ] = 1
dSR$Choice[ dSR$Resp == Competitors ] = 2
dSR$Choice[ dSR$Resp == Errors ] = 3
dSR$Choice[ dSR$Resp == Unknown ] = 4

imgTar = floor( dSR$ImageNum )
imgCom = round( (dSR$ImageNum - imgTar)*1000 )

tmp = aggregate( dSR$Choice == 1, list( 
  imgTar, dSR$Subject ), function(x) sum(x)/length(x) )

sel = d$SR == 0 & d$IT == 1
tmp2 = aggregate( rep(0,sum(sel)), 
                  list( d$ImageNum[sel], d$S[sel] ), sum )

tmp3 = aggregate( dSR$Choice == 2, list( 
  imgCom, dSR$Subject ), function(x) sum(x)/length(x) )

sel = d$SR == 0 & d$IT == 2
tmp4 = aggregate( rep(0,sum(sel)), 
                  list( d$ImageNum[sel], d$S[sel] ), sum )

cv = rbind( tmp, tmp2, tmp3, tmp4 )
o = order( cv[,2], cv[,1] )
cv = cv[o,]

cv2 = numeric( nrow(d) )
for ( n in 1:N ) {
  sel = d$Subject == n
  img = d$ImageNum[sel]
  ord = order( img )
  cv2[sel][ord] = cv$x[sel]
}
d$SR_cr = cv2 # Save covariate for practice performance

tmp = aggregate( dSR$Choice == 2, list( dSR$Subject ), mean )
# tmp$x = scale( tmp$x ) Scale and center covariate
tmp$x = tmp$x/max(tmp$x) # Normalize covariate
d$SR_a = rep( tmp$x, each = 144 )

rm( sel, Targets, Competitors, Errors, Unknown )
rm( imgTar, imgCom, tmp, tmp2, tmp3, tmp4 )
rm( cv, o, cv2, img, ord, n )

if (runCode[1]) {
  
  # Load in package for mixed modeling
  library(lme4)
  
  Null_model = glmer( Y ~ (1|S) + (1|I), data = d, 
                      family = binomial(link='logit'), weights = Trial )
  
  RIF_model = glmer( Y ~ RIF + (1|S) + (1|I), data = d, 
                     family = binomial(link='logit'), weights = Trial )
  
  # d$SR_cr_2 = d$SR_cr*(d$ImageType-1) - .5
  d$SR_b = d$SR_a*as.numeric( d$IT == 2 & d$SR == 1 )
  RIF_model_2 = glmer( Y ~ SR_b + (1|S) + (1|I), data = d, 
                       family = binomial(link='logit'), weights = Trial )
  
  #PT_model = glmer( Y ~ PT + SR_cr + (1|S) + (1|I), data = d,
  #                  family = binomial(link='logit'), weights = Trial )
  
  d$P_1 = d$PT*(1-(d$ImageType - 1))
  d$P_2 = d$PT*(d$ImageType - 1)
  
  PT_model = glmer( Y ~ P_1 + P_2 + (1|S) + (1|I), data = d,
                    family = binomial(link='logit'), weights = Trial )
  
  
  comp = anova( Null_model, RIF_model, PT_model )
  dAIC = comp$AIC - min( comp$AIC )
  weights = exp( -.5*dAIC )
  weights = weights/sum(weights)
  names(weights) = c('Null','RIF','PT')
  print( round( weights, 2 ) )
  
  obs = aggregate( d$Y, list( d$IT, d$SR, d$S ), mean )
  colnames( obs ) = c('IT','SR','S','P')
  
  # Increment for items
  inc_I = numeric( nrow(d) )
  inc = 1
  for ( i in sort( unique( d$ImageNum ) ) ) {
    inc_I[ d$ImageNum == i ] = inc
    inc = inc + 1
  }
  rm( inc, i )
  
  prm = coef( RIF_model )
  modelPred = d$RIF*prm$I[1,2] + 
    prm$S[ d$Subject, 1 ] + 
    ( prm$I[ inc_I, 1 ] - summary( RIF_model )$coefficients[1,1] )
  mp_rif = aggregate( logistic( modelPred ), 
                      list( d$IT, d$SR, d$S ), mean )
  
  prm = coef( RIF_model_2 )
  #d$RIF*prm$I[1,2] +
  modelPred = d$SR_b*prm$I[1,2] + 
    prm$S[ d$Subject, 1 ] + 
    ( prm$I[ inc_I, 1 ] - summary( RIF_model )$coefficients[1,1] )
  mp_rif2 = aggregate( logistic( modelPred ), 
                      list( d$IT, d$SR, d$S ), mean )
  
  
  prm = coef( PT_model )
  modelPred = d$P_1*prm$I[1,2] + 
    d$P_2*prm$I[1,3] + 
    prm$S[ d$Subject, 1 ] + 
    (prm$I[ inc_I, 1 ] - summary( PT_model )$coefficients[1,1] )
  mp_pt = aggregate( logistic( modelPred ), 
                     list( d$IT, d$SR, d$S ), mean )

  prm = coef( Null_model )
  modelPred = prm$S[ d$Subject, 1 ] + 
    (prm$I[ inc_I, 1 ] - summary( PT_model )$coefficients[1,1] )
  mp_null = aggregate( logistic( modelPred ), 
                     list( d$IT, d$SR, d$S ), mean )
  
  x11(width=12,height=6)
  par( mfrow=c(2,6) )
  for ( n in 1:12 ) {
    
    plot( c(.5,4.5),c(.5,1),type='n',bty='n',
          xaxt='n',ylab = ' ', xlab = ' ',
          main = paste( 'Subject', n ) )
    sel = obs$S == n
    #pd0 = mp_null$x[sel]
    pd1 = mp_rif$x[sel]
    pd2 = mp_pt$x[sel]
    pd3 = mp_rif2$x[sel]
    ob = obs$P[sel]
    #lines( 1:4, pd0[c(3,1,4,2)], col = 'grey' )
    lines( 1:4, pd1[c(3,1,4,2)], col = 'blue' )
    lines( 1:4, pd2[c(3,1,4,2)], col = 'red' )
    lines( 1:4, pd3[c(3,1,4,2)], col = 'green' )
    points( 1:4, ob[c(3,1,4,2)], pch = 21, bg = 'grey', cex = 1.5 )
    if (n>6) axis(1,1:4,c('T-SR','T-BS','C-SR','C-BS'),tick=F)
  }
  
  x11(width=12,height=6)
  par( mfrow=c(2,6) )
  for ( n in 13:24 ) {
    
    plot( c(.5,4.5),c(.5,1),type='n',bty='n',
          xaxt='n',ylab = ' ', xlab = ' ',
          main = paste( 'Subject', n ) )
    sel = obs$S == n
    #pd0 = mp_null$x[sel]
    pd1 = mp_rif$x[sel]
    pd2 = mp_pt$x[sel]
    pd3 = mp_rif2$x[sel]
    ob = obs$P[sel]
    #lines( 1:4, pd0[c(3,1,4,2)], col = 'grey' )
    lines( 1:4, pd1[c(3,1,4,2)], col = 'blue' )
    lines( 1:4, pd2[c(3,1,4,2)], col = 'red' )
    lines( 1:4, pd3[c(3,1,4,2)], col = 'green' )
    points( 1:4, ob[c(3,1,4,2)], pch = 21, bg = 'grey', cex = 1.5 )
    if (n>18) axis(1,1:4,c('T-SR','T-BS','C-SR','C-BS'),tick=F)
  }
  
}

if (runCode[2]) {
  
  library( rstan )
  
  model_script = "
  data {
    int Ns;
    int Ni;
    int No;
    int Nc;
    int Y[ No ];
    int indS[ No ];
    int indI[ No ];
    int indC[ No ];
    matrix[ Nc+2, 4 ] Priors;
  }
  parameters {
    real betaS[Ns];
    real betaI[Ni];
    matrix[Ns,Nc] betaC;
    real<lower=0> sigS;
    real<lower=0> sigI;
    real mu_betaC[Nc];
    real<lower=0> sig_betaC[Nc];
  }
  transformed parameters {
    real alpha[No];
    
    for (no in 1:No) {
      alpha[no] <- betaC[ indS[no], indC[no] ] + 
        betaS[ indS[ no ] ] + 
        betaI[ indI[ no ] ];
    }
  }
  model {

    // Priors
    sigS ~ gamma(Priors[1,1],Priors[1,2]);
    sigI ~ gamma(Priors[2,1],Priors[2,2]);
    for (nc in 1:Nc) {
      mu_betaC[nc] ~ normal( Priors[nc+2,1], Priors[nc+2,2] );
      sig_betaC[nc] ~ gamma( Priors[nc+2,3], Priors[nc+2,4] );
    }
    
    // Hierarchy
    betaS ~ normal(0,sigS);
    betaI ~ normal(0,sigI);
    for ( nc in 1:Nc) {
      col(betaC,nc) ~ normal( mu_betaC[nc], sig_betaC[nc] );
    }
    
    // Likelihood
    Y ~ bernoulli_logit(alpha);
  }
  generated quantities {
    // Variable declaration
    real<lower=0,upper=1> theta[No];
    
    for (no in 1:No) {
      theta[no] <- inv_logit( alpha[no] );
    }
  }
  "
  
  # Save as .stan file
  setwd('Stan_scripts')
  writeChar( model_script, "Test_model.stan" )
  
  indI = numeric( nrow( d ) )
  inc = 1
  for ( i in unique( d$ImageNum ) ) {
    indI[ d$ImageNum == i ] = inc
    inc = inc + 1
  }
  
  indC = numeric( nrow( d ) )
  indC[ d$IT == 1 & d$SR == 1 ] = 1
  indC[ d$IT == 1 & d$SR == 0 ] = 2
  indC[ d$IT == 2 & d$SR == 1 ] = 3
  indC[ d$IT == 2 & d$SR == 0 ] = 4
  
  Priors = matrix( 0, 6, 4 )
  Priors[1,1:2] = c(2,4)
  Priors[2,1:2] = c(2,4)
  Priors[3,] = c(logit(.75),logit(.75)-logit(.7),2,4)
  Priors[4,] = c(logit(.75),logit(.75)-logit(.7),2,4)
  Priors[5,] = c(logit(.75),logit(.75)-logit(.7),2,4)
  Priors[6,] = c(logit(.75),logit(.75)-logit(.7),2,4)
  
  stan_dat = list(
    Ns = N,
    Ni = length( unique( indI ) ),
    No = nrow(d),
    Nc = 4,
    Y = d$Y,
    indS = d$Subject,
    indI = indI,
    indC = indC,
    Priors = Priors
  )
  
  burn = 500 # Burn-in
  niter = 1250 # Number of samples to approximate posterior
  
  startTime = Sys.time() # To assess run-time
  fit = stan(file = 'Test_model.stan', data = stan_dat, 
             warmup = burn, iter = burn+niter, 
             chains = 8 )
  
  post = extract(fit)
  # Report run time
  runTime = Sys.time() - startTime
  print( runTime )
  rm( startTime, runTime )
  
  # Observed
  obs = aggregate( d$Y, list( d$IT, d$SR, d$S ), mean )
  colnames( obs ) = c('IT','SR','S','P')
  
  # Collapse model predictions
  modelPred = apply( post$theta, 2, mean )
  mp = aggregate( theta, list( d$IT, d$SR, d$S ), mean )
  colnames( mp ) = c('IT','SR','S','P')
  
  x11(width=12,height=6)
  par( mfrow=c(2,6) )
  for ( n in 1:12 ) {
    
    plot( c(.5,4.5),c(.5,1),type='n',bty='n',
          xaxt='n',ylab = ' ', xlab = ' ',
          main = paste( 'Subject', n ) )
    sel = obs$S == n
    pd = mp$x[sel]
    ob = obs$P[sel]
    lines( 1:4, pd[c(3,1,4,2)] , col = 'blue' )
    points( 1:4, ob[c(3,1,4,2)], pch = 21, bg = 'grey', cex = 1.5 )
    if (n>6) axis(1,1:4,c('T-SR','T-BS','C-SR','C-BS'),tick=F)
  }
  
  x11(width=12,height=6)
  par( mfrow=c(2,6) )
  for ( n in 13:24 ) {
    
    plot( c(.5,4.5),c(.5,1),type='n',bty='n',
          xaxt='n',ylab = ' ', xlab = ' ',
          main = paste( 'Subject', n ) )
    sel = obs$S == n
    pd = mp$x[sel]
    ob = obs$P[sel]
    lines( 1:4, pd[c(3,1,4,2)] , col = 'blue' )
    points( 1:4, ob[c(3,1,4,2)], pch = 21, bg = 'grey', cex = 1.5 )
    if (n>18) axis(1,1:4,c('T-SR','T-BS','C-SR','C-BS'),tick=F)
  }
  
  x11()
  par( mfrow=c(2,2) )
  ttl = c('T-SR','T-BS','C-SR','C-BS')
  for ( i in 1:4 ) {
    hist( post$mu_betaC[,i], col = 'grey', border = 'white',
          freq = F, xlab = ttl[i], main = ' ', bty = 'l' )
  }
  mtext('Group-level means',side=3,line=-2,outer=T)
  
  x11()
  par( mfrow=c(2,2) )
  ttl = c('T-SR','T-BS','C-SR','C-BS')
  for ( i in 1:4 ) {
    hist( post$sig_betaC[,i], col = 'grey', border = 'white',
          freq = F, xlab = ttl[i], main = ' ', bty = 'l' )
  }
  mtext('Group-level standard deviations',side=3,line=-2,outer=T)
  
  
  
}

setwd( orig_dir )
