#
#
#

# Prior predictive check for selective retrieval task
setwd('Stan_scripts')

model_script = "
data {
  int<lower=1> No; // Total number of observations
  int<lower=1> Ns; // Total number of subjects
  int<lower=2> K; // Number of response categories
  int indS[No]; // Subject index per trial
  matrix[K-1,4] Priors; // Matrix of parameter values for priors
}
model {
}
generated quantities {
  // Variable declarations
  matrix[ Ns, K-1 ] beta_raw; // Subject-level coefficients
  real mu_beta[K-1]; // Group-level means
  real<lower=0> sigma_beta[K-1]; // Group-level standard deviations
  matrix[ Ns, K ] beta; // Coefficients for all categories
  int<lower=1,upper=K> Y[No]; // Simulated responses
  int Targets;
  int Competitors;
  int Errors;
  int Unknown;
  vector[Ns] zeros;
  
  // Generate parameters from priors
  
  for (i in 1:(K-1)) {
    mu_beta[i] <- normal_rng( Priors[i,1], Priors[i,2] );
    sigma_beta[i] <- gamma_rng( Priors[i,3], Priors[i,4] );
  }
  
  // Hierarchy
  for (i in 1:3) {
    for (ns in 1:Ns) {
      beta_raw[ns,i] <- normal_rng( mu_beta[i], sigma_beta[i] );
    }
  }
  zeros <- rep_vector(0, Ns);
  beta <- append_col( beta_raw, zeros );
  
  // Simulate observations
  Targets <- 0;
  Competitors <- 0;
  Errors <- 0;
  Unknown <- 0;
  for ( no in 1:No ) {
    Y[no] <- categorical_rng( softmax( to_vector( beta[ indS[no] ] ) ) );
    # Category frequencies
    if ( Y[no] == 1 ) Targets <- Targets + 1;
    if ( Y[no] == 2 ) Competitors <- Competitors + 1;
    if ( Y[no] == 3 ) Errors <- Errors + 1;
    if ( Y[no] == 4 ) Unknown <- Unknown + 1;
  }
  
}
"

writeChar( model_script, "SR_prior_check.stan" )

Ns = 48
No = 54*Ns
indS = rep( 1:Ns, each = 54 )

mu = reverseSoftmax( c( .747, .091, .025, .137 ), 
                     restrict = c( F, F, F, T ) )

Priors = cbind( c( 1.700, -.409, -1.700 ),
                c( .3, .3, .3 ),
                c( 2, 2, 2 ),
                c( 8, 8, 8 ) )

stan_dat = list(
  No = No,
  Ns = Ns,
  K = 4,
  Priors = Priors
)

niter = 1250
chains = 8

fit = stan(
  file = 'SR_prior_check.stan',
  data = stan_dat,
  iter = niter,
  chains = chains,
  algorithm = 'Fixed_param'
)

post = extract( fit )

x11();
layout( cbind( c(1,3),c(2,4) ) )
hist( post$Targets/No, col = 'grey', border = 'white',
      bty = 'l', xlab = 'P(Target)', freq = T )
legend('topright', as.character( round( mean( post$Targets/No ), 2 ) ), 
       bty = 'n' )
hist( post$Competitors/No, col = 'grey', border = 'white',
      bty = 'l', xlab = 'P(Competitors)', freq = T )
legend('topright', as.character( round( mean( post$Competitors/No ), 2 ) ), 
       bty = 'n' )
hist( post$Errors/No, col = 'grey', border = 'white',
      bty = 'l', xlab = 'P(Errors)', freq = T )
legend('topright', as.character( round( mean( post$Errors/No ), 2 ) ), 
       bty = 'n' )
hist( post$Unknown/No, col = 'grey', border = 'white',
      bty = 'l', xlab = 'P(Unknown)', freq = T )
legend('topright', as.character( round( mean( post$Unknown/No ), 2 ) ), 
       bty = 'n' )

setwd( orig_dir )