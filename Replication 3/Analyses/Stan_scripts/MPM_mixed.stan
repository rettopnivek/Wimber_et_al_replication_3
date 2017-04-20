data {
  int No; // Number of observations
  int Ns; // Number of subjects
  int Ni; // Number of items
  int K; // Number of fixed effects
  int subjIndex[ No ]; // Index for subjects
  int itemIndex[ No ]; // Index for items
  int Y[ No ]; // Accuracy ( 0 = error, 1 = correct )
  matrix[ No, K ] X; // Design matrix
  vector[ No ] beta_offsets; // Offsets for fixed effects
  int prior_type; // Indicate
  matrix[ K + 2, 2 ] Priors; // Values for priors
}
parameters {
  vector[ K ] beta; // Fixed effects
  vector[ Ns ] alpha_s_raw; // Standardized subject-level effects
  vector[ Ni ] alpha_i_raw; // Standardized item-level effects
  real<lower=0.0> sigma_s; // Standard deviation for subject effects
  real<lower=0.0> sigma_i; // Standard deviation for item effects
}
transformed parameters {
  real theta[ No ]; // P( Correct )
  
  // Local block
  {
    // Subject/item effects in raw scale
    vector[ Ns ] alpha_s;
    vector[ Ni ] alpha_i;
    // Predicted value for logit of P(Recall)
    vector[ No ] alpha;
    real eta; // P(Recall)
    
    // Re-scale subject/item effects based on standard deviation
    alpha_s = alpha_s_raw * sigma_s;
    alpha_i = alpha_i_raw * sigma_i;
    
    alpha = beta_offsets + X * beta + 
            alpha_s[ subjIndex ] + alpha_i[ itemIndex ];
    
    // Loop over observations
    for ( n in 1:No ) {
      // Bound between 0 and 1
      eta = inv_logit( alpha[n] );
      // Correct for guessing
      theta[n] = eta + 0.5 * ( 1.0 - eta );
    }
  }
}
model {
  
  // Priors
  
  // Informative
  if ( prior_type == 1 ) {
    for ( k in 1:K ) {
      beta[k] ~ normal( Priors[ k, 1 ], Priors[ k, 2 ] );
    }
  }
  sigma_s ~ gamma( Priors[ K + 1, 1 ], Priors[ K + 1, 2 ] );
  sigma_i ~ gamma( Priors[ K + 2, 1 ], Priors[ K + 2, 2 ] );
  
  // Hierarchy
  alpha_s_raw ~ normal( 0.0, 1.0 );
  alpha_i_raw ~ normal( 0.0, 1.0 );
  
  // Likelihood
  Y ~ bernoulli( theta );
}
generated quantities {
  // Save matrix of log-likelihood values
  vector[ No ] logLik;
  
  for ( no in 1:No ) {
    logLik[no] = bernoulli_lpmf( Y[no] | theta[no] );
  }
}

