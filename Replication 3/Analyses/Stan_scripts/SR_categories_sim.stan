data {
  int<lower=1> No; // Total number of observations
  int<lower=1> Ns; // Total number of subjects
  int<lower=2> K; // Number of response categories
  int<lower=1> Ni; // Number of MCMC samples drawn
  int indS[No]; // Subject index per trial
  // Posteriors for hierarchical parameters
  real mu_beta[ Ni, K - 1 ]; // Group means
  real<lower=0.0> sigma_beta[ Ni, K - 1 ]; // Group standard deviations
  real beta_raw[ Ni, Ns, K - 1]; // Subject estimates
}
model {
  // Included so model will compile
}
generated quantities {
  real P[Ni,K]; // Proportions for each response category
  matrix[ Ns, K ] beta; // Coefficients for all categories
  vector[ K ]  theta[ Ns ]; // Category probabilities
  
  // Declare local block
  {
    int Y[No]; // Observed responses
    real count [ K ];
    real progress[ 3 ];
    int prog;
    
    // Track progress
    progress[1] = .25 * Ni;
    progress[2] = .5 * Ni;
    progress[3] = .75 * Ni;
    prog = 0;
    
    // Loop over posterior samples
    for ( ni in 1:Ni ) {
      
      if ( ni > progress[1] && prog == 0 ) {
        print( "25% done" );
        prog = 1;
      }
      
      if ( ni > progress[2] && prog == 1 ) {
        print( "50% done" );
        prog = 2;
      }
      
      if ( ni > progress[3] && prog == 2 ) {
        print( "75% done" );
        prog = 3;
      }
      
      // Loop over subjects
      for ( ns in 1:Ns ) {
        
        // Loop over categories
        for ( k in 1:K ) {
          // Simulate subject-level parameters
          if ( k < K ) {
            beta[ ns, k ] = normal_rng( mu_beta[ ni,  k ], 
                                      .5 );
            //beta[ ns, k ] = beta_raw[ ni, ns, k ];
          } else {
            beta[ ns, k ] = 0.0; // Fix 'unknown' category to 0
          }
          // Convert to probabilities
          theta[ ns ] = softmax( to_vector( beta[ ns ] ) );
        }
        
      }
      
      // Initialize count variable
      count[1] = 0.0; count[2] = 0.0;
      count[3] = 0.0; count[4] = 0.0;
      
      // Simulate responses
      for ( no in 1:No ) {
        Y[no] = categorical_rng( theta[ indS[no] ] );
        
        for ( k in 1:K ) {
          if ( Y[no] == k ) count[k] = count[k] + 1.0;
        }
        
      }
      
      // Calculate proportions per category
      for ( k in 1:K ) {
        P[ni,k] = count[k]/No;
      }
      
    }
  }
}

