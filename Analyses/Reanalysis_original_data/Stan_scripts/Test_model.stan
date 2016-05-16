
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
   