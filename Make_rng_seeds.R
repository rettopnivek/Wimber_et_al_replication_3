
# Generate set of random seeds
Ntotal = 96
rng_seeds = round( runif(Ntotal)*10000 )

# Ensure no replicates
while( length( unique(rng_seeds) ) != Ntotal ) {
  rng_seeds = round( runif(Ntotal)*10000 )
}

# Save results in text file
write.table( cbind( rng_seeds ), file = 'RNG_seeds.txt',
             col.names = F, row.names = F, quote = F )

