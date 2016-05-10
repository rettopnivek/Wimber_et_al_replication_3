#----------------------------------------------#
# Script to extract data from experiment files #
# Kevin Potter                                 #
# Updated 05/09/2016                           #
#----------------------------------------------#

# Clear workspace
rm(list = ls())

# Save current working directory
orig_dir = getwd()

# Change directory to subject files
setwd('..') # Move up a folder
# setwd('Subjects')
setwd('Simulated_subjects')

# Get all file names
allFiles = dir()

# Create an empty variable to store data
allData = c()

# Loop over files
for ( i in 1:length( allFiles ) ) {
  
  fname = allFiles[i]
  
  tmp = strsplit( fname, split = '_' )[[1]]
  if ( tmp[1] == 'Subject' ) {
    if ( length( tmp ) == 2 & length( grep('+csv',tmp[2]) ) == 1 ) {
      curData = read.table( file=fname, header = T, sep=',' )
      allData = rbind( allData, curData )
    }
  }
  
}

# Extract sample size
N = length( unique( allData$Subject ) )

# Save data
setwd(orig_dir)
# save( allData, N, file = 'All_subject_data.RData' )
save( allData, N, file = 'All_subject_data_Sim.RData' )