#---------------------------------#
# Wimber et al (2015) replication #
# Pilot data analyses             #
# 4/11/2015                       #
#---------------------------------#

###--------------###
### Read in data ###
###--------------###

# Save current working directory
orig_dir = getwd()

# Set working directory to folder with subjects
setwd('..')
setwd('Pilot_data_April_10_2015')

# Number of subjects
N = length(dir())/2 # Divide by 2 because of matlab files

# File names for .csv files
f_names = paste('Subject_',1:6,'.csv',sep='')

# Read in data and convert to data frame
all.dat = c()
for (n in 1:N) {
	tmp = read.table(f_names[n],header=T,sep=',')
	all.dat = rbind(all.dat, tmp[-1,])
}
all.dat = cbind( rep(1:6,each=305), all.dat)
colnames(all.dat) = c('Subject',colnames(tmp))
all.dat = as.data.frame(all.dat)

# Read in stimulus order data
setwd(orig_dir);

# File names for .csv files
f_names = paste('S',1:6,'_stimulus_order.csv',sep='')

# Read in data
all.order = c()
for (n in 1:N) {
	tmp = read.table(f_names[1],header=T,sep=',')
	all.order = c( all.order, list(tmp) )
}

# Clean up workspace
rm(n); rm(f_names);

# Forthcoming

ind = all.dat$Cond==6 & all.dat$Subject==1 # Select subset of trials

all.dat[ind,c('imageIDT','Correct','Response','RT','Accuracy')]

ind = all.dat$Cond==1 # Select subset of trials
aggregate(all.dat$Accuracy,list(all.dat$Subject,all.dat$Cond),mean)

# For condition 6 (final recognition test) indicate whether
# image was assigned to baseline condition (2) or not (1)
ind = all.dat$Cond==6 # Select subset of trials
tmp = as.character(all.dat$imageIDT[ind]) # Convert to string
# Extract the image file name and isolate its unique number
imNum = matrix(0,length(tmp),3)
for (i in 1:length(tmp)) {
	v1 = strsplit(tmp[i],split='_')
	v2 = as.numeric( strsplit(v1[[1]][3],split='.bmp') )
	imNum[i,2] = v2
}
rm(v1); rm(v2); rm(tmp); rm(i)
imNum[,1] = all.dat$Subject[ind]

# Link up image with its baseline status
# Loop through subjects
for (n in 1:N) {
	# Extract baseline status
	ord = cbind( all.order[[n]]$numImage, all.order[[n]]$Base )
	beg = 36*(n-1)
	for (i in 1:nrow(ord)) {
		
		for (j in 1:36) {
			if (imNum[beg+j,2] == ord[i,1]) imNum[beg+j,3]=ord[i,2]
		}
		
	}
}


