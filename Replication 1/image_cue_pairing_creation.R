# Create temporary image files (F = face, O = object, S = scene)
category = rep(c('F_','O_','S_'),each=48) # Set the categories
f_name = rep('Image_',144)
ext = '.jpg'
images = paste(category,f_name,1:144,ext,sep='') # Combine the strings to
                                                 # make the file names
ctgry = rep(c('F','O','S'),each=48) # Clean indicators for the categories
# Create the jpeg files
for (i in 1:144) {
	jpeg(filename = images[i])
	plot(c(0,1),c(0,1),type='n',bty='o',xlab='',ylab='',xaxt='n',yaxt='n')
	text(.5,.5,ctgry[i],cex=9)
	dev.off()
}

# Create temporary image lure files
f_name = rep('Lure_',144)
lures = paste(category,f_name,1:144,ext,sep='') # Combine the strings to
                                                 # make the file names
lr = rep(c('Fl','Ol','Sl'),each=48) # Clean indicators for the categories
# Create the jpeg files
for (i in 1:144) {
	jpeg(filename = lures[i])
	plot(c(0,1),c(0,1),type='n',bty='o',xlab='',ylab='',xaxt='n',yaxt='n')
	text(.5,.5,lr[i],cex=9)
	dev.off()
}

# Create labels and cue words
lbl = paste(ctgry,rep('Image',144),1:144)
cues = paste('cue word',1:72)
# Create text files with the labels and cue words
write.table(cbind(lbl),file='Labels.txt',quote=F,row.names=F,col.names=F)
write.table(cbind(cues),file='Cue_words.txt',quote=F,row.names=F,col.names=F)

# Create the word picture association indices 

# First, randomly divide each category into 
# first and second sets
sel = sample(1:48,48)
first = sel[1:24]
second = sel[25:48]

sel = sample(1:48,48)
first = c(first,sel[1:24]+48)
second = c(second,sel[25:48]+48)

sel = sample(1:48,48)
first = c(first,sel[1:24]+96)
second = c(second,sel[25:48]+96)

# Next reorganize the second set so that the
# categories don't match between sets
sel = c(25:48,49:72)
sel = sample(sel,48)
temp = sel[1:24]
sel = sel[25:48]
rem = sel[sel<49]

sel = sel[sel>48]
l = 24-length(sel)
sel2 = sample(1:24,24)
sel = sample(c(sel,sel2[1:l]),24)
temp = c(temp,sel)
sel=c(rem,sel2[(l+1):24])

sel = sample(sel,24)
temp = c(temp,sel)
cond = c(rep(1,54),rep(2,18))

pairings = cbind(sample(1:72,72),first,second[temp],cond)



# Rename files provided by Maria Wimber
new_file_names = paste(rep('F_',96),
											 rep(c('Image_','Lure_'),48),
											 rep(1:48,each=2),
											 rep('.bmp',96),sep='')
file.rename(dir(),new_file_names)

new_file_names = paste(rep('O_',96),
											 rep(c('Image_','Lure_'),48),
											 rep(1:48+48,each=2),
											 rep('.bmp',96),sep='')
file.rename(dir(),new_file_names)

new_file_names = paste(rep('S_',96),
											 rep(c('Image_','Lure_'),48),
											 rep(1:48+96,each=2),
											 rep('.bmp',96),sep='')
file.rename(dir(),new_file_names)

#
orig_dr = getwd()
setwd("finalSelectionFaces/finalSelection/")
temp = dir()[seq(1,96,2)]
setwd(orig_dr)
write.table(cbind(temp),file='Labels2.txt',quote=F,row.names=F,col.names=F)
setwd("finalSelectionObjects/finalSelectionObjects/")
temp = dir()[seq(1,96,2)]
setwd(orig_dr)
write.table(cbind(temp),file='Labels2.txt',quote=F,row.names=F,col.names=F,
						append=T)
setwd("finalSelectionScenes/finalSelection/")
temp = dir()[seq(1,96,2)]
setwd(orig_dr)
write.table(cbind(temp),file='Labels2.txt',quote=F,row.names=F,col.names=F,
						append=T)


of = dir()
nf=unlist(strsplit(of,split='.bmp'))
tmp = strsplit(nf,split='e_')
newf = rep(' ',288)
for (i in 1:288) {
	inc = 0
	if (i > 96 & i < 193) inc = 48
	if (i > 192 & i < 289) inc = 96
	newf[i] = paste(tmp[[i]][1],'e_',as.numeric(tmp[[i]][2])+inc,'.bmp',sep='')
}

# Reduce the number of stimuli provided Maria Wimber
labels = read.table('Labels.txt',header=F,sep=',')
setwd('Images')
old.names = dir()
type = rep(c(1,2,1,2,1,2),each=48)
category = rep( 1:3,each=48*2)

image.Numbers = numeric(length(old.names))
for (i in 1:length(old.names)) {
  image.Numbers[i] = as.numeric(strsplit(strsplit(old.names[i],'_')[[1]][3],'.bmp'))
}
# ind = 1:288
ind = 1:48

# Randomly select a subset of images and lures to keep, and update the labels
# appropriately

ctgry = 1
ctgry = 2
ctgry = 3

new.labels = c()
ctgry.name = c('F','O','S')

# temp = ind[type==1 & category==ctgry]
temp = 1:48
select = sample(temp,24)
new.images = old.names[type==1 & category==ctgry][select]
new.lures = old.names[type==2 & category==ctgry][select]
new.labels = c(new.labels, 
               as.character(labels[ image.Numbers[type==2 & category==ctgry][select],1]))

# Remove the other files
rm.images = old.names[type==1 & category==ctgry][-select]
rm.lures = old.names[type==2 & category==ctgry][-select]
file.remove(rm.images);
file.remove(rm.lures)

# Rename the files
tmp.names = paste('image',1:24,'.bmp',sep='')
act.name = paste(ctgry.name[ctgry],'_Image_',1:24+(24*(ctgry-1)),'.bmp',sep='')
file.rename(new.images,tmp.names)
file.rename(tmp.names,act.name)

tmp.names = paste('lure',1:24,'.bmp',sep='')
act.name = paste(ctgry.name[ctgry],'_Lure_',1:24+(24*(ctgry-1)),'.bmp',sep='')
file.rename(new.lures,tmp.names)
file.rename(tmp.names,act.name)

setwd(orig_dr)
write.table(cbind(new.labels),file='Labels2.txt',quote=F,row.names=F,col.names=F,
            append=F)