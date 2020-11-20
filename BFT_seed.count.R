## Demo script for birdsfoot trefoil seed counting and dimension quantification
## This script will demonstrate:
#1) seperating foreground (seed) from background
#2) count seeds
#3) measure average seed area
#4) measure variance surrounding average seed area
## Date: 11.18.2020
## Author: Garett Heineck
##
## Remeber to UPDATE R!
R.Version()[c('version.string','nickname')]
## Hey! Is your version 4.0.2 "Taking Off Again?"

## Required packages - may need installation first
## PLEASE UPDATE R (if you have not done so recently)
library(tidyverse)
library(readxl)
library(dplyr)
library(jpeg)
library(EBImage) #needs to be downloaded from (https://www.bioconductor.org/packages/release/bioc/html/EBImage.html)***
#
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("EBImage")
#
library(randomForest)
library(stringr)
library(ggplot2)
library(cowplot)
##############
##############
##############


##############
##############
##############
## Required file paths
#parent directory folder named "BFT_seed.ct_8.5.20"***
#To run this you need a folder named "BFT_seed.ct_8.5.20"***
img_BFT_seed.demo.8.5.20<- "/Users/heine237/Documents/GitHub/BFT_seed.ct_11.18.20" #NOTE: change to your own file path***
#************************#
## Creating folders to store all image output
#NOTE: the folder "original_img" is where the images you want to process need to be***
folders <- c("training data_8.5.20", 
             "results", 
             "original_img", 
             "crop_img",
             "seed_tray_img",
             "S1_foreground_classify",
             "S2_foreground_overlay",
             "S3_foreground_watershed" 
             ) #adding in the correct folder list*** 
for (i in 1:length(folders))  { 
  dir.create(paste(img_BFT_seed.demo.8.5.20,folders[i], sep="/")) 
}
#NOTE: you may see Warning messages, that is ok as default setting will not overwrite existing folders***
#NOTE: make sure the "original_img" folder has images in it***
##############
##############
##############


##############
##############
##############
## Read in datasheet.
## The data contains information about each image captured in the originalimages folder
## THis may be visual observations of the seeds or pedigree information
## You can open the excel spreadsheet to read the column descriptions
#************************#
BFT_seed.dat<- read_excel(paste(img_BFT_seed.demo.8.5.20, "results", "BFT_seed.ct_data.xlsx", sep = "/"), 
                              skip = 6, 
                              na = ".")
summary(BFT_seed.dat)
##############
##############
##############


##############
##############
##############
## This step crops the original images.
## Cropping your images is important to reduce processing time. 
## In this case the color card is not very useful so it will be removed
top<-   0.30 #proportion taken off the top***
bottom<-0.30 #proportion taken off the bottom***
left<-  0.20 #proportion taken off the left***
right<- 0.20 #proportion taken off the right***
#************************#
original_img.path<- list.files(path=paste(img_BFT_seed.demo.8.5.20, "original_img",sep = "/"), full.names = T)
original_img.name<- list.files(path=paste(img_BFT_seed.demo.8.5.20, "original_img",sep = "/"), full.names = F)
folder_crop_img<-   (paste(img_BFT_seed.demo.8.5.20,"crop_img",sep = "/"))
#************************#
for(i in 1:length(original_img.path)){
  temp1<- readImage(original_img.path[i])
  if(dim(temp1)[1]<dim(temp1)[2]){
    left.set<- round(min(c(dim(temp1)[1],dim(temp1)[2])) * left)
    right.set<- min(c(dim(temp1)[1],dim(temp1)[2])) - round(min(c(dim(temp1)[1],dim(temp1)[2])) * right) 
    top.set<- round(max(c(dim(temp1)[1],dim(temp1)[2])) * top)
    bottom.set<- round(max(c(dim(temp1)[1],dim(temp1)[2])) - (max(c(dim(temp1)[1],dim(temp1)[2])) * bottom))
    temp3<-temp1[left.set:right.set,
                 top.set:bottom.set,]
  } else {
    temp2<- rotate(temp1, 90)
    dim(temp2)
    left.set<- round(min(c(dim(temp1)[1],dim(temp1)[2])) * left)
    right.set<- min(c(dim(temp1)[1],dim(temp1)[2])) - round(min(c(dim(temp1)[1],dim(temp1)[2])) * right) 
    top.set<- round(max(c(dim(temp1)[1],dim(temp1)[2])) * top)
    bottom.set<- round(max(c(dim(temp1)[1],dim(temp1)[2])) - (max(c(dim(temp1)[1],dim(temp1)[2])) * bottom))
    temp3<-temp2[left.set:right.set,
                 top.set:bottom.set,]
  }
  writeImage(temp3, paste(folder_crop_img, "/", original_img.name[i], sep = ""), quality = 100)
}
##############
##############
##############


##############
##############
##############
## We now need to load the training data.
## Information on how to create training data can be found in the TRAINING DATA HELP GUIDE.
## SEE: https://github.com/GarettHeineck/making-training-data
## Collectively the training mixes are called a palette and are saved in the training palette folder.
## The palette has many RGB mixes, each helps in predicting different features within the image.
#************************#
palette_directory_BFT.seed<- paste(img_BFT_seed.demo.8.5.20, "training data_8.5.20",sep = "/") #file path where mixes are saved***
#************************#
mixes_names<- list.files(path=palette_directory_BFT.seed,pattern="*.csv",full.names = FALSE) #name directory for what is in the palette folder***
mixes_path<- list.files(path=palette_directory_BFT.seed, pattern="*.csv", full.names = TRUE) #path directory for what is in the palette folder***
training.palette_BFT.seed<- data.frame()
#this for() loop will systematically re arrange and condense each mix file in the training palette folder***
#the reason I am doing this is to allow the script to update itself upon adding additional mixes***
for (i in 1:length(mixes_path)){
  temp_mix<- read.csv(mixes_path[i])
  temp_mix$band<- NA
  temp_mix$band[1:which(temp_mix$Label == "Red")] <- "Red"
  temp_mix$band[(which(temp_mix$Label == "Red")+1):which(temp_mix$Label == "Green")] <- "Green"
  temp_mix$band[(which(temp_mix$Label == "Green")+1):which(temp_mix$Label == "Blue")] <- "Blue"
  temp<- split(temp_mix, temp_mix$band)
  temp2<- do.call("cbind", split(temp_mix, temp_mix$band))
  image<- temp2$Blue.Label[i]
  mix<- mixes_names[i]
  temp3<- data.frame(mix, image, x=temp2[5]$Blue.X, y=temp2[6]$Blue.Y, red=temp2[18]$Red.Mean, green=temp2[11]$Green.Mean, blue=temp2[4]$Blue.Mean)
  training.palette_BFT.seed<- rbind(training.palette_BFT.seed, temp3) 
}
summary(training.palette_BFT.seed) #summarizing the training palette***
count(training.palette_BFT.seed, mix) %>% View #counting observations in each mix of the training palette*** 
##############
##############
##############


##############
##############
##############
## We will now make the random forest models to detect different features in the cropped images.
## A different random forest model will be needed for each feature.
## Here were are detecting three features: 
# 1) the seed tray + seeds from background
# 2) the seeds from seed tray
#************************#
#model to seperate seed tray + seeds from background***
palette_selection_seed.tray<- filter(training.palette_BFT.seed)
palette_selection_seed.tray$classification<- c(rep(0, len=800), rep(1, len=900)) #selecting the mixes (1=foreground)***
palette_selection_seed.tray %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_BFT_seed.tray<- randomForest(classification~(red+green+blue),data=palette_selection_seed.tray, ntree=30,mtry = 1,importance=TRUE)
print(rfm_BFT_seed.tray)
plot(rfm_BFT_seed.tray) #ntree is set to 90, that looks about right, could probably go down to 40***
importance(rfm_BFT_seed.tray) 

#************************#
#model to seperate seeds from seed tray***
palette_selection_seed.all<- filter(training.palette_BFT.seed, !grepl("null_01", mix) & !grepl("border", mix))
palette_selection_seed.all$classification<- c(rep(0, len=300), rep(1, len=600)) #selecting the mixes (1=foreground)***
palette_selection_seed.all %>% group_by(mix) %>% summarise(avg=mean(classification)) 
rfm_BFT_seed.all<- randomForest(classification~(red+green+blue),data=palette_selection_seed.all, ntree=30,mtry = 1,importance=TRUE)
print(rfm_BFT_seed.all)
plot(rfm_BFT_seed.all) #ntree is set to 90, that looks about right, could probably go down to 40***
importance(rfm_BFT_seed.all) 
#************************#
##############
##############
##############


##############
##############
##############
## Running the image processing loop.
## This is a really large loop that is broken up into 4 sections.
#************************#
#each path is for an image***
folder_crop_img<-  (paste(img_BFT_seed.demo.8.5.20,"crop_img",sep = "/"))
#************************#
folder_seed_tray<- (paste(img_BFT_seed.demo.8.5.20,"seed_tray_img",sep = "/"))
folder_S1<- (paste(img_BFT_seed.demo.8.5.20,"S1_foreground_classify",sep = "/"))
folder_S2<- (paste(img_BFT_seed.demo.8.5.20,"S2_foreground_overlay",sep = "/"))
folder_S3<-  (paste(img_BFT_seed.demo.8.5.20,"S3_foreground_watershed",sep = "/"))
#************************#
#check to make sure all the cropped image show up***
paths_cropped_BFT.seed<- list.files(path=folder_crop_img,full.names = TRUE)
names_cropped_BFT.seed<- list.files(path=folder_crop_img,full.names = FALSE) 
#create a data frome to collect numeric output from the analysis***
img.stats_BFT.seed.demo_8.5.20<- data.frame()
#************************#
#************************#
#************************#
for (i in 1:length(paths_cropped_BFT.seed)) {
  #************************#
  #section 1***
  img.01<- readImage(paths_cropped_BFT.seed[i])
  coor<- as.data.frame(as.table(img.01[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.01[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.01[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.01[,,3]))[3]
  img.dat.01<- cbind(coor, red, green, blue)
  colnames(img.dat.01)<- c("y","x","red","green","blue")
  img.dat.01$classify<- predict(rfm_BFT_seed.tray, img.dat.01)
  img.dat.01$thresh<- ifelse(img.dat.01$classify>0.80, img.dat.01$classify, 0)  #Set threshold to 80%, NOTE: I am retaining the non-binary probabilities***
  img.02<- matrix(img.dat.01$thresh, nrow=nrow(img.01), ncol=ncol(img.01))
  filter = makeBrush(size = 21, shape = 'disc') 
  img.03<- closing(img.02,makeBrush(11, shape='disc'))
  label_img.03<- bwlabel(img.03)
  obj.interest<- data.frame(table(label_img.03)) %>%
    filter(!row_number()== 1) %>%
    filter(Freq == max(Freq)) %>%
    mutate(label_img.03 = as.numeric(label_img.03))
   select.obj = rmObjects(label_img.03, c(1:(obj.interest[[1]]-2),(obj.interest[[1]]):max(label_img.03)))
  replace<-which(select.obj<0.8)
  picred<-img.01[,,1] # we have to operate on each matrix seperately in jpeg array. 
  picblue<-img.01[,,2]
  picgreen<-img.01[,,3]
  picred[replace]<-0 # replace all the pixel values in the cropped, orginal image with 0 at the coordinates
  picblue[replace]<-0
  picgreen[replace]<-0
  img.04<- array(c(picred, picblue, picgreen), dim=dim(img.01))
  writeJPEG(img.04, paste(folder_seed_tray, "/", "seed_tray_", names_cropped_BFT.seed[i], sep = ""), quality = 1)
  #************************#
  #section 2***
  #************************#
  paths_seed_tray_BFT.seed<- list.files(path=folder_seed_tray,full.names = TRUE)
  img.04<- readImage(paths_seed_tray_BFT.seed[i])
  coor<- as.data.frame(as.table(img.04[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.04[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.04[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.04[,,3]))[3]
  img.dat.02<- cbind(coor, red, green, blue)
  colnames(img.dat.02)<- c("y","x","red","green","blue")
  img.dat.02$classify<- predict(rfm_BFT_seed.all, img.dat.02)
  img.dat.02$thresh<- ifelse(img.dat.02$classify>0.95, img.dat.02$classify, 0)  #Set threshold to 30%, NOTE: I am retaining the non-binary probabilities***
  img.05<- matrix(img.dat.02$thresh, nrow=nrow(img.04), ncol=ncol(img.04))
  img.06<- gblur(img.05, sigma = 2)
  replace<-which(img.06<0.5)
  leafpicred<-img.04[,,1] # we have to operate on each matrix seperately in jpeg array. 
  leafpicblue<-img.04[,,2]
  leafpicgreen<-img.04[,,3]
  leafpicred[replace]<-0 # replace all the pixel values in the cropped, orginal image with 0 at the coordinates
  leafpicblue[replace]<-0
  leafpicgreen[replace]<-0
  img.07<- array(c(leafpicred, leafpicblue, leafpicgreen), dim=dim(img.04))
  display(img.07)
  writeJPEG(img.05, paste(folder_S1, "/", "S1_", names_cropped_BFT.seed[i], sep = ""), quality = 1)
  writeJPEG(img.07, paste(folder_S2, "/", "S2_", names_cropped_BFT.seed[i], sep = ""), quality = 1)
  #************************#
  #section 3***
  #************************#
  paths_S2<- list.files(path=folder_S2,full.names = TRUE)
  img.08<- readImage(paths_S2[i])
  coor<- as.data.frame(as.table(img.08[,,1]))[1:2]
  red<- 255*as.data.frame(as.table(img.08[,,1]))[3]
  green<- 255*as.data.frame(as.table(img.08[,,2]))[3]
  blue<- 255*as.data.frame(as.table(img.08[,,3]))[3]
  img.dat.03<- cbind(coor, red, green, blue)
  colnames(img.dat.03)<- c("y","x","red","green","blue")
  img.dat.03$classify<- predict(rfm_BFT_seed.all, img.dat.03)
  img.dat.03$thresh<- ifelse(img.dat.03$classify>0.70, img.dat.03$classify, 0)  #Set threshold to 30%, NOTE: I am retaining the non-binary probabilities***
  img.09<- matrix(img.dat.03$thresh, nrow=nrow(img.08), ncol=ncol(img.08))
  image_watershed.01<- watershed(distmap(img.09), tolerance=.4, ext=1)
  writeJPEG(colorLabels(image_watershed.01), paste(folder_S3, "/", "S3_", names_cropped_BFT.seed[i], sep = ""), quality = 1)
  #************************#
  #section 4***
  #************************#
  seed.featr<- data.frame(computeFeatures.shape(image_watershed.01)) 
  
  write.stats<- data.frame(img.ID=              str_sub(names_cropped_BFT.seed[i]), #unique image ID***
                           seed.ct=             length(seed.featr$s.area),
                           mean.seed.area=      mean(seed.featr$s.area),
                           sd.seed.area=        sd(seed.featr$s.area),
                           mean.perimeter=      mean(seed.featr$s.perimeter),
                           sd.perimeter=        sd(seed.featr$s.perimeter),
                           mean.radius=         mean(seed.featr$s.radius.mean),
                           sd.radius=           sd(seed.featr$s.radius.mean)
                           )
  
  img.stats_BFT.seed.demo_8.5.20<-rbind(img.stats_BFT.seed.demo_8.5.20, write.stats) 
}

#writing the output statistics to the parent directory folder***
write.csv(img.stats_IWG.seed.demo_4.6.19, paste(img_BFT_seed.demo.8.5.20, "results","img.stats_BFT.seed.demo_8.5.20.csv", sep = "/"))
##############
##############
##############


##############
##############
##############
## Data analysis
dat<- left_join(potato_seed.dat, img.stats_potato.seed.demo_8.5.20, by="img.ID")

lm1<- summary(lm(manual_seed.ct~seed.ct, data = dat))

ggplot(dat, aes(x=seed.ct, y=manual_seed.ct)) +
  geom_point(color="red4", size=3)+
  geom_smooth(method = "lm",
              color="black")+
  labs(x="predicted seed count", y="manual seed count",
       title = paste("R^2=", round(lm1$r.squared,4), "slope=", round(lm1[[4]][2],2)))+
  theme(axis.title = element_text(size=18),
        axis.text = element_text(size=16))
