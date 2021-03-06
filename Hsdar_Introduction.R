# HSDAR INTRO (following https://cran.r-project.org/web/packages/hsdar/vignettes/Hsdar-intro.pdf)
# 2020-06-18
# Marguerite, Sergio, Tabby, Ifeanyi, Kamal

# load HSDAR
library(hsdar)

# set working directory
setwd("~/Desktop/R/R_programs/Tweedie/Hsdar_Tutorial/Hsdar")

# import data as csv file
dat.sample <- read.csv("ASTRAL_DB_Example_Dataset_HSDAR.csv", header=FALSE)

# subset the information needed to build a speclib
# WARNING!! This will only work if the csv files are structured in the exact same way. 
# structure is one column per wavelength and one row per record (according to Hsdar requirements)
# additionally, we have the wavelength and plot ID info in the .csv files such that:
# row 1: 'wavelength', ....'one column per wavelength...'
# column 1: 'wavelength', 'sampleID_1', 'sampleID_2',.....'sampleID_n'
# row2, column2: contains all reflectance values

# 1. isolate just the wavelengths that appear in the dataset
wavelength.sample <- unlist(dat.sample[1,2:ncol(dat.sample)])

# 2. isolate the plot ID information (this could be further decomposed into site, project, year, etc)
# Ifeanyi has code to split the ID variable into individual elements
# the code from Ifeanyi will work on data frames and so probably need to play around a bit for it to work on this vector
rowid.sample <- as.character(dat.sample[2:nrow(dat.sample),1])

# 2. Extract metadata from data file. 
dat.metadata$id <- rowid.sample

# split the id string into seperate parts
dat.metadata$location <- as.factor(sapply(strsplit(dat.metadata$id,"_"),"[",1))
# project is the second part of each id string
dat.metadata$site <- as.factor(sapply(strsplit(dat.metadata$id,"_"),"[",2))
# plot is the third part
dat.metadata$plot <-as.factor(sapply(strsplit(dat.metadata$id,"_"),"[",3))
# DOY is the fifth part of id
dat.metadata$DOY <- as.numeric(sapply(strsplit(dat.metadata$id,"_"),"[",5))
# time is the sixth part
dat.metadata$time <- as.factor(sapply(strsplit(dat.metadata$id,"_"),"[",6))
# year is the seventh part
dat.metadata$year <- as.numeric(sapply(strsplit(dat.metadata$id,"_"),"[",7))


# 3. extract just the data to use: REFLECTANCE VALUES ONLY!!! 
dat.use <- as.matrix(dat.sample[2:nrow(dat.sample),2:ncol(dat.sample)])

# 4. create speclib
speclib.test <- speclib(dat.use, wavelength.sample)

# 5. add the metadata (plot ID info) to speclib
SI(speclib.test) <- dat.metadata #rowid.sample
names(SI(speclib.test))

# Now you are ready to work with the speclib!!! 

# If the lib contains spectra outside the desired range, it can be clipped like this:
sl.short <- speclib.test[,speclib.test$wavelength<1800]

# play around with some plots (following section 3 in Hsdar Introduction)
par(mfrow = c(2,2))
plot(speclib.test[,speclib.test$wavelength<1800], main="Default Plot")
plot(sl.short, FUN=50,  main="50th and 2nd spectrum of Speclib")
plot(sl.short, FUN = 2, col = "blue", new = FALSE)


# plot using ID name to extract specific records from two different plots
plot(subset(sl.short,id=="Brw_MISP_45_PL_215_11:54_2017_REFL"))
plot(subset(sl.short,id=="Brw_MISP_44_PL_215_11:54_2017_REFL"), new=FALSE, col="green")

# could also plot all MISP spectra
# (I think default of Hsdar is to show a mean when multiple spectra are selected)
plot(subset(sl.short,site=="MISP"))


# Resample to satellite data (section 5.5)
get.sensor.characteristics(0)

# resample to Landsat 8 and 4
spectral_data_LS8 <- spectralResampling(sl.short,"Landsat8")
spectral_data_LS4 <- spectralResampling(sl.short,"Landsat4")

# plot measured data and resampled data from the same plot
plot(subset(sl.short,id=="Brw_MISP_45_PL_215_11:54_2017_REFL"))
plot(subset(spectral_data_LS8,id=="Brw_MISP_45_PL_215_11:54_2017_REFL"), new=FALSE, col="green")
plot(subset(spectral_data_LS4,id=="Brw_MISP_45_PL_215_11:54_2017_REFL"), new=FALSE, col="blue")

# or could do it for all of MISP
plot(subset(sl.short,site=="MISP"))
plot(subset(spectral_data_LS8,site=="MISP"), new=FALSE, col="green")

# plot multiple things
# NOTE: scales are not the same. have to figure out how to set axes in plot() function

par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
# or could do it for all of MISP
plot(subset(sl.short,site=="MISP"), main="Brw MISP", ylim=c(-100,400))
plot(subset(spectral_data_LS8,site=="MISP"), new=FALSE, col="green")

# or could do it for all of CAlM
plot(subset(sl.short,site=="CALM"), main="Brw CALM", ylim=c(-100,400))
plot(subset(spectral_data_LS8,site=="CALM"), new=FALSE, col="green")


# Chapter 7: Calculating Indices
ndvi <- vegindex(sl.short,"NDVI")
# join the vector of ndvi values with the sample IDs so we know what's what.
ndvi <- data.frame(rowid.sample,ndvi)

ggplot(ndvi, aes(rowid.sample,ndvi))+geom_point()

# calculate multiple indices at once
vegindex()

choose.index <- c("NDVI","CARI","TCARI")

vi <- vegindex(sl.short,index=choose.index)

vi <- cbind(dat.metadata,vi)

# graph by index number using plot
plot(vi$NDVI)

# Extract Red Edge parameters
rd <- rededge(sl.short)

# add sample ID
rd <- cbind(dat.metadata, rd)

# The output of the index calculator is a data frame so you can merge it to the metadata and 
# work with it like you typically would in R. 
# eg, could combine the indices and red edge:

colnames (rd)
vi.rd <- merge(vi,rd,by=c("id","location","site","plot","DOY","time","year"))

# eg, coul plot NDVI by DOY for the different sample locations and sites
ggplot(vi.rd, aes(DOY,NDVI, colour=factor(paste(location,site,plot,sep="_"))))+
  geom_point()+
  geom_line()+
  facet_grid(location~site)

# eg, could plot NDVI by year for the different sample locations and sites
ggplot(vi.rd, aes(year,NDVI, colour=factor(paste(location,site,plot,sep="_"))))+
  geom_point()+
  geom_line()+
  facet_grid(location~site)

# eg, or change index, and coul plot CARI by DOY for the different sample locations and sites
ggplot(vi.rd, aes(DOY,CARI, colour=factor(paste(location,site,plot,sep="_"))))+
  geom_point()+
  geom_line()+
  facet_grid(location~site)

# eg, plot CARI for each DOY and split by years 
ggplot(vi.rd, aes(DOY,CARI, colour=factor(paste(location,site,plot,sep="_"))))+
  geom_point()+
  geom_line()+
  facet_grid(year~site)+
  theme(legend.position = "none") # suppress legend
