#http://www.kevjohnson.org/pdfs/Introduction_to_Ggplot.pdf
# Be sure to install Image Magic (www.imagemagick.org)

library(raster)
library(maptools)
library(rgdal)
library(animation)
library(RColorBrewer)
library(ggplot2)
library(rgeos)
#library(scales)

#if file existsload data from "animated_graphs/aninmated_graphsdata_jpn.rds"
load(file = "animated_graphsdata_jpn.rds")

#Raster lib getData:  name = GADM  (dataset name)
#name = GADM  (dataset name)
#country = "3letterIsoName"
#GADM is a spatial database of the location of the world's administrative areas
jpnb<-getData("GADM", country="JPN", level=1)

#worldclim -- 'worldclim' is a database of global interpolated climate data
jpnp<-getData("worldclim", var="prec", res=0.5, lon=138, lat=36)

#create matrix from jpnp (precipitation RasterStack dataset) and jpnb (geographic polygon dataframe)
#extract the data values (precipitation data) from rasterstack (jpnp) at locations indicated by spatial data (jpnb)
jpnpe <- extract(jpnp, jpnb, fun = mean, na.rm = T)

#convert matrix to dataframe
jpnpe<-data.frame(jpnpe)

#copy precipitation data into jpnb@data
jpnb@data<-jpnpe

#extract columnames from jpnb@data [x1,...,X12]
dat<-colnames(jpnb@data)

#create id data from row.names id: [1,...,47]
jpnb@data$id<-row.names(jpnb@data)

#convert location (shapefile) data into easy to use dataframe data which ggplot2 can understand
#region parameter indicates which data to select
jpnbf<-fortify(jpnb, region="id")

#merge converted shaped data (dataframe) with precipitation data based on id
jpnb.df<-merge(jpnbf,jpnb@data, by="id")



#rename column names from X1,...X12 to Months
colnames(jpnb.df)[8:19]<-c("January","February","March","April","May","June","July","August","September","October","November","December")

#Create Color palette for map consisting of range of 9 colors
col<-brewer.pal(9,"Spectral")
for (i in 8:19){
  #retrieve Month column name
  var<-colnames(jpnb.df)[i]
  
  #specify dataset  ggplot(x=Long, y=Lat, by group)) 
  p<-ggplot(jpnb.df, aes(long,lat, group=group)) + 
     #fill color based on preciptation data fill polygon color by precipitation data 
      geom_polygon(aes_string(fill=var)) + coord_equal()
  
  #create polygon borders
 #Assume lat.group in original code wrong/typo, since jpnb has no group column
 # p<-p+geom_polygon(data=jpnb, aes(long,lat.group=group),fill=NA, colour="black")
  p <- p + geom_polygon(data = jpnb, aes(long,lat),fill = NA, colour="black")
  #Color Gradient legend 
  #p<-p+scale_fill_gradientn("Rainfall",limits=c(0,330),guide="legend", breaks=seq(0,330,30), colours=col)
  
  p <- p + scale_fill_gradientn("Rainfall", limits=c(0,500), breaks = seq(0,500,50), colours=col) + guides(fill = guide_legend(reverse = TRUE))
  p <- p + theme_bw() + ggtitle(var)
  print(p)
  ggsave(file=paste("rainr_",i,".png", sep=""))
}

#Create file name vector
files = sprintf('rainr_%d.png', 8:19)
#create aninmated gif from png files using image magick
im.convert(files, output = 'r_rainfall.gif')

#to list datasets in all packages
# data(package = .packages(all.available = TRUE))
# data(package = "raster")   //for specific package