##############################################################################################################
# 
# 
# Program to extract Crop prudiction index (CPPI) fromm the GSSURGO data base 
# 
#   State: INDIANA 
# 
# 
#  Felipe Montes 2020/04/10
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard()

setwd("C:\\Felipe\\VegetationPhotosynthesisRespirationModel") ;   # 



###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


# Install the packages that are needed #

# install.packages('fields', dep=T)

# install.packages('LatticeKrig', dep=T)

# install.packages('rgeos', dep=T)

# install.packages('RColorBrewer', dep=T)

# install.packages('KernSmooth', dep=T)

# install.packages('sf', dep=T)

###############################################################################################################
#                           load the libraries that are neded   
###############################################################################################################
library(rgdal) ; 



library(sp) ;

library(raster) ;


library(fields);


library(rgeos)

library(RColorBrewer)


library(sf)

library(openxlsx)

###############################################################################################################
#                          load data from the .gdb ESRI file geodatabase
###############################################################################################################

#path where the directory with the godatabase is

fgdb<-"C:\\Felipe\\VegetationPhotosynthesisRespirationModel\\INDIANA\\soils_GSSURGO_in_3809713_03\\soils\\gssurgo_g_in\\gSSURGO_IN.gdb"


# read the MUYKEY Polygons
ogrListLayers(fgdb)
INDIANA.SOILS<-readOGR(dsn=fgdb, layer='MUPOLYGON')

str(INDIANA.SOILS@data)
str(coordinates(INDIANA.SOILS))
str(row.names(INDIANA.SOILS@data))

# read the component table using the sf package. Rgdal does not have the tool to read qattribute table data without geometries


INDIANA.Comp<-sf::st_read(dsn=fgdb, layer='component')

#  View(INDIANA.Comp)

# str(INDIANA.Comp)

# str(INDIANA.Comp$mukey)

# str(INDIANA.Comp[INDIANA.Comp$majcompflag == 'Yes',])

#select the dominant component

INDIANA.DominantC<- aggregate(comppct_r ~ mukey, data=INDIANA.Comp, FUN="max" , drop=T, simplify=T) ;



#  str(INDIANA.DominantC) ;  which(is.na(INDIANA.DominantC),arr.ind=T)

#Select the dominat components from the Component table for each MUYKEY

INDIANA.Comp$mukey.comppct_r<-paste(as.character(INDIANA.Comp$mukey),INDIANA.Comp$comppct_r,sep="_") ;


INDIANA.DominantC$mukey.comppct_r<-paste(as.character(INDIANA.DominantC$mukey),INDIANA.DominantC$comppct_r,sep="_") ;

INDIANA.Dom.Comp<-INDIANA.Comp[which(INDIANA.Comp$mukey.comppct_r %in% INDIANA.DominantC$mukey.comppct_r),]


#   str(INDIANA.Dom.Comp)

#  remove duplicated dominant components

which(duplicated(INDIANA.Dom.Comp$mukey.comppct_r))

INDIANA.Dom.Comp.uniq<-INDIANA.Dom.Comp[which(!duplicated(INDIANA.Dom.Comp$mukey.comppct_r)),] ;


#  str(INDIANA.Dom.Comp.uniq) ; names(INDIANA.Dom.Comp.uniq)

#  View(INDIANA.Dom.Comp.uniq)

#  read the cocropyld table 

INDIANA.cocropyld<-sf::st_read(dsn=fgdb, layer='cocropyld')

#   str(INDIANA.cocropyld)

# Select corn from the crop name

INDIANA.Corn<-INDIANA.cocropyld[which(INDIANA.cocropyld$cropname == 'Corn'), ] ;

# merge INDIANA.cocropyld with the INDIANA.Dom.Comp


INDIANA.Corn$cokey.Int<-as.numeric(as.character(INDIANA.Corn$cokey)) ;

INDIANA.Dom.Comp.uniq$cokey.Int<-as.numeric(as.character(INDIANA.Dom.Comp.uniq$cokey)) ;


INDIANA.Mukey.Corn<-merge(INDIANA.Dom.Comp.uniq,INDIANA.Corn, by='cokey.Int', all.x=T)

#  str(INDIANA.Mukey.Corn$cokey) ; names(INDIANA.Mukey.Corn)
duplicated(INDIANA.Corn$cokey)

INDIANA.Corn[which(duplicated(INDIANA.Corn$cokey.Int)),]

View(INDIANA.Mukey.Corn)

# Join INDIANA.Mukey.Corn with INDIANA.SOILS@data

INDIANA.SOILS@data$MUKEY.INT<-as.integer(as.character(INDIANA.SOILS@data$MUKEY)) ;

str(INDIANA.SOILS@data$MUKEY.INT)

INDIANA.Mukey.Corn$MUKEY.INT<-as.integer(as.character(INDIANA.Mukey.Corn$mukey)) ;

str(INDIANA.Mukey.Corn$MUKEY.INT)  ; names(INDIANA.Mukey.Corn)

INDIANA.CPPI<-merge(INDIANA.SOILS@data,INDIANA.Mukey.Corn, by='MUKEY.INT') ;

str(INDIANA.CPPI)

# merge with the MUKEYPOLYGON spoatial layer


#  str(INDIANA.SOILS@data$MUKEY) ;  str(INDIANA.Mukey.Corn$mukey)
names(INDIANA.CPPI)


INDIANA.SOILS@data<-INDIANA.CPPI[,c("MUKEY","nonirryield_r")]

writeOGR(INDIANA.SOILS, "INDIANA_CORNYLD.shp", layer="INDIANA_CORNYLD", driver="ESRI Shapefile")

str(INDIANA.SOILS@data)
str(INDIANA.Mukey.Corn$mukey.factor)

####  Field  Soil Samples 

Soil.Samples<-read.xlsx("C:\\Felipe\\VegetationPhotosynthesisRespirationModel\\IndianaSoilSamples.xlsx", sheet= "SoilSamplesIndiana", startRow = 1 ,colNames = T , cols= c(1,2), rows=c(seq(1,7)) );

### Create Spatial points

Soil.Samples.sp<-SpatialPointsDataFrame(coords=Soil.Samples, data=Soil.Samples, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
writeOGR(Soil.Samples.sp, "C:\\Users\\frm10\\Downloads\\SoilSamples.shp" , layer = "SoilSamples", driver="ESRI Shapefile" )




# extract soil Mukesys from the points in the Soil samples
Soil.Samples.sp.conus<-spTransform(Soil.Samples.sp, INDIANA.SOILS@proj4string)


writeOGR(Soil.Samples.sp.conus, "C:\\Users\\frm10\\Downloads\\SoilSamples.conus.shp" , layer = "SoilSamplesConus", driver="ESRI Shapefile" )

Soil.Samples.Surgo<-intersect(Soil.Samples.sp.conus,INDIANA.SOILS);

Soil.Samples.Surgo.info<-INDIANA.Dom.Comp.uniq[which(INDIANA.Dom.Comp.uniq$mukey %in% Soil.Samples.Surgo@data$MUKEY),]
 

Data.Soil.Samples<-merge(Soil.Samples.Surgo@data,Soil.Samples.Surgo.info, by.x='MUKEY', by.y= 'mukey' ) ;

write.xlsx(Data.Soil.Samples, file="SoilSamplesData.xlsx")
