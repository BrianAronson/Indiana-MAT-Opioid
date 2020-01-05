#0) load libraries
    library(docshop)
    library(tigris)
    library(sf)
    options(tigris_class = "sf")
    options(tigris_use_cache = TRUE)
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #b) set directory to ACS folder
        shape.dir<-file.path(main.dir,"Shape files")
    #c) create directory
        dir.create(shape.dir, showWarnings = FALSE)

#1) pull in shapefiles
    tracts.12 <- counties(cb=F,year=2012)
    names(tracts.12)[2]<-"counties"
    tracts.18 <- counties(cb=F,year=2018)
    names(tracts.18)[2]<-"counties"

#2) append objects and save
    saveRDS(tracts.12,file.path(shape.dir,"county shapes 2012.rds"))
    saveRDS(tracts.18,file.path(shape.dir,"county shapes 2018.rds"))