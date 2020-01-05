#0) load libraries
    library(docshop)
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #b) set directory to PDMP folder
        PDMP.dir<-file.path(main.dir,"PDMP")
    #c) create directory
        dir.create(PDMP.dir, showWarnings = FALSE)
        
#1) load pdmp data
    tdir<-"hidden"
    df07<-s3read_csv(paste(tdir,"zip5_county2007_pdmp.csv",sep=""))
    df08<-s3read_csv(paste(tdir,"zip5_county2008_pdmp.csv",sep=""))
    df09<-s3read_csv(paste(tdir,"zip5_county2009_pdmp.csv",sep=""))
    df10<-s3read_csv(paste(tdir,"zip5_county2010_pdmp.csv",sep=""))
    df11<-s3read_csv(paste(tdir,"zip5_county2011_pdmp.csv",sep=""))
    df12<-s3read_csv(paste(tdir,"zip5_county2012_pdmp.csv",sep=""))
    df13<-s3read_csv(paste(tdir,"zip5_county2013_pdmp.csv",sep=""))
    df14<-s3read_csv(paste(tdir,"zip5_county2014_pdmp.csv",sep=""))
    df15<-s3read_csv(paste(tdir,"zip5_county2015_pdmp.csv",sep=""))
    df16<-s3read_csv(paste(tdir,"zip5_county2016_pdmp.csv",sep=""))
    df17<-s3read_csv(paste(tdir,"zip5_county2017_pdmp.csv",sep=""))
    df18<-s3read_csv(paste(tdir,"zip5_county2018_pdmp.csv",sep=""))

#2) merge pdmp data
    ldf<-list(df07,df08,df09,df10,df11,df12,df13,df14,df15,df16,df17,df18)
    pdmp<-do.call(rbind,ldf)

#3) add year column for above
    pdmp$year<-unlist(mapply(rep, times = sapply(ldf,nrow), x = 2007:2018))

#4) clean workspace
    suppressWarnings(rm("tdf","ldf","df07","df08","df09","df10","df11","df12","df13","df14","df15","df16","df17","df18"))
    gc()

#5) add useful variables
    pdmp$no_has_MAT2<-pdmp$n_MAT_patients/pdmp$n_people
    
#6) save
    write.fst(pdmp,file.path(PDMP.dir,"pdmp (county).fst"),compress = 100)

