#0) Prepare workspace
    #a) load packages
        library(docshop)

    #b) find main directory; use AWS if working from EC2
        main.dir<-"hidden"

    #c) name files and directories to be created
        quantdir<-file.path(main.dir,"Pill quantities")
        quantpath<-file.path(quantdir,"Pill quantities (county yearly).fst")
        
    #d) create directory for files to save
        put_folder(quantdir)    

    #d) load data for identifying opioids and MAT
        df.drugs_all = s3read_csv(file.path(
            "hidden", 
            "additional_data", 
            "drug.tsv"))
        df.drugs_all[,NDC:=create_leading_zeros(NDC,11)]
        df.drugs_opioid = df.drugs_all[Class=='Opioid',]
        df.drugs_MAT = s3read_csv(file.path(
            "hidden",
            "additional_data",
            "MAT_drug.tsv"
        ))
        df.drugs_MAT[,NDC:=create_leading_zeros(NDC,11)]

#1) Prepare objects for use in loop
    #a) list to append looped datatables to
        l.prescriptions_by_county<-list()
        
    #b) v.dates/quarters to read 
        v.dates<-paste0(rep(2007:2018,each=4),c("q1","q2","q3","q4"),sep="")
        v.dates<-v.dates[-((length(v.dates)-2):length(v.dates))]

        
#2) Start loop    
for(i in 1:length(v.dates)){

#3) Iteratively input names of raw prescription data and merged geo data
    v.raw_prescriptions_name <-
        paste("hidden",
              v.dates[i],
              ".fst",
              sep = "")
    
    v.merged_geo_name <-
        paste("hidden",
            v.dates[i],
            ".csv.gz",
            sep = "")
    
#4) Load data, just grabbing necessary variables (I am assuming that "QUANTITY" refers to number of pills)
    df.raw_prescriptions <- s3read_any(v.raw_prescriptions_name, columns = c("PATID", "NDC", "QUANTITY"))
    df.merged_geo <- s3read_any(v.merged_geo_name, select = c("PATID", "county"))
    
#5) Estimate pills sold
   #a) merge datasets
        df.prescriptions_geo<-merge(x=df.raw_prescriptions,y=df.merged_geo,by="PATID",all.x = T)
        
    #b) identify whether drug is opioid or MAT
        df.prescriptions_geo$opioid <- df.prescriptions_geo$NDC %in% df.drugs_opioid$NDC
        df.prescriptions_geo$MAT <- df.prescriptions_geo$NDC %in% df.drugs_MAT$NDC
        
    #c) count pills per county
        df.prescriptions_by_county <- df.prescriptions_geo[, .(
            MAT_pill_total = sum(QUANTITY[MAT],na.rm=T),
            opioid_pill_total = sum(QUANTITY[opioid],na.rm=T)
            ), 
            by = "county"]
        
    #d) append to list
        df.prescriptions_by_county$date<-v.dates[i]
        l.prescriptions_by_county[[i]]<-df.prescriptions_by_county
        print(i)
} 

        
#6) Save pill quantities
    #a) bind list to data.table
        df.quarterly_prescriptions_by_county<-rbindlist(l.prescriptions_by_county)
         
    #b) aggregate by year (rather than by quarter, as is currently)
        df.quarterly_prescriptions_by_county$date <- substr(df.quarterly_prescriptions_by_county$date, 1, 4)
        
        df.yearly_prescriptions_by_county <- df.quarterly_prescriptions_by_county[, .(
            MAT_pill_total = sum(MAT_pill_total,na.rm = T),
            opioid_pill_total = sum(opioid_pill_total,na.rm = T)
            ), 
            by = c("county", "date")]
        
    #c) save
        s3write_fst(df.yearly_prescriptions_by_county,quantpath)   
        