#0) load libraries
    library(docshop)
    library(geosphere)
    library(stringdist)
    
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
        }
    #b) find file paths
        pdmppath<-file.path(main.dir,"PDMP","pdmp (county).fst")
        acspath<-file.path(main.dir,"ACS","ACS 2017 (county).fst")
        deapath<-file.path(main.dir,"DEA","4 - county files","dea.fst")
        quantpath<-file.path(main.dir,"Pill quantities","Pill quantities (county yearly).fst")
        mergedpath<-file.path(main.dir,"Merged files")
        dir.create(mergedpath,showWarnings = F)

#1) read data
    pdmp<-read.fst(pdmppath)
    acs<-read.fst(acspath)
    dea<-read.fst(deapath)
    quants<-read.fst(quantpath)

#1.5) merge quants into pdmp
    quants$county %in% pdmp$county
    quants$year<-as.numeric(quants$date)
    quants$date<-NULL
    pdmp<-merge(pdmp,quants, by=c("county","year"),all.x=T)
    
#2) treat duplicated pdmp rows (where only difference is zip) as single zips
    a<-duplicated(pdmp[,3:length(pdmp)])
    pdmp<-pdmp[!a,]

#2.5) match county names
    acs$county<-as.numeric(acs$county)
    #a) set both to lower, remove unecessary info from acs county names
        dea$county<-tolower(dea$county)
        dea$county.name<-dea$county
        dea$county.name<-gsub("saint","st.",dea$county.name)
        acs$county.name<-tolower(acs$county.name)
        acs$county.name<-gsub("saint","st.",acs$county.name)
        acs$state<-as.character(sapply(acs$county.name, function(x) strsplit(x,", ")[[1]][2]))
        acs$county.name<-as.character(sapply(acs$county.name, function(x) strsplit(x,",")[[1]][1]))
        acs$county.name2<-as.character(sapply(acs$county.name, 
                            function(x) {
                                y<-strsplit(x," ")[[1]]
                                paste(y[1:(length(y)-1)],collapse=" ")
                            })
                     )
        acs$county.name3<-as.character(sapply(acs$county.name2,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name4<-as.character(sapply(acs$county.name3,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name5<-as.character(sapply(acs$county.name4,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name6<-as.character(sapply(acs$county.name5,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name7<-as.character(sapply(acs$county.name6,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name8<-as.character(sapply(acs$county.name7,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name9<-as.character(sapply(acs$county.name8,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        acs$county.name10<-as.character(sapply(acs$county.name9,function(x) {y<-strsplit(x," ")[[1]]; paste(y[1:(length(y)-1)],collapse=" ")})        )
        
    #b) find corresponding state names in ACS
        statedf<-data.table(state.name=c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming","American Samoa","District of Columbia","Federated States of Micronesia","Guam","Marshall Islands","Northern Mariana Islands","Palau","Puerto Rico","Virgin Islands"),
                  state=c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","AS","DC","FM","GU","MH","MP","PW","PR","VI"))
        statedf$state.name<-tolower(statedf$state.name)
        acs$state.name<-acs$state
        acs$state<-NULL
        acs<-merge(statedf,acs,by="state.name")
        
        
    #c) find corresponding acs county names in dea; lazily and iteratively account for inexact matches
        ClosestMatch2 = function(string, stringVector){
            stringVector[amatch(string, stringVector, maxDist=1)]
        }
        temp<-rep(NA,nrow(acs))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name2)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name3)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name4)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name5)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name6)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name7)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name8)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name9)[is.na(temp)], paste(dea$state,dea$county.name))
        temp[is.na(temp)]<-ClosestMatch2(paste(acs$state,acs$county.name10)[is.na(temp)], paste(dea$state,dea$county.name))
        acs$county.name.match<-temp
        temp<-unique(paste(dea$state,dea$county.name)[!paste(dea$state,dea$county.name) %in% temp])
        temp<-temp[!grepl("null",temp)]
        temp2<-ClosestMatch2(temp,paste(acs$state,acs$county.name2))
        temp<-temp[!is.na(temp2)]
        temp2<-temp2[!is.na(temp2)]
        for(i in 1:length(temp2)){
            dea$county[paste(dea$state,dea$county.name)==temp[i]]<=temp2[i]
        }

    #d) merge county geos into dea
        dea$county<-NULL
        dea<-dea[-which(dea$county.name=="null"),]
        temp<-acs[,c("county","county.name.match")]
        dea<-data.table(dea)
        temp<-data.table(temp)
        dea$county.name.match<-paste(dea$state,dea$county.name)
        dea<-merge(dea,temp,by="county.name.match")
            #there are a few counties with multiple GEOIDs
        dea$county.name.match<-NULL
        vars<-c("county.name2","county.name3","county.name4","county.name5","county.name6","county.name7","county.name8","county.name9","county.name10","county.name.match")
        acs<-acs[,!names(acs) %in% vars,with=F]
        
        
#3) merge pdmp and dea by county years
    df<-merge(pdmp,dea,by=c("county","year"),all=T)
    #replace variables ending with x and y with only one value
        varnames<-names(df)
        probs<-substr(varnames,nchar(varnames)-1,nchar(varnames)) %in% c(".x",".y")
        probname<-varnames[probs]
        corname<-substr(varnames,1,nchar(varnames)-2)[probs]
        tofix<-unique(corname)
        fixes<-list()
        for(i in 1:length(tofix)){
            #pull out matching variables
                tdf<-df[probname[corname==tofix[i]]]
            #replace NA in first vector with values in second vector
                tdf[,1][is.na(tdf[,1])]<-tdf[,2][is.na(tdf[,1])]
            #bind corrected variable to DF
                fixes[[i]]<-tdf[,1]
                df<-cbind(df,fixes[[i]])
                names(df)[ncol(df)]<-tofix[i]
        }
        #kill duplicate variables
            df[,probname]<-NULL

#4) merge df with acs data
    df2<-merge(df,acs,by="county",all=T)

#5) save
    write.fst(df2,file.path(mergedpath,"DEA PDMP ACS (county).fst"),compress = 100)
        
#6) aggregate interesting variables from pdmp and dea across years of interest
    #a) match years between datasets
        dea<-dea[dea$year %in% unique(pdmp$year),]
        pdmp<-pdmp[pdmp$year %in% unique(dea$year),]
    #b) aggregate variables across years
        dea<-data.table(dea)
        dea2<-dea[,.(pills=mean(pills)),by=c("county","state")]
        pdmp<-data.table(pdmp)
        pdmp2<-pdmp[, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    #c) merge pdmp and dea by zipcode years (can't be zipcode2012 if want unique matches)
        df<-merge(pdmp2,dea2,by=c("county"),all=T)

#7) merge df with acs data
    acs<-data.table(acs)
    df2<-merge(df,acs,by="county",all=T)

#8) save
    write.fst(df2,file.path(mergedpath,"DEA PDMP ACS yearless (county).fst"),compress = 100)