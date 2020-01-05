#0) load libraries
    library(docshop)
    library(geosphere)
    library(ggplot2)
    library(reshape2)
    library(stringr)
    library(sp)
    library(rgeos)
    library(leaflet)
    library(tidycensus)
    library(ggplot2)
    library(scales)
    library(tigris)
    library(sf)
    options(tigris_class = "sf")
    options(tigris_use_cache = TRUE)
    
    #a) find main directory
        if(grepl("briarons",getwd())){
          main.dir<-"C:/Users/briarons/Desktop/Analysis - Data/Postdoc"
          fig.dir<-"C:/Users/briarons/Desktop/Temp - Figures/"
        }
        if(grepl("bda13",getwd())){
          main.dir<-"C:/Users/bda13/Desktop/Analysis - Data/Postdoc"
          fig.dir<-"C:/Users/bda13/Desktop/Temp - Figures/"
        }
        
    #b) find file paths
        mergeddir<-file.path(main.dir,"Merged files")
        mergedpath<-file.path(mergeddir,"DEA PDMP ACS (county).fst")
        shape.dir<-file.path(main.dir,"Shape files")
        dir.create(mergeddir,showWarnings = F)
        dir.create(shape.dir, showWarnings = FALSE)
        
#1) read data
    df<-read.fst(mergedpath,as.data.table = T)
    # tracts<-readRDS(file.path(shape.dir,"county shapes 2018.rds"))
    tracts <- counties(cb=F,year=2017,state="IN",resolution = "20m")

    
#2) create vars of interest
    df$pMAT<-df$n_MAT_patients/df$n_people
    df$pillsperperson<-df$n_opioid_prescriptions/df$ind.pop
    df$pillsperperson[is.infinite(df$pillsperperson)]<-NA
    df$inchome<-df$income.median/df$homevalue.median
    df$ins.pillsperperson<-df$opioid_pill_total/df$ind.pop
    df<-df[df$state.name=="indiana",]
    
#3) aggregate variablesacross years of interest
    df.2<-df[df$year %in% 2010:2012, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    df.4<-df[df$year %in% 2016:2018, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]

#4) Merge relevant county level info into shape files
    tracts$GEOID2<-as.numeric(tracts$GEOID)
    #a) 2010-2012
        df.2$GEOID2<-df.2$county
        df.22<-df.2[,c("no_has_MAT","ins.pillsperperson","GEOID2")]
        countydf2<-merge(tracts,df.22,by="GEOID2",all.x=T)
    #b) 2016-2018
        df.4$GEOID2<-df.4$county
        df.42<-df.4[,c("insurance.private","no_has_MAT","ins.pillsperperson","GEOID2")]
        countydf<-merge(tracts,df.42,by="GEOID2",all.x=T)    
    #c) summary stats
        # summary(df.2$ins.pillsperperson)
        # summary(df.4$ins.pillsperperson)
        # cor(countydf2$ins.pillsperperson,countydf$ins.pillsperperson,use="complete")
        # summary(countydf$ins.pillsperperson-countydf2$ins.pillsperperson)
    
    
#5) plot pillsperpop in 2016-2018
    p<-ggplot()+
        geom_sf(
            data = countydf,
            aes(fill = ins.pillsperperson),
            size = 0.5,
            color="grey50"
        )+
            coord_sf(crs = sf::st_crs(countydf), datum = NA)+
            # scale_fill_gradient(low = "white",high="red",na.value="white")+
            scale_fill_gradient2(low = "white",mid="#FFF6F6",high="#cc0000",na.value="white")+#,midpoint=20,limits=c(0,125)
            theme_void() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  # legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  text=element_text(family="serif",face="bold",size=20),
                  axis.title=element_blank())+
        labs(fill="Pills per person",title="         Opioids Sold in Indiana (Insurance Data, 2016-2018)")
        ggsave(tsave1<-paste(fig.dir,"Opioids Sold in Indiana (Insurance Data, 2016-2018).png",sep=""),p,width=8.5,height=8,units="in")
        browseURL(tsave1)

#6) plot pillsperpop in 2010-2012
    p<-ggplot()+
        geom_sf(
            data = countydf2,
            aes(fill = ins.pillsperperson),
            size = 0.5,
            color="grey50"
        )+
            coord_sf(crs = sf::st_crs(countydf), datum = NA)+
            # scale_fill_gradient(low = "white",high="red",na.value="white")+
            scale_fill_gradient2(low = "white",mid="#FFF6F6",high="#cc0000",na.value="white")+#,midpoint=20,limits=c(0,125)
            theme_void() +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  # legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  text=element_text(family="serif",face="bold",size=20),
                  axis.title=element_blank())+
        labs(fill="Pills per person",title="         Opioids Sold in Indiana (Insurance Data, 2010-2012)")
        ggsave(tsave1<-paste(fig.dir,"Opioids Sold in Indiana (Insurance Data, 2010-2012).png",sep=""),p,width=8.5,height=8,units="in")
        browseURL(tsave1)
        