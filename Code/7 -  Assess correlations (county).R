#0) load libraries
    library(docshop)
    library(geosphere)
    library(ggplot2)
    library(reshape2)
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
        dir.create(mergeddir,showWarnings = F)

#1) read data
    df<-read.fst(mergedpath,as.data.table = T)

#2) create vars of interest
    df$pMAT<-df$n_MAT_patients/df$n_people
    df$pillsperperson<-df$pills/df$ind.pop
    df$pillsperperson[is.infinite(df$pillsperperson)]<-NA
    df$inchome<-df$income.median/df$homevalue.median
    df$ins.pillsperperson<-df$opioid_pill_total/df$ind.pop
        
    df<-df[df$year!=2006,]

#3) aggregate variablesacross years of interest
    df.1<-df[df$year %in% 2007:2009, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    df.2<-df[df$year %in% 2010:2012, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    df.3<-df[df$year %in% 2013:2015, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]
    df.4<-df[df$year %in% 2016:2018, lapply(.SD, base::mean, na.rm=TRUE), by=c("county") ]

    cor(df.2$MAT_pill_total,df.2$opioid_pill_total,use="complete")
    cor(df.2$MAT_pill_total/df.2$n_people,df.2$opioid_pill_total/df.2$n_people,use="complete")
    cor(df.2$n_opioid_prescriptions/df.2$n_people,df.2$opioid_pill_total/df.2$n_people,use="complete")
    cor(df.2$pillsperperson,df.2$opioid_pill_total/df.2$ind.pop,use="complete")
    cor(df.2$pills/df.2$ind.pop,df.2$opioid_pill_total/df.2$ind.pop,use="complete")
    cor(df.2$ins.pillsperperson[df.2$ind.pop>20000],df.2$pillsperperson[df.2$ind.pop>20000],use="complete")
    
    sum(df$opioid_pill_total[df$year%in%2007:2012 & df$ind.pop>20000],na.rm = T)/1000000    
    sum(df$pills[df$year%in%2007:2012 & df$ind.pop>20000],na.rm = T)/1000000
    table(df.2$ind.pop>20000)

#3) select interesting variables
    keepvars<-c(
        "Q.pillsperperson",
            "pillsperperson",
            "no_has_opioid",
            "no_high_mme",
            "no_with_overdosed",
            "no_has_MAT",
            "no_has_overlap",
            "no_n_unique_pharmacy4",
            "no_n_unique_provider4",
            "education.lessthanhs",
            "education.masters",
            "income.median",
            "poverty.number",
            "gini",
            "inchome",
            "insurance.public",
            "insurance.private",
            "insurance.none",
            "age.median",
            "sex.male",
            "race.white",
            "marital.married",
            "ind.pop"
            )
    
    varnames<-c(
        "Q.pillsperperson",
        "DEA.pillsperperson",
            "% on Opioids",
            "% on High MME",
            "% Overdosed",
            "% on MAT",
            "% Overlapping RX",
            "% Pharma shopping",
            "% Doctor shopping",
            "% Education < HS",
            "% Education > Masters",
            "Median Income",
            "% Poverty",
            "Gini",
            "Income / Home Value",
            "% Public Insurance",
            "% Private Insurance",
            "% Uninsured",
            "Median Age",
            "% Male",
            "% White",
            "% Married",
            "Population"
            )
    
    df.12 <-df.1[, keepvars,with=F]
    df.22 <-df.2[, keepvars,with=F]
    df.32 <-df.3[, keepvars,with=F]
    df.42 <-df.4[, keepvars,with=F]
    
    # df.22<-df.22[df.22$ind.pop>20000,]
    dfl<-list(df.12,df.22,df.32,df.42)
    
i=2    
# for(i in 1:(length(dfl)+1)){
    if(i<(length(dfl)+1)){
    df2<-dfl[[i]]
    #c) limit outliers
        outlierfun<- function(x,min=.02,max=.98){
            x[x<quantile(x,min,na.rm=T)]<-quantile(x,min,na.rm=T)
            x[x>quantile(x,max,na.rm=T)]<-quantile(x,max,na.rm=T)
            return(x)
        }
        df2<-as.data.frame(sapply(df2,outlierfun))
    #4) look at correlations
        cormat<-round(cor(df2,use="pairwise.complete.obs"),2)
        cormat<-cormat[,1:5]
        cormat<-cormat[,1:2]
        
    }
    
    if(i==(length(dfl)+1)){
        df2<-dfl[[1]]
        df2<-as.data.frame(sapply(df2,outlierfun))
        cormat<-round(cor(df2,use="pairwise.complete.obs"),2)
        cormat1<-cormat[,1:5]
        
        df2<-dfl[[4]]
        df2<-as.data.frame(sapply(df2,outlierfun))
        cormat<-round(cor(df2,use="pairwise.complete.obs"),2)
        cormat2<-cormat[,1:5]
        
        cormat<-cormat2-cormat1
        diag(cormat)<-NA
    }
    
#5) graph results
    #a) Prep graph
        #i) remove upper triangle
            get_lower_tri <- function(cormat){
              cormat[upper.tri(cormat)]<- NA
              return(cormat)
            }
            lower_tri <- get_lower_tri(cormat)
        #ii) Put into three columns
            melted_cormat <- melt(lower_tri, na.rm = F,variable.factor=F)
            # melted_cormat$Var1<-as.character(melted_cormat$Var1)
            # melted_cormat$Var2<-as.character(melted_cormat$Var2)
        #iii) Format results
            melted_cormat$val2<-melted_cormat$value
            melted_cormat$val2<-sprintf("%.2f", melted_cormat$val2)
        #iv) Erase duplicates from melted values and format
            melted_cormat$val2[melted_cormat$value==1]<-""
            melted_cormat$value[melted_cormat$value==1]<-0
            # melted_cormat[melted_cormat$Var1==melted_cormat$Var2,]$value<-0
            melted_cormat$val2<-ifelse(sign(as.numeric(melted_cormat$val2))==-1,paste("(",(str_sub(melted_cormat$val2,2,5)),")",sep=""),melted_cormat$val2)
            melted_cormat$val2[melted_cormat$val2=="-0.00"]<-"0.00"
            melted_cormat$value[is.na(melted_cormat$value)]<-0
            
            # melted_cormat$value[melted_cormat$value==1]<-0
        #v) Plot heatmap
            ggheatmap<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
              geom_tile(color = "white")+
              scale_fill_gradient2(low = "#1bc3e5", high = "#e53d1b",
                                   midpoint = 0, limit = c(-1,1), space = "Lab",
                                   name="Pearson\nCorrelation") +
              theme_minimal()+
                theme(
                    axis.text.x = element_text(
                        angle = 45,
                        vjust = 1,
                        size = 14, #axis label text size
                        hjust = 1
                    ),
                    axis.text.y = element_text(size = 14) #axis label text size
                ) #+
                #coord_fixed()
    #b) graph with text, prettier labels. etc.
        #i) change variable names
        #ii) graph
            ggheatmap2<-ggheatmap +
              geom_text(aes(Var2, Var1, label = val2), color = "black", 
                        size = 4.5,hjust=.5) +
              theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.text = element_text(size = 15),
                legend.title = element_text(size = 15),
                plot.title = element_text(size=20, hjust=.38),
                plot.subtitle = element_text(size=20, hjust=.38),
                plot.margin = margin(.5, 4, 0, 0, "cm"))+
              guides(fill = guide_colorbar(barwidth = 3, barheight = 15))+
              ggtitle("Correlations among Q and DEA",subtitle="Pills per person (2010-2012)")+
              scale_y_discrete(labels= (varnames))+
              scale_x_discrete(labels= (varnames))
        
        #iii)save
              ggsave(
                  tsave1 <-
                      paste(
                          fig.dir,
                          i,"Correlations among Q and DEA.png",
                          sep = ""
                      ),
                  ggheatmap2,
                  width = 7,
                  height = 8,
                  units = "in"
              )
              browseURL(tsave1)
              print(i)


# }


