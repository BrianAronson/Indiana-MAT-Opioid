#NOTE: should eventually change to grab multiple years of ACS

#0) load libraries
    library(docshop)
    library(tidycensus)
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
        ACS.dir<-file.path(main.dir,"ACS")
    #c) create directory
        dir.create(ACS.dir, showWarnings = FALSE)

#.5) set years to grab census variables from
    years<-c(2012,2017)
    for(i in 1:length(years)){
    
        
#1) grab census variables
    mykey="XXXX"
    
    census_info <- get_acs(
        year=years[i],
        # survey = "acs1", #geography must be at least county and year>=2012
        geography = "county", #zcta requires year at least = 2011; some variables (e.g. B27020) not available until later
        geometry = F,
        key = mykey,
        output = "wide",
        cache_table = T,
        variables = c(
            sex.male.uind="B01001_002",
            sex.female.uind="B01001_026",
            race.black.uind="B01001B_001",
            race.native.uind="B01001C_001",
            race.asian.uind="B01001D_001",
            race.pacific.uind="B01001E_001",
            race.other.uind="B01001F_001",
            race.multiracial.uind="B01001G_001",
            race.white.uind="B01001H_001",
            race.hispanic.uind="B01001I_001",
            age.median.uind="B01002_001",
            citizenship.us.uind="B05001_002",
            citizenship.natural.uind="B05001_005",
            citizenship.not.uind="B05001_006",
            born.samestate.uind="B05002_003",
            immigrant.uind="B05005_001",
            uind="B01003_001",
            
            language.onlyenglish.uover5="B06007_002",
            language.englishgood1.uworks="B06007_004",
            language.englishbad1.uworks="B06007_005",
            language.englishgood2.uworks="B06007_007",
            language.englishbad2.uworks="B06007_008",
            uover5="B06007_001",
            
            marital.nevermarried.uover15="B06008_002",
            marital.married.uover15="B06008_003",
            marital.divorced.uover15="B06008_004",
            marital.separated.uover15="B06008_005",
            marital.widowed.uover15="B06008_006",
            uover15="B06008_001",
            
            education.lessthanhs.uover25="B06009_002",
            education.hs.uover25="B06009_003",
            education.somecollege.uover25="B06009_004",
            education.bachelors.uover25="B06009_005",
            education.masters.uover25="B06009_006",
            uover25="B06009_001",
            
            work.none.uhouse="B08202_002",
            work.one.uhouse="B08202_003",
            work.two.uhouse="B08202_004",
            work.three.uhouse="B08202_005",
            uhouse="B08202_001",
            
            poverty.number.upoverty="B17001_002",
            poverty.numberabove2x.upoverty="C17002_008",
            upoverty="B17001_001",
            
            vacant.uhousing="B25002_003",
            uhousing="B25002_001",
            
            insurance.private.uinsurance="B27020_004",
            insurance.public.uinsurance="B27020_005",
            insurance.none.uinsurance="B27020_006",
            uinsurance="B27020_002",

            home.value.median="B25077_001",
            income.median="B06011_001",
            income.median.uhouse="B19013_001",
            gini.uhouse="B19083_001"
            
          )
    )
    
#2) save data
    names(census_info)[names(census_info)=="NAME"]<-"county.name"
    names(census_info)[names(census_info)=="GEOID"]<-"county"
    write.fst(census_info,file.path(ACS.dir,"ACS (raw county).fst"),compress = 100)
    
#3) clean data / make new vars
    #a) eliminate useless variables
        tnames<-names(census_info)
        keepvars<-substr(tnames,nchar(tnames),nchar(tnames))=="E"
        keepvars[1:2]<-T
        df<-census_info[,keepvars]
        rmvars<-names(df[c(-1,-2)])
    #b create variables that represent proportions of population
        df$sex.male<-df$sex.male.uindE/df$uindE
        df$sex.female<-df$sex.female.uindE/df$uindE
        df$race.black<-df$race.black.uindE/df$uindE
        df$race.native<-df$race.native.uindE/df$uindE
        df$race.asian<-df$race.asian.uindE/df$uindE
        df$race.pacific<-df$race.pacific.uindE/df$uindE
        df$race.other<-df$race.other.uindE/df$uindE
        df$race.multiracial<-df$race.multiracial.uindE/df$uindE
        df$race.white<-df$race.white.uindE/df$uindE
        df$race.hispanic<-df$race.hispanic.uindE/df$uindE
        df$citizenship.us<-df$citizenship.us.uindE/df$uindE
        df$citizenship.natural<-df$citizenship.natural.uindE/df$uindE
        df$citizenship.not<-df$citizenship.not.uindE/df$uindE
        df$born.samestate<-df$born.samestate.uindE/df$uindE
        df$immigrant<-df$immigrant.uindE/df$uindE
        df$language.englishgood1<-df$language.englishgood1.uworksE/df$uover5E
        df$language.englishbad1<-df$language.englishbad1.uworksE/df$uover5E
        df$language.englishgood2<-df$language.englishgood2.uworksE/df$uover5E
        df$language.englishbad2<-df$language.englishbad2.uworksE/df$uover5E
        df$language.onlyenglish<-df$language.onlyenglish.uover5E/df$uover5E
        df$marital.nevermarried<-df$marital.nevermarried.uover15E/df$uover15E
        df$marital.married<-df$marital.married.uover15E/df$uover15E
        df$marital.divorced<-df$marital.divorced.uover15E/df$uover15E
        df$marital.separated<-df$marital.separated.uover15E/df$uover15E
        df$marital.widowed<-df$marital.widowed.uover15E/df$uover15E
        df$education.lessthanhs<-df$education.lessthanhs.uover25E/df$uover25E
        df$education.hs<-df$education.hs.uover25E/df$uover25E
        df$education.somecollege<-df$education.somecollege.uover25E/df$uover25E
        df$education.bachelors<-df$education.bachelors.uover25E/df$uover25E
        df$education.masters<-df$education.masters.uover25E/df$uover25E
        df$work.none<-df$work.none.uhouseE/df$uhouseE
        df$work.one<-df$work.one.uhouseE/df$uhouseE
        df$work.two<-df$work.two.uhouseE/df$uhouseE
        df$work.three<-df$work.three.uhouseE/df$uhouseE
        df$poverty.number<-df$poverty.number.upovertyE/df$upovertyE
        df$poverty.numberabove2x<-df$poverty.numberabove2x.upovertyE/df$upovertyE
        df$vacant<-df$vacant.uhousingE/df$uhousingE
        df$insurance.private<-df$insurance.private.uinsuranceE/df$uinsuranceE
        df$insurance.public<-df$insurance.public.uinsuranceE/df$uinsuranceE
        df$insurance.none<-df$insurance.none.uinsuranceE/df$uinsuranceE
        df$homevalue.median<-df$home.value.medianE
        df$income.median<-df$income.medianE
        df$income.median.house<-df$income.median.uhouseE
        df$age.median<-df$age.median.uindE
        df$gini<-df$gini.uhouseE
        df$ind.pop<-df$uindE
        df$house.pop<-df$uhouseE
        df2<-df
        df2[,c(rmvars)]<-NULL

#4) save data
    fname<-paste("ACS ",years[i]," (county).fst",sep="")
    fname2<-paste("ACS ",years[i]," (best county).fst",sep="")
    write.fst(df2,file.path(ACS.dir,fname),compress = 100)
    
#5) make and save small list of good acs zip variables
    df3<-df2[,c("county","county.name","ind.pop","sex.male","race.white","immigrant","marital.married","education.lessthanhs","poverty.numberabove2x","vacant","insurance.none","homevalue.median","income.median","income.median.house","age.median","gini")]
    write.fst(df3,file.path(ACS.dir,fname2),compress = 100)
    print(i)
    }
    i=2
    