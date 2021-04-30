# Analysis of monthly revisions in the "real time" tables
# November 12, 2020

setwd("/Users/philipsmith/Documents/R/Revisions")

pkgs <- c("cansim","tidyverse","lubridate","anytime","ggthemes","ggpubr",
          "gridExtra","grid","gtable","reshape2","ggrepel","pracma",
          "seasonal","FactoMineR","RcppBDT","forecast","profvis",
          "RColorBrewer","WriteXLS","gdata","scales","ggpubr","ggtext",
          "tibble","simplecolors","stringr","data.table")
inst <- lapply(pkgs,library,character.only=TRUE)

table01_id <- "36-10-0491-01" # "Real-time" revisions to monthly GDP
file_refresh <- FALSE
save_file <- paste0(table01_id,".rds")
if(!file.exists(save_file)|file_refresh){
  table01 <- get_cansim(table01_id,refresh=file_refresh)
  saveRDS(table01,file=save_file)
} else {
  table01 <- readRDS(save_file)
}
# It takes a long time to execute the following statement
# because the data frame is very big (2 or 3 minutes)
View(table01) 

(a0 <- unique(table01$REF_DATE)) # Jan 1997 to date
(a1 <- unique(table01$GEO)) # Canada
(a2 <- unique(table01$`Seasonal adjustment`)) # "Seasonally adjusted at annual rates", "Trading-day adjusted"
(a3 <- unique(table01$Prices)) # "Chained (2012) dollars", "2012 constant prices"
(a4 <- unique(table01$`North American Industry Classification System (NAICS)`)) # 287 industries
(a5 <- unique(table01$Release)) # Release dates from March 03, 2015 to date

r0 <- filter(table01,`Seasonal adjustment`==
    "Seasonally adjusted at annual rates",Prices=="Chained (2012) dollars")
r0 <- select(r0,REF_DATE,NAICS=
    "North American Industry Classification System (NAICS)",Release,VALUE)
r0$REF_DATE <- as.Date(paste0(r0$REF_DATE,"-01"))
# Industry filtering is a good idea because the full r0 has over 5M rows
# After this filtering there are still over 1M rows
r1 <- filter(r0,REF_DATE>as.Date("2014-12-01"))
# After the next filtering there are less than 500 rows

Rev_table <- function(indST,indLG) {
  r1 <- filter(r1,NAICS==indLG)
  r1 <- rename(r1,"REF"="REF_DATE","VAL"="VALUE")
  r1 <- as.data.table(r1)
  r1 <- r1[,REL:=as.Date(Release,format="%B %d, %Y")]
  r1 <- r1[,DAY:=day(REL)]
  r1 <- r1[,MONTH:=month(REL)]
  r1 <- r1[,YEAR:=year(REL)]
  #r1[,.N,DAY] # Display the frequency for all days of the week
  # Result is that 1, 2 and 3 occur occasionally, 21, 22 and 23 sometimes (Xmas)
  # and 28, 29, 30 and 31 most of the time
  r1 <- r1[,.(REF,REL,VAL,DAY,MONTH,YEAR)]
  # Convert release dates to months (make DAY=1 and shift MONTH back if needed)
  # For example, release date of February 2, 2020 becomes January 2, 2020
  r1 <- r1[,MONTH:=ifelse(DAY==1 | DAY==2 | DAY==3 | DAY==4 | DAY==5,
    MONTH-1,MONTH)]
  # Now reconstruct the release date as the first of the month in that
  # possibly modified month
  r1 <- r1[,REL:=as.Date(paste0(YEAR,"-",MONTH,"-01"))]
  r1 <- r1[,.(REF,REL,VAL)]
  # Now construct the MoM growth rates
  do_per_REL <- function( DF ) {
    rng <- range( DF$REF ) # watch out for missing months?
    DF <- (   data.frame( REF = seq( rng[ 1 ], rng[ 2 ], by = "month" ) )
          %>% left_join( DF, by = "REF" )
          %>% arrange( REF )
          )
    with( DF
        , data.frame( REF2 = REF[ -1 ]
                    , VAL2 = 100 * diff( VAL ) / VAL[ -length( VAL ) ]
                    )
        )
  }
  df1 <- nest(r1, data = -REL )
  df1 <- rename(df1, REL2 = REL )
  df1 <- rowwise(df1)
  df1 <- mutate(df1, data = list( do_per_REL( data ) ) )
  df1 <- ungroup(df1)
  df1 <- unnest(df1, cols = "data" )
  df2 <- select(df1, REF2, REL2, VAL2 )
  df2 <- arrange(df2, REF2, desc( REL2 ), VAL2 )
  df4 <- df2[!is.na(df2$VAL2),]
  df4 <- filter(df4,REF2>=as.Date("2015-01-01"))
  if (indST=="ALL") {
    REF2 <- c(as.Date("2020-03-01"),as.Date("2020-04-01"),
      as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),
      as.Date("2020-08-01"),as.Date("2020-09-01"))
    REL2 <- c(as.Date("2020-04-01"),as.Date("2020-05-01"),
      as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
      as.Date("2020-09-01"),as.Date("2020-10-01"))
    VAL2 <- c(-9.0,-11.0,3.0,5.0,3.0,1.0,0.7)
    newdf4 <- data.frame(REF2,REL2,VAL2)
    df4 <- rbind(df4,newdf4)
    df4 <- arrange(df4,REF2,REL2)
  }
  df5 <- as.data.table(df4)
  df5 <- df5[,range:=max(VAL2)-min(VAL2),by=REF2]
  df5 <- df5[,REF2:=paste0(as.character(REF2),"\n Range = ",
    as.character(round(range,2)))]
  c1 <- ggplot(df5,aes(x=REL2,y=VAL2,group=REF2))+
    geom_line()+
    labs(title=paste0("Monthly GDP, ",indLG),
         subtitle=paste0("Revisions to monthly growth rates, Fisher chained ",
           "indexes, seasonally adjusted\nEach chart shows the ",
           "evolution of the growth rate for a particular ",
           "reference month from its initial estimate to its current estimate"),
         caption=paste0("Source: Statistics Canada table ",table01_id,
                        ". @PhilSmith26"),
         x="",y="Monthly % change") +
    theme(strip.text.x=element_text(size=6,colour="black",face="bold"),
          strip.background=element_rect(colour="black",fill=sc("violet1")),
          panel.background=element_rect(fill=sc("brightgreen1"),colour="black")) +
    theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
    theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
    theme(plot.title = element_text(size=16,face="bold")) +
    theme(axis.text.x = element_text(angle=30,hjust=1,size=6)) +
    theme(axis.text.y = element_text(size=6))+
    facet_wrap(~REF2,scales="free_y")
  c1
  ggsave(paste0("GDPrev_",indST,".jpg"),c1,height=8,
    width=10,dpi=600)
  return(df4)
}  

indST <- "ALL"
indLG <- "All industries [T001]" 
r2 <- Rev_table(indST,indLG)

# Lowest-level industries in monthly GDP (36-10-0434-01)
Lowest_level_industries <- c(
  "Crop production (except cannabis) [111X]",                                                                   
  "Cannabis production (licensed) [111CL]",                                                                     
  "Cannabis production (unlicensed) [111CU]",                                                                   
  "Animal production [112]",                                                                                    
  "Forestry and logging [113]",                                                                                 
  "Fishing, hunting and trapping [114]",                                                                        
  "Support activities for agriculture and forestry [115]",                                                      
  "Oil and gas extraction (except oil sands) [21111]",                                                          
  "Oil sands extraction [21114]",                                                                               
  "Coal mining [2121]",                                                                                         
  "Metal ore mining [2122]",                                                                                    
  "Iron ore mining [21221]",                                                                                    
  "Gold and silver ore mining [21222]",                                                                         
  "Copper, nickel, lead and zinc ore mining [21223]",                                                           
  "Other metal ore mining [21229]",                                                                             
  "Stone mining and quarrying [21231]",                                                                         
  "Sand, gravel, clay, and ceramic and refractory minerals mining and quarrying [21232]",
  "Other non-metallic mineral mining and quarrying [21239]",                                                    
  "Potash mining [212396]",                                                                                     
  "Other non-metallic mineral mining and quarrying (except potash) [21239X]",
  "Support activities for mining and oil and gas extraction [213]",                                             
  "Electric power generation, transmission and distribution [2211]",                                            
  "Natural gas distribution [2212]",                                                                            
  "Water, sewage and other systems [2213]",                                                                     
  "Residential building construction [23A]",                                                                    
  "Non-residential building construction [23B]",                                                                
  "Repair construction [23D]",                                                                                  
  "Engineering and other construction activities [23X]",                                                        
  "Animal food manufacturing [3111]",                                                                           
  "Grain and oilseed milling [3112]",                                                                           
  "Sugar and confectionery product manufacturing [3113]",                                                       
  "Fruit and vegetable preserving and specialty food manufacturing [3114]",                                     
  "Dairy product manufacturing [3115]",                                                                         
  "Meat product manufacturing [3116]",                                                                          
  "Seafood product preparation and packaging [3117]",                                                           
  "Bakeries and tortilla manufacturing [3118]",                                                                 
  "Other food manufacturing [3119]",                                                                            
  "Soft drink and ice manufacturing [31211]",                                                                   
  "Breweries [31212]",                                                                                          
  "Wineries and distilleries [3121A]",                                                                          
  "Tobacco manufacturing [3122]",                                                                               
  "Textile and textile product mills [31A]",                                                                    
  "Clothing and leather and allied product manufacturing [31B]",                                                
  "Sawmills and wood preservation [3211]",                                                                      
  "Veneer, plywood and engineered wood product manufacturing [3212]",                                           
  "Other wood product manufacturing [3219]",                                                                    
  "Pulp, paper and paperboard mills [3221]",                                                                    
  "Converted paper product manufacturing [3222]",                                                               
  "Printing and related support activities [323]",                                                              
  "Petroleum refineries [32411]",                                                                               
  "Petroleum and coal products manufacturing (except petroleum refineries) [3241A]",                            
  "Basic chemical manufacturing [3251]",                                                                        
  "Resin, synthetic rubber, and artificial and synthetic fibres and filaments manufacturing [3252]",            
  "Pesticide, fertilizer and other agricultural chemical manufacturing [3253]",                                 
  "Pharmaceutical and medicine manufacturing [3254]",                                                           
  "Paint, coating and adhesive manufacturing [3255]",                                                           
  "Soap, cleaning compound and toilet preparation manufacturing [3256]",                                        
  "Other chemical product manufacturing [3259]",                                                                
  "Plastic product manufacturing [3261]",                                                                       
  "Rubber product manufacturing [3262]",                                                                        
  "Cement and concrete product manufacturing [3273]",                                                           
  "Non-metallic mineral product manufacturing (except cement and concrete products) [327A]",
  "Iron and steel mills and ferro-alloy manufacturing [3311]",                                                  
  "Steel product manufacturing from purchased steel [3312]",                                                    
  "Alumina and aluminum production and processing [3313]",                                                      
  "Non-ferrous metal (except aluminum) production and processing [3314]",                                       
  "Foundries [3315]",                                                                                           
  "Forging and stamping [3321]",                                                                                
  "Architectural and structural metals manufacturing [3323]",                                                   
  "Boiler, tank and shipping container manufacturing [3324]",                                                   
  "Hardware manufacturing [3325]",                                                                              
  "Spring and wire product manufacturing [3326]",                                                               
  "Machine shops, turned product, and screw, nut and bolt manufacturing [3327]",                                
  "Coating, engraving, heat treating and allied activities [3328]",                                             
  "Cutlery, hand tools and other fabricated metal product manufacturing [332A]",                                
  "Agricultural, construction and mining machinery manufacturing [3331]",                                       
  "Industrial machinery manufacturing [3332]",                                                                  
  "Commercial and service industry machinery manufacturing [3333]",                                             
  "Ventilation, heating, air-conditioning and commercial refrigeration equipment manufacturing [3334]",
  "Metalworking machinery manufacturing [3335]",                                                                
  "Engine, turbine and power transmission equipment manufacturing [3336]",                                      
  "Other general-purpose machinery manufacturing [3339]",                                                       
  "Computer and peripheral equipment manufacturing [3341]",                                                     
  "Electronic product manufacturing [334B]",                                                                    
  "Communications equipment manufacturing [3342]",                                                              
  "Semiconductor and other electronic component manufacturing [3344]",                                          
  "Other electronic product manufacturing [334A]",                                                              
  "Electric lighting equipment manufacturing [3351]",                                                           
  "Household appliance manufacturing [3352]",                                                                   
  "Electrical equipment manufacturing [3353]",                                                                  
  "Other electrical equipment and component manufacturing [3359]",                                              
  "Motor vehicle manufacturing [3361]",                                                                         
  "Motor vehicle body and trailer manufacturing [3362]",                                                        
  "Motor vehicle parts manufacturing [3363]",                                                                   
  "Aerospace product and parts manufacturing [3364]",                                                           
  "Miscellaneous transportation equipment manufacturing [336W]",                                                
  "Railroad rolling stock manufacturing [3365]",                                                                
  "Ship and boat building [3366]",                                                                              
  "Other transportation equipment manufacturing [3369]",                                                        
  "Household and institutional furniture and kitchen cabinet manufacturing [3371]",                             
  "Office furniture (including fixtures) manufacturing [3372]",                                                 
  "Other furniture-related product manufacturing [3379]",                                                       
  "Medical equipment and supplies manufacturing [3391]",                                                        
  "Other miscellaneous manufacturing [3399]",                                                                   
  "Farm product wholesaler-distributors [411]",                                                                 
  "Petroleum product wholesaler-distributors [412]",                                                            
  "Food, beverage and tobacco wholesaler-distributors [413]",                                                   
  "Personal and household goods wholesaler-distributors [414]",                                                 
  "Motor vehicle and parts wholesaler-distributors [415]",                                                      
  "Building material and supplies wholesaler-distributors [416]",                                               
  "Machinery, equipment and supplies wholesaler-distributors [417]",                                            
  "Miscellaneous wholesaler-distributors [418]",                                                                
  "Wholesale electronic markets, and agents and brokers [419]",                                                 
  "Motor vehicle and parts dealers [441]",                                                                      
  "Furniture and home furnishings stores [442]",                                                                
  "Electronics and appliance stores [443]",                                                                     
  "Building material and garden equipment and supplies dealers [444]",                                          
  "Food and beverage stores [445]",                                                                             
  "Health and personal care stores [446]",                                                                      
  "Gasoline stations [447]",                                                                                    
  "Clothing and clothing accessories stores [448]",                                                             
  "Sporting goods, hobby, book and music stores [451]",                                                         
  "General merchandise stores [452]",                                                                           
  "Miscellaneous store retailers (except cannabis) [453A]",                                                     
  "Cannabis stores (licensed) [453BL]",                                                                         
  "Cannabis stores (unlicensed) [453BU]",                                                                       
  "Non-store retailers [454]",                                                                                  
  "Air transportation [481]",                                                                                   
  "Rail transportation [482]",                                                                                  
  "Water transportation [483]",                                                                                 
  "Truck transportation [484]",                                                                                 
  "Urban transit systems [4851]",                                                                               
  "Taxi and limousine service [4853]",                                                                          
  "Other transit and ground passenger transportation and scenic and sightseeing transportation [48A]",
  "Pipeline transportation of natural gas [4862]",                                                              
  "Crude oil and other pipeline transportation [486A]",                                                         
  "Support activities for transportation [488]",                                                                
  "Postal service [491]",                                                                                       
  "Couriers and messengers [492]",                                                                              
  "Warehousing and storage [493]",                                                                              
  "Publishing industries (except Internet) [511]",                                                              
  "Motion picture and sound recording industries [512]",                                                        
  "Radio and television broadcasting [5151]",                                                                   
  "Pay and specialty television [5152]",                                                                        
  "Telecommunications [517]",                                                                                   
  "Data processing, hosting, and related services [518]",                                                       
  "Other information services [519]",                                                                           
  "Local credit unions [52213]",                                                                                
  "Banking, monetary authorities and other depository credit intermediation [52BX]",
  "Non-depository credit intermediation [5222]",                                                                
  "Activities related to credit intermediation [5223]",                                                         
  "Insurance carriers [5241]",                                                                                  
  "Agencies, brokerages and other insurance related activities [5242]",                                         
  "Financial investment services, funds and other financial vehicles [52A]",                                    
  "Lessors of real estate [5311]",                                                                              
  "Owner-occupied dwellings [5311A]",                                                                           
  "Offices of real estate agents and brokers and activities related to real estate [531A]",
  "Automotive equipment rental and leasing [5321]",                                                             
  "Rental and leasing services (except automotive equipment) [532A]",                                           
  "Lessors of non-financial intangible assets (except copyrighted works) [533]",                                
  "Legal services [5411]",                                                                                      
  "Accounting, tax preparation, bookkeeping and payroll services [5412]",                                       
  "Architectural, engineering and related services [5413]",                                                     
  "Computer systems design and related services [5415]",                                                        
  "Advertising, public relations, and related services [5418]",                                                 
  "Other professional, scientific and technical services including scientific research and development [541B]",
  "Specialized design services [5414]",                                                                         
  "Management, scientific and technical consulting services [5416]",                                            
  "Scientific research and development services [5417]",                                                        
  "Other professional, scientific and technical services [5419]",                                               
  "Management of companies and enterprises [55]",                                                               
  "Office administrative services [5611]",                                                                      
  "Employment services [5613]",                                                                                 
  "Business support services [5614]",                                                                           
  "Travel arrangement and reservation services [5615]",                                                         
  "Investigation and security services [5616]",                                                                 
  "Services to buildings and dwellings [5617]",                                                                 
  "Facilities and other support services [561A]",                                                               
  "Waste management and remediation services [562]",                                                            
  "Elementary and secondary schools [6111]",                                                                    
  "Community colleges and C.E.G.E.P.s [6112]",                                                                  
  "Universities [6113]",                                                                                        
  "Other educational services [611A]",                                                                          
  "Ambulatory health care services [621]",                                                                      
  "Hospitals [622]",                                                                                            
  "Nursing and residential care facilities [623]",                                                              
  "Social Assistance [624]",                                                                                    
  "Health care [62X]",                                                                                          
  "Performing arts, spectator sports and related industries, and heritage institutions [71A]",
  "Gambling industries [7132]",                                                                                 
  "Amusement and recreation industries [713A]",                                                                 
  "Accommodation services [721]",                                                                               
  "Food services and drinking places [722]",                                                                    
  "Repair and maintenance [811]",                                                                               
  "Personal and laundry services [812]",                                                                        
  "Religious, grant-making, civic, and professional and similar organizations [813]",                           
  "Private households [814]",                                                                                   
  "Defence services [9111]",                                                                                    
  "Federal government public administration (except defence) [911A]",                                           
  "Provincial and territorial public administration [912]",                                                     
  "Local, municipal and regional public administration [913]",                                                  
  "Aboriginal public administration [914]")
