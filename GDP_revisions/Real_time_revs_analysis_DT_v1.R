# Analysis of monthly revisions
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

r0 <- filter(table01,`Seasonal adjustment`=="Seasonally adjusted at annual rates",
  Prices=="Chained (2012) dollars")
r0 <- select(r0,REF_DATE,NAICS="North American Industry Classification System (NAICS)",
  Release,VALUE)
r0$REF_DATE <- as.Date(paste0(r0$REF_DATE,"-01"))
# This filtering is a good idea because the full r0 has over 5M rows
# After this filtering there are still over 1M rows
#r0 <- filter(r0,REF_DATE>as.Date("2014-12-01"))
# After the next filtering there are less than 500 rows
r0 <- filter(r0,NAICS=="All industries [T001]")
r0 <- as.data.table(r0)
r0 <- r0[,REL_DATE:=as.Date(Release,format="%B %d, %Y")]
r0 <- r0[,DAY:=day(REL_DATE)]
r0 <- r0[,MONTH:=month(REL_DATE)]
r0 <- r0[,YEAR:=year(REL_DATE)]
r0[,.N,DAY] # Display the frequency for all days of the week
# Result is that 1, 2 and 3 occur occasionally, 21, 22 and 23 sometimes (Xmas)
# and 28, 29, 30 and 31 most of the time
r0 <- r0[,.(REF_DATE,REL_DATE,VALUE,DAY,MONTH,YEAR)]
# Convert release dates to months (make DAY=1 and shift MONTH back if needed)
# For example, release date of February 2, 2020 becomes January 2, 2020
r0 <- r0[,MONTH:=ifelse(DAY==1 | DAY==2 | DAY==3 | DAY==4 | DAY==5,
  MONTH-1,MONTH)]
# Now reconstruct the release date as the first of the month in that
# possibly modified month
r0 <- r0[,REL_DATE:=as.Date(paste0(YEAR,"-",MONTH,"-01"))]
r0 <- r0[,.(REF_DATE,REL_DATE,VALUE)]
# Now construct the MoM growth rates
do_per_REL <- function( DF ) {
  rng <- range( DF$REF_DATE ) # watch out for missing months?
  DF <- (   data.frame( REF_DATE = seq( rng[ 1 ], rng[ 2 ], by = "month" ) )
        %>% left_join( DF, by = "REF_DATE" )
        %>% arrange( REF_DATE )
        )
  with( DF
      , data.frame( REFF2 = REF_DATE[ -1 ]
                  , VALL2 = 100 * diff( VALUE ) / VALUE[ -length( VALUE ) ]
                  )
      )
}
df1 <- nest(r0, data = -REL_DATE )
df1 <- rename(df1, RELL2 = REL_DATE )
df1 <- rowwise(df1)
df1 <- mutate(df1, data = list( do_per_REL( data ) ) )
df1 <- ungroup(df1)
df1 <- unnest(df1, cols = "data" )
df2 <- select(df1, REFF2, RELL2, VALL2 )
df2 <- arrange(df2, REFF2, desc( RELL2 ), VALL2 )
df2

df3 <- filter(df2,REFF2==as.Date("2017-01-01"))
df3 <- df3[!is.na(df3$VALL2),]
ggplot(df3)+
  geom_line(aes(x=RELL2,y=VALL2))

df4 <- df2[!is.na(df2$VALL2),]
df4 <- filter(df4,REFF2>=as.Date("2015-01-01"))
c1 <- ggplot(df4)+
  geom_line(aes(x=RELL2,y=VALL2,group=1))+
  labs(title="GDP, all industries, revisions to monthly growth rates",
       subtitle=paste0("Fisher chained indexes, seasonally adjusted\n",
         "Each chart shows the evolution of the growth rate for a particular ",
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
  facet_wrap(~REFF2,scales="free_y")
c1
ggsave("GDPrev_all_ind.jpg",c1,height=8,
  width=10,dpi=600)
