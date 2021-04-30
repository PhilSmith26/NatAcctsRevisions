# Analysis of monthly revisions in the "real time" tables
# GDP, seasonally adjusted
# November 18, 2020

setwd("/Users/philipsmith/Documents/R/Revisions/GDP_revisions")

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
# View(table01) 

(a0 <- unique(table01$REF_DATE)) # Jan 1992 to date
(a1 <- unique(table01$GEO)) # provs and terr - no Canada
(a3 <- unique(table01$`North American Industry Classification System (NAICS)`)) # 287 industries
(a4 <- unique(table01$Release)) # Release dates from March 03, 2015 to date
(a5 <- unique(table01$`Seasonal adjustment`))
(a6 <- unique(table01$Prices))

r0 <- filter(table01,`Seasonal adjustment`=="Seasonally adjusted at annual rates",
  Prices=="Chained (2012) dollars")
r0 <- select(r0,REF_DATE,NAICS=
    "North American Industry Classification System (NAICS)",Release,VALUE)
r0$REF_DATE <- as.Date(paste0(r0$REF_DATE,"-01"))
r0 <- filter(r0,REF_DATE>as.Date("2014-12-01"))

#================================================================================
# The LUP function retrieves values from the r2 data frame, for a given
# reference date and a given LEAD (LEAD=0 for the first estimate)
LUP <- function(REF_value,LEAD) { # lookup function
  if (class(REF_value)=="character") REF_value <- as.Date(REF_value)
  REL_value <- as.Date(REF_value %m+% months(2+LEAD))
  df <- filter(r2,REF2_pure==REF_value & REL2==REL_value)
  return(df$VAL2)
}
#================================================================================
LUPV <- function(REF_vector,LEAD) { # lookup vector function
  if (class(REF_vector)=="character") REF_vector <- as.Date(REF_vector)
  REL_vector <- as.Date(REF_vector %m+% months(2+LEAD))
  VAL_vector <- numeric()
  for (i in 1:length(REF_vector)) {
    VAL_vector[i] <- LUP(REF_vector[i],LEAD)
  }
  return(VAL_vector)
}
#================================================================================
RMS <- function(x) {
  rms <- sqrt(mean((x-mean(x))^2))
}
#================================================================================
do_per_REL <- function(DF) {
    # range() returns a vector containing the minimum and maximum of 
    # all the given arguments.
    rng <- range(DF$REF) # watch out for missing months?
    DF <- (data.frame(REF=seq(rng[1],rng[2],by="month"))
      # left_join() returns all rows from x, and all columns from x and y. 
      # Rows in x with no match in y will have NA values in the new columns. 
      # If there are multiple matches between x and y, all combinations of 
      # the matches are returned.
      # The next statement compares the input DF to the sequential one
      # just created and adds NAs if any are missing in DF
      %>% left_join(DF,by="REF")
          %>% arrange(REF) # arrange REF from first to last reference period
          )
    # with() evaluates an R expression in an environment constructed from data, 
    # possibly modifying (a copy of) the original data.
    # REF2=REF[-1] just drops the first item in the vector, which is
    # necessary because the % change calculation shortens the vector
    with(DF,data.frame(REF2=REF[-1],VAL2=100*diff(VAL)/VAL[-length(VAL)]))
  }
#================================================================================
Rev_table <- function(indLG,indST) {
  r1 <- filter(r0,NAICS==indLG) # retain only data for chosen industry
  indST <- sub(".*?\\[","[",indLG)
  r1 <- rename(r1,"REF"="REF_DATE","VAL"="VALUE") # use shorter variable names
  r1 <- as.data.table(r1) # convert to data.table
  r1 <- r1[,REL:=as.Date(Release,format="%B %d, %Y")] # convert REL to a date
  r1 <- r1[,DAY:=day(REL)]       # extract day
  r1 <- r1[,MONTH:=month(REL)]   # extract month
  r1 <- r1[,YEAR:=year(REL)]     # extract year
  # r1[,.N,DAY] # Display the frequency for all days of the week
  # Result is that 1, 2 and 3 occur occasionally, 21, 22 and 23 sometimes (Xmas)
  # and 28, 29, 30 and 31 most of the time
  r1 <- r1[,.(REF,REL,VAL,DAY,MONTH,YEAR)] # select only the variables needed
  # Convert release dates to months (make DAY=1 and shift MONTH back if needed)
  # For example, release date of February 2, 2020 becomes January 1, 2020
  r1 <- r1[,MONTH:=ifelse(DAY==1 | DAY==2 | DAY==3 | DAY==4 | DAY==5,
    MONTH-1,MONTH)]
  # Now reconstruct the release date as the first of the month in that
  # possibly modified month
  r1 <- r1[,REL:=as.Date(paste0(YEAR,"-",MONTH,"-01"))]
  r1 <- r1[,.(REF,REL,VAL)] # de-select DAY, MONTH and YEAR variables
  # Nesting creates a list-column of data frames; unnesting flattens it back 
  # out into regular columns. Nesting is implicitly a summarising operation: 
  # you get one row for each group defined by the non-nested columns. This is 
  # useful in conjunction with other summaries that work with whole datasets, 
  # most notably models.
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
  if (indST=="[44-45]") {
    REF2 <- c(as.Date("2020-04-01"),
      as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),
      as.Date("2020-08-01"),as.Date("2020-09-01"))
    REL2 <- c(as.Date("2020-05-01"),
      as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
      as.Date("2020-09-01"),as.Date("2020-10-01"))
    VAL2 <- c(-15.6,19.1,24.5,0.7,1.1,0.0)
    newdf4 <- data.frame(REF2,REL2,VAL2)
    df4 <- rbind(df4,newdf4)
    df4 <- arrange(df4,REF2,REL2)
  }
  df5 <- as.data.table(df4)
  df5 <- df5[,range:=max(VAL2)-min(VAL2),by=REF2]
  df5 <- df5[,REF2_pure:=REF2]
  df5 <- df5[,REF2:=paste0(str_sub(as.character(REF2),1,7),"\n Range = ",
    as.character(round(range,2)))]
  c1 <- ggplot(df5,aes(x=REL2,y=VAL2,group=REF2))+
    geom_line()+
    labs(title=as.character(indLG),
         subtitle=paste0("Revisions to monthly growth rates, ",
           "seasonally adjusted\nEach chart shows the ",
           "evolution of the growth rate for a particular ",
           "reference month from its initial estimate to its current estimate"),
         caption=paste0("Source: Statistics Canada table ",table01_id,
                        ". @PhilSmith26"),
         x="",y="Monthly % change") +
    theme(strip.text.x=element_text(size=4,colour="black",face="bold"),
          strip.background=element_rect(colour="black",fill=sc("violet1")),
          panel.background=element_rect(fill=sc("brightgreen1"),colour="black")) +
    theme(panel.grid.minor=element_line(colour="lightgrey",size=0.5)) +
    theme(panel.grid.major=element_line(colour="grey",size=0.5)) +
    theme(plot.title = element_text(size=16,face="bold")) +
    theme(plot.subtitle = element_text(size=6,face="bold")) +
    theme(axis.text.x = element_text(angle=30,hjust=1,size=6)) +
    theme(axis.text.y = element_text(size=6))+
    facet_wrap(~REF2,scales="free_y")
  ggsave(paste0(indST,"_revision_paths.jpg"),c1,height=8,
    width=10,dpi=600)
  print(c1)
  return(df5)
}  

#pdf("GDP_revisions.pdf", paper="letter")
#for (indLG in unique(r0$NAICS)) {
pdf("GDP_revisions_test.pdf", paper="letter")
for (indLG in c("All industries [T001]",
  "Goods-producing industries [T002]",
  "Service-producing industries [T003]")) {
  indST <- sub(".*?\\[","[",indLG)
  r2 <- Rev_table(indLG,indST)
  rng <- range(str_sub(r2$REF2,1,10))
  r3 <- data.frame(REF=seq.Date(as.Date(paste0(str_sub(rng[1],1,7),"-01")),
    as.Date(paste0(str_sub(rng[2],1,7),"-01")),by="month"))
  r3 <- filter(r3,REF<=as.Date("2019-12-01"))
  r3 <- mutate(r3,VAL00=round(LUPV(REF,0),2),
                  VAL01=round(LUPV(REF,1),2),
                  VAL02=round(LUPV(REF,2),2),
                  VAL03=round(LUPV(REF,3),2),
                  VAL04=round(LUPV(REF,4),2),
                  REV01=round(VAL01-VAL00,2),
                  REV02=round(VAL02-VAL01,2),
                  REV03=round(VAL03-VAL02,2),
                  REV04=round(VAL04-VAL03,2))
  (MEAN01 <- mean(r3$REV01))
  (MEAN02 <- mean(r3$REV02))
  (MEAN03 <- mean(r3$REV03))
  (MEAN04 <- mean(r3$REV04))
  (MABS01 <- mean(abs(r3$REV01)))
  (MABS02 <- mean(abs(r3$REV02)))
  (MABS03 <- mean(abs(r3$REV03)))
  (MABS04 <- mean(abs(r3$REV04)))
  (RMS01 <- RMS(r3$REV01))
  (RMS02 <- RMS(r3$REV02))
  (RMS03 <- RMS(r3$REV03))
  (RMS04 <- RMS(r3$REV04))
  p0 <- ggplot(r3)+
    ggtitle(paste0(indLG,
      "\nPoint plot for 1-, 2-, 3- and 4-month-later growth rate revisions",
      "\nRed = 1-month-later, blue = 2-months-later,\n",
      "purple = 3-months-later, orange = 4-months-later"))+
    geom_point(aes(x=REF,y=REV01),colour="red")+
    geom_point(aes(x=REF,y=REV02),colour="blue")+
    geom_point(aes(x=REF,y=REV03),colour="purple")+
    geom_point(aes(x=REF,y=REV04),colour="chocolate2")+
    labs(x="Reference months, February 2015 to December 2019",
         y="Percentage point revisions to growth rates",
      caption=paste0("The red dots are the second monthly growth rate estimate minus ",
      "the initial monthly growth rate estimate.\n",
                     "The blue dots are the third monthly growth rate estimate minus ",
      "the second monthly growth rate estimate. \n",
                     "Similarly for the purple and orange dots. Source: Statistics ",
      "Canada table ",table01_id,". @PhilSmith26"))+
    geom_hline(yintercept=0)
  print(p0)
  p1 <- ggplot(r3)+
    ggtitle(paste0(indLG,
      "\nEstimated probability density for one-month-later growth rate revisions",
      "\nMean revision = ",round(MEAN01,2),
      "\nMean absolute revision = ",round(MABS01,2),
      "\nRoot mean squared revision = ",round(RMS01,2)))+
    geom_density(aes(x=REV04),fill="hotpink2",colour="black",alpha=1.0)+
    geom_density(aes(x=REV03),fill="darkolivegreen2",colour="black",alpha=1.0)+
    geom_density(aes(x=REV02),fill="cadetblue2",colour="black",alpha=1.0)+
    geom_density(aes(x=REV01),fill="bisque",colour="black",alpha=1.0)+
    labs(x="Revisions to monthly growth rates, in percentage points",y="Probability density",
      caption=paste0("Source: Statistics Canada table ",table01_id,". @PhilSmith26"))+
    geom_vline(xintercept=0,linetype="dotted")  
  print(p1)
}  
dev.off()
