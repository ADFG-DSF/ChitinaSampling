## packages, etc

# library(readxl)      # for reading from .xlsx - decided not to use
library(tidyverse)   # for some data operations
library(janitor)     # for cleaning variable names


## read raw data

years <- 2019:2022

# All data sheets were read and structured as lists, with each list element
# representing the data from a given year.
# Note: xlsx sheets were converted to csv for more reliable reading
sockeye <- list()   # sockeye raw sampling data from all years
chinook <- list()   # chinook raw sampling data from all years
GSD <- list()   # GSD harvest reported from all years
CSD <- list()   # CSD harvest reported from all years

for(iyear in seq_along(years)) {
  sockeye[[iyear]] <- read.csv(file=paste0("Data/", years[iyear], " Raw Data - Sockeye.csv")) %>% 
    clean_names %>% remove_empty(which="cols")
  chinook[[iyear]] <- read.csv(file=paste0("Data/", years[iyear], " Raw Data - Chinook.csv")) %>% 
    clean_names %>% remove_empty(which="cols")
  GSD[[iyear]] <- read.csv(file=paste0("Data/", years[iyear], " GSD HARVEST Reported.csv")) %>% 
    clean_names %>% remove_empty(which="cols")
  CSD[[iyear]] <- read.csv(file=paste0("Data/", years[iyear], " CSD HARVEST Reported.csv")) %>% 
    clean_names %>% remove_empty(which="cols")
}



## data checks & cleaning

## make sure column names are consistent

# this is a quick function that returns which column names are used in which
# year, in order to help identify inconsistencies.
checknames <- function(x) {  
  namelist <- lapply(x, names)
  uniquenames <- unique(unlist(namelist))
  namemat <- matrix(nrow=length(uniquenames), ncol=4)
  for(i in 1:4) namemat[,i] <- uniquenames %in% namelist[[i]]
  rownames(namemat) <- uniquenames
  return(namemat)
}

## for each data list, running checknames() and renaming columns as necessary...

# checknames(x=sockeye)
sockeye[[1]] <- sockeye[[1]] %>% rename(length_me_a_fin=length_me_afin) %>%
  rename(subsample_row=sub_sample_row) %>%
  rename(subsample_fish=sub_sample_fish) %>%
  rename(fishery_fed_or_state_f_s=fishery_fed_state)
checknames(x=sockeye)

# checknames(x=chinook)
chinook[[3]] <- chinook[[3]] %>% rename(tag_number=nve_yellow_tag_number) %>%
  rename(notes=notes_include_radio_tag_in_this_column)
chinook[[4]] <- chinook[[4]] %>% rename(tag_number=nve_yellow_tag_number) %>%
  rename(notes=notes_include_radio_tag_in_this_column)
checknames(x=chinook)

checknames(x=GSD)

# checknames(x=CSD)
CSD[[4]] <- CSD[[4]] %>% rename(harvdate=harvest_date) %>% 
  rename(red=reds) %>% 
  rename(king=kings)
CSD[[1]] <- CSD[[1]] %>% rename(red=sockeye)
CSD[[2]] <- CSD[[2]] %>% rename(red=sockeye)
checknames(x=CSD)


## A relatively small number of rows (~1% of fish) were missing sample_week in  
## REPORTED HARVEST (GSD and CSD) because dates fell outside of the weeks defined
## for ASL/otolith sampling.
## ASSIGNING THESE TO EARLIEST/LATEST SAMPLE WEEK FOR THE RESPECTIVE YEAR, depending
## on whether dates were before/after weeks were defined.
## Note that these are only used for ASL stratification scheme, in which strata 
## are defined by multiple weeks.
## This is done so that totals will be consistent among tables/publications.

# checking consistency in harvdate
sapply(GSD, function(x) head(x$harvdate))
sapply(CSD, function(x) head(x$harvdate))
# all are formatted consistently!

# reformatting dates as dates
for(i in 1:length(years)) {
  GSD[[i]]$harvdate <- as.Date(GSD[[i]]$harvdate, format="%m/%d/%Y")
  CSD[[i]]$harvdate <- as.Date(CSD[[i]]$harvdate, format="%m/%d/%Y")
}

# making sure it worked!
for(i in 1:length(years)) {
  summary(GSD[[i]]$harvdate) %>% print
  summary(CSD[[i]]$harvdate) %>% print
}

# extracting month, and checking NA values for sample_week
for(i in 1:length(years)) {
  # GSD[[i]]$month <- format(GSD[[i]]$harvdate, "%m")
  # with(GSD[[i]], table(month, sample_week, useNA="ifany")) %>% print
  CSD[[i]]$month <- format(CSD[[i]]$harvdate, "%m")
  with(CSD[[i]], table(month, sample_week, useNA="ifany")) %>% print
}

## I think I should just redefine sample_week - LOTS of missing entries for 2020
sample_weeks <- read.csv("Data/sample_weeks.csv")
firstdates <- lastdates <- matrix(nrow=nrow(sample_weeks), ncol=ncol(sample_weeks)-1)
for(i in 1:nrow(sample_weeks)) {
  for(j in 2:ncol(sample_weeks)) {
    firstdates[i, j-1] <- strsplit(sample_weeks[i,j], " - ")[[1]][1] %>% 
      paste0("/",years[j-1]) #%>%
    # as.Date(format="%m/%d/%Y")
    lastdates[i, j-1] <- strsplit(sample_weeks[i,j], " - ")[[1]][2] %>% 
      paste0("/",years[j-1]) %>% str_remove_all("b")#%>%
    # as.Date(format="%m/%d/%Y")
  }
}

for(i in 1:length(years)) {
  GSD[[i]]$sample_week2 <- as.numeric(cut(GSD[[i]]$harvdate, 
                                          c(min(GSD[[i]]$harvdate, na.rm=TRUE), 
                                            # the weird [] thing in the following line excludes 1 line if 2019 and 2 lines otherwise
                                            as.Date(firstdates[,i], format="%m/%d/%Y")[-(1:(1+(i!=1)))],
                                            max(GSD[[i]]$harvdate, na.rm=TRUE)),
                                          right=FALSE, include.lowest=TRUE)) - (i==1)  # the subtraction includes week zero for 2019
  with(GSD[[i]], table(sample_week2, sample_week, useNA="ifany")) %>% print
}
for(i in 1:length(years)) {
  CSD[[i]]$sample_week2 <- as.numeric(cut(CSD[[i]]$harvdate, 
                                          c(min(CSD[[i]]$harvdate, na.rm=TRUE), 
                                            # the weird [] thing in the following line excludes 1 line if 2019 and 2 lines otherwise
                                            as.Date(firstdates[,i], format="%m/%d/%Y")[-(1:(1+(i!=1)))],
                                            max(CSD[[i]]$harvdate, na.rm=TRUE)),
                                          right=FALSE, include.lowest=TRUE)) - (i==1)  # the subtraction includes week zero for 2019
  with(CSD[[i]], table(sample_week2, sample_week, useNA="ifany")) %>% print
}


## this is scary but I'm moving this to the ASL section!!

# ##### rewriting $sample_week from $sample_week2
# for(i in 1:length(years)) {
#   CSD[[i]]$sample_week <- CSD[[i]]$sample_week2
#   GSD[[i]]$sample_week <- GSD[[i]]$sample_week2
# }



## data check: tabulating reported harvest by sample week
# note: this only reports the existence of rows of data, NOT totals!!!
sapply(CSD, function(x) table(x$sample_week, useNA = "ifany"))
sapply(GSD, function(x) table(x$sample_week, useNA = "ifany"))

## noticed that week was missing for weeks 1-4 in 2020.  Backfilling this.
# first, telling R to interpret date field as dates...
CSD[[2]]$harvdate <- as.Date(CSD[[2]]$harvdate, format="%m/%d/%Y")
GSD[[2]]$harvdate <- as.Date(GSD[[2]]$harvdate, format="%m/%d/%Y")

# then, filling sample_week appropriately
CSD[[2]]$sample_week[CSD[[2]]$harvdate >= as.Date("2020-06-01") & CSD[[2]]$harvdate <= as.Date("2020-06-07")] <- 1
CSD[[2]]$sample_week[CSD[[2]]$harvdate >= as.Date("2020-06-08") & CSD[[2]]$harvdate <= as.Date("2020-06-14")] <- 2
CSD[[2]]$sample_week[CSD[[2]]$harvdate >= as.Date("2020-06-15") & CSD[[2]]$harvdate <= as.Date("2020-06-21")] <- 3
CSD[[2]]$sample_week[CSD[[2]]$harvdate >= as.Date("2020-06-22") & CSD[[2]]$harvdate <= as.Date("2020-06-28")] <- 4
with(CSD[[2]], table(harvdate, sample_week, useNA="ifany"))  # making sure it worked

GSD[[2]]$sample_week[GSD[[2]]$harvdate >= as.Date("2020-06-01") & GSD[[2]]$harvdate <= as.Date("2020-06-07")] <- 1
GSD[[2]]$sample_week[GSD[[2]]$harvdate >= as.Date("2020-06-08") & GSD[[2]]$harvdate <= as.Date("2020-06-14")] <- 2
GSD[[2]]$sample_week[GSD[[2]]$harvdate >= as.Date("2020-06-15") & GSD[[2]]$harvdate <= as.Date("2020-06-21")] <- 3
GSD[[2]]$sample_week[GSD[[2]]$harvdate >= as.Date("2020-06-22") & GSD[[2]]$harvdate <= as.Date("2020-06-28")] <- 4
with(GSD[[2]], table(harvdate, sample_week, useNA="ifany"))  # making sure it worked

##  tabulating reported harvest by sample week - AGAIN
# note: this only reports the existence of rows of data, NOT totals!!!
sapply(CSD, function(x) table(x$sample_week, useNA = "ifany"))
sapply(GSD, function(x) table(x$sample_week, useNA = "ifany"))



## Sockeye Salmon Hatchery Contribution

# tabulate otolith sampling by year, sample week, and origin
sockeye_allyrsdf <- rbind(cbind(select(sockeye[[1]], c("sample_week", "origin_h_w", "subdistrict")), year=2019),
                  cbind(select(sockeye[[2]], c("sample_week", "origin_h_w", "subdistrict")), year=2020),
                  cbind(select(sockeye[[3]], c("sample_week", "origin_h_w", "subdistrict")), year=2021),
                  cbind(select(sockeye[[4]], c("sample_week", "origin_h_w", "subdistrict")), year=2022))
sockeye_allyrstab <- as.array(table(sockeye_allyrsdf))
sockeye_allyrstab <- sockeye_allyrstab[, dimnames(sockeye_allyrstab)$origin_h_w %in% c("H", "W"), ,]
sockeye_allyrstab <- sockeye_allyrstab[, , dimnames(sockeye_allyrstab)$subdistrict %in% 
                                         c("Chitina Subdistrict", "Glennallen Subdistrict"), ]

# tabulate harvest by year and sample week
GSD_allyrsdf <- rbind(cbind(select(GSD[[1]], c("sample_week", "red")), year=2019),
                      cbind(select(GSD[[2]], c("sample_week", "red")), year=2020),
                      cbind(select(GSD[[3]], c("sample_week", "red")), year=2021),
                      cbind(select(GSD[[4]], c("sample_week", "red")), year=2022))
GSD_allyrstab <- array(dim=dim(sockeye_allyrstab)[c(1,4)])
for(i in 1:nrow(GSD_allyrstab)) {  # this is not very elegant but will work
  for(j in 1:ncol(GSD_allyrstab)) {
    GSD_allyrstab[i,j] <- sum(GSD_allyrsdf$red[GSD_allyrsdf$sample_week==dimnames(sockeye_allyrstab)$sample_week[i] &
                                                 GSD_allyrsdf$year==years[j]], na.rm=T)
  }
}
rownames(GSD_allyrstab) <- dimnames(sockeye_allyrstab)$sample_week
colnames(GSD_allyrstab) <- dimnames(sockeye_allyrstab)$year

CSD_allyrsdf <- rbind(cbind(select(CSD[[1]], c("sample_week", "red")), year=2019),
                      cbind(select(CSD[[2]], c("sample_week", "red")), year=2020),
                      cbind(select(CSD[[3]], c("sample_week", "red")), year=2021),
                      cbind(select(CSD[[4]], c("sample_week", "red")), year=2022))
CSD_allyrstab <- array(dim=dim(sockeye_allyrstab)[c(1,4)])
for(i in 1:nrow(CSD_allyrstab)) {  # this is not very elegant but will work
  for(j in 1:ncol(CSD_allyrstab)) {
    CSD_allyrstab[i,j] <- sum(CSD_allyrsdf$red[CSD_allyrsdf$sample_week==dimnames(sockeye_allyrstab)$sample_week[i] &
                                                 CSD_allyrsdf$year==years[j]], na.rm=T)
  }
}
rownames(CSD_allyrstab) <- dimnames(sockeye_allyrstab)$sample_week
colnames(CSD_allyrstab) <- dimnames(sockeye_allyrstab)$year


## calculations from operational plan!

# filling in variables
n_ji <- apply(sockeye_allyrstab, c(1,3,4), sum)
n_ji_GSD <- n_ji[, dimnames(n_ji)$subdistrict == "Glennallen Subdistrict", ]
n_ji_CSD <- n_ji[, dimnames(n_ji)$subdistrict == "Chitina Subdistrict", ]
o_ji <- sockeye_allyrstab[, dimnames(sockeye_allyrstab)$origin_h_w =="H" , , ]
o_ji_GSD <- o_ji[, dimnames(o_ji)$subdistrict == "Glennallen Subdistrict", ]
o_ji_CSD <- o_ji[, dimnames(o_ji)$subdistrict == "Chitina Subdistrict", ]

# this would be a logical place to expand total harvest
N_GSD <- GSD_allyrstab
N_CSD <- CSD_allyrstab

# equation 1
C_ji_GSD <- o_ji_GSD/n_ji_GSD*N_GSD
C_ji_CSD <- o_ji_CSD/n_ji_CSD*N_CSD

# equation 2
C_j_GSD <- colSums(C_ji_GSD, na.rm=T)
C_j_CSD <- colSums(C_ji_CSD, na.rm=T)
# NOTE: I don't think this is appropriate for 2022 GSD, given the missing data

# for table
p_GSD <- o_ji_GSD/n_ji_GSD
p_CSD <- o_ji_CSD/n_ji_CSD
vp_GSD <- p_GSD*(1-p_GSD)/(n_ji_GSD-1)
vp_CSD <- p_CSD*(1-p_CSD)/(n_ji_CSD-1)

# making a quick exploratory plot of the proportions
par(mfrow=c(1,2))
plot(NA, xlim=c(1,13), ylim=0:1, xlab="Sample week", ylab="Hatchery proportion", main="Glennallen Subdistrict")
for(i in 1:4) {
  lines(as.numeric(rownames(p_GSD)), p_GSD[,i], col=i+1, lwd=2)
  points(as.numeric(rownames(p_GSD)), p_GSD[,i], col=i+1, pch=16)
}
lines(as.numeric(rownames(p_GSD)), rowMeans(p_GSD, na.rm=T), lty=2)
legend("topleft",lwd=2,col=2:5,legend=years)
plot(NA, xlim=c(1,13), ylim=0:1, xlab="Sample week", ylab="Hatchery proportion", main="Chitina Subdistrict")
for(i in 1:4) {
  lines(as.numeric(rownames(p_CSD)), p_CSD[,i], col=i+1, lwd=2)
  points(as.numeric(rownames(p_CSD)), p_CSD[,i], col=i+1, pch=16)
}
lines(as.numeric(rownames(p_CSD)), rowMeans(p_CSD, na.rm=T), lty=2)
legend("topleft",lwd=2,col=2:5,legend=years)

# equations 3 & 4 from OP are not appropriate

# equation 3 split apart by strata (makes more sense)
V_C_ji_GSD <- (N_GSD^2)*o_ji_GSD/(n_ji_GSD^2)*(1-p_GSD)
V_C_ji_CSD <- (N_CSD^2)*o_ji_CSD/(n_ji_CSD^2)*(1-p_CSD)
V_C_h_GSD <- colSums(V_C_ji_GSD, na.rm=T)
V_C_h_CSD <- colSums(V_C_ji_CSD, na.rm=T)

SE_C_ji_GSD <- sqrt(V_C_ji_GSD)
SE_C_h_GSD <- sqrt(V_C_h_GSD)
SE_C_ji_CSD <- sqrt(V_C_ji_CSD)
SE_C_h_CSD <- sqrt(V_C_h_CSD)

printse <- function(est, se, percent=F, digits=2) {
  if(percent) {
    out <- paste0(round(100*est, digits), " (", round(100*se, digits),")%")
  } else {
    out <- paste0(round(est, digits), " (", round(se, digits),")")
  }
  return(out)
}

# making tables that mimic the structure of T5 and T6 in report
# T5_update <- data.frame(1:nrow(n_ji_GSD))
# for(i in seq_along(years)) {
#   T5_update[, 3*(i-1)+1] <- n_ji_GSD[,i]
#   T5_update[, 3*(i-1)+2] <- printse(p_GSD[,i], sqrt(vp_GSD[,i]), percent=T, digits=1)
#   T5_update[, 3*(i-1)+3] <- printse(C_ji_GSD[,i], SE_C_ji_GSD[,i], digits=0)
# }
# names(T5_update) <- paste(rep(years,each=3), rep(c("n","p","c")))
# lastrow <- rep("", ncol(T5_update))
# lastrow[3*(1:4)] <- printse(colSums(C_ji_GSD, na.rm=T), sqrt(colSums(V_C_ji_GSD, na.rm=T)), digits=0)
# T5_update <- rbind(T5_update, lastrow)
# rownames(T5_update) <- c(rownames(n_ji_GSD), "total")

T5_update <- data.frame(1:nrow(n_ji_GSD))
for(i in seq_along(years)) {
  T5_update[, 7*(i-1)+1] <- n_ji_GSD[,i]
  T5_update[, 7*(i-1)+2] <- printse(p_GSD[,i], sqrt(vp_GSD[,i]), percent=T, digits=1)
  T5_update[, 7*(i-1)+3] <- p_GSD[,i]
  T5_update[, 7*(i-1)+4] <- sqrt(vp_GSD[,i])
  T5_update[, 7*(i-1)+5] <- printse(C_ji_GSD[,i], SE_C_ji_GSD[,i], digits=0)
  T5_update[, 7*(i-1)+6] <- C_ji_GSD[,i]
  T5_update[, 7*(i-1)+7] <- SE_C_ji_GSD[,i]
}
names(T5_update) <- paste(rep(years,each=7), rep(c("n","p","p_raw","se_p_raw","c","c_raw","se_c_raw")))
lastrow <- rep("", ncol(T5_update))
lastrow[7*(1:4)-2] <- printse(colSums(C_ji_GSD, na.rm=T), sqrt(colSums(V_C_ji_GSD, na.rm=T)), digits=0)
lastrow[7*(1:4)-1] <- colSums(C_ji_GSD, na.rm=T)
lastrow[7*(1:4)-0] <- sqrt(colSums(V_C_ji_GSD, na.rm=T))
T5_update <- rbind(T5_update, lastrow)
rownames(T5_update) <- c(rownames(n_ji_GSD), "total")

# T6_update <- data.frame(1:nrow(n_ji_CSD))
# for(i in seq_along(years)) {
#   T6_update[, 3*(i-1)+1] <- n_ji_CSD[,i]
#   T6_update[, 3*(i-1)+2] <- printse(p_CSD[,i], sqrt(vp_CSD[,i]), percent=T, digits=1)
#   T6_update[, 3*(i-1)+3] <- printse(C_ji_CSD[,i], SE_C_ji_CSD[,i], digits=0)
# }
# names(T6_update) <- paste(rep(years,each=3), rep(c("n","p","c")))
# lastrow <- rep("", ncol(T6_update))
# lastrow[3*(1:4)] <- printse(colSums(C_ji_CSD, na.rm=T), sqrt(colSums(V_C_ji_CSD, na.rm=T)), digits=0)
# T6_update <- rbind(T6_update, lastrow)
# rownames(T6_update) <- c(rownames(n_ji_CSD), "total")

T6_update <- data.frame(1:nrow(n_ji_CSD))
for(i in seq_along(years)) {
  T6_update[, 7*(i-1)+1] <- n_ji_CSD[,i]
  T6_update[, 7*(i-1)+2] <- printse(p_CSD[,i], sqrt(vp_CSD[,i]), percent=T, digits=1)
  T6_update[, 7*(i-1)+3] <- p_CSD[,i]
  T6_update[, 7*(i-1)+4] <- sqrt(vp_CSD[,i])
  T6_update[, 7*(i-1)+5] <- printse(C_ji_CSD[,i], SE_C_ji_CSD[,i], digits=0)
  T6_update[, 7*(i-1)+6] <- C_ji_CSD[,i]
  T6_update[, 7*(i-1)+7] <- SE_C_ji_CSD[,i]
}
names(T6_update) <- paste(rep(years,each=7), rep(c("n","p","p_raw","se_p_raw","c","c_raw","se_c_raw")))
lastrow <- rep("", ncol(T6_update))
lastrow[7*(1:4)-2] <- printse(colSums(C_ji_CSD, na.rm=T), sqrt(colSums(V_C_ji_CSD, na.rm=T)), digits=0)
lastrow[7*(1:4)-1] <- colSums(C_ji_CSD, na.rm=T)
lastrow[7*(1:4)-0] <- sqrt(colSums(V_C_ji_CSD, na.rm=T))
T6_update <- rbind(T6_update, lastrow)
rownames(T6_update) <- c(rownames(n_ji_CSD), "total")



### Sockeye and Chinook Salmon ASL Composition

##### rewriting $sample_week from $sample_week2
for(i in 1:length(years)) {
  CSD[[i]]$sample_week <- CSD[[i]]$sample_week2
  GSD[[i]]$sample_week <- GSD[[i]]$sample_week2
}



## First testing if stratification is needed

# tabulating harvest by week
CSD_redbyweek <- sapply(CSD, function(x) tapply(x$red, x$sample_week, sum, na.rm=T))
GSD_redbyweek <- sapply(GSD, function(x) tapply(x$red, x$sample_week, sum, na.rm=T))
CSD_kingbyweek <- sapply(CSD, function(x) tapply(x$king, x$sample_week, sum, na.rm=T))
GSD_kingbyweek <- sapply(GSD, function(x) tapply(x$king, x$sample_week, sum, na.rm=T))

pooled_kingbyweek <- list()
for(i in 1:4) pooled_kingbyweek[[i]] <- CSD_kingbyweek[[i]] + GSD_kingbyweek[[i]]

# finding harvest quartiles (by date) to assign strata
findquartile <- function(x) {
  xx <- cumsum(x)/sum(x)
  xx25 <- which.min(abs(xx-.25))
  xx50 <- which.min(abs(xx-.5))
  xx75 <- which.min(abs(xx-.75))
  # return(c(xx25,xx50,xx75))
  sample_week <- as.numeric(names(x))
  strata <- rep(NA, length(sample_week))
  strata[1:xx25] <- 1
  strata[(xx25+1):xx50] <- 2
  strata[(xx50+1):xx75] <- 3
  strata[(xx75+1):length(strata)] <- 4
  return(data.frame(sample_week, strata))
}
CSD_redbyweek_quartiles <- lapply(CSD_redbyweek, findquartile)
GSD_redbyweek_quartiles <- lapply(GSD_redbyweek, findquartile)
CSD_kingbyweek_quartiles <- lapply(CSD_kingbyweek, findquartile)
GSD_kingbyweek_quartiles <- lapply(GSD_kingbyweek, findquartile)
pooled_kingbyweek_quartiles <- lapply(pooled_kingbyweek, findquartile)

# then assigning quartile-based strata to harvesting data.frames
for(i in 1:4) {
  CSD[[i]] <- CSD[[i]] %>% left_join(CSD_redbyweek_quartiles[[i]]) %>% rename(strata_red=strata)
  CSD[[i]] <- CSD[[i]] %>% left_join(CSD_kingbyweek_quartiles[[i]]) %>% rename(strata_king=strata)
  GSD[[i]] <- GSD[[i]] %>% left_join(GSD_redbyweek_quartiles[[i]]) %>% rename(strata_red=strata)
  GSD[[i]] <- GSD[[i]] %>% left_join(GSD_kingbyweek_quartiles[[i]]) %>% rename(strata_king=strata)
}
# append GSD & CSD, assign quartile-based strata
allSD <- list()
for(i in 1:4) {
  allSD[[i]] <- data.frame(sample_week=c(CSD[[i]]$sample_week, GSD[[i]]$sample_week),
                           king=c(CSD[[i]]$king, GSD[[i]]$king))
  allSD[[i]] <- allSD[[i]] %>% left_join(pooled_kingbyweek_quartiles[[i]]) %>% rename(strata_king_pooled=strata)
}

# assigning quartile-based strata to sampling data.frames ==> first separating by fishery
sockeye_CSD <- sapply(sockeye, function(x) subset(x, subdistrict=="Chitina Subdistrict"))
sockeye_GSD <- sapply(sockeye, function(x) subset(x, subdistrict=="Glennallen Subdistrict"))
chinook_CSD <- sapply(chinook, function(x) subset(x, subdistrict=="CSD"))
chinook_GSD <- sapply(chinook, function(x) subset(x, subdistrict=="GSD"))

# then assigning quartile-based strata to sampling data.frames
for(i in 1:4) {
  sockeye_CSD[[i]] <- sockeye_CSD[[i]] %>% left_join(CSD_redbyweek_quartiles[[i]]) %>% rename(strata_red=strata)
  chinook_CSD[[i]] <- chinook_CSD[[i]] %>% left_join(CSD_kingbyweek_quartiles[[i]]) %>% rename(strata_king=strata)
  sockeye_GSD[[i]] <- sockeye_GSD[[i]] %>% left_join(GSD_redbyweek_quartiles[[i]]) %>% rename(strata_red=strata)
  chinook_GSD[[i]] <- chinook_GSD[[i]] %>% left_join(GSD_kingbyweek_quartiles[[i]]) %>% rename(strata_king=strata)
  chinook[[i]] <- chinook[[i]] %>% left_join(pooled_kingbyweek_quartiles[[i]]) %>% rename(strata_king_pooled=strata)
}

# # finally, actually testing proportions by strata!
# testthetab <- function(x) {
#   rawtab <- table(x$age, x$strata_red)
#   # print(rawtab)
#   bettertab <- rawtab[substr(rownames(rawtab),1,1) %in% 0:9, ]
#   # print(bettertab)
#   stillbettertab <- bettertab[rowSums(bettertab)>20,]
#   print(stillbettertab)
#   print(chisq.test(stillbettertab))
# }
# testthetab2 <- function(x) {
#   rawtab <- table(x$age, x$strata_king)
#   # print(rawtab)
#   bettertab <- rawtab[substr(rownames(rawtab),1,1) %in% 0:9, ]
#   # print(bettertab)
#   stillbettertab <- bettertab[rowSums(bettertab)>2,]
#   print(stillbettertab)
#   print(chisq.test(stillbettertab))
# }
# testthetab(x=sockeye_CSD[[1]])
# sapply(sockeye_CSD, testthetab)
# sapply(chinook_CSD, testthetab2)
# sapply(sockeye_GSD, testthetab)
# sapply(chinook_GSD, testthetab2)


# darn, will need to stratify proportions for sockeye

# should censor bad values 
fixages <- function(x) {
  x1 <- x[substr(x$age,1,1) %in% 0:9, ]
  # x1$age <- paste(as.character(x1$age), sep=",",collapse="")
  # x1$age <- paste(as.character(x1$age), sep=".",collapse="")
  x1$totage <- 1 + as.numeric(substr(x1$age,1,1)) + as.numeric(substr(x1$age,nchar(x1$age),nchar(x1$age)))
  return(x1)
}
fixsex <- function(x) {
  x1 <- x[x$sex %in% c("F","M"), ]
  x1$sex[x1$sex == "M"] <- " M"   # this is a hack to make M print before F for consistency with report
  return(x1)
}

## investigating values present, and testing fix functions
for(i in 1:4) print(table(sockeye_CSD[[i]]$age))
for(i in 1:4) print(table(sockeye_GSD[[i]]$age))
for(i in 1:4) print(table(chinook_CSD[[i]]$age))
for(i in 1:4) print(table(chinook_GSD[[i]]$age))
for(i in 1:4) print(table(fixages(sockeye_CSD[[i]])$age))
for(i in 1:4) print(table(fixages(sockeye_GSD[[i]])$age))
for(i in 1:4) print(table(fixages(chinook_CSD[[i]])$age))
for(i in 1:4) print(table(fixages(chinook_GSD[[i]])$age))

for(i in 1:4) print(table(sockeye_CSD[[i]]$sex))
for(i in 1:4) print(table(sockeye_GSD[[i]]$sex))
for(i in 1:4) print(table(chinook_CSD[[i]]$sex))
for(i in 1:4) print(table(chinook_GSD[[i]]$sex))
for(i in 1:4) print(table(fixsex(sockeye_CSD[[i]])$sex))
for(i in 1:4) print(table(fixsex(sockeye_GSD[[i]])$sex))
for(i in 1:4) print(table(fixsex(chinook_CSD[[i]])$sex))
for(i in 1:4) print(table(fixsex(chinook_GSD[[i]])$sex))


## generalized stratified proportion function   ------ verify this thing!!!
## ALSO -- include option for expanding total harvest
stratprop <- function(cat, strat, Nstrat, Ntot=NULL) { # membership vec, strata vec, strata weights
  ntz <- table(strat, cat)
  nz <- colSums(ntz)
  nt <- rowSums(ntz)
  ptz <- ntz/nt
  vptz <- ptz*(1-ptz)/(nt-1)  # NOT going to do fpc here!
  Nt <- as.numeric(Nstrat)
  Ntz <- Nt*ptz
  vNtz <- (Nt^2)*vptz
  Nz <- colSums(Ntz)
  vNz <- colSums(vNtz)
  if(is.null(Ntot)) Ntot <- sum(Nt)
  pz <- Nz/Ntot   ## -- this would be where to redefine Ntot or similar
  vpz <- vNz/(Ntot^2)
  prop_for_tab <- paste0(round(100*pz, 1), " (", round(100*sqrt(vpz), 1), ")%")
  harv_for_tab <- paste0(round(Nz, 0), " (", round(sqrt(vNz), 0), ")")
  data.frame(nz,prop_for_tab,p_raw=pz,se_p_raw=sqrt(vpz),
             harv_for_tab,c_raw=Nz,se_c_raw=sqrt(vNz))
}

## chi-squared test to see if stratification is necessary
strattest <- function(cat, strat, Nstrat=NULL, printtab=F) { # membership vec, strata vec, strata weights
  ntz <- table(strat, cat)
  print(chisq.test(ntz))
  if(printtab) print(ntz)
}

## For each year, creating tables that are consistent with T8 & friends.
## This happens by first creating tables by each combination of factors, then
## splicing them together in the same order as in T8.

## Following each spliced table is a sequence of chi-squared tests associated
## with each table section.
GSD_RED_ASL_update <- CSD_RED_ASL_update <- list()
for(i in 1:4) {
  aa1 <- with(fixsex(fixages(sockeye_GSD[[i]])), 
              stratprop(cat=paste(totage,age,sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa2 <- with(fixsex(fixages(sockeye_GSD[[i]])), 
              stratprop(cat=paste(totage,age,"total"), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa3 <- with(fixsex(fixages(sockeye_GSD[[i]])), 
              stratprop(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa4 <- with(fixsex(sockeye_GSD[[i]]), 
              stratprop(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa12 <- rbind(aa1,aa2)
  aa12a <- aa12[order(rownames(aa12)), ]
  GSD_RED_ASL_update[[i]] <- rbind(aa12a, aa3, aa4)
  print(GSD_RED_ASL_update[[i]])   ### this will become knitr::kable
  with(fixsex(fixages(sockeye_GSD[[i]])), 
       strattest(cat=paste(totage,age,sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(fixages(sockeye_GSD[[i]])), 
       strattest(cat=paste(totage,age,"total"), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(fixages(sockeye_GSD[[i]])), 
       strattest(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(sockeye_GSD[[i]]), 
       strattest(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(GSD[[i]]$red, GSD[[i]]$strata_red, sum, na.rm=T)))
  
  
  aa1 <- with(fixsex(fixages(sockeye_CSD[[i]])), 
              stratprop(cat=paste(totage,age,sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa2 <- with(fixsex(fixages(sockeye_CSD[[i]])), 
              stratprop(cat=paste(totage,age,"total"), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa3 <- with(fixsex(fixages(sockeye_CSD[[i]])), 
              stratprop(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa4 <- with(fixsex(sockeye_CSD[[i]]), 
              stratprop(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  
  aa12 <- rbind(aa1,aa2)
  aa12a <- aa12[order(rownames(aa12)), ]
  CSD_RED_ASL_update[[i]] <- rbind(aa12a, aa3, aa4)
  print(CSD_RED_ASL_update[[i]])   ### this will become knitr::kable
  with(fixsex(fixages(sockeye_CSD[[i]])), 
       strattest(cat=paste(totage,age,sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(fixages(sockeye_CSD[[i]])), 
       strattest(cat=paste(totage,age,"total"), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(fixages(sockeye_CSD[[i]])), 
       strattest(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
  with(fixsex(sockeye_CSD[[i]]), 
       strattest(cat=paste(sex), 
                        strat=strata_red,  
                        Nstrat=tapply(CSD[[i]]$red, CSD[[i]]$strata_red, sum, na.rm=T)))
}

### Chinook ASL was pooled by both subdistricts in report!
### need to test if this was appropriate
# # creating pooled objects  -- might not be needed, just use chinook!
# chinook_pooled <- list()
# for(i in 1:4) {
#   chinook_pooled[[i]] <- data.frame(age=c(chinook_GSD[[i]]$age, chinook_CSD[[i]]$age),
#                                     sex=c(chinook_GSD[[i]]$sex, chinook_CSD[[i]]$sex),
#                                     strata_king=c(chinook_GSD[[i]]$strata_king, chinook_CSD[[i]]$strata_king),
#                                     subdistrict=c(rep("GSD", nrow(chinook_GSD[[i]])), rep("CSD", nrow(chinook_CSD[[i]]))))
# }
# 
# ## testing if pooling by subdistrict is appropriate
# for(i in 1:4) {
#   cat("year",i,'\n')
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
#                  strat=subdistrict, printtab=T))
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(totage), # was paste(totage,age)
#                  strat=subdistrict, printtab=T)) 
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(sex),
#                  strat=subdistrict, printtab=T))  
#   with(fixsex(chinook_pooled[[i]]),
#        strattest(cat=paste(sex),
#                  strat=subdistrict, printtab=T)) 
# }
# ## take-home: only evidence of difference by subdistrict in year 3 (2021) for first two tests (ages)
# ## ==> idea: stratify by subdistrict?

## testing if pooling by subdistrict is appropriate
for(i in 1:4) {
  cat("year",i,'\n')
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
                 strat=subdistrict, printtab=T))
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(totage), # was paste(totage,age)
                 strat=subdistrict, printtab=T)) 
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(sex),
                 strat=subdistrict, printtab=T))  
  with(fixsex(chinook[[i]]),
       strattest(cat=paste(sex),
                 strat=subdistrict, printtab=T)) 
}
## take-home: only evidence of difference by subdistrict in year 3 (2021) for first two tests (ages)
## ==> idea: stratify by subdistrict?

## testing if pooling by time strata is appropriate - GSD
for(i in 1:4) {
  cat("year",i,'\n')
  with(fixsex(fixages(chinook_GSD[[i]])),
       strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
                 strat=strata_king, printtab=T))
  with(fixsex(fixages(chinook_GSD[[i]])),
       strattest(cat=paste(totage), # was paste(totage,age)
                 strat=strata_king, printtab=T)) 
  with(fixsex(fixages(chinook_GSD[[i]])),
       strattest(cat=paste(sex),
                 strat=strata_king, printtab=T))  
  with(fixsex(chinook_GSD[[i]]),
       strattest(cat=paste(sex),
                 strat=strata_king, printtab=T)) 
}
## take-home: I think sample sizes might be too small for this to be meaningful

## testing if pooling by time strata is appropriate - CSD
for(i in 1:4) {   # - stratification breaks in year 2 (2020)  
  # ==> sample_week was not filled in for week<5 
  # ==> Chitina sampling stops after week 8
  cat("year",i,'\n')
  with(fixsex(fixages(chinook_CSD[[i]])),
       strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
                 strat=strata_king, printtab=T))
  with(fixsex(fixages(chinook_CSD[[i]])),
       strattest(cat=paste(totage), # was paste(totage,age)
                 strat=strata_king, printtab=T)) 
  with(fixsex(fixages(chinook_CSD[[i]])),
       strattest(cat=paste(sex),
                 strat=strata_king, printtab=T))  
  with(fixsex(chinook_CSD[[i]]),
       strattest(cat=paste(sex),
                 strat=strata_king, printtab=T)) 
}
# take-home: no problems detected

# ## testing if pooling by time strata is appropriate - pooled - not quite accurate but exploratory
# for(i in 1:4) {   
#   cat("year",i,'\n')
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
#                  strat=strata_king, printtab=T))
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(totage), # was paste(totage,age)
#                  strat=strata_king, printtab=T)) 
#   with(fixsex(fixages(chinook_pooled[[i]])),
#        strattest(cat=paste(sex),
#                  strat=strata_king, printtab=T))  
#   with(fixsex(chinook_pooled[[i]]),
#        strattest(cat=paste(sex),
#                  strat=strata_king, printtab=T)) 
# }

## testing if pooling by time strata is appropriate 
for(i in 1:4) {   
  cat("year",i,'\n')
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(totage,sex), # was paste(totage,age,sex)
                 strat=strata_king_pooled, printtab=T))
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(totage), # was paste(totage,age)
                 strat=strata_king_pooled, printtab=T)) 
  with(fixsex(fixages(chinook[[i]])),
       strattest(cat=paste(sex),
                 strat=strata_king_pooled, printtab=T))  
  with(fixsex(chinook[[i]]),
       strattest(cat=paste(sex),
                 strat=strata_king_pooled, printtab=T)) 
}
## take-home: problems in years 2 and 3 - maybe stratify temporally throughout??


## For each year, creating tables that are consistent with T8 & friends.
## This happens by first creating tables by each combination of factors, then
## splicing them together in the same order as in T8.

## Following each spliced table is a sequence of chi-squared tests associated
## with each table section.
KING_ASL_update <- list()
for(i in 1:4) {
  aa1 <- with(fixsex(fixages(chinook[[i]])), 
              stratprop(cat=paste(totage,age,sex), 
                        strat=strata_king_pooled,  
                        Nstrat=tapply(allSD[[i]]$king, allSD[[i]]$strata_king_pooled, sum, na.rm=T)))
  
  aa2 <- with(fixsex(fixages(chinook[[i]])), 
              stratprop(cat=paste(totage,age,"total"), 
                        strat=strata_king_pooled,  
                        Nstrat=tapply(allSD[[i]]$king, allSD[[i]]$strata_king_pooled, sum, na.rm=T)))
  
  aa3 <- with(fixsex(fixages(chinook[[i]])), 
              stratprop(cat=paste(sex), 
                        strat=strata_king_pooled,  
                        Nstrat=tapply(allSD[[i]]$king, allSD[[i]]$strata_king_pooled, sum, na.rm=T)))
  
  aa4 <- with(fixsex(chinook[[i]]), 
              stratprop(cat=paste(sex), 
                        strat=strata_king_pooled,  
                        Nstrat=tapply(allSD[[i]]$king, allSD[[i]]$strata_king_pooled, sum, na.rm=T)))
  
  aa12 <- rbind(aa1,aa2)
  aa12a <- aa12[order(rownames(aa12)), ]
  KING_ASL_update[[i]] <- rbind(aa12a, aa3, aa4)
  print(KING_ASL_update[[i]])   ### this will become knitr::kable
  with(fixsex(fixages(chinook[[i]])), 
       strattest(cat=paste(totage,age,sex), 
                 strat=strata_king_pooled,  
                 Nstrat=NULL))
  with(fixsex(fixages(chinook[[i]])), 
       strattest(cat=paste(totage,age,"total"), 
                 strat=strata_king_pooled,  
                 Nstrat=NULL))
  with(fixsex(fixages(chinook[[i]])), 
       strattest(cat=paste(sex), 
                 strat=strata_king_pooled,  
                 Nstrat=NULL))
  with(fixsex(chinook[[i]]), 
       strattest(cat=paste(sex), 
                 strat=strata_king_pooled,  
                 Nstrat=NULL))
}


### Writing tables to external .csv files!
### The tables created here are similar in format to T5 (and similar) and T8 (and similar) 
### with the intent of easily copy-pasting columns as needed.

### TODO: maybe split estimate & se into separate (additional) columns if that's easier for formatting

### fix it in stratprop(), just make new columns - done
### make new columns in t5_update and t6_update - done
## still need table of quartile dates

# CSD_redbyweek_quartiles 
# GSD_redbyweek_quartiles 
# CSD_kingbyweek_quartiles 
# GSD_kingbyweek_quartiles 
# pooled_kingbyweek_quartiles 
CSD_redbyweek_quartiles1 <- GSD_redbyweek_quartiles1 <- pooled_kingbyweek_quartiles1 <- list()
# sample_weeks
# firstdates
firstdates1 <- as.data.frame(cbind(sample_week=0:13, firstdates))
for(i in 2:5) firstdates1[,i] <- as.Date(firstdates1[,i], format="%m/%d/%Y")
firstdates1[,1] <- as.numeric(firstdates1[,1])
lastdates1 <- as.data.frame(cbind(sample_week=0:13, lastdates))
for(i in 2:5) lastdates1[,i] <- as.Date(lastdates1[,i], format="%m/%d/%Y")
lastdates1[,1] <- as.numeric(lastdates1[,1])


### ok this is just going to be ugly
strat_table <- data.frame(fishery=c(rep("Red GSD", 4), rep("Red CSD", 4), rep("King pooled", 4)),
                          stratum=rep(1:4, 3))
strat_table[, 1:16+2] <- NA
for(iyear in 1:4) {
  for(istratum in 1:4) {
    strat_table[istratum, iyear*4-1] <- with(GSD_redbyweek_quartiles[[iyear]], min(sample_week[strata==istratum])) # min week GSD
    strat_table[istratum, iyear*4-0] <- with(GSD_redbyweek_quartiles[[iyear]], max(sample_week[strata==istratum])) # max week GSD
    strat_table[istratum, iyear*4+1] <- firstdates1[(0:13)==strat_table[istratum, iyear*4-1], iyear+1] %>% as.character # min date GSD
    strat_table[istratum, iyear*4+2] <- lastdates1[(0:13)==strat_table[istratum, iyear*4-0], iyear+1] %>% as.character # max date GSD
    
    strat_table[istratum+4, iyear*4-1] <- with(CSD_redbyweek_quartiles[[iyear]], min(sample_week[strata==istratum])) # min week CSD
    strat_table[istratum+4, iyear*4-0] <- with(CSD_redbyweek_quartiles[[iyear]], max(sample_week[strata==istratum])) # max week CSD
    strat_table[istratum+4, iyear*4+1] <- firstdates1[(0:13)==strat_table[istratum+4, iyear*4-1], iyear+1] %>% as.character# min date CSD
    strat_table[istratum+4, iyear*4+2] <- lastdates1[(0:13)==strat_table[istratum+4, iyear*4-0], iyear+1] %>% as.character# max date CSD
    
    strat_table[istratum+8, iyear*4-1] <- with(pooled_kingbyweek_quartiles[[iyear]], min(sample_week[strata==istratum]))# min week king
    strat_table[istratum+8, iyear*4-0] <- with(pooled_kingbyweek_quartiles[[iyear]], max(sample_week[strata==istratum]))# max week king
    strat_table[istratum+8, iyear*4+1] <- firstdates1[(0:13)==strat_table[istratum+8, iyear*4-1], iyear+1] %>% as.character# min date king
    strat_table[istratum+8, iyear*4+2] <- lastdates1[(0:13)==strat_table[istratum+8, iyear*4-0], iyear+1] %>% as.character# max date king
  }
}
names(strat_table) <- c("Fishery","Stratum", paste(rep(years, each=4), c("min_week","max_week","min_date","max_date")))

write.csv(strat_table, file="output/strat_table.csv")

write.csv(T5_update, file="output/T5_update.csv")
write.csv(T6_update, file="output/T6_update.csv")

# GSD_RED_ASL_update  
tbl_num <- c(8, 13, 16, 19)
for(i in 1:4) write.csv(GSD_RED_ASL_update[[i]], file=paste0("output/T", tbl_num[i], "_update.csv"))

# CSD_RED_ASL_update  
tbl_num <- c(10, 14, 17, 20)
for(i in 1:4) write.csv(CSD_RED_ASL_update[[i]], file=paste0("output/T", tbl_num[i], "_update.csv"))

# KING_ASL_update  
tbl_num <- c(12, 15, 18, 21)
for(i in 1:4) write.csv(KING_ASL_update[[i]], file=paste0("output/T", tbl_num[i], "_update.csv"))

