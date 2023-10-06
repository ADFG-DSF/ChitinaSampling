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
n_i <- apply(sockeye_allyrstab, c(1,3,4), sum)
n_i_GSD <- n_i[, dimnames(n_i)$subdistrict == "Glennallen Subdistrict", ]
n_i_CSD <- n_i[, dimnames(n_i)$subdistrict == "Chitina Subdistrict", ]
o_hi <- sockeye_allyrstab[, dimnames(sockeye_allyrstab)$origin_h_w =="H" , , ]
o_hi_GSD <- o_hi[, dimnames(o_hi)$subdistrict == "Glennallen Subdistrict", ]
o_hi_CSD <- o_hi[, dimnames(o_hi)$subdistrict == "Chitina Subdistrict", ]

# this would be a logical place to expand total harvest
N_GSD <- GSD_allyrstab
N_CSD <- CSD_allyrstab

# equation 1
C_hi_GSD <- o_hi_GSD/n_i_GSD*N_GSD
C_hi_CSD <- o_hi_CSD/n_i_CSD*N_CSD

# equation 2
C_sh_GSD <- colSums(C_hi_GSD, na.rm=T)
C_sh_CSD <- colSums(C_hi_CSD, na.rm=T)
# NOTE: I don't think this is appropriate for 2022 GSD, given the missing data

# for table
p_GSD <- o_hi_GSD/n_i_GSD
p_CSD <- o_hi_CSD/n_i_CSD
vp_GSD <- p_GSD*(1-p_GSD)/(n_i_GSD-1)
vp_CSD <- p_CSD*(1-p_CSD)/(n_i_CSD-1)

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

# equation 3 -- is this needed??
# equation 4 -- is this needed??

# equation 5 split apart by strata (makes more sense)
V_C_hi_GSD <- (N_GSD^2)*o_hi_GSD/(n_i_GSD^2)*(1-p_GSD)
V_C_hi_CSD <- (N_CSD^2)*o_hi_CSD/(n_i_CSD^2)*(1-p_CSD)
V_C_h_GSD <- colSums(V_C_hi_GSD, na.rm=T)
V_C_h_CSD <- colSums(V_C_hi_CSD, na.rm=T)

SE_C_hi_GSD <- sqrt(V_C_hi_GSD)
SE_C_h_GSD <- sqrt(V_C_h_GSD)
SE_C_hi_CSD <- sqrt(V_C_hi_CSD)
SE_C_h_CSD <- sqrt(V_C_h_CSD)

printse <- function(est, se, percent=F, digits=2) {
  if(percent) {
    out <- paste0(round(100*est, digits), "% (", round(100*se, digits),"%)")
  } else {
    out <- paste0(round(est, digits), " (", round(se, digits),")")
  }
  return(out)
}

# making tables that mimic the structure of T5 and T6 in report
T5_update <- data.frame(1:nrow(n_i_GSD))
for(i in seq_along(years)) {
  T5_update[, 3*(i-1)+1] <- n_i_GSD[,i]
  T5_update[, 3*(i-1)+2] <- printse(p_GSD[,i], sqrt(vp_GSD[,i]), percent=T, digits=1)
  T5_update[, 3*(i-1)+3] <- printse(C_hi_GSD[,i], SE_C_hi_GSD[,i], digits=0)
}
names(T5_update) <- paste(rep(years,each=3), rep(c("n","p","c")))

T6_update <- data.frame(1:nrow(n_i_CSD))
for(i in seq_along(years)) {
  T6_update[, 3*(i-1)+1] <- n_i_CSD[,i]
  T6_update[, 3*(i-1)+2] <- printse(p_CSD[,i], sqrt(vp_CSD[,i]), percent=T, digits=1)
  T6_update[, 3*(i-1)+3] <- printse(C_hi_CSD[,i], SE_C_hi_CSD[,i], digits=0)
}
names(T6_update) <- paste(rep(years,each=3), rep(c("n","p","c")))




### Sockeye and Chinook Salmon ASL Composition

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
  prop_for_tab <- paste0(round(100*pz, 2), "% (", round(100*sqrt(vpz), 2), "%)")
  harv_for_tab <- paste0(round(Nz, 0), " (", round(sqrt(vNz), 0), ")")
  data.frame(nz,pz,vpz,prop_for_tab,harv_for_tab)
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
