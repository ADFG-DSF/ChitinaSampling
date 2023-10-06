## Contribution of Gulkana Hatchery Sockeye Salmon Returns and Age and Length Composition of the Harvest of Sockeye and Chinook Salmon in the Upper Copper River Subsistence and Personal Use Fisheries

This project uses otolith sampling to estimate the proportion of the harvest of sockeye salmon in the Upper Copper River subsistence and personal use fisheries to have origin in the Gulkana Hatchery.  Proportions are estimated for each week from two fisheries: the Chitina Subdistrict (CSD) personal use fishery and the Glennallen Subdistrict (GSD) subsistence fishery.

In addition, age, sex, and length (ASL) contribution is estimated for the returns of both sockeye and Chinook salmon in both fisheries.

## Operational Plan

A previous version of the Operational Plan (from 2016) has been published and is available online.  The current Operational Plan (written in 2019) is very similar in all methodology.

http://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.3F.2016.08.pdf

## Folder structure

### Data/

Separate .csv files are provided for sampling and reported harvest, for each fishery:

* 20xx CSD HARVEST Reported.csv: yearly harvest reported from the Chitina Subdistrict
* 20xx GSD HARVEST Reported.csv: yearly harvest reported from the Glennallen Subdistrict
* 20xx Raw Data - Sockeye.csv: yearly sockeye salmon sampling data
* 20xx Raw Data - Chinook.csv: yearly Chinook salmon sampling data

### R/

* Chitina_sampling_2019_2022.R: all data operations & calculations for analysis

### output/

R-generated summary tables, corresponding to numbered tables in the report.  Tables 
were written to external .csv files, with the intent of facilitating copy-pasting
to the appropriate columns.

* T5_update.csv and T6_update.csv: Estimated weekly Hatchery proportion and contributions 
to the GSD and CSD fisheries, respectively, with standard errors.

* T8_update.csv (and 13, 16, 19): Sockeye salmon ASL for the GSD fishery for each year.
Estimates of proportion and harvest contribution (plus standard errors) are temporally
stratified according to reported harvest quartiles.

* T10_update.csv (and 14, 17, 20): Sockeye salmon ASL for the CSD fishery for each year.
Estimates of proportion and harvest contribution (plus standard errors) are temporally
stratified according to reported harvest quartiles.

* T12_update.csv (and 15, 18, 21): Chinook salmon ASL for both fisheries combined for each year.
Estimates of proportion and harvest contribution (plus standard errors) are temporally
stratified according to reported harvest quartiles.
