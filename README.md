# Improved partitioning of commercial and non-commercial Deep7 bottomfish catch in the main Hawaiian Islands



## Summary
For the Deep7 bottomfish research track, adjustments were made to both catch rate and fishing effort to exclude fishing trips that were not entirely recreational/non-commercial from the HMRFS data. Two adjustments were made during the catch rate estimation: 1) catch claimed as sold in HMRFS was excluded; and 2) catch claimed as non-sold by expense fishers and part-time commercial fishers was also excluded. Fishing effort estimates (derived from telephone and mail surveys) were adjusted to exclude trips from expense and part-time commercial fishers. The estimates from this exploration may better define the catch from fishers who do not have a commercial marine license. 
## Data Sources
1) Intercept survey data (SAS data files for 2003-2022) from the Hawaii Marine Recreational Fishing Survey (HMRFS) were provided by NMFS Office of Science and Technology (OST) and Marine Recreational Information Program (MRIP)
2) Fishing effort estimates were downloaded from MRIP query https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries
3) Coastal Household Telephone Survey (CHTS, 2003-2017) was previously downloaded from NMFS OST/MRIP
4) Customized fishing effort estimates from CHTS with county-level fishing trip estimates was produced/provided by Rob Andrews (NMFS OST/MRIP)
5) Access data files from HMRFS in 2003-2022 from NMFS/MRIP and Hawaii Division of Aquatic Resources

## R Script and Data Input   
The R script was modified from Ma et al. 2023. When "catch_type" is set as "Non-sold catch', the non-commercial catch estimates presented in Ma et al. (2023) can be reproduced. The catch estimates in the current exploration are provided when catch type is set as "Pure recreational". The data input includes

a) i1-i3 files: trip profile, unavaialble catch, and available (observed) catch used to estimate catch rate and variance of catch rate

b) Pure recreational fishers: ID code of pure recreational fishers in the survey data (based on 2003-2011 Access survey data 2011-2022 SAS survey data) used to filter out recreational expense fishers and part-time commercial fishers

c) Fishing effort time series: the fishing effort estimates in 2003-2022 used to estimate catch (as the product of catch rate and fishing effort)

d) Mean weight: the mean weight estimates for 7 bottomfish species used to convert catch number to catch weight

e) Multiplier: the adjustments for fishing effort calibration due to telphone survey to mail survey transition (M1), correction for an error in Maui population tally in 2003-2010 (M2), and proportion of pure recreational fishing trips (M3)

## Installing
R packages including "sas7bdat" (to read SAS data files), "this.path" (file management), "zoo" (moving average), and "KFAS" (Kalman Filter and smoother).
can be
## Resources
Github repository for the non-commercial catch estimates presented at the 2023 Joint Statitical Meetings
https://github.com/hgmagithub/Non-commercial-Catch-Estimation-for-Deep7-Bottomfish-in-the-Main-Hawaiian-Islands

## Version Control Platform
- Git

## License
See the [LICENSE.md](./LICENSE.md) for details

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
