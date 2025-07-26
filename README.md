# Improved partitioning of commercial and non-commercial Deep7 bottomfish catch in the main Hawaiian Islands



## Summary
For the Deep7 bottomfish research track, adjustments were made to both catch rate and fishing effort to exclude fishing trips that were not entirely recreational/non-commercial from the HMRFS data. Two adjustments were made during the catch rate estimation: 1) catch claimed as sold in HMRFS was excluded; and 2) catch claimed as non-sold by expense fishers and part-time commercial fishers was also excluded. Fishing effort estimates (derived from telephone and mail surveys) were adjusted to exclude trips from expense and part-time commercial fishers. The estimates from this exploration may better define the catch from fishers who do not have a commercial marine license. 
## Data and Scripts
1) Intercept survey data (SAS data files for 2003-2022) from the Hawaii Marine Recreational Fishing Survey (HMRFS), provided by NMFS Office of Science and Technology (OST) and Marine Recreational Information Program (MRIP)
2) Coastal Household Telephone Survey (CHTS, 2003-2017) from NMFS OST/MRIP
3) Customized fishing effort estimates from CHTS with county-lelvel fishing trip estimates (provided by Rob Andrews, NMFS OST/MRIP)
4) Access data files from HMRFS in 2003-2022 
5) The R script was modified from Ma et al. 2023. When "catch_type" is set as "Non-sold catch', the non-commercial catch estimates presented in Ma et al. (2023) can be reproduced. The catch estimates in the current exploration is provided when catch type is set as "Pure recreational". 

## Installing
R packages including "sas7bdat" (to read SAS data files), "this.path" (file management), "zoo" (moving average), and "KFAS" (Kalman Filter and smoother).

## Resources
Github repository for the non-commercial catch estimates presented at the 2023 Joint Statitical Meetings
https://github.com/hgmagithub/Non-commercial-Catch-Estimation-for-Deep7-Bottomfish-in-the-Main-Hawaiian-Islands

## Version Control Platform
- Git

## License
See the [LICENSE.md](./LICENSE.md) for details

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
