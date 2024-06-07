# missvcqiR
Missed Opportunities for Vaccination Coverage Quality Indicators (MISS VCQI) R Package 
 
This is the repository for the R implementation of MISS VCQI, a set of R programs meant to analyze data from missed opportunities for vaccination (MOV) surveys. MISS VCQI analyses are consistent with guidance from the Pan American Health Organization (PAHO) [Methodology for the Evaluation of Missed Opportunities for Vaccination](https://www3.paho.org/hq/dmdocuments/2014/MissedOpportunity-Vaccination-Protocol-2013.pdf) (2013). 

To install this package, ensure you (a) have R version 4.2.1 or later and (b) have installed [RTools](https://cran.r-project.org/bin/windows/Rtools/). Then run the following commands: 

``` r
if (!requireNamespace("pak")){install.packages("pak")}

pak::pkg_install("BiostatGlobalConsulting/missvcqiR")
```
