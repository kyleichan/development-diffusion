*
* An Exploration of Technology Diffusion
* Diego Comin and Bart Hobijn (2007)
* 
* Purpose:
* Programs for estimation of the adoption lags for countries other than the US 
* 
* Programming:
* Diego Comin, dcomin@hbs.edu, and Bart Hobijn, bart.hobijn@ny.frb.org
* December 2007
*
* Execution:
* Run this file and output will be written to a Tables.txt file in the output directory.

* Setting the STATA version in which we are running this DO file
version 9

* Initializing the program by clearing the memory and allocating enough space to load all the data
clear
set more off, permanently
set linesize 255
set trace off
set mem 300m
set logtype text

* Setting the location of the files 
* NOTE: Change the rootpath to where you have decompressed the files from the zip-file to.
local rootpath = "C:\Bart\Research\Histtech\progs2\ReplicationFiles\"
local logpath = "\output\"

* Loading the data
* local datafile `rootpath'`logpath'results.dta
local datafile `rootpath'`logpath'DTA\Results.dta
* local groupsfile `rootpath'`logpath'groups.dta
local groupsfile `rootpath'`logpath'DTA\groups.dta
use `datafile', clear

local logfilename "tables"
local logfilename `rootpath'`logpath'`logfilename'

capture log close
capture log using `logfilename', replace
capture set logtype text
capture set trace off
capture log off


* Sorting the observations by technology such that we can run comments by technology
sort technologyname

* -------------------------------------------------------------------------------------------------------
* Table 1 Part I: 
* Tables with fraction of cases classified into:
* 1		Estimate OK
* -1		Estimate imprecise
* -2		Estimate implausible
* -------------------------------------------------------------------------------------------------------
capture log on
capture noisily display "Table 1 Part I : Fraction of estimates that is classified in each of the categories"
capture noisily tabulate technology plausprecr if plausprecr != -3
capture noisily display "Comment: (1) plausible, (-1) plausible but imprecise (-2) imprecise"
capture log off

* -------------------------------------------------------------------------------------------------------
* Table 1 Part II : 
* Tables with summary statistics of the estimated adoptionlags:
* -------------------------------------------------------------------------------------------------------
capture gen lagsprecise = yearestr - inventionyear if plausprecr == 1
capture by technologyname : egen meanyearprecise = mean(yearestr) if plausprecr == 1 
gen dyearprecise = yearestr - meanyearprecise 
gen byte positiveR2 = r2r > 0

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 1 Part II: Summary statistics of adoption lags for precise estimates"
capture noisily tabstat lagsprecise , by(technologyname) stats( n mean sd p1 p10 median p90 p99 )
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

* -------------------------------------------------------------------------------------------------------
* Table 1 Part III: 
* Part of table related to the goodness of fit:
* -------------------------------------------------------------------------------------------------------
capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 1 Part IIIa: Number of negative and positive R2s"
capture noisily tabulate technologyname positiveR2 if plausprecr == 1
capture noisily display "Comment: (1) postive R2, (0) negative R2."
capture noisily display "         These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture drop R2ifpositive
gen R2ifpositive = r2r if positiveR2 == 1 & plausprecr == 1
capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
display "Table 1 Part IIIb: Distribution of positive R2s"
capture noisily tabstat R2ifpositive , by(technologyname) stats( mean sd p1 p10 median p90 p99 )
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

* -------------------------------------------------------------------------------------------------------
* Table 3: 
* Anova and fixed effect regressions
* -------------------------------------------------------------------------------------------------------
capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ia: ANOVA of adoption lags"
capture noisily anova lagsprecise country_no tech_no
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ib: Fixed effect regression, country fixed effect"
capture noisily xi : regress lagsprecise i.country_no
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ic: Fixed effect regression, technology fixed effect"
capture noisily xi : regress lagsprecise i.tech_no
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Id: Fixed effect regression, country and technology fixed effects"
capture noisily xi , noomit : regress lagsprecise i.country_no i.tech_no , noconstant
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ie: Fixed effect regression with invention dates -- all years, country fixed effect"
capture noisily xi , noomit : regress lagsprecise inventionyear i.country_no , noconstant
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ie: Fixed effect regression with invention dates -- before 1950, country fixed effect"
capture noisily xi , noomit : regress lagsprecise inventionyear i.country_no if inventionyear < 1951 , noconstant
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 3 Part Ie: Fixed effect regression with invention dates -- after 1950, country fixed effect"
capture noisily xi , noomit : regress lagsprecise inventionyear i.country_no if inventionyear > 1950 , noconstant
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

* -------------------------------------------------------------------------------------------------------
* Table 4:
* Robustness check and test results
* -------------------------------------------------------------------------------------------------------
gen byte bigenoughsample = nobs > 11
gen byte plausibleandprecise = plausprecr == 1
gen byte preciseandnullnotrejected = plausprecr==1 &  testrvsu>.05
gen byte rprecise_uprecise = plausprecr==1  & plausprecu==1
gen byte rprecise_unot = plausprecr==1  & plausprecu!=1
gen byte rnot_uprecise = plausprecr!=1  & plausprecu==1

gen techspecificlagcorrs = 0
local starttechnology 1
local endtechnology 15
forvalues technology_no = `starttechnology'(1)`endtechnology' {

capture quietly correlate yearestr yearestu if plausprecr==1 &  plausprecu==1 & tech_no == `technology_no'
replace techspecificlagcorrs = r(rho) if tech_no == `technology_no'

}
capture quietly correlate yearestr yearestu if plausprecr==1 &  plausprecu==1
gen totallagcorr = r(rho)

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 4 Part I: Number of cases of plausible and precise estimates for restricted and unrestricted models."
capture noisily tabstat bigenoughsample plausibleandprecise preciseandnullnotrejected rprecise_uprecise rprecise_unot rnot_uprecise, by(technologyname) stats( sum ) labelwidth(32)
capture log off

capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 4 Part II: Correlations between adoption lags in case both estimates are plausible and precise and overall correlation."
capture noisily tabstat techspecificlagcorrs totallagcorr , by(technologyname) stats( mean ) labelwidth(32)
capture log off

sort country_no
merge  country_no using `groupsfile' , uniqusing nokeep
* -------------------------------------------------------------------------------------------------------
* Table 5: 
* Country groups case studies
* -------------------------------------------------------------------------------------------------------
drop if group=="."
replace group="japan" if country_no==73
capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 5: Average deviation from average adoption year for different country groups and technologies"
capture noisily table technologyname group, contents(mean  dyearprecise n  dyearprecise ) center
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

* -------------------------------------------------------------------------------------------------------
* Table 6:
* East Asian Tigers case studies
* -------------------------------------------------------------------------------------------------------
local sampleselectcond "(country_no == 130 | country_no == 62 | country_no == 79 | country_no == 143 ) & ( tech_no == 2 | tech_no == 12 | tech_no == 13 )"
capture log on
capture noisily display " "
capture noisily display " "
capture noisily display " "
capture noisily display "Table 6: Estimated adoption dates of three recent technologies of EATs"
capture noisily table technologyname countryname if `sampleselectcond' , contents(mean yearestr) center
capture noisily display "Note: These results are for plausible and precisely estimated adoption lag cases"
capture log off

capture log close


