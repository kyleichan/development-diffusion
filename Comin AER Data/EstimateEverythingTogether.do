*
* An Exploration of Technology Diffusion
* Diego Comin and Bart Hobijn (2009)
* 
* Purpose:
* Programs for estimation of the adoption lags for all countries
* 
* Programming:
* Bart Hobijn, bart.hobijn@sf.frb.org
* March 2009
* For information about replicating the results, please use the readme.txt file in this directory.
*
* Execution:
* DO (not Run) this file and output will be written to sequence of country specific text files in directory for the technology 
* for which program is run as well as to results.dta resultsUSA.dta datasets for use by subsequent program.
*
* 
* ==================================================================================================================================== 
* Single Technologies:
* ------------------------------------------------------------------------------------------------------------------------------------ 
* 1.  Cars 
* 2.  PCs
* 3.  Electricity
* 4.  Trucks
* 5.  Blast oxygen		  
* 6.  Railways - freight
* 7.  Railways - passengers
* 8.  Telegrams
* 9.  Telephones
*10.  Aviation - freight        
*11.  Aviation - passengers 
*12.	Cellphones
*13.	Internet users
*14.	MRI units    
*15.  Steam- and motorships                                                                                                                                                                                                                                                                                                                                                       
*                                                                                                                 
* ==================================================================================================================================== 
* List of Countries:
* ------------------------------------------------------------------------------------------------------------------------------------
*	1	AFG	Afghanistan
*	2	ALB	Albania
*	3	ALG	Algeria
*	4	ANG	Angola
*	5	ARG	Argentina
*	6	ARM	Armenia
*	7	AUL	Australia
*	8	AUS	Austria
*	10	AZE	Azerbaijan
*	11	BAN	Bangladesh
*	12	BEL	Belarus
*	13	BLG	Belgium
*	14	BLZ	Belize
*	15	BEN	Benin
*	16	BOL	Bolivia
*	17	BOS	Bosnia-Herzegovina
*	18	BOT	Botswana
*	19	BRA	Brazil
*	20	BUL	Bulgaria
*	21	BKF	Burkina Faso
*	22	BUR	Burundi
*	23	CMB	Cambodia
*	24	CAM	Cameroon
*	25	CAN	Canada
*	26	CAR	Central African Republic
*	27	CHD	Chad
*	28	CHL	Chile
*	29	CHN	China
*	30	COL	Colombia
*	31	CON	Republic of the Congo
*	32	COS	Costa Rica
*	33	CRO	Croatia
*	34	CUB	Cuba
*	35	CZE	Czechoslovakia
*	36	CZR	Czech Republic
*	37	DEN	Denmark
*	38	DOM	Dominican Republic
*	39	ECU	Ecuador
*	40	EGY	Egypt
*	41	ELS	El Salvador
*	42	EQU	Equatorial Guinea
*	43	ERT	Eritrea
*	44	EST	Estonia
*	45	ETH	Ethiopia
*	46	FIN	Finland
*	47	FRA	France
*	48	GAB	Gabon
*	49	GAM	Gambia
*	50	GEO	Georgia
*	51	GER	Germany
*	52	GME	East Germany
*	54	GHA	Ghana
*	55	GRE	Greece
*	56	GUA	Guatemala
*	57	GUI	Guinea
*	58	GNB	Guinea-Bissau
*	59	GUY	Guyana
*	60	HAI	Haiti
*	61	HON	Honduras
*	62	HNK	Hong Kong
*	63	HUN	Hungary
*	64	ICE	Iceland
*	65	IND	India
*	66	IDS	Indonesia
*	67	IRN	Iran
*	68	IRQ	Iraq
*	69	IRE	Ireland
*	70	ISR	Israel
*	71	ITA	Italy
*	72	IVO	Ivory Coast
*	73	JAP	Japan
*	74	JOR	Jordan
*	75	KAZ	Kazakhstan
*	76	KEN	Kenya
*	77	KOR	Korea
*	78	KRN	North Korea
*	79	KRS	South Korea
*	80	KUW	Kuwait
*	81	KYR	Kyrgyzstan
*	82	LAO	Laos
*	83	LTV	Latvia
*	84	LEB	Lebanon
*	85	LES	Lesotho
*	86	LIB	Liberia
*	87	LBY	Libya
*	88	LTH	Lithuania
*	89	LUX	Luxembourg
*	90	MAC	Macedonia
*	91	MAD	Madagascar
*	92	MLW	Malawi
*	93	MLY	Malaysia
*	94	MAL	Mali
*	95	MAU	Mauritania
*	96	MAS	Mauritius
*	97	MEX	Mexico
*	98	MOL	Moldova
*	99	MON	Mongolia
*	100	MNT	Montenegro
*	101	MOR	Morocco
*	102	MOZ	Mozambique
*	103	MYA	Burma
*	104	NAM	Namibia
*	105	NEP	Nepal
*	106	NTH	Netherlands
*	107	NEW	New Zealand
*	108	NIC	Nicaragua
*	109	NIG	Niger
*	110	NGA	Nigeria
*	111	NOR	Norway
*	112	OMA	Oman
*	114	PAK	Pakistan
*	115	PAN	Panama
*	116	PNG	Papua New Guinea
*	117	PRG	Paraguay
*	118	PER	Peru
*	119	PHI	Philippines
*	120	POL	Poland
*	121	POR	Portugal
*	122	ROM	Romania
*	123	RUS	Russia
*	124	RWA	Rwanda
*	125	SAU	Saudi Arabia
*	126	SEN	Senegal
*	127	SER	Serbia
*	128	SBM	Serbia and Montenegro
*	129	SRL	Sierra Leone
*	130	SNG	Singapore
*	131	SVK	Slovak Republic
*	132	SVN	Slovenia
*	133	SOM	Somalia
*	134	SAF	South Africa
*	135	SPA	Spain
*	136	SRI	Sri Lanka
*	137	SUD	Sudan
*	138	SRN	Suriname
*	139	SWA	Swaziland
*	140	SWE	Sweden
*	141	SWZ	Switzerland
*	142	SYR	Syria
*	143	TAI	Taiwan
*	144	TAJ	Tajikistan
*	145	TAN	Tanzania
*	146	THA	Thailand
*	147	TOG	Togo
*	148	TUN	Tunisia
*	149	TUR	Turkey
*	150	TKM	Turkmenistan
*	151	UAE	United Arab Emirates
*	152	UGA	Uganda
*	153	UKR	Ukraine
*	154	UNK	United Kingdom
*	155	USA	United States
*	156	URG	Uruguay
*	158	UZB	Uzbekistan
*	159	VNZ	Venezuela
*	160	VTM	Vietnam
*	161	VTN	North Vietnam
*	162	VTS	South Vietnam
*	163	YEM	Yemen
*	164	YUG	Yugoslavia
*	165	ZAI	Democratic Republic of the Congo
*	166	ZAM	Zambia
*	167	ZBW	Zimbabwe
*	169	YPR	South Yemen
*	170	ICH	Indochina
*	171	FGN	French Guiana
*
*

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
local location = 1
* NOTE: Change the rootpath to where you have decompressed the files from the zip-file to.
local rootpath = "C:\Bart\Research\Histtech\progs2\ReplicationFiles\"
local logpath = "\output\"
local logpathUSA = "`logpath'USA\"
local resultsfilename "Results"

* Loading the data
local datafile `rootpath'CHExplorationData.dta
use `datafile', clear

* Setting the calibrated parameter values
local alfapar = 0.30
local mupar = 1.30

* Choosing the technologies for which the program is run
local starttechnology 1
local endtechnology 15

* Choosing the countries for which the program is run
* Note: country = 0 corresponds to the model estimation for the USA that is used as the benchmark for the other estimates
local startcountry 0
* local endcountry 0
local endcountry 171   

* Choosing the technologies for which the program is run
local firstspec 1
local lastspec 2

* Set the minimum number of observations for which the model will be estimated
local minimumnobs = 10

* Set the maximum number of iterations 
local max_iter = 50

* The country number of the U.S. in the dataset
local US_country_no = 155

* Transforming the variables in logs 
gen t = year
gen lnY = lymad
gen lnL = lpopmad 
gen lnYpercap = lnY - lnL

* ======================================================================================================================================
* Initializing the files that contain the results 
* --------------------------------------------------------------------------------------------------------------------------------------

* Resetting the dataset with the results for all countries
tempname allresults
local resultsfilefullname  "`rootpath'`logpath'`resultsfilename'"
postfile `allresults' country_no tech_no str3 countryname str20 technologyname inventionyear nobs fixedeffectr yearestr elascoef1r elascoef2r trendr fixedeffectstderrr yeareststderrr elascoef1stderrr elascoef2stderrr trendstderrr r2r r2detrendedr hasconvergedr plausprecr fixedeffectu yearestu elascoef1u elascoef2u trendu fixedeffectstderru yeareststderru elascoef1stderru elascoef2stderru trendstderru r2u r2detrendedu hasconvergedu plausprecu testrvsu using `resultsfilefullname' , every(1) replace

* Resetting the dataset with the results for the USA
tempname usaresults
local resultsfilenameUSA "`resultsfilename'USA"
local resultsfilefullnameUSA  `rootpath'`logpathUSA'`resultsfilenameUSA'
postfile `usaresults' country_no tech_no str3 countryname str20 technologyname inventionyear nobs fixedeffect yearest elascoef1 elascoef2 trend fixedeffectstderr yeareststderr elascoef1stderr elascoef2stderr trendstder r2 r2detrended hasconverged plausprec using `resultsfilefullnameUSA' , every(1) replace

* ======================================================================================================================================
* Looping over the technologies for which we estimate the model 
* (see list above for which technology corresponds to which technology_no)
* --------------------------------------------------------------------------------------------------------------------------------------
forvalues technology_no = `starttechnology'(1)`endtechnology' {

* Setting the technology specific variable names
* 1.  Cars 
if `technology_no' == 1 {
	gen lnYtou = lcar
	local techname "Cars"
	local inventionyear = 1885
}
* 2.  PCs
else if `technology_no' == 2 {
	gen lnYtou = lpc
	local techname "PCs"
	local inventionyear = 1973
} 
* 3.  Electricity
else if `technology_no' == 3 {
	gen lnYtou = lel
	local techname "Electricity"
	local inventionyear = 1882
}
* 4.  Trucks
else if `technology_no' == 4 {
	gen lnYtou = ltruc
	local techname "Trucks"
	local inventionyear = 1885
}
* 5.  Blast oxygen
else if `technology_no' == 5 {
	gen lnYtou = lbof
	local techname "bof"
	local inventionyear = 1950
}
* 6.  Railways - freight
else if `technology_no' == 6 {
	gen lnYtou = lrailf
	local techname "rail_freight"
	local inventionyear = 1825
}
* 7.  Railways - passengers
else if `technology_no' == 7 {
	gen lnYtou = lrailp
	local techname "rail_pass"
	local inventionyear = 1825
}
* 8.  Telegrams
else if `technology_no' == 8 {
	gen lnYtou = ltelgraf
	local techname "Telegraph"
	local inventionyear = 1835
}
* 9.  Telephones
else if `technology_no' == 9 {
	gen lnYtou = ltel
	local techname "Telephone"
	local inventionyear = 1876
}
*10.  Aviation - freigth
else if `technology_no' == 10 {
	gen lnYtou = lat
	local techname "Aviation_freight"
	local inventionyear = 1903
}
*11.  Aviation - passengers 
else if `technology_no' == 11 {
	gen lnYtou = lap
	local techname "Aviation_passengers"
	local inventionyear = 1903
}
*12.  Cellphones
else if `technology_no' == 12 {
	gen lnYtou = ln(cellphone)
	local techname "Cellphones"
	local inventionyear = 1973
}
*13.  Internet
else if `technology_no' == 13 {
	gen lnYtou = ln(internetuser)
	local techname "Internet"
	local inventionyear = 1983
}
*14.  MRIs
else if `technology_no' == 14 {
	gen lnYtou = ln(med_mriunit)
	local techname "MRI"
	local inventionyear = 1977
}
*15.  Steam- and motorships
else if `technology_no' == 15 {
	gen lnYtou = ltonste
	local techname "Ships"
	local inventionyear = 1788
	if _N > 0 {
		* Interpolation of data 
		sort year
		capture tsset  year, yearly
		capture ipolate lymad  year, gen(lymadipol)
		capture replace lymad = lymadipol
		drop lymadipol	
		sort year
		capture tsset year, yearly
		capture ipolate  lpopmad  year, gen(lpopmadipol)
		capture replace lpopmad = lpopmadipol
		drop lpopmadipol
	}
}
* Undefined technology number ;-(
else {
	* this is the case when the variable is not defined
	error 111
}

* Setting the critical level of the standard error above which we classify estimate as imprecise
local criticalstderr = sqrt( 2003 - `inventionyear' )

* ======================================================================================================================================
* Looping over the country for which we estimate the model by choosing the country_no 
* (see list above for which country corresponds to which country_no)
* --------------------------------------------------------------------------------------------------------------------------------------
forvalues this_country_no = `startcountry'(1)`endcountry' {

* Determining whether we are in the case in which we calculate the U.S. benchmark cases or whether we are in the estimation for all the
* countries in the sample.
if `this_country_no' == 0 {
	local chosen_country_no = `US_country_no'
}
else { 
	local chosen_country_no = `this_country_no'
}

* Making sure nothing is logged anywhere and basically resetting the logging.
capture log off

* Make sure we only proceed if we have data for the chosen country number
set trace off
quietly summarize t if country_no==`chosen_country_no'
local nobs = r(N)
if `nobs' > 0 {

* Keeping only observations for the country that is studied and initializing the variable with the name of the country
local sampleselectstr "country_no==`chosen_country_no' & t != . & lnY != . & lnYtou != . & lnL != . & lnYpercap != . "

* Setting the starting value of the adoption year
set trace off
quietly summarize t if `sampleselectstr'
local nobs = r(N) 
local minttemp = r(min)
capture local b2start = (`minttemp'-5)
gen thiscountryvar = country if country_no==`chosen_country_no'
levelsof thiscountryvar , local(country_name)
local country_name = substr(`country_name',1,3)
drop thiscountryvar

* ======================================================================================================================================
* Looping over the different model specifications
* --------------------------------------------------------------------------------------------------------------------------------------
forvalues model_spec = `firstspec'(1)`lastspec' {

    * If we are estimating the benchmark estimates for the USA then we do not have the value of the parameters to impose the cross-country
    * restrictions.
    if `this_country_no' == 0 {
	if `model_spec' == 1 {
		local b3usar = "{b3}"
		local b4usar = "{b3}"
		local b5usar = "{b5}"
	}   
    }

   local b3usa = "`b3usar'"
   local b4usa = "`b4usar'"
   local b5usa = "`b5usar'"

    * =========================================================================================================================================
    * Defining the command string for the NL statement
    * -----------------------------------------------------------------------------------------------------------------------------------------
    local b3str = "{b3}"
    local b4str = "{b4}"
    local b5str = "{b5}"
    * This is the model estimated for the paper
    if `model_spec' == 1 {
	local b2str = "{b2=`b2start'}"
    	local b3str = "`b3usa'"
    	local b4str = "`b3str'"
    	local b5str = "`b5usa'"	
	matrix input whichcoefs = ( 1 , 2 , 0 , 0 , 0 )
    } 
    * This the model that restricts the trends and per capita GDP parameters to be the same across countries
    else if `model_spec' == 2{
	local b2str = "{b2=`b2start'}"
	local b3str = "{b3=`b3usa'}"
    	local b4str = "`b4usa'"
    	local b5str = "`b5usa'"
	matrix input whichcoefs = ( 1 , 2 , 3 , 0 , 0 )	
    }
    else {
	* this is the case when the variable is not defined
	error 111
    }
    local eqstring "( lnYtou = lnY + {b1} + (`mupar'-1)*ln(t-`b2str')*`b3str' - (1-`alfapar')*lnYpercap*`b4str' + `b5str'*t  ) "
    * =========================================================================================================================================
    
    * Adding parts to logfilename that distinguish the model specification
    local slashstr "\"
    local logfilename "`country_name'_`techname'spec`model_spec'"
    * Pointing to the USA specific directory that contains the benchmark estimates
    if `this_country_no' == 0 {
	    local logfilefullname `rootpath'`logpathUSA'`logfilename'	
    }
    * pointing to the technology-specific directory that contains the estimates for all countries
    else {
    	    local logfilefullname `rootpath'`logpath'`techname'`slashstr'`logfilename'
    }

    * Pointing to the file to which we would like to log our results. We make sure this is a txt-file that we can later edit to trans-
    * fer our results to another application
    capture log close
    capture log using `logfilefullname', replace
    capture set logtype text
    capture set trace off
    capture log off
    
    * This makes sure that the benchmark cases only get estimated for model_spec in {1,2}
    display `model_spec'
    if `this_country_no' == 0 & `model_spec' > 1 {
	local donotestimate = 1
    }
    else {
	local donotestimate = 0
    }

    if `nobs' < `minimumnobs' | `donotestimate' == 1 {
    	set trace off
    	capture log on
	if `nobs' < `minimumnobs' {
		display "`logfilename': THERE ARE NOT ENOUGH OBSERVATIONS " 
	}
	else {
		display "`logfilename': THIS IS A MODEL SPECIFICATION NOT ESTIMATED FOR THE U.S. AS BENCHMARK" 
	}
    	capture log off
    	set trace off
	local fixedeffectvalue = .
	local trendest = . 
	local elascoef1 = . 
	local yearest = . 
	local elascoef2 = . 
	local r2 = .
	local r2detrended = .
	local hasconverged = .
	local commentstr = -3
    }
    else {

	* Displaying the header for this estimation
	local headerline "============================================================================================================="
	local headerstr "Estimation for `techname' and `country_name', specification `model_spec'"
    
     	* Estimating the non-linear parameters using the nl command from STATA
    	capture log on
	display "`headerline'"
	display "`headerstr'"
	display "`headerline'"
	display "   "
	display "US parameters:"
	display "-------------------------------------------------------------------"
	display "elascoef1 = `b3usa'"
	display "elascoef2 = `b4usa'"
	display "trend     = `b5usa'"
	display "-------------------------------------------------------------------"
	display "   "
	display "Model specification estimated:"
	display "`eqstring'"
	display "-------------------------------------------------------------------"

    	capture noisily nl `eqstring' if `sampleselectstr' , iterate(`max_iter')
    	capture log off
    	set trace off
	* set trace on

	capture matrix define V = e(V)
	capture scalar dummyscal = sqrt(V[1,1])
     capture local fixedeffectstderr = dummyscal
    	capture scalar dummyscal = sqrt(V[2,2])
    	capture local yeareststderr = dummyscal
    	capture predict lnYtouhat if e(sample)
    	capture local r2 = e(r2)
	capture local hasconverged = e(converge)
    	
	set trace off
	* set trace on

	capture matrix define b = e(b)
    	capture scalar fixedeffectscalar = b[1,1]
    	capture local fixedeffectvalue = fixedeffectscalar
    	capture scalar yearestscalar = b[1,2]
    	capture local yearest = yearestscalar 

	if `this_country_no' == 0  {
		* Extracting the relevant estimation results to write to the results dataset	   	
		capture scalar elascoef1scalar = b[1,3]
		capture local elascoef1 = elascoef1scalar
		capture scalar elascoef2scalar = b[1,3]
		capture local elascoef2 = elascoef2scalar
		capture scalar trendestscalar = b[1,4]
 		capture local trendest = trendestscalar
		capture local b3usar = `elascoef1'
		capture local b4usar = `elascoef2'
		capture local b5usar = `trendest'
		capture local b3usavar = V[3,3]
		capture scalar dummyscal = sqrt(V[3,3])
	     capture local elascoef1stderr = dummyscal
		capture scalar dummyscal = sqrt(V[3,3])
	     capture local elascoef2stderr = dummyscal
		capture scalar dummyscal = sqrt(V[4,4])
	     capture local trendeststderr = dummyscal						
	}
	else { 

		* Extracting the relevant estimation results to write to the results dataset	   	
		if whichcoefs[1,3] == 0 {
			capture local elascoef1 = `b3usa'	
			capture local elascoef1stderr = .
		}
		else { 
			capture scalar elascoef1scalar = b[1,whichcoefs[1,3]]
    			capture local elascoef1 = elascoef1scalar
			capture scalar dummyscal = sqrt(V[whichcoefs[1,3],whichcoefs[1,3]])
	      	capture local elascoef1stderr = dummyscal
		}
		if whichcoefs[1,4] == 0 {
			capture local elascoef2 = `b4usa'
			capture local elascoef2stderr = .	
		}
		else { 
			capture scalar elascoef2scalar = b[1,whichcoefs[1,4]]
    			capture local elascoef2 = elascoef2scalar
			capture scalar dummyscal = sqrt(V[whichcoefs[1,4],whichcoefs[1,4]])
	      	capture local elascoef2stderr = dummyscal
		}
		if whichcoefs[1,5] == 0 {
			capture local trendest = `b5usa'
			capture local trendstderr = .	
		}
		else { 
			capture scalar trendestscalar = b[1,whichcoefs[1,5]]
    			capture local trendest = trendestscalar
			capture scalar dummyscal = sqrt(V[whichcoefs[1,5],whichcoefs[1,5]])
	      	capture local trendeststderr = dummyscal
		}
	}

	if `model_spec' == 2 {
		local b3var = V[3,3]
	}

	* Figuring out whether this estimate is plausible, precise, or plausible and precise
	if `yearest' < (`inventionyear'-10) {
    		local commentstr = -2
    	}
    	else if `yeareststderr' == . {
    		local commentstr = -1
    	}
    	else if `yeareststderr' > `criticalstderr' {
    		local commentstr = -1
    	}
    	else {
    		local commentstr = 1
    	}
    	set trace off
	* set trace on
    	
	* Calculating the `detrended' R-squared 
    	capture regress lnYtou t if e(sample)
	capture predict lnYtoudetrended if e(sample) , residuals
    	capture regress lnYtouhat t if e(sample)
    	capture predict lnYtouhatdetrended if e(sample) , residuals
    	capture log on
    	capture regress lnYtoudetrended lnYtouhatdetrended if e(sample)
    	capture log off
    	capture local r2detrended = e(r2)
	drop lnYtoudetrended lnYtouhat lnYtouhatdetrended

    * end of if for which there are enough observations and we are not in the benchmark case specs 3...6
    }

	if `model_spec' == 1 {
		local fixedeffectvaluer = `fixedeffectvalue'
		local trendestr = `trendest' 
		local elascoef1r = `elascoef1' 
		local yearestr = `yearest' 
		local elascoef2r = `elascoef2'
		local fixedeffectstderrr = `fixedeffectstderr'
		local yeareststderrr = `yeareststderr' 
		local elascoef1stderrr = `elascoef1stderr'
		local elascoef2stderrr = `elascoef2stderr'
		local trendeststderrr = `trendeststderr'
		local r2r = `r2'
		local r2detrendedr = `r2detrended'
		local commentstrr = `commentstr'
		local hasconvergedr = `hasconverged' 
	}
	else if `model_spec' == 2 {
		local fixedeffectvalueu = `fixedeffectvalue'
		local trendestu = `trendest' 
		local elascoef1u = `elascoef1'
		local yearestu = `yearest'
		local elascoef2u = `elascoef2' 
		local fixedeffectstderru = `fixedeffectstderr'
		local yeareststderru = `yeareststderr' 
		local elascoef1stderru = `elascoef1stderr'
		local elascoef2stderru = `elascoef2stderr'
		local trendeststderru = `trendeststderr'
		local r2u = `r2'
		local r2detrendedu = `r2detrended'
		local hasconvergedu = `hasconverged'
		local commentstru = `commentstr'
		local testrvsu = ( `elascoef1u' - `b3usa' )/sqrt( `b3var' + `b3usavar' )
		local ptestrvsu = 2*(1-normal(abs(`testrvsu')))
	}

* --------------------------------------------------------------------------------------------------------------------------------------
* End the loop over model_spec
* --------------------------------------------------------------------------------------------------------------------------------------
}

set trace off

* Writing this to the results dataset
if `this_country_no' == 0 {
	post `usaresults' (`chosen_country_no') (`technology_no') ("`country_name'") ("`techname'") (`inventionyear') (`nobs') (`fixedeffectvaluer') (`yearestr') (`elascoef1r') (`elascoef2r') (`trendestr') (`fixedeffectstderrr') (`yeareststderrr') (`elascoef1stderrr') (`elascoef2stderrr') (`trendeststderrr') (`r2r') (`r2detrendedr') (`hasconvergedr') (`commentstrr')	
} 
else if `this_country_no' != 0  { 
	post `allresults' (`chosen_country_no') (`technology_no') ("`country_name'") ("`techname'") (`inventionyear') (`nobs') (`fixedeffectvaluer') (`yearestr') (`elascoef1r') (`elascoef2r') (`trendestr') (`fixedeffectstderrr') (`yeareststderrr') (`elascoef1stderrr') (`elascoef2stderrr') (`trendeststderrr') (`r2r') (`r2detrendedr') (`hasconvergedr') (`commentstrr') (`fixedeffectvalueu') (`yearestu') (`elascoef1u') (`elascoef2u') (`trendestu') (`fixedeffectstderrr') (`yeareststderrr') (`elascoef1stderru') (`elascoef2stderru') (`trendeststderrr') (`r2u') (`r2detrendedu') (`hasconvergedu') (`commentstru') (`ptestrvsu') 
} 

* End of if that makes sure there is any data for the country
}

* --------------------------------------------------------------------------------------------------------------------------------------
* End the loop over the country_no
* --------------------------------------------------------------------------------------------------------------------------------------
}

drop lnYtou

* --------------------------------------------------------------------------------------------------------------------------------------
* End the loop over the technology_no
* --------------------------------------------------------------------------------------------------------------------------------------
}

* closing all the postfiles to which the results are being written
postutil clear    

