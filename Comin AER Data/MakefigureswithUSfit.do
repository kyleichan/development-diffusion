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
* DO (not Run) this file and output will be written to sequence of country specific text files in directory for the technology 
* for which program is run as well as a Result**.txt file in the output directory.
*
* 
* ==================================================================================================================================== 
* Single Technologies:
* ------------------------------------------------------------------------------------------------------------------------------------ 
* 1.  Cars 
* 2.  PCs
* 3.  Electricity
* 4.  Trucks
* 5.  Blast oxygen		  (This has a problem for the estimation for the U.S.)
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

* Setting the STATA version in which we are running this DO file
version 9

* Initializing the program by clearing the memory and allocating enough space to load all the data
clear
set more off, permanently
set linesize 255
set trace on
set mem 300m
set logtype text

* Setting the location of the files 
* NOTE: Change the rootpath to where you have decompressed the files from the zip-file to.
local rootpath = "C:\Bart\Research\Histtech\progs2\ReplicationFiles\"
local logpath = "\output\"

* Choosing the technologies for which the program is run
local starttechnology 1
local endtechnology 15

* Setting the calibrated parameter values
local alfapar = 0.30
local mupar = 1.30

* Set the maximum number of iterations 
local max_iter = 50

* The country number of the U.S. in the dataset
local US_country_no = 155

* Loading the data
local datafile `rootpath'CHExplorationData.dta
use `datafile', clear

* Defining the variables
gen t = year
gen lnY = lymad
gen lnL = lpopmad 
gen lnYpercap = lnY - lnL

keep if country_no==`US_country_no' 

* ======================================================================================================================================
* Looping over the technologies 
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

* Keeping only observations for the country that is studied and initializing the variable with the name of the country
local sampleselectstr " t != . & lnY != . & lnYtou != . & lnL != . & lnYpercap != . "

* Setting the starting value of the adoption year 
quietly summarize t if `sampleselectstr'
local nobs = r(N) 
local minttemp = r(min)
capture local b2start = (`minttemp'-5)

* Defining the command string for the NL statement when model is estimated without gamma
local eqstring "( lnYtou = lnY + {b1} + (`mupar'-1)*ln(t-{b2=`b2start'})*{b3} - (1-`alfapar')*lnYpercap*{b3} + {b5}*t  ) "
* Checking whether there are enough observations left to actually estimate the model
local nobs = _N

* Adding parts to logfilename that distinguish the model specification
local underscorestr "_"
local fitfilestr "USFIT_"
local logfilename `fitfilestr'`underscorestr'`techname'

* Pointing to the file to which we would like to log our results. We make sure this is a txt-file that we can later edit to trans-
* fer our results to another application
local slashstr "FitFigures\\"
local logfilefullname  `rootpath'`logpath'`slashstr'`logfilename'
capture log close
capture log using `logfilefullname', replace
capture set logtype text
capture set trace off
capture log off

* Initializing the output strings as missings
local yearest "."
local yearstderr "."
local r2 "."
local r2detrended "."

* Estimating the non-linear parameters using the nl command from STATA
capture log on
capture noisily display "Model specification estimated:"
capture noisily display "`eqstring'"
capture noisily display "-------------------------------------------------------------------"
capture noisily nl `eqstring' if `sampleselectstr' , iterate(`max_iter')
capture predict lnYtouhat if e(sample)
capture noisily list t lnYtou lnYtouhat , separator(0)
capture log off
set trace off

* Closing the log-file
capture log close
set trace off

drop lnYtou lnYtouhat

* --------------------------------------------------------------------------------------------------------------------------------------
* End the loop over the technologies
* --------------------------------------------------------------------------------------------------------------------------------------
}

* clear the data from memory to reload for next country
* clear

