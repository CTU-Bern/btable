*! version 1.0.1 29mar2021
cap program drop btable
program btable, nclass
        
version 15

syntax varlist [if] [in], SAVing(string) ///
	[BY(varname) ///
	CATegorical(varlist) CONTInuous(varlist) ///
	order(numlist) ///
	count(varlist) exp_time(varlist) exp_time_units(string) nbreg ///
	tte(varlist) fail(varlist) trunc_time(string) surv_time_units(string) ///
	ALLlevels DENOMinator(string)  ///
	effect(string) test(string) ///
	user_var(string) user_command(string) user_parse(string) MARgins ///
	user_post(string) user_name(string) ///
	Level(cilevel) ///
	prci(string) ///
	rdci(string) rrci(string) orci(string) hlmdci(string) mwsci(string) ///
	irrci(string) ///
	ttest(string) qreg(string) ///
	lr(string) ///
	chi2_lr ///
	cox_lr poisson_lr nbreg_lr ///
	ranksum_exact ///
	poisson_test(string) ///
	rmst_model(string) ///
	hr_model(string) ///
	fpm_df(string)   ///
	type(string) /// old option, not used anymore, only for compatibility
	parent(varlist) kid(varlist) parent_level(string) ///
	] 	
	
marksample touse, novarlist


**************	
*entry checks
****************
	
qui ds `varlist', has(type string)		
local wcs: word count `r(varlist)'
if `wcs'!=0 {
	dis as error "String variables not allowed: `r(varlist)'"
	exit
	//exit= 999, clear
} 

capture confirm string variable `by'
if _rc==0 {
	dis as error "Group variable cannot be a string."
	exit
}
	
cap assert "`type'"=="" if "`categorical'" !="" | "`continuous'" !=""
if _rc {
	dis as error "Options type and categorical/continuous cannot be combined"
	exit
}

if "`type'"!="" {
	dis as text "Option type has been replaced, consider using categorical and continuous."
}


if "`denominator'"!="" {
cap assert "`denominator'"=="nonmiss" 
	if _rc {
		dis as text "Option for denominator not recognized, nonmiss assumed"
	}
}

if "`lr'"!="" {
	local lrwc: word count `lr' 
	forvalues lri = 1/`lrwc' {
		local lrwi: word `lri' of `lr'
		cap assert inlist("`lrwi'","chi2","cox","poisson","nbreg")
		if _rc {
			dis as text "`lrwi' is not a valid option for lr --- will be ignored"
		}
	}
}

*varname that corresponds to variable type:
local key conti cat count tte miss
foreach v of local varlist {
	cap assert !inlist("`v'","conti","cat","count","tte","miss")
	if _rc {
		dis as text "Variable name `v' corresponds to a variable type. May cause problems with btable_format."
	}
}	


if "`count'"!="" {
	local cwc: word count `count'
	local twc: word count `exp_time'
	local lwc: word count `exp_time_units'
	cap assert `cwc'==`twc'
	if _rc & `twc'!=1 {
		dis as error "For each count variable, an exposure time variable has to be specified via exp_time."
		exit
	}
	cap assert `twc'==`lwc'
	if _rc & `lwc'!=1 & `lwc'!=0  {
		local text1 "The number of units for the exposure time does not correspond to"
		local text2 "the number of exposure time variables - option exp_time_units is ignored."
		dis as text "`text1' `text2'"
		
		local exp_time_units
		local lwc 0
		local etime_unit
		local etime_post
	}
	if `lwc'==0 {
		local etime_unit
		local etime_post
	}
	if `lwc'>0 {
		local etime_unit str244 etime_unit
		local etime_post
		local etime_post ("")
	}
}

if "`tte'"!="" {
	local ttewc: word count `tte'
	local ttefwc: word count `fail'
	local trtwc: word count `trunc_time'
	local stuwc: word count `surv_time_units'
	local rmwc: word count `rmst_model'
	local dfwc: word count `fpm_df'
	
	//failure
	cap assert `ttewc'==`ttefwc'
	if _rc {
		dis as error "For each time-to-event variable a failure variable has to be given."
		exit
	}
	foreach l of local fail {
		cap assert inlist(`l',0,1,.)
		if _rc {
			dis as error "Variable `l' contains invalid numbers."
			dis as error "Variables defined in fail have to be coded as 0 (censored) and 1 (failure)."
			exit
		}
	}
	
	//trunc_time
	cap assert `ttewc'==`trtwc'
	if _rc & `trtwc'!=1 & `trtwc'!=0  {
		dis as text "Trunction time has to be given for each variable - option trunc_time is ignored."
		local trunc_time
	}
	foreach l of local trunc_time {
		cap confirm number `l'
		if _rc {
			dis as text "Only numbers allowed in trunc_time - option trunc_time is ignored."
			local trunc_time
		}
	}	
	
	cap assert `ttewc'==`stuwc'
	if _rc & `stuwc'!=1 & `stuwc'!=0  {
		local text1 "The number of units for the survival time does not correspond to"
		local text2 "the number of time-to-event variables - option surv_time_units is ignored."
		dis as text "`text1' `text2'"
		
		local surv_time_units
		local stuwc 0
		local stime_unit
		local stime_post
	}
	if `stuwc'==0 {
		local stime_unit
		local stime_post
	}
	if `stuwc'>0 {
		local stime_unit str244 stime_unit
		local stime_post
		local stime_post ("")
	}
	cap assert inlist(`rmwc',0,1,`ttewc')
	if _rc {
		local text1 "Length of rmst_model has to be either one or correspond to the number of tte variables - "
		local text2 "default (npar) used."
		dis as text "`text1' `text2'"
		local rmst_model npar
	}
	if "`rmst_model'" != "" {
		local fpmwc = 0
		foreach l of local rmst_model {
			cap assert inlist("`l'","npar","fpm") //pseudo would be a possibility
			if _rc {
				dis as text "Option for rmst_model not recognized. Only npar and fpm allowed. Default (npar) used."
				local rmst_model npar
			}
			if "`l'" == "fpm" {
				local fpmwc = `fpmwc' + 1 
			}	
		}
		cap assert inlist(`dfwc',0,1,`fpmwc')
		if _rc {
			local text1 "The numbers of elements in fpm_df has to be 1 or correspond to the number of fpm in rmst_model - "
			local text2 "default (3) used."
			dis as text "`text1' `text2'"
			local fpm_df 3
		}
		if "`fpm_df'" !="" {
			foreach l of local fpm_df {
				cap confirm integer number `l'
				if _rc {
					dis as text "Only integers allowed in fpm_df - default (3) used."
					local fpm_df 3
				}
			}
		}	
	}
}

if "`parent'"!="" {
	cap assert "`kid'"!=""
	if _rc {
		dis as text "Option kid not specified, parent has no effect."
		local parent
	}
}

if "`kid'"!="" {
	cap assert "`parent'"!=""
	if _rc {
		dis as text "Option parent not specified, kid has no effect."
		local kid
	}
	else {
		cap assert "`parent_level'"!=""
		if _rc {
			dis as text "Option parent_level not specifed --- 1 is assumed."
			local parent_level 1
		}
		
		local kwc: word count `kid'
		local pwc: word count `parent'
		local plwc: word count `parent_level'
		
		cap assert `kwc'==`pwc'
		if _rc {
			dis as text "Options kid and parent do not have the same number of words --- options ignored."
			local kid
			local parent
			local parent_level
		}
		else {
			cap assert inlist(`plwc',1,`kwc')
			if _rc {
				dis as text "Options parent_level does not have the same number of words as kid and parent --- 1 is assumed."
				local parent_level 1
			}
		}	
	}
}

*ordering
*****************

if "`by'"!="" {
	qui levelsof `by', local(glev)
	local wc: word count `glev'
	if "`order'" != "" {
		local wcg: word count `order'
		cap assert `wc'==`wcg'
		if _rc {
			dis as text "All levels of by variable have to be included in order --- ordering ignored."
			local order=""
		}
		
		foreach o of local order {
			cap assert strpos("`glev'","`o'")>0
			if _rc {
				dis as text "Level `o' not in by variable --- ordering ignored."
				local order=""
			}
			local ocomp=subinstr("`order'","`o'","",1)
			cap assert strpos("`ocomp'","`o'")==0
			if _rc {
				dis as text "Duplicates in order option --- ordering ignored."
				local order=""
				
			}
		}

	}
}


*options for test
******************
if "`test'" == "none" {
	local testlist
} 
else {
	
	local testlist fisher chi2 ttest ranksum qreg anova kwallis poisson logrank cox rmstd
	
	if "`test'" != "" & "`test'" !="all" {
	
		tempname lent lcheck ldiff
		local `lent' = strlower("`test'")
		local `lcheck': list `lent' in testlist
		if ``lcheck'' == 0 {
			local `ldiff': list `lent' - testlist
			local `ldiff' = subinstr("``ldiff''"," ",", ",.)
			dis as text "Unrecognized test (``ldiff''), will be ignored. Only fisher, chi2, ttest, ranksum, qreg, anova, kwallis, poisson, logrank, cox and rmstd are allowed."
		}
		
		local testlist = strlower("`test'")
	}
	
}

if strpos("`testlist'","fisher")>0 {
	local fisher fisher
}


*options for effect measure
********************************
if "`effect'" == "none" {
	local effectlist
} 
else {
	local effectlist rd rr or meand medd hlmd mws ird irr hr rmstd
	
	if "`effect'" != "" & "`effect'" !="all" {
	
		tempname lent lcheck ldiff
		local `lent' = strlower("`effect'")
		local `lcheck': list `lent' in effectlist
		if ``lcheck'' == 0 {
			local `ldiff': list `lent' - effectlist
			local `ldiff' = subinstr("``ldiff''"," ",", ",.)
			dis as text "Unrecognized effect measures (``ldiff''), will be ignored. Only rd, rr, or, meand, medd, hlmd, mws, ird, irr, hr and rmstd are allowed."
		}
		
		local effectlist=strlower("`effect'")
	}
}


*nbreg
************************
if "`nbreg'"!="" & "`test'" != "none"  {

local testlist `testlist' nbreg

}



*options for confidence intervals	
***********************************

*prop
if "`prci'"=="" {
	local prci="wilson"
}
else {
	local prci=strlower("`prci'")
}
if  "`prci'"!="" & !inlist("`prci'","exact","wald","wilson","agresti","jeffreys") {
	dis as text "Unknown option for prci, only exact, wald, wilson, agresti and jeffreys are allowed. Default (wilson) is used."
	local prci="wilson"
}

*rd
if "`rdci'"!="" {
	local rdci=strlower("`rdci'")
}
if "`rdci'"!="" & strpos("`effectlist'","rd")==0 {
	dis as text "Risk difference (rd) not included in effect list, option rdci ignored."
}
if  "`rdci'"!="" & !inlist("`rdci'","wald","ac","nhs","wall","mn") {
	dis as text "Unknown option for rdci, only wald, AC, NHS, wall, or MN are allowed. Default (wald) is used."
	local rdci=""
}

*rr
if "`rrci'"!="" {
	local rrci=strlower("`rrci'")
}
if "`rrci'"!="" & strpos("`effectlist'","rr")==0 {
	dis as text "Risk ratio (rr) not included in effect list, option rrci ignored."
}
if  "`rrci'"!="" & !inlist("`rrci'","katz","koopman") {
	dis as text "Unknown option for rrci, only katz and koopman are allowed. Default (katz) is used."
	local rrci=""
}


*or
if "`orci'"!="" {
	local orci=strlower("`orci'")
}
if "`orci'"!="" & strpos("`effectlist'","or")==0 {
	dis as text "Odds ratio (or) not included in effect list, option orci ignored."
}
if  "`orci'"!="" & !inlist("`orci'","woolf","exact","cornfield","agresti","gart") {
	dis as text "Unknown option for orci, only woolf, exact, cornfield, agresti, gart are allowed. Default (woolf) is used."
	local orci=""
}

*hlmd
if "`hlmdci'"!="" {
	local hlmdci=strlower("`hlmdci'")
}
if "`hlmdci'"!="" & strpos("`effectlist'","hlmd")==0 {
	dis as text "Hodges-Lehmann difference (hlmd) not included in effect list, option hlmdci ignored."
}
if  "`hlmdci'"!="" & !inlist("`hlmdci'","lehmann","newson") {
	dis as text "Unknown option for hlmdci, only newson and lehmann are allowed. Default (newson) is used."
	local hlmdci=""
}

*mws
if "`mwsci'"!="" {
	local mwsci=strlower("`mwsci'")
}
if "`mwsci'"!="" & strpos("`effectlist'","mws")==0 {
	dis as text "Mann-Whitney statistic (mws) not included in effect list, option mwsci ignored."
}
if  "`mwsci'"!="" & !inlist("`mwsci'","jackknife","delong","bamber","hanley","fay") {
	dis as text "Unknown option for mwsci, only delong, jackknife, bamber, and hanley are allowed. Default (delong) is used."
	local mwsci=""
}

*irr
if "`irrci'"!="" {
	local irrci=strlower("`irrci'")
}
if "`irrci'"!="" & strpos("`effectlist'","irr")==0 {
	dis as text "Incidence rate ratio (irr) not included in effect list, option irrci ignored."
}
if  "`irrci'"!="" & !inlist("`irrci'","exact","oim","robust") {
	dis as text "Unknown option for irrci, only exact, oim, and robust are allowed. Default (exact) is used."
	local irrci=""
}
if "`irrci'"!="" & "`nbreg'" != "" {
	dis as text "Incidence rate ratio (irr) calculated by negative binomial regression, irrci is ignored."
}


*options for test
*******************

if "`ttest'"!="" {
cap assert strpos("`testlist'","ttest")>0 
	if _rc {
		dis as text  "ttest not included in test, option ttest has no effect."
	}
}

if "`chi2_lr'"!="" | strpos("`lr'","chi2")>0 {
	cap assert strpos("`testlist'","chi2")>0 
	if _rc {
		dis as text  "chi2 not included in test, option lr(chi2) has no effect."
	}
}

if "`ranksum_exact'"!="" {
cap assert strpos("`testlist'","ranksum")>0 
	if _rc {
		dis as text  "ranksum not included in test, option ranksum_exact has no effect."
	}
}

if "`cox_lr'"!="" | strpos("`lr'","cox")>0 {
cap assert strpos("`testlist'","cox")>0 
	if _rc {
		dis as text  "cox not included in test, option lr(cox) has no effect."
	}
}

if "`poisson_test'"!="" {
cap assert strpos("`testlist'","poisson")>0 
	if _rc {
		dis as text "Poisson not included in test, option poisson_test has no effect."
	}
}

if "`poisson_lr'"!="" | strpos("`lr'","poisson")>0 {
	cap assert strpos("`testlist'","poisson")>0
	if _rc {
		dis as text  "Poisson not included in test, option lr(poisson) has no effect."
	}
	cap assert "`poisson_test'" !=""
	if _rc {
		dis as text  "poisson_test not specified, option lr(poisson) has no effect."
	}
	cap assert "`poisson_test'" != "robust" 
	if _rc {
		dis as text  "Likelihood ratio test nor possible with robust standard errors, option lr(poisson) has no effect."
	}
}

if "`nbreg_lr'"!="" | strpos("`lr'","nbreg")>0 {
cap assert  "`nbreg'" !=""
	if _rc {
		dis as text  "Option nbreg not specified, lr(nbreg) has no effect."
	}
}


*defaults for tte
************************

if "`rmst_model'" == "" {
	local rmst_model npar
}

if "`fpm_df'" == "" {
	local fpm_df = 3
}
local counter_fpm = 0


*install auxilliary files
***********************

if !inlist("`rdci'","","wald") &  strpos("`effectlist'","rd")>0 {
	cap which rdci
	if _rc {
		dis as error "rdci is required; type -ssc install rdci- to obtain it"
		exit 499
		//dis "Installing auxiliary ado-file rdci: "
		//ssc install rdci
	}
}
if "`rrci'"=="koopman"  &  strpos("`effectlist'","rr")>0 {
	cap which koopman
	if _rc {
		dis as error "koopman is required; type -net install sg154, from(http://www.stata.com/stb/stb58)- to obtain it"
		exit 499
		//dis "Installing auxiliary ado-file koopman: "
		//net install sg154, from(http://www.stata.com/stb/stb58)
	}
}
if ("`orci'"=="gart" | "`orci'"=="agresti") &  strpos("`effectlist'","or")>0 {
	cap which oddsrci
	if _rc {
		dis as error  "oddsrci is required; type -net install sbe30, from(http://www.stata.com/stb/stb51)- to obtain it"
		exit 499
		//dis "Installing auxiliary ado-file oddsrci: "
		//net install sbe30, from(http://www.stata.com/stb/stb51)
	}
}
if "`hlmdci'"=="lehmann" &  strpos("`effectlist'","hlmd")>0 {
	cap which npshift
	if _rc {
		dis as error  "npshift is required; type -net install sg123, from(http://www.stata.com/stb/stb52)- to obtain it"
		exit 499
		//dis "Installing auxiliary ado-file npshift: "
		//net install sg123, from(http://www.stata.com/stb/stb52)
	}
}
if (strpos("`effectlist'","hlmd")>0 &  "`hlmdci'"!="lehmann") ///
	| strpos("`effectlist'","mws")>0 & "`mwsci'"=="jackknife" {
	cap which somersd
	if _rc {
		dis as error "somersd is required; type -ssc install somersd- to obtain it"
		exit 499
		//dis "Installing auxiliary package somersd: "
		//ssc install somersd
	}
}		
	

***************
*sample size
****************

qui count if `touse'
local nt=r(N)
	
//sample size limit for Fisher's exact test	
local nlimfish=5000
local nclimfish=10



*******************
*by group
*************

local pvar

if "`by'"!="" {
	
	cap assert !missing(`by')
	if _rc {
		dis as text "Missings in by variable will be ignored."
	}
	
	
	*effect measures and tests  for postfile
	
	qui levelsof `by' if `touse', local(glev)	
	
	if "`order'" != "" {
		//local glev `intervention' `=subinstr("`glev'","`intervention'","",1)'  //"  
		local glev `order'
	}
	
	local gc: word count `glev'
	local pvar 
	local i=0
	foreach g of local glev {
		local i=`i'+1
		local pvar `pvar' ntot_`i' nnonmiss_`i' nlev_`i' ///
			pr_`i' prlci_`i' pruci_`i' ///
			mean_`i' sd_`i'  meanlci_`i' meanuci_`i' ///
			p50_`i' p25_`i' p75_`i' iqr_`i' min_`i' max_`i' range_`i' sum_`i' ///
			nevents_`i' etime_`i' ir_`i' irlci_`i' iruci_`i' ///
			nfails_`i' stime_`i' ///
			st50_`i' st50lci_`i' st50uci_`i' st25_`i' st75_`i' ///
			rmst_`i' rmstlci_`i' rmstuci_`i'
			
		qui count if `by'==`g' & `touse'
		local nt_`g'=r(N)
		local gl_`g': label (`by') `g'
	}
	if `gc'==2 {
		local diff 	p_chi2 p_fisher ///
					rd rd_lci rd_uci ///
					rr rr_lci rr_uci ///
					or or_lci or_uci ///
					p_ttest p_ranksum p_qreg ///
					meand meand_lci meand_uci ///
					medd medd_lci medd_uci ///
					hlmd hlmd_lci hlmd_uci ///
					mws mws_lci mws_uci ///
					p_poisson p_nbreg ///
					ird ird_lci ird_uci ///
					irr irr_lci irr_uci ///
					p_logrank p_cox p_rmstd ///
					hr hr_lci hr_uci ///
					rmstd rmstd_lci rmstd_uci
	}
	if `gc'>2 {
		local diff p_chi2 p_fisher p_anova p_kwallis ///
			p_poisson p_nbreg ///
			p_logrank p_cox
	}
	
	local pvar `pvar' `diff'
	
	
	*prepare and save group file

	preserve
	local vlab: val label `by'
	local varlab: var label `by'
	local nall
	
	if "`vlab'"!="" {
		cap uselabel `vlab', clear
		if _rc==0 {
			qui gen vname="`by'"
			qui gen vlabel="`varlab'"
			qui gen nt=`nt'
			qui gen ntg=.
			qui count
			forvalues i=1/`r(N)' {
				local na=value[`i']
				if "`nt_`na''"!="" {
					qui replace ntg=`nt_`na'' in `i'
				}
				else {
					qui replace ntg=0 in `i'
				}
			}
			qui sum nt 
			local nls1=r(mean)
			qui sum ntg
			local nls2=r(sum)
			qui count
			if `r(N)'!=`gc' | `nls1' != `nls2' {
				display "Not all levels of the by variable are labeled."
				local nall=1
			} 
			else {
				if "`order'"!="" {
					local i=1
					tempvar seqg
					qui gen `seqg' = .
					foreach o of local order {
						qui replace `seqg' = `o' in `i'
						local i=`i'+1
					}
					sort `seqg'
					qui drop `seqg'
				}
			}
		}	
	}	
	if "`vlab'"=="" | _rc==111 | "`nall'" != "" {
		clear
		qui set obs `gc'
		qui gen lname=""
		qui gen value=.
		qui gen label=""
		qui gen vname="`by'"
		qui gen vlabel="`varlab'"
		qui gen nt=`nt'
		qui gen ntg=.
		local i=0
		foreach g of local glev {
			local i=`i'+1
			qui replace ntg=`nt_`g'' in `i'	
			qui replace value=`g' in `i'
			qui replace label="`gl_`g''" in `i'
		}
	}
	
	qui count
	forvalues i=1/`r(N)' {
		local glab`i'=label[`i']
	}
	
	cap drop trunc
	qui save "`saving'_group", replace
	restore
}



***************************	
*user defined commands
*****************************

//defaults
local add_user_var 
local add_user_var_base user_pe user_lci user_uci user_p	
local add_user_var_post
local add_user_var_post_base 
local uname = 0

if "`user_parse'"=="" {
	local user_parse "|"
}

//check number of entries
if "`user_var'" != "" {
	
	//check that user_var contain only valid varnames or parsing character
	local blockcheck = 1
	tempname v
	foreach `v' of local user_var {
		
		if "``v''" != "`user_parse'" {
			
			cap ds ``v''
			if _rc {
	
				cap assert inlist("``v''","conti","cat","tte","count")
				
				if _rc {
					dis as text "Error in user_var: ``v'' is not an existing variable, conti, cat, tte or count---user options ignored"
					local blockcheck = 0
				}
			}
		}	
	}
	
	cap assert "`user_command'" != "" 
	if _rc {
		dis as text "user_command not given, user options ignored"
		local blockcheck = 0
	}
		
	tempname bcount bcount1 bcount2 bcount3	bcount4
	tempname a
	local `a': subinstr local user_var "`user_parse'" "`user_parse'", all count(local `bcount')
	local `bcount1' = ``bcount'' + 1 
	
	tempname a
	local user_command_h = subinstr("`user_command'","||","",.) 
	local `a': subinstr local user_command_h "`user_parse'" "`user_parse'", all count(local `bcount')
	local `bcount2' = ``bcount'' + 1 

	cap assert ``bcount1'' == ``bcount2''
	if _rc {
		dis as text "The number of user_var does not match the number of user_command---user options ignored"
		local blockcheck = 0
	}
	
	if "`user_post'" != "" {
		tempname a
		local `a': subinstr local user_post "`user_parse'" "`user_parse'", all count(local `bcount')
		local `bcount3' = ``bcount'' + 1
		cap assert ``bcount1'' == ``bcount3'' |  ``bcount3''==1

		if _rc {
			dis as text "The number of user_post does not match the number of user_var---user options ignored"
			local blockcheck = 0
		}
	}
	
	if "`user_name'" != "" {
		tempname a
		local `a': subinstr local user_name "`user_parse'" "`user_parse'", all count(local `bcount')
		local `bcount4' = ``bcount'' + 1
		if "`user_post'" != "" {
			cap assert ``bcount3'' == ``bcount4'' |  ``bcount4''==1
			if _rc {
				dis as text "The number of user_name does not match the number of user_post---user options ignored"
				local blockcheck = 0
			}
		}
		else {
			cap assert ``bcount1'' == ``bcount4'' |  ``bcount4''==1
			if _rc {
				dis as text "The number of user_name does not match the number of user_var---user options ignored"
				local blockcheck = 0
			}
		}
	}
	
	if `blockcheck'==1 {
	
		//repair for || in mixed
		tempname cr1
		local user_commandr = subinstr("`user_command'","||","`cr1'",.)

		tknz "`user_var'", parse("`user_parse'") nochar stub(var)
		tknz "`user_commandr'", parse("`user_parse'") nochar stub(command)
		
		if "`user_post'" != "" {
			tknz "`user_post'", parse("`user_parse'") nochar stub(post)
		}
		if "`user_name'" != "" {
			tknz "`user_name'", parse("`user_parse'") nochar stub(name)
		}

		
		if "`user_post'" != "" {	
			
			local add_user_var
					
			forvalues cun = 1/``bcount3'' {
			
				local uwc: word count `post`cun''	
				
				if "`user_name'" != "" {
					
					if ``bcount4''==1 {
						
						if `cun'==1 {
							local unu: list uniq user_name
							local uwcn: word count `unu'
							cap assert `uwc' == `uwcn'
							if _rc {
								dis as text "Number of unique words in user_name does not match user_post---user_name ignored."
								local user_name 
							}
							else {	
								local add_user_var `user_name'
								local uname = 1
							}
						}
					}
					else {
						local unu: list uniq name`cun'
						local uwcn: word count `unu'
						
						cap assert `uwc' == `uwcn'
						if _rc {
							dis as text "Number of unique words in user_name does not match user_post---user_name ignored."
							local user_name 
						}
						else {
							local add_user_var `add_user_var' `name`cun''
						}
					}
				}
				if "`user_name'" == "" { 
					forvalues i=1/`uwc' {
						if ``bcount3''==1  & ``bcount1''==1 {
							local add_user_var `add_user_var' user`i'
						}
						else {
							if ``bcount3''==1  & ``bcount1''>1 {
								forvalues j=1/``bcount1'' {
									local add_user_var `add_user_var' user`j'`i'
								}
							}
							else {
								local add_user_var `add_user_var' user`cun'`i'
							}	
						}
					}
				}				
			}	
		} 
		
		else {
			if "`user_name'" != "" {
				
				local ern 0
				local add_user_var
				if "`by'"=="" | inlist("`gc'","1","2") {
					local checkno 4
				}
				else {
					local checkno 1
				}
				
				
				if ``bcount4''==1 {
					local unu: list uniq user_name
					local wcun: word count `unu'
					cap assert `wcun'==`checkno'
					if _rc {
						local ern 1
					}
					else {
						local add_user_var `user_name'
					}
				} 
				else {
					forvalues i=1/``bcount4'' {
						local unu: list uniq name`i'
						local wcun: word count `unu'
						cap assert `wcun'==`checkno'
						if _rc {
							local ern 1
						}
						else {
							local add_user_var `add_user_var' `name`i''
						}
					}
				}
				if `ern'==1 {
					dis as text "Incorrect number of user_names---user_name ignored"
					local user_name
				}
			}
			if "`user_name'" == "" { 
				if ``bcount1''==1 {
					local add_user_var user_pe user_lci user_uci user_p		
				} 
				else {	
					local add_user_var
					forvalues i=1/``bcount1'' {
						local add_user_var `add_user_var' user`i'_pe user`i'_lci user`i'_uci user`i'_p	
					}
				}
			}
		}
		
		//name of variables to post and empty post
		local add_user_var_un: list uniq add_user_var 
		local awc: word count `add_user_var_un'
		forvalues i=1/`awc' {
			local add_user_var_post_base `add_user_var_post_base' (.)
		}
	
	}	
}		

//dis "`add_user_var'"
//dis "`add_user_var_un'"
//dis "`add_user_var_post_base'"


************
*postfile
************

tempname res
postfile `res' str244 varname str244 varlabel str50 vtype ///
	level str244 levlabel nlev `etime_unit' `stime_unit' ///
	ntot_t nnonmiss_t nlev_t ///
	pr_t prlci_t pruci_t ///
	mean_t sd_t meanlci_t meanuci_t ///
	p50_t p25_t p75_t iqr_t min_t max_t range_t sum_t ///
	nevents_t etime_t ir_t  irlci_t  iruci_t ///
	nfails_t stime_t ///
	st50_t st50lci_t st50uci_t st25_t st75_t ///
	rmst_t rmstlci_t rmstuci_t ///
	`pvar' /// 
	`add_user_var_un' ///
	kid ///
	using "`saving'", replace

	
**************
*loop through variables
**************
	
local counter=0			  
foreach var of local varlist {
	
	local counter=`counter'+1
	local testlist2 `testlist'  //variable specific testlist

	
	*sample size overall and in each group	
	*******************************
	
	qui count if `touse'
	local nt=r(N)
	qui count if !missing(`var') & `touse'
	local nnm=r(N)
	
	tempvar touse2
	tempvar touse3
	
	gen `touse2'=`touse'
	gen `touse3'=`touse'

	if "`denominator'"!="" & inlist("`denominator'","nonmiss","nmiss","nomiss") {
		qui replace `touse2'=0 if missing(`var')
	}

	if "`by'"!="" {
	foreach g of local glev { 
		qui count if `by'==`g' & `touse'
		local nt_`g'=r(N)
		qui count if !missing(`var') & `by'==`g' & `touse'
		local nnm_`g'=r(N)
		}
	}	
		
	local kidp = 0
	
	if strpos(" `kid' "," `var' ")>0  {

		local pos: list posof "`var'" in kid
		local parenti: word `pos' of `parent'
		local plwc: word count `parent_level'
		if `plwc'==1 {
			local levi `parent_level'
		}
		else {
			local levi: word `pos' of `parent_level'
		}
		
		qui count if `touse2' & `parenti' == `levi'
		local nt = r(N)
		qui count if !missing(`var') & `touse2' & `parenti' == `levi'
		local nnm=r(N)
		
		qui replace `touse2' = 0 if `parenti' != `levi'
		qui replace `touse3' = 0 if `parenti' != `levi'
		
		if "`by'"!="" {
		foreach g of local glev { 
			qui count if `by'==`g' & `touse'  & `parenti' == `levi'
			local nt_`g'=r(N)
			qui count if !missing(`var') & `by'==`g' & `touse'  & `parenti' == `levi'
			local nnm_`g'=r(N)
			}
		}
		
		//local parent rep78 bin
		//local kid bin headroom
		//local parenti bin
		
		local intr: list kid & parenti
		local wcintr: word count `intr'
		local wcc `wcintr'
		
		while `wcintr' != 0 {
			local pos: list posof "`intr'" in kid
			local parentj: word `pos' of `parent'
			local intr: list kid & parentj
			local wcintr: word count `intr'
			local wcc = `wcc' + `wcintr'
		}
		
		
		local kidp = 1 + `wcc'
	}
	
	//dis "`var': `nt'"
		
		
	*variable levels		
	*****************
	
	local lb: var label `var'
	
	//levels actually present
	qui levelsof `var', local(lev)
	
	//all levels
	if "`alllevels'"!="" {
		local lbl: val label `var'
		if "`lbl'" !="" {
			preserve
			uselabel `lbl', clear
			qui levelsof value, local(lev)
			restore
		}
	}
	local lc: word count `lev'
	
	
	*variable type	
	********************

	local stopt
	local typei
	if "`type'"!="" {
	
		local wc: word count `type'
		forvalues i=1/`wc' {
			local wi: word `i' of `type'
			if "`wi'"=="`var'" {
				local i1=`i'+1
				local typei: word `i1' of `type'
			}
		}
	}
	
	if "`categorical'"!="" {
	
		local cwc: word count `categorical'
		
		forvalues i = 1/`cwc'  {
		
			local wi: word `i' of `categorical' 
			if "`var'"=="`wi'" {
				local typei="cat"
			}
		}
	}
	

	if "`continuous'" != "" {
		
		local cwc: word count `continuous'
		
		forvalues i = 1/`cwc'  {
			
			local wi: word `i' of `continuous' 
			
			if "`var'"=="`wi'" {
			
				cap assert "`typei'"==""
				if _rc {
					dis as text "Variable `var' mentioned under categorical and continuous, default option used."
					local typei
					local stopt="yes"
				}
				else {
					local typei="conti"
				}
			}
		}
	}
	
	if "`count'"!="" & "`stopt'" != "yes"  {
		
		if strpos("`count'","`var'")>0 {
		
			cap assert "`typei'"=="" 
			if _rc {
				dis as text "Variable `var' mentioned under count and categorical or continuous, count option ignored."
			}
			else {
				local pcount = 0
				local wcs: word count `count'
				forvalues j = 1/`wcs' {
					if word("`count'", `j') == "`var'" {
						local pcount = `j'
						continue, break
					}
				}
				if `pcount' != 0 {
				//local timevar: word `pcount' of `exp_time'	
					local typei="count"
				}
			}
		}	
	}

	if "`tte'"!="" & "`stopt'" != "yes"  {
	
		if strpos("`tte'","`var'")>0 {
			cap assert "`typei'"==""  
			if _rc {
				dis as text "Variable `var' mentioned under tte and categorical, continuous or count, tte option ignored."
			}
			else {
				local ttcount = 0
				local wcs: word count `tte'
				forvalues j = 1/`wcs' {
					if word("`tte'", `j') == "`var'" {
						local ttcount = `j'
						continue, break
					}
				}
				//local timevar: word `pcount' of `exp_time'	
				if `ttcount' != 0 {
					local typei="tte"
				}	
			}
		}
	}
	
	
	*defaults for continuous and categorical 
	if "`typei'"=="" & `lc' <=5 {
		local typei = "cat"
	}
	if "`typei'"=="" & `lc' >5 {
		local typei = "conti"
	}
	
	*user-defined effect
	*************************
	if "`user_var'" != "" {
		if `blockcheck'==1 {
			
			local add_user_var_post `add_user_var_post_base'
			
			if strpos("`user_var'","`var'")>0 | strpos("`user_var'","`typei'")>0 {	
				
				local add_user_var_post
				local add_user_var_post_bc `add_user_var_post_base'
				local nobc 0
				
				forvalues bc = 1/``bcount1'' {
					
					if  strpos(" `var`bc'' "," `var' ")>0 | strpos("`var`bc''","`typei'")>0 {
						
						local commande = subinstr("`command`bc''","`cr1'","||",.)
						local commande = subinstr("`commande'","variable","`var'",.)
						local commande = subinstr("`commande'","by","`by'",.)
						
						cap ereturn clear
						cap return clear
						qui `commande'
	
						if "`e(sample)'"!="" {
							tempvar used
							qui gen `used' = e(sample)
							cap assert `used' == `touse3'
							if _rc {
								dis as text "Estimation sample from user_command does not correspond to main sample."
								dis as text "If or in may be missing in user_command."
							}
						}
						
						if "`user_post'"=="" {
							local checkpost 1
							
							cap matrix list e(b)
							if _rc {
								local checkpost 0 
							}
							cap matrix list e(V)
							if _rc {
								local checkpost 0 
							}
							
							if `checkpost'==0 {
								dis as text "User-defined variables cannot be obtained, as e(b) and/or e(V) is missing."
								dis as text "Use option user_post to specify results from user-defined command."
							}
							else {
								
								if "`by'"=="" | inlist("`gc'","1","2") {
								
									tempname B V B1 V1 Bi
									matrix `B' = e(b)
									matrix `V' = e(V)
									local df
									local df =  e(df_r)

									local cnl: colfullnames `B'
									
									local intterm 0
									local byn
									foreach cn of local cnl {
										if strpos("`cn'","`by'")>0 &  strpos("`cn'","#")==0 {
											matrix `Bi' = `B'[1,"`cn'"]
											if `Bi'[1,1]!=0 {
												local byn `cn'
											}
										}
										
										if  strpos("`cn'","#")>0 {
											local intterm 1
										}
									}
									
									if `intterm' == 1 {
										
										if "`margins'" != "" {
											dis as text "Marginal effects reported from user-defined command."
											qui margins `by', pwcompare post
											matrix `B' = e(b_vs)
											matrix `V' = e(V_vs)
											local byn: colfullnames `B'
										}
										else {
											dis as text "Interaction term in user_command, only the main effect is reported."
											dis as text "Specify options margins for marginal effects."
										}
									}	
									
									if `intterm' == 0 & "`margins'"!="" {
										dis as text "No interaction term detected, margins my not have an effect."
									}
									
									matrix `B1' = `B'[1,"`byn'"]
									matrix `V1' = `V'["`byn'","`byn'"]
									
									local beta_t = `B1'[1,1]/sqrt(`V1'[1,1])
									
									local ttlev=1-(1-`level'/100)/2
									
									if !inlist("`df'","",".") {
										local qtl = invt(`df',`ttlev')
										local beta4 = 2*(1-t(`df',abs(`beta_t')))
									}
									else {
										local qtl = invnormal(`ttlev')
										local beta4 = 2*(1-normal(abs(`beta_t')))
									}
									
									local beta1  = `B1'[1,1]
									local beta2 = `B1'[1,1] - `qtl' * sqrt(`V1'[1,1])
									local beta3 = `B1'[1,1] + `qtl' * sqrt(`V1'[1,1])
									
									//local add_user_var_post `add_user_var_post' (`beta1') (`beta2') (`beta3') (`beta4')	
									
									if "`user_name'"=="" {
										forvalues j=1/4 {
											local k = (`bc'-1)*4 + `j'
											repl_list, base(`add_user_var_post_bc') posvar(`k') repl((`beta`j''))
											local add_user_var_post_bc `rlist'
										}								
									}
									else {
										forvalues j=1/4 {
											if ``bcount4''==1 {
												local wj: word `j' of `user_name'
											}
											else {
												local wj: word `j' of `name`bc''
											}
											local posvar : list posof "`wj'" in add_user_var_un
											
											//replace non-empty element:
											local chrep: word `posvar' of `add_user_var_post_bc'
											if "`chrep'"!="(.)" & "`errm'"!="1" {
												local errm 1
												dis as text ///
													"Variable `var' in two user-defined commands with same name---will be overwritten."
											}

											//program to replace element of a local list
											repl_list, base(`add_user_var_post_bc') posvar(`posvar') repl((`beta`j''))
											
											local add_user_var_post_bc `rlist'
										}
									}
								}
								else {
									qui testparm i.`by'
									local p_user = `r(p)'
									if "`user_name'"=="" {
										local k = (`bc'-1)*4 + 4
										repl_list, base(`add_user_var_post_bc') posvar(`k') repl((`p_user'))
										local add_user_var_post_bc `rlist'
									}
									else {
										repl_list, base(`add_user_var_post_bc') posvar(`bc') repl((`p_user'))
										local add_user_var_post_bc `rlist'
									}
								
								}
							}
						}
						else {
							if ``bcount3''==1 & ``bcount1''==1 {
								local nobc 1
								local uwc: word count `user_post'
								qui tokenize `user_post'
								forvalues j=1/`uwc' {
									local beta`j' = ``j''
									local add_user_var_post `add_user_var_post' (`beta`j'')
								}	
							} 
							else {
								if ``bcount3''==1 {
									local uwc: word count `user_post'
									qui tokenize `user_post'
								} 
								else {
									local uwc: word count `post`bc''
									qui tokenize `post`bc''
								}
								
								//local add_user_var_post `add_user_var_post_base'
								local errm 
								forvalues j=1/`uwc' {
									
									local beta`j' = ``j''

									if "`user_name'"=="" {
										if "``bcount4''"=="1" & "``bcount1''"=="1" {
											local wj user`j'
										} 
										else {
											local wj user`bc'`j'
										}
									}
									else {
										if ``bcount4''==1 {
											local wj: word `j' of `user_name'
										}
										else {
											local wj: word `j' of `name`bc''
										}
									}
									
									//dis "``bcount4''"
									//dis "`wj'"
									//dis "`add_user_var_un'"
									local posvar : list posof "`wj'" in add_user_var_un
									
									//replace non-empty element:
									local chrep: word `posvar' of `add_user_var_post_bc'
									if "`chrep'"!="(.)" & "`errm'"!="1" {
										local errm 1
										dis as text ///
											"Variable `var' in two user-defined commands with same name---will be overwritten."
									}

									//program to replace element of a local list
									repl_list, base(`add_user_var_post_bc') posvar(`posvar') repl((`beta`j''))
									
									local add_user_var_post_bc `rlist'

								}
							}	
						}	
					}
				}
				if `nobc'==0 {
				local add_user_var_post `add_user_var_post_bc'
				}
				
			}
		}	
	}
	
	
	*categorical variables:
	****************************
	
	if ("`typei'"=="cat")   {
		
		*over all levels:
		
		qui count if `touse2'
		local nl=r(N)
		
		if "`denominator'"!="" {
			local pri=`nl'/`nnm'
			cap cii proportions `nnm' `nl', level(`level') `prci'
			if _rc {
				local prilci = .
				local priuci = .
			}
			else {
				local prilci = `r(lb)'
				local priuci = `r(ub)'
			}	
		}
		else {
			local pri=`nl'/`nt'
			cap cii proportions `nt' `nl', level(`level') `prci'
			if _rc {
				local prilci = .
				local priuci = .
			}
			else {
				local prilci = `r(lb)'
				local priuci = `r(ub)'
			}	
		}
		
		if "`by'"=="" {
			post `res' ("`var'") ("`lb'") ("cat1") (.) ("") (`lc') `etime_post' `stime_post' ///
					 (`nt') (`nnm') (`nl') ///
					 (`pri') (.) (.) ///
					 (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `add_user_var_post' ///
					 (`kidp')
		}
		else {
		
			//fisher sensible?
			if (`lc'>`nclimfish' | `nt'>`nlimfish') & "`fisher'"=="" &  strpos("`testlist2'","fisher")>0 {
				local outp=cond(`lc'>5,"as there are many levels","as the sample size is larger than `nlimfish'")
				dis as text "Fisher's exact test for `var' is omitted `outp'."
				dis as text "Use test(fisher) to use the test anyway, might be slow."
				local testlist2=subinstr("`testlist2'","fisher","",.)
			}
		
			if strpos("`testlist2'","fisher")>0 {
				qui cap tab `var' `by' if `touse2', exact
				local p_fi=r(p_exact)
	
				if "`denominator'"=="" {
					cap assert !missing(`var') if `touse2' & !missing(`by')
					if _rc {
						dis as text "Missings in `var' are ignored for overall Fisher's exact test"
					}
				}	
				
			} 
			else {
				local p_fi=.
			}
			if strpos("`testlist2'","chi2")>0 {
			
				if "`chi2_lr'"=="" &  strpos("`lr'","chi2")==0 {
					qui cap tab `var' `by' if `touse2', chi2
					local p_chi2=r(p)
				}
				else {
					qui cap tab `var' `by' if `touse2', lrchi2
					local p_chi2=r(p_lr)
					//if conti correction: exactcc
				}
				
				if "`denominator'"=="" {
					cap assert !missing(`var') if `touse2' & !missing(`by')
					if _rc {
						dis as text "Missings in `var' are ignored for overall chi-squared test"
					}
				}
			}
			else {
				local p_chi2=.
			}
			
			local fp
			foreach g of local glev {
				qui count if `by'==`g' & `touse2'
					local nl_`g'=r(N)	
					
					if "`denominator'"!="" {
						local pri_`g'=`nl_`g''/`nnm_`g''
						cap cii proportions `nnm_`g'' `nl_`g'', level(`level') `prci'
						if _rc {
							local prilci_`g' = .
							local priuci_`g' = .
						}
						else {
							local prilci_`g' = `r(lb)'
							local priuci_`g' = `r(ub)'
						}
					}
					else {
						local pri_`g'=`nl_`g''/`nt_`g''
						cap cii proportions `nt_`g'' `nl_`g'', level(`level') `prci'
						if _rc {
							local prilci_`g' = .
							local priuci_`g' = .
						}
						else {
							local prilci_`g' = `r(lb)'
							local priuci_`g' = `r(ub)'
						}	
					}
					
					local fp `fp' (`nt_`g'') (`nnm_`g'') (`nl_`g'') ///
						(`pri_`g'') (`prilci_`g'') (`priuci_`g'') ///
						(.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
			}
			
			if `gc'==1 {
				post `res' ("`var'") ("`lb'") ("cat1") (.) ("") (`lc') `etime_post' `stime_post'  ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (.)  (.)  ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 `add_user_var_post' ///
						 (`kidp')
			}
			if `gc'==2 {
				post `res' ("`var'") ("`lb'") ("cat1") (.) ("") (`lc') `etime_post' `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (.)  (.)  ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 (`p_chi2') (`p_fi') ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.)  ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 `add_user_var_post' ///
						 (`kidp')
						  
			}
			if `gc'>2  {
				post `res' ("`var'") ("`lb'") ("cat1") (.) ("") (`lc') `etime_post' `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (.)  (.)  ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 (`p_chi2') (`p_fi') ///
						 (.) (.) ///
						 (.) (.) ///
						 (.) (.) ///
						 `add_user_var_post' ///
						 (`kidp')

			}
		}
		
		
		* foreach level:
		
		local i=0	
		foreach l of local lev {
			local i=`i'+1
			local lbl: label (`var') `l'
	
			qui count if `var'==`l' & `touse2'
			local nl=r(N)
			
			if "`denominator'"!="" {
				local pri=`nl'/`nnm'
				cap cii proportions `nnm' `nl', level(`level') `prci'
				if _rc {
					local prilci = .
					local priuci = .
				}
				else {
					local prilci = `r(lb)'
					local priuci = `r(ub)'
				}
			}
			else {
				local pri=`nl'/`nt'
				cap cii proportions `nt' `nl', level(`level') `prci'
				if _rc {
					local prilci = .
					local priuci = .
				}
				else {
					local prilci = `r(lb)'
					local priuci = `r(ub)'
				}	
				
			}
		
			if "`by'"=="" {
				post `res' ("`var'") ("`lb'") ("cat") (`l') ("`lbl'") (`lc') `etime_post'  `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (`prilci')  (`priuci')  ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `add_user_var_post' ///
						 (`kidp')

			} 
			else {
				local fp
				foreach g of local glev {
					qui count if `var'==`l' & `by'==`g' & `touse2'
					local nl_`g'=r(N)	
					
					if "`denominator'"!="" {
						local pri_`g'=`nl_`g''/`nnm_`g''
						cap cii proportions `nnm_`g'' `nl_`g'', level(`level') `prci'
						if _rc {
							local prilci_`g' = .
							local priuci_`g' = .
						}
						else {
							local prilci_`g' = `r(lb)'
							local priuci_`g' = `r(ub)'
						}
					}
					else {
						local pri_`g'=`nl_`g''/`nt_`g''
						cap cii proportions `nt_`g'' `nl_`g'', level(`level') `prci'
						if _rc {
							local prilci_`g' = .
							local priuci_`g' = .
						}
						else {
							local prilci_`g' = `r(lb)'
							local priuci_`g' = `r(ub)'
						}
					}
					
					local fp `fp' (`nt_`g'') (`nnm_`g'') (`nl_`g'') ///
						(`pri_`g'') (`prilci_`g'') (`priuci_`g'') ///
						(.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) ///
						(.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
				}
				
				if `gc'==1  {
					post `res' ("`var'") ("`lb'") ("cat") (`l') ("`lbl'") (`lc') `etime_post'  `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (`prilci') (`priuci') ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 `add_user_var_post' ///
						 (`kidp')
				}
				if `gc'==2 {
									
					local g1: word 1 of `glev'
					local g2: word 2 of `glev'
					tempvar grrc
					qui recode `by' (`g1'=1) (`g2'=0), gen(`grrc')
					
					tempvar vrc
							
					if "`denominator'"!="" {
						qui recode `var' (`l'=1) (nonmissing=0) , gen(`vrc')	
					}
					else {
						qui recode `var' (`l'=1) (nonmissing=0) (missing=0), gen(`vrc')
					}
					
					if strpos("`testlist2'","chi2")>0 {
					
						if "`chi2_lr'"=="" & strpos("`lr'","chi2")==0 {
							qui cap tab `vrc' `grrc' if `touse3', chi2
							local p_chi2=r(p)	
						}
						else {
							qui cap tab `vrc' `grrc' if `touse3', lrchi2
							local p_chi2=r(p_lr)
						}
							
					}
					else {
						local p_chi2=.
					}
					
					if strpos("`testlist2'","fisher")>0 {
						qui cap tab `vrc' `grrc' if `touse3', exact
						local p_fi=r(p_exact)
					}
					else {
						local p_fi=.
					}
					
					if strpos("`effectlist'","rd")==0 & strpos("`effectlist'","rr")==0 ///
						& strpos("`effectlist'","or")==0 {
						local rd=.
						local rd_lci=.
						local rd_uci=.
						local rr=.
						local rr_lci=.
						local rr_uci=.
						local or=.
						local or_lci=.
						local or_uci=.	
					}
					else {
						qui cap cs `vrc' `grrc' if `touse3', or woolf level(`level')
						local rd=r(rd)
						local rd_lci=r(lb_rd)
						local rd_uci=r(ub_rd)
						local rr=r(rr)
						local rr_lci=r(lb_rr)
						local rr_uci=r(ub_rr)
						local or=r(or)
						local or_lci=r(lb_or)
						local or_uci=r(ub_or)
						
						if strpos("`effectlist'","rd")==0 {
							local rd=.
							local rd_lci=.
							local rd_uci=.
						}
						if strpos("`effectlist'","rr")==0 {
							local rr=.
							local rr_lci=.
							local rr_uci=.
						}
						if strpos("`effectlist'","or")==0 {
							local or=.
							local or_lci=.
							local or_uci=.	
						}
					}
					
					
					*alternative confidence intervals for rd, rr and or
					
					//rd
					if "`rdci'"!="" & "`rdci'"!="wald" &  strpos("`effectlist'","rd")>0  {
					
						qui rdci `vrc' `grrc' if `touse3', level(`level') 
						
						if ("`rdci'"=="ac") {
							local rd_lci=r(lb_ac)
							local rd_uci=r(ub_ac)
						}
						if ("`rdci'"=="nhs") {
							local rd_lci=r(lb_ne)
							local rd_uci=r(ub_ne)
						}
						if ("`rdci'"=="wall") {
							local rd_lci=r(lb_wa)
							local rd_uci=r(ub_wa)
						}
						if ("`rdci'"=="mn") {
							local rd_lci=r(lb_mn)
							local rd_uci=r(ub_mn)
						}
						
					}
					
					//rr
					if "`rrci'"!="" & "`rrci'"!="katz" &  strpos("`effectlist'","rr")>0  {
					
						if ("`rrci'"=="koopman") {
							tempvar grrcrc
							qui recode `grrc' (0=1) (1=0), gen(`grrcrc')
							preserve
							keep  `vrc' `grrcrc' `touse3'
							qui koopman `vrc' `grrcrc' if `touse3', level(`level')
							restore
							local rr_lci=r(theta_l)
							local rr_uci=r(theta_u)
						}
						
					}
					
					//or
					if "`orci'"!="" & "`orci'"!="woolf" &  strpos("`effectlist'","or")>0  {
					
						if "`orci'"=="exact" {
							qui cap cc `vrc' `grrc' if `touse3', level(`level')
							if _rc {
								dis as text "Error using exact confidence intervals for odds ratio for `var':`lbl'"
							}
							local or_lci=r(lb_or)
							local or_uci=r(ub_or)
						}
						if "`orci'"=="cornfield" {
							qui cap cs `vrc' `grrc' if `touse3', or level(`level')
							if _rc {
								dis as text "Error using cornfield confidence intervals for odds ratio for `var':`lbl'"
							}
							local or_lci=r(lb_or)
							local or_uci=r(ub_or)
						}
						if "`orci'"=="gart" {
							qui cap oddsrci if `touse3', c1(`vrc') c2(`grrc') gart level(`level')
							if _rc {
								dis as text "Error using gart confidence intervals for odds ratio for `var':`lbl'"
							}
							local or_lci=r(lb_Gart)
							local or_uci=r(ub_Gart)	
						}
						if "`orci'"=="agresti" {
							qui cap oddsrci if `touse3', c1(`vrc') c2(`grrc')  agresti level(`level')
							if _rc {
								dis as text "Error using agresti confidence intervals for odds ratio for `var':`lbl'"
							}
							local or_lci=r(lb_Agres)
							local or_uci=r(ub_Agres)
						}
					}
						
				post `res' ("`var'") ("`lb'") ("cat") (`l') ("`lbl'") (`lc') `etime_post'  `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri')  (`prilci')  (`priuci') ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 (`p_chi2') (`p_fi') ///
						 (`rd') (`rd_lci') (`rd_uci') ///
						 (`rr') (`rr_lci') (`rr_uci') ///
						 (`or') (`or_lci') (`or_uci') ///
						 (.) (.) (.) ///
						 (.) (.) (.)  ///
						 (.) (.) (.)  ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 (.) (.) (.) ///
						 `add_user_var_post' ///
						 (`kidp')
				}

				if `gc'>2 {
				
					tempvar vrc
					
					if "`denominator'"!="" {
						qui recode `var' (`l'=1) (nonmissing=0) , gen(`vrc')	
					}
					else {
						qui recode `var' (`l'=1) (nonmissing=0) (missing=0), gen(`vrc')
					}
				
					if strpos("`testlist2'","chi2")>0 {
						
						if "`chi2_lr'"=="" & strpos("`lr'","chi2")==0 {
							qui cap tab `vrc' `by' if `touse3', chi2
							local p_chi2=r(p)	
						}
						else {
							qui cap tab `vrc' `by' if `touse3', lrchi2
							local p_chi2=r(p_lr)
						}
							
					}
					else {
						local p_chi2=.
					}
					
					if strpos("`testlist2'","fisher")>0 {
						
						qui cap tab `vrc' `by' if `touse3', exact
						local p_fi=r(p_exact)
					}
					else {
						local p_fi=.
					}
				
				
				
					post `res' ("`var'") ("`lb'") ("cat") (`l') ("`lbl'") (`lc') `etime_post'  `stime_post' ///
						 (`nt') (`nnm') (`nl') ///
						 (`pri') (`prilci') (`priuci') ///
						 (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) /// 
						 (.) (.) (.) (.) (.) ///
						 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
						 `fp' ///
						 (`p_chi2') (`p_fi') ///
						 (.) (.) ///
						 (.) (.) ///
						 (.) (.) ///
						 `add_user_var_post' ///
						 (`kidp')
				}
			}
		}
	}   

	
	*continuous variables:	
	******************************
	
	if ("`typei'"=="conti")   { 
	
		qui sum `var' if `touse3', d
		local mean  = r(mean)
		local sd    = r(sd)
		local p50   = r(p50)
		local p25   = r(p25)
		local p75   = r(p75)
		local iqr   = r(p75)-r(p25)
		local min   = r(min)
		local max   = r(max)
		local range = r(max) - r(min)
		local sum = r(sum)
		
		qui ci mean `var' if `touse3', level(`level')
		local meanlci = r(lb)
        local meanuci = r(ub)

		if "`by'"=="" {
				post `res' ("`var'") ("`lb'") ("conti") (.) ("") (.) `etime_post'  `stime_post' ///
				 (`nt') (`nnm') (.) ///
				 (.) (.) (.) ///
				 (`mean') (`sd') (`meanlci') (`meanuci') ///
				 (`p50') (`p25') (`p75') (`iqr') (`min') (`max') (`range') (`sum') ///
				 (.) (.) (.) (.) (.) ///
				 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
				 `add_user_var_post' ///
				 (`kidp')
		} 
		else {
			
			local fp		
			
			foreach g of local glev {
			
				qui sum `var' if `by'==`g' & `touse3',d
				local mean_`g' = r(mean)
				local sd_`g'   = r(sd)
				local p50_`g'  = r(p50)
				local p25_`g'  = r(p25)
				local p75_`g'  = r(p75)
				local iqr_`g'  = r(p75)-r(p25)
				local min_`g'  = r(min)
				local max_`g'  = r(max)
				local range_`g'  = r(max)-r(min)
				local sum_`g'  = r(sum)
				
				qui ci mean `var' if `by'==`g' & `touse3', level(`level')
				local meanlci_`g' = r(lb)
				local meanuci_`g' = r(ub)
		
				local fp `fp' (`nt_`g'') (`nnm_`g'') (.) ///
					(.) (.) (.) ///
					(`mean_`g'') (`sd_`g'')  (`meanlci_`g'') (`meanuci_`g'') ///
					(`p50_`g'') (`p25_`g'') (`p75_`g'') (`iqr_`g'') (`min_`g'') (`max_`g'') (`range_`g'') (`sum_`g'') ///
					(.) (.) (.) (.) (.) ///
					(.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
				}
			
			if `gc'==1 {
			
				post `res' ("`var'") ("`lb'") ("conti") (.) ("") (.) `etime_post'  `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (`mean') (`sd') (`meanlci') (`meanuci') ///
					 (`p50') (`p25') (`p75') (`iqr') (`min') (`max') (`range') (`sum') ///
					 (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `fp' ///
					 `add_user_var_post' ///
					 (`kidp')
			}
			
			if `gc'==2 {
			
				//local by random_grp
				//levelsof `by', local(glev)
				//local var age
				local w1: word 1 of `glev'
				local w2: word 2 of `glev'
				tempvar grrc
				qui recode `by' (`w1'=0) (`w2'=1), gen(`grrc')
				tempvar grrc_rev
				qui recode `by' (`w1'=1) (`w2'=0), gen(`grrc_rev')
					
				//non zero entries
				qui sum `var' if `grrc'==0 & `touse3'
				local n0t=r(N)
				qui sum `var' if `grrc'==1 & `touse3'
				local n1t=r(N)
				
				if `n0t'!=0 & `n1t'!=0  { 
						
					//ttest	
					if strpos("`testlist2'","ttest")==0 & strpos("`effectlist'","meand")==0  {
						local par_diff=.
						local par_diff_lci=.
						local par_diff_uci=.
						local par_p=.
					} 
					else {
						qui ttest `var' if `touse3', by(`grrc') `ttest' level(`level')
						local par_diff= r(mu_1)-r(mu_2)
						local ttlev=1-(1-`level'/100)/2
						local par_diff_lci=`par_diff' - invt(r(df_t),`ttlev')* r(se)
						local par_diff_uci=`par_diff' + invt(r(df_t),`ttlev')* r(se)
						local par_p=r(p)

						if strpos("`testlist2'","ttest")==0 {
							local par_p=.
						}
						if strpos("`effectlist'","meand")==0 {
							local par_diff=.
							local par_diff_lci=.
							local par_diff_uci=.		
						}
					}
					
					//Wilcoxon rank-sum test
					if strpos("`testlist2'","ranksum")>0  {		

						if "`ranksum_exact'" == "" {
							qui ranksum `var' if `touse3', by(`grrc')
							cap local npar_p=2*(1-normal(abs(`r(z)')))
						}
						else {
							cap ranksum `var' if `touse3', by(`grrc') exact
							if _rc {
								//for Stata 15, exact option is not available
								dis as text "Exact Wilcoxon rank-sum test not available --- asymptotic version used"
								qui ranksum `var' if `touse3', by(`grrc')
								cap local npar_p=2*(1-normal(abs(`r(z)')))
							}
							else {
								if "`r(p_exact)'"=="" {
									cap local npar_p=2*(1-normal(abs(`r(z)'))) 
								}
								else {
									local npar_p=`r(p_exact)'
								}
							}
						}
					}
					else {
						local npar_p=.
					}
					
					//quantile regression for median 
					//median difference between groups	
					if strpos("`testlist2'","qreg")==0 & strpos("`effectlist'","medd")==0  {
						local qreg_diff=.
                          local qreg_diff_lci=.
                          local qreg_diff_uci=.
                          local qreg_p=.
					}
					else {
						cap qreg `var' ib1.`grrc' if `touse3', quantile(.5) level(`level') `qreg'
						if _rc {
							dis as text "Error in qreg for `var'"
						}
						tempname A
						matrix `A'=r(table)
						local qreg_diff=`A'[1,1]
						local qreg_diff_lci=`A'[5,1]
						local qreg_diff_uci=`A'[6,1]
						local qreg_p=`A'[4,1]
						
						if strpos("`testlist2'","qreg")==0  {
							local qreg_p=.
						}	
						if strpos("`effectlist'","medd")==0  {
							local qreg_diff=.
							local qreg_diff_lci=.
							local qreg_diff_uci=.
						}	
					}
					
					//Hodges-Lehmann median differences, possible effect size for Wilcoxon rank-sum test
					//median difference between individuals
					if strpos("`effectlist'","hlmd")>0 {
						if ("`hlmdci'"=="lehmann") {
							cap npshift `var' if `touse3', by(`grrc_rev') level(`level')
							local cend_diff= r(theta)
							local cend_lci= r(theta_l)
							local cend_uci= r(theta_u)		
						}
						else {
							cap cendif `var' if `touse3', by(`grrc') level(`level')
							if _rc {
								dis as text "Error in cendif for `var'"
							}
							matrix B=r(cimat)
							local cend_diff=B[1,2]
							local cend_lci=B[1,3]
							local cend_uci=B[1,4]
						}
					}
					else {
						local cend_diff=.
						local cend_lci=.
						local cend_uci=.
					}
					
					//Mann-Withney statistic, probabilistic index, Harrell's C, mws
					//probability of an observation in group 0 having a true value that is higher than an observation in group 1
					if strpos("`effectlist'","mws")>0  {
					
						if "`mwsci'"=="fay"  | "`mwsci'"==""  { 
							wmwTest `var' if `touse3', by(`grrc_rev') 
							local mws=`r(estimate)' 	
							local mws_lci=`r(lci)'
							local mws_uci=`r(uci)'
						}
						if "`mwsci'"=="jackknife"   { 
							qui somersd `grrc_rev' `var' if `touse3', transf(c) tdist level(`level')
							//c specifies Harrell's c; tdist: t instead of normal
							matrix C=r(table)
							local mws=C[1,1]
							local mws_lci=C[5,1]
							local mws_uci=C[6,1]
						}
						if "`mwsci'"=="delong" {
							qui roctab `grrc_rev' `var' if `touse3', level(`level')
							local mws=r(area)
							local mws_lci=r(lb)
							local mws_uci=r(ub)
						}
						if ("`mwsci'"=="bamber") {
							qui roctab `grrc_rev' `var' if `touse3', bamber level(`level')
							local mws=r(area)
							local mws_lci=r(lb)
							local mws_uci=r(ub)
						}
						if ("`mwsci'"=="hanley") {
							qui roctab `grrc_rev' `var' if `touse3', hanley level(`level')
							local mws=r(area)
							local mws_lci=r(lb)
							local mws_uci=r(ub)
						}
						
					}
					else {
						local mws=.
						local mws_lci=.
						local mws_uci=.
					}
					
					local fd  (`par_p') (`npar_p') (`qreg_p') ///
							(`par_diff') (`par_diff_lci') (`par_diff_uci')  ///
							(`qreg_diff') (`qreg_diff_lci') (`qreg_diff_uci') ///
							(`cend_diff') (`cend_lci') (`cend_uci') ///
							(`mws') (`mws_lci') (`mws_uci')
				}
				else {
					local fd (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
				}
				post `res' ("`var'") ("`lb'") ("conti") (.) ("") (.) `etime_post'  `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (`mean') (`sd') (`meanlci') (`meanuci') ///
					 (`p50') (`p25') (`p75') (`iqr') (`min') (`max') (`range') (`sum') ///
					 (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `fp' ///
					 (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 `fd' ///
					 (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					`add_user_var_post'	///
					(`kidp')					
			}
			
			if `gc'>2 {
			
				if strpos("`testlist2'","anova")>0  {
					qui cap anova `var' `by' if `touse3'
					if _rc {
						dis as text "Error in anova for `var'"
					}
					cap local par_p=1-F(`e(df_1)',`e(df_r)',`e(F_1)') 
					if _rc {
						local par_p=.
					}
				}
				else {
					local par_p=.
				}
				
				if strpos("`testlist2'","kwallis")>0  {
					qui cap kwallis `var' if `touse3', by(`by')
					if _rc {
						dis as text "Error in kwallis for `var'"
					}
					cap local npar_p= 1-chi2(r(df),r(chi2_adj))
					if _rc {
						local npar_p=.
					}
				}
				else {
					local npar_p=.
				}
				
				local fd  (`par_p') (`npar_p')
			
			
			post `res' ("`var'") ("`lb'") ("conti") (.) ("") (.) `etime_post'  `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (`mean') (`sd') (`meanlci') (`meanuci') ///
					 (`p50') (`p25') (`p75') (`iqr') (`min') (`max') (`range') (`sum') ///
					 (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `fp' ///
					 (.) (.) ///
					 `fd' ///
					 (.) (.) ///
					 (.) (.) ///
					 `add_user_var_post' ///
					 (`kidp')
			}				
		}	 
	}
	
	
	*count variables:
	**********************
	
	if ("`typei'"=="count") {
		
		//dis "`var' `timevar'"
		
		local unit time
		local timevar: word `pcount' of `exp_time'	
		
		if "`timevar'"=="" {
			assert `twc'==1 | `twc'==0
			local timevar: word 1 of `exp_time'
		}
		
		if "`exp_time_units'"!="" {
			local ewc: word count `exp_time_units'
			if `ewc'==1 {
				local unit `exp_time_units'
				local etime_post ("`unit'")
			}
			else {
				local unit: word `pcount' of `exp_time_units'
				local etime_post ("`unit'")
			}	
		}
		else {
			local etime_post
		}
	
		qui sum `var' if `touse3', d
		local nevents  = r(sum)
		
		qui sum `timevar' if `touse3', d
		local etime  = r(sum)
		
		qui ci means `var' if `touse3', poisson exposure(`timevar')
		local ir = r(mean)
		local irlci = r(lb)
		local iruci = r(ub)
		
		if "`by'"=="" {
				post `res' ("`var'") ("`lb'") ("count") (.) ("") (.) `etime_post' `stime_post' ///
				 (`nt') (`nnm') (.) ///
				 (.) (.) (.) ///
				 (.) (.) (.) (.)  ///
				 (.) (.) (.) (.) (.) (.) (.) (.) ///
				 (`nevents') (`etime') (`ir') (`irlci') (`iruci') ///
				 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
				 `add_user_var_post' ///
				 (`kidp')
				 
		} 
		else {
			
			local fp		
			foreach g of local glev {
			
				qui sum `var' if `by'==`g' &  `touse3',d
				local nevents_`g' = r(sum)
				
				qui sum `timevar' if `by'==`g' & `touse3', d
				local etime_`g'  = r(sum)
				
				qui ci means `var' if `by'==`g' &`touse3', poisson exposure(`timevar')
				local ir_`g' = r(mean)
				local irlci_`g' = r(lb)
				local iruci_`g' = r(ub)
				
				local fp `fp' (`nt_`g'') (`nnm_`g'') (.) ///
					(.) (.) (.) ///
					(.) (.) (.) (.)  ///
					(.) (.) (.) (.) (.) (.) (.) (.) ///
					(`nevents_`g'') (`etime_`g'') (`ir_`g'')  (`irlci_`g'') (`iruci_`g'') ///
					(.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
				}
			
			if `gc'==1 {
				post `res' ("`var'") ("`lb'") ("count") (.) ("") (.) `etime_post' `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) (.)  ///
					 (.) (.) (.) (.) (.) (.) (.) (.) ///
					  (`nevents') (`etime') (`ir') (`irlci') (`iruci') ///
					  (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `fp' ///
					 `add_user_var_post' ///
					 (`kidp')
			}
			
			if `gc'==2 {
			
				//local by random_grp
				//levelsof `by', local(glev)
				//local var age
				local g1: word 1 of `glev'
				local g2: word 2 of `glev'
				tempvar grrc
				qui recode `by' (`g1'=1) (`g2'=0), gen(`grrc')
					
					
				//no test and effects
				if strpos("`testlist2'","poisson")==0  ///
					& strpos("`effectlist'","irr")==0 & strpos("`effectlist'","ird")==0 {
				
					local irr=.
					local irr_lci=.
					local irr_uci=.
				
					local ird=.
					local ird_lci=.
					local ird_uci=.
					
					local p_poi=.
					local p_nbreg=.
				}
				
				else {
					
					qui cap ir `var' `grrc' `timevar'	
					local irr=r(irr)
					local irr_lci=r(lb_irr)
					local irr_uci=r(ub_irr)
					
					local ird=r(ird)
					local ird_lci=r(lb_ird)
					local ird_uci=r(ub_ird)
					
					//stata 16:
					if "`r(p_twosided_midp)'"!="" {
						local p_poi = r(p_twosided_midp)
					}
					else {
						local p_poi = r(p)*2
					}
					
					local p_nbreg = .
					
					if strpos("`effectlist'","ird")==0 {
						local ird=.
						local ird_lci=.
						local ird_uci=.
					}
					
					if strpos("`effectlist'","irr")==0 {
						local irr=.
						local irr_lci=.
						local irr_uci=.
					}
					
					if strpos("`testlist2'","poisson")==0 {
							local p_poi=.
					}	
				
										
					
					*alternative confidence intervals for irr
					local p_poi_oim
					local p_poi_robust
					
					if "`irrci'"!="" & "`irrci'"!="exact" &  strpos("`effectlist'","irr")>0  {
						if ("`irrci'"=="oim") {
							cap poisson `var' `grrc' if `touse3', level(`level') exposure(`timevar') vce(oim)
							if _rc {
								dis as text "irrci(oim) not estimable for `var'"
								local irr_lci = .
								local irr_uci = .
								local p_poi_oim = .
							}
							else {
								tempname A
								matrix `A' = r(table)
								local irr_lci = exp(`A'[5,1])
								local irr_uci = exp(`A'[6,1])
								local p_poi_oim = `A'[4,1]
								if "`poisson_lr'" != "" | strpos("`lr'","poisson")>0 {
									local p_poi_oim = 1-chi2(`e(df_m)',`e(chi2)')
								}
							}	
						}
						if ("`irrci'"=="robust") {
							cap poisson `var' `grrc' if `touse3', level(`level') exposure(`timevar') vce(robust)
							if _rc {
								dis as text "irrci(robust) not estimable for `var'"
								local irr_lci = .
								local irr_uci = .
								local p_poi_robust = .
							}
							else {
								tempname A
								matrix `A' = r(table)
								local irr_lci = exp(`A'[5,1])
								local irr_uci = exp(`A'[6,1])
								local p_poi_robust  = `A'[4,1]
							}
						}
					}	
				
					*alternative Poisson p-values:
					if "`poisson_test'"!="" & "`poisson_test'"!="midp" & strpos("`testlist2'","poisson")>0 {				
						if ("`poisson_test'"=="oim") {
							if "`p_poi_oim'"== "" {
								cap poisson `var' `grrc' if `touse3', level(`level') exposure(`timevar') vce(oim)
								if _rc {
									dis as text "poisson_test(oim) not estimable for `var'"
									local p_poi = .
								}
								else {
									tempname A
									matrix `A' = r(table)
									local p_poi = `A'[4,1]
									if "`poisson_lr'" != "" | strpos("`lr'","poisson")>0 {
										local p_poi = 1-chi2(`e(df_m)',`e(chi2)')
									}
								}
							}	
							else {
								local p_poi = `p_poi_oim'
							}
						}
						if ("`poisson_test'"=="robust") {
							if  "`p_poi_robust'" == "" {
								cap poisson `var' `grrc' if `touse3', level(`level') exposure(`timevar') vce(robust)
								if _rc {
									dis as text "poisson_test(robust) not estimable for `var'"
									local p_poi = .
								}
								else {
									tempname A
									matrix `A' = r(table)
									local p_poi = `A'[4,1]
								}	
							} 
							else {
								local p_poi = `p_poi_robust'
							}	
						}
						if ("`poisson_test'"=="exact") {
							qui cap ir `var' `grrc' `timevar'
							if "`r(p_twosided_exact)'" != "" {
								local p_poi =  `r(p_twosided_exact)'
							}
							else {
								dis as text "Exact Poisson p-values not available --- default midp used."
								local p_poi = r(p)*2
							}
						}
					}	
					
					if "`nbreg'" != "" {
				
						cap nbreg `var' `grrc' if `touse3', level(`level') exposure(`timevar')
						if _rc {
							dis as text "nbreg not fitting, consider skipping nbreg option"
							local p_nbreg = .
							local irr = .
							local irr_lci = .
							local irr_uci = .
						}
						else {
							tempname A
							matrix `A' = r(table)
							local irr 	  = exp(`A'[1,1])
							local irr_lci = exp(`A'[5,1])
							local irr_uci = exp(`A'[6,1])
						
							if strpos("`testlist2'","nbreg") {
								local p_nbreg = `A'[4,1]
								if "`nbreg_lr'" != "" | strpos("`lr'","nbreg")>0 {
									local p_nbreg = 1-chi2(`e(df_m)',`e(chi2)')
								}
							}
						}	
					}
				}
				
				post `res' ("`var'") ("`lb'") ("count") (.) ("") (.) `etime_post' `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) (.)  ///
					 (.) (.) (.) (.) (.) (.) (.) (.) ///
					 (`nevents') (`etime') (`ir') (`irlci') (`iruci') ///
					 (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					 `fp' ///
					 (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.)  ///
					 (.) (.) (.)  ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (`p_poi') (`p_nbreg') ///
					 (`ird') (`ird_lci') (`ird_uci') ///	
					 (`irr') (`irr_lci') (`irr_uci') ///
					 (.) (.) (.)  ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 `add_user_var_post' ///
					 (`kidp')
				
				local post_nbreg (.)		
					 
			}
			
			if `gc'>2 {	
				
				local p_poi=.
				local p_nbreg=.
				
				*alternative Poisson p-values:
				if strpos("`testlist2'","poisson")!=0 {
				
					if "`poisson_test'"=="" | "`poisson_test'"=="exact" {
						dis as text "Exact Poisson test not available for by variables with more than two levels, oim used."
						local poisson_test oim
					}
					
					if ("`poisson_test'"=="oim") {
						qui poisson `var' i.`by' if `touse3', level(`level') exposure(`timevar') vce(oim)
						if "`poisson_lr'" != "" |  strpos("`lr'","poisson")>0 {
							local p_poi = 1-chi2(`e(df_m)',`e(chi2)')
						}
						else {
							qui testparm i.`by'
							local p_poi = `r(p)'
						}
					}
					if ("`poisson_test'"=="robust") {
						qui poisson `var' i.`by' if `touse3', level(`level') exposure(`timevar') vce(robust)
						if "`poisson_lr'" != "" |  strpos("`lr'","poisson")>0 {
							local p_poi = 1-chi2(`e(df_m)',`e(chi2)')
						}
						else {
							qui testparm i.`by'
							local p_poi = `r(p)'
						}
					}
				}	
				
				if "`nbreg'" != "" & strpos("`testlist2'","nbreg") {
				
					cap nbreg `var' i.`by' if `touse3', level(`level') exposure(`timevar')
					if _rc {
						dis as text "nbreg not fitting, consider skipping nbreg option"
						local p_nbreg = .
						local irr = .
						local irr_lci = .
						local irr_uci = .
					}
					else {
						if "`nbreg_lr'" != "" | strpos("`lr'","nbreg")>0 {
							local p_nbreg = 1-chi2(`e(df_m)',`e(chi2)')
						} 
						else {
							qui testparm i.`by'
							local p_nbreg = `r(p)'
						}	
					}	
				}
				
				post `res' ("`var'") ("`lb'") ("count") (.) ("") (.) `etime_post' `stime_post' ///
					(`nt') (`nnm') (.) ///
					(.) (.) (.) ///
					(.) (.) (.) (.)  ///
					(.) (.) (.) (.) (.) (.) (.) (.) ///
					(`nevents') (`etime') (`ir') (`irlci') (`iruci') ///
					(.) (.) (.) (.) (.) (.) (.) (.) (.) (.) ///
					`fp' ///
					(.) (.) ///
					(.) (.) ///
					(`p_poi') (`p_nbreg') ///
					(.) (.) ///
					`add_user_var_post' ///
					(`kidp')
			}				
		}	 
	}

	
	
	*time to event variable
	*************************
	
	if ("`typei'"=="tte") {
	
	preserve	

		local trunctime
		local failvar: word `ttcount' of `fail'	
		
		if "`trunc_time'" != "" {
			if `trtwc'==1 {
				local trunctime: word 1 of `trunc_time'
			}
			else {
				local trunctime: word `ttcount' of `trunc_time'
			}
		}
		else {
			local trunctime
		}
		
		if "`surv_time_units'"!="" {
			local swc: word count `surv_time_units'
			if `swc'==1 {
				local sunit `surv_time_units'
				local stime_post ("`sunit'")
			}
			else {
				local sunit: word `ttcount' of `surv_time_units'
				local stime_post ("`sunit'")
			}	
		}
		else {
			local stime_post
		}
		
		if "`rmst_model'" != "" {
			local rmwc: word count `rmst_model'
			if `rmwc'==1 {
				local rmst_model_i: word 1 of `rmst_model'
			}
			else {
				local rmst_model_i: word `ttcount' of `rmst_model'
			}
		}
		else {
			local rmst_model_i npar
		}
		
		local dfwc: word count `fpm_df'
		
		if "`rmst_model_i'" == "fpm" {
			local counter_fpm = `counter_fpm' + 1
			if `dfwc'==1 {
				local fpm_df_i = `fpm_df'
			}
			else {
				local fpm_df_i: word `counter_fpm' of `fpm_df'
			}
		}
		
		qui sum `failvar' if `touse3', d
		local nfails  = r(sum)
		
		qui sum `var' if `touse3', d
		local stime  = r(sum)
		
		qui stset `var' if `touse3', fail(`failvar')
		
		qui stci if `touse3'
		local st50 = `r(p50)'
		local st50lci = `r(lb)'
		local st50uci = `r(ub)'
		
		qui stci if `touse3', p(25)
		local st25 = `r(p25)'
		local st25lci = `r(lb)'
		local st25uci = `r(ub)'
		
		qui stci if `touse3', p(75)
		local st75 = `r(p75)'
		local st75lci = `r(lb)'
		local st75uci = `r(ub)'
		
		if "`trunc_time'" == "" {
			if "`by'"=="" {
				qui sum _t if `touse'
				local tmax1=`r(max)'
				local text1= "Truncation time not given for RMST, longest observed time was used"
				local text2= "(`=string(`tmax1',"%20.2f")')"
				dis as text "`text1' `text2'"
				qui RMST if `touse3', `rmst_model_i' df(`fpm_df_i')
			} 
			else {
				qui sum `var'
				local mic=`r(max)'
				foreach g of local glev {
					qui sum `var' if `by'==`g'
					local mi=`r(max)'
					local mic=min(`mic',`mi')
				}
				qui RMST if `touse3', tmax(`mic') `rmst_model_i' df(`fpm_df_i')
			}
		} 
		else {
			qui RMST if `touse3', tmax(`trunctime') `rmst_model_i' df(`fpm_df_i')
		}
		
		local rmst = 	`r(rmst_`rmst_model_i')'
		local rmstlci = `r(rmst_lci_`rmst_model_i')'
		local rmstuci = `r(rmst_uci_`rmst_model_i')'
		
		if "`by'"=="" {
			post `res' ("`var'") ("`lb'") ("tte") (.) ("") (.) `etime_post' `stime_post' ///
				(`nt') (`nnm') (.) ///
				(.) (.) (.) ///
				(.) (.) (.) (.)  ///
				(.) (.) (.) (.) (.) (.) (.) (.) ///
				(.) (.) (.) (.) (.) ///
				(`nfails') (`stime') (`st50') (`st50lci') (`st50uci') (`st25') (`st75') ///
				(`rmst') (`rmstlci') (`rmstuci') ///
				`add_user_var_post' ///
				(`kidp')
				 
		} 
		else {
			
			//minimal of maximal observed time in each group:
			qui sum `var'
			local mic=`r(max)'
			foreach g of local glev {
				qui sum `var' if `by'==`g'
				local mi=`r(max)'
				local mic=min(`mic',`mi')
			}
				
			if "`trunctime'"=="" {
				local trunctime = `mic'
				local text1= "Truncation time not given for RMST, the minimum of the largest observed event time"
				local text2= "over all groups is used to calculate mean restricted survival time"
				local text3= "(`=string(`trunctime',"%20.2f")')."
				dis as text "`text1' `text2' `text3'"
			}
			else {
				if `trunctime'>`mic' {
					local text1= "Truncation time for `var' is longer than the"
					local text2= "minimum of the longest observation time in all groups"
					local text3= "(`=string(`trunctime',"%20.2f")' vs `=string(`mic',"%20.2f")')."
					local text4= "The restricted mean survival times are not comparable between groups."
					local text5= "Consider reducing the truncation time!"
					dis as text "`text1' `text2' `text3' `text4' `text5'"
				}	
			}
			
			local fp		
			foreach g of local glev {
			
				qui sum `failvar' if `by'==`g' &  `touse3',d
				local nfails_`g' = r(sum)
				
				qui sum `var' if `by'==`g' & `touse3', d
				local stime_`g'  = r(sum)
				
				qui stci if `by'==`g' & `touse3'
				local st50_`g' = `r(p50)'
				local st50lci_`g' = `r(lb)'
				local st50uci_`g' = `r(ub)'
				
				qui stci if `by'==`g' & `touse3', p(25)
				local st25_`g' = `r(p25)'
				local st25lci_`g' = `r(lb)'
				local st25uci_`g' = `r(ub)'
				
				qui stci if `by'==`g' & `touse3', p(75)
				local st75_`g' = `r(p75)'
				local st75lci_`g' = `r(lb)'
				local st75uci_`g' = `r(ub)'
				
				qui RMST if `by'==`g' & `touse3', `rmst_model_i' tmax(`trunctime') df(`fpm_df_i')
				local rmst_`g' = 	`r(rmst_`rmst_model_i')'
				local rmstlci_`g' = `r(rmst_lci_`rmst_model_i')'
				local rmstuci_`g' = `r(rmst_uci_`rmst_model_i')'	
				
				local fp `fp' (`nt_`g'') (`nnm_`g'') (.) ///
					(.) (.) (.) ///
					(.) (.) (.) (.)  ///
					(.) (.) (.) (.)  (.) (.) (.) (.) ///
					(.) (.) (.) (.) (.) ///
					(`nfails_`g'') (`stime_`g'') (`st50_`g'') (`st50lci_`g'') (`st50uci_`g'') ///
					(`st25_`g'') (`st75_`g'') ///
					(`rmst_`g'') (`rmstlci_`g'') (`rmstuci_`g'')
				}
			
			if `gc'==1 {
				post `res' ("`var'") ("`lb'") ("tte") (.) ("") (.) `etime_post' `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) (.)  ///
					 (.) (.) (.) (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) ///
				     (`nfails') (`stime') (`st50') (`st50lci') (`st50uci') (`st25') (`st75') ///
					 (`rmst') (`rmstlci') (`rmstuci') ///
					 `fp' ///
					 `add_user_var_post' ///
					 (`kidp')
			}
			
			if `gc'==2 {
			
				//local by random_grp
				//levelsof `by', local(glev)
				//local var age
				local g1: word 1 of `glev'
				local g2: word 2 of `glev'
				tempvar grrc
				qui recode `by' (`g1'=1) (`g2'=0), gen(`grrc')
					
		
				//no test and effects
				if strpos("`testlist2'","logrank")==0 & strpos("`testlist2'","cox")==0  ///
					& strpos("`testlist2'","rmstd")==0 ///
					& strpos("`effectlist'","hr")==0 & strpos("`effectlist'","rmstd")==0 {
				
					local hr=.
					local hr_lci=.
					local hr_uci=.
					
					local rmstd=.
					local rmstd_lci=.
					local rmstd_uci=.
					
					local p_logrank=.
					local p_cox=.
					local p_rmstd=.
					
					
				}
				
				else {
				
					qui stset `var' if `touse3', fail(`failvar')		
					
					*cox
					if strpos("`effectlist'","hr")!=0 | strpos("`testlist2'","cox")!=0 {
								
						qui cap stcox ib1.`grrc'		
						tempname A
						matrix `A'=r(table)
						local hr=1/`A'[1,1]
						local hr_lci=1/`A'[6,1]
						local hr_uci=1/`A'[5,1]
						local p_cox=`A'[4,1]
						if "`cox_lr'" != ""  | strpos("`lr'","cox")>0 {
							local p_cox = 1-chi2(`e(df_m)',`e(chi2)')
						}	
					} 
					if strpos("`effectlist'","hr")==0 {
						local hr=.
						local hr_lci=.
						local hr_uci=.
					}
					if  strpos("`testlist2'","cox")==0 {
						local p_cox=.
					}
					
					
					*logrank
					if  strpos("`testlist2'","logrank")!=0 {
						qui cap sts test `grrc'
						local p_logrank = 1-chi2(`r(df)',`r(chi2)')
					}
					else {
						local p_logrank=.
					}
					
					*rmst
					if strpos("`effectlist'","rmstd")!=0 | strpos("`testlist2'","rmstd")!=0 {
					
						qui RMST `grrc' if `touse3', `rmst_model_i' tmax(`trunctime') df(`fpm_df_i')
						local rmstd = 	`r(rmstd_`rmst_model_i')'
						local rmstd_lci = `r(rmstd_lci_`rmst_model_i')'
						local rmstd_uci = `r(rmstd_uci_`rmst_model_i')'
						local p_rmstd = `r(rmstd_p_`rmst_model_i')'
					}
					
					if strpos("`effectlist'","rmstd")==0 {
						local rmstd=.
						local rmstd_lci=.
						local rmstd_uci=.
					}

					
					if strpos("`testlist2'","rmstd")==0 {
							local p_rmstd=.
					}
						
				}						
					
				
				post `res' ("`var'") ("`lb'") ("tte") (.) ("") (.) `etime_post' `stime_post' ///
					 (`nt') (`nnm') (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) (.)  ///
					 (.) (.) (.) (.) (.) (.) (.) (.) ///
					 (.) (.) (.) (.) (.) ///
				     (`nfails') (`stime') (`st50') (`st50lci') (`st50uci') (`st25') (`st75') ///
					 (`rmst') (`rmstlci') (`rmstuci') ///
					 `fp' ///
					 (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) (.)  ///
					 (.) (.) (.)  ///
					 (.) (.) (.) ///
					 (.) (.) (.) ///
					 (.) (.) ///
					 (.) (.) (.) ///	
					 (.) (.) (.) ///
					 (`p_logrank') (`p_cox') (`p_rmstd') ///
					 (`hr') (`hr_lci') (`hr_uci') ///
					 (`rmstd') (`rmstd_lci') (`rmstd_uci') ///
					`add_user_var_post'	 ///
					(`kidp')					
					 
			}
			
			if `gc'>2 {	
			
				local p_logrank = . 
				local p_cox = .
				
				*logrank
				if  strpos("`testlist2'","logrank")!=0 {
					qui cap sts test `by'
					local p_logrank = 1-chi2(`r(df)',`r(chi2)')
				}
			
				*cox
				if strpos("`effectlist'","hr")!=0 | strpos("`testlist2'","cox")!=0 {					
					qui cap stcox i.`by'		
					qui testparm i.`by'
					local p_cox = `r(p)'	
					if "`cox_lr'" != ""  | strpos("`lr'","cox")>0 {
						local p_cox = 1-chi2(`e(df_m)',`e(chi2)')
					}
				} 
				
						
				post `res' ("`var'") ("`lb'") ("tte") (.) ("") (.) `etime_post' `stime_post' ///
					(`nt') (`nnm') (.) ///
					(.) (.) (.) ///
					(.) (.) (.) (.)  ///
					(.) (.) (.) (.) (.) (.) (.) (.) ///
					(.) (.) (.) (.) (.) ///
				    (`nfails') (`stime') (`st50') (`st50lci') (`st50uci') (`st25')  (`st75')  ///
					(`rmst') (`rmstlci') (`rmstuci') ///
					`fp' ///
					(.) (.) ///
					(.) (.) ///
					(.) (.) ///
					(`p_logrank') (`p_cox') ///
					`add_user_var_post' ///
					(`kidp')
			}				
		}	 
		
	restore	
	}
	
}

postclose `res'	


*prepare final table
******************

preserve

*formatting

use "`saving'", clear
format varlabel %-244s
format levlabel %-244s
qui compress

qui levelsof vtype, local(tlev) clean

*drop cat
if strpos("`tlev'","cat")==0 {
	qui drop nlev* levlabel level pr_* prlci_* pruci_*
}

*drop conti
if strpos("`tlev'","conti")==0 {
	qui drop mean_* sd_* meanlci_* meanuci_* p50_* p25_* p75_* iqr_* min_* max_* range_* sum_*
}


*drop count 
if "`count'"=="" {
	qui drop nevents_* etime_* ir_* irlci_* iruci_*
}

*drop tte 
if "`tte'"=="" {
	qui drop nfails_* stime_* st50_* st50lci_* st50uci_* st25_* st75_* rmst_* rmstlci_* rmstuci_*
}


*delete missing effect measures and tests:
if "`by'"!="" {
	if "`diff'"!="" {
		foreach var of varlist `diff' {
			cap assert mi(`var')
			if !_rc {
				qui drop `var'
			}
		}
	}
}	

*delete missing user vars
if "`add_user_var_un'"!="" {
	foreach var of varlist `add_user_var_un' {
		cap assert mi(`var')
		if !_rc {
			qui drop `var'
		}
	}
}


*drop kid
qui sum kid
if `r(sum)' == 0 {
	cap drop kid
}
 
*label variables

label var varname 		"Variable name"
label var varlabel		"Variable label"
label var vtype 		"Variable type"

cap label var level 	"Level"
cap label var levlabel	"Level label"
cap label var nlev		"Number of levels for categorical variables"

label var ntot_t 		"Total number of observations"
label var nnonmiss_t 	"Number of non-missing observations"

cap label var nlev_t 	"Number of observation per category"
cap label var pr_t 		"Proportion in category"
cap label var prlci_t 	"Proportion, lower `level'% confidence limit"
cap label var pruci_t 	"Proportion, upper `level'% confidence limit"

cap label var mean_t 	"Mean"
cap label var sd_t 		"Standard deviation"
cap label var meanlci_t "Mean, lower `level'% confidence limit"
cap label var meanuci_t	"Mean, upper `level'% confidence limit"
cap label var p50_t 	"Median"
cap label var p25_t 	"Lower quartile"
cap label var p75_t 	"Upper quartile"	
cap label var iqr_t 	"Interquartile range"	
cap label var min_t 	"Minimum"
cap label var max_t 	"Maximum"
cap label var range_t 	"Range"
cap label var range_t 	"Sum"

cap label var nevents_t "Number of events"
cap label var etime_t 	"Exposure time"
cap label var etime_unit "Unit for exposure time"
cap label var ir_t 		"Incidence rate"
cap label var irlci_t 	"Incidence rate, lower `level'% confidence limit"
cap label var iruci_t 	"Incidence rate, upper `level'% confidence limit"

cap label var nfails_t 	"Number of failure"
cap label var stime_t 	"Time at risk"
cap label var stime_unit "Unit for time-to-event"
cap label var st50_t 	"Median survival time"
cap label var st50lci_t "Median survival time, lower `level'% confidence limit"
cap label var st50uci_t "Median survival time, upper `level'% confidence limit"
cap label var st25_t 	"Lower quartile survival time"
cap label var st75_t 	"Upper quartile survival time"
cap label var rmst_t 	"Restricted mean survival time"
cap label var rmstlci_t "Restricted mean survival time, lower `level'% confidence limit"
cap label var rmstuci_t "Restricted mean survival time, upper `level'% confidence limit"

if "`gc'"!="" {
	if `gc'>1 {
		local i=0
		foreach g of local glev {
			local i=`i'+1
			
			cap label var ntot_`i' 		"`glab`i'': Total number of observations"	
			cap label var nnonmiss_`i'	"`glab`i'': Number of non-missing observations"
			cap label var nlev_`i'		"`glab`i'': Number of observation per category"
			cap label var pr_`i'		"`glab`i'': Proportion in category"
			cap label var prlci_`i'		"`glab`i'': Proportion, lower `level'% confidence limit"
			cap label var pruci_`i'		"`glab`i'': Proportion, upper `level'% confidence limit"
			
			cap label var mean_`i'      "`glab`i'': Mean"
			cap label var sd_`i'        "`glab`i'': Standard deviation"
			cap label var meanlci_`i'   "`glab`i'': Mean, lower `level'% confidence limit"
			cap label var meanuci_`i'   "`glab`i'': Mean, upper `level'% confidence limit"
			cap label var p50_`i'       "`glab`i'': Median"
			cap label var p25_`i'       "`glab`i'': Lower quartile"
			cap label var p75_`i'       "`glab`i'': Upper quartile"	
			cap label var iqr_`i'       "`glab`i'': Interquartile range"	
			cap label var min_`i'       "`glab`i'': Minimum"
			cap label var max_`i'       "`glab`i'': Maximum"
			cap label var range_`i'     "`glab`i'': Range"
			cap label var sum_`i'    	"`glab`i'': Sum"
			
			cap label var nevents_`i' 	"`glab`i'': Number of events"
			cap label var etime_`i' 	"`glab`i'': Exposure time"
			cap label var ir_`i' 	    "`glab`i'': Incidence rate"
			cap label var irlci_`i'		"`glab`i'': Incidence rate, lower `level'% confidence limit"
			cap label var iruci_`i'		"`glab`i'': Incidence rate, upper `level'% confidence limit"
			
			cap label var nfails_`i' 	"`glab`i'': Number of failure"
			cap label var stime_`i' 	"`glab`i'': Time at risk"
			cap label var st50_`i' 		"`glab`i'': Median survival time"
			cap label var st50lci_`i' 	"`glab`i'': Median survival time, lower `level'% confidence limit"
			cap label var st50uci_`i' 	"`glab`i'': Median survival time, upper `level'% confidence limit"
			cap label var st25_`i' 		"`glab`i'': Lower quartile survival time"
			cap label var st75_`i' 		"`glab`i'': Upper quartile survival time"
			cap label var rmst_`i' 		"`glab`i'': Restricted mean survival time"
			cap label var rmstlci_`i' 	"`glab`i'': Restricted mean survival time, lower `level'% confidence limit"
			cap label var rmstuci_`i' 	"`glab`i'': Restricted mean survival time, upper `level'% confidence limit"

		}
		
		if "`chi2_lr'"=="" & strpos("`lr'","chi2")>0 {
			cap label var p_chi2 	"P-value from Pearson's chi-squared test"
		}
		else {
			cap label var p_chi2	"P-value from likelihood ratio chi-squared test"
		}
		
		cap label var p_fisher		"P-value from Fisher's exact test"
		cap label var rd 			"Risk difference"
		cap label var rd_lci		"Risk difference, lower `level'% confidence limit"
		cap label var rd_uci        "Risk difference, upper `level'% confidence limit"
		cap label var rr 			"Risk ratio"
		cap label var rr_lci        "Risk ratio, lower `level'% confidence limit"
		cap label var rr_uci        "Risk ratio, upper `level'% confidence limit"
		cap label var or 			"Odds ratio"
		cap label var or_lci        "Odds ratio, lower `level'% confidence limit"
		cap label var or_uci        "Odds ratio, upper `level'% confidence limit"
		
		
		cap label var p_ttest 		"P-value from Student's t-test"
		cap label var p_ranksum 	"P-value from Wilcoxon's rank-sum test"
		cap label var p_qreg		"P-value from quantile regression"	
		cap label var p_qreg		"P-value from quantile regression"	
		cap label var p_anova		"P-value from ANOVA" 
		cap label var p_kwallis		"P-value from Kruskal-Wallis test"
		
		cap label var meand 		"Mean difference"
		cap label var meand_lci 	"Mean difference, lower `level'% confidence limit"
		cap label var meand_uci 	"Mean difference, upper `level'% confidence limit"
		cap label var medd 			"Median difference"
		cap label var medd_lci   	"Median difference, lower `level'% confidence limit"
		cap label var medd_uci      "Median difference, upper `level'% confidence limit"
		cap label var hlmd 			"Hodges-Lehmann median difference"
		cap label var hlmd_lci      "Hodges-Lehmann median difference, lower `level'% confidence limit"
		cap label var hlmd_uci      "Hodges-Lehmann median difference, upper `level'% confidence limit"
		cap label var mws 			"Mann-Withney statistic"	
		cap label var mws_lci 		"Mann-Withney statistic, lower `level'% confidence limit"
		cap label var mws_uci		"Mann-Withney statistic, upper `level'% confidence limit"	
		 
		cap label var p_poisson		"P-value from Poisson exact test"
		if "`poisson_test'" == "oim" {
			cap label var p_poisson	"P-value from Poisson regression"
			if "`poisson_lr'"!="" | strpos("`lr'","poisson")>0 {
				cap label var p_poisson	"P-value from Poisson regression based on a likelihood ratio test"
			}
		}
		if "`poisson_test'" == "robust" {
			cap label var p_poisson	"P-value from Poisson regression with robust standard errors"
		}
		cap label var p_nbreg 		"P-value from negative binomial regression"
		if "`nbreg_lr'"!="" | strpos("`lr'","nbreg")>0 {
			cap label var p_nbreg 		"P-value from negative binomial regression  based on a likelihood ratio test"
		}		
		cap label var ird 			"Incidence rate difference"
		cap label var ird_lci		"Incidence rate difference, lower `level'% confidence limit"
		cap label var ird_uci       "Incidence rate difference, upper `level'% confidence limit"
		cap label var irr 			"Incidence rate ratio"
		cap label var irr_lci       "Incidence rate ratio, lower `level'% confidence limit"
		cap label var irr_uci       "Incidence rate ratio, upper `level'% confidence limit"
		
		cap label var p_logrank		"P-value from log-rank test"
		cap label var p_cox			"P-value from Cox model"
		if "`cox_lr'"!="" | strpos("`lr'","cox")>0 {
			cap label var p_cox			"P-value from Cox model based on a likelihood ratio test"
		}
		cap label var p_rmstd		"P-value from test for restricted mean survival time difference"
		cap label var hr 			"Hazard ratio"
		cap label var hr_lci		"Hazard ratio, lower `level'% confidence limit"
		cap label var hr_uci        "Hazard ratio, upper `level'% confidence limit"
		cap label var rmstd 		"Restriced mean survival time difference"
		cap label var rmstd_lci		"Restriced mean survival time difference, lower `level'% confidence limit"
		cap label var rmstd_uci     "Restriced mean survival time difference, upper `level'% confidence limit"
		
	}
}

cap label var user_pe "User-defined point estimate"
cap label var user_lci "User-defined lower `level'% confidence limit"
cap label var user_uci "User-defined upper `level'% confidence limit"
cap label var user_p "User-defined p-value"


save "`saving'", replace
restore


end	

/**********************************************************************************************************************/

********************************
*Function for replacing specific element in a list
********************

cap program drop repl_list 
program repl_list

syntax , base(string) posvar(string) repl(string)

local posvar1 = `posvar' -1
local posvar2 = `posvar' + 1
local wk: word count `base'

local fi
forvalues k=1/`posvar1' {
	local f1: word `k' of `base'
	local fi `fi' `f1'
}
local sec
forvalues k=`posvar2'/`wk' {
	local f1: word `k' of `base'
	local sec `sec' `f1'
}
local base2 `fi' `repl' `sec'

c_local rlist `base2'
//returns rlist without removing results stored in rclass

end








