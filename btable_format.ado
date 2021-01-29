*! version 1.0.0 26jan2021
cap program drop btable_format
program btable_format, nclass

version 16

syntax using [, clear ///
	ncol(string) drop(string) nametot(string) nrow  ///
	design(string) inset(string) inset_row(string) parse(string) ///
	DEScriptive(string) EFFect(string) ///
	DIGits(string)  CATDigits0(string) ///
	digits_effect(string) ///
	digits_type(string) maxdec(string) ///
	digits_type_effect(string) maxdec_effect(string) ///
	PERCentage(string) ABBReviation ///
	test(string)  ///
	p_format(string) p_digits(string) p_breaks(string) p_all  /// p_lab 
	collapse(string) collapselev(string) NOBracket(string) ///
	block(string) block_head(string) block_inset(string) block_parse(string) ///
	nonmiss(string) ///
	inset_kid(string) ///
	namemiss(string) ///
	] 	
					
*for backwards compatibility
********************************

assert "`digits_diff'"=="" | "`digits_effect'"==""
if "`digits_effect'"=="" {
	local digits_effect `digits_diff'
}
assert "`type_diff'"=="" | "`type_effect'"==""
if "`type_effect'"=="" {
	local type_effect `type_diff'
}
assert "`ft_diff'"=="" | "`ft_effect'"==""
if "`ft_effect'"=="" {
	local ft_effect `ft_diff'
}


*install auxilliary files
***********************
if "`block'"!="" {
	cap which tknz
	if _rc {
		dis as error "tknz is required; type -ssc install tknz- to obtain it"
		exit 499
		//dis "Installing auxiliary ado-file tknz: "
		//ssc install tknz
	}
}

*parsing chat default
******************
if "`parse'"=="" {
	local parse "|"
}

*entry checks
****************
local exit 0

preserve
use `using', `clear'
qui compress
qui levelsof varname, local(vlev)


*new name for ft_type: descriptive
local ft_type `descriptive'
local ft_type_effect `effect'


*test
if "`test'"!=""  {
	local wc: word count `test'
	forvalues w=1/`wc' {
		local wi: word `w' of `test'
		local inl 0
		foreach vle of local vlev {
			if "`wi'" =="`vle'" {
				local inl 1
			}
		}
		if `inl' != 1 {
			if !inlist("`wi'","conti","cat","count","tte") & ///
				!inlist("`wi'","ttest","ranksum","qreg","fisher","chi2","anova","kwallis") & ///
				!inlist("`wi'","poisson","nbreg","logrank","cox","rmstd") & ///
				!inlist("`wi'","`parse'") {
					cap confirm variable `wi'
					if _rc {
						local resp1="`wi': invalid test." 
						local resp2="Please enter any of varname, conti, cat, count, tte," 
						local resp3="ttest, ranksum, qreg, fisher, chi2, anova, kwallis, poisson, nbreg, logrank, cox, and rmstd,"
						local resp4=" or a valid variable name."
						dis as error "`resp1'"
						dis as error "`resp2' `resp3' `resp4'"
						local exit 1
				}	
			}
		}
	}
}


*varname that corresponds to variable type:

local key conti cat count tte 
foreach k of local key {
	qui count if varname == "`k'"
	if `r(N)'>0 {
		dis as error "Variable types (conti, cat, count and tte) are not allowed as variable names."
		dis as error "Please use another name than `k'."
		local exit 1
	}
}				


restore
if `exit'==1 {
	exit
}

*design
if "`design'"!="" {
cap assert inlist("`design'","row","long","column","wide","missing")
	if _rc {
		dis as text "Option for design not recognized, default (column) used."
		local design="column"
	}
}

if "`namemiss'"=="" {
	local namemiss "missing"
}

*inset
if "`inset'"=="" {
	local inset = "    "
}
if "`inset'"=="none" {
	local inset = ""
}
	
*inset row
if "`inset_row'"=="" {
	local inset_row=" - "
}
if "`inset_row'"=="none" {
	local inset_row = ""
}


*load data						
use `using', `clear'
qui compress
	
*all variables 
qui ds
local allv `r(varlist)'	
	
*no tests
local notest="no"
cap qui desc p_*
if _rc {
	local notest="yes"
	if "`test'"!="" | "`p_format'" !="" | "`p_all'" !="" {
		dis as text "No test column in input data, all test options ignored."
	}
}

*no effect
local noeffect="no"
local elist rd rr or meand medd hlmd mws irr ird hr rmstd user
local crc=0
foreach el of local elist {
	local counter=`counter'+1
	cap qui desc `el'*
	if  _rc {
		local crc=`crc'+1
	}
}

if `crc'==`counter' {
	local noeffect="yes"
	if  "`digits_effect'" !="" | "`type_effect'" != "" | "`ft_effect'" != "" {
		dis as text "No effects column in input data, all effect options ignored."
	}
}	


*name for total column
if "`nametot'"=="" {
	local nametot="Total"
}

*no of groups
qui ds ntot*
local ngroups: word count `r(varlist)'
local ngroups=`ngroups'-1
//dis "`ngroups'"

*groupfile
local groupfile="`2'_group"
if `ngroups'>=1 {
	preserve
	use "`groupfile'", clear
	qui drop if ntg==0
	qui count
	local ng=r(N)
	assert `ng'==`ngroups'
	forvalues i=1/`ngroups' {
		local lb`i'=label[`i']
		local nt`i'=ntg[`i']
	}
	local nt=nt[1]
	restore
}


*ci level
local cilev
cap local lbh: var label meanlci_t
cap local cilev=substr("`lbh'",strpos("`lbh'","%")-2,3)
if "`cilev'"=="" {
	local cilev "95%"
}

*label for poisson test
local lbpoisson = "exact"
cap local lb_p_poisson: var label p_poisson
if strpos("`lb_p_poisson'","regression")>0 {
	local lbpoisson = "oim"
}
if strpos("`lb_p_poisson'","robust")>0 {
	local lbpoisson = "robust"
}


*check if denominator is non-missing 
local denom all
cap ds nlev_t
if !_rc {
	cap assert nlev_t==ntot_t if vtype=="cat1"
	if _rc {
		local denom nonmiss
		cap assert nlev_t==nnonmiss_t if vtype=="cat1"
		if _rc & strpos("`vtypes'","cat")>0 {
			dis as text "Problems with denominator for categorical variables, total used"
			local denom all
		}
	} 
}


if "`denom'"=="nonmiss" & "`design'"=="missing" {	
	dis as text "Design missing is used together with option denominator(nonmiss) in btable, categorical variables may sum up to more than the total."
}
	
	
*empty locals, vars	
local collect_type	
local collect_type_effect	
local collect_test_conti
local collect_test_cat
local collect_test_count
local collect_test_tte
local frac_mws
local frac_rd


tempvar rdp
tempvar rdp_lci
tempvar rdp_uci
qui gen `rdp'=.
qui gen `rdp_lci'=.
qui gen `rdp_uci'=.	
tempvar pop
tempvar pop_lci
tempvar pop_uci
qui gen `pop'=.
qui gen `pop_lci'=.
qui gen `pop_uci'=.	

tempname ssy


*missing variables
//variable types
local vtypes2
qui levelsof vtype, local(vtypes)
foreach l of local vtypes {
	local vtypes2 `vtypes2' `l'
}

//categorical vars:	
qui ds
if strpos("`r(varlist)'","levlabel")==0 {
	qui gen levlabel=""
}

//effects
if strpos("`vtypes2'","cat")>0 & `ngroups'==2 {

	qui ds

	if strpos("`r(varlist)'"," rd ")==0 {
		qui gen rd=.
		qui gen rd_lci=.
		qui gen rd_uci=.
		if (strpos("`type_effect'","rd")>0)  & strpos("`drop'","diff")==0 {
			dis as text "Variable rd is missing, risk difference not available"
		}
	}
	if strpos("`r(varlist)'"," rr ")==0  {
		qui gen rr=.
		qui gen rr_lci=.
		qui gen rr_uci=.
		if strpos("`type_effect'","rr")>0 & strpos("`drop'","diff")==0 {
			dis as text "Variable rr is missing, risk ratio not available"
		}
	}
	if strpos("`r(varlist)'"," or ")==0 {
		qui gen or=.
		qui gen or_lci=.
		qui gen or_uci=.
		if strpos("`type_effect'","or")>0 & strpos("`drop'","diff")==0  {
			dis as text "Variable or is missing, odds ratio not available"
		}
	}
}

//pvalues

local default_cat "fisher"

if strpos("`vtypes2'","cat")>0 & `ngroups'>0 {
	
	if strpos("`r(varlist)'","p_chi2")>0 & strpos("`r(varlist)'","fisher")==0 {
		local default_cat "chi2"
		if strpos("`test'","chi2")==0 {
			dis as text "Default test (fisher) not available, chi2 used."
		}
	}
	
	qui ds	
	if strpos("`r(varlist)'","p_chi2")==0 {
		qui gen p_chi2=.
		if strpos("`test'","chi2")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_chi2 is missing, chi-squared test not available"
		}
	}
	if strpos("`r(varlist)'","p_fisher")==0 {
		qui gen p_fisher=.
		if strpos("`test'","fisher")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_fisher is missing, Fisher's exact test not available"
		}
	}
	
}


//effect and pvalues

if strpos("`vtypes2'","conti")>0 & `ngroups'==2 {
	
	qui ds	
	if strpos("`r(varlist)'"," meand ")==0 {
		qui gen meand=.
		qui gen meand_lci=.
		qui gen meand_uci=.
		if (strpos("`type_effect'","meand")>0) & strpos("`drop'","diff")==0 { 
			dis as text "Variable meand is missing, mean difference not available"
		}
	}
	if strpos("`r(varlist)'"," medd ")==0 {
		qui gen medd=.
		qui gen medd_lci=.
		qui gen medd_uci=.
		if strpos("`type_effect'","medd")>0 & strpos("`drop'","diff")==0 {
			dis as text "Variable medd is missing, median difference not available"
		}
	}
	if strpos("`r(varlist)'"," hlmd ")==0 {
		qui gen hlmd=.
		qui gen hlmd_lci=.
		qui gen hlmd_uci=.
		if strpos("`type_effect'","hlmd")>0  & strpos("`drop'","diff")==0 {
			dis as text "Variable hlmd is missing, location difference not available"
		}
	}
	if strpos("`r(varlist)'","mws")==0 {
		qui gen mws=.
		qui gen mws_lci=.
		qui gen mws_uci=.
		if strpos("`type_effect'","mws")>0 & strpos("`drop'","diff")==0 {
			dis as text "Variable mws is missing, MWS not available"
		}
	}
	
	if strpos("`r(varlist)'","p_ttest")==0 {
		qui gen p_ttest=.
		if strpos("`test'","ttest")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_ttest is missing, Student's t-test test not available"
		}
	}	
	if strpos("`r(varlist)'","p_ranksum")==0 {
		qui gen p_ranksum=.
		if strpos("`test'","ranksum")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 &  ///
			"`notest'"!="yes" {
			dis as text "Variable p_ranksum is missing, Wilcoxon rank-sum test not available"
		}
	} 
	if strpos("`r(varlist)'","p_qreg")==0 {
		qui gen p_qreg=.
		if (strpos("`test'","qreg")>0 | strpos("`test'","med")>0) & ///
			strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & "`notest'"!="yes" {
			dis as text "Variable p_qreg is missing, p-value from quantile regression not available"
		}
	}
}

//pvalues for more than 1 group
if strpos("`vtypes2'","conti")>0 & `ngroups'>2 {
	
	qui ds	
	if strpos("`r(varlist)'","p_anova")==0 {
		qui gen p_anova=.
		if strpos("`test'","anova")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 {
			dis as text "Variable p_anova is missing, p-value from ANOVA not available"
		}
	}
	if strpos("`r(varlist)'","p_kwallis")==0 {
		qui gen p_kwallis=.
		if strpos("`test'","kwallis")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 {
			dis as text "Variable p_kwallis is missing, p-value from Kruskal-Wallis test not available"
		}
	}
}

//count variables

//effects
if strpos("`vtypes2'","count")>0 & `ngroups'==2 {

	qui ds

	if strpos("`r(varlist)'"," ird ")==0 {
		qui gen ird=.
		qui gen ird_lci=.
		qui gen ird_uci=.
		if (strpos("`type_effect'","ird")>0)  & strpos("`drop'","diff")==0 {
			dis as text "Variable ird is missing, incidence rate difference not available"
		}
	}
	if strpos("`r(varlist)'"," irr ")==0  {
		qui gen irr=.
		qui gen irr_lci=.
		qui gen irr_uci=.
		if strpos("`type_effect'","irr")>0 & strpos("`drop'","diff")==0 {
			dis as text "Variable irr is missing, incidence rate ratio not available"
		}
	}
}

//pvalues

if strpos("`vtypes2'","count")>0 & `ngroups'>0 {
	qui ds	
	if strpos("`r(varlist)'","p_poisson")==0 {
		qui gen p_poisson=.
		if strpos("`test'","poisson")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_poisson is missing, poisson test not available"
		}
	}
	if strpos("`r(varlist)'","p_nbreg")==0 {
		qui gen p_nbreg=.
		if strpos("`test'","nbreg")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_nbreg is missing, negative binomial test not available"
		}
	}
}	


//tte variables

//effects
if strpos("`vtypes2'","tte")>0 & `ngroups'==2 {

	qui ds

	if strpos("`r(varlist)'"," hr ")==0 {
		qui gen hr=.
		qui gen hr_lci=.
		qui gen hr_uci=.
		if (strpos("`type_effect'","hr")>0)  & strpos("`drop'","diff")==0 {
			dis as text "Variable hr is missing, hazard ratio not available"
		}
	}
	if strpos("`r(varlist)'"," rmstd ")==0  {
		qui gen irr=.
		qui gen irr_lci=.
		qui gen irr_uci=.
		if strpos("`type_effect'","rmstd")>0 & strpos("`drop'","diff")==0 {
			dis as text "Variable rmstd is missing, restricted mean survival time difference not available"
		}
	}
}

//pvalues

if strpos("`vtypes2'","tte")>0 & `ngroups'>0 {
	qui ds	
	if strpos("`r(varlist)'","p_logrank")==0 {
		qui gen p_logrank=.
		if strpos("`test'","logrank")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_logrank is missing, log rank test not available"
		}
	}
	if strpos("`r(varlist)'","p_cox")==0 {
		qui gen p_cox=.
		if strpos("`test'","cox")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_cox is missing, p-value from Cox model not available"
		}
	}
	if strpos("`r(varlist)'","p_rmstd")==0 {
		qui gen p_rmstd=.
		if strpos("`test'","rmstd")>0 & strpos("`drop'","p")==0 & strpos("`drop'","test")==0 & ///
			"`notest'"!="yes" {
			dis as text "Variable p_rmstd is missing, p-value from restricted mean survival time difference not available."
		}
	}
}	


//point estimate but no ci:
local elist rd rr or meand medd hlmd mws ird irr hr rmstd

foreach l of local elist {
	qui ds 
	if strpos("`r(varlist)'","`l'")>0 {
		cap assert strpos("`r(varlist)'","`l'_lci")>0 
		if _rc {
			cap gen `l'_lci=.
		}
		cap assert strpos("`r(varlist)'","`l'_uci")>0 
		if _rc {
			cap gen `l'_uci=.
		}
	}
}	
	
*percentages for rd and/or mws
if "`percentage'" != "" {
	if strpos("`percentage'","rd")> 0 {
		qui replace rd=100*rd
		qui replace rd_lci=100*rd_lci
		qui replace rd_uci=100*rd_uci
	}
	if strpos("`percentage'","mws")> 0 {
		qui replace mws=100*mws
		qui replace mws_lci=100*mws_lci
		qui replace mws_uci=100*mws_uci
	}
}


*possible keywords
******************

*list of variable types
local vtypelist conti cat count tte

*user variables:
local uvars

foreach v of local allv {
	local vm = subinstr("`v'","_t","",.)
	forvalue i = 1/`ngroups' {
		local vm = subinstr("`vm'","_`i'","",.)
	}
	local uvars `uvars' `vm'
}
local ddups: list dups uvars
local uvars: list uvars - ddups 

*list of defined descriptives
local desclist mean sd meanlci meanuci median lq uq iqr min max range sum ///
		nlev ntot pr prlci pruci prop proplci propuci perc perclci percuci ///
		ir irlci iruci nevents etime ///
		st50 st50lci st50uci rmst rmstlci rmstuci nfails stime st25 st75 ///
		`uvars'
		
local nuse varlabel nnonmiss levlabel varname
	
local ddups: list dups desclist
local desclist: list desclist - ddups 
local desclist: list desclist - nuse 
	
sortlistlen, listin(`desclist')
local desclist `r(listout)'
		

*list of defined effect measures
local efflist meand medd hlmd mws rd rr or ///
			irr ird hr rmstd ///
			meand_uci medd_uci hlmd_uci mws_uci rd_uci rr_uci or_uci irr_uci ird_uci hr_uci rmstd_uci ///
			meand_lci medd_lci hlmd_lci mws_lci rd_lci rr_lci or_lci irr_lci ird_lci hr_lci rmstd_lci ///
			`uvars'

local ddups: list dups  efflist
local  efflist: list  efflist - ddups 
local  efflist: list  efflist - nuse 
	
sortlistlen, listin(`efflist')
local  efflist `r(listout)'


*list of pvalues
local pvlist
	
*tempname to replace % signs
tempname rp

	
*design	
***********
if "`design'"=="" {
	local design column
}

tempvar nrowvar
qui gen `nrowvar' = 1
tempvar varname_sp
qui gen `varname_sp' = " " + varname + " "
tempvar vtype_sp
qui gen `vtype_sp' = " " + vtype + " "

local inptypelist ft_type ft_type_effect test digits digits_effect digits_type digits_type_effect maxdec maxdec_effect

*tokenize ft_type, ft_type_effect, test digits, ...

local anyparse 0
local ca 

foreach inptype of local inptypelist {
	
	if strpos("``inptype''","`parse'")>0 {
		
		local anyparse 1

		//tempname b fcount 
		//local `b': subinstr local `inptype' "`parse'" "`parse'", all count(local `fcount')
		//local `inptype'_count = ``fcount'' + 1 
		
		tknz "``inptype''", parse("`parse'") nochar stub(`inptype')
		local `inptype'_count = `s(items)'
		
		forvalues j=2/`s(items)' {
			qui replace `nrowvar' = `j' if strpos(" ``inptype'`j'' ",`varname_sp')>0 | ///
				strpos(" ``inptype'`j'' ",`vtype_sp')>0
		}
	}
	else {
		local `inptype'_count 1
		local `inptype'1  ``inptype''
	}
	local ca `ca' ``inptype'_count',
}
qui replace `nrowvar'=`nrowvar'[_n+1] if varname==varname[_n+1] & vtype=="cat1"

//count maximal number of measures
local nrows=1
local ca = reverse(subinstr(reverse("`ca'"),",","",1))
matrix CA = `ca'
mata : st_matrix("CB", rowmax(st_matrix("CA")))
if (CB[1,1]>`nrows') {
	local nrows = CB[1,1]
}

if `anyparse'==1 {
	cap assert inlist("`design'","missing","row","long")
	if _rc {
		dis as text "Design `design' not allowed with more than one descriptive measure per variable, row is used."
		local design row
	}
}
	
//nrows per variable?
//list varname vtype `nrowvar'
//dis "`nrows'"
//exit
 
forvalues mrow = 1/`nrows' {
	
	foreach inptype of local inptypelist {
		
		//a) assume the measure is the same as previous one if not specified
		if  "``inptype''" != "" & "``inptype'`mrow''"=="" {
			local mrow1 = `mrow'-1
			if `mrow'==1 {
				local `inptype' ``inptype''
			}
			else {
				local `inptype' ``inptype'`mrow1''
			}
		}
		else {
			local `inptype' ``inptype'`mrow''
		}
		
		//b) assume missing if none specified
		//local `inptype' ``inptype'`mrow''
		
	}
	
	if `mrow'>1 {
			
		local genadd _v`mrow'
	} 
	else {
		local genadd
	}

		
	****************
	*descriptives:  formats, type, test, digits
	************
	
	tempvar ft_typev
	tempvar ftv
	tempvar digitsv
	tempvar digits_typev
	tempvar maxdecv
	tempvar collapselevv
	
	*ft_type
	**************
	genvars2, input(`ft_type') gen(`ft_typev')
	
	//list varname `ft_typev'
	
	*defaults:
	qui replace `ft_typev'="nlev (perc%)" if vtype=="cat" & missing(`ft_typev')
	qui replace `ft_typev'="mean (sd)" if vtype=="conti" & missing(`ft_typev')
	qui replace `ft_typev'="nevents (etime)" if vtype=="count" & missing(`ft_typev')
	qui replace `ft_typev'="nfails (stime)" if vtype=="tte" & missing(`ft_typev')
	
	qui replace `ft_typev'="" if vtype=="cat1"
	
	local mtypemax: word count `desclist'
	
	local wvlist
	forvalues j =1/`mtypemax' {
		tempvar w`j'
		qui gen `w`j''=""
		local wvlist `wvlist' `w`j''
	}
	
	tempvar ft_typev_helper
	qui gen `ft_typev_helper'=`ft_typev'

	forvalues j=1/`mtypemax' {
	
		tempvar ipos 
		qui gen `ipos' =  strlen(`ft_typev_helper')
		foreach mtype of local desclist {
				tempvar sph
				qui  gen `sph'= strpos(`ft_typev_helper',"`mtype'")
				qui replace `w`j'' = "`mtype'" if `sph'>0 & `sph'<`ipos'
				qui replace `ipos' = `sph' if `sph'>0 & `sph'<`ipos'
				qui drop `sph'
			}
		qui replace `ft_typev_helper'=subinstr(`ft_typev_helper',`w`j'',"",1)
		qui drop `ipos'	
	}
	
	
	//list varname `ft_typev' `wvlist'
	//list varname `ft_typev' `w1' `w2' `w3'
	
	*generate ft from ft_type
	qui gen `ftv' = ""
	qui replace `ftv' = `ft_typev'
	
	//if any % signs: remove
	qui replace `ftv' = subinstr(`ftv',"%","`rp'",.)
	
	foreach m of local desclist {
		qui replace `ftv' = subinstr(`ftv',"`m'","%",.)
	}
	
	//list varname `ft_typev' `ftv' `w1' `w2' `w3'
	
	
	*number of non-missing w's
	*********************************
	tempvar rnm
	qui egen `rnm' = rownonmiss(`wvlist'), strok
	//list `rnm'
	qui sum `rnm'
	local mtypemax `r(max)'
	local wvlist
	forvalues j =1/`mtypemax' {
		local wvlist `wvlist' `w`j''
	}
	
	
	*digits
	****************
	genvars2, input(`digits') gen(`digitsv') nostring addmeas(miss)

	//list varname `digitsv'
	
	genvars2, input(`digits_type') gen(`digits_typev') addmeas(miss)
	
	//if only one word
	local dwc: word count `digits_type'
	if `dwc'==1 {
		qui replace  `digits_typev' = "`digits_type'" if missing(`digits_typev')
		local digits_type_miss `digits_type'
	}
	
	genvars2, input(`maxdec') gen(`maxdecv') nostring addmeas(miss)
	local dwc: word count `maxdec'
	if `dwc'==1 {
		qui replace  `maxdecv' = `maxdec' if missing(`maxdecv')
		local maxdec_miss `maxdec' 
	}
	
	//list varname `digits_typev'
	
	*defaults 
	//sigidig if digits type not given:
	qui replace `digits_typev' = "sigdig" if missing(`digits_typev') & missing(`digitsv')
	
	//if digitsv is decimals, use defaults from formatting	
	qui replace `digitsv' = . if  missing(`digitsv') & `digits_typev' == "decimals"
	
	//defaults for maxdec via formatting


	*format and digits for missing
	********************************
	//ft_cat?
	local digits_miss
	if strpos("`digits'","miss")>0 {
		local wscat = substr("`digits'",strpos("`digits'","miss"),.)
		local digits_miss: word 2 of `wscat'
	}
	else {
		if strpos("`digits'","cat")>0 {
			local wscat = substr("`digits'",strpos("`digits'","cat"),.)
			local digits_miss: word 2 of `wscat'
		}
		else {
			qui sum `digitsv' if vtype=="cat",d
			if `r(N)'!=0 {
				local digits_miss = r(p50)
			}	
		}
	}	
	
	if  "`digits_type'" == "" {
		if "`digits_miss'"=="" {
			local digits_type_miss sigdig
		}
		else {
			local digits_type_miss decimals
		}
	}
	else {
		if "`digits_type_miss'"=="" {	
			if strpos("`digits_type'","miss")>0 {
				local wscat = substr("`digits_type'",strpos("`digits_type'","miss"),.)
				local digits_type_miss: word 2 of `wscat'
			}
			else {
				if strpos("`digits_type'","cat")>0 {
					local wscat = substr("`digits_type'",strpos("`digits_type'","cat"),.)
					local digits_type_miss: word 2 of `wscat'
				}
				else {
					local digits_type_miss
				}
			}	
		}	
	}
	
	
	if  "`maxdec_miss'" == "" {
		if strpos("`maxdec'","miss")>0 { 
			local wscat = substr("`maxdec'",strpos("`maxdec'","miss"),.)
			local maxdec_miss: word 2 of `wscat'
		}
		else {
			if strpos("`maxdec'","cat")>0 {
				local wscat = substr("`maxdec'",strpos("`maxdec'","cat"),.)
				local maxdec_miss: word 2 of `wscat'

			}
			else {
				local maxdec_miss
			}
		}	
	}
	
	//list `digitsv' `digits_typev'
	//dis "`digits_miss', `digits_type_miss', `maxdec_miss'"
	
	//default if none given 
	if "`digits_type_miss'"=="sigdig" & "`digits_miss'"=="" & "`maxdec_miss'"=="" {
		local digits_miss 2
		local maxdec_miss 2
	}
	
	
	*collapse: variables from original file
	**********
	if "`collapse'" != "" {	
		cap ds_var, dslist(`collapse') vname(varname)
		if _rc {
			dis as text "Invalid varlist in collapse --- option ignored."
			local collapse
		}
		else {
			local collapse `r(varlist)'
		}
	}


	*collapselev
	*****************
	genvars2, input(`collapselev') gen(`collapselevv') nostring
	
	//list varname `collapselevv'
	qui sum `collapselevv'
	
	if `r(N)'==0 & "`collapselev'" != "" {
		
		local dwcl: word count `collapselev'
		local dwc: word count `collapse'
		
		if `dwcl'==1 {
			qui replace  `collapselevv' = `collapselev' if missing(`collapselevv')
		}
		else {
			
			cap assert `dwcl' == `dwc'
			if _rc {
				dis as text "Option collapselev incorrectly specified --- default (2) is used."
			}
			else {
				forvalues dvi=1/`dwcl' {
				
					local dci: word `dvi' of `collapse'
					local dcli: word `dvi' of `collapselev'
					qui replace `collapselevv' = `dcli' if varname=="`dci'" & missing(`collapselevv')
		
				}
			}			
		}
	}
	
	//dis "`collapse'"
	//list varname `collapselevv'
	
	
	
	if `ngroups'>=2 {
		
		*************	
		*effect	
		***************

		tempvar ft_type_effectv
		tempvar ft_effectv
		tempvar testv
		tempvar digits_effectv
		tempvar digits_type_effectv
		tempvar maxdec_effectv

		
		*ft_type_effect
		**************
		
		genvars2, input(`ft_type_effect') gen(`ft_type_effectv')
		
		*defaults 
		if strpos("`percentage'","rd")== 0 {
			qui replace `ft_type_effectv'="rd (rd_lci to rd_uci)" if vtype=="cat" & missing(`ft_type_effectv')
		}
		else {
			qui replace `ft_type_effectv'="rd% (rd_lci to rd_uci%)" if vtype=="cat" & missing(`ft_type_effectv')
		}
		qui replace `ft_type_effectv'="meand (meand_lci - meand_uci)" ///
			if vtype=="conti" & missing(`ft_type_effectv') & strpos(`ft_typev',"mean")>0
		qui replace `ft_type_effectv'="mws (mws_lci - mws_uci)" ///
			if vtype=="conti" & missing(`ft_type_effectv') & strpos(`ft_typev',"median")>0
		qui replace `ft_type_effectv'="meand (meand_lci - meand_uci)" ///
			if vtype=="conti" & missing(`ft_type_effectv')
			
		qui replace `ft_type_effectv'="irr (irr_lci - irr_uci)" if vtype=="count" & missing(`ft_type_effectv')
		qui replace `ft_type_effectv'="hr (hr_lci - hr_uci)" if vtype=="tte" & missing(`ft_type_effectv')

		
		*generate vars to choose measure
		
		local mtypeeffmax: word count `efflist'		
		//local mtypeeffmax=3	
		
		local ewvlist	
		forvalues j =1/`mtypeeffmax' {
			tempvar ew`j'
			qui gen `ew`j''=""
			local ewvlist `ewvlist' `ew`j''
		}
		
		
		//add pe to point estimate:
		tempvar ft_typev_helper
		qui gen `ft_typev_helper' = `ft_type_effectv'

		forvalues j=1/`mtypeeffmax' {
		
			tempvar ipos 
			qui gen `ipos' =  strlen(`ft_typev_helper')
			foreach mtype of local efflist {
					tempvar sph
					qui  gen `sph'= strpos(`ft_typev_helper',"`mtype'")
					qui replace `ew`j'' = "`mtype'" if `sph'>0 & `sph'<`ipos'
					qui replace `ipos' = `sph' if `sph'>0 & `sph'<`ipos'
					qui drop `sph'
				}
			qui replace `ft_typev_helper'=subinstr(`ft_typev_helper',`ew`j'',"",1)	
			qui drop `ipos'	
		}

		
		//list varname `ewvlist'
		//list varname `ft_type_effectv' `ew1' `ew2' `ew3'
		
		*generate format  from ft_type_effect
		qui gen `ft_effectv' = ""
		qui replace `ft_effectv' = `ft_type_effectv'
		
		//if any % signs: remove
		qui replace `ft_effectv'  = subinstr(`ft_effectv' ,"%","`rp'",.)
		
		foreach m of local efflist {
			qui replace `ft_effectv' = subinstr(`ft_effectv',"`m'","%",.)
		}
		
		//not for cat1:
		qui replace `ft_effectv'="" if vtype=="cat1"
		
		//list `ft_effectv'
		
		*defaults if missing
		qui replace `ft_effectv' =  "% (% to %)" if missing(`ft_effectv') & vtype!="cat1"
		
		
		*percentages for rd and/or mws
		//if "`percentage'" != "" {
		//	if strpos("`percentage'","rd")> 0 {
		//		qui replace `ft_effectv' =  "%% (% to %%)" if missing(`ft_effectv') & `type_effectv'=="rd"
		//	}
		//	if strpos("`percentage'","mws")> 0 {
		//		qui replace `ft_effectv' =  "%% (% to %%)" if missing(`ft_effectv') & `type_effectv'=="mws"
		//	}
		//}
		
		
		*reduce number of  ew's to non-missing
		*********************************
		tempvar rnm
		qui egen `rnm' = rownonmiss(`ewvlist'), strok
		qui sum `rnm'
		local mtypeeffmax `r(max)'
		local ewvlist
		forvalues j =1/`mtypeeffmax' {
			local ewvlist `ewvlist' `ew`j''
		}
		

		*digits_effect
		genvars2, input(`digits_effect') gen(`digits_effectv') nostring
		//from digits if missing 
		qui replace `digits_effectv' = `digitsv' if missing(`digits_effectv')
		
		genvars2, input(`digits_type_effect') gen(`digits_type_effectv')
		//list varname `digits_type_effectv'
		
		
		//if only one word
		local edwc: word count `digits_type_effect'
		if `edwc'==1 {
			qui replace  `digits_type_effectv' = "`digits_type_effect'" if missing(`digits_type_effectv')
		}
		//from digits_type if missing 
		qui replace `digits_type_effectv' = `digits_typev' if missing(`digits_type_effectv')
		
	
		genvars2, input(`maxdec_effect') gen(`maxdec_effectv') nostring
		local dwc: word count `maxdec_effect'
		if `dwc'==1 {
			qui replace `maxdec_effectv' = `maxdec_effect' if missing(`maxdec_effectv')
		}
		//from digits_type if missing 
		qui replace `maxdec_effectv' = `maxdecv' if missing(`maxdec_effectv')
		
		//list varname vtype `digits_type_effectv' `maxdec_effectv'
		
		*default
		//use default from formatting4
		
		*test
		**********
		genvars2, gen(`testv') input(`test')
		
		*defaults if missing
		
		//conti:
		//ttest/anova if mean difference as effect:
		qui replace `testv' = "ttest" if missing(`testv') & `ngroups'==2 & strpos(`ft_type_effectv',"meand")>0
		qui replace `testv' = "anova" if missing(`testv') & `ngroups'>2  & strpos(`ft_type_effectv',"meand")>0 

		//ranksum/kwallis if mws or hlmd as effect
		qui replace `testv' = "ranksum" if missing(`testv') & `ngroups'==2 & ///
			(strpos(`ft_type_effectv',"mws")>0 | strpos(`ft_type_effectv',"hlmd")>0) 
		qui replace `testv' = "kwallis"  if missing(`testv') & `ngroups'>2 & ///
			(strpos(`ft_type_effectv',"mws")>0 | strpos(`ft_type_effectv',"hlmd")>0) 
		
		//qreg if median difference as effect
		qui replace `testv' = "qreg"    if missing(`testv') & `ngroups'==2 & strpos(`ft_type_effectv',"medd")>0
		qui replace `testv' = "kwallis" if missing(`testv') & `ngroups'>2  & strpos(`ft_type_effectv',"medd")>0
		
		//ttest/anova if no effect but mean as descriptive
		qui replace `testv' = "ttest" if missing(`testv') & `ngroups'==2 & vtype=="conti" ///
			& strpos(`ft_typev',"mean")>0	
		qui replace `testv' = "anova" if missing(`testv') & `ngroups'>2  & vtype=="conti" ///
			& strpos(`ft_typev',"mean")>0
		
		//ranksum/kwallis if no effect but median as descriptive
		qui replace `testv' = "ranksum" if missing(`testv') & `ngroups'==2 & vtype=="conti" ///
			& strpos(`ft_typev',"median")>0	
		qui replace `testv' = "kwallis" if missing(`testv') & `ngroups'>2  & vtype=="conti" ///
			& strpos(`ft_typev',"median")>0
		
		//ttest/anova if nothing given
		qui replace `testv' = "ttest" if missing(`testv') & `ngroups'==2 & vtype=="conti"
		qui replace `testv' = "anova" if missing(`testv') & `ngroups'>2  & vtype=="conti"
		
		//cat:
		qui replace `testv' = "`default_cat'" if missing(`testv') & vtype=="cat"
		
		//count:
		qui replace `testv' = "poisson" if missing(`testv') & vtype=="count"
		
		//tte:
		qui replace `testv' = "logrank" if missing(`testv')  & vtype=="tte" 
		
		
		*test need to be defined for cat1, same as below
		qui replace `testv'=`testv'[_n+1] if varname==varname[_n+1] & vtype=="cat1"
		qui assert `testv'==`testv'[_n+1] if varname==varname[_n+1] 
		
	}
	
	if `ngroups'>2 {
		qui replace `testv' = "anova" if `testv'=="ttest"
		qui replace `testv' = "kwallis" if `testv'=="ranksum"
	}
	
	//list varname `testv'
	
	//list varname `ft_effectv' `type_effectv' `testv' `ew1'
	// here
	
	*format
	*******************

	foreach var of varlist ntot* {
			
		local pos=strpos("`var'","_")+1
		local na=substr("`var'",`pos',.)
		
		//perc
		tempvar perc_`na' perclci_`na' percuci_`na'
		qui cap gen perc_`na'= 100 * pr_`na'
		qui cap gen perclci_`na'= 100 * prlci_`na'
		qui cap gen percuci_`na'= 100 * pruci_`na'
				
		local i=0
		local vuselist
		tempvar intpos
		qui gen `intpos'=""
		
		foreach v of varlist `wvlist' {
			
			local i=`i' + 1
			
			tempvar `v'g
			qui gen ``v'g' = `v'
			qui replace ``v'g' = subinstr(``v'g',"median","p50",.)
			qui replace ``v'g' = subinstr(``v'g',"lq","p25",.)
			qui replace ``v'g' = subinstr(``v'g',"uq","p75",.)	
			qui replace ``v'g' = subinstr(``v'g',"prop","pr",.)
			
			if "`denom'"=="nonmiss" {
				qui replace ``v'g' = subinstr(``v'g',"ntot","nnonmiss",.)
			}
			
			qui replace ``v'g' = ``v'g' + "_" +  "`na'" if  !missing(``v'g')	

			//list ``v'g'
			qui replace `intpos'=`intpos' + "`i' " if inlist(``v'g',"nlev_`na'","ntot_`na'","nevents_`na'","nfails_`na'")
			
			tempvar vuse`i'
			qui gen `vuse`i'' = .
			qui ds
			local varlist `r(varlist)'
			foreach l of local varlist {
				tempvar helper
				qui gen `helper'=1 if ``v'g'=="`l'"
				qui sum `helper'
				if `r(N)'!=0 {
					qui replace `vuse`i'' = `l' if ``v'g' == "`l'"
				}
				qui drop `helper'
			}	
			//list ``v'g' `vuse`i''
			local vuselist `vuselist' `vuse`i''
		}
	
		tempvar nomiss
		qui egen `nomiss' = rowmiss(`vuselist')
			
		//list varname `vuselist' `ftv' vtype `intpos' `digitsv'
		
		qui formatting4 `vuselist' if `nomiss'!=`mtypemax', gen(out_`na'`genadd') ///  
					ft(`ftv') digits(`digitsv') digits_type(`digits_typev') maxdec(`maxdecv') ///
					type(vtype) intpos(`intpos') catdigits0(`catdigits0')		
		cap drop perc*
		
		qui replace out_`na'`genadd' = subinstr(out_`na'`genadd',"`a'","%",.)
		
		//list varname levlabel out_`na'`genadd' `digitsv'
		//list varname `vuse1' `vuse2' `vuse3' out_`na' `ftv' `typev'
	}
	
	
	if `ngroups'==2 {
				
		local i=0
		local vuselist
		
		//list varname `ewvlist'
		foreach v of varlist `ewvlist' {
			
			local i=`i' + 1
			
			tempvar vuse`i'
			qui gen `vuse`i'' = .

			qui ds
			local varlist `r(varlist)'
			foreach l of local varlist {
				tempvar helper
				qui gen `helper'=1 if `v'=="`l'"
				qui sum `helper'
				if `r(N)'!=0 {
					qui replace `vuse`i'' = `l' if `v'=="`l'"
				}
				qui drop `helper'
			}
			local vuselist `vuselist' `vuse`i''
		}	
		
		tempvar nomiss
		qui egen `nomiss' = rowmiss(`vuselist')
		
		//list varname `digits_effectv' `digits_type_effectv'
		
		qui formatting4 `vuselist' if `nomiss' != 3, gen(out_d`genadd')  ///
				ft(`ft_effectv') digits(`digits_effectv') ///  type(`type_effectv')
				digits_type(`digits_type_effectv') maxdec(`maxdec_effectv') catdigits0(`catdigits0')		
		
		//list varname `vuselist' out_d`genadd' `digits_effectv' `maxdecv' `digits_type_effectv' 
	}
	//list varname `maxdecv' out_d`genadd'
	
	*p-values
	*********

	if `ngroups'>1 {
	
		local i=0
		
		tempvar vuse
		qui gen `vuse' = .

		tempvar vg
		qui gen `vg' = `testv'
		qui replace `vg' =  "p_" + `testv' ///
			if inlist(`testv',"ttest","ranksum","qreg","fisher","chi2","anova","kwallis") | ///
				inlist(`testv',"poisson","nbreg","logrank","cox","rmstd")
		
		//list varname `testv' `vg'
		
		qui ds
		local varlist `r(varlist)'
		foreach l of local varlist {
			tempvar helper
			qui gen `helper'=1 if `vg'=="`l'"
			qui sum `helper'
			if `r(N)'!=0 {
				qui replace `vuse' = `l' if `vg'=="`l'"
			}
			qui drop `helper'
		}
		
		format_p `vuse', gen(pv`genadd') format("`p_format'") digits("`p_digits'") breaks("`p_breaks'")
		
		//list varname `vuse' pv`genadd'
		
		//if "`p_all'"=="" { 
		//	qui replace pv`genadd'="" if vtype=="cat"
		//}
			 
	}
	

	//if categorical variables without any subcategories, no generation of out, error below
	cap assert !missing(out_t`genadd')
	if _rc {
			cap qui gen out_t`genadd'=""
		forvalue ngs=1/`ngroups' {
			cap qui gen out_`ngs'`genadd'=""
		}
		if `ngroups'==2 {
			cap qui gen out_d`genadd'=""
		}
	}		
	
	*prepare columns and labels 
	***********	

	*title for descriptives
	
	tempvar labels
	qui gen `labels' = `ftv'
	qui replace `labels' = subinstr(`labels',"%%","%`a'",.)
	
	forvalues j=1/`mtypemax' {
		qui replace `labels' = subinstr(`labels',"%",`w`j'',1)
	}
	
	//list varname `labels'
	qui replace `labels' = subinstr(`labels',"`a'","%",.)
	qui replace `labels' = subinstr(`labels',"`rp'","%",.)
	qui replace `labels' = subinstr(`labels',"nlev","n",.) if vtype=="cat"
	qui replace `labels' = subinstr(`labels',"ntot","N",.) if vtype=="cat"
	qui replace `labels' = subinstr(`labels',"nevents","n",.) if vtype=="count"
	qui replace `labels' = subinstr(`labels',"etime","person-time",.) if vtype=="count"
	qui replace `labels' = subinstr(`labels',"nfails","failures",.) if vtype=="tte"
	qui replace `labels' = subinstr(`labels',"stime","person-time at risk",.) if vtype=="tte"
	
	//don't show %-sign:
	qui replace `labels' = subinstr(`labels',"%","",.) if vtype=="cat"
	
	//standardize ci label type=mean_ci
	local cimeas mean perc prop pr ir st50 rmst
	foreach cim of local cimeas {
		tempvar sp1 sp2
		qui gen `sp1'=strpos(`labels',"`cim'lci")
		qui gen `sp2'=strpos(`labels',"`cim'uci")
		local sli=strlen("`cim'_uci")
		qui replace `labels' = subinstr(`labels',substr(`labels',`sp1',`sp2'-`sp1'+`sli'-1),"`cilev' CI",1) ///
			if `sp1'>0 & `sp2'>0
	}
	
	qui replace `labels' = subinstr(`labels',"perc ","%",.) if vtype=="cat" & strpos(`labels',"CI")==0
	qui replace `labels' = subinstr(`labels',"perc","%",.) if vtype=="cat"  & strpos(`labels',"CI")==0
	
	//full name for percentage and proportion
	qui replace `labels' = subinstr(`labels',"perc","percentage",.) if vtype=="cat"
	qui replace `labels' = subinstr(`labels',"prop","proportion",.) if vtype=="cat"
	qui replace `labels' = subinstr(`labels',"pr","proportion",.) if vtype=="cat" & strpos(`labels',"proportion")==0
	qui replace `labels' = subinstr(`labels',"ir","incidence",.) if vtype=="count"
	//list varname `labels' vtype
	qui replace `labels' = subinstr(`labels',"st50","median survival time",.) if vtype=="tte"
	qui replace `labels' = subinstr(`labels',"st25","lq",.) if vtype=="tte"
	qui replace `labels' = subinstr(`labels',"st75","uq",.) if vtype=="tte"
	qui replace `labels' = subinstr(`labels',"rmst","restricted mean survival time",.) if vtype=="tte"
	
	*title effect
	//list varname `ft_type_effectv' `ft_effectv' `ew1' `ew2' `ew3'
	
	if `ngroups'==2 {
		tempvar labels_effect
		tempvar labels_effect_abbr


		tempvar labels_effect1
		qui gen `labels_effect1' = `ft_effectv'
		qui replace `labels_effect1' = subinstr(`labels_effect1',"%%","%`a'",.)
		
		forvalues j=1/`mtypeeffmax' {
			qui replace `labels_effect1' = subinstr(`labels_effect1',"%",`ew`j'',1)
		}
		
		//list varname `labels'
		qui replace `labels_effect1' = subinstr(`labels_effect1',"`a'","%",.)
		qui replace `labels_effect1' = subinstr(`labels_effect1',"`rp'","%",.)
		
		//don't show %-sign:
		qui replace `labels_effect1' = subinstr(`labels_effect1'," %","",.)
		qui replace `labels_effect1' = subinstr(`labels_effect1',"%","",.)
		
		//standardize ci label type=mean_ci
		local cimeas meand medd hlmd mws ird irr rd rr or hr rmstd user
		foreach cim of local cimeas {
			tempvar sp1 sp2
			qui gen `sp1'=strpos(`labels_effect1',"`cim'_lci")
			qui gen `sp2'=strpos(`labels_effect1',"`cim'_uci")
			local sli=strlen("`cim'_uci")
			qui replace `labels_effect1' = subinstr(`labels_effect1', ///
				substr(`labels_effect1',`sp1',`sp2'-`sp1'+`sli'),"`cilev' CI",1) ///
				if `sp1'>0 & `sp2'>0
		}
		qui gen `labels_effect' = `labels_effect1'
		qui gen `labels_effect_abbr' = `labels_effect1'
		
		//list `labels_effect'
		
		qui replace `labels_effect' = subinstr(`labels_effect',"meand","mean difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"meand","MD",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"medd","median difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"medd","MedD",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"hlmd","Hodges-Lehmann median difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"hlmd","HLMD",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"mws","Mann-Whitney statistic",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"mws","MWS",.)
        
		qui replace `labels_effect' = subinstr(`labels_effect',"ird","incidence rate difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"ird","IRD",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"irr","incidence rate ratio",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"irr","IRR",.)
		
		qui replace `labels_effect' = subinstr(`labels_effect',"rd","risk difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"rd","RD",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"rr","risk ratio",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"rr","RR",.)
		qui replace `labels_effect' = subinstr(`labels_effect',"or","odds ratio",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"or","OR",.)
		
		qui replace `labels_effect' = subinstr(`labels_effect',"hr","hazard ratio",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"hr","HR",.)	
		qui replace `labels_effect' = subinstr(`labels_effect',"rmstd","restricted mean survival time difference",.)
		qui replace `labels_effect_abbr' = subinstr(`labels_effect_abbr',"rmstd","RMSTD",.)	

	}
	
	
	*generate helper variables
	************************
	
	*helper desc
	qui gen desc_info`genadd' = `labels'
	
	//exposure time
	cap ds etime_unit
	if !_rc {
		qui replace desc_info`genadd' = ///
			subinstr( desc_info`genadd',"person-time","person-" + etime_unit,.) if vtype=="count"
		
		//activate to include time unit in incidence	
		//qui replace desc_info`genadd' = ///
		//	subinstr( desc_info`genadd',"incidence","incidence per person-" + etime_unit,.) if vtype=="count"
	}
	
	//survival time
	cap ds stime_unit
	if !_rc {
		
		//activate to include units.
		qui replace desc_info`genadd' = ///
			subinstr( desc_info`genadd',"person-time","person-" + stime_unit,.) if vtype=="tte"
		//qui replace desc_info`genadd' = ///
		//	subinstr( desc_info`genadd',"median survival time","median survival time (" + stime_unit + ")",.) ///
		//	if vtype=="tte"
		//qui replace desc_info`genadd' = ///
		//	subinstr( desc_info`genadd',"restricted mean survival time","restricted mean survival time (" + stime_unit + ")",.)	///
		//	if vtype=="tte"
	}
	
	
	*helper effect
	if `ngroups'==2 {
		cap gen effect_info`genadd' = `labels_effect'
		//cap gen effect_info`genadd' = subinstr(`labels_effect'," (`cilev' CI)","",.)
		if !_rc {
			qui replace effect_info`genadd' = `labels_effect'
			//qui replace effect_info`genadd' = subinstr(`labels_effect'," (`cilev' CI)","",.)
		}
		
		if "`abbreviation'"!="" {
			cap gen abbr_effect_info`genadd' = `labels_effect_abbr'
			//cap gen abbr_effect_info`genadd' = subinstr(`labels_effect_abbr'," (`cilev' CI)","",.)
			if !_rc {
				qui replace abbr_effect_info`genadd' = `labels_effect_abbr'
				//qui replace abbr_effect_info`genadd' = subinstr(`labels_effect_abbr'," (`cilev' CI)","",.)
			}
		}
	}

	*helper test
	if `ngroups'>=1 {
		cap gen test_info`genadd' = `testv'
		if !_rc {
			qui replace test_info`genadd' = `testv'
		
			if "`p_all'"=="" { 
				qui replace test_info`genadd' = "" if vtype=="cat"
			}
	
			qui replace test_info`genadd' = "chi-squared test" if test_info`genadd'=="chi2"
			qui replace test_info`genadd' = "Fisher's exact test" if test_info`genadd'=="fisher"
			qui replace test_info`genadd' = "Student's t-test" if test_info`genadd'=="ttest"
			qui replace test_info`genadd' = "Wilcoxon-Mann-Whitney test" if test_info`genadd'=="ranksum"
			if inlist("`lbpoisson'","","exact") {
				qui replace test_info`genadd' = "Poisson exact test" if test_info`genadd'=="poisson"
			}
			if "`lbpoisson'"=="oim" {
				qui replace test_info`genadd' = "Wald-type from Poisson regression" if test_info`genadd'=="poisson"
			}
			if "`lbpoisson'"=="robust" {
				qui replace test_info`genadd' = "robust from Poisson regression" if test_info`genadd'=="poisson"
			}
			qui replace test_info`genadd' = "Wald-type from negative binomial" if test_info`genadd'=="nbreg"
			qui replace test_info`genadd' = "log-rank test" if test_info`genadd'=="logrank"
			qui replace test_info`genadd' = "Wald-type from Cox model" if test_info`genadd'=="cox"
			qui replace test_info`genadd' = "Wald-type from restricted mean survival time difference" if test_info`genadd'=="rmstd"
			qui replace test_info`genadd' = "Kruskal-Wallis test" if test_info`genadd'=="kwallis"
			qui replace test_info`genadd' = "ANOVA" if test_info`genadd'=="anova"
		}
	}
	
	*replace if descriptive not mentioned 
	//qui levelsof varname, local(vlev)
	//local vlev conti cat count tte `addmeas' `vlev'
	//local inspec: list ft_type2 & vlev
	//dis "`inspec'"
	
}

*remove variables if n==0
************************************************
foreach var of varlist ntot* {
	local pos=strpos("`var'","_")+1
	local na=substr("`var'",`pos',.)
	if "`denom'"=="nonmiss" {
		qui replace out_`na' = "" if nnonmiss_`na'==0
	}
	else {
		qui replace out_`na' = "" if ntot_`na'==0
	}
}

	
*ncolumn
************	
if `ngroups'<=1 {  		//no diff, no pv
	local nt=ntot_t[1]
}
	
local ord
foreach var of varlist ntot* {
	local pos=strpos("`var'","_")+1
	local na=substr("`var'",`pos',.)
	qui gen ns_`na'=""
	qui replace ns_`na'=string(nnonmiss_`na') if inlist(vtype,"cat1","conti","count","tte")
}

*inset
***********
qui replace levlabel="`inset'" + levlabel if vtype=="cat"
qui replace levlabel=varlabel if inlist(vtype,"cat1","conti","count","tte")
qui replace levlabel=varname if missing(levlabel) & inlist(vtype,"cat1","conti","count","tte")

		
	
*collapse categorical variables if one or two categories
*********************************

//list varname vtype levlabel nlev out_d*

if "`collapse'" != "" {
	
	collapse_cat, collapse(`collapse') collapselevv(`collapselevv') design(`design')

	if "`nobracket'"!="" {
		foreach db of local nobracket {
			qui replace levlabel=subinstr(levlabel," (`db')","",.)
		}
	}
	
	cap drop seq	

}


*p-values for each level of catgorical variables
*****************************************

if "`p_all'"=="" { 
	cap ds pv*
	if !_rc {
		foreach var of varlist pv* {
			qui replace `var'="" if vtype=="cat"
		}	
	}	
}
		

*design row or column or missing
**************************
//on level of locals
//apply_design, design(`design') nrows(`nrows') inset_row("`inset_row'") inset("`inset'")

//on level of variables
//on level of vars
//list varname vtype `nrowvar'
//gen nrowvar = `nrowvar'
//exit

//dis "nonmiss `nonmiss'"
//dis "design `nonmiss'"
//dis "denom `denom'"

apply_design_var, nrowvar(`nrowvar') design(`design') inset_row("`inset_row'") inset("`inset'") ///
	nonmiss(`nonmiss') varname_sp(`varname_sp') denom(`denom') ///
	namemiss(`namemiss') digits_miss(`digits_miss') digits_type_miss(`digits_type_miss') maxdec_miss(`maxdec_miss')
	
	
*headers
******************
tempvar desc_info2
qui gen `desc_info2' = desc_info
cap ds etime_unit
if !_rc  {
	qui levelsof etime_unit, local(eulev)
	local neulev: word count `eulev'
	if `neulev'!=1 {
		qui replace `desc_info2' = subinstr(`desc_info2',"person-" + etime_unit,"person-time",.)
		qui replace `desc_info2' = subinstr(`desc_info2',"incidence per person-" + etime_unit,"incidence",.)
	}
}
cap ds stime_unit
if !_rc  {
	qui levelsof stime_unit, local(eulev)
	local neulev: word count `eulev'
	if `neulev'!=1 {
		qui replace `desc_info2' = subinstr(`desc_info2',"person-" + stime_unit,"person-time",.)
		qui replace `desc_info2' = ///
			subinstr(`desc_info2',"median survival time (" + stime_unit + ")","median survival time",.) 
		qui replace `desc_info2' = ///
			subinstr(`desc_info2',"restricted mean survival time (" + stime_unit + ")","restricted mean survival time",.)
	}
}

*descriptive header
local titledesc
//qui levelsof desc_info, local(levdesc)
qui levelsof `desc_info2', local(levdesc)
local lwc: word count `levdesc'
forvalues j=1/`lwc' {
	local wj: word `j' of `levdesc'
	if `j'==1 {
		local titledesc `wj'
	}
	if !inlist(`j',1,`lwc') {
		local titledesc `titledesc', `wj'
	}
	if `j'==`lwc' & `j' !=1 {
		local titledesc `titledesc' or `wj'
	}
}
	
*column with n

if "`ncol'" != "" {

	local insl 1
	empty_line in 1, position("before")
	if "`nrow'"=="" {
		qui replace ns_t="`nametot' (N = `nt')" in 1
		qui replace out_t="`nametot' (N = `nt')" in 1
		local nkeep ns_t
		
		if `ngroups'>=1 {
			forvalues i=1/`ngroups' {
				qui replace ns_`i'="`lb`i'' (N = `nt`i'')" in 1
				qui replace out_`i'="`lb`i'' (N = `nt`i'')" in 1
				local nkeep `nkeep' ns_`i'
			}
		}	
	}
	else {
		empty_line in 1, position("before")
		local insl 2
		qui replace ns_t="`nametot'" in 1
		qui replace out_t="`nametot'" in 1
		qui replace ns_t="(N = `nt')" in 2
		qui replace out_t="(N = `nt')" in 2
		local nkeep ns_t
		
		if `ngroups'>=1 {
			forvalues i=1/`ngroups' {
				qui replace ns_`i'="`lb`i''" in 1
				qui replace out_`i'="`lb`i''" in 1
				qui replace ns_`i'="(N = `nt`i'')" in 2
				qui replace out_`i'="(N = `nt`i'')" in 2
				local nkeep `nkeep' ns_`i'
			}
		}		
	}
	
	if inlist("`design'","","column","wide") {
		empty_line in `insl', position("after")
		foreach var of varlist ns* {
			qui replace `var'="`ncol'" in `r(insert_line)'
		}
		foreach var of varlist out* {
				qui replace `var'="`titledesc'" in `r(insert_line)'
		}
	}
}

*column without n	
else {

	drop ns*
	
	local nkeep
	local insl 1
	empty_line in 1, position("before")
	if "`nrow'"=="" {
		qui replace out_t="`nametot' (N = `nt')" in 1
		if `ngroups'>=1 {
			forvalues i=1/`ngroups' {
				qui replace out_`i'="`lb`i'' (N = `nt`i'')" in 1
			}
		}
	} 
	else {
		empty_line in 1, position("before")
		local insl 2
		qui replace out_t="`nametot'" in 1
		qui replace out_t="(N = `nt')" in 2
		
		if `ngroups'>=1 {
			forvalues i=1/`ngroups' {
				qui replace out_`i'="`lb`i''" in 1
				qui replace out_`i'="(N = `nt`i'')" in 2
			}
		}	
	}
	
	if inlist("`design'","","column","wide") {
		empty_line in `insl', position("after") 
		qui replace out_t="`titledesc'" in `r(insert_line)'
		if `ngroups'>=1 {
			forvalues i=1/`ngroups' {
				qui replace out_`i'="`titledesc'" in `r(insert_line)'
			}
		}
	}
}

*diff 
if `ngroups'==2 {
		
	if "`abbreviation'"=="" {
		qui levelsof effect_info, local(leveffect)
	}
	else {
		qui levelsof abbr_effect_info, local(leveffect)
	}

	local titleeffect

	local lwc: word count `leveffect'
	
	forvalues j=1/`lwc' {
		local wj: word `j' of `leveffect'
		local wj = subinstr("`wj'"," (`cilev' CI)","",.)
	
		if `j'==1 {
			local titleeffect `wj'
		}
		if !inlist(`j',1,`lwc')  {
			local titleeffect `titleeffect', `wj'
		}
		if `j'==`lwc' & `j'!=1 {
			local titleeffect `titleeffect' or `wj'
		}
	}
	
	local titleeffect = strupper(substr("`titleeffect'",1,1)) + substr("`titleeffect'",2,.)

	if "`abbreviation'"!="" {
		
		local titleeffect_leg = "*" + "`titleeffect'"
		local titleeffect_leg = subinstr("`titleeffect_leg'"," or `wj'",", `wj'",1)
		local titleeffect = "`titleeffect'" + "*" 
		local titleeffect_leg = subinstr("`titleeffect_leg'"," MD,"," MD: mean difference,",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","*MD,","*MD: mean difference,",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","MedD","MedD: median difference",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","HLMD","HLMD: Hodges-Lehmann difference",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","MWS","MWS: Mann-Whitney statistic",.)
				
		tempvar ab
		local titleeffect_leg = subinstr("`titleeffect_leg'","IRD","D`ab': incidence rate difference",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","IRR","R`ab': incidence rate ratio",.)
		
		local titleeffect_leg = subinstr("`titleeffect_leg'","RD","RD: risk difference",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","RR","RR: risk ratio",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","OR","OR: odds ratio",.)

		local titleeffect_leg = subinstr("`titleeffect_leg'","D`ab'","IRD",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'","R`ab'","IRR",.)
		local titleeffect_leg = subinstr("`titleeffect_leg'",", user","",.)
		
		local titleeffect_leg = strupper(substr("`titleeffect_leg'",1,1)) + substr("`titleeffect_leg'",2,.)
	}
	
	
	qui replace out_d="`titleeffect' (`cilev' CI)" in 1
	//qui replace out_d=subinstr("`titleeffect'"," (`cilev' CI)","",.) + " (`cilev' CI)" in 1

	
	if inlist("`design'","","column","wide") {
		qui replace out_d="" in 2
	}
}	

*pval
if `ngroups'>1 {	
	qui replace pv="P-value" in 1
	if inlist("`design'","","column","wide") {
		qui replace pv="" in 2
	}
}	


*helper	
qui replace desc_info = "Descriptives" in 1
cap replace effect_info = "Effects" in 1
cap replace abbr_effect_info = "Effects" in 1
cap replace test_info = "Tests" in 1
	

*Footer: explanation for abbr at bottom
*******************************
if "`abbreviation'"!="" {
	qui count
	qui empty_line in `r(N)'
	qui replace levlabel = "`titleeffect_leg'" in `r(insert_line)'
	//qui replace levlabel = subinstr("`titleeffect_leg'"," (`cilev' CI)","",.) in `r(insert_line)'
}

*blocking
*************

//defaults
if "`block_parse'"=="" {
	local block_parse "|"
}
if "`block_inset'"=="" {
	local block_inset "    "
}
if "`block_inset'"=="none" {
	local block_inset ""
}

if "`block'" != "" {
	
	//check that block contain only valid varnames or parsing character
	local blockcheck = 1
	local blockm = subinstr("`block'","`block_parse'"," ",.)
	tempname v
	foreach `v' of local blockm {
		if "``v''" != "`block_parse'" {
			cap ds_var, dslist(``v'') vname(varname)
			if _rc {
				dis as text "``v'' not found among varname or parsing character (given by block_parse), blocking ignored"
				local blockcheck = 0
			}
		}	
	}

	cap assert "`block_head'" != "" 
	if _rc {
		dis as text "Block headers not given, blocking ignored"
		local blockcheck = 0
	}
		
	tempname bcount bcount1 bcount2		
	tempname a
	local `a': subinstr local block "`block_parse'" "`block_parse'", all count(local `bcount')
	local `bcount1' = ``bcount'' + 1 
	tempname a
	local `a': subinstr local block_head "`block_parse'" "`block_parse'", all count(local `bcount')
	local `bcount2' = ``bcount'' + 1 
	cap assert ``bcount1'' == ``bcount2''
	if _rc {
		dis as text "The number of blocks does not match the number of header, blocking ignored"
		local blockcheck = 0
	}
		
		
	if `blockcheck' == 1 {	
					
		tknz "`block'", parse("`block_parse'") nochar stub(var)
		tknz "`block_head'", parse("`block_parse'") nochar stub(head)

		forvalues i = 1/``bcount1'' {
			tempname bvlist
			tempvar seq
			qui gen `seq' = _n
			
			ds_var, dslist(`var`i'') vname(varname)
			local `bvlist' `r(varlist)'

			
			tempname wc wl hl
			
			local `wc': word count ``bvlist''
			local `wl': word 1 of ``bvlist'' //block leader, determines position
			local `hl' `head`i''
			
			tempname lstart lend
			qui sum `seq' if varname == "``wl''" 
			local `lstart' = `r(min)'
			local `lend' = `r(max)'

			empty_line if varname == "``wl''" & `seq'==``lstart'', position(before)
			qui replace `seq' = `seq'[_n-1] + 1/10000 in `r(insert_line)'
			qui replace levlabel = "``hl''" in `r(insert_line)'
			
			qui replace levlabel = "`block_inset'" + levlabel if varname == "``wl''"				
			
			forvalues j = 2/``wc'' {
				tempname wj
				local `wj': word `j' of ``bvlist''
				tempvar seq2
				qui gen `seq2' = _n if varname == "``wj''"
				qui replace `seq' = ``lend'' + `j'/1000 + `seq2'/1000 if varname == "``wj''"
				qui replace levlabel = "`block_inset'" + levlabel if varname == "``wj''"
				qui drop `seq2'
			}
			
			sort `seq'
			qui drop `seq'

		}
	}
}


*inset for kids
*******************
cap sum kid 
if !_rc {

	tempvar inset_kidv
	qui gen `inset_kidv' = "`inset_kid'"

	if "`inset_kid'"=="" {
		qui replace `inset_kidv' = "    "
	}
	if "`inset_kid'"=="none" {
		qui replace `inset_kidv' = ""
	}

	cap sum kid 
	local kidm = `r(max)'
	if `kidm'>=2 {
		forvalues k=2/`kidm' {
			//list varname if kid==`k'
			qui replace `inset_kidv' = `inset_kidv' + `inset_kidv' if kid == `k'
		}
	}	
	
	cap replace levlabel = `inset_kidv' + levlabel if kid>=1 & !missing(kid)
	qui drop `inset_kidv'
}	


* keep and order  variables
***************************
local ord 

foreach var of varlist ntot* {
	local pos=strpos("`var'","_")+1
	local na=substr("`var'",`pos',.)
	if "`nkeep'"=="" {
		local ord `ord' out_`na'
	}
	else {
		local ord `ord' ns_`na' out_`na'
	}
}


if `ngroups'<=1 {  		//no diff, no pv
	keep levlabel `ord' desc_info
	order levlabel `ord' desc_info 
} 
if `ngroups'==2 {
	keep levlabel `ord' out_d pv *_info 
	order levlabel `ord' out_d pv *_info 
}
if `ngroups'>2 {   	//no diff
	keep levlabel `ord' pv desc_info test_info
	order levlabel `ord' pv desc_info test_info
}



*re-insert % sign in ft_type
******************************
foreach var of varlist _all {
	qui replace `var' = subinstr(`var',"`rp'","%",.)
}


*variable name
******************
cap rename levlabel variable


*drop 
**********

*drop if notest or noeffect
if "`notest'"=="yes" {
	cap qui drop pv
	cap qui drop test_info
}
if "`noeffect'"=="yes" {
	cap qui drop out_d
	cap qui drop effect_info
	cap qui drop abbr_effect_info
}


*drop if necessary	
if "`drop'"!="" {
	if strpos("`drop'","total")>0 {
		cap drop out_t
		cap drop ns_t
	}
	if strpos("`drop'","effect")>0 {
		cap drop out_d effect_info
		if "`abbreviation'"!="" & `ngroups'==2 {
			qui count
			qui drop in `r(N)'
		}
	}		
	if strpos("`drop'","test")>0 {
		cap drop pv test_info
	}
	
	if strpos("`drop'","helper")>0 | strpos("`drop'","info")>0 {
		cap drop desc_info
		cap drop effect_info
		cap drop abbr_effect_info
		cap drop test_info
	}
	
	foreach wd of local drop {
		cap drop `wd'
	}
}		

*left align
local fmt: format variable
if strpos("`fmt'","%-")==0 {
	local fmt: subinstr local fmt "%" "%-"
}
format variable `fmt'
qui compress
	
	
end


/**********************************************************************************************************************/
	

********************************************	
*function to get information from input strings
***************************************

cap program drop genvars2
program genvars2, nclass
version 16
syntax, GENerate(name) [input(string) NOSTRing addmeas(string)]

if "`nostring'" == "" { 
	cap gen `generate'=""
}
else {
	cap gen `generate'=.
}
 
if "`input'"!="" {
	
	qui levelsof varname, local(vlev)
	local vlev conti cat count tte `addmeas' `vlev'
	
	foreach vl of local vlev {
		
		local pvl: list posof "`vl'" in input
		
		if `pvl' > 0 {	
			
			local pnext = `pvl' + 1 
			local next: word `pnext' of `input'
			local nivl: list next in vlev
	
			while `nivl'==0 {
			
			
				if inlist("`vl'","conti","cat","count","tte") {
					if "`nostring'" == "" {
						qui replace `generate' = `generate' + "`next' " ///
							if vtype=="`vl'" & strpos("`input'",varname)==0
					} 
					else {
						qui replace `generate'=real("`next'") ///
							if vtype=="`vl'" & strpos("`input'",varname)==0
					}
				}
				else {
					if "`nostring'" == "" {
						qui replace `generate' = `generate' + "`next' " if varname=="`vl'"
					}
					else {
						qui replace `generate' = real("`next'") if varname=="`vl'"
					}	
				}
				
				local pnext = `pnext' + 1
				local next: word `pnext' of `input'
				local nivl: list next in vlev
				
			}
			if "`nostring'" == "" {
				qui replace `generate' = strtrim(`generate')
			}
			
		}
	}
}
end

/**********************************************************************************************************************/


******************
*function for formatting
***************	

*formatting4 based on varlists
***************************************

cap program drop formatting4
program formatting4, nclass
version 16
syntax varlist [if] [in], GENerate(name) [ft(varname) digits(varname) digits_type(varname) maxdec(varname) ///
	type(varname) intpos(varname) CATDigits0(string)]

tokenize `varlist'
marksample touse, novarlist

*one of type of format needs to be given
cap confirm variable `ft'
if _rc {
	dis as error "Specify variable ft"
	exit, clear
}

local wc: word count `varlist'
local wc2 = 0
forvalues v = 1/`wc' {
	cap assert missing(``v'') 
	if _rc {
		local wc2 = `wc2' + 1
	}
}
local wc = `wc2'

local vars ft digits_type digits maxdec 
foreach v of local vars {
	tempvar `v't
	cap confirm variable ``v''
	if _rc {
		qui gen ``v't' = .
	}
	else {
		qui gen ``v't' = ``v''
	}
}

*empty vars
cap confirm variable `intpos'
if _rc {
	tempvar intpos
	qui gen `intpos'=""
}


*check type 
cap assert `ftt' != "" if vtype!="cat1"
if _rc {
	dis as text "Entries without type and format do not give an output"
}

	
*gen output variable	
capture confirm new variable `generate'
if !_rc {
	qui gen `generate'="" if `touse'
	local newvar=1
}
else {
	local newvar=0
}


*any entries
local nme=0
forvalue i=1/`wc' {
	qui count if !missing(``i'')
	local nme = `nme' + `r(N)'
}
if `nme'==0 {
	exit, clear
}

	
*check digits_type 
cap assert inlist(`digits_typet',"","sigdig","decimals")
if _rc==9 {
	dis as text "`digits_typet' is not a valid option for digits_type, the default (decimals) is used."
	replace `digits_typet' = "decimals" if !inlist(`digits_typet',"","sigdig","decimals") & !missing(`digitst')
	replace `digits_typet' = "sigdig" if !inlist(`digits_typet',"","sigdig","decimals") & missing(`digitst')
}	

*standard setting if any digits given: decimals
qui replace `digits_typet' ="decimals" if `digits_typet'=="" & !missing(`digitst')

*standard setting ig no digits given
qui replace `digits_typet' ="sigdig" if `digits_typet'=="" & missing(`digitst')

	
*maximal decimal:
//if no digits given: assume two
//if digits given: max of digits for type

cap confirm variable `type'
if _rc {
	qui sum `digitst' 
	if `r(N)' == 0 {
		qui replace `maxdect' = 2 if `maxdect'==.  & inlist(`type',"cat","cat1")
		qui replace `maxdect' = 2 if `maxdect'==.  & !inlist(`type',"cat","cat1")
	}
	else {
		qui replace `maxdect' = `r(max)' if `maxdect'==.
	}
}
else {
	qui levelsof `type', local(lev)
	foreach l of local lev {
		qui sum `digitst' if `type'=="`l'"
		if `r(N)' == 0 {
			qui replace `maxdect' = 2 if `maxdect'==.  & `type'=="`l'"
			qui replace `maxdect' = 2 if `maxdect'==.  & `type'=="`l'"
		}
		else {
			qui replace `maxdect' = `r(max)' if `maxdect'==. & `type'=="`l'"
		}
	}
}

//list varname `maxdect'

*input formats:  amount of %	
tempname a
tempvar ftv
qui gen `ftv'=`ftt'
qui replace `ftv' = subinstr(`ftv',"%%","%`a'",.)
tempvar np
qui gen `np' = strlen(`ftv') - strlen(subinstr(`ftv', "%", "", .)) if !missing(`ftv')	


*input formats: number of digits
//tempvar ft
//gen `ftt' = "%.1 (%.2 to %%.2)"
//local wc = 3

tempvar ftp
qui gen `ftp'=`ftv'
forvalues i=1/`wc' {
	tempvar ndig`i'
	qui gen `ndig`i'' = substr(`ftp',strpos(`ftp',".")+1,1)
	cap assert real(`ndig`i'') == `ndig`i''
	if _rc {
		qui replace `ndig`i''=""
	}
	qui replace `ftp'=subinstr(`ftp',".","",1)
	qui destring `ndig`i'', replace
}					


*generate output variable and set to fr
qui replace `generate'=`ftv' if `touse'==1

//list  `generate' `ftv' `touse'

*format 
***********


forvalues i=1/`wc' {

	//list ``i''
	tempvar le`i'
	tempvar le2`i'
	tempvar digform`i'
	qui gen `le`i''=.
	qui gen `le2`i''=.
	qui gen `digform`i''=""
		
	
	*digits not given: 
	
	qui replace `le`i''=length(string(abs(``i''),"%20.0f")) if  missing(`digitst')
	qui replace `le2`i''=abs(``i'') if `digits_typet'=="sigdig" & missing(`digitst')
	
	//sigdig: 2 significant digits	
	qui replace `digitst' = 2 if `digits_typet'=="sigdig" & missing(`digitst')
		
	//decimals
	qui replace `digform`i''="%20.2f" if `le`i''==1 & abs(``i'')<1 &`digits_typet'=="decimals" & missing(`digitst')
	qui replace `digform`i''="%20.1f" if `le`i''==1 & abs(``i'')>=1 &`digits_typet'=="decimals" & missing(`digitst')
	//qui replace `digform`i''="%20.2f" if `le`i''==1 & `digits_typet'=="decimals" & missing(`digitst')
	qui replace `digform`i''="%20.0f" if `le`i''==2 & `digits_typet'=="decimals" & missing(`digitst')
	qui replace `digform`i''="%20.0f" if `le`i''>=3 & `digits_typet'=="decimals" & missing(`digitst')	
	qui replace `digform`i''="%20.1f" if `le2`i''<1 & `digits_typet'=="decimals" & missing(`digitst')

	
	*digits given
	qui replace `le`i'' = `digitst' - strlen(string(abs(``i''),"%20.0f")) ///
		if  `digits_typet'=="sigdig" & !missing(`digitst')
	qui replace `le`i''=0 if `le`i''<0   & `digits_typet'=="sigdig" & !missing(`digitst')
	
	//sigdig
	tempvar msum
	qui gen `msum'=abs(``i'')
	qui sum  `msum'
	local min=`r(min)'
	local limit=1
	while `min'<`limit' {
		qui replace `le`i''=`le`i''+1 ///
			if abs(``i'') < `limit'  & `digits_typet'=="sigdig" & !missing(`digitst')
		local limit=`limit'/10
	}
	qui levelsof  `le`i'', local(lev)
	foreach l of local lev {
		qui replace `digform`i''="%20.`l'f" ///
			if `le`i''==`l' & `l'<`maxdect'  & `digits_typet'=="sigdig" & !missing(`digitst')
		qui replace `digform`i'' = "%20." + string(`maxdect') + "f" /// 
			if `le`i''==`l'& `l'>=`maxdect'  & `digits_typet'=="sigdig" & !missing(`digitst')
	}
	
		
	//decimals
	qui replace `digform`i'' = "%20." + string(`digitst') + "f" ///
		if `digits_typet'=="decimals" & !missing(`digitst') & `digitst'<=`maxdect'
	qui replace `digform`i'' = "%20." + string(`maxdect') + "f" ///
		if `digits_typet'=="decimals" & !missing(`digitst') & `digitst'>`maxdect'
	
	//given by points
	qui replace `digform`i'' = "%20." + string(`ndig`i'') + "f" if !missing(`ndig`i'')  & `ndig`i''<=`maxdect'
	qui replace `digform`i'' = "%20." + string(`maxdect') + "f" if !missing(`ndig`i'')  & `ndig`i''<=`maxdect'	
		
		
	//replace integers if type==cat | type == count
	cap confirm variable `type'
	if !_rc {
		tempvar dig0
		qui gen `dig0'=.
		qui replace `dig0' =1 if round(``i'',1)==``i'' & inlist(`type',"cat","count","tte") & strpos(`intpos',"`i'")>0
		qui replace `digform`i''="%20.0f" if `dig0'==1
		//list varname ``i'' `dig0' `type' `intpos'
		
		//no digits if categorical and 0 and perc0 option
		if "`catdigits0'"!="" {
			local cdwc: word count `catdigits0'
			forvalues k=1/`cdwc' {
				local cdk: word `k' of `catdigits0'
				qui replace `digform`i''="%20.0f" if  `type'=="cat" & inlist(`cdk',``i'')
			}
		}
	}
	
	*generate output
	qui replace `generate'=subinstr(`generate',"%",string(``i'',`digform`i''),1) if `touse'==1		
}

*re-introduce
qui replace `generate'=subinstr(`generate',"`a'","%",.) if `touse'==1
	
	
end


*formatting2 based on locals
************************************

cap program drop formatting2
program formatting2, nclass
version 16
syntax varlist [if] [in], GENerate(name) [type(string) digits(string) digits_type(string) maxdec(string) ///
	rm_perc add_perc ft(string) dig_force01 dig_force02 dig_force03 dig_force04 dig_force05]

tokenize `varlist'
marksample touse, novarlist
tempvar nt
tempvar ns
tempname a

local wc: word count `varlist'


*entry checks
if !inlist("`type'","","ci","mean","median","cat","ci_range") {	
	dis as error "Please specify valid type option (mean, median, cat, ci, median_range)"
	exit
}


*check type 
if "`type'"=="" & "`ft'"=="" {
	dis as error "Please enter type or format"
	exit
}

if inlist("`type'","ci")  {
	cap assert `wc'==3
	if _rc==9 {
		dis as error "Please enter three variables (point estimate, lower confidence limit, upper confidence limit)"
		exit
	}	
}
if "`type'"=="mean" {
	cap assert `wc'==2
		if _rc==9 {
			dis as error  "Please enter two variables with mean option (mean, standard deviation)"
			exit
		}
}
if "`type'"=="median" {
	cap assert inrange(`wc',2,3,5) 
	if _rc==9 {
		dis as error "Please enter two variables (median, iqr), three variables (median, lq, uq), or five variables (median, min, lq, uq, max) with median option"
		exit
	}	
}

if "`type'"=="cat" {
	cap assert `wc'==2
	if _rc==9 {
		dis as error "Please enter two variables with cat option (number of successes, number of trials)"
		exit
	}	
	if round(`1',1)!=`1' | round(`2',1)!=`2' {
		dis as error "Please enter two integers with cat option (number of successes, number of trials)"	
	}	
}
	
	
*gen output variable	
capture confirm new variable `generate'
if !_rc {
	qui gen `generate'="" if `touse'
	local newvar=1
}
else {
	local newvar=0
}


*any entries
local nme=0
forvalue i=1/`wc' {
	qui count if !missing(``i'')
	local nme = `nme' + `r(N)'
}
if `nme'==0 {
	exit
}

	
*digits_type
cap assert inlist("`digits_type'","","sigdig","decimals")
if _rc==9 {
	dis as text "`digits_type' is not a valid option for digits_type, the default (decimals) is used."
	local digits_type decimals
}

//standard setting: decimals
if "`digits_type'"=="" {
	local digits_type decimals
}
	
*maximal decimal:
if "`maxdec'"=="" {
	local maxdec 10
}


*input formats:  amount of %	
if "`ft'" != "" {
	local ft=subinstr("`ft'","%%","%`a'",.)
	local np=length("`ft'") - length(subinstr("`ft'", "%", "", .))	
	
	if `np'!=`wc' {
		dis as text "Format incorrectly specified, standard option used"
		local ft
	}	
}

*input formats: number of digits
//local wc 3
//local ft "%.1 (% to %%)"
if strpos("`ft'",".")>0 {
	local tcount=0
	foreach l of local ft {
		if strpos("`l'","%")>0 {
			local tcount=`tcount'+1
			if strpos("`l'",".")>0 {
			
				//check if number is >9
				local ndigc=substr("`l'",strpos("`l'",".")+2,1)
				cap assert real("`ndigc'")==`ndigc'
				if _rc {
					local nnumb=1
				}
				else {
					local nnumb=2
				}
					
				//get number of digits 
				local ndig`tcount'=substr("`l'",strpos("`l'",".")+1,`nnumb')
				local nplus=1
				cap assert real("`ndig`tcount''")==`ndig`tcount''
				if _rc {
					local ndig`tcount'=0
					local nplus=0
				}
				local rest=subinstr("`l'",substr("`l'",strpos("`l'","."),`nnumb'+`nplus'),"",.)
				local ftra `ftra' `rest' 		
			}
			else {
				local ftra `ftra' `l'
			}	
		}
		else {
			local ftra `ftra' `l'
		}
	}
	assert `tcount' == `wc'
	local ft `ftra'
}


*standard formats
if "`ft'"=="" {
	if inlist("`type'","","ci") &  "`type'" != "ci_range" {
		local ft="% (% - %)"
	}
	if "`type'"=="mean" {
		local ft="% (%)"
	}
	if "`type'"=="median" & `wc'==2 {
		local ft="% [%]"
	}
	if "`type'"=="median" & `wc'==3 {
		local ft="% [%, %]"
	}
	if "`type'"=="median" & `wc'==5 {
		local ft="% [%, %, %, %]"
	}
	if "`type'"=="cat" {
		local ft="% (%`a')"
	}
}

*format continuous
if inlist("`type'","","ci","mean","median") {

	*digits
	if "`digits'"==""  {
		if inlist("`digits_type'","sigdig") {
			forvalues i=1/`wc' {
				tempvar le`i'
				tempvar digform`i'
				qui gen `le`i''=length(string(abs(``i''),"%20.0f"))
				qui gen `digform`i''="%20.2f" if `le`i''==1
				qui replace `digform`i''="%20.1f" if `le`i''==2
				qui replace `digform`i''="%20.0f" if `le`i''>=3
				if "`ndig`i''"!="" {
					qui replace `digform`i''="%20.`ndig`i''f"
				}
			}	
		}
		else {
			forvalues i=1/`wc' {
				tempvar le`i'
				tempvar digform`i'
				qui gen `le`i''=length(string(abs(`1'),"%20.0f"))
				qui gen `digform`i''="%20.2f" if `le`i''==1
				qui replace `digform`i''="%20.1f" if `le`i''==2
				qui replace `digform`i''="%20.0f" if `le`i''>=3
				if "`ndig`i''"!="" {
					qui replace `digform`i''="%20.`ndig`i''f"
				}
			}		
		}
	}		
	else {	
		if inlist("`digits_type'","sigdig") {
			forvalues i=1/`wc' {
				tempvar le`i'
				tempvar digform`i'
				qui gen `le`i''=`digits'-length(string(abs(``i''),"%20.0f"))
				qui replace `le`i''=0 if `le`i''<0
				tempvar msum
				qui gen `msum'=abs(``i'')
				qui sum  `msum'
				local min=`r(min)'
				local limit=1
				while `min'<`limit' {
					qui replace `le`i''=`le`i''+1 if abs(``i'')<`limit'
					local limit=`limit'/10
				}
				qui levelsof  `le`i'', local(lev)
				qui gen `digform`i''=""
				foreach l of local lev {
					if `l'<`maxdec' {
						qui replace `digform`i''="%20.`l'f" if `le`i''==`l'
					} 
					else {
						qui replace `digform`i''="%20.`maxdec'f" if `le`i''==`l'
					}
				}
				if "`ndig`i''"!="" {
					qui replace `digform`i''="%20.`ndig`i''f"
				}
			}	
		}
		else {
			forvalues i=1/`wc' {
				tempvar digform`i'
				if `digits'<`maxdec' {
					qui gen `digform`i''="%20.`digits'f"
				}	
				else {
					qui gen `digform`i''="%20.`maxdec'f"
				}
				
				if "`ndig`i''"!="" {
					if `ndig`i''<`maxdec' {
						qui replace `digform`i''="%20.`ndig`i''f"
					}
					else {
						qui replace `digform`i''="%20.`maxdec'f"
					}
				}
			}
		}
	}
		

	//forec 0 option	
	forvalues i=1/`wc' {	
		if "`dig_force0`i''"!="" {
			qui replace `digform`i''="%20.0f"
		}
	}
	
	//list `digform'	
	qui replace `generate'="`ft'" if `touse'==1
	
	forvalues i=1/`wc' {		
		qui replace `generate'=subinstr(`generate',"%",string(``i'',`digform`i''),1) if `touse'==1 
	}		
	qui replace `generate'=subinstr(`generate',"`a'","%",.) if `touse'==1 
	
}

*format categorical
if "`type'" == "cat" {
		
	qui sum `1' if `touse'==1
	local m1=r(mean)
	qui sum `2' if `touse'==1
	local m2=r(mean)
	
	if `m1'>`m2' {
		qui gen `ns'=`2'
		gen `nt'=`1'
		if "`ndig2'"!="" {
			local ndig=`ndig2'
		}
	}
	if `m2'>=`m1' {
		qui gen `ns'=`1'
		qui gen `nt'=`2'
		if "`ndig1'"!="" {
			local ndig=`ndig1'
		}
	}	
	
	if "`rm_perc'" != "" {
		local end=")"
	}
	else {
	local end="%)"
	}
	
	//digits
	if "`digits'"=="" {
		if inlist("`digits_type'","sigdig") {
			tempvar le
			tempvar digform
			qui gen `le'=length(string(`ns'/`nt'*100,"%20.0f"))
			
			qui gen `digform'=""
			if `maxdec'>=2 | missing(`maxdec') {
				qui replace `digform'="%20.2f" if `le'==1
				qui replace `digform'="%20.1f" if `le'==2
				qui replace `digform'="%20.0f" if `le'>=3
			} 
			if `maxdec'==1 {
				qui replace `digform'="%20.1f" if inlist(`le',1,2)
				qui replace `digform'="%20.0f" if `le'>=3
			}
			if `maxdec'==0 {
				qui replace `digform'="%20.0f"
			}
		
			if "`ndig'"!="" {
				qui replace `digform'="%20.`ndig'f"
			}	
		}
		else {
			tempvar digform
			qui gen `digform'="%20.0f"
		}
	}
	else {
		if inlist("`digits_type'","sigdig") {
			tempvar le
			tempvar digform
			qui gen `le'=`digits'-length(string(`ns'/`nt'*100,"%20.0f"))
			qui replace `le'=0 if `le'<0
			tempvar rper
			qui gen `rper' = `ns'/`nt'*100
			qui sum `rper'
			local min=min(abs(r(min)),abs(r(max)))
			local limit=1
			if `min'!=0 {
				while `min'<`limit' {
					qui replace `le'=`le'+1 if `rper'<`limit'
					local limit=`limit'/10
				}
			}
			qui levelsof  `le', local(lev)
			qui gen `digform'=""
			foreach l of local lev {
				if `l'<`maxdec' {
					qui replace `digform'="%20.`l'f" if `le'==`l'
				} 
				else {
					qui replace `digform'="%20.`maxdec'f" if `le'==`l'
				}
			}
			if "`ndig'"!="" {
				qui replace `digform'="%20.`ndig'f"
			}
		}	
		else {
			tempvar digform
			if `digits'<`maxdec'  {
				qui gen `digform'="%20.`digits'f"
			}
			else {
				qui gen `digform'="%20.`maxdec'f"
			}
			
			if "`ndig'"!="" {
				if `ndig'<`maxdec'  {
					qui replace `digform'="%20.`ndig'f"
				}
				else {
					qui replace `digform'="%20.`maxdec'f"
				}
			}
		}
	}
	
	//list `digform'
	
	qui replace `generate'="`ft'" if `touse'==1
	qui replace `generate'=subinstr(`generate',"%",string(`ns',"%20.0f"),1)  if `touse'==1 
	qui replace `generate'=subinstr(`generate',"%",string(`ns'/`nt'*100,`digform'),1) if `touse'==1
	qui replace `generate'=subinstr(`generate',"`a'","%",.) if `touse'==1
}


			
end


/**********************************************************************************************************************/
	
*******************
*function to generate empty lines
****************	

cap program drop empty_line
program empty_line, rclass 
version 16
syntax [if] [in] [, GENerate(name) position(string)]

marksample touse	
tempvar seq
tempvar expanded

if !inlist("`position'" ,"after","before","") {
	dis as error "use after or before as position arguments"
}
else {
	qui gen `seq'=_n
	qui expand 2 if `touse'==1, gen(`expanded')

	if "`position'"=="" {
		qui replace `seq'=`seq'+0.1 if `expanded'==1 
	}
	else {
		if "`position'"=="after" {
			qui replace `seq'=`seq'+0.1 if `expanded'==1
		}
		if "`position'"=="before" {
			qui replace `seq'=`seq'-0.1 if `expanded'==1
		}
	}
	sort `seq'
	qui sum `seq' if `expanded'==1
	cap local insert_line=ceil(`r(max)')

	if "`generate'" != "" {
		confirm new variable `generate'
		qui gen `generate'=`expanded'
	}

	foreach var of varlist _all {
		local vartype: type `var'
		if strpos("`vartype'","str")>0 {
			qui replace `var'="" if `expanded'==1
		}
		else {
			qui replace `var'=. if `expanded'==1
		}
	}		
	cap return scalar insert_line=`insert_line'
}

end		
	
/**********************************************************************************************************************/
		
	
*******************
*function for formatting p-values
***************

cap program drop format_p
program format_p, nclass
version 16

syntax varname [if] [in], GENerate(name) [format(string) DIGits(string) breaks(string)] 

marksample touse, novarlist

capture confirm new variable `generate'
if !_rc {
	qui gen `generate'="" if `touse'
	local newvar=1
}
else {
	local newvar=0
	tempvar saved
	qui gen `saved'=`generate'
}
	

*entry checks
if "`digits'"!="" & "`format'"!=""  {
	dis as error "Specifiy either digits or format"
	if `newvar'==1 {
		qui drop `generate'
	} 
	else {
		qui replace `generate'=`saved'
	}
	exit, clear
}
if "`breaks'"!="" & "`format'"!=""  {
	dis as error "Specifiy either breaks or format"
	if `newvar'==1 {
		qui drop `generate'
	} 
	else {
		qui replace `generate'=`saved'
	}
	exit, clear
}
if "`breaks'"!="" & "`digits'"==""  {
	dis as error "Specifiy digits together with breaks"
	if `newvar'==1 {
		qui drop `generate'
	} 
	else {
		qui replace `generate'=`saved'
	}
	exit, clear
}
	
*standard
************
if "`digits'"=="" & "`breaks'"=="" & "`format'"=="" {
	local digits 2 3
	local breaks 1 0.05 0.001
}


*via format
***************

if "`digits'"=="" & "`breaks'"=="" & "`format'"!="" {
	
	if inlist(strlower("`format'"),"lancet34") {
		local digits 2 3 4
		local breaks 1 0.05 0.001 0.0001
	}
	
	if inlist(strlower("`format'"),"lancet4") {
		local digits 2 4
		local breaks 1 0.05 0.0001
	}

	if inlist(strlower("`format'"),"lancet3") {
		local digits 2 3
		local breaks 1 0.05 0.001
	}
	if strlower("`format'")=="nejm" {
		local digits 2 3
		local breaks 1 0.01 0.001
	}
	if !inlist(strlower("`format'"),"lancet34","lancet3","lancet4","nejm") {
		dis as text "Format `format' not available, standard used"
		local digits 2 3
		local breaks 1 0.05 0.001
	}
}
	
	
*via digits only
***********

if "`digits'"!="" & "`breaks'"==""  {
	local wcd: word count `digits'
	cap assert `wcd'==1
	if _rc {
		dis as error "Option breaks has to be specified if more than one number is given for digits."
		if `newvar'==1 {
			qui drop `generate'
		} 
		else {
			qui replace `generate'=`saved'
		}
		exit, clear
	}
	else {
		local ff="%20.`digits'f"
		local thr=10^(-`digits')
		local thrf=string(`thr',"`ff'")
		qui replace `generate'="<`thrf'" if `varlist'<`thr' & `touse'==1
		qui replace `generate'=string(`varlist',"`ff'") if `varlist'>=`thr' & `touse'==1
	}
}


*via digits and threshold
**********************
if "`digits'" != "" & "`breaks'"!=""  {
	
	
	*direction
	local wct1: word 1 of `breaks'
	local rbreaks=reverse("`breaks'")
	local rwcte: word 1 of  `rbreaks'
	local wcte = reverse("`rwcte'")
	
	
	if `wct1' > `wcte' {
		local direction "descending"
	}
	else {
		local direction "ascending"
	}
	
	*add 1 and 0 at start and beginning
	if "`direction'" == "descending" {
		if "`wct1'" != "1" {
			local breaks 1 `breaks'
		}
	}
	if "`direction'" == "ascending" {
		if "`wcte'" != "1" {
			local breaks `breaks' 1
		}
	}
	
	local er = 0
	cap numlist "`breaks'", sort `direction'
	if _rc {
		dis as text "Breaks are not strictly monotone (i.e. ascending or descending), defaults used."
		local er = 1
	}
	
	local wct: word count `breaks'
	local wcd: word count `digits'	
	
	cap assert `wct' == `wcd'+1
	if _rc {
		dis as text "Digits and threshold do not agree, defaults used."
		local er = 1
	}
	
	if `er' == 1 {	
		local digits 2 3
		local breaks 1 0.05 0.001
		local direction "descending"
		local wct1: word 1 of `breaks'
		local rbreaks=reverse("`breaks'")
		local rwcte: word 1 of  `rbreaks'
		local wcte = reverse("`rwcte'")
		local wct: word count `breaks'
		local wcd: word count `digits'	
	}
	
	if "`direction'" == "ascending" {
	
		qui replace `generate'="<" + "`wct1'" if `varlist' < `wct1' & `touse'==1
		
		local imax=`wct'-1
		forvalues i=1/`imax' {
		
			local j=`i'+1
			
			local wi: word `i' of `breaks'
			local wj: word `j' of `breaks'
			local di: word `i' of `digits' 
			
			qui replace `generate'= string(`varlist',"%20.`di'f") ///
				if `varlist'>=`wi' & `varlist'<`wj' & `touse'==1
				
			//special case of pvalue is exactly 1	
			qui replace `generate'= string(`varlist',"%20.`di'f") ///
				if `varlist'==1 & `wj'==1 & `touse'==1	
				
		}
	}

	if "`direction'" == "descending" {
	
		qui replace `generate'="<" + "`wcte'" if `varlist' < `wcte' & `touse'==1
		
		local imax=`wct'-1
		forvalues i=1/`imax' {
		
			local j=`i'+1
			
			local wi: word `i' of `breaks'
			local wj: word `j' of `breaks'
			local di: word `i' of `digits' 
		
			cap assert `wj'<`wi'
			
			qui replace `generate'= string(`varlist',"%20.`di'f") ///
				if `varlist'<`wi' & `varlist'>=`wj' & `touse'==1
			
			//special case of pvalue is exactly 1	
			qui replace `generate'= string(`varlist',"%20.`di'f") ///
				if `varlist'==1 & `wi'==1 & `touse'==1	
		}
	}
	
}



qui replace `generate'="" if `generate'=="."
qui compress `generate'
	
end

/**********************************************************************************************************************/
		
	
*******************
*function for sorting lists according to length of each element 
***************	

//longest is first unless option shortfirst is specified

cap program drop sortlistlen
program sortlistlen, rclass

syntax, listin(string) [SHORTfirst]

tempname a

local desclisth		
foreach dl of local listin {
	local sl = length("`dl'")
	local desclisth `desclisth' `sl'`a'`dl'
}	

local sl2 = length("`a'")

local desclisth: list sort desclisth

if "`shortfirst'" ==  "" {

	local desclists
	foreach dl of local desclisth {
		local sl = substr("`dl'",strpos("`dl'","`a'") + `sl2',.)
		local desclists `sl' `desclists' 
	}
}
else {
	local desclists
	foreach dl of local desclisth {
		local sl = substr("`dl'",strpos("`dl'","`a'") + `sl2',.)
		local desclists  `desclists' `sl'
	}
}
return local listout `desclists'

end

/**********************************************************************************************************************/
	
	
***********************************
*Program to apply design types in btable
*Generates appropriate number of lines and reshapes the additional descriptive and effect measures in wide
*****************************


cap program drop apply_design_var
program apply_design_var, nclass
syntax [, nrowvar(varname) design(string) inset(string) inset_row(string) nonmiss(string) varname_sp(varname) ///
	namemiss(string) denom(string) digits_miss(string) digits_type_miss(string) maxdec_miss(string) ]


if "`nrowvar'"=="" {
	qui gen `nrowvar' = 1
}

if "`design'"=="" {
	local design column
}

if "`inset'"=="" {
	local inset "    "
}

if "`inset_row'"=="" {
	local inset_row " - "
}

local elist
cap confirm variable out_d
if !_rc {
	local elist `elist' out_d effect_info
}
cap confirm variable pv
if !_rc {
	local elist `elist' pv test_info
}


 *find missings
 *******************
tempvar nonmissvar
qui gen `nonmissvar' = .			
			
if "`nonmiss'"!="" {
	cap ds_var, dslist(`nonmiss') vname(varname)
	if _rc {
		dis as text "Incorrect varlist for nonmiss --- option ignored"
	}
	else {
		local nonmissvl `r(varlist)'
		local nmwc: word count `nonmissvl'
		forvalues nmi = 1/`nmwc' {
			local nmwi: word `nmi' of `nonmissvl' 
			qui replace `nonmissvar' = 1 if strpos(" `nmwi' ",`varname_sp')>0
		}
	}	
}

tempvar miss 
qui gen `miss' = 0
//if "`design'"=="missing" & "`denom'"!="nonmiss"  {
if "`design'"=="missing"   {
	qui replace `miss' = 1 if ntot_t != nnonmiss_t & `nonmissvar' != 1 //& inlist(vtype,"conti","tte","count")
}

//dis "`denom'"
//list varname `varname_sp' `miss' `nonmissvar'

tempvar expf
tempvar seq 
tempvar srow
tempvar nrow
 
*cat
************
qui gen `seq'=_n
 
*add row for missings
qui expand 2 if `miss'==1 & varname != varname[_n+1] & vtype=="cat"
sort `seq'
tempvar lastrow
qui gen `lastrow' = 1 if varname != varname[_n+1] & vtype=="cat"

cap replace varname = varname[_n-1] if `lastrow'==1 & `miss'==1
cap replace levlabel="`inset'" + "`namemiss'" if `lastrow'==1 & `miss'==1
cap replace desc_info="n (%)" if `lastrow'==1 & `miss'==1

foreach var of local elist {
	cap replace `var' = "" if `lastrow'==1 & `miss'==1
}
 
foreach var of varlist ntot* {
	
	local pos=strpos("`var'","_")+1
	local na=substr("`var'",`pos',.)
	tempvar nmiss_`na'
	tempvar outh
	
	qui gen `nmiss_`na'' = ntot_`na' - nnonmiss_`na'

	//default 
	//dis "`digits_miss', `digits_type_miss', `maxdec_miss'"
	formatting2 `nmiss_`na'' ntot_`na' if `lastrow'==1 & `miss'==1, ///
						type(cat) gen(`outh') ft(`ft_cat') ///
						digits(`digits_miss') digits_type(`digits_type_miss') maxdec(`maxdec_miss')
	//list `outh'					
	qui replace out_`na' = `outh' if `lastrow'==1 & `miss'==1
	qui drop `nmiss_`na''
} 

*expand for more than one descriptive
qui replace `seq' = _n
qui bysort varname (`seq'): gen `srow' = _n 
qui bysort varname (`srow'): gen `nrow' = _N 
sort `seq'

qui gen `expf' = `nrowvar' if inlist(vtype,"cat","cat1")
//for collapsed, additional row
qui replace `expf' = `nrowvar' + 1 if `nrowvar'>1 & `nrow'==1 & inlist(vtype,"cat","cat1")

tempvar expanded
qui expand `expf', gen(`expanded')

tempvar seq_exp
qui bysort `seq' (`expanded'): gen `seq_exp' = _n if `expanded'==1
qui replace `seq' = `seq' + `nrow' - `srow' + `srow'/`nrow' - 0.0000001  if `expanded'==1
sort `seq' `seq_exp'

local nmax=1
qui sum `expf' if inlist(vtype,"cat","cat1")
if `r(N)' != 0 {
	local nmax = `r(max)'
}

//list varname `srow' `nrow' `miss' `seq_exp'
cap ds pv
if _rc {
	qui ds out_? *_info
	local vlist `r(varlist)'
}
else {
	qui ds out_? pv *_info
	local vlist `r(varlist)'
}


if `nmax'>1 {
	tempvar norep
	qui gen `norep' = 1 if `miss'==1 & `lastrow'==1
	
	//collapsed with more than 1 row
	foreach var of local vlist {
		qui replace `var' = "" if `nrow'==1 & vtype=="cat1" & `nrowvar'>1 & `seq_exp'==.
	
	}
	
	forvalues k = 2/`nmax' {
		foreach var of local vlist {
			cap replace `var' = `var'_v`k' if `seq_exp'==`k' & `norep' != 1 & `nrow'!=1
			
			//collapses with more than 1 row
			if `k'>2 {
				local l=`k'-1
				qui replace `var' = `var'_v`l' if `seq_exp'==`k' & `norep' != 1 & `nrow'==1
			}
		}
		//collapsed label
		qui replace levlabel = "`inset'" + desc_info if `seq_exp'==`k' & `nrow'==1
	
	}
} 

//list varname levlabel desc_info vtype `nrow' `seq_exp'

if "`design'"!="column" {
	qui replace levlabel=levlabel + "`inset_row'" + desc_info[_n+1] if vtype=="cat1" & `nrow'>1
	qui replace levlabel=levlabel + "`inset_row'" + desc_info ///
		if vtype=="cat1" & `nrow'==1 & !missing(desc_info) & `seq_exp'==.
}

//collapsed
//qui replace levlabel=levlabel + "`inset_row'" + desc_info if vtype=="cat1" & `nrow'==1 



cap drop `expf' 
cap drop `seq'
cap drop `srow'
cap drop `nrow'
cap drop `lastrow'

 
*conti, count, tte
************
qui gen `seq'=_n
 
//expand for more than one row
qui gen     `expf' = `nrowvar' if inlist(vtype,"conti","tte","count")
qui replace `expf' = `nrowvar' + 1 + `miss' if `miss' == 1  & inlist(vtype,"conti","tte","count")
qui replace `expf' = `nrowvar' + 1  if `miss' == 0 & `nrowvar'>1 & inlist(vtype,"conti","tte","count")

qui expand `expf'

sort `seq'
qui bysort varname (`seq'): gen `srow' = _n 
qui bysort varname (`srow'): gen `nrow' = _N 
sort `seq' `srow'
tempvar nc
qui gen `nc' = `expf' - `miss'


*missings in last row		
qui replace levlabel="`inset'" + "missing" + "`inset_row'" + "n (%)"  ///
	 if inlist(vtype,"conti","tte","count") & `srow'==`nrow' & `miss'==1
qui replace desc_info="n (%)" if  inlist(vtype,"conti","tte","count") & `srow'==`nrow' & `miss'==1
foreach var of local elist {
	cap replace `var' = "" if  inlist(vtype,"conti","tte","count") & `srow'==`nrow' & `miss'==1
}
foreach var of varlist ntot* {
		
	local pos=strpos("`var'","_")+1
	local na=substr("`var'",`pos',.)
	tempvar nmiss_`na'
	tempvar outh
	
	qui gen `nmiss_`na'' = ntot_`na' - nnonmiss_`na'

	formatting2 `nmiss_`na'' ntot_`na' if  inlist(vtype,"conti","tte","count") & `srow'==`nrow' & `miss'==1, ///
						type(cat) gen(`outh') ft(`ft_cat') ///
						digits(`digits_miss') digits_type(`digits_type_miss') maxdec(`maxdec_miss')
	qui replace out_`na' = `outh' if  inlist(vtype,"conti","tte","count") & `srow'==`nrow' & `miss'==1	
	qui drop `nmiss_`na''
}
	

*all the other rows

//empty first line:
foreach var of varlist out_* *_info `elist' {  //ns_* 
	cap replace `var' = "" if `srow'==1 & `expf'>1 &  inlist(vtype,"conti","tte","count") 
}

//adapt label for second
qui replace levlabel = "`inset'" + desc_info if `srow'==2 & `expf'>1 & inlist(vtype,"conti","tte","count")

//all larger
local nmax=1
qui sum `nc' if inlist(vtype,"conti","tte","count")
if `r(N)' != 0 {
	local nmax = `r(max)'
}

if `nmax'>2 {
	forvalues k = 3/`nmax' {
		foreach var of varlist out_? *_info `elist' {
			local l = `k' - 1
			qui replace `var' = `var'_v`l' if `srow' == `k' & `expf'>1 & inlist(vtype,"conti","tte","count")
		}
		qui replace levlabel = "`inset'" + desc_info if `srow' == `k' & `expf'>1 & inlist(vtype,"conti","tte","count")
	}
}

//labels if no expand
if inlist("`design'","row","missing") {
	qui replace levlabel=levlabel + "`inset_row'" + desc_info if `expf'==1 & inlist(vtype,"conti","tte","count")
}	

//n's only in 1?
foreach var of varlist ns_* {
	cap replace `var' = "" if `srow'>1 & `expf'>1 & inlist(vtype,"conti","tte","count")
}
		
		
cap drop out_t_v?
cap drop out_1_v?
cap drop out_2_v?
cap drop out_d_v?
cap drop test_v?


end

	
		
/**********************************************************************************************************************/
	
	
***********************************
*Program to use wildcards based on levels of a variable
*****************************

cap program drop ds_var
program ds_var, rclass
syntax, dslist(string) vname(varname)

local ovnames
forvalues j = 1/`=_N' {
	local yi = `vname'[`j']
	local ovnames `ovnames' `yi'
}
local ovnames: list uniq ovnames

if "`dslist'" != "" {	
	preserve
	clear
	foreach ovaname of local ovnames {
		qui gen `ovaname'=.
	}
	cap ds `dslist'
	if !_rc {
		local ds_all `r(varlist)'
		local ds_dups : list dups ds_all
		local ds_out : list ds_all - ds_dups
	} 
	else {	
		local ds_out
		exit()
	}	
	restore
} 
else {
	local ds_out
}

return local varlist `ds_out'

end	
	
	
	
		
/**********************************************************************************************************************/
	
	
***********************************
*Program to collapse categorical variables with two levels
*****************************


cap program drop collapse_cat
program collapse_cat, nclass
syntax, collapse(string) collapselevv(varname) design(string) 


qui replace `collapselevv' = 2 if missing(`collapselevv')

local cwc: word count `collapse'

forvalues cw=1/`cwc' {
	
	local cvarl: word `cw' of `collapse'

	foreach cvar of local cvarl {
		tempvar checkcvar
		qui gen `checkcvar' = 1 if varname=="`cvar'"
		qui sum `checkcvar'
		cap assert `r(mean)' == 1
		if _rc {
			dis as text "Variable `cvar' is not included in varlist, cannot be collapsed"
		}
		else {
			cap assert inlist(nlev,1,2) if varname=="`cvar'"
			if _rc {
				dis as text "Variable `cvar' cannot be collapsed as it has more than two levels"
			}
			else {
				local vwm=0
				
				if "`design'"=="missing" & "`denom'"!="nonmiss" {
					cap assert ntot_t == nnonmiss_t if varname=="`cvar'" & vtype=="cat1"
					if _rc {
						local vwm=1
					}
				} 
				else {
					if "`design'"=="missing" & "`denom'"=="nonmiss" {
						local vwm=0
						cap assert nnonmiss_t==nlev_t if varname=="`cvar'" & vtype=="cat1"
						if _rc {
							local vwm=1
						}
					}
				}
				
				if `vwm'==1 {
					dis as text "Variable `cvar' has missings and is not collapsed in design missing."
				} 
				else {
				
					qui sum nlev if varname=="`cvar'"
					
					*2 categories
					if `r(mean)'==2 {
					
						cap drop seq
						qui egen seq=seq() if nlev==2 & varname=="`cvar'", from(1) to(3)
						
						if inlist("`design'","row","long","missing") {

							qui replace levlabel=levlabel + " (" + trim(levlabel[_n+`collapselevv']) + ")" ///
								if varname=="`cvar'" & seq==1
								
							foreach var of varlist desc_info* {	
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1	
							}	
						} 
						else {
							qui replace levlabel=levlabel + " (" + trim(levlabel[_n+`collapselevv']) + ")" ///
								if varname=="`cvar'" & seq==1
							foreach var of varlist desc_info*  {		
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1	
							}	
						}
						
						cap confirm variable effect_info
						if !_rc {
							foreach var of varlist  effect_info*  {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}	
						}
						cap confirm variable abbr_effect_info
						if !_rc {
							foreach var of varlist abbr_effect_info*  {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}	
						}
						
						//list varname out* pv*
						
						foreach var of varlist out* {
							qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
						}
						cap ds pv*
						if !_rc {
							foreach var of varlist pv* {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}
						}
						
						qui drop if nlev==2 & seq!=1 & varname=="`cvar'"
					}
					else {
					
					*1 category
						dis as text "Variable `cvar' has only one level, consider using option alllevels in btable."
						cap assert `collapselevv'==1 if varname=="`cvar'"
						if _rc {
							dis as text "Option collapselev(2) is not applicable for `cvar', collapselev(1) is used."
							qui replace `collapselevv'=1 if varname=="`cvar'"
						}
						
						
						cap drop seq
						qui egen seq=seq() if nlev==1 & varname=="`cvar'", from(1) to(2)
						
						if inlist("`design'","row","long","missing") {
							

							qui replace levlabel=subinstr(levlabel,"`inset_row'" + desc_info[_n+`collapselevv'], ///
								" (" + trim(levlabel[_n+`collapselevv']) + ")" + "`inset_row'" + ///
								desc_info[_n+`collapselevv'],.) ///
								if varname=="`cvar'" & seq==1		
							
							foreach var of varlist desc_info* {
								qui replace desc_info=desc_info[_n+`collapselevv'] if varname=="`cvar'" & seq==1	
							}	
						} 
						else {
							qui replace levlabel=levlabel + " (" + trim(levlabel[_n+`collapselevv']) + ")" ///
								if varname=="`cvar'" & seq==1
								
							foreach var of varlist desc_info* {		
								qui replace desc_info=desc_info[_n+`collapselevv'] if varname=="`cvar'" & seq==1	
							}
						}
						//list varname levlabel nlev seq `collapselevv' desc_info
						
						cap confirm variable effect_info
						if !_rc {
							foreach var of varlist  effect_info* {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}
						}
						
						cap confirm variable abbr_effect_info
						if !_rc {
							foreach var of varlist  abbr_effect_info* {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}
						}
						
						foreach var of varlist out* {
							qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
						}
						cap ds pv*
						if !_rc {
							foreach var of varlist pv* {
								qui replace `var'=`var'[_n+`collapselevv'] if varname=="`cvar'" & seq==1
							}	
						}
						
						qui drop if nlev==1 & seq!=1 & varname=="`cvar'"
					}
					//list varname levlabel nlev
					
				}	
			}
		}	
	}	
}

	
end

	