/*********************************
RMST
Calculates restricted mean survival time and its difference between groups with three methods

1) non-parametric integration of Kaplan-Meier curves via stci

2) flexible parametric survival models in each group with 2 df on the cumulative hazard scale according to

	Royston2013
	Royston, P. & Parmar, M. K. 
	Restricted mean survival time: an alternative to the hazard ratio for the design and analysis of randomized trials with a time-to-event outcome 
	BMC Med Res Methodol, Springer Nature, 2013, 13
	
	Royston2011
	Royston, P. & Parmar, M. K. 
	The use of restricted mean survival time to estimate the treatment effect in randomized clinical trials 
	when the proportional hazards assumption is in doubt 
	Statistics in medicine, Wiley Online Library, 2011, 30, 2409-2421

3) pseudovalues according to Parner2010:
 
	Parner, Erik T and Andersen, Per K
	Regression analysis of censored data using pseudo-observations
	The Stata Journal 10:3, 2010
	
Author: Lukas Bütikofer
Created 14.04.2016
Last version: 22.01.2021
***********************************/


cap program drop RMST
program RMST, rclass

version 15
syntax [varname(default=none)] [if] [in] [, tmax(string) df(string) npar fpm pseudo Level(cilevel)]

st_is 2 analysis

marksample touse, novarlist

if "`df'"=="" {
	local df 3
} 
if "`dftvc'"=="" {
	local dftvc 0
	//proportional hazard
} 



if "`npar'" == "" & "`fpm'" =="" & "`pseudo'" == "" {
	local npar = 1
	local fpm = 1
	local pseudo = 1
}


if "`level'"!="" {
	local alpha = (100 - `level')/100
} 
else {
	local alpha=0.05
}

local quant=1-`alpha'/2


if "`fpm'" != "" {
	cap which stpm2
	if _rc {
		dis as error "stpm2 is required; type -ssc install stpm2- to obtain it"
		exit 499
	}
}
if "`pseudo'" != "" {
	cap which stpmean
	if _rc {
		dis as error "rmst is required; type -net install rmst, from(http://www.homepages.ucl.ac.uk/~ucakjpr/stata)- to obtain it"
		exit 499
	}
}




*********************
*no group
*******************
if "`varlist'"=="" {

	if "`tmax'"=="" {
		qui sum _t if `touse'
		local tmax=`r(max)'
		dis as text "tmax missing, longest observed time was used: `=string(`tmax',"%4.2f")'"
	} 
	
	return scalar tmax=`tmax'
		
	*1) non-parametric
	**********************
	if "`npar'" != "" {

		preserve 
		qui stset _t, fail(_d) exit(time `tmax')
		qui stci if `touse', rmean level(`level')
		restore
		
		local r= 	`r(rmean)'
		local se=	`r(se)'
		local r_lci=`r(lb)'
		local r_uci=`r(ub)'
		local z=`r'/`se'
		local p=2*(1-normal(abs(`z')))

		return scalar rmst_npar=`r'
		return scalar rmst_lci_npar=`r_lci'
		return scalar rmst_uci_npar=`r_uci'
		tempname A
		matrix `A'= `r' \ `se' \ `z' \ ///
				`p' \ `r_lci' \ `r_uci' 	
		mat colnames `A'= ""
		mat rownames `A' = b se t pvalue ll ul 
		return matrix table_npar `A'
	
	}
	
	*2) parametric
	*******************
	if "`fpm'" != "" {
		
		tempvar rmst
		tempvar rmst_se
		
		qui stpm2  if  `touse' , scale(hazard) df(`df') 
		qui predict `rmst', rmst tmax(`tmax') stdp 
			
		local rmst_lci=`rmst'[1]-invnormal(`quant')*`rmst'_se[1]
		local rmst_uci=`rmst'[1]+invnormal(`quant')*`rmst'_se[1]
		local rmst_z=`rmst'[1]/`rmst'_se[1]
		local rmst_p=2*(1-normal(abs(`rmst_z')))

		return scalar tmax_fpm=`tmax'	
		return scalar rmst_fpm=`rmst'[1]
		return scalar rmst_lci_fpm=`rmst_lci'
		return scalar rmst_uci_fpm=`rmst_uci'
								  
		tempname B
		matrix `B'= `rmst'[1] \ `rmst'_se[1] \ ///
					`rmst_z' \  `rmst_p'  \ ///
					`rmst_lci' \ `rmst_uci' 	
		mat colnames `B'= ""
		mat rownames `B' = b se t pvalue ll ul 
		return matrix table_fpm `B'
	}

	* 3) pseudovalues
	*********************
	if "`pseudo'" != "" {
	
		tempvar pmean50
		tempname P
		
		local tmax_ = `tmax' - `tmax'/10^6
		qui stpmean if  `touse' , at(`tmax_') generate(`pmean50')
				
		qui glm `pmean50' if  `touse' ,  family(gaussian) link(id) vce(robust)
		matrix `P' = r(table)
		
		local r     = `P'[1,1]
		local se    = `P'[2,1]
		local r_lci = `P'[5,1]
		local r_uci = `P'[6,1]
		local z 	= `P'[3,1]
		local p		= `P'[4,1]
		return scalar rmst_pseudo=`r'
		return scalar rmst_lci_pseudo=`r_lci'
		return scalar rmst_uci_pseudo=`r_uci'
		tempname C
		matrix `C'= `r' \ `se' \ `z' \ ///
				`p' \ `r_lci' \ `r_uci' 	
		mat colnames `C'= ""
		mat rownames `C' = b se t pvalue ll ul 
		return matrix table_pseudo `C'
	}
		
}
else {

*********************
*group
*******************

	qui levelsof `varlist', local(lev)
	local wc: word count `lev'
	cap assert `wc'==2
	if _rc {
		dis as text "Group variable does not have two levels, difference is not calculated."
	}
	
	if "`tmax'"=="" {
		local counter=0
		foreach l of local lev {
			qui sum _t if `varlist'==`l'
			local m`counter'=`r(max)'
			local counter=`counter'+1
		}
		local tmax=min(`m0',`m1')	
		dis as text ///
			"tmax missing, minimum of the longest observed times in both groups was used (`=string(`tmax',"%4.2f")')."
	}
	
	return scalar tmax=`tmax'
	
	dis as result "Restricted mean survival time calculated at `=string(`tmax',"%4.2f")'"
	
	
	*1) non-parametric approach
	*************************
	if "`npar'" != "" {

		preserve 
		
		qui stset _t, fail(_d) exit(time `tmax')
			
		local i=0

		foreach l of local lev {
		 
			local lb`i': label (`varlist') `l'
	
			qui stci if `varlist'==`l' & `touse', rmean level(`level')
				
			local r`i'=r(rmean)
			local se`i'=r(se)
			local r`i'_lci=r(lb)
			local r`i'_uci=r(ub)
			local z`i'=`r`i''/`se`i''
			local p`i'=2*(1-normal(abs(`z`i'')))
			
			return scalar rmst`l'_npar=`r`i''
			return scalar rmst`l'_lci_npar=`r`i'_lci'
			return scalar rmst`l'_uci_npar=`r`i'_uci'
			
			tempname I
			matrix `I' = `r`i'' \ `se`i'' \ `z`i'' \ `p`i'' \ `r`i'_lci' \  `r`i'_uci'
			mat colnames `I'= "`lb`i''"
			mat rownames `I' = b se t pvalue ll ul 
			
			if `i'==0 {
				tempname Ic
				matrix `Ic' = `I'
			}
			else {
				matrix `Ic' = `Ic' , `I'
			}
			
			local i=`i'+1					
		}
	
		restore
			
		if `wc'!=2 {
			return matrix table_npar `Ic'
		}
		else {
			local rD=`r1'-`r0'
			local seD=sqrt(`se1'^2+`se0'^2)
			local rD_lci=`rD'-invnorm(`quant')*`seD'
			local rD_uci=`rD'+invnorm(`quant')*`seD'

			local zD=`rD'/`seD'
			local pD=2*(1-normal(abs(`zD')))
			
			dis ""
			dis as result "    based on non-parametric method:"
			dis as result string(`rD',"%4.3f") + " (" + string(`rD_lci',"%4.3f") + " to " + ///
				string(`rD_uci',"%4.3f") + "), p = " + string(`pD',"%5.4f") 
			
			
			return scalar rmstd_npar=`rD'
			return scalar rmstd_lci_npar=`rD_lci'
			return scalar rmstd_uci_npar=`rD_uci'
			return scalar rmstd_p_npar=`pD'
			
			tempname A
			matrix `A'= `r0', `r1', `rD' \ `se0' , `se1' , `seD' \ `z0' , `z1' , `zD' \ ///
					`p0' , `p1' , `pD' \ `r0_lci', `r1_lci', `rD_lci' \ `r0_uci', `r1_uci', `rD_uci' 	
			mat colnames `A'= "`lb0'" "`lb1'" "difference"
			mat rownames `A' = b se t pvalue ll ul 
			return matrix table_npar `A'
		
		}
		
	}
	
	
	*2) parametric approach
	************************	
	if "`fpm'"!="" {
		
		local i=0
		foreach l of local lev {
			
			local lb`i': label (`varlist') `l'
			
			tempvar rmst`i'
			tempvar rmst`i'_se
			
			qui stpm2  if `varlist'==`l'  & `touse' , scale(hazard) df(`df') 
			qui predict `rmst`i'', rmst tmax(`tmax') stdp 
			
			local rmst`i'_lci=`rmst`i''[1]-invnormal(`quant')*`rmst`i''_se[1]
			local rmst`i'_uci=`rmst`i''[1]+invnormal(`quant')*`rmst`i''_se[1]
			local rmst`i'_z=`rmst`i''[1]/`rmst`i''_se[1]
			local rmst`i'_p=2*(1-normal(abs(`rmst`i'_z')))
			
			return scalar rmst`l'_fpm=`rmst`i''[1]
			return scalar rmst`l'_lci_fpm=`rmst`i'_lci'
			return scalar rmst`l'_uci_fpm=`rmst`i'_uci'

			tempname I
			matrix `I' = `rmst`i''[1] \ `rmst`i''_se[1] \ `rmst`i'_z' \ `rmst`i'_p' \ `rmst`i'_lci' \  `rmst`i'_uci'
			mat colnames `I'= "`lb`i''"
			mat rownames `I' = b se t pvalue ll ul 
			
			if `i'==0 {
				tempname Ic
				matrix `Ic' = `I'
			}
			else {
				matrix `Ic' = `Ic' , `I'
			}
			
			local i=`i'+1
			
		}
		
		if `wc'!=2 {
			return matrix table_fpm `Ic'
		}
		else {
			
			local rmstd=`rmst1'[1]-`rmst0'[1]
			local rmstd_se=sqrt(`rmst1'_se[1]^2 + `rmst0'_se[1]^2)
			local rmstd_lci=`rmstd'-invnormal(`quant')*`rmstd_se'
			local rmstd_uci=`rmstd'+invnormal(`quant')*`rmstd_se'
			local rmstd_z=`rmstd'/`rmstd_se'
			local rmstd_p=2*(1-normal(abs(`rmstd_z')))
			
			dis ""
			dis as result "    based on flexible parametric survival models:"
			dis as result string(`rmstd',"%4.3f") + " (" + string(`rmstd_lci',"%4.3f") + " to " + ///
				string(`rmstd_uci',"%4.3f") + "), p = " + string(`rmstd_p',"%5.4f") 
								  
			return scalar rmstd_fpm=`rmstd'
			return scalar rmstd_lci_fpm=`rmstd_lci'
			return scalar rmstd_uci_fpm=`rmstd_uci'
			return scalar rmstd_p_fpm=`rmstd_p'
			
			tempname B
			matrix `B'= `rmst0'[1], `rmst1'[1], `rmstd' \ `rmst0'_se[1] , `rmst1'_se[1] , `rmstd_se' \ ///
						`rmst0_z' , `rmst1_z' , `rmstd_z' \  `rmst0_p' , `rmst1_p' , `rmstd_p' \ ///
						`rmst0_lci', `rmst1_lci', `rmstd_lci' \ `rmst0_uci', `rmst1_uci', `rmstd_uci' 	
			mat colnames `B'= "`lb0'" "`lb1'" "difference"
			mat rownames `B' = b se t pvalue ll ul 
			return matrix table_fpm `B'
		}	
	
	}
	
	*3)  Pseudovalues
	********************	
	if "`pseudo'" != "" {
	
		tempname P M PM
		tempvar pmean50
		
		qui stpmean, at(`tmax') generate(`pmean50')
		
		qui glm `pmean50' i.`varlist'  if `touse',  family(gaussian) link(id) vce(robust)
		matrix `P' = r(table)
		
		qui margins `varlist'
		matrix `M' = r(table)
		
		matrix `PM' = `M'[1..6,1...] , `P'[1..6,2]
		
		local i=0
		foreach l of local lev {
			local i=`i'+1
			local lb`i': label (`varlist') `l'
			return scalar rmst`l'_pseudo=`M'[1,`i']
			return scalar rmst`l'_lci_pseudo=`M'[5,`i']
			return scalar rmst`l'_uci_pseudo=`M'[6,`i']
		
		}
	
			
			
		local rmstd = `P'[1,2]
		local rmstd_lci = `P'[5,2]
		local rmstd_uci = `P'[6,2]
		local rmstd_p = `P'[4,2]
		
		dis ""
		dis as result "    based on pseudovalues:"
		dis as result string(`rmstd',"%4.3f") + " (" + string(`rmstd_lci',"%4.3f") + " to " + ///
			string(`rmstd_uci',"%4.3f") + "), p = " + string(`rmstd_p',"%5.4f") 
		
		return scalar rmstd_pseudo=`rmstd'
		return scalar rmstd_lci_pseudo=`rmstd_lci'
		return scalar rmstd_uci_pseudo=`rmstd_uci'
		return scalar rmstd_p_pseudo=`rmstd_p'		
			
		mat colnames `PM'= "`lb0'" "`lb1'" "difference"
		mat rownames `PM' = b se t pvalue ll ul 
		return matrix table_pseudo `PM'
	
	}
	
}

end
