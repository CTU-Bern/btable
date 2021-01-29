*! version 1.0.0 26jan2021
cap program drop wmwTest
program wmwTest, rclass
version 15

syntax varname [if] [in], by(varname) ///
	[phiNull(string) CORRect alternative(string) Level(cilevel) NOTIEadjust vmethod(string) ]

tempname glev
tempname touse

marksample `touse', novarlist

*ignore missings in by variable	
cap assert !missing(`by')
if _rc {
	//dis "Missings in by variable will be ignored"
	cap replace ``touse''=0 if missing(`by')
}

*ignore missings in varname
cap assert !missing(`varlist')
if _rc {
	//dis as text "Missings in `varlist' will be ignored"
	cap replace ``touse''=0 if missing(`varlist')
}
	
	
*load moremata for roots
cap mata mata which mm_root()
if _rc {
    di as error "mm_root() from -moremata- is required; type -ssc install moremata- to obtain it"
    exit 499
}

*null hyopthesis for test, default: 0.5
if "`phiNull'"=="" {
	local phi0 0.5
} 
else {
	local phi0 `phiNull'
}

*continuity correction if correct is included
if "`correct'"=="" {
	local correct 0
}
else {
	local correct 1
}

*variance function for the asymptotic method, LAPH or PO, very similar he first is faster
if "`vmethod'"=="" | "`vmethod'" == "LAPH" {
	local vmeth 0
} 
if "`vmethod'"=="PO" {
	local vmeth 1
}
if "`vmethod'"!="" & !inlist("`vmethod'","LAPH","PO") {
	dis as text "Invalid vmethod, default (LAPH) used"
	local vmeth 0
}
assert inlist(`vmeth',0,1)

*alternative
if "`alternative'"=="" | "`alternative'"=="two-sided" {
	local alt 0
}
if  "`alternative'"=="greater" {
	local alt 1
}
if  "`alternative'"=="less" {
	local alt 2
}
if  "`alternative'"!="" & !inlist("`alternative'","two-sided","greater","less") {
	dis as text "Invalid alternative, only two-sided, greater or less allowed. Default two-sided used."
	local alt 0
}

*level
if "`level'"=="" {
	local level 95
}
local alpha = (100 - `level')/100


qui levelsof `by' if ``touse'', local(`glev')
local wc: word count ``glev''
assert `wc'==2
forvalues i=1/`wc' {
	local l`i': word `i' of ``glev''
	qui count if `by'==`l`i'' & ``touse''
	local n`i'=`r(N)'
}
qui count if ``touse''
local n=`r(N)'
assert `n'==`n1' + `n2'

tempvar r
qui egen `r'=rank(`varlist') if ``touse''
qui sum `r' if `by'==`l2' & ``touse''
local sum2 = `r(sum)'
local phi = (`sum2' - `n2' * (`n2' + 1)/2)/(`n2' * `n1')


preserve
qui keep if ``touse''
tempvar nties ntiesd
qui duplicates tag `r', gen(`nties')
qui replace `nties'=`nties'+1
qui duplicates drop `r', force
qui gen `ntiesd'=`nties'^3-`nties'
qui sum `ntiesd'
local ntiess=`r(sum)'
restore
local tiefactor=1-`ntiess'/(`n'*(`n'+1)*(`n'-1))

if "`notieadjust'" != "" {
	if (`tiefactor' < 1) {
		dis as error "tie factor removed, but there are ties in the data."
	}	
    local tiefactor = 1	
}

if (`tiefactor' == 0) {
	local lci = 0
	local uci = 1
    local pv = 1
}
else {
	local corrless=0
	local corrgr=0
	if (`correct'==1) {
		local corrless = -0.5/(`n2'* `n1')
		local corrgr = 0.5/(`n2' * `n1')
	}
	if (`vmeth' == 0) {
        local v = `tiefactor' * (`phi0' * (1 - `phi0')/(`n2' * `n1')) *  ///
			(1 + ((`n2' + `n1' - 2)/2) * ((1 - `phi0')/(2 - `phi0') + `phi0'/(1 + `phi0')))	  
	}
	else {
		 //Vpo(PHI, tf, ny, nx)
		 //?Vpo
		 //more complicated not implemented at the moment
		 dis as error "vmethod PO not implemented at the moment"
		 exit, clear
	}
	
	local z0Less = (`phi' - `phi0' - `corrless')/sqrt(`v')
	local z0Gr = (`phi' - `phi0' - `corrgr')/sqrt(`v')	
	
	if `alt'==0 {
		local pv=2*min(normal(`z0Less'),1-normal(`z0Gr'))
	}
	if `alt'==1 {
		 local pv=1-normal(`z0Gr') 
	}
	if `alt'==2 {
		 local pv=normal(`z0Less')
	}
	local epsilon = 10^(-8)
	local phimin = `epsilon'
    local phimax = 1 - `epsilon'
	if `alt'==0 {
		local zql=invnormal(`alpha'/2)
		local zqu=invnormal(1-`alpha'/2)
	}
	if inlist(`alt',1,2) {
		local zql=invnormal(`alpha')
		local zqu=invnormal(1-`alpha')
	}

	tempname A CI
	matrix `A'=`phi',`tiefactor',`corrgr',`corrless',`n1',`n2',`zql',`zqu',`phimin',`phimax',`epsilon'
	qui mata: mci("`A'","`CI'")
	
	if `alt'==0 {
		local lci =  `CI'[1,1]
		local uci =  `CI'[1,2]	
		local lcis=string(`lci',"%10.4f")
		local ucis=string(`uci',"%10.4f")
	}
	if `alt'==1 {
		local lci = `CI'[1,1]
		local uci = 1
		local lcis=string(`lci',"%10.4f")
		local ucis=string(`uci',"%10.4f")
	}
	if `alt'==2 {
		local lci = 0
		local uci  = `CI'[1,2] 
		local lcis=string(`lci',"%10.4f")
		local ucis=string(`uci',"%10.4f")
	}
	
}

local phis=string(`phi',"%10.4f")
//dis as text "Mann-Whitney statistic:" 
//dis as text "`phis' (`lcis' to `ucis')"
//dis as text "P-value = `=string(`pv'),"%4.4f")'"

return scalar p=`pv'
return scalar uci=`uci'
return scalar lci=`lci'
return scalar estimate=`phi'

end


capture mata mata drop mci
clear mata
	mata:
	void mci(string scalar vname, string scalar CI) 
	{
		A = st_matrix(vname)
		PHI = A[1,1]
		TF = A[1,2]
		CORRGR = A[1,3]
		CORRLESS =  A[1,4]
		nx = A[1,5]
		ny = A[1,6]
		ZQL = A[1,7]
		ZQU = A[1,8]
		PHIMIN = A[1,9]
		PHIMAX = A[1,10]
		EPSILON=A[1,11]
		
		mm_root(x=., &myfunc_lci(), PHIMIN, PHIMAX, EPSILON, 1000, PHI, CORRGR, TF, nx, ny, ZQU)
		lci=x
		mm_root(x=., &myfunc_uci(), PHIMIN, PHIMAX, EPSILON, 1000, PHI, CORRLESS, TF, nx, ny, ZQL)
		uci=x
		ma=lci, uci
		st_matrix(CI, ma)
	}		
end

mata:	
	function myfunc_lci(x, PHI, CORRGR, TF, nx, ny, ZQU) ///
		return((PHI - x - CORRGR)/ ///
		sqrt(TF * (x * (1 - x)/(ny * nx)) * (1 + ((ny + nx - 2)/2) * ((1 - x)/(2 - x) + x/(1 + x)))) ///
		- ZQU)
end		

mata:
	function myfunc_uci(x, PHI, CORRLESS, TF, nx, ny, ZQL) ///
		return((PHI - x - CORRLESS)/ ///
		sqrt(TF * (x * (1 - x)/(ny * nx)) * (1 + ((ny + nx - 2)/2) * ((1 - x)/(2 - x) + x/(1 + x)))) ///
		- ZQL)
end
