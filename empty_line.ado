*! version 1.0.1 28apr2021
cap program drop empty_line
program empty_line, rclass 
version 16
syntax [if] [in] [, Generate(name) POSition(string)]

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
	