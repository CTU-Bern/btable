
** OPEN STATA VIA THE DO FILE (sets the working directory appropriately)

adopath ++ ".."

* help for btable
h btable

* help for btable_format
h btable_format





sysuse auto2, clear
*br
replace price = . in 1
*recode foreign as per btable
clonevar foreign2 = foreign
recode foreign2 (1 = 0) (0 = 1)
tab foreign foreign2


tempfile file1
tempfile file2
tempfile file3


summ price, d
foreach i in "N" "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" {
	local `i' = r(`i')
	di "``i''"
}
local iqr = `p75' - `p25'
local range = `max' - `min'
local ntot = _N
local nnonmiss `N'

summ price if foreign == 0, d
foreach i in "N" "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" {
	local `i'0 = r(`i')
	di "``i'0'"
}
local iqr0 = `p750' - `p250'
local range0 = `max0' - `min0'
local nnonmiss0 `N0'
count if foreign == 0
local ntot1 = r(N)

* comparisons
ttest price, by(foreign)
local p_ttest = r(p)
local meand = r(mu_1) - r(mu_2)
local meand_lci = `meand' - invt(r(df_t), 0.975) * r(se)
local meand_uci = `meand' + invt(r(df_t), 0.975) * r(se)
ranksum price, by(foreign2)
local p_ranksum = r(p)
qreg price ib1.foreign
tempname A
matrix `A' = r(table)
local medd = `A'[1,1]
di "`medd'"
local medd_lci = `A'[5,1]
local medd_uci = `A'[6,1]
test 0.foreign
local p_qreg = r(p)
** HLMD
cendif price, by(foreign)
tempname A
matrix `A' = r(cimat)
local hlmd = `A'[1,2]
local hlmd_lci = `A'[1,3]
local hlmd_uci = `A'[1,4]
* lehmann
npshift price, by(foreign2)
local lehmannhlmd = r(theta)
local lehmannhlmd_lci = r(theta_l)
local lehmannhlmd_uci = r(theta_u)
tempfile lehmann
** MWS
* wmwTest default
wmwTest price, by(foreign2)
local mws = r(estimate)
local mws_lci = r(lci)
local mws_uci = r(uci)
ranksum price, by(foreign) porder
di "`mws' `= 1 - `= r(porder)''"
assert round(`mws', 4) == round(`= 1 - `= r(porder)'', 4)
* jackknife
somersd foreign2 price, transf(c) tdist
tempname A 
matrix `A' = r(table)
local jackknifemws = `A'[1,1]
local jackknifemws_lci = `A'[5,1]
local jackknifemws_uci = `A'[6,1]
tempfile jackknife
* delong
roctab foreign2 price
local delongmws=r(area)
local delongmws_lci=r(lb)
local delongmws_uci=r(ub)
tempfile delong
* bamber
roctab foreign2 price
local bambermws=r(area)
local bambermws_lci=r(lb)
local bambermws_uci=r(ub)
tempfile bamber
* hanley
roctab foreign2 price
local hanleymws=r(area)
local hanleymws_lci=r(lb)
local hanleymws_uci=r(ub)
tempfile hanley


btable price, saving("`file1'")
btable price, saving("`file2'") by(foreign)
btable price, saving("`lehmann'") by(foreign) hlmdci(lehmann)
foreach i in "jackknife" "delong" "bamber" "hanley" {
	btable price, saving("``i''") by(foreign) mwsci(`i')
}

* default
use "`file1'", clear

assert ntot_t == `ntot'
di "mean `mean'"

foreach i in "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" /*"iqr"*/ "range" {
	di "`i'_t == ``i''"
	assert round(`i'_t, 4) == round(``i'', 4)
}

assert varname == "price"
assert varlabel == "Price"
assert vtype == "conti"


use "`file2'", clear


assert ntot_t == `ntot'
di "mean `mean'"

foreach i in "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" "iqr" "range" "nnonmiss" {
	di "`i'_t == ``i''"
	assert round(`i'_t, 4) == round(``i'', 4)
}


assert ntot_1 == `ntot1'
di "mean `mean'"

foreach i in "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" "iqr" "range" "nnonmiss" {
	di "`i'_1 == ``i'0'"
	assert round(`i'_1, 4) == round(``i'0', 4)
}

assert varname == "price"
assert varlabel == "Price"
assert vtype == "conti"

* comparisons
foreach i in "p_ttest" "p_ranksum" "p_qreg" "meand" "meand_lci" "meand_uci" "medd" "medd_lci" "medd_uci" "hlmd" "hlmd_lci" "hlmd_uci" "mws" "mws_lci" "mws_uci" {
	di "`i' == ``i''"
	assert round(`i', 4) == round(``i'', 4)
}

use "`lehmann'", clear
assert ntot_t == `ntot'
di "mean `mean'"

foreach i in "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" "iqr" "range" "nnonmiss" {
	di "`i'_t == ``i''"
	assert round(`i'_t, 4) == round(``i'', 4)
}


assert ntot_1 == `ntot1'
di "mean `mean'"

foreach i in "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" "iqr" "range" "nnonmiss" {
	di "`i'_1 == ``i'0'"
	assert round(`i'_1, 4) == round(``i'0', 4)
}

assert varname == "price"
assert varlabel == "Price"
assert vtype == "conti"

* comparisons
foreach i in "p_ttest" "p_ranksum" "p_qreg" "meand" "meand_lci" "meand_uci" "medd" "medd_lci" "medd_uci" "mws" "mws_lci" "mws_uci" {
	di "`i' == ``i''"
	assert round(`i', 4) == round(``i'', 4)
}
foreach i in "hlmd" "hlmd_lci" "hlmd_uci"  {
	di "lehmann`i' == `lehmann`i''"
	cap assert `i' == `lehmann`i''
}

foreach i in "jackknife" "delong" "bamber" "hanley" {
	use "``i''", clear
	foreach j in "mws" "mws_lci" "mws_uci" {
		di "`i'`j' == ``i'`j''"
		assert round(`j', 4) == round(``i'`j'', 4)
	}
	
}

** formatting

btable_format using "`file1'"


btable_format using "`file2'"
