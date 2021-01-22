
** OPEN STATA VIA THE DO FILE (sets the working directory appropriately)

adopath ++ ".."

* help for btable
h btable

* help for btable_format
h btable_format





sysuse auto2, clear
br
replace price = . in 1


tempfile file1
tempfile file2


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


btable price, saving("`file1'")
btable price, saving("`file2'") by(foreign)

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




** formatting

btable_format using "`file1'"


btable_format using "`file2'"
