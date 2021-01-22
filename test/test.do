
** OPEN STATA VIA THE DO FILE (sets the working directory appropriately)

adopath ++ ".."

* help for btable
h btable

* help for btable_format
h btable_format





sysuse auto2, clear
br


tempfile file1
tempfile file2


summ price, d
foreach i in "N" "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" {
	local `i' = r(`i')
	di "``i''"
}
local iqr = `p75' - `p25'
local range = `max' - `min'

summ price if foreign == 0, d
foreach i in "N" "mean" "sd" "min" "max" "p25" "p50" "p75" "sum" {
	local `i'0 = r(`i')
	di "``i'0'"
}
local iqr = `p750' - `p250'
local range = `max0' - `min0'


btable price, saving("`file1'")
btable price, saving("`file2'") by(foreign)

* default
use "`file1'", clear

assert ntot_t == `N'
di "mean `mean'"
assert round(mean_t, 4) == round(`mean', 4)
di "sd `sd'"
assert round(sd_t, 4) == round(`sd', 4)
di "p50 `p50'"
assert round(p50_t, 4) == round(`p50', 4)
di "p25 `p25'"
assert round(p25_t, 4) == round(`p25', 4)
di "p75 `p75'"
assert round(p75_t, 4) == round(`p75', 4)
di "iqr `iqr'"
*assert round(iqr_t, 4) == round(`iqr', 4)
di "min `min'"
assert round(min_t, 4) == round(`min', 4)
di "max `max'"
assert round(max_t, 4) == round(`max', 4)
di "range `range'"
assert round(range_t, 4) == round(`range', 4)
di "sum `sum'"
assert round(sum_t, 4) == round(`sum', 4)




use "`file2'", clear


assert ntot_t == `N'
di "mean `mean'"
assert round(mean_t, 4) == round(`mean', 4)
di "sd `sd'"
assert round(sd_t, 4) == round(`sd', 4)
di "p50 `p50'"
assert round(p50_t, 4) == round(`p50', 4)
di "p25 `p25'"
assert round(p25_t, 4) == round(`p25', 4)
di "p75 `p75'"
assert round(p75_t, 4) == round(`p75', 4)
di "iqr `iqr'"
*assert round(iqr_t, 4) == round(`iqr', 4)
di "min `min'"
assert round(min_t, 4) == round(`min', 4)
di "max `max'"
assert round(max_t, 4) == round(`max', 4)
di "range `range'"
assert round(range_t, 4) == round(`range', 4)
di "sum `sum'"
assert round(sum_t, 4) == round(`sum', 4)


assert ntot_1 == `N0'
di "mean `mean'"
assert round(mean_1, 4) == round(`mean0', 4)
di "sd `sd'"
assert round(sd_1, 4) == round(`sd0', 4)
di "p50 `p50'"
assert round(p50_1, 4) == round(`p500', 4)
di "p25 `p25'"
assert round(p25_1, 4) == round(`p250', 4)
di "p75 `p75'"
assert round(p75_1, 4) == round(`p750', 4)
di "iqr `iqr'"
assert round(iqr_1, 4) == round(`iqr0', 4)
di "min `min'"
assert round(min_1, 4) == round(`min0', 4)
di "max `max'"
assert round(max_1, 4) == round(`max0', 4)
di "range `range'"
assert round(range_1, 4) == round(`range0', 4)
di "sum `sum'"
assert round(sum_1, 4) == round(`sum0', 4)





btable_format using "`file1'"


btable_format using "`file2'"
