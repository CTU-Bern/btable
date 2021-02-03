* MASTERFILE FOR RUNNING TESTS
** OPEN STATA VIA THE DO FILE (sets the working directory appropriately)



* load btable
adopath ++ ".."


* generic tests for help files
* help for btable
h btable

* help for btable_format
h btable_format


* specific tests
do 01_conti.do

