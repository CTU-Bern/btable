_v. 1.1.0_  

`btable` <img src='sticker.png' align="right" height="200">
========

Summary tables are frequently constructed during data analysis either to get an overview 
over the dataset or for a report or publication. 
They may e.g. include patient baseline characteristics—commonly found as table 1 in biomedical publications—or
outcome variables with effect measures and p-values.

The btable-package can summarize continuous, categorical, count and time-to-event variables 
within one table using various descriptives. 
The summary can be grouped using a grouping variable, 
and various effect measures with confidence intervals and p-values can be calculated. 

The table is constructed in a two-step approach using two functions: 
`btable` produces an unformatted, raw table, which is then formatted by 
`btable_format` to produce a final, publication-ready table. 
By default, the raw table contains all summary measures, and—if there are two groups—effect measures and p-values. 
Optionally, the table can be restricted to effect measures of choice and a number of alternative calculations 
for confidence intervals are available. 	


Installation
------------

In order to install `btable` from github the github-package is required:

	net install github, from("https://haghish.github.io/github/")

You can then install the development version of `btable` with:

	github install CTU-Bern/btable


Example
------------

	# load example dataset
	sysuse auto2
	
	# generate table
	tempfile rawtab
	btable price mpg rep78 headroom, by(foreign) saving("`rawtab'")
	
	# format table 
	btable_format using "`rawtab'", clear
	

Author
------

**Lukas Bütikofer**  
CTU Bern  
lukas.buetikofer@ctu.unibe.ch  
<https://github.com/CTU-Bern/btable>  
