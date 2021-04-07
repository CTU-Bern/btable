{smcl}
{* *! version 1.0.0  26jan2021}{...}
{hline}
{cmd:help RMST}
{hline}

{title:Title}

{phang}
{bf:RMST} {hline 2} Calculates restricted mean survival time and its difference between groups.

{marker syntax}{...}
{title:Syntax}

{p 4 6 2}
{cmdab:RMST} [{it:{help varlist:groupvar}}] {ifin} 
[, {it:options}]

{synoptset 25 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{opt tmax(#)}}truncation time at which the RMST is calculated{p_end}
{synopt:{opt npar}}non-parametric method based on the Kaplan-Meier estimator {p_end}
{synopt:{opt fpm}}flexible parametric survival model{p_end}
{synopt:{opt pseudo}}pseudovalue method{p_end}
{synopt:{opt df(#)}}degrees of freedom for flexible parametric survival model, 3 by default {p_end}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
You must {cmd:stset} your data before using {cmd:RMST}; see {manhelp stset ST}.{p_end}

{marker description}{...}
{title:Description}

{pstd}
{cmd:RMST} Calculates the restricted mean survival time and its difference between groups 
	(if a {it:groupvar} with two levels is given) on st data.
	Three methods are used by default:
	non-parametric integration of Kaplan-Meier curves,
	flexible parametric survival models ({help btable##Royston2013:Royston, 2013}, {help btable##Royston2011:Royston, 2011}),
	and the pseudovalue method described by {help btable##Parner2010:Parner, 2010}.

	
{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opt tmax(#)}} specifies the restriction or truncation time at which the restricted mean survival time is
	calculated. If not given, the longest observed event time (if {cmd:groupvar} is not specified)
		or the minimum of the longest observed event times in both groups is used.

{phang}
{opt npar} specifies that the restricted mean survival time is calculated using 
	non-parametric integration of Kaplan-Meier curves via {help stci} is used.

{phang}
{opt fpm} specifies that the restricted mean survival time is calculated using 
	flexible parametric survival models via {help stpm2} 
		({help btable##Royston2013:Royston, 2013}, {help btable##Royston2011:Royston, 2011}).

{phang}
{opt pseudo} specifies that the restricted mean survival time is calculated using 
	the pseudovalue method desctibed by {help btable##Parner2010:Parner, 2010}.

{phang}
{opt df(#)} specifies the degrees of freedom for flexible parametric survival models, default is 3.

{phang}
{opt l:evel(#)} set confidence level; default is {cmd:level(95)}


{marker results}{...}
{title:Stored results}

{synoptset 22 tabbed}{...}
{p2col 5 22 19 2: Scalars}{p_end}
{synopt:{cmd:r(rmstd_p_pseudo)}}p-value for comparison based on pseudovalues{p_end}
{synopt:{cmd:r(rmstd_uci_pseudo)}}lower confidence limit for difference between groups based on pseudovalues{p_end}
{synopt:{cmd:r(rmstd_lci_pseudo)}}upper confidence limit for difference between groups based on pseudovalues{p_end}
{synopt:{cmd:r(rmstd_pseudo)}}difference between groups based on pseudovalues{p_end}
{synopt:{cmd:r(rmst1_uci_pseudo)}}upper confidence limit for second group based on pseudovalues{p_end}
{synopt:{cmd:r(rmst1_lci_pseudo)}}lower confidence limit for second group based on pseudovalues{p_end}
{synopt:{cmd:r(rmst1_pseudo)}}point estimate for group second group on pseudovalues{p_end}
{synopt:{cmd:r(rmst0_uci_pseudo)}}upper confidence limit for first group based on pseudovalues{p_end}
{synopt:{cmd:r(rmst0_lci_pseudo)}}lower confidence limit for first group based on pseudovalues{p_end}
{synopt:{cmd:r(rmst0_pseudo)}}point estimate for first group based on pseudovalues{p_end}
{synopt:{cmd:r(rmstd_p_fpm)}}p-value for comparison based on flexible parametric survival models {p_end}
{synopt:{cmd:r(rmstd_uci_fpm)}}lower confidence limit for difference between groups based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmstd_lci_fpm)}}upper confidence limit for difference between groups based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmstd_fpm)}}difference between groups based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst1_uci_fpm)}}upper confidence limit for second group based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst1_lci_fpm)}}lower confidence limit for second group based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst1_fpm)}}point estimate for group second group on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst0_uci_fpm)}}}upper confidence limit for first group based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst0_lci_fpm)}}lower confidence limit for first group based on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmst0_fpm)}}point estimate for group second group on flexible parametric survival models{p_end}
{synopt:{cmd:r(rmstd_p_npar)}}p-value for comparison based on non-parametric method{p_end}
{synopt:{cmd:r(rmstd_uci_npar)}}lower confidence limit for difference between groups based on non-parametric method{p_end}
{synopt:{cmd:r(rmstd_lci_npar)}}upper confidence limit for difference between groups based on non-parametric method{p_end}
{synopt:{cmd:r(rmstd_npar)}}difference between groups based on non-parametric method{p_end}
{synopt:{cmd:r(rmst1_uci_npar)}}upper confidence limit for second group based on non-parametric method{p_end}
{synopt:{cmd:r(rmst1_lci_npar)}}lower confidence limit for second group based on non-parametric method{p_end}
{synopt:{cmd:r(rmst1_npar)}}point estimate for group second group on non-parametric method{p_end}
{synopt:{cmd:r(rmst0_uci_npar)}}}upper confidence limit for first group based on non-parametric method{p_end}
{synopt:{cmd:r(rmst0_lci_npar)}}lower confidence limit for first group based on non-parametric method{p_end}
{synopt:{cmd:r(rmst0_npar)}}point estimate for group second group on non-parametric method{p_end}
{synopt:{cmd:r(tmax)}}truncation time

{p2col 5 23 26 2: Matrices}{p_end}
{synopt:{cmd:r(table_pseudo)}}all results based on  pseudovalues{p_end}
{synopt:{cmd:r(table_fpm)}}all results based on  flexible parametric survival models{p_end}
{synopt:{cmd:e(r(table_npar))}}all results based on non-parametric method{p_end}


{marker examples}{...}
{title:Examples}

{phang2}{cmd:. webuse drugtr}{p_end}
{phang2}{cmd:. stset}

{pstd}Overall{p_end}
{phang2}{cmd:. RMST}

{pstd}Difference between groups{p_end}
{phang2}{cmd:. RMST drug}

{marker references}{...}
{title:References}

{marker Parner2010}
{phang}
Parner, E.T. and Andersen, P. K. 2010
Regression analysis of censored data using pseudo-observations
{it: The Stata Journal} 10:3
	
{marker Royston2013}
{phang}
Royston, P. & Parmar, M. K. 2013.
Restricted mean survival time: an alternative to the hazard ratio for the design and analysis of randomized trials with a time-to-event outcome 
{it:BMC Med Res Methodol} 13.

{marker Royston2011}
{phang}
Royston, P. & Parmar, M. K. 2011.
The use of restricted mean survival time to estimate the treatment effect in randomized clinical trials 
when the proportional hazards assumption is in doubt 
{it: Statistics in medicine} 30, 2409-2421.


