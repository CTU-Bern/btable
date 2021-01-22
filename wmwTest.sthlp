{smcl}
{* *! version 1.0.0  26mar2017}{...}
{hline}
{cmd:help wmwTest}
{hline}

{title:Title}

{phang}
{bf:wmwTest} {hline 2} Asymptotic confidence interval for the Mann-Whitney statistic.


{marker syntax}{...}
{title:Syntax}

{p 4 6 2}
{cmdab:wmwTest} {varname} {ifin} {cmd:,} 
{cmd:by(}{it:{help varlist:groupvar}}{cmd:)} 
[{it:options}]

{synoptset 25 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{p2coldent:* {opth by:(varlist:groupvar)}}grouping variable{p_end}
{synopt:{opt phiNull(#)}}null hypothesis value for the test, default 0.5{p_end}
{synopt:{opt corr:ect}}include continuity correction {p_end}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt:{opth alternative(string)}}alternative hypothesis, either two-sided (default), greater or less{p_end}
{synopt:{opth vmethod(string)}}variance function for the asymptotic method, either LAPH (default) or PO{p_end}
{synopt:{opt notie:adjust}}remove tie adjustment, not recommended{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}* {opt by(groupvar)} is required.{p_end}

{marker description}{...}
{title:Description}

{pstd}
The Mann-Whitney statistic is the probability that the outcome is larger in a random 
	observation from one group (X) than in a random observation from the other (Y)
	plus half the probability that they are equal, i.e. phi = Pr[ X<Y] + 0.5 Pr[X=Y].
{cmd:wmwTest} gives an asymptotic confidence interval for the Mann-Whitney statistic 
	that is compatible with the Wilcoxon-Mann-Whitney test. 
	It was derived from function {it:wmwTest} of R-package {it:asht} ({help btable##Fay2020:Fay, 2020})
	based on {help btable##Fay2018:Fay, 2018}.
	

{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{cmd:by(}{it:{help varlist:groupvar}}{cmd:)} is required.  It specifies the name of the grouping variable.

{phang}
{opt phiNull(#)}} specifies the null hypothesis value for the test, the default is 0.5.

{phang}
{opt corr:ect} include continuity correction in the normal approximation for the p-value and confidence interval

{phang}
{opt l:evel(#)} set confidence level; default is {cmd:level(95)}

{phang}
{opt alternative(string)} alternative hypothesis, either {it:two-sided} (default), {it:greater} or {it:less}.

{phang}
{opt vmethod(string)} variance function for the asymptotic method, either {it:LAPH} (default) or {it:PO}. 
	{it:LAPH} gives the combination Lehmann alternative and proportional hazards method, 
	and {it:PO} gives the proportional odds method. 
	The methods are nearly identical, but the {it:PO}  method takes longer.

{phang}
{opt notie:adjust} removes the tie adjustment. Does not have an effect if 
	there are no ties. Generally not recommended but can 
		be used to reproduce method 5 of {help btable##Newcombe2006:Newcombe, 2006}.

{marker results}{...}
{title:Stored results}

{synoptset 22 tabbed}{...}
{p2col 5 22 19 2: Scalars}{p_end}
{synopt:{cmd:r(estimate)}}point estimate{p_end}
{synopt:{cmd:r(lci)}}lower confidence limit{p_end}
{synopt:{cmd:r(uci)}}upper confidence limit{p_end}
{synopt:{cmd:r(p)}}p-value{p_end}

{marker examples}{...}
{title:Examples}

{phang2}{cmd:. webuse fuel2}{p_end}
{phang2}{cmd:. wmwTest mpg, by(treat)}


{marker references}{...}
{title:References}

{marker Fay2018}{...}
{phang}
Fay, M. P. and Malinovsky, Y. 2018. 
Confidence intervals of the Mann-Whitney parameter that are compatible with the Wilcoxon-Mann-Whitney test. 
{it:Statistics in medicine} 1-16.

{marker Fay2020}{...}
{phang}
Fay, M. P. 2020. 
asht: Applied Statistical Hypothesis Tests. 
R package version 0.9.6. https://CRAN.R-project.org/package=asht

{marker Newcombe2006}{...}
{phang}
Newcombe, R. G. 2006.
Confidence intervals for an effect size measure based on the Mann-Whitney statistic. 
Part 2: asymptotic methods and evaluation.
{it: Statistics in medicine} 25(4): 559-573. 



