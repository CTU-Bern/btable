{smcl}
{* *! version 1.0.0  26mar2017}{...}
{hline}
{cmd:help wmwTest}
{hline}

{title:Title}

{phang}
{bf:wmwTest} {hline 2} Asymptotic confidence interval for the Mann-Whitney statistic adapted from R-package asht.


{marker syntax}{...}
{title:Syntax}

{p 4 6 2}
{cmdab:wmwTest} {varname} {ifin} {cmd:,} 
{opth by(varname)} 
[{it:options}]

{synoptset 35 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{p2coldent:* {opth by:(varlist:groupvar)}}grouping variable{p_end}
{synopt:{opt phiNull(#)}}null hypothesis value for the Mann-Whitney statistic, default 0.5{p_end}
{synopt:{opt corr:ect}}include continuity correction {p_end}
{synopt:{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt:{opth alternative(string)}}alternative hypothesis, either two-sided (default), greater or less{p_end}
{synopt:{opt notie:adjust}}remove tie adjustment, not recommended{p_end}
{synopt:{opth vmethod(string)}} variance function for the asymptotic method, either LAPH (default) or PO{p_end}

