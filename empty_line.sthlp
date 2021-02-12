{smcl}
{* *! version 1.0.0  26jan2021}{...}
{hline}
{cmd:help empty_line}
{hline}

{title:Title}

{phang}
{bf:empty_line} {hline 2} Inserts empty observations (rows) in a data set.

{marker syntax}{...}
{title:Syntax}

{p 4 6 2}
{cmdab:empty_line} {ifin} [{cmd:,} {opth gen:erate(newvar)} {opth position(string)}]

{marker description}{...}
{title:Description}

{pstd}
{cmd:empty_line} creates empty observations (rows) within a data set 
	after or before observations specified by {cmd:if} or {cmd:in}.

{marker options}{...}
{title:Options}
{p 4 6 2}

{phang}
{opth generate(newvar)} creates new variable {it:newvar} containing {cmd:0} if the observation
originally appeared in the dataset and {cmd:1} if the observation is inserted.

{phang}
{opth position(string)}} specifies whether the observation is inserted {it:after}(default) or {it:before} 
	the observation specified by {cmd:in} or {cmd:if}.


{marker results}{...}
{title:Stored results}

{synoptset 22 tabbed}{...}
{p2col 5 22 19 2: Scalars}{p_end}
{synopt:{cmd: r(insert_line)}}Row at which the observation was inserted (if only one){p_end}

{marker examples}{...}
{title:Examples}

{pstd}Add an observation on top{p_end}
{phang2}{cmd:. sysuse auto}{p_end}
{phang2}{cmd:. empty_line in 1, position(before)}{p_end}
{phang2}{cmd:. replace make="Header" in `r(insert_line)'}{p_end}
{phang2}{cmd:. list in 1/5}{p_end}

{pstd}Add an observation before domestic and foreign cars{p_end}
{phang2}{cmd:. sysuse auto}{p_end}
{phang2}{cmd:. sort foreign make}{p_end}
{phang2}{cmd:. empty_line if foreign!=foreign[_n-1], gen(inserted) position(before)}{p_end}
{phang2}{cmd:. replace make="Domestic cars" if inserted==1 & foreign[_n+1]==0}{p_end}
{phang2}{cmd:. replace make="Foreign cars" if inserted==1 & foreign[_n+1]==1}{p_end}
{phang2}{cmd:. list}{p_end}
