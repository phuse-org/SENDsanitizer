# SENDsanitizer  

Based on example SEND Repeat-Dose Toxicity Studies, generates new studies that
follow similar dose responsive trends for modelling purposes. It currently
generates ts, dm, tx, bw, lb, om, mi domains. 

## Overview

A random number is generated for STUDYID and utilized across all domains during data creation. Dates replaced with 'XXXX-XX-XX' in relevant domains.

__TS,  DM,  TX__
In the TS domain, identifiable information like study facility details, location, study title, and vehicle names are replaced with pre-defined values ('FAKE FACILITY' for facility, 'XXXX-XX-XX' for dates). Details of study director, animal purchasing location, and test facility country are eliminated.

In the DM domain, STUDYID is replaced with a randomly generated STUDYID, and USUBJID is changed to a unique number by concatenating the randomly generated STUDYID and SUBJID. ARM replaced with Control, LD, MD, or HD as appropriate.
ARMCD replaced with dose order. Where dose order defines in following way   
Control to 1  
LD to 2  
MD to 3  
HD to 4  

In TX domain, the SET variable is updated with Control, LD, MD, or HD values.
TXPARMCD Variable:  
group label (GRPLBL) information is replaced by concatenating dose order and dose value (i.e. __Group 1, Control__ where 1 is order and Control is the dose). 
TRTDOS is replaced with dose (Control, LD, MD, or HD). ARMCD and SPGRPCD  replaced with dose order (1,2,3,4).  
Value of the parameter SPLRNAM, SSPONSOR, SPREFID and SPLRLOC which contain identifiable information in TXPARMCD were removed  


__BW LB OM Domain__  

A Bayesian Regression model were build for each domain of BW, LB and OM using MCMCregress function from MCMCpack R package [MCMCpack: Markov Chain Monte Carlo
in R](https://doi.org/10.18637/jss.v042.i09). Numerical Data then generated using this model.

STUDYID, USUBJID, and dates are replaced as previously described within the LB, BW and OM domain.

__LB__ 
A group of Control, male rats whrere LBCAT is __CLINICAL CHEMISTRY__ choosen.
Mean of all LBTEST (PHOS, CA, CHOL, BIL, ALP etc) calculated for that group.  

let's say following table have Mean of LBTESTCD.   
 
| LBTESTCD  |MEAN|
|:----------|:---|
|ALT        |58  |
|PROT       |60  |
|ALB        |36  |
|CL         |105 |
|GLUC       |7   |

To build a model for __ALT__, algorithmically we choose nearest two value, in that case we choose __PROT__ and __ALB__  

`MCMCpack::MCMCregress("ALT ~ PROT + ALB", b0=0,B0=0.1, data= line)`  

__line__ is the dataframe of LBSTRESN and LBTESTCD  

| ALT  |PROT |ALB |CL  |GLUC |
|:-----|:----|:---|:---|:----|
|58    |59   |36  |105 |6    |
|59    |60   |37  |104 |9    |
|57    |61   |35  |106 |6    |


Markov Chain Monte Carlo (MCMC) output:  

|Intercept  |PROT     |ALB      |sigma2  |
|:----------|:--------|:--------|:-------|
|0.05027991 |2.3874235|-1.956665|327.2743|
|-4.40402458|1.9721626|-1.627903|682.1745|
|0.31785474 |2.9303049|-3.017695|535.8846|


randomly choosen a model and calculate the __ALT__ value for a specific animal.  

`ALT = Intercept + Cof_PROT * PROT + cof_ALB * ALB`  

- some noise added

`noise <- stats::rnorm(1,mean=0,sd=(standard_deviation_of_that_group_for_ALT)`  

`final_value = ALT + noise`  

In case value returned negative, algorithmically a different model choosen. Code 
try that max 10 times. If it fails, absolute value is taken as final value.   

__MI domain__   

percentage of the incidence and serverity was calculated.  


## Installation  

Development version can be installed from GitHub.

```
# install devtools if already not installed 
install.packages("devtools")

#install toxSummary package
devtools::install_github('phuse-org/SENDsanitizer')
```

## Generate fake xpt files  

```
library(SENDsanitizer)
SENDsanitizer::sanitize(path='path/to/directory/of/xpt/files/of/study/',
nubmer=1,
where_to_save='path/to/directory/where/generated/files/should/save/')

```


## Install from cloned repo  


Clone the [GitHub](https://github.com/phuse-org/SENDsanitizer) repo
and set repo as working directory.

```
devtools::load_all(".")
SENDsanitizer::sanitize(path='path/to/directory/of/xpt/files/of/study/',
nubmer=1,
where_to_save='path/to/directory/where/generated/files/should/save/')
```


Notes on Example SEND Studies:
It is recommended to use multiple example studies for better results.  These
SEND format example studies must have similar arms/dosing regimens (with the
option to include or exclude recovery animals), SEND Version, and have the same
SSTYP and species. The script checks for these values to be similar and will
provide errors based on which of these conditions is not met.

