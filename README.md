# SENDsanitizer  

Based on example SEND Repeat-Dose Toxicity Studies, generates new studies that
follow similar dose responsive trends for modelling purposes. It currently
generates bw, dm, ds, ex, lb, mi, ta, ts, tx domains.

## Overview

A random number is generated for STUDYID and utilized across all domains during data creation. Dates replaced with 'XXXX-XX-XX' in relevant domains.

__TS, TA, DM, DS, TX, EX__  
In the TS domain, identifiable information like study facility details, location, study title, and vehicle names are replaced with pre-defined values ('FAKE FACILITY' for facility, 'XXXX-XX-XX' for dates). Details of study director, animal purchasing location, and test facility country are eliminated.

For the TA domain, STUDYID is substituted with a random number, and ARM is replaced with predefined values like Control, LD, MD, and HD for various doses.

In the DM domain, STUDYID is replaced with a randomly generated STUDYID, and USUBJID is changed to a unique number by concatenating the randomly generated STUDYID and SUBJID.

Within the DS domain, STUDYID, USUBJID, and dates are replaced as previously described. 

In TX domain the SET variable is updated with Control, LD, MD, or HD values, and group-level information is replaced with SET values (Control, LD, MD or HD) 
value of the parameter SPLRNAM, SSPONSOR, SPREFID and SPLRLOC which contain identifiable information in TXPARMCD were removed

EX domain 


__BW LB OM Domain__  

A Bayesian Regression model were build for each domain of BW, LB and OM using MCMCregress function from MCMCpack R package [MCMCpack: Markov Chain Monte Carlo
in R](https://doi.org/10.18637/jss.v042.i09). Numerical Data then generated using this model.

STUDYID, USUBJID, and dates are replaced as previously described within the LB, BW and OM domain.

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

