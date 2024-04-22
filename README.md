# SENDsanitizer  

Based on example SEND Repeat-Dose Toxicity Studies, generates new studies that
follow similar dose responsive trends for modelling purposes. It currently
generates bw, dm, ds, ex, lb, mi, ta, ts, tx domains.

## Overview
scripts generate a random number for STUDYID to use in different domains.
All indentifiable information is removed from study given. 
All date information is replaced with 'XXXX-XX-XX' where appropriate.

## MCMC method

how we are using to generate data?


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

