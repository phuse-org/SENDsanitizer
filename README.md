# SENDsanitizer  

__SENDsanitizer__ is an __R__ __package__ to generate synthetic data from real data. 

## Overview
The Standard for Exchange of Nonclinical Data (__SEND__), developed by the Clinical
Data Interchange Standards Consortium (CDISC), offers a __structured__ __electronic__
__format__  to organize and exchange __nonclinical__ study data among sponsor companies,
contract research organizations (CROs), and health authorities.

SENDsanitizer is an R package designed to generate synthetic SEND-formatted data
by modifying real SEND-formatted  datasets. It anonymizes the data by replacing
sensitive information like dates and other specific details with predefined
text, ensuring that  identities cannot be traced. Additionally, potentially
identifiable data elements are removed entirely to maintain privacy.
For numerical values, SENDsanitizer generates synthetic
data using Bayesian regression model.   

## Installation  

Development version can be installed from GitHub.

```
# install devtools if already not installed 
install.packages("devtools")

#install toxSummary package
devtools::install_github('phuse-org/SENDsanitizer')
```

#### To generate one synthetic study dataset from one real study  

```
library(SENDsanitizer)
SENDsanitizer::sanitize(path='path/to/directory/of/xpt/files/of/study/',
where_to_save='path/to/directory/where/generated/files/should/be/saved/')

```



##### input  
Following domain data must be in a direcotry to generate synthetic data.  
```
study_dir/  
  - ds.xpt
  - pc.xpt
  - tx.xpt
  - ts.xpt
  - dm.xpt
  - bw.xpt
  - lb.xpt
  - mi.xpt
  - om.xpt
```
##### output   
Synthetic data will be generated for following domain:  
```
 save_dir/  
  - tx.xpt
  - ts.xpt
  - dm.xpt
  - bw.xpt
  - lb.xpt
  - mi.xpt
  - om.xpt

```
#### To generate one synthetic study dataset from multiple real study  

```
library(SENDsanitizer)
study_01 <- 'path/to/directory/of/xpt/files/of/study_01/'
study_02 <- 'path/to/directory/of/xpt/files/of/study_02/'
multiple_studies <- c(study_01,study_02)
SENDsanitizer::sanitize(path= multiple_studies,
where_to_save='path/to/directory/where/generated/files/should/be/saved/')

```

### Install from cloned repo  


Clone the [GitHub](https://github.com/phuse-org/SENDsanitizer) repo
and set repo as working directory.

```
devtools::load_all(".")
SENDsanitizer::sanitize(path='path/to/directory/of/xpt/files/of/study/',
where_to_save='path/to/directory/where/generated/files/should/be/saved/')
```


Notes on Example SEND Studies:  
It is recommended to use multiple example studies for better results.  These
SEND format example studies must have similar arms/dosing regimens (with the
option to include or exclude recovery animals), SEND Version, and have the same
SSTYP and species. The script checks for these values to be similar and will
provide errors based on which of these conditions is not met.

