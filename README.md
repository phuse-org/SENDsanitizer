# SENDsanitizer  

Based on example SEND Repeat-Dose Toxicity Studies, generates new studies that
follow similar dose responsive trends for modelling purposes. It currently
generates bw, dm, ds, ex, lb, mi, ta, ts, tx domains.

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




How to Use: The script is designed to run out of one folder on your computer.
Place SEND format studies that you wish to use as examples in that folder and
open 'app.R'. Run the script and follow instructions in the side bar to load
exmaple studies into the app. Pressing the button at the bottom of the Sidebar
will start study generation. Generated 'sanitizied' SEND studies will display on
the left of the app and can be exported into folders of .xpt files that will
appear in the same directory as the app.R file.

Notes on Example SEND Studies: It is recommended to use multiple example studies
for better results. These SEND format example studies must have similar
arms/dosing regimens (with the option to include or exclude recovery animals),
SEND Version, and have the same SSTYP and species. The script checks for these
values to be similar and will provide errors based on which of these conditions
is not met.

Benchmarking: App will print plots of High Dose Male Animals generated vs. the
examples' values for BW, ALB, RBC, and SPGRAV as benchmarking in the Rstudio
console automatically. These plots can be expanded to other values as well.
