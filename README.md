# SENDsanitizer
Based on example SEND Repeat-Dose Toxicity Studies, generates new studies that follow similar dose responsive trends for modelling purposes. It currently generates bw, dm, ds, ex, lb, mi, ta, ts, tx domains. 

## How to Use:
The script is designed to run out of one folder on your computer. Place SEND format studies that you wish to use as examples in that folder and open 'app.R'. Run the script and follow instructions in the side bar to load exmaple studies into the app. Pressing the button at the bottom of the Sidebar will start study generation. Generated 'sanitizied' SEND studies will display on the left of the app and can be exported into folders of .xpt files that will appear in the same directory as the app.R file.

## Notes on Example SEND Studies:
It is recommended to use multiple example studies for better results. These SEND format exmaple studies must have similar arms/dosing regimens (with the option to include or exclude recovery animals), SEND Version, and have the same SSTYP and species. The script checks for these values to be similar and will provide errors based on which of these conditions is not met. 
