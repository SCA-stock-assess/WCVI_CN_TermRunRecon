# WCVI_CN_TermRunRecon
Data wrangling and functions to support annual WCVI chinook terminal run reconstruction. Does not replace Term Excel files. 

Major dependency on saaWeb package, download from here: https://github.com/Pacific-salmon-assess/saaWeb

## TermNIT workflow notes: 

01-catch-data.R
+ All of section 1 tables regarding catch data: marine rec, marine commercial, marine ESSR, f/w rec 
+ Age/stock comp data related to catch data 

02-escapement-sexAge-correction.R
+ Age correction for stratified age sampling of M and F to apply to non-catch data
+ Technically comes last in the original excel files, but needed prior to escapement section 

03-escapement.R
+ All non-catch escapement: broodstock (incl culls/morts/other), natural spawners 
+ Mainstem and tributaries
+ Requires sex/age correction 
+ Stock comp from broodstock 


Maintainer:
Katie Davidson
