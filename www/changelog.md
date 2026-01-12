---
title: "Shiny Fish-Assess Changelog"
---

*Issues*


*Feature Requests*  
LAMBDAs?  
Discarding by number.

tv allows for one block only - make a separate block for each parameter  
nsamples to be calculated rather than just be sum of fields  
catch_ts <- include the error  
discard fleets (like in Dhufish extended model)  
meanbodysizedata (like in Dhufish extended model)  
discards from biological data (RAP)
Dome-shaped selectivity
Spline selectivity
Extra comp data

Multi-area models and parameterisation e.g.  
  Recruitment distribution - RecrDist_GP_1_area_1, RecrDist_GP_1_area_2 (by area)  


*Dhufish Extended model app progress*  
Some minor sample selection.  
RAP logbook data (retained + released fish) - need to add this as a fleet??  
1996+1997 (Alex data)  
Meant Wt info - WANT TO CHECK WITH ANDRE IF THIS IS WORTH INCLUDING  
Options to fix Retention (and add custom values) - ALSO - Emily has a seperate chunk for each fleet, I have them mirrored  
Discard mortality lines  
Forecast file - Info not added to app  


*SCD Progress*  
Snapper  
Bight Redfish  
Hapuku  
Blue morwong  
Western blue groper  

Catch data to 2022 - M:\\Fisheries Research\\FinFish\\South Coast Demersal\\Stock Assessments\\2018 & 2019 Sampling\\Catch graphs\\Catch data to 2021
File: Commercial catch data to 2022.xlsx  

Catch rates to 2022 - M:\\Fisheries Research\\FinFish\\South Coast Demersal\\Stock Assessments\\2018 & 2019 Sampling\\Catch rates

...seems old, perhaps try

M:\\Fisheries Research\FinFish\\South Coast Demersal\SRFAR\\2024-25  
File: Graphs for SoFAR SCD to 2023-24.JNB  

__v0.80__  
Comp data that has sexes and some NAs. App now uses both - Andre suggests to do this.
Added model comparison section - Francis and Dirichlet
Added bias and tuning section
Changed catch_ts format to long, so it matches effort and is better for migration into SQL



__v0.79__  
Charter Length data  
Added option to subset length data by Sector  
Parallel processing for multiple SS runs  
Data subset logic fixes, to improve speed  

__v0.78__  
Added senstivities tab for jitter, r0 profile, h profile, m profile and retrospective analysis. All running in parallel!  
Fixed up outputs to include all ss files and contents of zip. r4ss stuff now in subdir called r4ss.  
Auto rename SS input files from control.ctl and data.dat to controlfile.ctl and datafile.dat, to match project naming conventions.

__v0.77__  
Restoring app statement via zips working, although sometimes odd behaviour  
Fixed Biological Parameters table issue of not rendering when some parameters missing  
Fixed plot height rendering issue for Biological length plot  
Included SCD_data, age data needs CONFIRMATION  

*NDSF Goldband Base Case*  
Keep Sex info out of the age data

Variable to add to app

Should be:  

3 #_Age(post-settlement)_for_L1;linear growth below this
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)

BUNCH OF BIO PARAMS

2021 # last year of main recr_devs; forecast devs start in following year #UPDATED

        #   0   3  0.1    0.181    0.2        6      3  #InitF_seas_1_fleet_1

1	0	0	0	#_FISTrap
15	0	0	1	#_Foreign_trawl   # Foregin fishing before fishery established and no comp data
15	0	0	1	#_Foreign_line    # Foregin fishing before fishery established and no comp data
15	0	0	1	#_Comm_trapMnthly
15	0	0	1	#_Comm_trapDaily 
15	0	0	1	#_Comm_line       # no line comp data, no info about retention
15	0	0	1	#_Rec_line        # no line comp data, no info about retention
15	0	0	1	#_Charter_line    # no line comp data, no info about retention

         0.01       100       30        1     0.01        0           3          0          0          0          0          0          0          0  #  Age_inflection_Type12_age_logistic_FISHERY1(1)


Tuning lines for Goldband 28 May  

       4     1    0.015576    #           1    0.015576 0.190506     0.015576   0.009219   0.482286 0.190506  len       FISTrap
       4     4    0.116701    #           1    0.116701 0.626495     0.116701   0.073748  13.203329 0.626495  len Comm_trapMnth
       5     1    0.007413    #           1    0.007413 0.128197     0.007413   0.004350   0.588171 0.128197  age       FISTrap

__v0.75__  
Selection boxes and Kimberley Data fixes  
Can now deselect all years and it doesnt disappear
Optimised SS_format_cond_age_length function to use data.table, much faster !!!  
Cleaned RE Kimbereley data from 1997,1998,1999 to match exatcly the fish used in 2015 NDSF assessment  


__v0.74__  
Mainly themeing changes, using DPIRD colour style (trying to)  
Revised selection logic for biological lengths. Works better but still not perfect

__v0.73__  
Added WCD data filtering as per Emily's code  
Removed CatchMSY lines from Biol table
UI sidebar tweaks for cleanliness
Commented out a bunch of debug messages
 
__v0.72__  
r4ss can run even when SS fails - partially fixed i think for singled SS3 button, left it on for batch, more useful to let it run.  
Removed redundant run SS3 button, now dynamically appears  
Updated catchrates for Red Emp Kim to match 28 May model
Added more options to Fishery Params  
Francis tuning values now written to output/ folder and shown on console.  

#### Model Setup for RE KIM May 28  

1972.4   #_last_early_yr_nobias_adj_in_MPD  
2002.0   #_first_yr_fullbias_adj_in_MPD  
2019.6   #_last_yr_fullbias_adj_in_MPD  
2020.3   #_first_recent_yr_nobias_adj_in_MPD  
0.9245  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)   

Tuning lines for RED 28 May  

      4     1    2.944622    #           1    2.944622 0.315638     2.944622   2.002433  30.452289 0.315638  len FISTrap      
      5     1    0.258582    #           1    0.258582 0.334947     0.258582   0.147395   2.435387 0.334947  age FISTrap  



#### Model Setup for RE KIM May 28 tv sel  
1967.5   #_last_early_yr_nobias_adj_in_MPD   
1996.8   #_first_yr_fullbias_adj_in_MPD  
2019.4   #_last_yr_fullbias_adj_in_MPD  
2019.9   #_first_recent_yr_nobias_adj_in_MPD  
0.9506  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)  

begin and end years of blocks  

    2008 2011  
    2012 2015  
    2016 2020  
    2021 2024  

      4     1    5.328853    #           1    5.328853 0.340902     5.328853   3.376112  28.765341 0.340902  len FISTrap     
      5     1    0.255742    #           1    0.255742 0.334749     0.255742   0.149936   1.756688 0.334749  age FISTrap    
      
#### Model Setup for RE KIM May 28 tv vbK  
1967.5   #_last_early_yr_nobias_adj_in_MPD  
1996.8   #_first_yr_fullbias_adj_in_MPD  
2019.4   #_last_yr_fullbias_adj_in_MPD  
2019.9   #_first_recent_yr_nobias_adj_in_MPD  
0.9506  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)   

begin and end years of blocks  

    2008 2011  
    2012 2015  
    2016 2020  
    2021 2024  

Francis weighting disabled i.e.  

      #4     1    6.880762    #    5.133680    6.880762 0.439102     1.340318   0.692745   35.11841 0.085534  len FISTrap     
      #5     1    0.081656    #    0.141934    0.081656 0.305520     0.575313   0.334888   21.79516 2.152551  age FISTrap  


#### Model Setup for RE KIM May 28 Dirichlet  
1964.9   #_last_early_yr_nobias_adj_in_MPD  
1998.3   #_first_yr_fullbias_adj_in_MPD  
2019.2   #_last_yr_fullbias_adj_in_MPD  
2020.5   #_first_recent_yr_nobias_adj_in_MPD   
0.9529  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)  

no tv growth  

Francis weighting disabled i.e.  

      #4     1    6.880762    #    5.133680    6.880762 0.439102     1.340318   0.692745   35.11841 0.085534  len FISTrap     
      #5     1    0.081656    #    0.141934    0.081656 0.305520     0.575313   0.334888   21.79516 2.152551  age FISTrap  
      


__v0.71__  
Matches Red Emperor Kimberley 20 May 2025 Model

Conditional ages partially working for biological ages, still an issue with fleets

fixedsiteonly modified to match version in Red Emperor Kimberley 20 May 2025 Model  
Removed redundant run SS3 button, now uses zip version only



1976.4   #_last_early_yr_nobias_adj_in_MPD   
2000.0   #_first_yr_fullbias_adj_in_MPD   
2021.2   #_last_yr_fullbias_adj_in_MPD   
2022.0   #_first_recent_yr_nobias_adj_in_MPD   
0.857  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)  

__v0.70__  
More subsets ie location/zone/bioregion - some data issues to resolve first  
Add text to shown available sectors (fleets) in biol data.  

__v0.695__   
Made length and age tabs more consistent  
Added sample sizes to drop down lists.  
consolidated all packages into app.R  
updated README  

__v0.692__  
Minor fixes for data import and how Dirichlet parameters switched on/off  

__v0.691__  
Fixed a duplication issue with fleetnames  

__v0.69__  
selectivity (+ time-varying) -  Size_inflection, Size_95%width - __IMPLEMENTED FOR 1 BLOCK ONLY__  
retention (+ time-varying) - Retain_L_infl, Retain_L_width __IMPLEMENTED FOR 1 BLOCK ONLY__  

tv growth rubie questions  
  -What phase to use? make sure it doesnt clases with existing options.  
  -Is it the same block and blockfxn setup as sel and ret?  
  
    1996 1996  
    1997 1997  
    1998 2001  
    2002 2006  
    2007 2009  
    2010 2014  
    2015 2018  
    2019 2022  
  
Some fixes to handling indice fleets  

__v0.68__  
*Rubies parameter combinations implemented*  
Dirichlet multinomial theta - ln(DM_theta) __IMPLEMENTED. THINK ITS OK NEEDS TESTING__  
Growth - L_at_Amin, L_at_Amax, VonBert_K, CV_young, CV_old - __IMPLEMENTED. THINK ITS OK NEEDS TESTING__  
Initial recruitment - SR_LN(R0) - __DONE__  
Initial F - InitF_seas_1_flt - __DONE__  
catchability Q - LnQ_base - __CONFIRM WITH RUBIE, ALREADY ESTIMATING THIS WHEN USING INDICES__  
Additional sd around cpue - Q_extraSD - __IMPLEMENTED. THINK ITS OK NEEDS TESTING__  

__v0.67__  
Fixed issue with max size not considering biol length data  
Add option to batch run lots of zip files.  

__v0.66__  
Added ability to create Schnute growth model inputs.  
Lines in control file now dynamically handle scnhute growth model inputs.  
 
__v0.65__  
Added ability to load from zip file and run model  
Model now runs from zip files  
References for biol table now showing for some. More formatting required.  
Moved project to github - https://github.com/BMC1986/Shiny-FishAssess  


__v0.63__  
add custom inputs for M,h,R0  
Conditional ages working for FIS ages i.e Red Emp Kim

__v0.62__  
Added dropdown lists for the years in length and age tab.  

__v0.61__  
Cleaned up some console outputs from SS_Input.R

__v0.60__  
Removed frdc88 length data from the app. It was not being properly in the SS model and was causing issues with the SS model.

__v0.59__  
BIOL data in but some issues around conditional ages.

Red emp FIS ages and lengths. Conditional Ages  
Red emp FIS ages only  
Red emp Biol lengths and ages - Not Working  
Red emp Biol lengths - Working  
Red emp FIS and Biol lengths - Working (Fleets incorrectly defined in data)  
Red emp FIS and Biol lengths and FIS ages - Working (Fleets incorrectly defined in data)  
WA Dhufish - Biol Length - WORKING  
WA Dhufish - Biol Ages - WORKING   
WA Dhufish - Biol Ages. Conditional Ages - NOT WORKING  

Snapper - Biol Lengths - Working  
Snapper - Biol Lengths with FRDC88 removed - Works, but have issue with FRD88 year not being properly deselected.  

__v0.58__  
Biol age data working in SS outputs  
Conditional Ages not working  

__v0.56__  
Biol age comps appears and create files. SS_Input.R not hnadling them properly

__v0.55__  
Fix FIS plot not showing back when selecting a nonFIS species, then a FIS species

__v0.54__  
WCD length data being exported. Can be used in SS model. Not split by area/sector yet  
Need to fix how it is assinged to a fleet.  

__v0.52__  
Added WCD biol data, although not used in SS model yet  
Added ability to disable FIS data  

__v0.51__  
Added FRDC88 length cmp data, merged with existing bioldbase data, all treated at Commercial.Trap - Need to add option to assign to a fleet.  
Removed refresh all button from the main menu.  
Tested working with R4.5.0  

__v0.5__  
Initial versioning.  
App working with FIS data.  