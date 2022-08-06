*******************************************************************************
//          Replicate Post Earnings Announcement Drift (PEAD) 
//
//                            August 2022
//                              Gen Li
//

/*      Notes: 

            Calculates quarterly standardized earnings surprises (SUE) based      
             on time-series (seasonal random walk model) and analyst EPS forecasts 
             using methodology in Livnat and Mendenhall (JAR, 2006)                
             Forms SUE-based portfolios,compares drift across Compustat and IBES   
             based earnings surprise definitions and across different time periods
            
            Please follow the link below for details:
            https://wrds-www.wharton.upenn.edu/pages/support/applications/portfolio-construction-and-market-anomalies/post-earnings-announcement-drift/


*/
*******************************************************************************
clear 
cls
set more off

global MyProject "/Users/genli/genli Dropbox/Gen Li/Datasets/IBES" // Change to your dropbox directory
global Datasets "/Users/genli/genli Dropbox/Gen Li/Datasets"


*	========================	*
*	 No need to change here
*	========================	*
cap mkdir "${MyProject}/_temp"  
cd "${MyProject}/_temp"  

cap mkdir "${MyProject}/result" 
gl result "${MyProject}/result"
gl data "${MyProject}/data"



/**********************************************************************/
/*  SECTION 1: Extract Estimates from IBES                    
    Notes: 

            # Extract estimates from IBES Unadjusted file and select    
            # the latest estimate for a firm within broker-analyst group
            # "fpi in (6,7)" selects quarterly forecast for the current 
            # and the next fiscal quarter    

            Required datasets:

            1) IBES detail unadjusted EPS (ibes.detu_epsus)
            2) IBES_CRSP_Link from WRDS website

*/
/**********************************************************************/
 
/*----------------------------------------------------*/
   /* [>   1.  create a temporary IBES dataset   <] */ 
/*----------------------------------------------------*/
use "${Datasets}/IBES/data/detu_epsus_fpi67.dta", clear    
rename *, lower

*     keep only fpi in (6,7)
destring fpi, replace
keep if inrange(fpi, 6, 7)

*     keep variables we want
keep ticker estimator analys pdf fpi value fpedats revdats revtims anndats anntims

save "ibes_temp.dta", replace


*     add IBES-CRSP link to IBES dataset
use "${Datasets}/IBES_CRSP_Link/data/IBES_CRSP_Link.dta", clear
rename *, lower
keep if score <= 1
rangejoin anndats sdate edate using "ibes_temp.dta", by(ticker)


*     Count number of estimates reported on primary/diluted basis 
bys ticker fpedats: egen p_count = total(pdf == "P") 
bys ticker fpedats: egen d_count = total(pdf == "D") 


*     Determine whether most analysts report estimates on primary/diluted basis 
*     following Livnat and Mendenhall (2006)  
gen basis = "P" if p_count > d_count
replace basis = "D" if p_count <= d_count

*     drop variables
drop p_count d_count pdf fpi


*     Keep the latest observation for a given analyst
*     Group by company fpedats estimator analys then pick the last record in the group
/* sort ticker fpedats estimator analys anndats anntims revdats revtims */
sort ticker fpedats estimator analys anndats revdats anntims revtims
by ticker fpedats estimator analys: keep if _n == _N 


*     save
save "ibes.dta", replace



/*------------------------------------ End of SECTION 1 ------------------------------------*/




/**********************************************************************/
/*  SECTION 2: Link Estimates with Actuals                    
    Notes: 

            # Link Unadjusted estimates with Unadjusted actuals and CRSP permnos  
            # Keep only the estimates issued within 90 days before the report date

            Required datasets:

            1) IBES actual unadjusted EPS (ibes.actu_epsus): "${Datasets}/IBES/data/Detail_History_Actuals_Unadjusted.dta"
            2) Stock index daily data (crsp.dsi): "${Datasets}/CRSP/data/CRSP_stock_index_daily.dta"
            3) Individual stock daily data (crsp.dsf): "${Datasets}/CRSP/data/CRSP_d_19251231_20211231.dta"

*/
/**********************************************************************/
 
/*----------------------------------------------------*/
   /* [>   1.  clean unadjusted actual data   <] */ 
/*----------------------------------------------------*/
use "${Datasets}/IBES/data/Detail_History_Actuals_Unadjusted.dta", clear

rename *, lower

*     keep only quarterly forecasts
tab pdicity
keep if pdicity == "QTR"

*     rename variables
rename (anndats value pends) (repdats act fpedats)

keep ticker repdats act fpedats acttims
duplicates drop
drop if missing(act)

*     check duplicated ticker fpedats and select the last report of EPS
duplicates tag ticker fpedats, gen(dup)
bys ticker fpedats repdats (acttims): keep if dup == 0 | (dup > 0 & _n == _N)
drop dup
duplicates drop ticker fpedats, force
drop acttims

*     save
save "ibes_act.dta", replace


 
/*----------------------------------------------------*/
   /* [>   2.  Join with the estimate piece of the data   <] */ 
/*----------------------------------------------------*/
*     Join with the estimate piece of the data
use ibes.dta, clear
merge m:1 ticker fpedats using ibes_act.dta
drop if _merge == 2
drop _merge

*     gap between announcement date and estimate report date
gen dgap = repdats - anndats
tab dgap

keep if inrange(dgap, 0, 90)
tab dgap

*     save
drop dgap 
keep if ~missing(repdats) & ~missing(anndats)
save "ibes1.dta", replace



/*----------------------------------------------------*/
   /* [>   3.  Select all relevant combinations of Permnos and Date <] */ 
/*----------------------------------------------------*/
use ibes1.dta, clear
keep permno anndats
duplicates drop
save ibes1_dt1.dta, replace

use ibes1.dta, clear
keep permno repdats
duplicates drop

rename repdats anndats
append using ibes1_dt1.dta
duplicates drop
save ibes_anndats.dta, replace 



/*----------------------------------------------------*/
/* [>   4.  
      # Adjust all estimate and earnings announcement dates to the closest
      # preceding trading date in CRSP to ensure that adjustment factors won't
      # be missing after the merge     
<] */ 
/*----------------------------------------------------*/

*     unique non-missing-cfacshr trade dates from crsp.dsi
use "${Datasets}/CRSP/data/CRSP_stock_index_daily.dta", clear
rename *, lower
keep date
duplicates drop
save "crsp_dats.dta", replace

*     unique non-missing-cfacshr trade dates from crsp.dsf
use "${Datasets}/CRSP/data/CRSP_d_19251231_20211231.dta", clear
rename *, lower
keep permno date cfacshr 
drop if missing(cfacshr)
/* drop cfacshr */
duplicates drop
save "unique_trading_days_cfacshr.dta", replace


*     Create up to 10 days prior dates relative to anndats
use ibes_anndats.dta, clear
keep permno anndats
duplicates drop

forvalues i = 0 (1) 10 {
      gen anndatsL`i' = anndats - `i'
      format anndatsL`i' %td       
}  


*     reshape (transpose) the df for later join with crsp trading dates
reshape long anndatsL, i(permno anndats) j(lag)
keep  permno anndats anndatsL
rename anndatsL date


*     merge with crsp trading dates
*     merge the CRSP adjustment factors for all estimate and report dates
merge m:1 permno date using "unique_trading_days_cfacshr.dta"
keep if _merge == 3
drop _merge

*     create the dgap (days gap) variable for min selection
gen dgap = anndats - date
tab dgap

*     choosing the row with the smallest dgap for a given anndats
bys permno anndats (dgap): keep if _n == 1

*     save
/* keep permno anndats date */
duplicates drop
/* save tradedates.dta, replace */
save ibes_anndats.dta, replace


 /* 
/*----------------------------------------------------*/
   /* [>   5.  # merge the CRSP adjustment factors for all estimate and report dates   <] */ 
/*----------------------------------------------------*/
*     extract CRSP adjustment factors
use "${Datasets}/CRSP/data/CRSP_d_19251231_20211231.dta", clear
rename *, lower
keep permno date cfacshr 
duplicates drop
save cfacshr.dta, replace


use ibes_anndats, clear
merge m:1 permno anndats using tradedates.dta, nogen keep(master match)
merge m:1 permno date using cfacshr.dta, nogen keep(master match)

save ibes_anndats.dta, replace
 */


/*------------------------------------ End of SECTION 2 ------------------------------------*/



/**********************************************************************/
/*  SECTION 3: Adjust Estimates with CFACSHR                    
    Notes: 

      # Put the estimate on the same per share basis as
      # company reported EPS using CRSP Adjustment factors. 
      # New_value is the estimate adjusted to be on the 
      # same basis with reported earnings.

*/
/**********************************************************************/
 
/*----------------------------------------------------*/
   /* [>   1.  create adjusted estimates   <] */ 
/*----------------------------------------------------*/
use ibes1.dta, clear

*     add cfacshr factors from CRSP
merge m:1 permno anndats using "ibes_anndats.dta", nogen keep(master match)
drop anndats date
rename cfacshr cfacshr_ann

rename repdats anndats
merge m:1 permno anndats using "ibes_anndats.dta", nogen keep(master match)
rename (anndats cfacshr) (repdats cfacshr_rep)


*     adjust estimate 
gen new_value = cfacshr_rep / cfacshr_ann * value


*     # Sanity check: there should be one most recent estimate for 
*     # a given firm-fiscal period end combination 
unique ticker fpedats estimator analys
// Number of unique values of ticker fpedats estimator analys is  2184086
// Number of records is  2184086


*     save
save ibes1.dta, replace


 
/*----------------------------------------------------*/
   /* [>   2.  Compute the median forecast based on estimates in the 90 days prior to the EAD   <] */ 
/*----------------------------------------------------*/
use ibes1.dta, clear

bys ticker fpedats basis repdats act: egen grp_permno = max(permno)
bys ticker fpedats basis repdats act: egen medest = median(new_value)
bys ticker fpedats basis repdats act: egen avgest = mean(new_value)
bys ticker fpedats basis repdats act: egen numest = count(new_value)

keep ticker fpedats basis repdats act grp_permno medest avgest numest
duplicates drop
rename grp_permno permno
save medest.dta, replace


/*------------------------------------ End of SECTION 3 ------------------------------------*/




/**********************************************************************/
/*  SECTION 4: Merge IBES with Compustat Data                    
    Notes: 

            Required datasets:

            1) CCM link table from WRDS: "${Datasets}/CCM/data/ccm_link.dta"
            2) Compustat (comp.fundq): "fundq.dta"
            3) 

*/
/**********************************************************************/
 
/*----------------------------------------------------*/
   /* [>   1.  create a ibes ticker - gvkey link   <] */ 
/*----------------------------------------------------*/
/* *     Get ibtic from Comp.security on WRDS
use "${Datasets}/Compustat/data/comp_security_20220803.dta", clear
keep ibtic gvkey
duplicates drop
drop if missing(ibtic)
bys gvkey: gen ibtic_num = _n
reshape wide ibtic, i(gvkey) j(ibtic_num)
rename ibtic* ticker* 
save "comp_gvkey_ibtic.dta", replace
 */

*     clean ccm link
use "${Datasets}/CCM/data/ccm_link.dta", clear
rename *, lower
keep if linkprim == "P" | linkprim == "C" 
keep gvkey lpermco lpermno linkdt linkenddt
rename (lpermno lpermco) (permno permco)

*     Fill linkenddt missing value (.E in SAS dataset) with a future date
count if missing(linkenddt)
replace linkenddt = mdy(12, 31, 2099) if missing(linkenddt)

save "_ccm.dta", replace


*     # high quality links from iclink
*     # score = 1 or 2
use "${Datasets}/IBES_CRSP_Link/data/IBES_CRSP_Link.dta", clear
rename *, lower
keep if score <= 2
drop score
save "iclink_hq.dta", replace


*     merge gvkey and ibes ticker
use "_ccm.dta", clear
joinby permno using "iclink_hq.dta"
drop permno permco ncusip
duplicates drop

*     find out the effective date range for the gvkey-ibes link
egen linkdt_new = rowmin(linkdt sdate)
egen linkenddt_new = rowmin(linkenddt edate)
format linkdt_new linkenddt_new %td

drop linkdt linkenddt sdate edate
bys gvkey ticker: egen linkdt = min(linkdt_new)
bys gvkey ticker: egen linkenddt = max(linkenddt_new)
format linkdt linkenddt %td
drop linkdt_new linkenddt_new
duplicates drop

save "gvkey_ticker_link.dta", replace


/*----------------------------------------------------*/
   /* [>   2.  Merge Compustat and median estimate   <] */ 
/*----------------------------------------------------*/
*     Compustat
/* unzipfile "${Datasets}/Compustat/data/fundq_20220803.zip", replace */
use "fundq.dta", clear
rename *, lower

*     several filters to clean data
keep if consol == "C" & popsrc == "D" & indfmt == "INDL" & datafmt == "STD"
keep if (atq > 0 | ~missing(saleq)) & (~missing(datacqtr))

*     keep only the variables we want
keep gvkey fyearq fqtr conm datadate rdq epsfxq epspxq cshoq prccq ajexq spiq cshoq cshprq cshfdq saleq atq fyr datafqtr cshoq prccq
gen double mcap = cshoq*prccq
save "comp.dta", replace


*     add IBES ticker
use "gvkey_ticker_link.dta", clear
rangejoin datadate linkdt linkenddt using "comp.dta", by(gvkey)

*     Merge with the median esitmates
gen fpedats = datadate
merge n:1 ticker fpedats using "medest.dta"
/* 
    Result                      Number of obs
    -----------------------------------------
    Not matched                       559,710
        from master                   556,488  (_merge==1)
        from using                      3,222  (_merge==2)

    Matched                           447,644  (_merge==3)
    -----------------------------------------
 */
drop if _merge == 2
drop _merge

*     Sort data and drop duplicates
drop if missing(medest)
duplicates tag gvkey fqtr fyearq, gen(dup)
tab dup
/* 
        dup |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |    442,792       99.43       99.43
          1 |      2,530        0.57      100.00
          2 |          9        0.00      100.00
          3 |          4        0.00      100.00
------------+-----------------------------------
      Total |    445,335      100.00
*/     
drop dup    // I don't know why there duplicated gvkey-fqtr-fyearq. One possible reason could be fiscal year change?

duplicates drop gvkey fqtr fyearq, force

save "sue.dta", replace

/*------------------------------------ End of SECTION 4 ------------------------------------*/



/**********************************************************************/
/*  SECTION 5: Calculate SUEs                    
    Notes: */
/**********************************************************************/
use sue.dta, clear

*      handling same qtr previous year
bys gvkey fqtr (fyearq): gen dif_fyearq = fyearq - fyearq[_n-1]
bys gvkey fqtr (fyearq): gen lagadj = ajexq[_n-1] if dif_fyearq == 1
bys gvkey fqtr (fyearq): gen lageps_p = epspxq[_n-1] if dif_fyearq == 1
bys gvkey fqtr (fyearq): gen lageps_d = epsfxq[_n-1] if dif_fyearq == 1
bys gvkey fqtr (fyearq): gen lagshr_p = cshprq[_n-1] if dif_fyearq == 1
bys gvkey fqtr (fyearq): gen lagshr_d = cshfdq[_n-1] if dif_fyearq == 1
bys gvkey fqtr (fyearq): gen lagspiq = spiq[_n-1] if dif_fyearq == 1


*     # handling reporting basis 
*     # Basis = P and missing are treated the same
*     actual 1
gen actual1 = epsfxq / ajexq if  basis == "D"
replace actual1 = epspxq / ajexq if basis != "D"


*     actual 2
gen temp1 = 0.65 * spiq/ cshfdq
replace temp1 = 0 if missing(temp1)

gen temp2 = 0.65*spiq/cshprq
replace temp2 = 0 if missing(temp2)

gen temp_epsfxq = epsfxq
replace temp_epsfxq = 0 if missing(temp_epsfxq)

gen temp_epspxq = epspxq
replace temp_epspxq = 0 if missing(temp_epspxq)

gen actual2 = (temp_epsfxq - temp1) / ajexq if basis == "D"
replace actual2 = (temp_epspxq - temp2) / ajexq if basis != "D"
drop temp1 temp2 temp_epspxq temp_epsfxq


*     expected1
gen expected1 = lageps_d / lagadj if basis == "D"
replace expected1 = lageps_p / lagadj if basis != "D"


*     expected2
gen temp1 = 0.65 * lagspiq / lagshr_d
replace temp1 = 0 if missing(temp1)

gen temp2 = 0.65 * lagspiq / lagshr_p
replace temp2 = 0 if missing(temp2)

gen temp_lageps_d = lageps_d
replace temp_lageps_d = 0 if missing(temp_lageps_d)

gen temp_lageps_p = lageps_p
replace temp_lageps_p = 0 if missing(temp_lageps_p)

gen expected2 = (temp_lageps_d - temp1) / lagadj if basis == "D"
replace expected2 = (temp_lageps_p - temp2) / lagadj if basis != "D"


*     # SUE calculations
gen sue1 = (actual1 - expected1) / (prccq / ajexq)
gen sue2 = (actual2 - expected2) / (prccq / ajexq)
gen sue3 = (act - medest) / prccq
gen sue4 = (act - avgest) / prccq

keep ticker permno gvkey conm fyearq fqtr fyr datadate repdats rdq sue1 sue2 sue3 sue4 basis act medest numest prccq mcap 

save "sue.dta", replace



/*----------------------------------------------------*/
   /* [>   2.  

*     Shifting the announcement date to be the next trading day
*     Defining the day after the following quarterly EA as leadrdq1   

<] */ 
/*----------------------------------------------------*/
use comp.dta, clear

*     unique rdq 
keep rdq
duplicates drop
drop if missing(rdq)

/* preserve 
use sue.dta, clear
keep repdats
duplicates drop
rename repdats rdq
save "rdq_temp.dta", replace
restore

append using "rdq_temp.dta"
duplicates drop */

*     Create up to 10 days post rdq relative to rdq
forvalues i = 0 (1) 10 {
      gen rdqF`i' = rdq + `i'
      format rdqF`i' %td       
}  

reshape long rdqF@, i(rdq) j(dgap)


*     Merge with crsp trading dates
rename rdqF date
merge m:1 date using crsp_dats.dta
keep if _merge == 3
drop _merge


*     keep only the min dgap
bys rdq (dgap): keep if _n == 1
tab dgap
drop dgap

*     save
rename date rdq1
save eads1.dta, replace


*     create sue_final
use sue.dta, clear

/* replace rdq = repdats if missing(rdq) & ~missing(repdats)   // if rdq from Compustat is missing, we use repdats from IBES */

merge n:1 rdq using eads1.dta
keep if _merge == 3
drop _merge

*     check dup
duplicates tag gvkey fyearq fqtr, gen(dup)
tab dup
drop dup

*     save
gsort gvkey -fyearq -fqtr
save sue_final.dta, replace



/*----------------------------------------------------*/
/* [>   3.  filter from Livnat & Mendenhall (2006)   

#- earnings announcement date is reported in Compustat                   
#- the price per share is available from Compustat at fiscal quarter end  
#- price is greater than $1                                              
#- the market (book) equity at fiscal quarter end is available and is    
# EADs in Compustat and in IBES (if available)should not differ by more  
# than one calendar day larger than $5 mil.    


<] */ 
/*----------------------------------------------------*/
use sue_final.dta, clear

*     # If last gvkey then leadrdq1 = rdq1+3 months
*     # Else leadrdq1 = next rdq1
bys gvkey (fyearq fqtr): gen leadrdq1 = rdq1[_n+1]
bys gvkey (fyearq fqtr): replace leadrdq1 = rdq1 + 90 if _n == _N
bys gvkey (fyearq fqtr): replace leadrdq1 = mdy(month(leadrdq1), day(rdq1), year(leadrdq1)) if _n == _N
bys gvkey (fyearq fqtr): replace leadrdq1 = rdq1 + 90 if _n == _N & missing(leadrdq1) // some rdq1 will become missing if we ensure the same day
format leadrdq1 %td


*     calculate gap between IBES EA date and Compustat EA date
gen dgap = repdats - rdq
replace dgap = 0 if missing(dgap)
drop if rdq1 == leadrdq1


*     # Various conditioning for filtering
gen cond1 = ~missing(sue1) & ~missing(sue2) & missing(repdats)
gen cond2 = ~missing(repdats) & inrange(dgap, -1, 1)
keep if cond1 == 1 |  cond2 == 1


*     # Impose restriction on price and marketcap
keep if ~missing(rdq) & prccq >1 & mcap > 5


*     label variables
label variable leadrdq1 "Lead Adjusted Report Date of Quarterly Earnings"
label variable basis "Primary/Diluted Basis"
label variable act "Actual Reported Earnings per Share"
label variable medest "EPS consensus forecast (median)"
label variable ticker "Historical IBES Ticker"
label variable sue1 "Earnings Surprise (Seasonal Random Walk)"
label variable sue2 "Earnings Surprise (Excluding Special items)"
label variable sue3 "Earnings Surprise (Analyst Forecast-based)"
label variable numest "Number of analyst forecasts used in Analyst-based SUE"


*     save
drop cond1 cond2
save sue_final.dta, replace



/*------------------------------------ End of SECTION 5 ------------------------------------*/




/**********************************************************************/
/*  SECTION 6: Form Portfolios Based on SUE                    
    Notes: 

    # Extract file of raw daily returns around and between EADs and link them 
    # to Standardized Earnings Surprises for forming SUE-based portfolios  
    # Records from dsf and dsi to calculate exret

      Required datasets:

      1) CRSP individual stock daily return (crsp.dsf)
      2) CRSP stock index daily return (crsp.dsi)
      3) Merge 1) and 2) we will get "${MyProject}/data/ds.dta"


*/
/**********************************************************************/
 
/*----------------------------------------------------*/
   /* [>   1.  Add return data for EA date windows   <] */ 
/*----------------------------------------------------*/
*     Below is the sas code to collect ds data from WRDS. However, it's easy to get merge it on your own
/* 
proc sql; 
   create table ds
   as select a.permno, a.prc, a.date, abs(a.prc*a.shrout) as mcap,
             a.ret,
             c.vwretd as mkt, (a.ret-c.vwretd) as exret
   from crsp.dsf a 
   left join crsp.dsi (keep=date vwretd) c
   on a.date=c.date;
quit; 

proc export data=ds outfile="/home/ds.dta"; run;
*/

use "${MyProject}/data/ds.dta", clear
rename *, lower
save, replace

*     # Records from sue_final that meet the condition
use sue_final.dta, clear
keep if ~missing(rdq) & ~missing(leadrdq1) & ~missing(permno) & leadrdq1 - rdq1 > 30

*     get range of Earnings Announcement window
gen lb_date = rdq1 -  5
gen ub_date = leadrdq1 + 5
format lb_date ub_date %td

*     save
keep permno rdq1  leadrdq1 sue1 sue2 sue3 sue4 lb_date ub_date
save "sfj_indexed.dta", replace

*     add return data for dates around EA date
use "sfj_indexed.dta", clear
rangejoin date lb_date ub_date using "${MyProject}/data/ds.dta", by(permno)

drop lb_date ub_date
save "crsprets.dta", replace


 
/*----------------------------------------------------*/
/* [>   2.  Estimate drift   

# To estimate the drift, sum daily returns over the period from  
# 1 day after the earnings announcement through the day of       
# the following quarterly earnings announcement     

<] */ 
/*----------------------------------------------------*/

use "crsprets.dta", clear

bys permno rdq1 (date): gen lagmcap = mcap[_n-1]

*     get the trading days since the announcement and keep only within 60 days after the announcement
keep if rdq1 <= date & date <= leadrdq1
bys permno rdq1 (date): gen ncount = _n - 1
tab ncount

keep if ncount <= 60

*     # Form quintiles based on SUE1-4
duplicates drop

*     SUE1
preserve
keep permno sue1 rdq1
duplicates drop
drop if missing(sue1)
egen sue1r = xtile(sue1), n(5) /* by(ncount) */
save temp.dta, replace
restore

merge m:1 permno rdq1 sue1 using temp.dta, nogen keep(master match)

*     SUE2
preserve
keep permno sue2 rdq1
duplicates drop
drop if missing(sue2)
egen sue2r = xtile(sue2), n(5) /* by(ncount) */
save temp.dta, replace
restore

merge m:1 permno rdq1 sue2 using temp.dta, nogen keep(master match)

*     SUE3
preserve
keep permno sue3 rdq1
duplicates drop
drop if missing(sue3)
egen sue3r = xtile(sue3), n(5) /* by(ncount) */
save temp.dta, replace
restore

merge m:1 permno rdq1 sue3 using temp.dta, nogen keep(master match)

*     SUE4
preserve
keep permno sue4 rdq1
duplicates drop
drop if missing(sue4)
egen sue4r = xtile(sue4), n(5) /* by(ncount) */
save temp.dta, replace
restore

merge m:1 permno rdq1 sue4 using temp.dta, nogen keep(master match)

*     save
save "peadrets.dta", replace


 
/*----------------------------------------------------*/
/* [>   2.  
# Form portfolios on Compustat-based SUEs (=sue1 or =sue2) or IBES-based SUE (=sue3)
# Code uses sue3   

<] */ 
/*----------------------------------------------------*/

*     loop over three SUEs
forvalues i = 1 (1) 4 {
      use peadrets.dta, clear

      keep if ~missing(sue`i'r)
      sort ncount sue`i'

      *     # Form value-weighted exret
      *     # Calculate group weight sum;
      bys ncount sue`i'r: egen count_sue`i'r = total(~missing(sue`i'r))
      bys ncount sue`i'r: egen wtmean_exret`i' = wtmean(exret), weight(lagmcap)

      *     keep variables we want
      keep ncount sue`i'r count_sue`i'r wtmean_exret`i'
      duplicates drop

      *     # set ncount=0 all five portfolio weighted returns to be 0
      replace wtmean_exret`i' = 0 if ncount == 0   


      *     calculate cumulative value-weighted excess return
      bys sue`i'r (ncount): gen cum_wtmean_exret`i' = sum(wtmean_exret`i')

      *     convert cum return to percentage format
      replace cum_wtmean_exret`i' = cum_wtmean_exret`i' * 100  

      *     save
      save "peadsue`i'port.dta", replace

}  


 
/*----------------------------------------------------*/
   /* [>   3.  Draw PEAD graphs   <] */ 
/*----------------------------------------------------*/
*     SUE3
use "peadsue3port.dta", clear

#delimit ;
twoway (line cum_wtmean_exret3 ncount if sue3r == 1, 
lwidth(medthick)) (line cum_wtmean_exret3 ncount if sue3r == 2, 
lwidth(medthick)) (line cum_wtmean_exret3 ncount if sue3r == 3, 
lwidth(medthick)) (line cum_wtmean_exret3 ncount if sue3r == 4, 
lwidth(medthick)) (line cum_wtmean_exret3 ncount if sue3r == 5, 
lwidth(medthick)) if ncount <= 50, 
ytitle(Cumulative Value-Weighted Excess Returns (%)) ylabel(#10) xtitle("Event time, t=0 is Earnings Announcement Date") xlabel(#10) 
legend(on order(1 "SUE1: Most negative SUE port" 2 "SUE2" 3 "SUE3" 4 "SUE4" 5 "SUE5: Most positive SUE port") rows(2) position(6)) 
scheme(white_tableau)
title("CARs for Analyst-based SUE Portfolios", size(medsmall));
#delimit cr

graph export "${result}/PEAD_SUE3.png", replace



*     SUE4
use "peadsue4port.dta", clear

#delimit ;
twoway (line cum_wtmean_exret4 ncount if sue4r == 1, 
lwidth(medthick)) (line cum_wtmean_exret4 ncount if sue4r == 2, 
lwidth(medthick)) (line cum_wtmean_exret4 ncount if sue4r == 3, 
lwidth(medthick)) (line cum_wtmean_exret4 ncount if sue4r == 4, 
lwidth(medthick)) (line cum_wtmean_exret4 ncount if sue4r == 5, 
lwidth(medthick)) if ncount <= 50, 
ytitle(Cumulative Value-Weighted Excess Returns (%)) ylabel(#10) xtitle("Event time, t=0 is Earnings Announcement Date") xlabel(#10) 
legend(on order(1 "SUE1: Most negative SUE port" 2 "SUE2" 3 "SUE3" 4 "SUE4" 5 "SUE5: Most positive SUE port") rows(2) position(6)) 
scheme(white_tableau);
#delimit cr

graph export "${result}/PEAD_SUE4.png", replace



*     SUE2
use "peadsue2port.dta", clear

#delimit ;
twoway (line cum_wtmean_exret2 ncount if sue2r == 1, 
lwidth(medthick)) (line cum_wtmean_exret2 ncount if sue2r == 2, 
lwidth(medthick)) (line cum_wtmean_exret2 ncount if sue2r == 3, 
lwidth(medthick)) (line cum_wtmean_exret2 ncount if sue2r == 4, 
lwidth(medthick)) (line cum_wtmean_exret2 ncount if sue2r == 5, 
lwidth(medthick)) if ncount <= 50, 
ytitle(Cumulative Value-Weighted Excess Returns (%)) ylabel(#10) xtitle("Event time, t=0 is Earnings Announcement Date") xlabel(#10) 
legend(on order(1 "SUE1: Most negative SUE port" 2 "SUE2" 3 "SUE3" 4 "SUE4" 5 "SUE5: Most positive SUE port") rows(2) position(6)) 
scheme(white_tableau);
#delimit cr

graph export "${result}/PEAD_SUE2.png", replace



*     SUE1
use "peadsue1port.dta", clear

#delimit ;
twoway (line cum_wtmean_exret1 ncount if sue1r == 1, 
lwidth(medthick)) (line cum_wtmean_exret1 ncount if sue1r == 2, 
lwidth(medthick)) (line cum_wtmean_exret1 ncount if sue1r == 3, 
lwidth(medthick)) (line cum_wtmean_exret1 ncount if sue1r == 4, 
lwidth(medthick)) (line cum_wtmean_exret1 ncount if sue1r == 5, 
lwidth(medthick)) if ncount <= 50, 
ytitle(Cumulative Value-Weighted Excess Returns (%)) ylabel(#10) xtitle("Event time, t=0 is Earnings Announcement Date") xlabel(#10) 
legend(on order(1 "SUE1: Most negative SUE port" 2 "SUE2" 3 "SUE3" 4 "SUE4" 5 "SUE5: Most positive SUE port") rows(2) position(6)) 
scheme(white_tableau);
#delimit cr

graph export "${result}/PEAD_SUE1.png", replace




/*------------------------------------ End of SECTION 6 ------------------------------------*/















