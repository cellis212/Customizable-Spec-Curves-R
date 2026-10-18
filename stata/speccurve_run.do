* speccurve_run.do -- execute a frozen specification plan in Stata.
*
* The R reference engine freezes the menu and exports a language-neutral plan:
*     Rscript paper/run.R plan manifest.json input.csv plan.csv
* This file runs every planned row and writes results.csv (same columns as the
* R and Python engines) and samples.csv (spec_id, row_id of every observation
* used), so tools/compare_implementations.py can check agreement row by row.
*
* Usage (from the repository root):
*     do stata/speccurve_run.do "plan.csv" "input.csv" "output_directory"
*
* Requires Stata 16+ and reghdfe (ssc install reghdfe; ssc install ftools).
* Conventions matched to the R reference:
*   - singletons are kept (fixest does not drop them);
*   - clustered errors use reghdfe's small-sample correction, which counts only
*     fixed effects not nested in the cluster, and t(G-1) critical values;
*   - Newey-West uses Bartlett weights with no n/(n-k) adjustment, on rows
*     sorted by time (positional lags), and t(n-1) critical values.
* Supported: linear_fixed_effects (cluster, hetero, iid) and time_series_mean
* (iid, newey_west). Every failure is recorded with a reason; no row is skipped.
* Not implemented: iv_fixed_effects and regression weights; such rows fail with
* that reason rather than running a different model.
* Status: written to match the R reference but not yet run against it. Unit
* trends are absorbed as unit##c.time; reghdfe's degrees-of-freedom count for
* these slopes may differ slightly from fixest's, so compare trend rows first.

version 16
args plan input outdir
if "`plan'" == "" | "`input'" == "" | "`outdir'" == "" {
    display as error "Usage: do stata/speccurve_run.do plan.csv input.csv output_directory"
    exit 198
}
capture mkdir "`outdir'"
capture confirm file "`outdir'/results.csv"
if !_rc {
    display as error "Output already exists; use a fresh run directory."
    exit 602
}

* ---- Read the plan into locals -------------------------------------------
import delimited using "`plan'", clear varnames(1) stringcols(_all) bindquote(strict)
local n = _N
local fields spec_id sample_mode panel estimator outcome treatment controls fixed_effects unit_trend ///
    unit time row_id vcov_type cluster lags confidence_level filters stata_if sample_vars formula vcov
capture confirm variable weights
local has_weights = !_rc
forvalues i = 1/`n' {
    foreach f of local fields {
        local `f'`i' = `f'[`i']
    }
    local weights`i' ""
    if `has_weights' local weights`i' = weights[`i']
}

* ---- Execute each row ------------------------------------------------------
import delimited using "`input'", clear varnames(1) case(preserve) bindquote(strict)
* Encode string fixed-effect and cluster identifiers (e.g. region_year) so that
* absorb() and vce(cluster) receive numeric groups; group membership is unchanged.
local groupvars ""
forvalues i = 1/`n' {
    local groupvars `groupvars' `fixed_effects`i'' `cluster`i''
}
local groupvars : list uniq groupvars
foreach v of local groupvars {
    capture confirm string variable `v'
    if !_rc {
        rename `v' _string_`v'
        encode _string_`v', generate(`v')
        drop _string_`v'
    }
}
tempfile analysis samples_long
save `analysis'

tempname results
postfile `results' str12 spec_id str20 status str244 error double(estimate std_error statistic p_value ///
    ci_lower ci_upper n_eligible n_obs n_units n_periods n_clusters) using "`outdir'/results_stata.dta", replace

clear
set obs 0
generate str12 spec_id = ""
generate str80 row_id = ""
save `samples_long', replace emptyok

forvalues i = 1/`n' {
    use `analysis', clear
    local status "failed"
    local error ""
    foreach s in estimate std_error statistic p_value ci_lower ci_upper n_obs n_units n_periods n_clusters {
        local `s' = .
    }
    * Complete cases on the planned variables, then the planned filters.
    foreach v of local sample_vars`i' {
        quietly drop if missing(`v')
    }
    quietly keep if `stata_if`i''
    local n_eligible = _N
    local level = 100 * real("`confidence_level`i''")
    local alpha = 1 - real("`confidence_level`i''")
    if `n_eligible' == 0 {
        local error "No complete observations."
    }
    else if "`weights`i''" != "" {
        local error "Regression weights are not implemented in the Stata executor."
    }
    else if "`estimator`i''" == "linear_fixed_effects" {
        local absorb "`fixed_effects`i''"
        if "`unit_trend`i''" == "1" {
            local absorb : subinstr local absorb "`unit`i''" "`unit`i''##c.`time`i''", word
        }
        if "`vcov_type`i''" == "cluster" local vce "vce(cluster `cluster`i'')"
        else if "`vcov_type`i''" == "hetero" local vce "vce(robust)"
        else local vce "vce(unadjusted)"
        capture noisily reghdfe `outcome`i'' `treatment`i'' `controls`i'', absorb(`absorb') `vce' keepsingletons
        if _rc {
            local error "reghdfe failed with return code `=_rc'."
        }
        else if _se[`treatment`i''] == 0 | missing(_se[`treatment`i'']) {
            local error "Focal coefficient is unidentified or absorbed."
        }
        else {
            local estimate = _b[`treatment`i'']
            local std_error = _se[`treatment`i'']
            local statistic = `estimate' / `std_error'
            local df = e(df_r)
            local p_value = 2 * ttail(`df', abs(`statistic'))
            local critical = invttail(`df', `alpha' / 2)
            local ci_lower = `estimate' - `critical' * `std_error'
            local ci_upper = `estimate' + `critical' * `std_error'
            local n_obs = e(N)
            if "`vcov_type`i''" == "cluster" local n_clusters = e(N_clust)
            quietly keep if e(sample)
        }
    }
    else if "`estimator`i''" == "time_series_mean" {
        quietly levelsof `unit`i'', local(series)
        quietly duplicates report `time`i''
        if `: word count `series'' != 1 {
            local error "A time-series mean needs exactly one series."
        }
        else if r(unique_value) != r(N) {
            local error "Duplicate periods in a time series."
        }
        else if _N < 3 {
            local error "Fewer than three periods."
        }
        else {
            sort `time`i''
            quietly summarize `outcome`i''
            local nn = r(N)
            local estimate = r(mean)
            if "`vcov_type`i''" == "iid" {
                local std_error = r(sd) / sqrt(`nn')
            }
            else {
                tempvar u prod
                quietly generate double `u' = `outcome`i'' - `estimate'
                quietly generate double `prod' = `u' * `u'
                quietly summarize `prod'
                local s = r(sum) / `nn'
                local L = min(real("`lags`i''"), `nn' - 1)
                forvalues l = 1/`L' {
                    quietly replace `prod' = `u' * `u'[_n - `l']
                    quietly summarize `prod'
                    local s = `s' + 2 * (1 - `l' / (real("`lags`i''") + 1)) * r(sum) / `nn'
                }
                local std_error = sqrt(`s' / `nn')
            }
            local statistic = `estimate' / `std_error'
            local p_value = 2 * ttail(`nn' - 1, abs(`statistic'))
            local critical = invttail(`nn' - 1, `alpha' / 2)
            local ci_lower = `estimate' - `critical' * `std_error'
            local ci_upper = `estimate' + `critical' * `std_error'
            local n_obs = `nn'
        }
    }
    else {
        local error "Unsupported estimator."
    }
    if "`error'" == "" {
        local status "ok"
        quietly levelsof `unit`i'', local(units)
        local n_units : word count `units'
        quietly levelsof `time`i'', local(periods)
        local n_periods : word count `periods'
        * Record the observations used for language-neutral sample hashes.
        keep `row_id`i''
        quietly tostring `row_id`i'', replace format(%20.0g) force
        if "`row_id`i''" != "row_id" rename `row_id`i'' row_id
        generate str12 spec_id = "`spec_id`i''"
        append using `samples_long'
        quietly save `samples_long', replace
    }
    post `results' ("`spec_id`i''") ("`status'") (`"`error'"') (`estimate') (`std_error') (`statistic') ///
        (`p_value') (`ci_lower') (`ci_upper') (`n_eligible') (`n_obs') (`n_units') (`n_periods') (`n_clusters')
}
postclose `results'

* ---- Write outputs with the plan's labels -----------------------------------
use `samples_long', clear
export delimited using "`outdir'/samples.csv", replace
import delimited using "`plan'", clear varnames(1) stringcols(_all) bindquote(strict)
keep spec_id sample_mode panel formula estimator outcome treatment vcov filters confidence_level
tempfile labels
save `labels'
use "`outdir'/results_stata.dta", clear
merge 1:1 spec_id using `labels', nogenerate assert(match)
order spec_id sample_mode panel formula estimator outcome treatment vcov filters confidence_level status error
* Full precision, so results can be compared with R and Python at 1e-8.
format estimate std_error statistic p_value ci_lower ci_upper %24.17g
export delimited using "`outdir'/results.csv", replace
erase "`outdir'/results_stata.dta"
display as result "Wrote `n' specification rows to `outdir'/results.csv"
