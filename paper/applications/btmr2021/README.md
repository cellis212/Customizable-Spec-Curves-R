# Example: a self-described "most conservative" specification

Bednarek, te Kaat, Ma and Rebucci (2021), "Capital Flows, Real Estate, and Local Cycles: Evidence from German Cities, Banks, and Firms," Review of Financial Studies 34(10), 5077-5134. The working paper calls Table 3, columns 7 and 8, "the most conservative specifications." The curve tests whether those columns sit at the conservative end of a defensible menu.

The published text says Table 3 columns 7 and 8 "are the most conservative specifications." The frozen menu `btmr-v1` (240 specifications) was written before any alternative was estimated. Its reference is the published column 7 estimate, 0.007 (0.004), N = 510, reproduced as 0.00732 (0.00440) with 85 city clusters; column 8 (published 0.011, reproduced 0.01074) is marked as an original specification.

## Data (not redistributed)

Harvard Dataverse doi:[10.7910/DVN/LQPRIA](https://doi.org/10.7910/DVN/LQPRIA) (CC0). Download the original-format files into `RAW/btmr_2021/`; the build script checks `citylevel.dta` against SHA-256 `9a5f7c824bf74040a02a993bd386d9dd9a4c4b26b1ddf1b3b59fe045b6ec64df`.

## Design notes

City and year fixed effects absorb the spread, the macro series, and city characteristics, so only interactions are identified, as in the authors' `Table3.do`. Standard errors are clustered by city (85 clusters); this does not address dependence created by the single aggregate spread series over six years.
