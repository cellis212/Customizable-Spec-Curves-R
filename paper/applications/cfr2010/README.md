# Design example: regression discontinuity (Cellini, Ferreira and Rothstein, 2010)

Cellini, Ferreira and Rothstein (2010), "The Value of School Facility Investments: Evidence from a Dynamic Regression Discontinuity Design," Quarterly Journal of Economics 125(1), 215-261. California school-district bond measures pass with two-thirds of the vote (55% after 2001). The dynamic design stacks each measure's district panel from two years before to six years after the vote, with a cubic in the vote share for each relative year, measure, calendar-year and relative-year fixed effects, clustered by district.

The frozen menu `cfr-v1` (336 specifications) was written before any alternative was estimated. Its reference is Table IV, Panel A: passage raises capital outlays per pupil three years later by 1,121 (244); the authors' log reports 1,121.41 (243.79), N = 6,970, and the engine reproduces 1,121.41.

## Data (not redistributed)

The authors' replication archive, linked from https://jesse-rothstein.com/papers/ (public Box folder `replication-CAschoolbonds-new`). Save `recursivepanel.dta.zip` unmodified in `RAW/cfr_2010/`; the build script checks SHA-256 `e99f583d5537f48b3dcee9e6207354e000ab4ce740a1f508c5f59727a48228d8`. The archive reproduces the fiscal outcomes only; house prices are proprietary and not included.

## Design notes

- Bandwidth, kernel, donut, and threshold choices refer to the vote margin (vote share minus required share) of the measure that defines each stack. Triangular kernel weights enter as regression weights (`tri_20`, `tri_10`, `tri_5`).
- fixest drops 24 fixed-effect singletons that Stata keeps (N = 6,946) and treats measure effects as nested in district clusters, so its standard error (222.6) is smaller than `areg`'s (243.8). The point estimate is unchanged.
- The regressors include the authors' raw cubic vote shares (up to about 10^5) beside binary terms, which limits R-Python agreement to about 1e-6 relative.
- Data-driven bandwidths with robust bias-corrected intervals (Calonico, Cattaneo and Titiunik, 2014) are not implemented; the windows here are fixed.
