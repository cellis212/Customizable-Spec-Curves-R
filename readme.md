# Specification Curve Analysis in R

## Paper workflow

This repository also contains the source for *Specification Curves in Empirical Finance: A Practical Guide*. The `paper/` directory provides a manuscript scaffold and a small, runnable reference workflow alongside the original customizable scripts below.

The new workflow currently supports linear fixed-effects models with one-way cluster-robust, pointwise confidence intervals. It compares available and common samples, preserves required controls, records failed specifications, and validates each result against a manifest frozen before estimation. It does not implement joint specification-curve inference. The two empirical application slots are unselected; the runnable demonstration uses generated data and establishes no empirical finding.

From the repository root:

```sh
make setup        # Install local commit/push checks and check R dependencies
make restore      # Restore the R package versions recorded in renv.lock
make check        # Check publication rules and scientific workflow invariants
make demo         # Run the complete generated-data example
make manuscript   # Compile the manuscript scaffold with Quarto and TeX
```

R 4.5.1 and the dependency versions in `renv.lock` were used for setup. The lockfile covers the new paper workflow; the original examples retain their separate package requirements below. The manuscript build also requires Quarto and a TeX installation. Run `make restore` if required R packages are missing. Generated demonstration files go under the ignored `local/output/` directory, or the directory specified by `SPEC_CURVES_OUTPUT_DIR`. The manuscript PDF goes under the ignored `paper/manuscript/output/` directory.

The source layout is:

- `paper/run.R`: dependency checks, manifest freezing, estimation, validation, and rendering.
- `paper/R/`: model, sample, provenance, and plotting functions.
- `paper/config/demo.json`: the generated-data specification menu.
- `paper/config/applications.json`: the two unselected application slots.
- `paper/tests/`: checks for missing/duplicated rows, changed inputs/manifests, common samples, unidentified coefficients, and baseline reproduction.
- `paper/manuscript/`: Quarto source, section files, and verified seed references.

For an empirical application, construct a justified application-specific menu using the demo schema, set `analysis_kind` to `application`, and supply `reference` with the published `estimate`, an absolute numerical `tolerance`, and a precise `source`. Required controls, fixed effects, and clustering remain fixed. The supported menu varies control blocks and available/common samples only. A new design or estimator needs a separately checked implementation. The preferred specification must reproduce its documented baseline before figures can be assembled.

```sh
Rscript paper/run.R freeze /path/to/menu.json /path/to/input.csv /path/to/manifest.json
Rscript paper/run.R estimate /path/to/manifest.json /path/to/input.csv /path/to/new-run
```

Freezing saves the complete grid, input hash, and analysis-source hashes. Keep the manifest and its `.sha256` sidecar together. A changed menu, input, or analysis source requires a new manifest version. CSV input is converted to an R data frame before hashing; keep variable types and input conventions stable between freezing and estimation. Failed or unidentified specifications remain in the results, block assembly, and must be addressed explicitly. Summary fractions are descriptive properties of the chosen set; pointwise intervals do not provide joint inference.

New commits use an explicit source allowlist, checked by local commit/push hooks and continuous integration. Data, generated figures/tables/PDFs, run records, transcripts, and project planning files are excluded. The three original public images below are retained unchanged. The checks are guardrails; review the staged diff before publishing source.

## Original customizable examples

This repository contains modifiable (not canned) code based on the R package `specr` to create extremely flexible and visually appealing specification curves in R. The files are also structured to create multiple spec curves across different dependent variables in an efficient manner and to allow sampling of specifications when there are many potential options. The code is designed to be intuitive and extremely customizable, allowing you to adapt it to any of your specific project needs. The repository includes two main files:

1. **Model Running File**: This file runs all the models. It requires significant customization based on your project, but it is structured in a way that makes it easy to adjust. AI coding models, such as Cursor, can assist in tailoring this file to your specific requirements. I find uploading your primary analysis file and then a list of the specifications you want to try gets you 90% of the way there. The included sample file lists options from Ellis, Takefumi, and Veith (2024).

2. **Curve Printing File**: This file takes the outcomes from the model running file and prints the specification curves. It includes markers ("#### MANUAL CHANGE REQUIRED ####") to indicate where manual adjustments are necessary. AI coding models, such as Cursor, will also assist here and can typically one-shot this file when you also upload the model running file. The included sample file lists options from Ellis, Takefumi, and Veith (2024) and will also create a Latex table summarizing all curves that are run.

## Sample Output Figures

Below are some sample output figures generated by the code:

![Sample Figure 1](./Sample_Output/SpecCurve.png)
Figure 1: Ellis, Cameron and Esson, Meghan and Liebman, Eli, Moral Hazard on the ACA Exchanges: Evidence from a Cost-Sharing Subsidy Discontinuity (July 12, 2024). Available at SSRN: https://ssrn.com/abstract=4892925 

![Sample Figure 2](./Sample_Output/spec_curve_treat.png)
Figure 2: Example from Collier, Benjamin and Ellis, Cameron and Keys, Benjamin J., The Cost of Consumer Collateral: Evidence from Bunching (January 1, 2022).  Available at SSRN: https://ssrn.com/abstract=4048069 

![Sample Figure 3](./Sample_Output/spec_curve_pca_liq_b.png)
Figure 3: Example from Ellis, Cameron and Ueno, Takefumi and Veith, Stefan, The Solvency II Framework: Causal Effects on Risk and Liquidity (August 29, 2024). Available at SSRN: https://ssrn.com/abstract=4940529 

See the above referenced papers for sample language you can use to describe these figures.

## Citations

Please cite the following if you use this code in your research:

- Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. Nature Human Behaviour, 4, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
- Masur, Philipp K. & Scharkow, M. (2020). specr: Conducting and Visualizing Specification Curve Analyses. Available from https://CRAN.R-project.org/package=specr.
- Ellis, C. (2024). Customizable Specification Curves in R [Computer software]. https://github.com/cellis212/Customizable-Spec-Curves-R

## Getting Started

To get started with this code, follow these steps:

1. **Clone the Repository**: Clone this repository to your local machine using `git clone` or download the .zip. 
2. **Install Required Packages**: Ensure that you have the necessary R packages installed. The required packages are loaded at the top of each script.
3. **Customize the Model Running File**: Adjust the model running file to fit your specific project needs. This includes defining your data, model specifications, subsets, etc. You can also define separate outputs (i.e. generate an elasticity, etc.).
4. **Run the Model Running File**: Execute the model running file to generate the model outcomes. This will naturally run in parallel.
5. **Customize the Curve Printing File**: Make any necessary manual adjustments to the curve printing file as indicated by the markers.
6. **Generate Specification Curves**: Run the curve printing file to generate and save the specification curves.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

