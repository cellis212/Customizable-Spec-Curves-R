# nolint start: line_length_linter, trailing_whitespace_linter.

# Pre-amble ---------------------------------------------------------------

# Clearing Memory
rm(list = ls())

# Loading Packages
library(tidyverse)
library(fixest)
library(haven) # Add haven for importing Stata data
library(foreach)
library(doParallel)

# Set Seed
set.seed(42)

# Setup parallel processing
cores <- detectCores() - 1 # Use all cores except one
cl <- makeCluster(cores)
registerDoParallel(cl)

# Loading Data ------------------------------------------------------------
dat <- haven::read_dta("../01 - Data/finaldata.20240818.dta")

# Data Cleaning -----------------------------------------------------------
# Any data cleaning for all regressions
# Fix name for Germany
dat$country_str <- ifelse(dat$country_str == "BD", "DE", dat$country_str)

# Options -----------------------------------------------------------------
Sample <- c("Broad", "Restricted", "Fully Balanced")
Winsorize <- c("Yes", "No")

# Control Variations
Current_Controls <- c("None", "Size (MV)", "Leverage", "ROA", "Risk-free rate", "Inflation rate")
Extra_Controls <- c("None", "Size (TA)", "Share turnover", "Market-to-book", "Asset growth")

# Update Leave_Out_Countries options to include all EU countries
Leave_Out_Countries <- c("None", "UK", "FR", "DE")

# Update Leave_Out_Industries options with numbers first
Leave_Out_Industries_Control <- c(
  "None",
  "0 - Agr., For., And Fishing",
  "1 - Mining",
  "2 - Construction",
  "3 - Manufacturing",
  "4 - Transportation, etc.",
  "5 - Wholesale Trade",
  "6 - Fin., Ins., and RE",
  "7 - Retail Trade",
  "8 - Services",
  "9 - Public Admin"
)

Leave_Out_Industries_Treat <- c(
  "None",
  "6311 - Life",
  "6321 - Accident & Health",
  "6324 - Hospital & MS",
  "6331 - Fire, Marine & Casualty",
  "6351 - Surety",
  "6361 - Title",
  "6399 - Carriers, NEC"
)

# This creates all combinations of your options
Models <- expand.grid(
  Sample = Sample,
  Leave_Out_Control = Current_Controls,
  Add_Control = Extra_Controls,
  Leave_Out_Countries = Leave_Out_Countries,
  Leave_Out_Industries_Control = Leave_Out_Industries_Control,
  Leave_Out_Industries_Treat = Leave_Out_Industries_Treat
)

# Define dependent variables
dep_vars <- c("lnexretvar_d", "lnicc_gebhardt", "pca_risk_b", "lnzscore", "lnmDD", "pca_default_b", "mroll", "lnpi", "pca_liq_b")

# Create columns for each dependent variable
for (dv in dep_vars) {
  Models[[paste0("treat_", dv)]] <- 0
  Models[[paste0("ub_treat_", dv)]] <- 0
  Models[[paste0("lb_treat_", dv)]] <- 0
}

# Number of models
nOpt <- nrow(Models)
nOpt

# Run the Models in Parallel ----------------------------------------------

# Use this if you want to run all models
ModList <- 1:nrow(Models)

# Use this if you want to run a random sample of models (for when you have a large number of options)
ModList <- c(1, sample(ModList, size = 10000, replace = FALSE))

Models <- foreach(i = ModList, .combine = rbind, .packages = c("tidyverse", "fixest", "haven")) %dopar% {
  # Sample selection
  if (Models$Sample[i] == "Broad") {
    subs <- which(dat$broad == 1)
  } else if (Models$Sample[i] == "Restricted") {
    subs <- which(dat$restricted == 1)
  } else if (Models$Sample[i] == "Fully Balanced") {
    # Create a fully balanced sample from the broad sample
    subs <- which(dat$broad == 1 & complete.cases(dat[, dep_vars]))
  }

  # Always use winsorized variables
  win_prefix <- case_when(
    Models$Sample[i] == "Restricted" ~ "wr",
    TRUE ~ "wb"
  )

  # Control Variations ----------------------------------------------------
  base_controls <- c(
    "Size (MV)" = paste0(win_prefix, "lnmv_usd_l"),
    "Leverage" = paste0(win_prefix, "lnlev"),
    "ROA" = paste0(win_prefix, "roa"),
    "Risk-free rate" = paste0(win_prefix, "mrf_y"),
    "Inflation rate" = paste0(win_prefix, "glt")
  )

  extra_controls <- c(
    "Size (TA)" = paste0(win_prefix, "lntotass_usd_l"),
    "Share turnover" = paste0(win_prefix, "lnshto"),
    "Market-to-book" = paste0(win_prefix, "lnmtb"),
    "Asset growth" = paste0(win_prefix, "assgr")
  )

  # Leave one out
  if (Models$Leave_Out_Control[i] != "None") {
    controls <- base_controls[names(base_controls) != Models$Leave_Out_Control[i]]
  } else {
    controls <- base_controls
  }

  # Add one in
  if (Models$Add_Control[i] != "None") {
    controls <- c(controls, extra_controls[names(extra_controls) == Models$Add_Control[i]])
  }

  # Leave Out Countries ---------------------------------------------------
  # Only for treatment group now
  if (Models$Leave_Out_Countries[i] != "None") {
    subs <- intersect(subs, which(!(dat$country_str == Models$Leave_Out_Countries[i])))
  }

  # Leave Out Industries --------------------------------------------------
  # For control group (1-digit SIC)
  if (Models$Leave_Out_Industries_Control[i] != "None") {
    sic_digit <- substr(Models$Leave_Out_Industries_Control[i], 1, 1)
    if (sic_digit == "6") {
      # Exclude Finance, Insurance, and RE, but keep insurance firms
      subs <- intersect(subs, which(!(substr(dat$sic4, 1, 1) == sic_digit) | dat$ins == 1))
    } else {
      subs <- intersect(subs, which(!(substr(dat$sic4, 1, 1) == sic_digit)))
    }
  }

  # For treatment group (specific 4-digit SIC codes)
  if (Models$Leave_Out_Industries_Treat[i] != "None") {
    sic_code <- substr(Models$Leave_Out_Industries_Treat[i], 1, 4)
    subs <- intersect(subs, which(!(dat$sic4 == as.numeric(sic_code))))
  }

  # Estimating the Models
  for (dv in dep_vars) {
    # Determine the correct winsorized variable name
    if (Models$Sample[i] == "Restricted") {
      if (endsWith(dv, "_b")) {
        win_dv <- paste0("wr", sub("_b$", "_r", dv))
      } else {
        win_dv <- paste0("wr", dv)
      }
    } else {
      win_dv <- paste0("wb", dv)
    }
    
    # Check if the winsorized variable exists, if not, use the non-winsorized version
    if (!win_dv %in% names(dat)) {
      win_dv <- dv
      warning(paste("Winsorized version of", dv, "not found. Using non-winsorized version."))
    }

    formula <- as.formula(paste(
      win_dv, "~",
      paste(c("eu:ins:post + ins:post + eu:post", controls), collapse = " + "),
      "| id + obsy"
    ))

    model <- tryCatch({
      feols(formula,
        data = dat[subs, ],
        cluster = ~id
      )
    }, error = function(e) {
      warning(paste("Error in model for", win_dv, ":", e$message))
      return(NULL)
    })

    if (!is.null(model)) {
      # Save estimates using the original dv name for column naming
      Models[i, paste0("treat_", dv)] <- coef(model)["eu:ins:post"]
      Models[i, paste0("ub_treat_", dv)] <- confint(model, level = .9)["eu:ins:post", 2]
      Models[i, paste0("lb_treat_", dv)] <- confint(model, level = .9)["eu:ins:post", 1]
    } else {
      Models[i, paste0("treat_", dv)] <- NA
      Models[i, paste0("ub_treat_", dv)] <- NA
      Models[i, paste0("lb_treat_", dv)] <- NA
    }
  }

  Models[i, ]
}

# Stop the cluster
stopCluster(cl)

# Saving
# save(Models, file = "../03 - Results/spec_curve_models.RData")

# nolint end