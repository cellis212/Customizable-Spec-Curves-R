# Pre-amble ---------------------------------------------------------------

# Clearing Memory
rm(list = ls())

# Loading packages
require(magrittr)
require(tidyverse)
require(cowplot)


#### MANUAL CHANGE REQUIRED ####
# Load models
load("../03 - Results/spec_curve_models.RData")

#### MANUAL CHANGE REQUIRED ####
# Names of treat vars
# Important they are in the same order as the Models columns
treatvars <- c("lnexretvar_d", "lnicc_gebhardt", "pca_risk_b", "lnzscore", "lnmDD", "pca_default_b", "mroll", "lnpi", "pca_liq_b")

col_location <- which(names(Models) %in% paste0("treat_", treatvars))

curves <- cbind(treatvars, col_location) %>% as.data.frame()
curves$col_location <- curves$col_location %>% as.character() %>% as.numeric()


#### AUTOMATIC DIRECTION DETERMINATION ####
# Direction of curves
# 1 for ascending, -1 for descending
curves$direct <- sapply(curves$col_location, function(col) {
  first_estimate <- Models[1, col]
  ifelse(first_estimate < 0, 1, -1)
})


#### MANUAL CHANGE REQUIRED ####
# Order to print the options in
printOrder <- c(
  "Sample", "Leave_Out_Control", "Add_Control",
  "Leave_Out_Countries", "Leave_Out_Countries_Control",
  "Leave_Out_Industries_Control", "Leave_Out_Industries_Treat"
)

# Initialize vectors to store results
perSig <- numeric(length = nrow(curves))
q05 <- numeric(length = nrow(curves))
q95 <- numeric(length = nrow(curves))
pref_coef <- numeric(length = nrow(curves))  

# Loop over different treat vars
for (curve_index in 1:nrow(curves)) {
# curve_index <- 1 (for testing)
  Models$estimate <- Models[, (curves[curve_index, 2])]
  Models$ub <- Models[, (curves[curve_index, 2] + 1)]
  Models$lb <- Models[, (curves[curve_index, 2] + 2)]

  # For testing or subsample
  l <- which(Models$estimate == 0)

  if (length(l) > 0) {
    Models <- Models[-l, ]
    print("Warning, some models may not have run!")
  }

  # Getting Significance
  Models$sig <- (sign(Models$ub) == sign(Models$lb)) %>% as.factor()

  # Ordering by estimate
  Models <- Models[order(Models$estimate * curves$direct[curve_index]), ]
  Models$Order <- 1:nrow(Models)


  #### MANUAL CHANGE REQUIRED ####
  # Get preferred specification
  pref <- which(Models$Sample == "Broad" &
    Models$Leave_Out_Control == "None" &
    Models$Add_Control == "None" &
    Models$Leave_Out_Countries == "None" &
    Models$Leave_Out_Industries_Control == "None" &
    Models$Leave_Out_Industries_Treat == "None")

  if (length(pref) == 0) {
    print("Preferred Spec not found!")
    pref_coef[curve_index] <- NA
  } else {
    pref_coef[curve_index] <- Models$estimate[pref]  # Store preferred specification coefficient
  }

  # Plotting ----------------------------------------------------------------

  # Makes sure that all ticks aren't gray if all significant
  if (length(unique(Models$sig)) == 1) {
    gstart <- 0
  } else {
    gstart <- .7
  }

  # Makes top curve
  curve <- ggplot(data = Models) +
    geom_point(
      mapping = aes(x = Order, y = estimate, color = sig), size = 2
    ) +
    scale_color_grey(
      start = gstart,
      end = 0
    ) +
    geom_linerange(
      mapping = aes(x = Order, ymin = lb, ymax = ub), colour = "blue", size = .05, alpha = .15
    ) +
    geom_vline(xintercept = pref, color = "red", linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Add this line
    theme(legend.position = "none") +
    labs(x = "Regression Number", y = "Estimate")

  #### MANUAL CHANGE REQUIRED ####
  # Specs (need to edit this based on column names and order)
  Models %>%
    gather(key, value, Sample:Leave_Out_Industries_Treat) -> plotDat

  # Makes bottom plot
  specs <- ggplot(
    data = plotDat,
    aes(
      x = plotDat$Order,
      y = plotDat$value,
      color = plotDat$sig
    )
  ) +
    scale_color_grey(
      start = gstart,
      end = 0
    ) +
    geom_point(
      size = 1, # Adjust this for tick size
      shape = 124
    ) +
    facet_grid(rows = vars(key), scales = "free", space = "free") +
    theme(
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text.y = element_text(size = 12, colour = "black"),  # Adjust this for left-side text size
      axis.text.x = element_text(colour = "black"),
      strip.text.x = element_blank(),
      strip.text.y = element_text(
        face = "bold",
        size = 11 # Adjust this for right-side text size
      ),
      strip.background.y = element_blank()
    ) +
    labs(x = "", y = "")

  #### MANUAL CHANGE REQUIRED ####
  # Fixing height for vars (lines in gp$heights that have null in the name are the ones to change)
  gp <- ggplotGrob(specs)

  gp$heights[13] <- gp$heights[13] * 1.6
  gp$heights[17] <- gp$heights[17] * 1.15


  plot_grid(curve,
    gp,
    labels = c(),
    align = "v",
    axis = "rbl",
    rel_heights = c(1.5, 6),
    ncol = 1
  )


  #### MANUAL CHANGE REQUIRED ####
  # This is where it gets saved to
  savename <- paste0(
    "../07 - Spec Curves/spec_curve_",
    curves[curve_index, 1], ".png"
  )


  #### MANUAL CHANGE REQUIRED ####
  # You'll need to play around with the height and width
  # Note that the text viewed in the plot preview in RStudio is not the same size as what is saved
  ggsave(
    filename = savename,
    width = 12,
    height = 14,
    units = "in"
  )

  # Store results
  q05[curve_index] <- quantile(Models$estimate, .05, na.rm = TRUE)
  q95[curve_index] <- quantile(Models$estimate, .95, na.rm = TRUE)

  m <- sum(Models$ub < 0)
  n <- sum(Models$lb > 0)
  l <- max(m, n)
  perSig[curve_index] <- l / nrow(Models)
}

#### MANUAL CHANGE REQUIRED ####
# Create a data frame with the results and add informative names
results_table <- data.frame(
  Curve = c("Total Risk", "ICC", "PCA Risk Factor", 
            "Z-Score", "Distance-to-Default", "PCA Default Factor",
            "Bid-Ask Spread", "Price Impact", "PCA Liquidity Factor"),
  Variable = curves$treatvars,
  PreferredCoef = pref_coef,  # New column
  PercentSignificant = perSig,
  Q05 = q05,
  Q95 = q95
)


# Function to format numbers
format_number <- function(x) {
  sprintf("%.3f", x)
}

#### MANUAL CHANGE REQUIRED ####
# Create LaTeX table with notes using threeparttable separated by panels
cat("\\begin{threeparttable}[htbp]
\\caption{Specification Curve Results}
\\label{tab:spec_curve_results}
\\begin{tabular}{lcccc}
\\hline
Variable & Preferred Coef. & Percent Significant & 5th Percentile & 95th Percentile \\\\
\\hline
\\multicolumn{5}{l}{\\textbf{Panel A: Equity Risk}} \\\\[0.5em]
", paste(sapply(1:3, function(i) {
  sprintf("\\hspace{0.5em}%s & %s & %.1f\\%% & %s & %s \\\\",
          results_table$Curve[i],
          format_number(results_table$PreferredCoef[i]),
          results_table$PercentSignificant[i] * 100,
          format_number(results_table$Q05[i]),
          format_number(results_table$Q95[i]))
}), collapse = "\n"), "
\\\\[0.5em]
\\hline
\\multicolumn{5}{l}{\\textbf{Panel B: Default Risk}} \\\\[0.5em]
", paste(sapply(4:6, function(i) {
  sprintf("\\hspace{0.5em}%s & %s & %.1f\\%% & %s & %s \\\\",
          results_table$Curve[i],
          format_number(results_table$PreferredCoef[i]),
          results_table$PercentSignificant[i] * 100,
          format_number(results_table$Q05[i]),
          format_number(results_table$Q95[i]))
}), collapse = "\n"), "
\\\\[0.5em]
\\hline
\\multicolumn{5}{l}{\\textbf{Panel C: Liquidity}} \\\\[0.5em]
", paste(sapply(7:9, function(i) {
  sprintf("\\hspace{0.5em}%s & %s & %.1f\\%% & %s & %s \\\\",
          results_table$Curve[i],
          format_number(results_table$PreferredCoef[i]),
          results_table$PercentSignificant[i] * 100,
          format_number(results_table$Q05[i]),
          format_number(results_table$Q95[i]))
}), collapse = "\n"), "
\\\\[0.5em]
\\hline
\\end{tabular}
\\begin{tablenotes}
\\small
\\item \\linespread{1}\\selectfont\\textit{Notes:} This table presents the results of specification curve analyses for various risk measures. 
'Preferred Coef.' shows the coefficient from the preferred specification, which uses the broad sample without any exclusions. 
'Percent Significant' indicates the percentage of specifications where the coefficient is statistically significant at the 10\\% level. 
'5th Percentile' and '95th Percentile' show the distribution of coefficients across all specifications. 
Panel A reports results for equity risk measures, Panel B for default risk measures, and Panel C for liquidity measures. 
ICC stands for Implied Cost of Capital, PCA for Principal Component Analysis, and Price Impact is measured using \\citepapos{Amihud2002} illiquidity ratio.
\\end{tablenotes}
\\end{threeparttable}
", file = "./Effects of the Solvency II framework/07 - Spec Curves/spec_curve_results.tex")