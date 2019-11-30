# The Effects of a 16-week Aerobic Exercise and Mindfulness-based Intervention 
# on Chronic Psychosocial Stress: A Nonrandomized Pilot and Feasibility Trial
#
#
# These commands can reproduce the analysis without the markdown file
# Before running any of the functions ensure you source the following:

  source("libraries.R")
  source("Rallfun-v35.txt")
  source("import_data.R")
  source("ProcFun_stress-v1.R")

#-------------------------------------------------------------------------------

  # Reproduce In-text Tables
  
  # Table 1
  
  baseline.res(sav2csv = TRUE, sav2dir = c("results","tables","table1.csv"))
  
  # Table 2
  
  descrip.res(dvs = c("attendance.pcnt.1", 
                      "meditation.tot.1", 
                      "runs.tot.1", 
                      "mean.pcnt.vo2max.1", 
                      "distance.km.tot.1", 
                      "runtime.tot.1"), 
              dataset = dat.wide, 
              sav2csv = TRUE, 
              sav2dir = c("results","tables","table2.csv"))
  
  # Table 3 - updated to use pre-test SD as standardizer for d (November 2019)
  
  pair.test.table2(dvs = c("pss",
                           "sdass",
                           "adass",
                           "ddass",
                           "who",
                           "maas",
                           "erq.cr",
                           "rrs.br",
                           "pswq",
                           "vo2max"), 
                  long.dataset = dat.long, 
                  sav2csv = TRUE, 
                  sav2dir = c("results", "tables", "table3.csv"))
  
  # Table 4
  
  lmm.res(dvs = c("vo2.dv",
                  "pcnt.vo2.dv",
                  "hr.dv",
                  "rpe.dv"), 
          long.dataset = dat.econ.long, 
          sav2csv = TRUE, 
          sav2dir = c("results", "tables", "table4.csv"))

  # Table 5

  aipe.table(retention.rate = 0.71,
             n.trial = 10, 
             max.per.trial = 20, 
             sav2csv = TRUE, 
             sav2dir = c("results", "tables", "table5.csv"))

#-------------------------------------------------------------------------------

  # Reproduce In-text Figures
  
  # Figure 1 (results only)
  
  feasibility(values = "flow")
  
  # Figure 2
  
  # 2A/B
  pair.test(dv = "pss", long.dataset = dat.long, plots = TRUE, silent = TRUE)
  
  # Figure 3
  
  # A
  emm.test(dv = "vo2.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE, silent = TRUE)
  
  # B
  emm.test(dv = "pcnt.vo2.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE, silent = TRUE)
  
  # C
  emm.test(dv = "hr.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE, silent = TRUE)
  
  # D
  emm.test(dv = "rpe.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE, silent = TRUE)
  
#-------------------------------------------------------------------------------
  
  # Reproduce In-text Statistics
  
  # Critical t and d values 
  
  crit.vals(n = 17, type = "paired", alpha = 0.2)
  
  # Common language effect size for PSS-10 gain scores
  
  pair.test(dv = "pss", long.dataset = dat.long, plots = FALSE, silent = TRUE)$gain.res$cl.ES
  
  # Sensitivity analysis for survivorship bias in PSS-10 gain scores
  
  pair.sensitivity(dv = "pss", dropout.n = 7, long.dataset = dat.long)
  
  # Feasibility rates
  
  feasibility(values = "f.rates")
  
  # Safeguard effect size for PSS-10 gain scores
  
  safeguard.es(dv = "pss", long.dataset = dat.long, type = "dz", silent = TRUE)$ci.LL_0.80

#-------------------------------------------------------------------------------
  
  # Sample Size Planning Analyses
  
  # Delta-z = -0.6
  
  aipe.d(d = -0.6, 
         type = "dz",
         conf.level = 0.95, 
         assurance = 0.99, 
         retention.rate = 0.71)
  
  # Delta-z = -0.37 (safeguard effect size)
  
  aipe.d(d = -0.37, 
         type = "dz",
         conf.level = 0.95, 
         assurance = 0.99, 
         retention.rate = 0.71)
  
  # Delta-s
  
  aipe.mult.d(dvals = seq(0.2, 0.8, 0.1), 
              type = "ds", 
              conf.level = 0.95, 
              assurance = 0.99, 
              return.order.d = "descending",
              retention.rate = 0.71)
  
#-------------------------------------------------------------------------------

  # Reproduce Supplementary Results tables
  
    # Updated to compute d_pre (use `pair.multiconf()`` for d_av) (November 2019)
  
  # Supp. results table 1
  
  pair.multiconf2(dvs = c("pss",
                          "sdass",
                          "adass",
                          "ddass",
                          "who",
                          "maas",
                          "erq.cr",
                          "rrs.br",
                          "pswq",
                          "vo2max"),
                 long.dataset = dat.long,
                 sav2csv = TRUE,
                 sav2dir = c("results", "tables", "supp_table1.csv"))
  
  # Supp. results table 2

  lmm.supp.res(dvs = c("vo2.dv",
                       "pcnt.vo2.dv",
                       "hr.dv",
                       "rpe.dv"), 
               long.dataset = dat.econ.long, 
               sav2csv = TRUE, 
               sav2dir = c("results", "tables", "supp_table2.csv"))

# Supp. results table 3

  supp.model(dv = "vo2.dv", 
             long.dataset = dat.econ.long, 
             sav2csv = TRUE, 
             sav2dir = c("results", "tables", "supp_table3.csv"))

# Supp. results table 4

  supp.model(dv = "pcnt.vo2.dv", 
             long.dataset = dat.econ.long, 
             sav2csv = TRUE, 
             sav2dir = c("results", "tables", "supp_table4.csv"))

# Supp. results table 5

  supp.model(dv = "hr.dv", 
             long.dataset = dat.econ.long, 
             sav2csv = TRUE, 
             sav2dir = c("results", "tables", "supp_table5.csv"))

# Supp. results table 6

  supp.model(dv = "rpe.dv", 
             long.dataset = dat.econ.long, 
             sav2csv = TRUE, 
             sav2dir = c("results", "tables", "supp_table6.csv"))

#-------------------------------------------------------------------------------

## Other functions

#-------------------------------------------------------------------------------

# Comprehensive results of 1-df effects
  
  # Note: these functions have not been update to report d_pre
  # Instead, they report d_av (using average sd as standardizer)

# Individually: replace "pss" with any variable in 'dat.long'
pair.test(dv = "pss", long.dataset = dat.long, plots = TRUE)

# Perform all 1-df analyses, generate all plots, and save results to csv
pair.test.res(dvs = c("pss",
                      "sdass",
                      "adass",
                      "ddass",
                      "who",
                      "maas",
                      "erq.cr",
                      "rrs.br",
                      "pswq",
                      "vo2max"), 
              long.dataset = dat.long, 
              plots = TRUE, 
              sav2csv = TRUE, 
              sav2dir = c("results", "paired_data","paired_data_res.csv"))

#-------------------------------------------------------------------------------

# Comprehensive model results of multi-df effects

# Individually compute model for each LMM

# Absolute oxygen cost
mixed.mod(dv = "vo2.dv", long.dataset = dat.econ.long, transf = "none")

# Relative oxygen cost
mixed.mod(dv = "pcnt.vo2.dv", long.dataset = dat.econ.long, transf = "none")

# Heart rate
mixed.mod(dv = "hr.dv", long.dataset = dat.econ.long, transf = "none")

# Perceived exertion
mixed.mod(dv = "rpe.dv", long.dataset = dat.econ.long, transf = "none")

# Compute all models for all LMMs and save to csv

mixed.mod.res(dvs = c("vo2.dv",
                      "pcnt.vo2.dv",
                      "hr.dv",
                      "rpe.dv"), 
              long.dataset = dat.econ.long, transf = "none", 
              sav2csv = TRUE, 
              sav2dir = c("results", "mixed_model_data", "mixed_mod_res.csv"))

#-------------------------------------------------------------------------------

# Comprehensive results of follow-up pairwise contrasts

# Individually compute pairwise contrasts for each LMM main effect and/or interaction
# and generate plots

# Absolute oxygen cost
emm.test(dv = "vo2.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE)

# Relative oxygen cost main effect and interaction follow-up
emm.test(dv = "pcnt.vo2.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE)
emm.test(dv = "pcnt.vo2.dv", long.dataset = dat.econ.long, effect = "timeXsamp", plots = TRUE)

# Heart rate
emm.test(dv = "hr.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE)

# Perceived exertion
emm.test(dv = "rpe.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE)

# Compute all pairwise contrasts, all plots, and save results in csv files

# Follow-up main effects

emm.test.res(dvs = c("vo2.dv",
                     "pcnt.vo2.dv",
                     "hr.dv",
                     "rpe.dv"), 
             long.dataset = dat.econ.long, 
             effect = "time", 
             sav2csv = TRUE, 
             plots = TRUE)

# Follow-up interactions

emm.test.res(dvs = c("vo2.dv",
                     "pcnt.vo2.dv",
                     "hr.dv",
                     "rpe.dv"), 
             long.dataset = dat.econ.long, 
             effect = "time", 
             sav2csv = TRUE, 
             plots = FALSE)

#-------------------------------------------------------------------------------

# Comprehensive assumption results of multi-df effects

# Absolute oxygen cost
mixed.assum(dv = "vo2.dv", long.dataset = dat.econ.long, transf = "none", print.plots = TRUE)
# Relative oxygen cost
mixed.assum(dv = "pcnt.vo2.dv", long.dataset = dat.econ.long, transf = "none", print.plots = TRUE)
# Heart rate
mixed.assum(dv = "hr.dv", long.dataset = dat.econ.long, transf = "none", print.plots = TRUE)
# Perceived exertion
mixed.assum(dv = "rpe.dv", long.dataset = dat.econ.long, transf = "none", print.plots = TRUE)

# Generate all assumption plots for all LMMs at once and save as png/eps

mixed.assum.res(dvs = c("vo2.dv",
                        "pcnt.vo2.dv",
                        "hr.dv",
                        "rpe.dv"), 
                long.dataset = dat.econ.long, 
                transf = "none")

#-------------------------------------------------------------------------------

# 2019 update - computation of d statistics for linear mixed model contrasts

#-------------------------------------------------------------------------------

# Compute Cohen's d for each main effect contrast for each linear mixed model
# Note - these may take some time since the CI is computed via a bootstrap
# simulation approach.

# Absolute oxygen cost
mixed.mod(dv = "vo2.dv",
          long.dataset = dat.econ.long, 
          transf = "none", 
          silent = T)$model %>% 
    gen_d(model = ., 
        spec = ~time*samp, 
        con_vector = c(-1/4, 1/4, -1/4, 1/4, -1/4, 1/4, -1/4, 1/4),
        conf_int = c(0.80, 0.95), 
        nsim = 2000, 
        seed_val = 2)
  

# Relative oxygen cost
mixed.mod(dv = "pcnt.vo2.dv",
          long.dataset = dat.econ.long, 
          transf = "none", 
          silent = T)$model %>% 
    gen_d(model = ., 
        spec = ~time*samp, 
        con_vector = c(-1/4, 1/4, -1/4, 1/4, -1/4, 1/4, -1/4, 1/4),
        conf_int = c(0.80, 0.95), 
        nsim = 2000, 
        seed_val = 2)

# Heart rate
mixed.mod(dv = "hr.dv",
          long.dataset = dat.econ.long, 
          transf = "none", 
          silent = T)$model %>% 
    gen_d(model = ., 
        spec = ~time*samp, 
        con_vector = c(-1/4, 1/4, -1/4, 1/4, -1/4, 1/4, -1/4, 1/4),
        conf_int = c(0.80, 0.95), 
        nsim = 2000, 
        seed_val = 2)

# Perceived exertion
mixed.mod(dv = "rpe.dv",
          long.dataset = dat.econ.long, 
          transf = "none", 
          silent = T)$model %>% 
    gen_d(model = ., 
        spec = ~time*samp, 
        con_vector = c(-1/4, 1/4, -1/4, 1/4, -1/4, 1/4, -1/4, 1/4),
        conf_int = c(0.80, 0.95), 
        nsim = 2000, 
        seed_val = 2)

# Call already-computed statistics that are saved in the `data` directory to avoid
# need to run simulations

# Absolute oxygen cost main effect
read_csv(here::here("data", "d_statistics", "d_vo2.csv"), col_types = cols())

# Relative oxygen cost main effect
read_csv(here::here("data", "d_statistics", "d_pcnt_vo2.csv"), col_types = cols())

# Heart rate main effect
read_csv(here::here("data", "d_statistics", "d_hr.csv"), col_types = cols())

# RPE main effect
read_csv(here::here("data", "d_statistics", "d_rpe.csv"), col_types = cols())


# Extract the d statistic results

# Absolute oxygen cost main effect
read_csv(here::here("data", "d_statistics", "d_vo2.csv"), col_types = cols()) %>% 
  {sprintf("%s: %.2f [%.2f, %.2f]", .$conf_int, .$d, .$d_ci_LL, .$d_ci_UL)} %>% 
  cbind()

# Relative oxygen cost main effect
read_csv(here::here("data", "d_statistics", "d_pcnt_vo2.csv"), col_types = cols()) %>% 
  {sprintf("%s: %.2f [%.2f, %.2f]", .$conf_int, .$d, .$d_ci_LL, .$d_ci_UL)} %>% 
  cbind()

# Heart rate main effect
read_csv(here::here("data", "d_statistics", "d_hr.csv"), col_types = cols()) %>% 
  {sprintf("%s: %.2f [%.2f, %.2f]", .$conf_int, .$d, .$d_ci_LL, .$d_ci_UL)} %>% 
  cbind()

# RPE main effect
read_csv(here::here("data", "d_statistics", "d_rpe.csv"), col_types = cols()) %>% 
  {sprintf("%s: %.2f [%.2f, %.2f]", .$conf_int, .$d, .$d_ci_LL, .$d_ci_UL)} %>% 
  cbind()

