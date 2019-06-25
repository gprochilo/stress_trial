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
  
  # Table 3
  
  pair.test.table(dvs = c("pss",
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
  
  # Supp. results table 1
  
  pair.multiconf(dvs = c("pss",
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

