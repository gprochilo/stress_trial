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

# Reproduce Results Tables

# Table 1

bl.table()

# Table 2

descrip.res(dv.list = dos.vars, df = "dat.wide", round = TRUE, sav2csv = TRUE, name = "baseline")

# Table 3

pair.table()

# Table 4

lmm.table() 

# Table 5

aipe.res(retention.rate = 0.7083, trial.reps = 10, assurance = 0.99)

#-------------------------------------------------------------------------------

# Reproduce in-text statistics

# Critical t and d values 

critical.t(n = 17, type = "dz", alpha = 0.2, twotailed = TRUE)

# Common language effect size for PSS-10 gain scores

pair.test(x = "pss", df.wide = "dat.wide", df.long = "dat.long", plotit = FALSE)$CL

# Sensitivity analysis for survivorship bias in PSS-10 gain scores

pair.test.sensitivity("pss", dropout.n = 7, df.wide = "dat.wide")$res

# Feasibility rates

feasibility(give = "feas.rates")

# Safeguard effect size for PSS-10 gain scores

safeguard.ES(dv = "pss", df = "dat.wide")

#-------------------------------------------------------------------------------

# Reproduce Supp. Results tables

# Supp. results table 1

pair.supp()

# Supp. results table 2

supp.lmm()

# Supp. results table 3

supp.model("vo2.dv")

# Supp. results table 4

supp.model("pcnt.vo2.dv")

# Supp. results table 5

supp.model("hr.dv")

# Supp. results table 6

supp.model("rpe.dv")

#-------------------------------------------------------------------------------

# Reproduce in-text figures

# Figure 1 (results only)

feasibility(give = "participant.flow")

# Figure

# 2A/B
pair.test(x = "pss", df.wide = "dat.wide", df.long = "dat.long", plotit = TRUE)

# 2C
pair.test(x = "maas", df.wide = "dat.wide", df.long = "dat.long", plotit = TRUE)

# 2D
pair.test(x = "erq.cr", df.wide = "dat.wide", df.long = "dat.long", plotit = TRUE)

# 2E
pair.test(x = "vo2max", df.wide = "dat.wide", df.long = "dat.long", plotit = TRUE)

# Figure 3

# A
emm.test(econ = "vo2.dv", df = "dat.econ.long", transf = "none", effect = "time")

#B
emm.test(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "time")

#C
emm.test(econ = "hr.dv", df = "dat.econ.long", transf = "none", effect = "time")

#D
emm.test(econ = "rpe.dv", df = "dat.econ.long", transf = "none", effect = "time")

# Figure 4

precPLOT.pair()

#-------------------------------------------------------------------------------

# AIPE with assurance simulations

# δz = -0.60

aipe.assurance(d = as.numeric(safeguard.ES(dv = "pss", df = "dat.wide")["est",][1]),
               reps = 2000, 
               n.seq = c(10,200,1),
               effect = "dz", 
               seed = TRUE)

# Safeguard δz = -0.37

aipe.assurance(d = as.numeric(safeguard.ES(dv = "pss", df = "dat.wide")["0.8",][1]),
               reps = 2000, 
               n.seq = c(10,200,1),
               effect = "dz",
               seed = TRUE)

# Between-group effect sizes δs

aipe.assurance(d = 0.8, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.7, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.6, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.5, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.4, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.3, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)
aipe.assurance(d = 0.2, reps = 2000, n.seq = c(10,900,1), effect = "ds", seed = TRUE)

#-------------------------------------------------------------------------------

## Other functions

#-------------------------------------------------------------------------------

# Comprehensive results of 1-df effects

# Individually (replace "pss" with any variable name in the vars vector)
pair.test(x = "pss",df.wide = "dat.wide", df.long = "dat.long", plotit = TRUE)

# Perform all 1-df analyses, generate all plots, and save results to csv
pair.res(vars, "dat.wide", "dat.long", plotit = TRUE, sav2csv = TRUE)

#-------------------------------------------------------------------------------

# Comprehensive model results of multi-df effects

# Individually compute model for each LMM

# Absolute oxygen cost
mixed.mod(econ = "vo2.dv",df = "dat.econ.long",transf = "none")
# Relative oxygen cost
mixed.mod(econ = "pcnt.vo2.dv",df = "dat.econ.long",transf = "none")
# Heart rate
mixed.mod(econ = "hr.dv",df = "dat.econ.long",transf = "none")
# Perceived exertion
mixed.mod(econ = "rpe.dv",df = "dat.econ.long",transf = "none")

# Compute all models for all LMMs and save to csv

mixed.model.res(econ = econ, df = "dat.econ.long", transf = "none", sav2csv = TRUE)

#-------------------------------------------------------------------------------

# Comprehensive results of follow-up pairwise contrasts

# Individually compute pairwise contrasts for each LMM main effect and/or interaction
# and generate plots

# Absolute oxygen cost
emm.test(econ = "vo2.dv", df = "dat.econ.long", transf = "none", effect = "time")
# Relative oxygen cost main effect and interaction follow-up
emm.test(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "time")
emm.test(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "timeXsamp")
# Heart rate
emm.test(econ = "hr.dv", df = "dat.econ.long", transf = "none", effect = "time")
# Perceived exertion
emm.test(econ = "rpe.dv", df = "dat.econ.long", transf = "none", effect = "time")

# Compute all pairwise contrasts, all plots, and save results in csv files

# Follow-up main effects
emm.test.res(econ = econ, df = "dat.econ.long", transf = "none", effect = "time", plotit = TRUE)
# Follow-up interactions
emm.test.res(econ = econ, df = "dat.econ.long", transf = "none", effect = "timeXsamp", plotit = FALSE)

#-------------------------------------------------------------------------------

# Comprehensive assumption results of multi-df effects

# Generate assumption plots for each LMM and save as png/eps

# Absolute oxygen cost
mixed.assum(econ = "vo2.dv",df = "dat.econ.long",transf = "none")
# Relative oxygen cost
mixed.assum(econ = "pcnt.vo2.dv",df = "dat.econ.long",transf = "none")
# Heart rate
mixed.assum(econ = "hr.dv",df = "dat.econ.long",transf = "none")
# Perceived exertion
mixed.assum(econ = "rpe.dv",df = "dat.econ.long",transf = "none")

# Generate all assumption plots for all LMMs at once and save as png/eps

mixed.assum.res(econ = econ, df = "dat.econ.long", transf = "none")

