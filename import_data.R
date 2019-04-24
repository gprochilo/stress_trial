# Import data stored in long format

dat.long <- read.csv(here("data","data_long.csv"))

# Change object class of id and time to factors

dat.long$id = as.factor(dat.long$id)
dat.long$time = as.factor(dat.long$time)

# Convert dataset to wide format to make some tests easier to perform

dat.wide <- reshape(dat.long, idvar = "id", timevar = "time", direction = "wide")

# Add PSS change score to dat.wide

pss.ch = dat.wide$pss.1-dat.wide$pss.0
dat.wide = cbind(dat.wide, pss.ch)

# Import aerobic economy data in long format

dat.econ.long <- read.csv(here("data","data_economy.csv"))

# Change object class of id, time, and samp to factors

dat.econ.long$id = as.factor(dat.econ.long$id)
dat.econ.long$time = as.factor(dat.econ.long$time)
dat.econ.long$samp = as.factor(dat.econ.long$samp)

# Remove participants with missing values for velocity 1 and 2

test = dat.econ.long[complete.cases(dat.econ.long),] # remove rows with NAs
n_occur = data.frame(table(test$id)) # Count how many responses each ID has given
keep.id = n_occur[n_occur$Freq == 8,] # Report IDs with full set of 6 responses
dat.econ.long = test[test$id %in% n_occur$Var1[keep.id$Var1],] # Remove IDs with < 6 responses

# Convert dataset to wide format to make it easier to perform some tests

dat.econ.wide <- reshape(dat.econ.long, idvar = c("id","samp"), timevar = "time", direction = "wide")
dat.econ.wide = reshape(dat.econ.wide, idvar = c("id"), timevar = "samp", direction = "wide")

# Import precision power analysis results (2000 repetitions)

ds_aipe_0.2 <- read.csv(here("data","precision_power","0.2_ds_aipe.csv"))
ds_aipe_0.3 <- read.csv(here("data","precision_power","0.3_ds_aipe.csv"))
ds_aipe_0.4 <- read.csv(here("data","precision_power","0.4_ds_aipe.csv"))
ds_aipe_0.5 <- read.csv(here("data","precision_power","0.5_ds_aipe.csv"))
ds_aipe_0.6 <- read.csv(here("data","precision_power","0.6_ds_aipe.csv"))
ds_aipe_0.7 <- read.csv(here("data","precision_power","0.7_ds_aipe.csv"))
ds_aipe_0.8 <- read.csv(here("data","precision_power","0.8_ds_aipe.csv"))
dz_aipe_0.60 <- read.csv(here("data","precision_power","0.6_dz_aipe.csv"))
dz_aipe_0.37 <- read.csv(here("data","precision_power","0.37_dz_aipe.csv"))

# Import feasibility data

feas <- read.csv(here("data","feasibility.csv"))
