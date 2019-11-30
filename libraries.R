# Libraries required for analysis of data
library(ggplot2)
library(MBESS)
library(scales)
library(HLMdiag) # modifies cooks.distance for lmer models
library(userfriendlyscience)
library(lmerTest)
library(MASS)
library(emmeans)
library(ggpubr)
library(here)
library(pwr)
library(DT) # for R Markdown tables
library(tidyverse)
library(broom)
library(furrr) # load furrr for multicore purrr mapping
# Ensure `select` and `filter` are the `dplyr` functions`
select <- dplyr::select
filter <- dplyr::filter