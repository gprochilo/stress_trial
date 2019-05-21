# MIT License
# 
# ProcFun_stress-v1: R functions for stress trial mansuscript.
# Copyright (c) 2019 Guy A. Prochilo
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# 
# Contact
# Name: Guy A. Prochilo
# Email: guy.prochilo@gmail.com
# 
# Last update: May 2019
#
# Cite as:
# Prochilo, G. A. (2019). ProcFun_stress-v1: R functions for stress trial mansuscript.
# Retrieved from https://github.com/gprochilo

#-------------------------------------------------------------------------------

# Print the above license whenever ProcFun_stress-v1.R is sourced
# If the license exceeds 32 lines in future, update 32 to a higher number

writeLines(readLines("ProcFun_stress-v1.R",32))

#-------------------------------------------------------------------------------
# Begin scripts
#-------------------------------------------------------------------------------

################################################################################
# Define a vector with dosage variable names
################################################################################

dos.vars <- cbind("attendance.pcnt.1",
                  "meditation.tot.1",
                  "runs.tot.1",
                  "mean.pcnt.vo2max.1",
                  "distance.km.tot.1",
                  "runtime.tot.1"
)

#-------------------------------------------------------------------------------

quiet <- function(x){ 

################################################################################
# This function suppresses print output of a function
# Retrieved from: http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#
# Example use:
# res = quiet(ci.sm(sm = 0.5, N = 50))
################################################################################
  
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#-------------------------------------------------------------------------------

feasibility <- function(give = "participant.flow"){

################################################################################
# This function returns the Participant Flow chart data and feasibility rates
#
# Example use:
# feasibility(give = "feas.rates")
# feasibility(give = "participant.flow")
################################################################################
  
  # Define a frequencies function to return frequencies of reasons

  # freq() is part of the userfriendlyscience package:
  # Peters, G. Y. (2018). userfriendlyscience: Quantitative analysis made accessible. R package
  # version 0.7.2. Retrieved from https://CRAN.R-project.org/package=userfriendlyscience
  
  freqs <- function(i){
    res = freq(i)
    
    res = as.data.frame(
      cbind(res$intermediate$categoryNames,
            as.numeric(res$intermediate$frequencies.raw),
            as.numeric(round(res$intermediate$frequencies.prop*100,2)))
    )
    
    res$V2 = as.numeric(as.character(res$V2))
    res$V3 = as.numeric(as.character(res$V3))
    
    res = res[order(res$V2, decreasing = TRUE),]
    colnames(res) = c("Reason", "Freq","%")
    
    return(res)
    
  }
  
  # Compute n for each stage of the participant flow
  
  # Enrolment
  n.applied = length(feas$id)
  n.ineligible = sum(na.omit(feas$ineligible == 1))
  n.eligible = n.applied-n.ineligible
  reason.ineligible = freqs(na.omit(feas$why.ineligible))
  n.declined = sum(na.omit(feas$they.declined == 1))
  
  # Assignment and allocation
  n.nonrandomized = sum(na.omit(feas$not.nonrandomized == 0))
  
  # Follow-up
  n.completed = sum(na.omit(feas$dropout == 0))
  n.dropout = sum(na.omit(feas$dropout == 1))
  reason.dropout = freqs(na.omit(feas$why.dropout))
  
  # Analysis
  n.quest = sum(na.omit(feas$missed.quest.t1 == 0))
  n.vo2max = sum(na.omit(feas$missed.vo2max.t1 == 0))
  n.economy.6km = sum(na.omit(feas$max.velocity >= 6))
  n.economy.8km = sum(na.omit(feas$max.velocity >= 8))
  n.economy.10km = sum(na.omit(feas$max.velocity >= 10))
  n.economy.12km = sum(na.omit(feas$max.velocity >= 12))
  n.economy.14km = sum(na.omit(feas$max.velocity >= 14))
    
  # Compute feasibility rates
  
  retention.rate = (n.completed/n.nonrandomized)*100
  dropout.rate = (n.dropout/n.nonrandomized)*100
  response.rate.quest = (n.quest/n.completed)*100
  response.rate.vo2max = (n.vo2max/n.completed)*100
  ae.rate.6km = (n.economy.6km/n.completed)*100
  ae.rate.8km = (n.economy.8km/n.completed)*100
  ae.rate.10km = (n.economy.10km/n.completed)*100
  ae.rate.12km = (n.economy.12km/n.completed)*100
  ae.rate.14km = (n.economy.14km/n.completed)*100
  recruitment.rate = (n.nonrandomized/n.applied)*100
  elig.rate = (1-(n.ineligible)/n.applied)*100
  
  # Return the following values if have selected the Participant Flow data  

  if(give == "participant.flow"){
  
  return(list(
    "Assessed for eligibility" = n.applied,
    "Excluded" = n.applied-n.nonrandomized,
    "Not meeting inclusion criteria" = n.ineligible,
    "Declined to participate" = n.declined,
    "Nonrandomized" = n.nonrandomized,
    "Allocated to intervention" = n.nonrandomized,
    "Did not receive allocated intervention" = n.nonrandomized-n.nonrandomized,
    "Completed intervention" = n.completed,
    "Discontinued intervention" = n.dropout,
    "Reasons for discontinuation" = reason.dropout,
    "N included in analysis (questionnaire)" = n.quest,
    "N excluded (questionnaire)" = n.completed-n.quest,
    "N included in analysis (vo2max)" = n.vo2max,
    "N excluded (vo2max)" = n.completed-n.vo2max,
    "N included in analysis (economy)" = n.economy.12km,
    "N excluded (economy)" = n.completed-n.economy.12km,
    "N excluded (economy) reason" = paste(n.completed-n.economy.12km,
                                          "did not attain a steady-state velocity across all of 6, 7, 10, and 12 km/h")
  ))
  }

  # Return the following values if have selected the Feasibility rates data   
  
  if(give == "feas.rates"){
    
    return(list(
    "Retention Rate" = cbind(n.nonrandomized,n.completed,retention.rate),
    "Dropout Rate" = cbind(n.nonrandomized,n.dropout,dropout.rate),
    "Questionnaire response rate" = cbind(n.quest,response.rate.quest),
    "VO2max response rate" = cbind(n.vo2max,response.rate.vo2max),
    "Aerobic Economy rate: 6 km/h" = cbind(n.economy.6km,ae.rate.6km),
    "Aerobic Economy rate: 8 km/h"  = cbind(n.economy.8km,ae.rate.8km),
    "Aerobic Economy rate: 10 km/h"  = cbind(n.economy.10km,ae.rate.10km),
    "Aerobic Economy rate: 12 km/h"  = cbind(n.economy.12km,ae.rate.12km),
    "Aerobic Economy rate: 14 km/h"  = cbind(n.economy.14km,ae.rate.14km),
    "Recruitment Rate" = cbind(n.applied,n.nonrandomized,recruitment.rate),
    "Eligibility inclusion rate" = cbind(n.applied,n.eligible,elig.rate),
    "Primary reasons for ineligibility" = reason.ineligible[1:3,]
    ))
  }
  
}

#-------------------------------------------------------------------------------

critical.t <- function(n,
                       type = c("dz","ds"),
                       alpha = .05, 
                       twotailed = TRUE){

################################################################################
# This function computes critical t and d values for a paired sample (dz) or
# unpaired two-sample (ds) t test. Also returns confidence limits on critical d
# and the margin-of-error (MoE) on that d value.
#
# Example use:
# critical.t(n = 17, type = "dz", alpha = 0.05, twotailed = TRUE)
################################################################################
  
  # Define parameters based on function input
  
  if(twotailed==TRUE){tail=2}
  if(twotailed!=TRUE){tail=1}
  if(type == "dz"){df = n-1}
  if(type == "ds"){df = n-2}
  
  # Compute the critical t value
  
  t.crit <- qt(1-(alpha/tail), df)
  
  # Compute the critical d value for a paired design and MoE
  
  # ci.sm is part of the MBESS package:
  # Kelley, K. (2018). MBESS: The MBESS R Package. R package version 4.4.3. 
  # Retrieved from https://CRAN.R-project.org/package=MBESS
  
  if(type == "dz"){
    d.crit = t.crit/sqrt(n)
    d.ci = quiet(ci.sm(sm = d.crit, N = n))
    d.ci.LL = d.ci$Lower.Conf.Limit.Standardized.Mean
    d.ci.UL = d.ci$Upper.Conf.Limit.Standardized.Mean
    MoE = (d.ci.UL-d.ci.LL)/2
    res = sprintf("t(%.f) = %.2f, p = %.3f, d = %.2f, 95%% CI [%.2f, %.2f]",
                  df,
                  t.crit,
                  alpha,
                  d.crit,
                  d.ci.LL,
                  d.ci.UL)
    
  }
  
  # Compute the critical d value for a two sample design and MoE
  
  # ci.smd is part of the MBESS package
  # Kelley, K. (2018). MBESS: The MBESS R Package. R package version 4.4.3. 
  # Retrieved from https://CRAN.R-project.org/package=MBESS
  
  if(type == "ds"){
    d.crit = (2*t.crit)/sqrt(df)
    d.ci = quiet(ci.smd(smd = d.crit, n.1 = n/2, n.2 = n/2))
    d.ci.LL = d.ci$Lower.Conf.Limit.smd
    d.ci.UL = d.ci$Upper.Conf.Limit.smd
    MoE = (d.ci.UL-d.ci.LL)/2
    res = sprintf("t(%.f) = %.2f, p = %.3f, d = %.2f, 95%% CI [%.2f, %.2f]",
                  df,
                  t.crit,
                  alpha,
                  d.crit,
                  d.ci.LL,
                  d.ci.UL)
    }

  return(list(t.crit = t.crit, 
              d.crit = d.crit,
              d.ci.LL = round(d.ci.LL,2),
              d.ci.UL = round(d.ci.UL,2),
              MoE = MoE,
              res = res))
}

#-------------------------------------------------------------------------------

################################################################################
# Defines a vector of baseline demographic variable names
################################################################################

bl = cbind("age.0",
          "male.0",
          "caucasian.0",
          "working.0",
          "bmi.0",
          "pss.0",
          "vo2max.0")

#-------------------------------------------------------------------------------

descrip <- function(dv, df, round = FALSE){
  
################################################################################
# This function returns descriptives
#
# Example use:
# descrip(dv = "pss.1", df = "dat.wide", round = FALSE)
################################################################################

  # Define baseline variable of interest
  
  x <- eval(parse(text = paste(df,"$",dv, sep ="")))
  
  # Compute descriptives and return them
  
  mu = mean(x)
  sdev = sd(x)
  min = min(x)
  max = max(x)
  
  res = data.frame(mu = mu,
                   sdev = sdev,
                   min = min,
                   max = max)
  
  if(round == TRUE){res = round(res,2)}
  
  return(res)

}

#-------------------------------------------------------------------------------

descrip.res <- function(dv.list, df, round = FALSE, sav2csv = TRUE, name){
  
################################################################################
# This function returns all relevant descriptives defined in dv.list and saves 
# them to a csv file
#
# Example use:
# descrip.res(dv.list = dos.vars, df = "dat.wide", round = TRUE, sav2csv = TRUE, name = "baseline")
################################################################################
  
  # Apply the descrip function to all variables listed in dv.list
  
  res = sapply(dv.list, function(i){
    descrip(i, df, round)
  })
  res = as.data.frame(t(res))
  
  # Clean up the results so they are presented nicely
  
  temp1 = sprintf("%.2f (%.2f)",res$mu, res$sdev)
  temp2 = sprintf("%.2f - %.2f",res$min, res$max)
  temp3 = cbind(temp1,temp2)
  colnames(temp3) = c("M (SD)", "Range")
  rownames(temp3) = dv.list
  clean.res = as.data.frame(temp3)  
  
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
  
  if(sav2csv == TRUE){
    
    temp = clean.res
    dv = rownames(temp)
    rownames(temp) = NULL
    temp = cbind(dv, temp)
    name = paste0(name, "_descrip.csv")
    
    write.table(temp,
                file = here("results","descriptives",name),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
    
  }
  
  res = as.data.frame(clean.res)
  
  return(res)
}
#-------------------------------------------------------------------------------

descrip.by <- function(dv, by = "pss.ch", df, dichot = NULL, count = FALSE){

################################################################################
# This function computes baseline descriptive relative to a 'by' variable. It
# returns summary statistics for the full sample, the sample reporting "<0" on 
# the 'by' variable, and the sampe reporting ">0" on the by variable.
#
# Example use:
# descrip.by(dv = "pss.0", by = "pss.ch", df = "dat.wide")
################################################################################
  
  # Define variables
  
  all.0 = eval(parse(text = paste(df,"$",dv, sep ="")))
  by.var = eval(parse(text = paste(df,"$",by, sep ="")))
  
  # Define a dataframe and flag those less than/equal or greater than zero on 
  # the 'by' variable
  
  dat <- as.data.frame(cbind(all.0,by.var))
  flag = dat$by.var < 0
  less.0 = dat$all.0[flag]
  greq.0 = dat$all.0[!flag]
  
  # Define a function to return relevant values
  
  return.these <- function(i){
    n = length(i)
    
    if(!is.null(dichot)){
      n = sum(i == dichot)
    }
    
    pcnt = (n/length(all.0))*100
    
    mu = mean(i)
    sdev = sd(i)
    min = min(i)
    max = max(i)

    
    return(list(n = n, pcnt = pcnt, mu = mu, sdev = sdev, min = min, max = max))
  }
  
  # Apply this function to the full sample, the sample reporting less than/equal
  # or greater than zero on the  'by' variable
  
  all = return.these(all.0)
  less = return.these(less.0)
  greq = return.these(greq.0)
  
  res = as.data.frame(t(cbind(all,less,greq)))
  
  # Return a dataframe depending on whether dichot is defined
  
  if(is.null(dichot)){
    temp1 = sprintf("%.2f (%.2f)",res$mu, res$sdev)
    temp2 = rbind(temp1)
    colnames(temp2) = cbind("All","by declined","by increased/no change")
    rownames(temp2) = dv
    clean.res = as.data.frame(temp2)  
  }
  
  if(!is.null(dichot) || count == TRUE){
  temp1 = sprintf("%s (%.2f%%)",res$n, res$pcnt)
  temp2 = rbind(temp1)
  colnames(temp2) = cbind("All","by declined","by increased/no change")
  rownames(temp2) = dv
  clean.res = as.data.frame(temp2)
  }

  return(res = list(t(res), clean.res = clean.res))
  
}

#-------------------------------------------------------------------------------

bl.table <- function(sav2csv = TRUE){
  
################################################################################
# This function returns Table 1 using the descrip.by function and saves results
# to a csv file.
# 
# Example use:
# bl.table()
################################################################################  
  
  res = 
  rbind(
  descrip.by(dv = "pss.ch", by = "pss.ch", df = "dat.wide", count = TRUE)$clean.res,
  descrip.by(dv = "pss.0", by = "pss.ch", df = "dat.wide")$clean.res,
  descrip.by(dv = "age.0", by = "pss.ch", df = "dat.wide")$clean.res,
  descrip.by(dv = "male.0", by = "pss.ch", df = "dat.wide", dichot = 1)$clean.res,
  descrip.by(dv = "male.0", by = "pss.ch", df = "dat.wide", dichot = 0)$clean.res,
  descrip.by(dv = "caucasian.0", by = "pss.ch", df = "dat.wide", dichot = 1)$clean.res,
  descrip.by(dv = "caucasian.0", by = "pss.ch", df = "dat.wide", dichot = 0)$clean.res,
  descrip.by(dv = "working.0", by = "pss.ch", df = "dat.wide", dichot = 1)$clean.res,
  descrip.by(dv = "vo2max.0", by = "pss.ch", df = "dat.wide")$clean.res,
  descrip.by(dv = "bmi.0", by = "pss.ch", df = "dat.wide")$clean.res
)
  
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
  
  if(sav2csv == TRUE){
    
    write.table(res,
                file = here("results","descriptives","table1.csv"),
                append = F,
                row.names = T,
                col.names = T,
                sep=",")

  }
  
  return(res)
  
  }

#-------------------------------------------------------------------------------

frq <- function(dv, df){

################################################################################
# This function returns frequencies for a variable
# 
# Example use:
# frq("pss.1", "dat.wide")
################################################################################  

  # Define the variable
  
  x <- eval(parse(text = paste(df,"$",dv, sep ="")))
  
  # Compute frequencies
  
  # freq() is part of the userfriendlyscience package:
  # Peters, G. Y. (2018). userfriendlyscience: Quantitative analysis made accessible. R package
  # version 0.7.2. Retrieved from https://CRAN.R-project.org/package=userfriendlyscience
  
  fres = freq(x)
  fres = cbind(as.numeric(fres$intermediate$categoryNames),
               as.numeric(fres$intermediate$frequencies.raw),
               as.numeric(round(fres$intermediate$frequencies.prop*100,2)))
  row.names(fres) = NULL
  colnames(fres) = c("value","freq","pcnt")
  
  fres = cbind("dv" = rep(dv, nrow(fres)),fres)
  fres = as.data.frame(fres)

  return(fres)
  
}

#-------------------------------------------------------------------------------

frq.res <- function(dv.list, df, sav2csv = TRUE, name){
  
################################################################################
# This function returns relevant baseline frequencies for a list of variables
# using the frq() function and saves these results to a csv file
#
# Example use:
# frq.res(dv.list = dos.vars, df = "dat.wide", sav2csv = TRUE, name = dosage)
################################################################################
  
  res = lapply(dv.list, function(i){
    frq(i, df)
  })

  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
 
  if(sav2csv == TRUE){
  
    name = paste0(name, "_freq.csv")
    
    cols = cbind("dv","value","freq","pcnt")
    
    write.table(cols,
                file = here("results","descriptives",name),
                append = F,
                row.names = F,
                col.names = F,
                sep=",")
    
    for(i in 1:length(res)){
    
    write.table(res[i],
                file = here("results","descriptives",name),
                append = T,
                row.names = F,
                col.names = F,
                sep=",")
    
    }
      
  }
  
  return(res)
}

#-------------------------------------------------------------------------------

################################################################################
# Defines a vector of variable names for all 1-df effects
################################################################################

vars = cbind("pss",
            "sdass",
            "adass",
            "ddass",
            "who",
            "maas",
            "erq.cr",
            "rrs.br",
            "pswq",
            "vo2max")

#-------------------------------------------------------------------------------
  
pair.test <- function(x, df.wide, df.long, plotit = FALSE){

################################################################################
# This function computes 1-df effects. It requires you to specify the name
# of a 1-df effect, and where this variable is in a wide and long dataframe. 
# It will compute all descriptive statistics, perform a t-test of T1-T0 gain
# scores, compute CIs from 75% to 95%, and compute different variants of 
# Cohen's d. It will also generate a plot of the T1 and T0 scores, and the T1-T0
# difference scores.
#
# Example use:
# pair.test(x = "pss",df.wide = "dat.wide", df.long = "dat.long")
################################################################################  
    
  # Define variables and df
  
  t0 <- eval(parse(text = paste(df.wide,"$",x,".0", sep ="")))
  t1 <- eval(parse(text = paste(df.wide,"$",x,".1",sep = "")))
  df.long <- eval(parse(text = paste(df.long)))
  
  # Compute descriptives
  
  # outpro is part of the Rallfun-v32 package
  # Wilcox, R. R. (2018). Rallfun-v35. Retrieved from https://dornsife.usc.edu/labs/rwilcox/software/
  
  mu.t0 = mean(t0)
  sd.t0 = sd(t0)
  t0.ci.LL = t.test(t0, conf.level = 0.95)$conf.int[1]
  t0.ci.UL = t.test(t0, conf.level = 0.95)$conf.int[2]
  
  mu.t1 = mean(t1)
  sd.t1 = sd(t1)
  t1.ci.LL = t.test(t1, conf.level = 0.95)$conf.int[1]
  t1.ci.UL = t.test(t1, conf.level = 0.95)$conf.int[2]
  
  ch = t1-t0
  n = length(ch)
  r <- cor(t1, t0, method = "pearson")
  mu = mean(ch)
  sd.ch <- sd(ch)
  sd.av = sqrt((sd(t1)^2+sd(t0)^2)/2)
  min = min(ch)
  max = max(ch)
  norm = shapiro.test(ch)$p.value
  out = outpro(ch, plotit = FALSE)
  
  # Compute t test
  
  # Main results
  test = t.test(ch, mu = 0)
  tval = as.numeric(test$statistic)
  df= as.numeric(test$parameter)
  pval = as.numeric(test$p.value)
  
  # Compute multiple CIs on means
  
  test.ci <- sapply(seq(0.75,0.95,0.05), function(i){
    test = t.test(ch, mu = 0, conf.level = i)
    ci.LL = test$conf.int[1]
    ci.UL = test$conf.int[2]
    c("ci.LL" = ci.LL,
      "ci.UL" = ci.UL
      )
  })
  
  test.ci = as.data.frame(t(test.ci))
  test.ci = cbind(seq(0.75,0.95,0.05),test.ci)
  colnames(test.ci) = c("conf.lvl","ci.LL","ci.UL")
  
  # Compute different effect sizes
  
  dz = mu/sd.ch
  CL = 1 - pnorm(mu/sd.ch) # Chance that a person picked at random from t0 will have a higher score than a person picked at random from t1
  d.av = mu/sd.av
  
  # Compute multiple CIs on d.av
  
  # conf.limits.nct is part of the MBESS package
  # Kelley, K. (2018). MBESS: The MBESS R Package. R package version 4.4.3. 
  # Retrieved from https://CRAN.R-project.org/package=MBESS
  
  d.ci <- sapply(seq(0.75,0.95,0.05), function(i){
    limits <- conf.limits.nct(df = df, t.value = test$statistic, conf.level = i)
    d.av.LL <- (limits$Lower.Limit*sd.ch)/(sd.av*sqrt(n))
    d.av.UL <- (limits$Upper.Limit*sd.ch)/(sd.av*sqrt(n))
    c("d.av.LL" = d.av.LL,
      "d.av.UL" = d.av.UL
    )
  })
  
  d.test.ci = as.data.frame(t(d.ci))
  d.test.ci = cbind(seq(0.75,0.95,0.05),d.test.ci)
  colnames(d.test.ci) = c("conf.lvl","ci.LL","ci.UL")
  
  # Perform a robust test of the 20% trimmed mean differences as a sensitivity 
  # test
  
  # trimpb is part of the Rallfun-v32 package
  # Wilcox, R. R. (2018). Rallfun-v35. Retrieved from https://dornsife.usc.edu/labs/rwilcox/software/
  
  test.rob = trimpb(x = t1-t0, tr = 0.2, nboot = 2000, pr = FALSE)
  mu.rob = test.rob$estimate
  pval.rob = test.rob$p.value
  ci.LL.rob = test.rob$ci[1]
  ci.UL.rob = test.rob$ci[2]
  
  # Generate a plots
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
  
  if(plotit == TRUE){
  
  mu.t0.text = sprintf("T0: M = %.2f, 95%% CI [%.2f, %.2f]", mu.t0, t0.ci.LL, t0.ci.UL)
  mu.t1.text = sprintf("T1: M = %.2f, 95%% CI [%.2f, %.2f]", mu.t1, t1.ci.LL, t1.ci.UL)
    
  # Generate a plot of the T1 and T0 scores
  
  pair.plot =

    suppressWarnings(
    
    ggplot() +
      geom_violin(dat = df.long, aes(x = factor(time), y = df.long[,x])) +
      geom_point(dat = df.long, aes(x = factor(time), y = df.long[,x], group = id), shape = 1) +
      geom_line(dat = df.long, aes(x = factor(time), y = df.long[,x], group = id), linetype = 2, size = 0.5) +
      geom_point(aes(x = factor(c(0,1)), y = c(mu.t0,mu.t1)), size = 2) +
      geom_line(aes(x = factor(c(0,1)), y = c(mu.t0,mu.t1), group =1), size = 1) +
      geom_errorbar(aes(x = factor(0), y = mu.t0), ymin = t0.ci.LL, ymax = t0.ci.UL, width = 0.1, size = 1) +
      geom_errorbar(aes(x = factor(1), y = mu.t1), ymin = t1.ci.LL, ymax = t1.ci.UL, width = 0.1, size = 1) +
      xlab("Time") + ylab("DV") + scale_x_discrete(labels=c("T1","T0"), limits = rev(levels(factor(c(0,1)))))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), name = paste(x, "Scores")) +
      geom_label(aes(x = factor(0), y = mu.t0), label = mu.t0.text, vjust = -1.2, size = 4.4) +
      geom_label(aes(x = factor(1), y = mu.t1), label = mu.t1.text, vjust = 2, size = 4.4) +
      theme_light() + 
      rotate()
      
    )
  
  print(pair.plot) 
  # Save plot as eps file
  ggsave(here("plots","pair_plots", paste(x,"_prepost.eps", sep = "")), plot = pair.plot, width = 7, height = 6, units = "in")
  ggsave(here("plots","pair_plots", paste(x,"_prepost.png", sep = "")), plot = pair.plot, width = 7, height = 6, units = "in")
  
  mu.ch.text = sprintf("Gain: M = %.2f, 95%% CI [%.2f, %.2f]", mu, test.ci$ci.LL[5], test.ci$ci.UL[5])
  
  # Generate a plot of the T1-T0 gain scores
  
  diff.plot =
    
    suppressWarnings(
      
    ggplot() +
      geom_violin(aes(x = "", y = ch), alpha = .8) +
      geom_point(aes(x = "", y = ch), shape = 1, position = position_jitter(width = 0.1,height = 0)) +
      geom_point(aes(x = "", y = mu)) +
    
      mapply(function(i,j){
      geom_errorbar(aes(x = "", y = mu), ymin = test.ci[i,]$ci.LL, ymax = test.ci[i,]$ci.UL, width = j, size = 1)
      }, 2:5, seq(0.05,0.20,0.05)) +
      
      geom_hline(yintercept = 0, linetype = 2) +
      theme_light() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), name = paste(x,"Gain Scores")) +
      geom_label(aes(x = "", y = mu), label = mu.ch.text, vjust = -2.1, size = 4.4) +
      rotate() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) 
    
)
  
  print(diff.plot)
  ggsave(here("plots","pair_diff_plots", paste0(x,"_change.eps")), plot = diff.plot, width = 6.65, height = 4.5, units = "in")
  ggsave(here("plots","pair_diff_plots", paste0(x,"_change.png")), plot = diff.plot, width = 6.65, height = 4.5, units = "in")
  
  }
  
  # Return all these values
  
  return(list(
    n = n,
    mu.t0 = mu.t0,
    sd.t0 = sd.t0,
    mu.t1 = mu.t1,
    sd.t1 = sd.t1,
    mu.ch = mu,
    sd.ch = sd.ch, 
    sd.av = sd.av,
    mu.ch.0.75.ci.LL = as.numeric(test.ci[1,][2]),
    mu.ch.0.75.ci.UL = as.numeric(test.ci[1,][3]),
    mu.ch.0.80.ci.LL = as.numeric(test.ci[2,][2]),
    mu.ch.0.80.ci.UL = as.numeric(test.ci[2,][3]),
    mu.ch.0.85.ci.LL = as.numeric(test.ci[3,][2]),
    mu.ch.0.85.ci.UL = as.numeric(test.ci[3,][3]),
    mu.ch.0.90.ci.LL = as.numeric(test.ci[4,][2]),
    mu.ch.0.90.ci.UL = as.numeric(test.ci[4,][3]),
    mu.ch.0.95.ci.LL = as.numeric(test.ci[5,][2]),
    mu.ch.0.95.ci.UL = as.numeric(test.ci[5,][3]),
    min = min,
    max = max,
    norm = norm,
    out = out$n.out,
    tval = tval,
    df = df,
    pval = pval,
    CL = CL,
    dz = dz,
    d.av = d.av,
    d.av.ch.0.75.ci.LL = as.numeric(d.test.ci[1,][2]),
    d.av.ch.0.75.ci.UL = as.numeric(d.test.ci[1,][3]),
    d.av.ch.0.80.ci.LL = as.numeric(d.test.ci[2,][2]),
    d.av.ch.0.80.ci.UL = as.numeric(d.test.ci[2,][3]),
    d.av.ch.0.85.ci.LL = as.numeric(d.test.ci[3,][2]),
    d.av.ch.0.85.ci.UL = as.numeric(d.test.ci[3,][3]),
    d.av.ch.0.90.ci.LL = as.numeric(d.test.ci[4,][2]),
    d.av.ch.0.90.ci.UL = as.numeric(d.test.ci[4,][3]),
    d.av.ch.0.95.ci.LL = as.numeric(d.test.ci[5,][2]),
    d.av.ch.0.95.ci.UL = as.numeric(d.test.ci[5,][3]),
    mu.rob = mu.rob,
    pval.rob = pval.rob,
    ci.LL.rob = ci.LL.rob,
    ci.UL.rob = ci.UL.rob))
  
}

#-------------------------------------------------------------------------------

pair.test.sensitivity <- function(x, dropout.n, df.wide){
  
################################################################################
# Sensitivity test assuming dropout.n gain scores = 0
#
# Example use:
# pair.test.sensitivity("pss", dropout.n = 7, df.wide = "dat.wide")
################################################################################
  
  # Store vectors and compute gain scores
  
  t0 <- eval(parse(text = paste(df.wide,"$",x,".0", sep ="")))
  t1 <- eval(parse(text = paste(df.wide,"$",x,".1",sep = "")))
  ch = t1-t0
  
  # Add zeros for number of dropouts assuming they would have had no change
  
  ch2 = c(ch, rep(0,dropout.n))

  # Compute descriptives
  
  n = length(ch2)
  mu = mean(ch2)
  sd.ch <- sd(ch2)
  min = min(ch2)
  max = max(ch2)
  
  # t test
  
  # Main results
  test = t.test(ch2, mu = 0)
  tval = as.numeric(test$statistic)
  df= as.numeric(test$parameter)
  pval = as.numeric(test$p.value)
  ci.LL = test$conf.int[1]
  ci.UL = test$conf.int[2]
  
  # Robust test
  
  # trimpb is part of the Rallfun-v32 package
  # Wilcox, R. R. (2018). Rallfun-v35. Retrieved from https://dornsife.usc.edu/labs/rwilcox/software/
  
  test.rob = trimpb(x = ch2, tr = 0.2, nboot = 2000, pr = FALSE)
  mu.rob = test.rob$estimate
  pval.rob = test.rob$p.value
  ci.LL.rob = test.rob$ci[1]
  ci.UL.rob = test.rob$ci[2]
  
  res = 
  sprintf("M (SD) = %.2f (%.2f), 95%% CI [%.2f, %.2f], p = %.3f, pR = %.3f",
          mu, sd.ch, ci.LL, ci.UL, pval, pval.rob)
  
  # Return these values
  
  return(list(
    n = n,
    mu = mu,
    sd.ch = sd.ch,
    min = min,
    max = max,
    tval = tval,
    df = df,
    pval = pval,
    ci.LL = ci.LL,
    ci.UL = ci.UL,
    mu.rob = mu.rob,
    pval.rob = pval.rob, 
    res = res))
  
}

#-------------------------------------------------------------------------------

pair.res <- function(vars, df.wide, df.long, plotit = FALSE, sav2csv = TRUE){
  
################################################################################
# Compute all 1-df tests and store results in a matrix. Generate all plots and
# save results to a csv file. 
#
# Example use:
# pair.res(vars, "dat.wide", "dat.long", plotit = TRUE, sav2csv = TRUE)
################################################################################
  
  res <- sapply(vars, function(i){
    pair.test(i, df.wide, df.long, plotit)
  })
  
  # Clean the main results data matrix for export
  
  res = t(res)
  dv = as.matrix(row.names(res))
  res = cbind(dv,res)
  
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
  
  if(sav2csv == TRUE){
    
    write.table(res,
                file = here("results","paired_data","paired_res.csv"),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
  }
  
  return(res[,-1])
}

#-------------------------------------------------------------------------------

pair.table <- function(sav2csv = TRUE){
 
################################################################################
# Reproduce Table 3
# 
# Example use:
# pair.table()
################################################################################
  
  res = 
  pair.res(vars = vars, df.wide = "dat.wide", df.long = "dat.long", plotit = FALSE, sav2csv = FALSE)
  
  res = as.data.frame(res)
  
  col1 = sprintf("%.2f (%.2f)",res$mu.t0, res$sd.t0)
  col2 = sprintf("%.2f (%.2f)",res$mu.t1, res$sd.t1)
  col3 = sprintf("%.2f (%.2f) [%.2f, %.2f]",
                 res$mu.ch, 
                 res$sd.ch, 
                 res$mu.ch.0.95.ci.LL,
                 res$mu.ch.0.95.ci.UL)
  col4 = sprintf("%.2f [%.2f, %.2f]", 
                 res$d.av, 
                 res$d.av.ch.0.95.ci.LL, 
                 res$d.av.ch.0.95.ci.UL)
  col5.temp = sprintf("%.3f", res$pval)
  col5 = substring(col5.temp, 2)
  flag = col5 == ".000"
  col5[flag] = "<.001"
  col6.temp = sprintf("%.3f", res$pval.rob)
  col6 = substring(col6.temp, 2)
  flag = col6 == ".000"
  col6[flag] = "<.001"
  
  temp1 = cbind(col1,col2,col3,col4,col5,col6)
  colnames(temp1) = c("T0: M (SD)",	"T1: M (SD)",	"Gain: M (SD) [95% CI]",	"dav [95% CI]",	"p",	"pR")
  rownames(temp1) = vars
  clean.res = as.data.frame(temp1)
 
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here 
  
  if(sav2csv == TRUE){
   
    dv = rownames(clean.res)
    temp = cbind(dv, clean.res)
    rownames(temp) = NULL
    
      write.table(temp,
                  file = here("results","paired_data","table3.csv"),
                  append = F,
                  row.names = F,
                  col.names = T,
                  sep=",")
      
  }
  
  return(clean.res)
  
}
  
#-------------------------------------------------------------------------------

pair.supp <- function(sav2csv = TRUE){
 
################################################################################
# Reproduce Supplementary Results Table 1
# 
# Example use:
# pair.supp()
################################################################################
  
  res = 
    pair.res(vars = vars, df.wide = "dat.wide", df.long = "dat.long", plotit = FALSE, sav2csv = FALSE)
  
  res = as.data.frame(res)
  
  res2 =
    lapply(X = vars, FUN = function(i){

  col1 = rbind(
    sprintf("%.2f",res$mu.ch[i]),
    sprintf("%.2f",res$d.av[i])
  )
  col2 = rbind(
    sprintf("[%.2f, %.2f]",res$mu.ch.0.80.ci.LL[i], res$mu.ch.0.80.ci.UL[i]),
    sprintf("[%.2f, %.2f]",res$d.av.ch.0.80.ci.LL[i], res$d.av.ch.0.80.ci.UL[i])
  )
  col3 = rbind(
    sprintf("[%.2f, %.2f]",res$mu.ch.0.85.ci.LL[i], res$mu.ch.0.85.ci.UL[i]),
    sprintf("[%.2f, %.2f]",res$d.av.ch.0.85.ci.LL[i], res$d.av.ch.0.85.ci.UL[i])
  )
  col4 = rbind(
    sprintf("[%.2f, %.2f]",res$mu.ch.0.90.ci.LL[i], res$mu.ch.0.90.ci.UL[i]),
    sprintf("[%.2f, %.2f]",res$d.av.ch.0.90.ci.LL[i], res$d.av.ch.0.90.ci.UL[i])
  )
  col5 = rbind(
    sprintf("[%.2f, %.2f]",res$mu.ch.0.95.ci.LL[i], res$mu.ch.0.95.ci.UL[i]),
    sprintf("[%.2f, %.2f]",res$d.av.ch.0.95.ci.LL[i], res$d.av.ch.0.95.ci.UL[i])
  )
  p = sprintf("%.3f",res$pval[i])
  p = substring(p,2)
  flag = p == ".000"
  p[flag] = "<.001"
  col6 = rbind(p,"")
  
  temp = cbind(col1, col2, col3, col4, col5, col6)
  
  colnames(temp) = c("Gain","80% CI",	"85% CI",	"90% CI",	"95% CI","p")
  rownames(temp) = c(paste0(i,"_M"), paste0(i,"_dav"))
  clean.res = as.data.frame(temp)
  clean.res
  
    })
  
  final = 
  rbind(as.data.frame(res2[1]),
        as.data.frame(res2[2]),
        as.data.frame(res2[3]),
        as.data.frame(res2[4]),
        as.data.frame(res2[5]),
        as.data.frame(res2[6]),
        as.data.frame(res2[7]),
        as.data.frame(res2[8]),
        as.data.frame(res2[9]),
        as.data.frame(res2[10]))
 
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
   
  if(sav2csv == TRUE){
    
    dv = rownames(final)
    temp = cbind(dv, final)
    rownames(temp) = NULL
    
    write.table(temp,
                file = here("results","paired_data","pair_supp.csv"),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
    
  }
  
  return(final)
  
}

#-------------------------------------------------------------------------------

################################################################################
# Defines a matrix of aerobic economy variable names
################################################################################

econ = cbind("vo2.dv",
             "pcnt.vo2.dv",
             "hr.dv",
             "rpe.dv")

#-------------------------------------------------------------------------------

mixed.mod <- function(econ, covar = NULL, df, transf = c("none","log","sqrt","exp"), return.dat = FALSE){

################################################################################
# This function computes LMM for each aerobic economy variable using lmer
#
# Example use:
# mixed.mod(econ = "vo2.dv",df = "dat.econ.long",transf = "none")
################################################################################
    
  # Define dataframe with dv = aerobic economy variable of interest

  dat <- eval(parse(text = paste(df)))
  dat$dv <- dat[,econ]
  if(!is.null(covar) == TRUE){dat$covar <- dat[,covar]}
  
  # Set Type III SS before running lmer
  options(contrasts=c("contr.sum","contr.poly"))
  
  # Run the following model if no covariate is specified
  
  # lmer() is part of the lmerTest Package
  # Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). 
  # lmerTest Package: Tests in Linear Mixed Effects Models. Journal of Statistical 
  # Software, 82(13), 1-26. doi: 10.18637/jss.v082.i13
  
  if(is.null(covar) == TRUE){
    
  if(transf == "none"){model <- lmer(dv ~ time*samp + (1|id), data = dat)}
  if(transf == "log"){model <- lmer(log(dv) ~ time*samp + (1|id), data = dat)}
  if(transf == "exp"){model <- lmer(exp(dv) ~ time*samp + (1|id), data = dat)}
  if(transf == "sqrt"){model <- lmer(sqrt(dv) ~ time*samp + (1|id), data = dat)}
  
  }
    
  # Run the following model if a covariate is specified
  
  if(!is.null(covar) == TRUE){
    
    if(transf == "none"){model <- lmer(dv ~ time*samp + covar + (1|id), data = dat)}
    if(transf == "log"){model <- lmer(log(dv) ~ time*samp + log(covar) + (1|id), data = dat)}
    if(transf == "exp"){model <- lmer(exp(dv) ~ time*samp + exp(covar) + (1|id), data = dat)}
    if(transf == "sqrt"){model <- lmer(sqrt(dv) ~ time*samp + sqrt(covar) + (1|id), data = dat)}
  
  }
  
  # Return model summary, model anova, model comparison, and model variance
  
  res1 = summary(model)
  res2 = anova(model)
  res3 = ranova(model)
  rfx <- as.data.frame(VarCorr(model))
  
  if(return.dat==TRUE){return(list(model = model, summary = res1, anova = res2, dat = dat))}
  
  return(list(model = model, 
              summary = res1, 
              anova = res2,
              rfx = rfx,
              delete.rand.eff = res3))
}

#-------------------------------------------------------------------------------

mixed.model.res <- function(econ, covar = NULL, df, transf, sav2csv = TRUE){

################################################################################
# This function computes all LMM results and saves results to a csv
#
# Example use:
# mixed.model.res(econ = econ, df = "dat.econ.long", transf = "none", sav2csv = TRUE)
################################################################################  
  
  # Compute all mixed model results and stores in matrix
  
  if(is.null(covar)){
  
  res <- lapply(econ, function(i){
    mod = mixed.mod(econ = i,df = df,transf = transf)
    mod = mod$anova
  })
  
  }
  
  if(!is.null(covar)){
  
  res <- mapply(function(i,j){
    mod = mixed.mod(econ = i, covar = j, df = df,transf = transf)
    mod = mod$anova
  }, econ, covar, SIMPLIFY = FALSE)
  
  }
  
  names(res) = econ
  
  # Saves results to csv file
  
  # here() is part of the here package
  # Kirill Müller (2017). here: A Simpler Way to Find Your Files. R package version 0.1.
  # https://CRAN.R-project.org/package=here
  
  if(sav2csv == TRUE){
    
    
    cols = cbind("dv", "factor","Sum Sq","Mean Sq","NumDF","DenDF","F value","Pr(>F)")
    
    write.table(cols,
                file = here("results","mixed_model_data","mixed_model_res.csv"),
                append = F,
                row.names = F,
                col.names = F,
                sep=",")
    
    for(j in 1:length(res)){
    
    temp = as.data.frame(res[j])
    dv = rep(econ[j], nrow(temp))
    factor = rownames(temp)
    temp = cbind(dv,factor,temp)
      
    write.table(temp,
                file = here("results","mixed_model_data","mixed_model_res.csv"),
                append = T,
                row.names = F,
                col.names = F,
                sep=",")
    
  }
    
  }
  
  return(res)
}

#-------------------------------------------------------------------------------

mixed.assum <- function(econ, covar = NULL, df, transf = c("none","log","sqrt","exp")){
  
################################################################################
# This function generate plots for testing each LMM assumption, and return
# participant-level and dv-level responses that may be outliers as identified
# through Cooks distance
#
# Example use:
# mixed.assum(econ = "vo2.dv",df = "dat.econ.long",transf = "none")
################################################################################  
  
  # Run the mixed.mod function and save the model
  
  model = mixed.mod(econ = econ, covar = covar, df = df,transf = transf, return.dat = TRUE)$model
  
  # Save the dataframe used in the mixed.model function
  
  dat = mixed.mod(econ = econ,covar = covar,df = df,transf = transf, return.dat = TRUE)$dat
  
# Check the assumption of linearity and homoskedasticity

  homosk <- 
    ggplot() +
    geom_point(aes(x = fitted(model),y = residuals(model))) +
    geom_smooth(aes(x = fitted(model),y = residuals(model)), se = F, method = "loess", color ="black")+
    geom_smooth(aes(x = fitted(model),y = residuals(model)), se = T, method = "lm")+
    geom_hline(yintercept = 0, linetype=2)+
    labs(x = "Fitted Values", y = "Residuals")+
    ggtitle(paste0(econ, " (",transf,"-transformation)")) +
    theme_light()
  
  # Save plot as eps file
  sapply(c(".eps",".png"), function(i){
    plot1name = paste0(econ,"_1","_homosk",i)
    ggsave(here("plots", "mixed_model_plots","assump_plots", plot1name), plot = homosk, width = 5, height = 5, units = "in")
  })

  # Check for normality using a QQ Plot
  
  # ggqq() is part of the userfriendlyscience package:
  # Peters, G. Y. (2018). userfriendlyscience: Quantitative analysis made accessible. R package
  # version 0.7.2. Retrieved from https://CRAN.R-project.org/package=userfriendlyscience
  
  ggqq = ggqq(resid(model))
  ggqq= ggqq + ggtitle(paste0(econ, " (",transf,"-transformation)"))
  
  # Save plot as eps file
  sapply(c(".eps",".png"), function(i){
    plot2name = paste0(econ,"_2","_qqplot",i)
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot2name), plot = ggqq, width = 5, height = 5, units = "in")
  })
  
  # Check for for outlier participants using criterion of Cooks Distance > 1
  
  # cooks.distance() function is modified from HLMdiag
  # Loy, A., & Hofmann, H. (2014). HLMdiag: A Suite of Diagnostics for 
  # Hierarchical Linear Models in R. 2014, 56(5), 28. doi: 10.18637/jss.v056.i05
  
  n = length(unique(dat$id))
  n.dv = length(dat$dv)
  cooksd_id <- cooks.distance(model, group = "id")[1:n]
  cooksd_dv <- cooks.distance(model)[1:n.dv]
  omit = 1
  
  vals_id = as.data.frame(cooksd_id)
  id = as.data.frame(unique(dat$id))
  colnames(id) = "id"
  vals_id = cbind(id,vals_id)
  rank = order(vals_id$cooksd_id)
  vals_id$rank[rank] = 1:nrow(vals_id)
  
  vals_dv = data.frame(matrix(nrow=length(cooksd_dv),ncol=0))
  vals_dv$id = dat$id
  vals_dv$time = dat$time
  vals_dv$samp = dat$samp
  vals_dv$dv = dat$dv
  vals_dv$cooksd_dv = cooksd_dv
  rankdv = order(vals_dv$cooksd_dv)
  vals_dv$rankdv[rankdv] = 1:nrow(vals_dv)
  
  cook_id <- 
    ggplot(data = vals_id, aes(x = cooksd_id, y = rank)) +
    geom_point() +
    geom_vline(xintercept = omit, linetype = 2) +
    labs(x = "Cooks Distance for ID", y = "Rank")+
    ggtitle(paste0(econ, " (",transf,"-transformation)"))+
    theme_light()
  
  # Save plot as eps and png files
  
  sapply(c(".eps",".png"), function(i){
    plot3name = paste0(econ,"_3","_cook_id",i)
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot3name), plot = cook_id, width = 5, height = 5, units = "in")
  })
  
  mu.cook_id = mean(vals_id$cooksd_id)
  min.cook_id = min(vals_id$cooksd_id)
  max.cook_id = max(vals_id$cooksd_id)
  
  extrid.1 <- which(vals_id$cooksd_id %in% sort(vals_id$cooksd_id,TRUE)[1])
  extrid.1.pos <- vals_id[extrid.1,]
  
  extrid.2 <- which(vals_id$cooksd_id %in% sort(vals_id$cooksd_id,TRUE)[2])
  extrid.2.pos <- vals_id[extrid.2,]
  
  extrid.3 <- which(vals_id$cooksd_id %in% sort(vals_id$cooksd_id,TRUE)[3])
  extrid.3.pos <- vals_id[extrid.3,]
  
  # Check for for outlier individual datapoints using criterion of Cooks Distance > 1
  
  cook_dv <- 
    ggplot(data = vals_dv, aes(x = vals_dv$cooksd_dv,y=rankdv)) +
    geom_point() +
    geom_vline(xintercept = omit, linetype = 2) +
    labs(x = "Cooks Distance for DV", y = "Rank")+
    ggtitle(paste0(econ, " (",transf,"-transformation)"))+
    theme_light()
  
  # Save plot as eps and png file
  sapply(c(".eps",".png"), function(i){
    plot4name = paste0(econ,"_4","_cook_dv",i)
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot4name), plot = cook_dv, width = 5, height = 5, units = "in")
  })
  
  mu.cook_dv = mean(vals_dv$cooksd_dv)
  min.cook_dv = min(vals_dv$cooksd_dv)
  max.cook_dv = max(vals_dv$cooksd_dv)
  
  extrdv.1 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[1])
  extrdv.1.pos <- vals_dv[extrdv.1,]
  
  extrdv.2 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[2])
  extrdv.2.pos <- vals_dv[extrdv.2,]
  
  extrdv.3 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[3])
  extrdv.3.pos <- vals_dv[extrdv.3,]
  
  return(list(extreme.id.1 = extrid.1.pos,
              extreme.id.2 = extrid.2.pos,
              extreme.id.3 = extrid.3.pos,
              extreme.dv.1 = extrdv.1.pos,
              extreme.dv.2 = extrdv.2.pos,
              extreme.dv.3 = extrdv.3.pos))
}

#-------------------------------------------------------------------------------

mixed.assum.res <- function(econ, covar = NULL, df, transf){

################################################################################
# This function generate all plots for testing LMM assumptions for each aerobic
# economy variable.
#
# Example use:
# mixed.assum.res(econ = econ, df = "dat.econ.long", transf = "none")
################################################################################    
  
  # Computes all mixed model assumption plots
  
  if(is.null(covar)){
  
  res <- sapply(econ, function(i){
    mixed.assum(econ = i, covar = covar, df = df,transf = transf)
  })
  return(res)
  }
  
  if(!is.null(covar)){
    
    res <- mapply(function(i,j){
      mixed.assum(econ = i, covar = j, df = df,transf = transf)
    }, econ, covar)
    return(res)
  }
  
}


#-------------------------------------------------------------------------------

emm.test <- function(econ, 
                     covar = NULL,
                     df, 
                     transf = c("none","log","sqrt","exp"), 
                     effect = c("time","samp","timeXsamp"),
                     plotit = TRUE){

################################################################################
# This function computes emmeans for follow-up to main effects and interactions
# in each LMM for the aerobic economy data
#
# Example use:
# emm.test(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "time")
# emm.test(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "timeXsamp")
################################################################################  
    
  # Save the model from mixed.mod
  
  model = mixed.mod(econ = econ,covar = covar,df = df,transf = transf, return.dat = TRUE)$model
  
  # Save the dataframe from mixed.mod
  
  dat = mixed.mod(econ = econ,covar = covar,df = df,transf = transf, return.dat = TRUE)$dat
  
  # Compute main effect with Satterthwaite df method
  
  # emmeans() is part of the emmeans package:
  # Lenth, R. (2019). emmeans: Estimated Marginal Means, aka Least-Squares Means 
  # [R package version 1.3.2]. Retrieved from https://CRAN.R-project.org/package=emmeans
  
  if(effect != "timeXsamp"){
  
  emm = emmeans(model,effect,lmer.df = "satterthwaite")
  emm.diff = pairs(emm, type="response", reverse=TRUE)

  # Inspect effect at multiple confidence intervals from 0.75 to 0.95
  
  conf.check =
    lapply(seq(0.75,0.95,0.05), function(i){
      res = confint(emm.diff, adjust = "none", level = c(i),reverse=TRUE)
      ci = res[5:6]
      cbind(ci)
    })

  names(conf.check) = paste0("ci.",seq(0.75,0.95,0.05))
  
  conf.check = as.data.frame(conf.check)
  emm.diff = as.data.frame(emm.diff)
  emm.diff = cbind(emm.diff, conf.check)
  
  }
  
  # Compute interaction effect with Satterthwaite df method
  
  emm.int = emmeans(model, pairwise~time | samp)
  emm.int.contrasts = pairs(emm.int$emmeans, reverse=TRUE)

  # Inspect interaction T1-T0 effects at multiple confidence intervals from 0.75 to 0.95
  
  conf.check.int =
    lapply(seq(0.75,0.95,0.05), function(i){
      res = confint(emm.int.contrasts, adjust = "none", level = i,reverse=TRUE)
      ci = res[6:7]
      ci
    })
  
  names(conf.check.int) = paste0("ci.",seq(0.75,0.95,0.05))
  
  conf.check.int= as.data.frame(conf.check.int)
  emm.int.contrasts = as.data.frame(emm.int.contrasts)
  emm.int.contrasts = cbind(emm.int.contrasts,conf.check.int)

  # Plot the results

  if(plotit == TRUE){
  
if(transf == "log"){dat$dv = log(dat$dv)}
e.df = as.data.frame(emm.int)

e.df = e.df[,c("emmeans.time",
              "emmeans.samp",
              "emmeans.emmean",
              "emmeans.lower.CL",
              "emmeans.upper.CL")]

suppressWarnings(
  
emm.plot <- 
ggplot()+
  geom_point(data = dat, aes(x = factor(samp), y = dv, group = factor(time)), position = position_dodge(0.2), shape = 1) +
  geom_point(data = e.df, aes(x = factor(emmeans.samp), y = emmeans.emmean, group = factor(emmeans.time), shape = factor(emmeans.time)), size = 2, position = position_dodge(0.2)) +
  geom_errorbar(data = e.df, aes(x = factor(emmeans.samp), y = emmeans.emmean, group = factor(emmeans.time)), ymin = e.df$emmeans.lower.CL, ymax = e.df$emmeans.upper.CL, position = position_dodge(-0.2),width = 0.1) +
  geom_line(data = e.df, aes(x = factor(emmeans.samp), y = emmeans.emmean, group = factor(emmeans.time)),position = position_dodge(0.2)) +
  scale_shape_discrete(name = "Time", labels = c("T0","T1")) +
  xlab("Velocity (km/h)") + ylab(econ) + scale_x_discrete(labels=c(6,8,10,12)) +
  ggtitle(paste0(econ," (",transf,"-transformation)")) +
  theme_light() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

)

print(emm.plot)
# Save plot as eps file
sapply(c(".eps",".png"), function(i){
  plotname = paste0(econ,"_lmm",i)
  ggsave(here("plots", "mixed_model_plots", "mixed_model_res", plotname), plot = emm.plot, width = 6.3, height = 4.5, units = "in")
})

}

if(effect == "time" || effect == "samp"){return(list(emmeans = as.data.frame(emm),
                                                     contrasts = emm.diff))}

if(effect== "timeXsamp"){return(list(emmeans = as.data.frame(emm.int$emmeans), 
                                     contrasts = emm.int.contrasts))}

}

#-------------------------------------------------------------------------------

emm.test.res <- function(econ, df, transf, effect, plotit = TRUE, sav2csv = TRUE){
  
################################################################################
# This function computes emmeans for follow-up to main effects and interactions
# for all LMM of aerobic economy data
#
# Example use:
# emm.test.res(econ = econ, df = "dat.econ.long", transf = "none", effect = "time", plotit = TRUE)
 ################################################################################   
  
  res <- sapply(econ, function(i){
    test = emm.test(econ = i,df = df,transf = transf, effect = effect, plotit = plotit)
  })
  
  if(sav2csv == TRUE){
  
  # Save emmeans
  
  if(effect != "timeXsamp"){cat = effect}
  if(effect == "timeXsamp"){cat = cbind("time","samp")}
  cols = cbind("dv", cat,"emmean","SE","df","ci.LL","ci.UL")
  name = paste0("emmeans_",effect,"_res.csv")
  
  write.table(cols,
              file = here("results","mixed_model_data",name),
              append = F,
              row.names = F,
              col.names = F,
              sep=",")
    
  for(j in 1:length(econ)){
    
    temp = as.data.frame(res["emmeans",][j])
    dv = rep(econ[j],nrow(temp))
    temp = cbind(dv, temp)
    
    write.table(temp,
                file = here("results","mixed_model_data",name),
                append = T,
                row.names = F,
                col.names = F,
                sep=",")
    
  }
  
  # Save contrasts
  
  est = "estimate"
  if(transf == "log"){est = "ratio"}
  if(effect == "timeXsamp"){contrast = cbind("contrast","samp")}
  
  cols = cbind("dv",
               "effect",
               contrast,
               est,
               "SE",
               "df",
               "tval",
               "pval",
               "0.75.ci.LL",
               "0.75.ci.UL",
               "0.80.ci.LL",
               "0.80.ci.UL",
               "0.85.ci.LL",
               "0.85.ci.UL",
               "0.90.ci.LL",
               "0.90.ci.UL",
               "0.95.ci.LL",
               "0.95.ci.UL",
               "d")
  
  name = paste0("contrasts_",effect,"_res.csv")
  
  write.table(cols,
              file = here("results","mixed_model_data",name),
              append = F,
              row.names = F,
              col.names = F,
              sep=",")
  
  for(j in 1:length(econ)){
    
    temp = as.data.frame(res["contrasts",][j])
    dv = rep(econ[j],nrow(temp))
    temp = cbind(dv, effect, temp)
    
    write.table(temp,
                file = here("results","mixed_model_data",name),
                append = T,
                row.names = F,
                col.names = F,
                sep=",")
    
  }
  
}
    
  
  return(res)
}

#-------------------------------------------------------------------------------

lmm.table <- function(sav2csv = TRUE){
  
################################################################################
# This function reproduces Table 4
#
# Example use:
# lmm.table()
################################################################################ 
  
  # Extract fixed effects
  
  ffx = mixed.model.res(econ = econ, df = "dat.econ.long", transf = "none", sav2csv = TRUE)
  
  ffx.vo2 = as.data.frame(ffx$vo2.dv)
  ffx.pcnt.vo2 = as.data.frame(ffx$pcnt.vo2.dv)
  ffx.hr = as.data.frame(ffx$hr.dv)
  ffx.rpe = as.data.frame(ffx$rpe)
  
  toy <- function(ffx.var, name){
    var = name
    col0 = rownames(ffx.var)
    col1 = sprintf("%.2f", ffx.var$`Sum Sq`)
    col2 = sprintf("%.2f",ffx.var$`Mean Sq`)
    col3 = sprintf("%.0f,%.0f", ffx.var$NumDF, ffx.var$DenDF)
    col3.1 = sprintf("%.2f", ffx.var$`F value`)
    col4.temp = sprintf("%.3f",ffx.var$`Pr(>F)`)
    col4 = substring(col4.temp, 2)
    flag = col4 == ".000"
    col4[flag] = "<.001"
    temp = cbind(var, col0, col1, col2, col3, col3.1, col4)
    colnames(temp) = c("var", "effect","SS", "MS", "df","F/t", "p")
    return(temp)
  }
  
  res = 
  rbind(
    toy(ffx.vo2, "vo2"),
    toy(ffx.pcnt.vo2, "pcnt.vo2"),
    toy(ffx.hr, "hr"),
    toy(ffx.rpe, "rpe")
  )
   
  # Extract emmeans for time (T1-T0)
  
  emm = 
  emm.test.res(econ = econ, df = "dat.econ.long", transf = "none", effect = "time", plotit = FALSE)

  emm.vo2 = as.data.frame(emm[,"vo2.dv"]$contrasts)
  emm.pcnt.vo2 = as.data.frame(emm[,"pcnt.vo2.dv"]$contrasts)
  emm.hr = as.data.frame(emm[,"hr.dv"]$contrasts)
  emm.rpe = as.data.frame(emm[,"rpe.dv"]$contrasts)
      
  toy2 <- function(emm.var, name){
    var = name
    col0 = "T1-T0"
    col1 = sprintf("%.2f [%.2f, %.2f]", emm.var$estimate, emm.var$ci.0.95.lower.CL, emm.var$ci.0.95.upper.CL)
    col2 = sprintf("%.2f",emm.var$SE)
    col3 = sprintf("%.0f", emm.var$df)
    col3.1 = sprintf("%.2f", emm.var$t)
    col4.temp = sprintf("%.3f",emm.var$p.value)
    col4 = substring(col4.temp, 2)
    flag = col4 == ".000"
    col4[flag] = "<.001"
    temp = cbind(var, col0, col1, col2, col3, col3.1, col4)
    colnames(temp) = c("var","contrast", "M [95% CI]","SE", "df","t", "p")
    return(temp)
  }
  
  res2 = 
    rbind(
      toy2(emm.vo2, "vo2"),
      toy2(emm.pcnt.vo2, "pcnt.vo2"),
      toy2(emm.hr, "hr"),
      toy2(emm.rpe, "rpe")
    )
  
  table4 =
  as.data.frame(
  rbind(res[c(1:3),],res2[1,],
        res[c(4:6),],res2[2,],
        res[c(7:9),],res2[3,],
        res[c(10:12),],res2[4,])
  )
  
  if(sav2csv == TRUE){
    
    write.table(table4,
                file = here("results","mixed_model_data","table4.csv"),
                append = F,
                row.names = T,
                col.names = T,
                sep=",")
  }
    
  return(table4)

}


#-------------------------------------------------------------------------------

supp.lmm <- function(sav2csv = TRUE){
  
################################################################################
# This function reproduces follow-up contrasts of each LMM at multiple confidence 
# limits. This is Supplementary Results Table 2
#
# Example use:
# supp.lmm()
################################################################################ 
  
  # Extract emmeans for time (T1-T0)
  
  emm = 
    emm.test.res(econ = econ, df = "dat.econ.long", transf = "none", effect = "time", plotit = FALSE)
  
  emm.vo2 = as.data.frame(emm[,"vo2.dv"]$contrasts)
  emm.pcnt.vo2 = as.data.frame(emm[,"pcnt.vo2.dv"]$contrasts)
  emm.hr = as.data.frame(emm[,"hr.dv"]$contrasts)
  emm.rpe = as.data.frame(emm[,"rpe.dv"]$contrasts)
  
  toy2 <- function(emm.var, name){
    var = name
    col0 = sprintf("%.2f", emm.var$estimate)
    col1 = sprintf("[%.2f, %.2f]", emm.var$ci.0.8.lower.CL, emm.var$ci.0.8.upper.CL)
    col2 = sprintf("[%.2f, %.2f]", emm.var$ci.0.85.lower.CL, emm.var$ci.0.85.upper.CL)
    col3 = sprintf("[%.2f, %.2f]", emm.var$ci.0.9.lower.CL, emm.var$ci.0.9.upper.CL)
    col4 = sprintf("[%.2f, %.2f]", emm.var$ci.0.95.lower.CL, emm.var$ci.0.95.upper.CL)
    p = sprintf("%.3f", emm.var$p.value)
    p = substring(p,2)
    flag = p == ".000"
    p[flag] = "<.001"
    col5 = p
    temp = cbind(name, col0, col1, col2, col3, col4,col5)
    colnames(temp) = c("name","gain", "80% CI",	"85% CI",	"90% CI",	"95% CI",	"p")
    return(temp)
  }
  
  # Extract emmeans for sample at each timepoint for pcnt.vo2 (T1-T0)
  
  emm.int = 
    emm.test.res(econ = "pcnt.vo2.dv", df = "dat.econ.long", transf = "none", effect = "timeXsamp", plotit = FALSE)
  
  pcnt.vo2.int = as.data.frame(emm.int["contrasts",])
  
  toy3 <- function(emm.var.int, name){
    var = name
    col0 = rbind(
      sprintf("%.2f", emm.var.int$estimate[1]),
      sprintf("%.2f", emm.var.int$estimate[2]),
      sprintf("%.2f", emm.var.int$estimate[3]),
      sprintf("%.2f", emm.var.int$estimate[4])
    )
    col1 = rbind(
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.8.lower.CL[1], emm.var.int$ci.0.8.upper.CL[1]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.8.lower.CL[2], emm.var.int$ci.0.8.upper.CL[2]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.8.lower.CL[3], emm.var.int$ci.0.8.upper.CL[3]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.8.lower.CL[4], emm.var.int$ci.0.8.upper.CL[4])
    )
    col2 = rbind(
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.85.lower.CL[1], emm.var.int$ci.0.85.upper.CL[1]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.85.lower.CL[2], emm.var.int$ci.0.85.upper.CL[2]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.85.lower.CL[3], emm.var.int$ci.0.85.upper.CL[3]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.85.lower.CL[4], emm.var.int$ci.0.85.upper.CL[4])
    )
    col3 = rbind(
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.9.lower.CL[1], emm.var.int$ci.0.9.upper.CL[1]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.9.lower.CL[2], emm.var.int$ci.0.9.upper.CL[2]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.9.lower.CL[3], emm.var.int$ci.0.9.upper.CL[3]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.9.lower.CL[4], emm.var.int$ci.0.9.upper.CL[4])
    )
    col4 = rbind(
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.95.lower.CL[1], emm.var.int$ci.0.95.upper.CL[1]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.95.lower.CL[2], emm.var.int$ci.0.95.upper.CL[2]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.95.lower.CL[3], emm.var.int$ci.0.95.upper.CL[3]),
      sprintf("[%.2f, %.2f]", emm.var.int$ci.0.95.lower.CL[4], emm.var.int$ci.0.95.upper.CL[4])
    )
    
    p = sprintf("%.3f", emm.var.int$p.value)
    p = substring(p,2)
    flag = p == ".000"
    p[flag] = "<.001"
    col5 = p
    temp = cbind(name, col0, col1, col2, col3, col4,col5)
    colnames(temp) = c("name","gain", "80% CI",	"85% CI",	"90% CI",	"95% CI",	"p")
    return(temp)
  }
  
  res2 = 
    rbind(
      toy2(emm.vo2, "vo2.time"),
      toy2(emm.pcnt.vo2, "pcnt.vo2.time"),
      toy3(pcnt.vo2.int, "pcnt.vo2.int"),
      toy2(emm.hr, "hr.time"),
      toy2(emm.rpe, "rpe.time")
    )
  
  if(sav2csv == TRUE){
    
    write.table(res2,
                file = here("results","mixed_model_data","supp_lmm.csv"),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
  }
  
  return(as.data.frame(res2))
  
}

#-------------------------------------------------------------------------------

supp.model <- function(ae.var, sav2csv = TRUE){
 
################################################################################
# This function reproduces the full model for each LMM. This is Supplementary
# Results Tables 3-6
#
# Example use:
# supp.model()
################################################################################ 
   
  x = mixed.mod(econ = ae.var,df = "dat.econ.long",transf = "none")
  
  cleanup <- function(ffx.var){
    
    # Random effects model fit
    
    rfx.del = as.data.frame(ffx.var$delete.rand.eff)
    rfx.del$`Pr(>Chisq)`[2] = round(rfx.del$`Pr(>Chisq)`,3)[2]
    rfx.del$`Pr(>Chisq)`[2] = format(rfx.del$`Pr(>Chisq)`,nsmall = 3)[2]
    rfx.del$`Pr(>Chisq)`[2] = substring(rfx.del$`Pr(>Chisq)`[2],2)
    flag = rfx.del$`Pr(>Chisq)`[2] == ".000"
    rfx.del$`Pr(>Chisq)`[2][flag] = "<.001"
    rfx.del[-c(5,6)] = round(rfx.del[-c(5,6)], 2)
    rfx.del = rfx.del[-1]
    rfx.del = rbind(colnames(rfx.del), rfx.del)
    colnames(rfx.del) = NULL
    
    # Summary estimates
    
    f.mod = as.data.frame(ffx.var$summary$coefficients)
    f.mod$`Pr(>|t|)` = round(f.mod$`Pr(>|t|)`,3)
    f.mod$`Pr(>|t|)` = format(f.mod$`Pr(>|t|)`,nsmall = 3)
    f.mod$`Pr(>|t|)` = substring(f.mod$`Pr(>|t|)`,2)
    flag = f.mod$`Pr(>|t|)` == ".000"
    f.mod$`Pr(>|t|)`[flag] = "<.001"
    f.mod[-5] = round(f.mod[-5],2)
    f.mod[-5] = format(f.mod[-5], nsmall = 2)
    f.mod = rbind(colnames(f.mod), f.mod)
    colnames(f.mod) = NULL
    
    # Random effects variance
    
    var = ffx.var$rfx
    col1 = rbind("Variance", 
                 sprintf("%.2f", var$vcov[1]),
                 sprintf("%.2f", var$vcov[2]))
    col2 = rbind("SD",
                 sprintf("%.2f", var$sdcor[1]),
                 sprintf("%.2f", var$sdcor[2]))
    temp = cbind(col1, col2)
    rownames(temp) = c("RFX",
                       "RFX (intercept)",
                       "Residual")
    
    return(list(delete = as.data.frame(rfx.del),
                model = as.data.frame(f.mod),
                variance = as.data.frame(temp)))
  }
  
  res = cleanup(x)
  
  if(sav2csv == TRUE){
    
    write.table(NULL, file = here("results","mixed_model_data",paste0(ae.var,"_supp.csv")))
    
    sapply(names(res), function(i){
      write.table(res[i],
                  file = here("results","mixed_model_data",paste0(ae.var,"_supp.csv")),
                  append = T,
                  row.names = T,
                  col.names = F,
                  sep=",")
    })

    
  }
  
  
  
  return(res)
  
  
}

#-------------------------------------------------------------------------------

safeguard.ES <- function(dv = "pss", df){
  
################################################################################
# This function computes a safeguard effect size for Cohen's dz (Cohen's d 
# computed with mean gain score divided by standard deviation of gain scores).
# It will return the lower limit for an 80%-85% 1-sided CI on dz.
# It will also return the margin-of-error (MoE) suggested to be targeted for
# each dz value, which is just dz/2
#
# Example use:
# safeguard.ES(dv = "pss", df = "dat.wide")
################################################################################ 

  # Define variables

  t0 <- eval(parse(text = paste(df,"$",dv,".0", sep ="")))
  t1 <- eval(parse(text = paste(df,"$",dv,".1",sep = "")))
  ch <- t1-t0

  # Compute descriptives

  mu = mean(ch)
  sdev = sd(ch)
  n = length(ch)
  df = n-1
  dz = mu/sdev
  tval = as.numeric(t.test(ch, mu = 0)$statistic)

  # Compute lower limit 75% to 95% CI on dz

  # conf.limits.nct is part of the MBESS package
  # Kelley, K. (2018). MBESS: The MBESS R Package. R package version 4.4.3. 
  # Retrieved from https://CRAN.R-project.org/package=MBESS
  
  dz.LL =
    sapply(seq(0.80,0.95,0.05), function(j){
      x = 1-(2*j) # compute a 1-sided confidence interval
      limits = conf.limits.nct(df = df, t.value = tval, conf.level = x)
      dz.ci <- c(limits$Lower.Limit/sqrt(n),limits$Upper.Limit/sqrt(n))
      dz.ci.LL <- dz.ci[which.min(abs(dz.ci))]
      dz.ci.LL
    })

  dz.LL = c(dz, dz.LL)
  MoE = abs(dz.LL/2)
  dz.LL = as.data.frame(dz.LL)
  row.names(dz.LL) <- c("est",seq(0.80,0.95,0.05))
  res = cbind(dz.LL, MoE)

  return(res)
  
}

#-------------------------------------------------------------------------------

aipe.assurance <- function(d,
                           w = d/2,
                           reps = 2000,
                           n.seq = c(10,200,1),
                           effect = c("dz", "ds"),
                           plotit = TRUE,
                           sav2csv = TRUE,
                           seed = FALSE){

################################################################################  
# This function computes the sample size required to attain a pre-specified 
# 95% CI half-width (i.e., margin-of-error) for a given d value with 99% 
# assurance. Accommodates dz (paired d) and ds (two sample d). 
#
# w is the margin-of-error. This defaults to d/2 if not specified.
#
# reps specified the number of iterations for each sample size simulation. 
#
# n.seq refers to the sequence of sample sizes to be assessed. Defaults to 
# c(10,200,1), which assesses n = 10 to n = 200 in increments of 1.
#
# seed = TRUE if you want a reproducible result each time you run the function.
#
# Example use:
# aipe.assurance(d = 0.5, reps = 2000, n.seq = c(10,200,1), effect = "dz", seed = TRUE)
################################################################################  
  
  # Exit if effect is not dz or ds
  
  if(effect != "dz" && effect != "ds"){
    return(print("Please specify effect as 'dz' or 'ds'"))
  }
  
  # Exit if n.seq is incorrectly specified
  
  if(length(n.seq) != 3){
    return(print("Please specify n.seq correctly. E.g., c(10,200,1)"))
  }
  
  # Define start, fin, and increm. 
  
  if(n.seq[1] < 5){n.seq[1] = 5}
  start = n.seq[1]
  fin = n.seq[2]
  increm = n.seq[3]
  
  # Define w as a positive number
  
  w = abs(w)
  
  # Set the seed for reproducible analyses
  
  if(seed == TRUE){set.seed(2)}
  
  print(sprintf("Simulating n = %s to n = %s in increments of %s with %s iterations",
                start, 
                fin, 
                increm,
                reps))

    
  # Run this if dz is specified
  
  # pbsapply() is part of the pbapply package:
  # Peter Solymos and Zygmunt Zawadzki (2019). pbapply: Adding Progress Bar
  # to '*apply' Functions. R package version 1.4-0.
  # https://CRAN.R-project.org/package=pbapply
  
    if(effect == "dz"){

    res <- pbsapply(seq(start,fin,increm),function(i){
      
      # The replicate function will replicate an experiment 'reps' times and
      # compute w.obt from each experiment. E.g., if reps = 100, the est 
      # variable will be a vector of 100 values of w.obt from 100 random 
      # experiments. 
      
      est <- replicate(n = reps, expr = {
        y = rnorm(n = i, mean = d, sd = 1)
        dz.ci = quiet(ci.sm(sm = mean(y)/sd(y), N = i))
        w.obt = (dz.ci$Upper.Conf.Limit.Standardized.Mean-
          dz.ci$Lower.Conf.Limit.Standardized.Mean)/2
        w.obt
      })
      
      # Sapply will run the replication by drawing samples starting at n = start 
      # to n = fin, in increments of n = increm. It will compute the long run 
      # probabability across your replicates that w.obt is less than or equal to 
      # a desired w for each sample size. 
      
      # It will combine sample size and probability of est <= w in a matrix and save
      # these in the variable res. This can then be plotted. 
      
      lr.prob <- sum(est <= w)/reps
      lr.prob.res <- cbind(i,lr.prob)
      lr.prob.res
      
    })
    }
    
  # Run this if dz is specified
  
    if(effect == "ds"){
      
      res <- pbsapply(seq(start,fin,increm),function(i){
        
        est <- replicate(n = reps, expr = {
            x1 <- rnorm(i, 0, 1)
            x2 <- rnorm(i, d, 1)
            tval = t.test(x1, x2, var.equal = TRUE)$statistic
            d.ci = ci.smd(ncp = tval, n.1 = i, n.2 = i)
            w.obt = (d.ci$Upper.Conf.Limit.smd-
                     d.ci$Lower.Conf.Limit.smd)/2
            w.obt
        })
        
        lr.prob <- sum(est <= w)/reps
        lr.prob.res <- cbind(i,lr.prob)
        lr.prob.res
      
    })
      
    }
      
  # Plot the results and save them
  
  # pretty_breaks is part of the scales package:
  # Hadley Wickham (2018). scales: Scale Functions for Visualization. R
  # package version 1.0.0. https://CRAN.R-project.org/package=scales
  
    if(plotit == TRUE){
    
    # Plot the result
  
    res.plot = as.data.frame(cbind(y = res[2,],x = res[1,]))
    x11()
    pl = 
    ggplot(data = res.plot, aes(x = x, y = y)) +
      geom_point() + 
      geom_line() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 20), name = paste0("Long-run probability that w.obt <= w (replications =",reps,")")) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 20), name = paste("Sample size")) +
      ggtitle(paste0("Planning for precision with d = ",round(d,3)," (w = ",round(w,3),")")) +
      theme_light()
    
    print(pl)
    
    # Save plot as eps file
    sapply(c(".eps",".png"), function(i){
      plotname = paste0(abs(round(d,2)),"_",effect,"_aipe",i)
      ggsave(here("plots","precision_power", plotname), plot = pl, width = 7, height = 5, units = "in")
    })
    
    
    }
  
  # Restructure res for a nice output
  
  res = as.data.frame(t(res))
  colnames(res) = c("n","power")

  if(sav2csv == TRUE){
    
    name = paste0(abs(round(d,2)),"_",effect, "_aipe.csv")
    
    write.table(res,
                file = here("results","precision_power",name),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
    
  }
  
  # Define a function that returns the first n where long-run probability that
  # w <= d/2 is equal to 1 (approximately 99% assurance)
  
  find.n <- function(i){
    
    rank = which(i$power == 1)[1]
    n = i$n[rank]
    
    return(n = round(n))
    
  }
  
  # Apply find.n function to your result to return n required with 99% assurance
  
  est.n = find.n(res)
  
    
  return("n" = est.n)
  }

#-------------------------------------------------------------------------------

precPLOT.pair <- function(pr = TRUE){
  
################################################################################ 
# This function generates a plot of sample size curves for the AIPE sample size
# analysis with assurance (dz only)
#
# Example use:
# precPLOT.pair()
################################################################################ 
   
  # Label x coordinates @ 50% power
  
  label.60 =
    which(round(dz_aipe_0.60$power,2) > 0.5 & round(dz_aipe_0.60$power,2) < 0.7)[1]
  label.60 = dz_aipe_0.60$n[label.60]
  
  label.37 =
    which(round(dz_aipe_0.37$power,2) > 0.5 & round(dz_aipe_0.37$power,2) < 0.7)[1]
  label.37 = dz_aipe_0.37$n[label.37]
  
  pl =
  ggplot() + 
    geom_point(data = dz_aipe_0.60, aes(x = n, y = power)) +
    geom_line(data = dz_aipe_0.60, aes(x = n, y = power)) +
    geom_point(data = dz_aipe_0.37, aes(x = n, y = power)) +
    geom_line(data = dz_aipe_0.37, aes(x = n, y = power), linetype = 2) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 20), 
                       name = expression(paste("Long-run probability that ",
                                               italic("MoE") <= "half of ", delta[z]))) + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20), name = paste("Sample size")) +
    ggtitle(paste0("Planning for precision")) +
    geom_label(aes(x = label.60, y = 0.55), label = expression(paste(delta[z],
                                                               "= -0.60"))) +
    geom_label(aes(x = label.37, y = 0.55), label = expression(paste("Safeguard ",delta[z],
                                                                "= -0.37"))) +
    theme_light()
  
  sapply(c(".eps",".png"), function(i){
    plotname = paste0("precPLOT_pair",i)
    ggsave(here("plots","precision_power", plotname), plot = pl, width = 7, height = 5, units = "in")
  })
  
  if(pr == TRUE){print(pl)}
  
}

#-------------------------------------------------------------------------------

aipe.res <- function(retention.rate = 0.7083, trial.reps = 10, assurance = 0.99, sav2csv = TRUE){
  
################################################################################ 
# This function reproduces Table 5
#
# Example use:
# aipe.res()
################################################################################ 
  
  find.n <- function(i){
    
    rank = which.min(abs(i$power-assurance))
    n = i$n[rank]
    adj.n = n/retention.rate
    
    return(list(n = round(n), adjusted.n = round(adj.n)))
    
  }
  
  res = t(cbind(
    "dz_0.60" = find.n(dz_aipe_0.60),
    "dz_0.37" = find.n(dz_aipe_0.37),
    "ds_0.8" = find.n(ds_aipe_0.8),
    "ds_0.7" = find.n(ds_aipe_0.7),
    "ds_0.6" = find.n(ds_aipe_0.6),
    "ds_0.5" = find.n(ds_aipe_0.5),
    "ds_0.4" = find.n(ds_aipe_0.4),
    "ds_0.3" = find.n(ds_aipe_0.3),
    "ds_0.2" = find.n(ds_aipe_0.2)
  ))
  
  required.n = ceiling(as.numeric(res[,"adjusted.n"])/trial.reps)
  
  moe = rbind(
    0.30, 
    0.19,
    0.40,
    0.35,
    0.3,
    0.25,
    0.2,
    0.15,
    0.1
  )
  
  res = cbind(moe, res, required.n)
  colnames(res) = c("MoE", "N", "N Adjusted","Recruitment Rate")
  
  if(sav2csv == TRUE){
    
    temp = res
    temp = cbind(rownames(temp),temp)
    colnames(temp)[1] = "d"

    write.table(temp,
                file = here("results","precision_power","table_5.csv"),
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
    
  }
  
  return(as.data.frame(res))
  
}

#-------------------------------------------------------------------------------
# End script
#-------------------------------------------------------------------------------