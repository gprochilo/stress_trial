#     GNU General Public License v3.0
#
#     ProcFun_stress-v1: R functions for stress trial mansuscript.
#     Copyright (C) 2019  Guy A. Prochilo
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 
#     Contact
#     Name: Guy A. Prochilo
#     Email: guy.prochilo@gmail.com
# 
#     Last update: June 2019
#
#     Cite as:
#     Prochilo, G. A. (2019). ProcFun_stress-v1: R functions for stress trial 
#     mansuscript.
#     Retrieved from https://github.com/gprochilo

#-------------------------------------------------------------------------------

  # Print the above license whenever ProcFun_stress-v1.R is sourced
  # If the license exceeds 32 lines in future, update 32 to a higher number
  
  writeLines(readLines("ProcFun_stress-v1.R",28))

#-------------------------------------------------------------------------------

quiet <- function(x){ 

  # This function suppresses the print output of functions that do not provide
  # the option to turn off print output 
  #
  # x: script for which you would like to suppress print output
  #
  # Examples:
  # Suppress the output of the MBESS::ci.sm function
  # quiet(MBESS::ci.sm(sm = 0.5, N = 50))
  #
  # Begin script

  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

#-------------------------------------------------------------------------------

feasibility <- function(values = c("f.rates","flow")){

  # This function returns the feasibility rate and participant flow data
  #
  # values: Character string specifying whether the return feasibility rates 
  # ('f.rates') or participant flow ('flow').
  #
  # Examples:
  # Compute and report participant flow data
  # feasibility(values = "flow")
  #
  # Begin script
  
  # Define a frequencies function to return frequencies of reasons
  
  freqs <- function(i){
    res = userfriendlyscience::freq(i)
    
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
  
  # Compute the number of observations for each stage of the participant flow
  
  # Enrolment
  n.applied = length(feas$id)
  n.ineligible = sum(stats::na.omit(feas$ineligible == 1))
  n.eligible = n.applied-n.ineligible
  reason.ineligible = freqs(stats::na.omit(feas$why.ineligible))
  n.declined = sum(stats::na.omit(feas$they.declined == 1))
  
  # Assignment and allocation
  n.nonrandomized = sum(stats::na.omit(feas$not.nonrandomized == 0))
  
  # Follow-up
  n.completed = sum(stats::na.omit(feas$dropout == 0))
  n.dropout = sum(stats::na.omit(feas$dropout == 1))
  reason.dropout = freqs(stats::na.omit(feas$why.dropout))
  
  # Analysis
  n.quest = sum(stats::na.omit(feas$missed.quest.t1 == 0))
  n.vo2max = sum(stats::na.omit(feas$missed.vo2max.t1 == 0))
  n.economy.6km = sum(stats::na.omit(feas$max.velocity >= 6))
  n.economy.8km = sum(stats::na.omit(feas$max.velocity >= 8))
  n.economy.10km = sum(stats::na.omit(feas$max.velocity >= 10))
  n.economy.12km = sum(stats::na.omit(feas$max.velocity >= 12))
  n.economy.14km = sum(stats::na.omit(feas$max.velocity >= 14))
    
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
  
  # Return the following values give = "participant.flow" 

  if(values == "flow"){
  
  res = list(
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
  )
  
  sink(file = here::here("results","feasibility","flow.txt"))
  print(res)
  sink()
  
  }

  # Return the following values if give = "feas.rates"  
  
  if(values == "f.rates"){
    
    res = list(
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
    )
    
    sink(file = here::here("results","feasibility","feasibility_rates.txt"))
    print(res)
    sink()
    
  }
  
  return(res)
  
}

#-------------------------------------------------------------------------------

crit.vals <- function(n,
                      type = c("paired","two.sample"),
                      alpha = .05,
                      silent = FALSE){
  
  # This function computes the critical t and d values for a paired sample or 
  # an unpaired two sample t test. It also returns confidence limits on the 
  # critical d value and the margin-of-error (moe) on d. 
  #
  # n: Total sample size
  # type: Character string specifying whether to return values for a paired 
  # sample ('paired') or unpaired sample ('two.sample') t test. 
  # alpha: maximum Type I error rate
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Examples:
  # Compute the critical t and d values for a paired-sample test
  # crit.vals(n = 17, type = "paired")
  #
  # Begin script
  
  # Define degrees of freedom for the specified test
  
  if(type == "paired"){df = n-1}
  if(type == "two.sample"){df = n-2}
  
  # Compute the critical t value
  
  t.crit <- qt(1-(alpha/2), df)
  
  # If type = "paired" perform the following computations
  
  if(type == "paired"){
    d.crit = t.crit/sqrt(n)
    ci = as.numeric(
      quiet(MBESS::ci.sm(sm = d.crit, N = n)[c(1,3)])
    )
    moe = (ci[2]-ci[1])/2
    
    res.d = sprintf("d.crit = %.2f, %.0f%% CI [%.2f, %.2f], MoE = %.2f",
                  d.crit,
                  (1-alpha)*100,
                  ci[1],
                  ci[2],
                  moe)
    
    res.t = sprintf("t.crit(%.0f) = %.2f, p.crit = %.3f",
                    df,
                    t.crit,
                    alpha)
    }
  
  # If type = "two.sample" perform the following computations
  
  if(type == "two.sample"){
    d.crit = (2*t.crit)/sqrt(df)
    ci = as.numeric(
      quiet(MBESS::ci.smd(smd = d.crit, n.1 = n/2, n.2 = n/2)[c(1,3)])
    )
    
    
    d.ci = quiet(ci.smd(smd = d.crit, n.1 = n/2, n.2 = n/2))
    d.ci.LL = d.ci$Lower.Conf.Limit.smd
    d.ci.UL = d.ci$Upper.Conf.Limit.smd
    moe = (ci[2]-ci[1])/2

    res.d = sprintf("d.crit = %.2f, %.0f%% CI [%.2f, %.2f], MoE = %.2f",
                    d.crit,
                    (1-alpha)*100,
                    ci[1],
                    ci[2],
                    moe)
    
    res.t = sprintf("t.crit(%.0f) = %.2f, p.crit = %.3f",
                    df,
                    t.crit,
                    alpha)
    }

  # Define the res variable as our results of interest
  
  res = list(d.crit = c("ci.LL" = ci[1], "d" = d.crit, "ci.UL" = ci[2]),
             moe = moe,
             df = df,
             t.crit = t.crit,
             p.crit = alpha)
  
  # Round the res variable to two decimal places
  
  res.round = lapply(res,round,2)
  
  
  # If silent = FALSE print the following strings
  
  if(silent == FALSE){
    
    cat(res.d,"\n")
    cat(res.t,"\n\n")    
    
  }
  
  print(res.round)
  
  return(
    invisible(list(d.crit = c("ci.LL" = ci[1], "d" = d.crit, "ci.UL" = ci[2]),
              moe = moe,
              df = df,
              t.crit = t.crit,
              p.crit = alpha))
  )
}

#-------------------------------------------------------------------------------

descrip <- function(dv, dataset, silent = FALSE){
  
  # This function computes the mean, standard deviation, min, and max values
  # for any given dv stored in a data frame.
  # 
  # dv: dependent variable defined as a character string
  # dataset: data frame containing the variables
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Examples:
  # Compute baseline descriptives for pss.0
  # descrip(dv = pss.0, dataset = dat.wide)
  #
  # Begin script

  # Define x from dataset

  if(!is.character(dv)){
    stop("dv must be defined as a character",
         call. = FALSE)
  }
  
  x = get(dv, dataset)
  
  # Compute the following descriptives
  
  mu = mean(x)
  sdev = sd(x)
  min = min(x)
  max = max(x)
  
  # Define res variable as a dataframe of descriptives
  
  res = data.frame(mu = mu,
                   sdev = sdev,
                   min = min,
                   max = max)
  
  # Round the data for printing
  
  res.round = round(res, 2)
  if(silent == FALSE){print(res.round)}
  
  # Return res variable to full set of decimal places
  
  return(invisible(res))

}

#-------------------------------------------------------------------------------

descrip.res <- function(dvs, 
                        dataset, 
                        sav2csv = FALSE,
                        sav2dir, 
                        silent = FALSE){
  
  # This function computes the mean, standard deviation, min, and max values
  # for a set of dependent variables whose string names are stored in a vector,
  # and then returns the results in a formatted APA-like table for reporting.
  # 
  # dvs: vector of dependent variable names as character strings
  # dataset: data frame containing the variables
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Examples: Report descriptive results for all variables defined by dvs
  # descrip.res(dvs = c("age.0","male.0","caucasian.0","working.0","bmi.0","pss.0","vo2max.0"), 
  #             dataset = dat.wide, 
  #             sav2csv = TRUE, sav2dir = c("results","tables","table1.csv"))
  #
  # Begin script
  
  # If sav2csv = TRUE we must specify our save directory and file name in sav2dir
  
  if(sav2csv == TRUE){
    if(missing(sav2dir)){
      stop("Please specify the directory tree as a vector of strings in 'sav2dir'",
           call. = FALSE)}
  }
  
  # If sav2dir is defined we must ensure the final element must be a string name
  # ending in the '.csv' file format
  
  if(!missing(sav2dir)){
    check = tail(sav2dir, n = 1)
    end = substr(check, nchar(check)-3+1, nchar(check))
    if(end != "csv"){
      stop("The final string in the 'sav2dir' vector is your csv file name and must end in '.csv'",
           call. = FALSE)
    }
  }
    
  # Apply all dvs string names to the descrip function and return a matrix
  # Transpose this matrix and save it as a data frame in the res variable
  
  res = sapply(dvs, function(i){
    descrip(dv = i, dataset = dataset, silent = TRUE)
  })
  
  res = as.data.frame(t(res))

  # Format the results in accordance with an APA-like table
  
  s1 = sprintf("%.2f (%.2f)",res$mu, res$sdev)
  s2 = sprintf("%.2f - %.2f",res$min, res$max)
  s3 = cbind(s1,s2)
  colnames(s3) = c("M (SD)", "Range")
  rownames(s3) = dvs
  final = as.data.frame(s3)
  
  # If sav2csv = TRUE save these results in the directory specified in sav2dir
  
  if(sav2csv == TRUE){
    
    final.csv = final
    dv = rownames(final.csv)
    rownames(final.csv) = NULL
    final.csv = cbind(dv, final.csv)
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    utils::write.table(final.csv,
                       file = dir,
                       append = F,
                       row.names = F,
                       col.names = T,
                       sep=",")
    
  }
  
  if(silent == FALSE){print(final)}
  
  return(invisible(final))
}

#-------------------------------------------------------------------------------

descrip.by <- function(dv, by, dataset, dichot = NULL, count = FALSE){
  
  # This function computes descriptives for 'dv' as a function of whether a 'by'
  # variable decreased (<0) or increased/stayed the same (>=0). It returns a 
  # three column data frame: column 1 is descriptives for full data set of 'dv',
  # column 2 is descriptives for 'dv' for only those who decreased on the 'by' 
  # variable, column 3 is descriptives for 'dv' for only those who increased/stayed
  # the same on the 'by' variable. 
  # 
  # dv: dependent variable name as string
  # by: by variable name as string
  # dataset: data frame containing the dv and by variables
  # dichot: if dichot is specified, the descriptives will be computed as counts
  # and percentages of the dichot level. E.g. dichot = 1 will return how many
  # observations were had a dummy value of 1 out of the full data set.
  # count: a logical indicating whether to return count and percentage data. If
  # factor is specified this logical is not necessary. 
  #
  # Example use:
  # Compute pss.0 scores for full dataset, those who experience reduction
  # in pss.ch, and those who experienced an increase or no change in pss.ch
  # descrip.by(dv = "pss.0", by = "pss.ch", dataset = dat.wide)
  
  # Begin script
  
  if(!is.character(dv) || !is.character(by)){
    stop("dv and by variable names must be defined as characters",
         call. = FALSE)
  }
  
  # Define x from dataset
  
  all = get(dv, dataset)
  by.var = get(by, dataset)
  
  dat = as.data.frame(cbind(all, by.var))
  
  # Flag variables in by.var < 0 and create two new vectors:
  # less: dv < 0 on by variable
  # greq: dv >= 0 on by variable
  
  flag = dat$by.var < 0
  less = dat$all[flag]
  greq = dat$all[!flag]
  
  # Return descriptives for all, less, and greq
  
  dothis <- function(i){
    
    n = length(i)
    if(!is.null(dichot)){n = sum(i == dichot)}
    pcnt = (n/length(all))*100
    mu = mean(i)
    sdev = sd(i)
    min = min(i)
    max = max(i)
    
    return(list(n = n, pcnt = pcnt, mu = mu, sdev = sdev, min = min, max = max))
    
  }
    
  # Apply the function to all, less, and greq
    
  aa = dothis(all)
  ll = dothis(less)
  gg = dothis(greq)
    
  # Combine these results into a transposed data frame  
  
  res = as.data.frame(t(cbind(aa, ll, gg)))
  rownames(res) = c("all", "less", "greq")
    
  # Format results as an APA-like table
    
  s1 = rbind(sprintf("%.2f (%.2f)", res$mu, res$sdev))
  if(!is.null(dichot) || count == TRUE){s1 = rbind(sprintf("%s (%.2f%%)", res$n, res$pcnt))}
  colnames(s1) = cbind("all", paste0(by,"<0"), paste0(by,">=0"))
  rownames(s1) = dv
  res2 = as.data.frame(s1)
    
  return(res = list(
    res1 = t(res), 
    res2 = res2))
  
}

#-------------------------------------------------------------------------------

baseline.res <- function(sav2csv = FALSE, sav2dir, silent = FALSE){
  
  # The baseline.res function is a lazy function that will return the baseline
  # table in an APA-like data frame. 
  #
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  # 
  # Example use:
  # Save the baseline.res data to a specific directory 
  # baseline.res(sav2csv = TRUE, sav2dir = c("results","tables","table1.csv"))
  
  # Begin script
  
  # If sav2csv = TRUE we must specify our save directory and file name in sav2dir
  
  if(sav2csv == TRUE){
    if(missing(sav2dir)){
      stop("Please specify the directory tree as a vector of strings in 'sav2dir'",
           call. = FALSE)}
  }
  
  # If sav2dir is defined we must ensure the final element must be a string name
  # ending in the '.csv' file format
  
  if(!missing(sav2dir)){
    check = tail(sav2dir, n = 1)
    end = substr(check, nchar(check)-3+1, nchar(check))
    if(end != "csv"){
      stop("The final string in the 'sav2dir' vector is your csv file name and must end in '.csv'",
           call. = FALSE)
    }
  }
  
  # Define the following data frame
  
  res = 
    rbind(
      "total" = descrip.by(dv = "pss.ch", by = "pss.ch", dataset = dat.wide, count = TRUE)$res2,
      "baseline.pss" = descrip.by(dv = "pss.0", by = "pss.ch", dataset = dat.wide)$res2,
      "age" = descrip.by(dv = "age.0", by = "pss.ch", dataset = dat.wide)$res2,
      "male" = descrip.by(dv = "male.0", by = "pss.ch", dataset = dat.wide, dichot = 1)$res2,
      "female" = descrip.by(dv = "male.0", by = "pss.ch", dataset = dat.wide, dichot = 0)$res2,
      "caucasian" = descrip.by(dv = "caucasian.0", by = "pss.ch", dataset = dat.wide, dichot = 1)$res2,
      "asian" = descrip.by(dv = "caucasian.0", by = "pss.ch", dataset = dat.wide, dichot = 0)$res2,
      "baseline.vo2max" = descrip.by(dv = "vo2max.0", by = "pss.ch", dataset = dat.wide)$res2,
      "baseline.bmi" = descrip.by(dv = "bmi.0", by = "pss.ch", dataset = dat.wide)$res2
    )
  
  # If sav2csv = TRUE save these results in the directory specified in sav2dir
  
  if(sav2csv == TRUE){
    
    s1 = rownames(res)
    s2 = cbind("dv" = s1, res)
    rownames(s2) = NULL
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    utils::write.table(s2,
                       file = dir,
                       append = F,
                       row.names = F,
                       col.names = T,
                       sep=",")
    
  }
  
  if(silent == FALSE){print(res)}
  
  return(invisible(res))
}

#-------------------------------------------------------------------------------

pair.test <- function(dv, 
                      long.dataset, 
                      plots = FALSE,
                      pair.plot.wXh = c(5,6),
                      diff.plot.wXh = c(3,6),
                      silent = FALSE){
  
  # This function computes descriptive and inferential statistics for t1 vs. t0
  # gains in variables with paired data points. Inferential statistics include 
  # standard and robust t tests. Unstandardized gain scores are computed with
  # 80-95% confidence limits. Standardized gain scores are computed as dz 
  # (Cohen's dz) and dav (Cohen's dav), and are each reported with their 80-95%
  # confidence limits. This function also generates a plot of t1 vs. t0 scores
  # and a plot of t1-t0 difference scores. The latter plots confidence limit 
  # error bars ranging from 80-95%. 
  #
  # dv: dependent variable string name
  # long.dataset: data frame storing data in long format
  # plots: logical indicating whether to generate and save plots
  # pair.plot.wXh: width and height dimensions for the pair plot (inches)
  # diff.plot.wXh: width and height dimensions for the diff plot (inches)
  # silent: logical indicating whether to print output or silence output
  #
  # Example use:
  # Compute pair.test for pss scores
  # pair.test(dv = "pss", long.dataset = dat.long, plots = TRUE)
  #
  # Begin script
  
  # Reshape long dataset into wide dataset
  
  dat.wide <- reshape(long.dataset, idvar = "id", timevar = "time", direction = "wide")
  
  # Define pre (t0) and post (t1) variables
  
  x = paste0(dv,".0")
  y = paste0(dv,".1")
  
  t0 = get(x, dat.wide)
  t1 = get(y, dat.wide)

  # Define a function to compute descriptive statistics for t0 and t1 data
  
  dothis <- function(i){
    
    mu = mean(i)
    sdev = sd(i)
    ci = as.numeric(stats::t.test(i, mu = 0)$conf.int)
    ci.LL = ci[1]
    ci.UL = ci[2]
    
    return(list(mu = mu, sdev = sdev, ci.LL = ci.LL, ci.UL = ci.UL))
    
  }
  
  # Compute descriptive statistics for t0 and t1
  
  t0.res = dothis(t0)
  t1.res = dothis(t1)
  
  # Define a function to compute descriptive and inferential statistics for
  # the t1-t0 gain score data
  
  gain <- function(i,j){
    ch = i-j
    n = length(ch)
    r = stats::cor(i, j, method = "pearson")
    mu = mean(ch)
    sdev = sd(ch)
    sdev.av = sqrt((sd(i)^2+sd(j)^2)/2)
    min = min(ch)
    max = max(ch)
    test = stats::t.test(ch, mu = 0)
    tval = as.numeric(test$statistic)
    df= as.numeric(test$parameter)
    pval = as.numeric(test$p.value)
    dz = mu/sdev
    dav = mu/sdev.av
    cl.ES = 1 - pnorm(mu/sdev)
    r.pval = trimpb(x = i-j, tr = 0.2, nboot = 2000, pr = FALSE)$p.value
    
    return(list(n = n, 
                r = r, 
                mu = mu, 
                sdev = sdev, 
                sdev.av = sdev.av, 
                min = min, 
                max = max,
                tval = tval,
                df = df,
                pval = pval,
                r.pval = r.pval,
                dz = dz,
                dav = dav,
                cl.ES = cl.ES))
  
    }
  
  # Compute descriptive and inferential statistics for gain score data
  
  gain.res = gain(t1, t0)
  
  # Define a function to compute confidence limits from 80-95% on gain scores
  
  test.ci <- function(x, y){
    
    data = x-y
    
    res1 = 
      sapply(seq(0.80, 0.95, 0.05), function(i){
        ci = t.test(x = data, mu = 0, conf.level = i)$conf.int
        c(ci[1], ci[2])
      })
    
    res2 = t(res1)
    colnames(res2) = c("ci.LL","ci.UL")
    res2 = c(res2[1,], res2[2,], res2[3,], res2[4, ])
    res2 = as.data.frame(rbind(res2))
    cols = rep(format(seq(0.80, 0.95, 0.05), nsmall = 2), each = 2)
    colnames(res2) = paste(colnames(res2), cols, sep = ".")
    res2 = as.list(res2)
    return(res2)
    
  }
  
  # Extract confidence limits from 80-90% on gain scores
  
  gain.ci = test.ci(t1,t0)
  
  # Define a function to compute 80-95% confidence limits on dz and dav
  
  dci <- function(d, n, tval, df, sdev, sdev.av, type = c("dz","dav")){
    
    if(type == "dz"){
      
      res1 = 
        sapply(seq(0.80, 0.95, 0.05), function(i){
        quiet(as.numeric(MBESS::ci.sm(sm = d, N = n, conf.level = i))[c(1,3)])
      })
      
      res2 = t(res1)
      colnames(res2) = c("ci.LL","ci.UL")
      res2 = c(res2[1,], res2[2,], res2[3,], res2[4, ])
      res2 = as.data.frame(rbind(res2))
      cols = rep(format(seq(0.80, 0.95, 0.05), nsmall = 2), each = 2)
      colnames(res2) = paste(colnames(res2), cols, sep = ".")
      res2 = as.list(res2)
      return(res2)
      
    }
    
    if(type == "dav"){
      
      res1 =
        sapply(seq(0.80,0.95,0.05), function(i){
        limits <- quiet(MBESS::conf.limits.nct(df = df, t.value = tval, conf.level = i))
        ci.LL <- (limits$Lower.Limit*sdev)/(sdev.av*sqrt(n))
        ci.UL <- (limits$Upper.Limit*sdev)/(sdev.av*sqrt(n))
        c(ci.LL, ci.UL)
        })
      
      res2 = t(res1)
      colnames(res2) = c("ci.LL","ci.UL")
      res2 = c(res2[1,], res2[2,], res2[3,], res2[4, ])
      res2 = as.data.frame(rbind(res2))
      cols = rep(format(seq(0.80, 0.95, 0.05), nsmall = 2), each = 2)
      colnames(res2) = paste(colnames(res2), cols, sep = ".")
      res2 = as.list(res2)
      return(res2)
      
    }
  }
  
  # Compute 80-95% confidence limits on dz and dav
  
  dav.ci = dci(d = gain.res$dav, 
               n = gain.res$n, 
               tval = gain.res$tval, 
               df = gain.res$df, 
               sdev = gain.res$sdev, 
               sdev.av = gain.res$sdev.av, 
               type = "dav")
  
  dz.ci = dci(d = gain.res$dz,
              n = gain.res$n, 
              type = "dz")

  
  # The following scripts generate a plot of t1 vs t0 scores and t1-t0 scores
  
  if(plots == TRUE){
    
    # Generate a plot of the T1 and T0 scores
    
    pair.plot =
      
      suppressWarnings(
        
        ggplot() +
          geom_violin(dat = long.dataset, aes(x = factor(time), y = long.dataset[,dv]), size = 0.3, alpha = 0.8) +
          geom_point(dat = long.dataset, aes(x = factor(time), y = long.dataset[,dv], group = id), shape = 1) +
          geom_line(dat = long.dataset, aes(x = factor(time), y = long.dataset[,dv], group = id), linetype = "dashed", size = 0.3) +
          geom_point(aes(x = factor(c(0,1)), y = c(t0.res$mu, t1.res$mu)), size = 2) +
          geom_line(aes(x = factor(c(0,1)), y = c(t0.res$mu, t1.res$mu), group =1), size = 1) +
          geom_errorbar(aes(x = factor(0), y = t0.res$mu), ymin = t0.res$ci.LL, ymax = t0.res$ci.UL, width = 0.1, size = 0.5) +
          geom_errorbar(aes(x = factor(1), y = t1.res$mu), ymin = t1.res$ci.LL, ymax = t1.res$ci.UL, width = 0.1, size = 0.5) +
          xlab("Time") + ylab("DV") + 
          scale_x_discrete(labels=c("T0","T1")) + 
          scale_y_continuous(breaks = scales::pretty_breaks(n = 10), name = paste(dv, "Scores")) +
          theme_light()  
        
      )
    
    print(pair.plot) 
    
    # Save plot as eps file
    
    ggsave(here("plots","pair_plots", paste(dv,"_prepost.eps", sep = "")), plot = pair.plot, width = pair.plot.wXh[1], height = pair.plot.wXh[2], units = "in")
    ggsave(here("plots","pair_plots", paste(dv,"_prepost.png", sep = "")), plot = pair.plot, width = pair.plot.wXh[1], height = pair.plot.wXh[2], units = "in")
    
    # Generate a plot of the T1-T0 gain scores
    
    diff.plot =
      
      suppressWarnings(
        
        ggplot() +
          geom_violin(aes(x = "", y = t1-t0), alpha = .8, size = 0.3) +
          geom_point(aes(x = "", y = t1-t0), shape = 1, position = position_jitter(width = 0.1, height = 0)) +
          geom_point(aes(x = "", y = gain.res$mu)) +
          
          mapply(function(i,j,k){
            geom_errorbar(aes(x = "", y = gain.res$mu), ymin = i, ymax = j, width = k, size = 0.5)
          }, c(gain.ci$ci.LL.0.80,
               gain.ci$ci.LL.0.85,
               gain.ci$ci.LL.0.90,
               gain.ci$ci.LL.0.95), 
             c(gain.ci$ci.UL.0.80,
               gain.ci$ci.UL.0.85,
               gain.ci$ci.UL.0.90,
               gain.ci$ci.UL.0.95),
             seq(0.05,0.20,0.05)
          ) +
          
          geom_hline(yintercept = 0, linetype = 2, size = 0.5) +
          theme_light() +
          scale_y_continuous(breaks = scales::pretty_breaks(n = 10), name = paste(x,"Gain Scores")) +
          xlab("T1-T0")
        
      )
    
    print(diff.plot)
    
    ggsave(here("plots","pair_diff_plots", paste0(dv,"_change.eps")), plot = diff.plot, width = diff.plot.wXh[1], height = diff.plot.wXh[2], units = "in")
    ggsave(here("plots","pair_diff_plots", paste0(dv,"_change.png")), plot = diff.plot, width = diff.plot.wXh[1], height = diff.plot.wXh[2], units = "in")
    
  }
  
  # Define the final output of the analysis
  
  final = list(
    t1.res = t1.res,
    t0.res = t0.res,
    gain.res = gain.res,
    gain.ci = gain.ci,
    dav.ci = dav.ci,
    dz.ci = dz.ci)
  
  # Return this output if silent = FALSE
  
  if(silent == FALSE){print(final)}
  
  return(invisible(final))
}
  
#-------------------------------------------------------------------------------

pair.sensitivity <- function(dv, dropout.n, long.dataset, silent = FALSE){
  
  # This function performs a sensitivity analysis on a dependent variable of 
  # interest that assumes all observations for 'dropout.n' would have reported
  # a zero change in t1-t0 gain scores. 
  #
  # dv: dependent variable string name
  # dropout.n: number of observations that dropped out before t1
  # long.dataset: data frame storing data in long format
  # silent: logical indicating whether to print output or silence output
  #
  # Example use:
  # Compute a sensitivity analysis on pss scores for a dropout equal to 7
  # pair.sensitivity(dv = "pss", dropout.n = 7, long.dataset = dat.long)
  #
  # Begin script
  
  # Reshape long dataset into wide dataset
  
  dat.wide <- reshape(long.dataset, idvar = "id", timevar = "time", direction = "wide")
  
  # Define pre (t0) and post (t1) variables
  
  x = paste0(dv,".0")
  y = paste0(dv,".1")
  
  t0 = get(x, dat.wide)
  t1 = get(y, dat.wide)
  
  # Add zero gain scores for the number of specified dropouts under the 
  # assumption there would have been no change in this variable
  
  ch = c(t1-t0, rep(0,dropout.n))

  gain <- function(i){
    n = length(i)
    mu = mean(i)
    sdev = sd(i)
    min = min(i)
    max = max(i)
    test = stats::t.test(i, mu = 0)
    ci = as.numeric(test$conf.int)
    tval = as.numeric(test$statistic)
    df= as.numeric(test$parameter)
    pval = as.numeric(test$p.value)
    dz = mu/sdev
    r.pval = trimpb(x = i, tr = 0.2, nboot = 2000, pr = FALSE)$p.value
    
    return(list(n = n, 
                mu = mu,
                ci = ci,
                sdev = sdev, 
                min = min, 
                max = max,
                tval = tval,
                df = df,
                pval = pval,
                r.pval = r.pval,
                dz = dz))
    
  }
  
  # Apply function to t1-t0 change scores and define the output as res1
  
  res1 = gain(ch)
  
  # Prepare the data in a reporting format and save this as res2
  
  res2 = 
  sprintf("M (SD) = %.2f (%.2f), 95%% CI [%.2f, %.2f], p = %.3f, pR = %.3f",
          res1$mu, 
          res1$sdev, 
          res1$ci[1], 
          res1$ci[2], 
          res1$pval, 
          res1$r.pval,
          res1$dz)
  
  # Re-define res1 as the combination of res2 and res1
  
  res1 = c("report" = res2, res1)
  
  # If silent = FALSE print res1
  
  if(silent == FALSE){
    print(res1)
  }
  
  return(invisible(res1))
  
}

#-------------------------------------------------------------------------------

pair.test.res <- function(dvs, 
                          long.dataset, 
                          plots = FALSE, 
                          pair.plot.wXh = c(7,6),
                          diff.plot.wXh = c(6.65,4.50),
                          sav2csv = FALSE,
                          sav2dir, 
                          silent = FALSE){

  # This function performs the pair.test function for a vector of variable name
  # strings defined by dvs, and returns all analyses in a data frame. 
  #
  # dvs: vector of dependent variable names as character strings
  # long.dataset: data frame containing the variables in long format
  # plots: a logical indicating whether or not to generate and print plots
  # pair.plot.wXh: width and height dimensions for the pair plot (inches)
  # diff.plot.wXh: width and height dimensions for the diff plot (inches)
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Example use:
  # Run pair.test for all of the following dvs stored in dat.long
  # pair.test.table(dvs = c("pss",
                        #   "sdass",
                        #   "adass",
                        #   "ddass",
                        #   "who",
                        #   "maas",
                        #   "erq.cr",
                        #   "rrs.br",
                        #   "pswq",
                        #   "vo2max"), 
                        # long.dataset = dat.long, 
                        # sav2csv = TRUE, 
                        # sav2dir = c("results", "tables", "table3.csv"))
  #
  # Begin script
  
  # Apply all variables stored in dvs to the following function
  
  s1 = 
    sapply(dvs, function(i){
      pair.test(dv = i, 
                long.dataset = long.dataset, 
                plots = plots, 
                pair.plot.wXh = pair.plot.wXh, 
                diff.plot.wXh = pair.plot.wXh, 
                silent = TRUE)
    })
  
  # Transpose the s1 matrix
  
  s1 = t(s1)
  
  # Apply all values stored in dvs to the following function
  
  s2 = 
  sapply(dvs, function(i){
  x = as.data.frame(s1[i,])
  cbind(x)
  })
  
  # Transpose s2, unlist the matrix, and save it as a data frame
  
  s2 = t(s2)
  s2 = as.data.frame(apply(s2, 2, unlist))
  
  # If sav2csv is true perform the following scripts
  
  if(sav2csv == TRUE){
    
    temp = cbind("dv" = rownames(s2), s2)
    rownames(temp) = NULL
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    utils::write.table(temp,
                       file = dir,
                       append = F,
                       row.names = F,
                       col.names = T,
                       sep=",")
  }
  
  if(silent == FALSE){
    cat(paste("note: columns 8 through to",length(s2[1,]), "are not shown", sep = " "),"\n")
    cat("note: assign the function to a variable to view the full set of results in R", "\n\n")
    print(s2[,1:7])
  }
  
  return(invisible(s2))
}

#-------------------------------------------------------------------------------

pair.test.table <- function(dvs, 
                            long.dataset, 
                            sav2csv = FALSE,
                            sav2dir, 
                            silent = FALSE){
 
  # This function performs the pair.test.res function for a vector of variable 
  # name strings defined by dvs, and returns all analyses in an APA-like table.
  # It does not generate plots.
  #
  # dvs: vector of dependent variable names as character strings
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Example use:
  # Run pair.test for all of the following dvs stored in dat.long
  # pair.test.table(dvs = c("pss",
                        #   "sdass",
                        #   "adass",
                        #   "ddass",
                        #   "who",
                        #   "maas",
                        #   "erq.cr",
                        #   "rrs.br",
                        #   "pswq",
                        #   "vo2max"), 
                        # long.dataset = dat.long, 
                        # sav2csv = TRUE, 
                        # sav2dir = c("results", "tables", "table3.csv"))
  #
  # Begin script
  
  # Define res as the result of pair.test.res
  
  res = 
    pair.test.res(dvs = dvs, 
                  long.dataset = long.dataset, 
                  plots = FALSE, 
                  sav2csv = FALSE, 
                  silent = TRUE)
  
  # Format results into an APA-like table
  
  col1 = sprintf("%.2f (%.2f)", res$t0.res.mu, res$t0.res.sdev)
  col2 = sprintf("%.2f (%.2f)", res$t1.res.mu, res$t1.res.sdev)
  col3 = sprintf("%.2f (%.2f) [%.2f, %.2f]",
                 res$gain.res.mu, 
                 res$gain.res.sdev, 
                 res$gain.ci.ci.LL.0.95,
                 res$gain.ci.ci.UL.0.95)
  col4 = sprintf("%.2f [%.2f, %.2f]", 
                 res$gain.res.dav, 
                 res$dav.ci.ci.LL.0.95, 
                 res$dav.ci.ci.UL.0.95)
  col4.5 = sprintf("%.2f", res$gain.res.tval)
  col5.temp = sprintf("%.3f", res$gain.res.pval)
  col5 = substring(col5.temp, 2)
  flag = col5 == ".000"
  col5[flag] = "<.001"
  col6.temp = sprintf("%.3f", res$gain.res.r.pval)
  col6 = substring(col6.temp, 2)
  flag = col6 == ".000"
  col6[flag] = "<.001"
  
  temp1 = cbind(col1, col2, col3, col4, col4.5, col5, col6)
  colnames(temp1) = c("T0: M (SD)",	"T1: M (SD)",	"Gain: M (SD) [95% CI]", "dav [95% CI]", "t",	"p",	"pR")
  rownames(temp1) = dvs
  final = as.data.frame(temp1)
  
  if(sav2csv == TRUE){
   
    dv = rownames(final)
    temp = cbind(dv, final)
    rownames(temp) = NULL
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
      write.table(temp,
                  file = dir,
                  append = F,
                  row.names = F,
                  col.names = T,
                  sep=",")
      
  }
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))
  
}

#-------------------------------------------------------------------------------

pair.multiconf <- function(dvs,
                           long.dataset,
                           sav2csv = FALSE,
                           sav2dir,
                           silent = FALSE){
 
  # This function performs the pair.test.res function for a vector of variable 
  # name strings defined by dvs, and returns unstandardized effect sizes and
  # standardized effect sizes with 80-95% confidence limits in an APA-like table.
  #
  # dvs: vector of dependent variable names as character strings
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Example use:
  # Run pair.test.res for all of the following dvs stored in dat.long
  # pair.multiconf(dvs = c("pss",
  #                        "sdass",
  #                        "adass",
  #                        "ddass",
  #                        "who",
  #                        "maas",
  #                        "erq.cr",
  #                        "rrs.br",
  #                        "pswq",
  #                        "vo2max"),
  #                long.dataset = dat.long,
  #                sav2csv = TRUE,
  #                sav2dir = c("results", "tables", "supp_table1.csv"))
  #
  # Begin scripts
  
  # Run the pair.test.res function for all variables in dvs and store as res
  
  res = 
    pair.test.res(dvs = dvs, 
                  long.dataset = long.dataset, 
                  plots = FALSE, 
                  sav2csv = FALSE, 
                  silent = TRUE)
  
  # Create a list of lists for the following results
  
  res2 =
    
    lapply(X = dvs, FUN = function(i){
      
      x = res[i,]
      
  col1 = rbind(
    sprintf("%.2f", x$gain.res.mu),
    sprintf("%.2f", x$gain.res.dav)
  )
  
  col2 = rbind(
    sprintf("[%.2f, %.2f]", x$gain.ci.ci.LL.0.80, x$gain.ci.ci.UL.0.80),
    sprintf("[%.2f, %.2f]", x$dav.ci.ci.LL.0.80, x$dav.ci.ci.UL.0.80)
  )
  
  col3 = rbind(
    sprintf("[%.2f, %.2f]", x$gain.ci.ci.LL.0.85, x$gain.ci.ci.UL.0.85),
    sprintf("[%.2f, %.2f]", x$dav.ci.ci.LL.0.85, x$dav.ci.ci.UL.0.85)
  )
  
  col4 = rbind(
    sprintf("[%.2f, %.2f]", x$gain.ci.ci.LL.0.90, x$gain.ci.ci.UL.0.90),
    sprintf("[%.2f, %.2f]", x$dav.ci.ci.LL.0.90, x$dav.ci.ci.UL.0.90)
  )
  
  col5 = rbind(
    sprintf("[%.2f, %.2f]", x$gain.ci.ci.LL.0.95, x$gain.ci.ci.UL.0.95),
    sprintf("[%.2f, %.2f]", x$dav.ci.ci.LL.0.95, x$dav.ci.ci.UL.0.95)
  )
  
  p = sprintf("%.3f", x$gain.res.pval)
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
  
  # Strip list of lists and yield a list with 'do.call' then create a data frame
  # using 'rbind'
  
  final = do.call(rbind, res2)
 
  # If sav2csv = TRUE we run the following scripts
   
  if(sav2csv == TRUE){
    
    dv = rownames(final)
    temp = cbind(dv, final)
    rownames(temp) = NULL
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    write.table(temp,
                file = dir,
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
    
  }
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))
  
}

#-------------------------------------------------------------------------------

mixed.mod <- function(dv, 
                      covar = NULL, 
                      long.dataset, 
                      transf = c("none","log"),
                      silent = FALSE){

  # This function conducts a linear mixed model fit by REML using lmerTest::lmer
  # on the aerobic economy variable specified by 'dv' and stored in 'long.dataset'.
  # The function allows for covariates if required, and a transformation of 'none'
  # or 'log' may be applied to the data.
  #
  # dv: dependent variable defined as a character string
  # long.dataset: data frame containing the variables in long format
  # transf: apply no transformation 'none' or a log transformation 'log' to the 
  # data.
  # silent: A logical indicating whether to return print output (FALSE) or 
  # suppress print output (TRUE)
  #
  # Example use:
  # Perform a linear mixed model fit by REML for vo2.dv
  # mixed.mod(dv = "vo2.dv", long.dataset = dat.econ.long, transf = "none")
  #
  # Begin script
    
  # Define 'dat' as our dataset data frame and create a new variable for the 
  # dv of interest and covariate of interest

  dat = long.dataset
  dat$dv = get(dv, dat)
  
  if(is.null(covar) == FALSE){dat$covar = get(covar, dat)}
  
  # Set Type III SS before running lmer

  options(contrasts=c("contr.sum","contr.poly"))
  
  # Run the following model if no covariate is specified
  

  if(is.null(covar) == TRUE){
    
  if(transf == "none"){model <- lmerTest::lmer(dv ~ time*samp + (1|id), data = dat)}
  if(transf == "log"){model <- lmerTest::lmer(log(dv) ~ time*samp + (1|id), data = dat)}

  }
    
  # Run the following model if a covariate is specified
  
  if(!is.null(covar) == TRUE){
    
    if(transf == "none"){model <- lmerTest::lmer(dv ~ time*samp + covar + (1|id), data = dat)}
    if(transf == "log"){model <- lmerTest::lmer(log(dv) ~ time*samp + log(covar) + (1|id), data = dat)}

  }
  
  # Return model summary, model anova, model comparison, and model variance
  
  res1 = summary(model)
  res2 = anova(model)
  res3 = ranova(model)
  rfx <- as.data.frame(VarCorr(model))
  
  # If silent = FALSE print the following results
  
  if(silent == FALSE){
    
    print(list(model = model, 
               summary = res1, 
               anova = res2,
               rfx = rfx,
               delete.rand.eff = res3))
    
  }
  
  
  return(invisible(list(model = model, 
                        summary = res1, 
                        anova = res2,
                        rfx = rfx,
                        delete.rand.eff = res3,
                        dat = dat)))
}

#-------------------------------------------------------------------------------

mixed.mod.res <- function(dvs, 
                          covars = NULL, 
                          long.dataset, 
                          transf = c("none", "log"), 
                          sav2csv = TRUE,
                          sav2dir,
                          silent = FALSE){
  
  # This function will run the mixed.mod function for all variables names stored
  # in dvs from long.dataset. The script will save the ANOVA results to a csv 
  # file.
  #
  # dvs: vector of dependent variable names as character strings
  # covars: vector of covariate variable names as character strings. Must match
  # the same order as the dvs vector for which they are covariates for. 
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results to a csv file
  # sav2dir: the directory location relative to the current directory. To save in
  # the sub directory 'results' and sub-sub directory 'tables', set sav2dir as:
  # c("results", "tables", "csvname.csv"), where 'csvname.csv' is the name of 
  # the csv file that this function generates.
  # silent: A logical indicating whether you want the function to return
  # print output or suppress print output.
  #
  # Example use:
  # Run mixed models for all aerobic economy variables
  # mixed.mod.res(dvs = c("vo2.dv",
  #                       "pcnt.vo2.dv",
  #                       "hr.dv",
  #                       "rpe.dv"), 
  #               long.dataset = dat.econ.long, transf = "none", 
  #               sav2csv = TRUE, 
  #               sav2dir = c("results", "mixed_model_data", "mixed_mod_res.csv"))
  #
  # Begin script

  # Compute all mixed model results and store this in a list
  # Run the following script if there are no covariates
  
  if(is.null(covars)){
  
  res <- lapply(dvs, function(i){
    mod = mixed.mod(dv = i, long.dataset = long.dataset, transf = transf, silent = TRUE)$anova
  })
  
  }
  
  # Run the following script if there are covariates
  
  if(!is.null(covars)){
  
  res <- mapply(function(i,j){
    mod = mixed.mod(dvs = i, covar = j, long.dataset = long.dataset, transf = transf, silent = TRUE)$anova
  }, dvs, covars, SIMPLIFY = FALSE)
  
  }
  
  # Name each element of list with correspond dependent variable name
  
  names(res) = dvs
  
  # If sav2csv = TRUE we run the following scripts
  # First create a new csv file with the headings defined by 'col'.
  # Then append each element of the 'res' list to this csv file
  
  if(sav2csv == TRUE){
    
    cols = cbind("dv", "factor","Sum Sq","Mean Sq","NumDF","DenDF","F value","Pr(>F)")
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    write.table(cols,
                file = dir,
                append = F,
                row.names = F,
                col.names = F,
                sep=",")
    
    for(j in 1:length(res)){
    
    temp = as.data.frame(res[j])
    dv = rep(dvs[j], nrow(temp))
    factor = rownames(temp)
    temp = cbind(dv,factor,temp)
      
    write.table(temp,
                file = dir,
                append = T,
                row.names = F,
                col.names = F,
                sep=",")
    
  }
    
  }
  
  # If silent = FALSE print res
  
  if(silent == FALSE){
    print(res)
  }
  
  return(invisible(res))
}

#-------------------------------------------------------------------------------

mixed.assum <- function(dv,
                        covar = NULL,
                        long.dataset,
                        transf = c("none", "log"),
                        print.plots = FALSE,
                        silent = FALSE){
  
  # This function generates plots to test all assumptions for linear mixed models
  # computed on the dv data. It also identifies potential participant-level and
  # response-level outliers through Cooks distance.
  #
  # dv: dependent variable defined as a character string
  # long.dataset: data frame containing the variables in long format
  # transf: apply no transformation 'none' or a log transformation 'log' to the 
  # data.
  # silent: A logical indicating whether to return print output (FALSE) or 
  # suppress print output (TRUE)
  #
  # Example use:
  # Check assumptions for mixed model fit for vo2.dv
  # mixed.assum(dv = "vo2.dv", long.dataset = dat.econ.long, transf = "none")
  #
  # Begin script
  
  # Run the mixed.mod function and save the model and dataset
  
  s1 = mixed.mod(dv = dv, 
                 covar = covar, 
                 long.dataset = long.dataset, 
                 transf = transf, 
                 silent = TRUE)

  model = s1$model
  dat = s1$dat

  # Check assumptions of linearity and homoskedasticity

  homosk <- 
    ggplot() +
    geom_point(aes(x = fitted(model),y = residuals(model))) +
    geom_smooth(aes(x = fitted(model),y = residuals(model)), se = F, method = "loess", color ="black")+
    geom_smooth(aes(x = fitted(model),y = residuals(model)), se = T, method = "lm")+
    geom_hline(yintercept = 0, linetype=2)+
    labs(x = "Fitted Values", y = "Residuals")+
    ggtitle(paste0(dv, " (",transf,"-transformation)")) +
    theme_light()
  
  # Save plot as png file
  
    plot1name = paste0(dv,"_1","_homosk.png")
    ggsave(here("plots", "mixed_model_plots","assump_plots", plot1name), plot = homosk, width = 5, height = 5, units = "in")
    
    if(print.plots == TRUE){print(homosk)}
    
  # Check for normality using a QQ Plot
  
  ggqq = userfriendlyscience::ggqq(resid(model))
  ggqq = ggqq + ggtitle(paste0(dv, " (",transf,"-transformation)"))
  
  
  # Save png file

    plot2name = paste0(dv,"_2","_qqplot.png")
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot2name), plot = ggqq, width = 5, height = 5, units = "in")
  
    if(print.plots == TRUE){print(ggqq)}
  
  # Check for for outlier participants using criterion of Cooks Distance > 1
  # cooks.distance() function is modified by HLMdiag package
  
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
    ggtitle(paste0(dv, " (",transf,"-transformation)"))+
    theme_light()
  
  # Save plot as png file
  
    plot3name = paste0(dv,"_3","_cook_id.png")
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot3name), plot = cook_id, width = 5, height = 5, units = "in")
  
    if(print.plots == TRUE){print(cook_id)}
    
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
    ggtitle(paste0(dv, " (",transf,"-transformation)"))+
    theme_light()
  
  # Save plot as png file

    plot4name = paste0(dv,"_4","_cook_dv.png")
    ggsave(here("plots", "mixed_model_plots", "assump_plots", plot4name), plot = cook_dv, width = 5, height = 5, units = "in")

    if(print.plots == TRUE){print(cook_dv)}
  
  mu.cook_dv = mean(vals_dv$cooksd_dv)
  min.cook_dv = min(vals_dv$cooksd_dv)
  max.cook_dv = max(vals_dv$cooksd_dv)
  
  extrdv.1 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[1])
  extrdv.1.pos <- vals_dv[extrdv.1,]
  
  extrdv.2 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[2])
  extrdv.2.pos <- vals_dv[extrdv.2,]
  
  extrdv.3 <- which(vals_dv$cooksd_dv %in% sort(vals_dv$cooksd_dv,TRUE)[3])
  extrdv.3.pos <- vals_dv[extrdv.3,]
  
  final = list(extreme.id.1 = extrid.1.pos,
               extreme.id.2 = extrid.2.pos,
               extreme.id.3 = extrid.3.pos,
               extreme.dv.1 = extrdv.1.pos,
               extreme.dv.2 = extrdv.2.pos,
               extreme.dv.3 = extrdv.3.pos)
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))
}

#-------------------------------------------------------------------------------

mixed.assum.res <- function(dvs, 
                            covars = NULL, 
                            long.dataset, 
                            transf = c("none","log"),
                            silent = FALSE){

  # This function runs the mixed.assum function for all variables listed in dvs
  # and stored in long.dataset. 
  #
  # dvs: vector of dependent variables defined as character strings
  # covars: vector of covariate variable names as character strings. Must match
  # the same order as the dvs vector for which they are covariates for. 
  # long.dataset: data frame containing the variables in long format
  # transf: apply no transformation 'none' or a log transformation 'log' to the 
  # data.
  # silent: A logical indicating whether to return print output (FALSE) or 
  # suppress print output (TRUE)
  #
  # Example use:
  # Check assumptions for all variables in dvs
  # mixed.assum.res(dvs = c("vo2.dv",
  #                         "pcnt.vo2.dv",
  #                         "hr.dv",
  #                         "rpe.dv"), 
  #                 long.dataset = dat.econ.long, 
  #                 transf = "none")
  #
  # Begin script
  
  # Computes all mixed model assumption plots
  # Run the following scripts if covar = NULL
  
  if(is.null(covar)){
  
  res <- sapply(dvs, function(i){
    mixed.assum(dv = i, 
                covar = NULL, 
                long.dataset = long.dataset, 
                transf = transf, 
                silent = TRUE, 
                print.plots = FALSE)
  })
  
  res = as.data.frame(apply(res, 2, unlist))
  res = round(res, 2)
  
  if(silent == FALSE){
    print(res)
  }
  
  return(invisible(res))
  
  }
  
  # Run the following scripts if covar is specified
  
  if(!is.null(covar)){
    
    res <- mapply(function(i,j){
      mixed.assum(dv = i, 
                  covar = j, 
                  long.dataset = long.dataset,
                  transf = transf,
                  silent = TRUE)}, 
      dvs, covars)
    
    res = as.data.frame(apply(res, 2, unlist))
    res = round(res, 2)
    
    if(silent == FALSE){
      print(res)
    }
    
    return(invisible(res))
  }
  
}


#-------------------------------------------------------------------------------

emm.test <- function(dv, 
                     covar = NULL,
                     long.dataset, 
                     effect = c("time","samp","timeXsamp"),
                     plots = FALSE,
                     plot.wXh = c(6.3, 4.5),
                     silent = FALSE){

  # This function computes estimated marginal means for main effects or follow-up
  # to interaction effects. Also generates a time-by-sample plot of the data. 
  # The 'transf' option was disabled because assumptions of data for each dv 
  # were adequate using no transformation of data.
  #
  # dv: dependent variable string name
  # covar: covariate string name
  # long.dataset: data frame which contains the dv and covariate in long format
  # effect: perform emmeans on main effect of time, sample, or their interaction
  # plots: a logical indicating whether to generate plots
  # plot.wXh: width and height dimensions for the plot (inches)
  # silent: a logical indicating whether to return print output
  #
  # Example use:
  # emm.test(dv = "vo2.dv", long.dataset = dat.econ.long, effect = "time", plots = TRUE)
    
  # Run the mixed.mod function and save the model and dataset
  
  s1 = mixed.mod(dv = dv, 
                 covar = covar, 
                 long.dataset = long.dataset, 
                 transf = "none", 
                 silent = TRUE)
  
  model = s1$model
  dat = s1$dat
  
  # Compute main effect with Satterthwaite df method

  if(effect != "timeXsamp"){
  
  emm = emmeans::emmeans(model,effect,lmer.df = "satterthwaite")
  emm.diff = pairs(emm, type="response", reverse=TRUE)

  # Inspect effect at multiple confidence intervals from 0.75 to 0.95
  
  conf.check =
    lapply(seq(0.75,0.95,0.05), function(i){
      res = stats::confint(emm.diff, adjust = "none", level = c(i),reverse=TRUE)
      ci = res[5:6]
      cbind(ci)
    })

  names(conf.check) = paste0("ci.",seq(0.75,0.95,0.05))
  
  conf.check = as.data.frame(conf.check)
  emm.diff = as.data.frame(emm.diff)
  emm.diff = cbind(emm.diff, conf.check)
  
  }
  
  # Compute interaction effect with Satterthwaite df method
  
  emm.int = emmeans::emmeans(model, pairwise~time | samp)
  emm.int.contrasts = pairs(emm.int$emmeans, reverse=TRUE)

  # Inspect interaction T1-T0 effects at multiple confidence intervals from 0.75 to 0.95
  
  conf.check.int =
    lapply(seq(0.75,0.95,0.05), function(i){
      res = stats::confint(emm.int.contrasts, adjust = "none", level = i,reverse=TRUE)
      ci = res[6:7]
      ci
    })
  
  names(conf.check.int) = paste0("ci.",seq(0.75,0.95,0.05))
  
  conf.check.int= as.data.frame(conf.check.int)
  emm.int.contrasts = as.data.frame(emm.int.contrasts)
  emm.int.contrasts = cbind(emm.int.contrasts,conf.check.int)

  # Plot the results

  if(plots == TRUE){
  
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
      xlab("Velocity (km/h)") + ylab(dv) + scale_x_discrete(labels=c(6,8,10,12)) +
      ggtitle(dv) +
      theme_light() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    )
    
    print(emm.plot)
    
    # Save plot as eps file
    
    sapply(c(".eps",".png"), function(i){
      plotname = paste0(dv,"_lmm",i)
      
      ggsave(here::here("plots", "mixed_model_plots", "mixed_model_res", plotname), 
             plot = emm.plot, 
             width = plot.wXh[1], 
             height = plot.wXh[2], 
             units = "in")
    })
    
    }

  if(effect == "time" || effect == "samp"){
    
    final = list(emmeans = as.data.frame(emm), contrasts = emm.diff)
    
    if(silent == FALSE){print(final)}
    
    return(invisible(final))
    
    }
  
  if(effect== "timeXsamp"){
    
    final = list(emmeans = as.data.frame(emm.int$emmeans), contrasts = emm.int.contrasts)
    
    if(silent == FALSE){print(final)}
    
    return(invisible(final))
  
  }
  }

#-------------------------------------------------------------------------------

emm.test.res <- function(dvs, 
                         covars = NULL, 
                         long.dataset, 
                         effect = c("time", "samp", "timXsamp"), 
                         plots = FALSE, 
                         plot.wXh = c(6.3, 4.5),
                         sav2csv = FALSE,
                         silent = FALSE){

  # This function applies all variables in dvs to the emm.test function and
  # returns emmean results and contrast results as two data frames. If specified,
  # it will save each of these data frames to two separate csv files. Transf has
  # been disabled. 
  #
  # dvs: vector of dependent variables defined as character strings
  # covars: vector of covariate variable names as character strings. Must match
  # the same order as the dvs vector for which they are covariates for. 
  # long.dataset: data frame containing the variables in long format
  # plots: a logical indicating whether to generate plots
  # plot.wXh: width and height dimensions for the plot (inches)
  # sav2csv: a logical indicating whether to save results as csv files
  # silent: a logical indicating whether to return print output
  #
  # Example use:
  # Apply emm.test to all variables in dvs
  # emm.test.res(dvs = c("vo2.dv",
  #                      "pcnt.vo2.dv",
  #                      "hr.dv",
  #                      "rpe.dv"), 
  #              long.dataset = dat.econ.long, 
  #              effect = "time", 
  #              sav2csv = TRUE, 
  #              plots = TRUE)
  #
  # Begin script
  
  # Apply emm.test to all dvs variables and save as a matrix in res
  
  # Do the following if there is no covariate
  
  if(is.null(covars)){
  
  res = sapply(dvs, function(i){
    
    test = emm.test(dv = i, 
                    covar = NULL,
                    long.dataset = long.dataset, 
                    effect = effect, 
                    plots = plots, 
                    plot.wXh = plot.wXh, 
                    silent = TRUE)
  })
  
  }
  
  # Do the following if there is a covariate
  
  if(!is.null(covars)){
    
    res = mapply(function(i, j){
      
      test = emm.test(dv = i, 
                      covar = j,
                      long.dataset = long.dataset, 
                      effect = effect, 
                      plots = plots, 
                      plot.wXh = plot.wXh, 
                      silent = TRUE)
      
    }, dvs, covars, SIMPLIFY = FALSE)
    
  }
  
  # Transpose the res matrix
  
  res = t(res)
  
  # Define 'means' as the 'emmeans' data
  # Strip the list of lists with 'do.call' and create a data frame with 'rbind'
  
  means = res[,"emmeans"]
  means = do.call(rbind, means)
  
  # Define 'contrasts' as the 'contrasts' data
  # Strip the list of lists with 'do.call' and create a data frame with 'rbind'
  
  contrasts = res[,"contrasts"]
  contrasts = do.call(rbind, contrasts)
  
  # If sav2csv = TRUE we save the means and contrasts data into separate csv files
  
  if(sav2csv == TRUE){
  
  # Save emmeans
  
  temp = cbind("dv" = row.names(means), means)
  row.names(temp) = NULL
  name = paste("emmeans", effect, "res.csv", sep = "_")
    
  write.table(temp,
              file = here::here("results", "mixed_model_data", name),
              append = F,
              row.names = F,
              col.names = T,
              sep = ",")
  
  # Save contrasts
  
  temp = cbind("dv" = row.names(contrasts), contrasts)
  row.names(temp) = NULL
  name = paste("contrasts", effect, "res.csv", sep = "_")
    
  write.table(temp,
              file = here::here("results", "mixed_model_data", name),
              append = F,
              row.names = F,
              col.names = T,
              sep = ",")
    
  }
  
  final = list(
    emmeans = means,
    contrasts = contrasts
  )
  
  # Print output if silent = FALSE
  
  if(silent == FALSE){
    print(final)
  }
    
  return(invisible(final))
  
}

#-------------------------------------------------------------------------------

lmm.res <- function(dvs,
                    long.dataset,
                    sav2csv = FALSE,
                    sav2dir,
                    silent = FALSE){
  
  # A lazy function that will return the summarized Type III ANOVA table and
  # follow-up pairewise contrasts of time for the linear mixed model for each
  # aerobic economy outcome
  #
  # dvs: vector of dependent variables defined as character strings
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results as csv files
  # sav2dir: the directory location relative to the current directory.
  # silent: a logical indicating whether to return print output
  #
  # Example use:
  # lmm.res(dvs = c("vo2.dv",
  #                 "pcnt.vo2.dv",
  #                 "hr.dv",
  #                 "rpe.dv"), 
  #         long.dataset = dat.econ.long, 
  #         sav2csv = TRUE, 
  #         sav2dir = c("results", "tables", "table4.csv"))
  #
  # Begin script
  
  ffx = 
    mixed.mod.res(dvs = dvs, 
                  covars = NULL, 
                  long.dataset = long.dataset, 
                  transf = "none", 
                  sav2csv = FALSE,
                  silent = TRUE)
  
  ffx.vo2 = as.data.frame(ffx$vo2.dv)
  ffx.pcnt.vo2 = as.data.frame(ffx$pcnt.vo2.dv)
  ffx.hr = as.data.frame(ffx$hr.dv)
  ffx.rpe = as.data.frame(ffx$rpe)
    
  dothis <- function(ffx.var, name){
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
      dothis(ffx.vo2, "vo2"),
      dothis(ffx.pcnt.vo2, "pcnt.vo2"),
      dothis(ffx.hr, "hr"),
      dothis(ffx.rpe, "rpe"))
   
  # Extract emmeans for time (T1-T0)
  
  emm = 
    emm.test.res(dvs = dvs, 
                 long.dataset = long.dataset, 
                 covars = NULL, 
                 effect = "time", 
                 plots = FALSE, 
                 sav2csv = FALSE, 
                 silent = TRUE)
  
  emm.vo2 = emm$contrasts["vo2.dv",]
  emm.pcnt.vo2 = emm$contrasts["pcnt.vo2.dv",]
  emm.hr = emm$contrasts["hr.dv",]
  emm.rpe = emm$contrasts["rpe.dv",]
      
  andthis <- function(emm.var, name){
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
      andthis(emm.vo2, "vo2"),
      andthis(emm.pcnt.vo2, "pcnt.vo2"),
      andthis(emm.hr, "hr"),
      andthis(emm.rpe, "rpe")
    )
  
  final = 
    as.data.frame(rbind(
      res[c(1:3),],res2[1,],
      res[c(4:6),],res2[2,],
      res[c(7:9),],res2[3,],
      res[c(10:12),],res2[4,]))
  
  if(sav2csv == TRUE){
    
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    write.table(final,
                file = dir,
                append = F,
                row.names = T,
                col.names = T,
                sep=",")
  }
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))

}

#-------------------------------------------------------------------------------

lmm.supp.res <- function(dvs, 
                         long.dataset, 
                         sav2csv = FALSE, 
                         sav2dir, 
                         silent = FALSE){
  
  # A lazy function that will return the follow-up contrasts of time for each 
  # lmm with confidence limits ranging from 80-95%. 
  #
  # dvs: vector of dependent variables defined as character strings
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results as csv files
  # sav2dir: the directory location relative to the current directory.
  # silent: a logical indicating whether to return print output
  #
  # Example use:
  # lmm.supp.res(dvs = c("vo2.dv",
  #                      "pcnt.vo2.dv",
  #                      "hr.dv",
  #                      "rpe.dv"), 
  #              long.dataset = dat.econ.long, 
  #              sav2csv = TRUE, 
  #              sav2dir = c("results", "tables", "supp_table2.csv"))
  #
  # Begin script
  
  # Extract emmeans for time (T1-T0)
  
  emm = 
    emm.test.res(dvs = dvs, 
                 long.dataset = long.dataset, 
                 covars = NULL, 
                 effect = "time", 
                 plots = FALSE, 
                 sav2csv = FALSE, 
                 silent = TRUE)
  
  emm.vo2 = emm$contrasts["vo2.dv",]
  emm.pcnt.vo2 = emm$contrasts["pcnt.vo2.dv",]
  emm.hr = emm$contrasts["hr.dv",]
  emm.rpe = emm$contrasts["rpe.dv",]
  
  dothis <- function(emm.var, name){
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
  
  pcnt.vo2.int = 
    emm.test.res(dv = "pcnt.vo2.dv", 
                 long.dataset = long.dataset, 
                 effect = "timeXsamp", 
                 plots = FALSE, 
                 sav2csv = FALSE, 
                 silent = TRUE)$contrasts
  
  alsothis <- function(emm.var.int, name){
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
      dothis(emm.vo2, "vo2.time"),
      dothis(emm.pcnt.vo2, "pcnt.vo2.time"),
      alsothis(pcnt.vo2.int, "pcnt.vo2.int"),
      dothis(emm.hr, "hr.time"),
      dothis(emm.rpe, "rpe.time")
    )
  
  if(sav2csv == TRUE){
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    write.table(res2,
                file = dir,
                append = F,
                row.names = F,
                col.names = T,
                sep=",")
  }
  
  res2 = as.data.frame(res2)
  
  if(silent == FALSE){
    print(res2)
  }
  
  return(invisible(res2))
  
}

#-------------------------------------------------------------------------------

supp.model <- function(dv,                          
                       long.dataset, 
                       sav2csv = FALSE, 
                       sav2dir, 
                       silent = FALSE){
 
  # This function will report model comparison results, beta estimates, and model 
  # variance for the LMM specified by 'dv' in an APA-like table. 
  #
  # dv: dependent variable name as string
  # long.dataset: data frame containing the variables in long format
  # sav2csv: a logical indicating whether to save results as csv files
  # sav2dir: the directory location relative to the current directory.
  # silent: a logical indicating whether to return print output
  #
  # Example use:
  # supp.model(dv = "vo2.dv", 
  #            long.dataset = dat.econ.long, 
  #            sav2csv = TRUE, 
  #            sav2dir = c("results", "tables", "supp_table3.csv"))
  #
  # Begin script
   
  x = mixed.mod(dv = dv, 
                long.dataset = long.dataset, 
                transf = "none", 
                silent = TRUE)
  
  dothis <- function(ffx.var){
    
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
  
  res = dothis(x)
  
  if(sav2csv == TRUE){
    
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    write.table(NULL, 
                file = dir)
    
    sapply(names(res), function(i){
      write.table(res[i],
                  file = dir,
                  append = T,
                  row.names = T,
                  col.names = F,
                  sep=",")
    })

    
  }
  
  if(silent == FALSE){
    print(res)
  }
  
  return(invisible(res))
  
  
}

#-------------------------------------------------------------------------------

safeguard.es <- function(dv, 
                         long.dataset, 
                         type = c("dz","dav"), 
                         silent = FALSE){

  # This function computes a safeguard effect size for paired data based on
  # Cohen's dz or Cohen's dav. The safeguard effect size is the lower limit of 
  # a 1-sided 80% confidence interval. For completement, the function reports 
  # the lower limits of 1-sided 80-95% confidence intervals. 
  #
  # dv: dependent variable name as string
  # long.dataset: data frame containing the variables in long format
  # type: one of 'dz' or 'dav' to specify which effect the safeguard effect size
  # is computed for
  # silent: a logical indicating whether to print or suppress print output
  #
  # Example use:
  # safeguard.es(dv = "pss", long.dataset = dat.long, type = "dz", silent = TRUE)$ci.LL_0.80

  # Reshape long dataset into wide dataset
  
  dat.wide <- reshape(long.dataset, idvar = "id", timevar = "time", direction = "wide")
  
  # Define pre (t0) and post (t1) variables
  
  x = paste0(dv,".0")
  y = paste0(dv,".1")
  
  t0 = get(x, dat.wide)
  t1 = get(y, dat.wide)
  
  # Define a function to compute descriptive and inferential statistics for
  # the t1-t0 gain score data
  
  gain <- function(i,j){
    ch = i-j
    n = length(ch)
    r = stats::cor(i, j, method = "pearson")
    mu = mean(ch)
    sdev = sd(ch)
    sdev.av = sqrt((sd(i)^2+sd(j)^2)/2)
    min = min(ch)
    max = max(ch)
    test = stats::t.test(ch, mu = 0)
    tval = as.numeric(test$statistic)
    df= as.numeric(test$parameter)
    pval = as.numeric(test$p.value)
    dz = mu/sdev
    dav = mu/sdev.av
    cl.ES = 1 - pnorm(mu/sdev)
    r.pval = trimpb(x = i-j, tr = 0.2, nboot = 2000, pr = FALSE)$p.value
    
    return(list(n = n, 
                r = r, 
                mu = mu, 
                sdev = sdev, 
                sdev.av = sdev.av, 
                min = min, 
                max = max,
                tval = tval,
                df = df,
                pval = pval,
                r.pval = r.pval,
                dz = dz,
                dav = dav,
                cl.ES = cl.ES))
    
  }
  
  gain.res = gain(t1,t0)
  
  # Define a function to compute 80-95% 1-sided confidence limits on dz and dav
  
  dci <- function(d, n, tval, df, sdev, sdev.av, type = c("dz","dav")){
    
    if(type == "dz"){
      
      res1 = 
        sapply(seq(0.80, 0.95, 0.05), function(i){
          conf = i*2-1
          res = quiet(as.numeric(MBESS::ci.sm(sm = d, N = n, conf.level = conf))[c(1,3)])
          res[which.min(abs(res))]
        })
      
      res2 = as.list(res1)
      names = format(seq(0.80, 0.95, 0.05), nsmall = 2)
      names(res2) = paste("ci.LL", names, sep = "_")
      return(res2)
      
    }
    
    if(type == "dav"){
      
      res1 =
        sapply(seq(0.80,0.95,0.05), function(i){
          conf = i*2-1
          limits <- quiet(MBESS::conf.limits.nct(df = df, t.value = tval, conf.level = conf))
          ci.LL <- (limits$Lower.Limit*sdev)/(sdev.av*sqrt(n))
          ci.UL <- (limits$Upper.Limit*sdev)/(sdev.av*sqrt(n))
          res = c(ci.LL, ci.UL)
          res[which.min(abs(res))]
        })
      
      res2 = as.list(res1)
      names = format(seq(0.80, 0.95, 0.05), nsmall = 2)
      names(res2) = paste("ci.LL", names, sep = "_")
      return(res2)
      
    }
  }
  
  # Compute 1-sided 80-95% confidence limits on dz and dav
  
  dav.ci = dci(d = gain.res$dav, 
               n = gain.res$n, 
               tval = gain.res$tval, 
               df = gain.res$df, 
               sdev = gain.res$sdev, 
               sdev.av = gain.res$sdev.av, 
               type = "dav")
  
  dz.ci = dci(d = gain.res$dz,
              n = gain.res$n, 
              type = "dz")
  
  
  if(type == "dz"){final = dz.ci}
  if(type == "dav"){final = dav.ci}
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))
  
}

#-------------------------------------------------------------------------------

aipe.d <- function(d, 
                   moe = abs(d/2), 
                   type = c("dz","ds"),
                   conf.level = 0.95, 
                   assurance = 0.99, 
                   retention.rate, 
                   silent = FALSE){
  
  if(type == "dz"){
    n = MBESS::ss.aipe.sm(sm = abs(d),
                          width = moe*2, 
                          conf.level = conf.level, 
                          assurance = assurance)
  }
  
  if(type == "ds"){
    n = MBESS::ss.aipe.smd(delta = d, 
                           width = moe*2, 
                           conf.level = conf.level,
                           assurance = assurance)
    
  }
  
  n.adj = ceiling(n/retention.rate)
  
  final = 
    cbind(d = d,
         moe = moe,
         n = n,
         n.adj = n.adj)
  
  rownames(final) = type
  
  if(silent == FALSE){
    print(final)
  }
  
  return(invisible(final))
  
}

#-------------------------------------------------------------------------------

aipe.mult.d <- function(dvals,                    
                        type = c("dz","ds"),
                        conf.level = 0.95, 
                        assurance = 0.99, 
                        retention.rate, 
                        return.order.d = "descending",
                        silent = FALSE){
  
  if(return.order.d == "ascending"){decreasing = FALSE}
  if(return.order.d == "descending"){decreasing = TRUE}
  
  dvals = dvals[order(dvals, decreasing = decreasing)]
  
  res = 
  sapply(dvals, function(i){
    
    aipe.d(d = i, 
           type = type, 
           conf.level = conf.level, 
           assurance = assurance, 
           retention.rate = retention.rate, 
           silent = TRUE)
    
  })
  
  res = as.data.frame(t(res))
  colnames(res) = c("d", "moe", "n", "n.adj")
  names = paste0(type, 1:length(res$d))
  rownames(res) = names
  
  if(silent == FALSE){
    print(res)
  }
  
  return(invisible(res))
  
}

#-------------------------------------------------------------------------------

aipe.table <- function(retention.rate = 0.71, 
                       n.trial = 10, 
                       max.per.trial = 20, 
                       sav2csv = FALSE, 
                       sav2dir){
  
  res1 = 
  aipe.d(d = -0.6, 
         type = "dz",
         conf.level = 0.95, 
         assurance = 0.99, 
         retention.rate = retention.rate, 
         silent = TRUE)
  
  res2 = 
  aipe.d(d = -0.37, 
         type = "dz",
         conf.level = 0.95, 
         assurance = 0.99, 
         retention.rate = retention.rate, 
         silent = TRUE)
  
  s1 = as.data.frame(rbind(res1, res2))
  
  s2 = 
    aipe.mult.d(dvals = seq(0.2, 0.8, 0.1), 
                type = "ds", 
                conf.level = 0.95, 
                assurance = 0.99, 
                return.order.d = "descending",
                retention.rate = retention.rate, 
                silent = TRUE)
  
  dothis <- function(i, type = c("paired", "two.sample")){
    
    pow.n = 
      sapply(i$d, function(j){
        n = pwr::pwr.t.test(d = j, sig.level = 0.05, power = 0.8, type = type)$n
        n = ceiling(n)
        })
    
    pow.n.adj = pow.n/retention.rate
    
    req.pr = ceiling(i$n.adj/n.trial)
    feas.pr = req.pr <= max.per.trial
    
    req.po = ceiling(pow.n.adj/n.trial)
    feas.po = req.po <= max.per.trial
    
    final = as.data.frame(
      cbind(req.pr, feas.pr, pow.n, pow.n.adj, req.po, feas.po))
    
    return(final)
    
  }

  temp1 = rbind(s1,s2)
  temp2 = rbind(dothis(s1, "paired"), dothis(s2, "two.sample"))
  temp3 = cbind(temp1, temp2)
  temp3$feas = temp3$feas.pr + temp3$feas.po
  temp3$feas[temp3$feas == 2] <- "Yes"
  temp3$feas[temp3$feas == 1] <- "*Yes"
  temp3$feas[temp3$feas == 0] <- "No"

  final = temp3[c(1:5,11)]
  
  if(sav2csv == TRUE){
    
    temp = cbind("type" = rownames(final), final)
    rownames(temp) = NULL
    sav2dir = paste0(sav2dir, collapse = "/")
    dir = paste(here::here(), sav2dir, sep = "/")
    
    utils::write.table(temp,
                       file = dir,
                       append = F,
                       row.names = F,
                       col.names = T,
                       sep=",")
    
  }
  
 return(final)
  
}
  
#-------------------------------------------------------------------------------
# End script
#-------------------------------------------------------------------------------