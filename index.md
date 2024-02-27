---
title: "Meta analysis of the relationship between sleep and concussion in students"
author: "Yizhou Yu"
date: "updated: <i>Feb-26-2024</i></h4>"
output:
  html_document:
    df_print: paged
    keep_md: yes
---

## Main analysis


```r
library(metafor)
```

```
## Loading required package: Matrix
```

```
## Loading required package: metadat
```

```
## Loading required package: numDeriv
```

```
## 
## Loading the 'metafor' package (version 4.4-0). For an
## introduction to the package please type: help(metafor)
```

```r
library(ggplot2)
```
load data

```r
dt = read.csv("dt/filtered_dt_2024.csv")
dt$author_name = paste(dt$Author, dt$Date, sep = " et al., ")
dt
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["Author"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Title"],"name":[2],"type":["chr"],"align":["left"]},{"label":["Filter.3.reviewer"],"name":[3],"type":["chr"],"align":["left"]},{"label":["Data.Extractor"],"name":[4],"type":["chr"],"align":["left"]},{"label":["Notes"],"name":[5],"type":["chr"],"align":["left"]},{"label":["Date"],"name":[6],"type":["int"],"align":["right"]},{"label":["N_concussion"],"name":[7],"type":["int"],"align":["right"]},{"label":["N_controls"],"name":[8],"type":["int"],"align":["right"]},{"label":["PSQI_control"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["PSQI_control_sd"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["PSQI_concussion"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["PSQI_concussion_sd"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["Mean.Age.in.Controls..years."],"name":[13],"type":["dbl"],"align":["right"]},{"label":["Mean.Age.in.Concusssed..years."],"name":[14],"type":["dbl"],"align":["right"]},{"label":["Sex..Male.n.....control"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["Sex..Male.n.....concussed"],"name":[16],"type":["dbl"],"align":["right"]},{"label":["Sex.Female.n.....control"],"name":[17],"type":["dbl"],"align":["right"]},{"label":["Sex..Female.n.....concussed"],"name":[18],"type":["dbl"],"align":["right"]},{"label":["bias"],"name":[19],"type":["int"],"align":["right"]},{"label":["author_name"],"name":[20],"type":["chr"],"align":["left"]}],"data":[{"1":"Gosselin","2":"Sleep following sport-related concussions.","3":"Anna","4":"Anna","5":"includes  3 athletes with concussions more than 6 months ago but less than 1 year ago","6":"2008","7":"10","8":"11","9":"3.50","10":"2.30","11":"7.70","12":"3.80","13":"22.60","14":"24.30","15":"0.6363","16":"0.7000","17":"0.3637","18":"0.3000","19":"4","20":"Gosselin et al., 2008"},{"1":"Bone","2":"The Effects of Concussion on Quantity and Quality of Sleep in Football Athletes.","3":"Eszti","4":"Eszti","5":"it might be a thesis?","6":"2022","7":"20","8":"7","9":"5.30","10":"2.60","11":"10.57","12":"6.43","13":"19.57","14":"20.05","15":"1.0000","16":"1.0000","17":"0.0000","18":"0.0000","19":"1","20":"Bone et al., 2022"},{"1":"Wilson","2":"Adolescents report sleep quality impairments acutely postconcussion","3":"Eszti","4":"Eszti","5":"I think this is good","6":"2020","7":"17","8":"51","9":"4.30","10":"2.64","11":"6.77","12":"3.47","13":"16.00","14":"16.00","15":"0.3500","16":"0.3500","17":"0.6500","18":"0.6500","19":"3","20":"Wilson et al., 2020"},{"1":"Hoffman","2":"Differences in sleep between concussed and nonconcussed college students: a matched case-control study.","3":"Eszti","4":"Eszti","5":"I think this is perfect","6":"2018","7":"20","8":"20","9":"4.20","10":"1.80","11":"7.80","12":"3.00","13":"20.04","14":"20.00","15":"0.4000","16":"0.8000","17":"0.6000","18":"0.6000","19":"4","20":"Hoffman et al., 2018"},{"1":"Albicini","2":"Ongoing daytime behavioural problems in university students following childhood mild traumatic brain injury","3":"Eszti","4":"Eszti","5":"not necessarily within 6 months of injury but control definetely good","6":"2016","7":"47","8":"197","9":"5.85","10":"2.73","11":"6.34","12":"3.61","13":"20.45","14":"20.36","15":"0.4210","16":"0.1800","17":"0.5530","18":"0.6170","19":"2","20":"Albicini et al., 2016"},{"1":"Schmidt","2":"A Longitudinal Investigation of Sleep Quality in Adolescents and Young Adults After Mild Traumatic Brain Injury","3":"Eszti","4":"Eszti","5":"looks perfect","6":"2016","7":"77","8":"43","9":"2.52","10":"3.48","11":"6.72","12":"4.00","13":"21.00","14":"20.40","15":"0.5814","16":"0.6753","17":"0.4186","18":"0.3247","19":"5","20":"Schmidt et al., 2016"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


## PSQI 

```r
smd_meta.psqi <- escalc(measure="SMD", 
                   m1i=PSQI_concussion, m2i=PSQI_control, 
                   sd1i=PSQI_concussion_sd, sd2i=PSQI_control_sd, 
                   n1i=N_concussion,
                   n2i=N_controls,
                   slab = author_name, data=dt)

reml.psqi = rma(yi,vi,method = "REML", data = smd_meta.psqi)
summary(reml.psqi)
```

```
## 
## Random-Effects Model (k = 6; tau^2 estimator: REML)
## 
##   logLik  deviance       AIC       BIC      AICc   
##  -3.5439    7.0879   11.0879   10.3067   17.0879   
## 
## tau^2 (estimated amount of total heterogeneity): 0.1749 (SE = 0.1712)
## tau (square root of estimated tau^2 value):      0.4182
## I^2 (total heterogeneity / total variability):   69.86%
## H^2 (total variability / sampling variability):  3.32
## 
## Test for Heterogeneity:
## Q(df = 5) = 20.6706, p-val = 0.0009
## 
## Model Results:
## 
## estimate      se    zval    pval   ci.lb   ci.ub      
##   0.8896  0.2138  4.1604  <.0001  0.4705  1.3087  *** 
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```
plot

```r
forest(reml.psqi)
text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
library(meta)
```

```
## Loading 'meta' package (version 7.0-0).
## Type 'help(meta)' for a brief overview.
## Readers of 'Meta-Analysis with R (Use R!)' should install
## older version of 'meta' package: https://tinyurl.com/dt4y5drs
```

```r
dt.metacont =  metacont(N_concussion, PSQI_concussion, PSQI_concussion_sd, 
                        N_controls, PSQI_control, PSQI_control_sd,
                        comb.fixed = T, comb.random = T, studlab = author_name,
                        data = dt, sm = "SMD") 


dt.metacont
```

```
## Number of studies: k = 6
## Number of observations: o = 520 (o.e = 191, o.c = 329)
## 
##                         SMD           95%-CI    z  p-value
## Common effect model  0.7021 [0.4963; 0.9079] 6.69 < 0.0001
## Random effects model 0.8878 [0.4679; 1.3077] 4.14 < 0.0001
## 
## Quantifying heterogeneity:
##  tau^2 = 0.1746 [0.0238; 1.1229]; tau = 0.4179 [0.1544; 1.0597]
##  I^2 = 75.6% [45.1%; 89.2%]; H = 2.03 [1.35; 3.04]
## 
## Test of heterogeneity:
##      Q d.f. p-value
##  20.51    5  0.0010
## 
## Details on meta-analytical method:
## - Inverse variance method
## - Restricted maximum-likelihood estimator for tau^2
## - Q-Profile method for confidence interval of tau^2 and tau
## - Hedges' g (bias corrected standardised mean difference; using exact formulae)
```
plot

```r
forest(dt.metacont, leftcols = c('studlab'))
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
funnel(dt.metacont)
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
dt_reformatted = dt[,c("author_name","PSQI_control","PSQI_control_sd","Mean.Age.in.Controls..years.","Sex.Female.n.....control","bias")]
colnames(dt_reformatted) <- c("author_name","PSQI","sd","age","female_ratio","bias")
dt_reformatted$concussed = FALSE
dt_reformatted_tmp = dt[,c("author_name","PSQI_concussion","PSQI_concussion_sd","Mean.Age.in.Concusssed..years.","Sex..Female.n.....concussed","bias")]
colnames(dt_reformatted_tmp) <- c("author_name","PSQI","sd","age","female_ratio","bias")
dt_reformatted_tmp$concussed = TRUE

dt_reformatted = cbind(dt_reformatted, dt_reformatted_tmp)
dt_reformatted$variance = dt_reformatted$sd^2

rma.da.adjusted <- rma(yi=PSQI, vi=variance, data = dt_reformatted, method = "DL",
                       mods = ~ age + female_ratio + bias)

rma.da.adjusted
```

```
## 
## Mixed-Effects Model (k = 6; tau^2 estimator: DL)
## 
## tau^2 (estimated amount of residual heterogeneity):     0 (SE = 5.6262)
## tau (square root of estimated tau^2 value):             0
## I^2 (residual heterogeneity / unaccounted variability): 0.00%
## H^2 (unaccounted variability / sampling variability):   1.00
## R^2 (amount of heterogeneity accounted for):            0.00%
## 
## Test for Residual Heterogeneity:
## QE(df = 2) = 0.0309, p-val = 0.9847
## 
## Test of Moderators (coefficients 2:4):
## QM(df = 3) = 0.8244, p-val = 0.8436
## 
## Model Results:
## 
##               estimate       se     zval    pval     ci.lb    ci.ub    
## intrcpt         3.0951  13.4020   0.2309  0.8174  -23.1725  29.3626    
## age             0.1593   0.6774   0.2351  0.8141   -1.1683   1.4868    
## female_ratio    2.9145   6.9498   0.4194  0.6750  -10.7070  16.5359    
## bias           -1.0138   1.2156  -0.8340  0.4043   -3.3964   1.3688    
## 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


```r
regplot(rma.da.adjusted, mod = "age", pi=TRUE)
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
pdf("fig/rma_adjusted_age.pdf", width = 4, height = 4)
```


```r
regplot(rma.da.adjusted, mod = "female_ratio")
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
pdf("fig/rma_adjusted_gender.pdf", width = 4, height = 4)
```


```r
regplot(rma.da.adjusted, mod = "bias", pi=TRUE)
```

![](sleepConcussion_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
