Hato Raton matrices merging
================
Elena Quintero
2021-05-11

``` r
library(tidyverse)
library(tibble)
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(ggpubr)
```

Load matrices

``` r
hr.mn <- read.csv("../Case_studies_datasets/hr_mn_clean.csv",header=T,sep=",",dec=".")
hr.obs <- read.csv("../Case_studies_datasets/hr_obs_clean.csv",header=T,sep=",",dec=".")
```

Convert all interactions in numeric

``` r
mn <- hr.mn%>%column_to_rownames(var="AASPECIES")
obs <- hr.obs%>%column_to_rownames(var="AASPECIES")
```

## Merging Methods

### 1. Qualitative Combination (QC)

``` r
# Sum up qualitative matrices
QS <- mn+obs
QC=QS
QC[QS!=0]=1
```

### 2. Quantitative Sum (QS)

``` r
# Sum up quantitative matrices
QS <- mn+obs
```

### 3. Grand Total Standardization (GTS)

``` r
# Grand total
mn.gts<- mn/sum(mn)
obs.gts<- obs/sum(obs)
GTS<- (mn.gts+obs.gts)/2
```

### 4. Min-Max Scaling (MMS)

``` r
# Min-Max scaling
mn.mms<- (mn-min(mn))/(max(mn)-min(mn))
obs.mms<- (obs-min(obs))/(max(obs)-min(obs))
MMS<- (mn.mms+obs.mms)/2
```

Save datasets:

``` r
write.csv(QC, file="HR_QC.csv")
write.csv(QS, file="HR_QS.csv")
write.csv(GTS, file="HR_GTS.csv")
write.csv(MMS, file="HR_MMS.csv")
```

### Data distribution for initial adjanjency matrix and merging methods

All data is log-transformed due to 0 inflated matrices
![](hr_matrices_merge_files/figure-gfm/histogram%20-%20distribution%20frequency-1.png)<!-- -->
