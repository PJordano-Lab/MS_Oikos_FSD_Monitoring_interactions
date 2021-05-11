*Pistacia lentiscus* matrices merging
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
pl.bc<- read.csv("../Case_studies_datasets/pl_bc_clean.csv",header=T,sep=",",dec=".")
pl.cam<- read.csv("../Case_studies_datasets/pl_cam_clean.csv",header=T,sep=",",dec=".")
```

Convert all interactions in numeric

``` r
bc <- as.data.frame(lapply(pl.bc, as.numeric))%>% 
    column_to_rownames(var="X")

cam <- as.data.frame(lapply(pl.cam, as.numeric)) %>% 
    column_to_rownames(var="X")
```

## Merging Methods

### 1. Qualitative Combination (QC)

``` r
# Sum up qualitative matrices
QS <- bc+cam
QC=QS
QC[QS!=0]=1
```

### 2. Quantitative Sum (QS)

``` r
# Sum up quantitative matrices
QS <- bc+cam
```

### 3. Grand Total Standardization (GTS)

``` r
# Grand total
bc.gts<- bc/sum(bc)
cam.gts<- cam/sum(cam)
GTS<- (bc.gts+cam.gts)/2
```

### 4. Min-Max Scaling (MMS)

``` r
# Min-Max scaling
bc.mms<- (bc-min(bc))/(max(bc)-min(bc))
cam.mms<- (cam-min(cam))/(max(cam)-min(cam))
MMS<- (bc.mms+cam.mms)/2
```

### 5. Sampling Effort Standardization (SES)

Load sampling effort:

``` r
bceff<- read.csv("../Case_studies_datasets/bc_sampling_effort.csv",header=T,sep=";",dec=".", comment.char = "#")

cameff<- read.csv("../Case_studies_datasets/cam_sampling_effort.csv",header=T, sep=",",dec=".")
```

In order to apply SES we convert all data into the same currency:
visits/h/plant.

#### 5.1. Time transformation

For DNA-barcoding dataset - divide number of samples by the number of
hours the seed tray was collecting samples under the individual plants.

For cameras dataset - divide visits by the number of hours the camera
was monitoring each individual plant.

``` r
bc.ses1 <- bc[,,]/(12*bceff$days_working) 
cam.ses1 <- cam[,,]/(cameff$time/60)
```

#### 5.2. Area transformation

For DNA-barcoding dataset - calculate area correction factor (*i.e.*,
plant area/seed tray area), and multiply the samples found by this
correction factor.

For cameras dataset - extrapolate visits to the total area of the plant.
The GoPro cameras focused the entire extension of the plant from the
side, so 50% of the plant was visible The correction for area in this
case multiplies by 2 the number of visits.

``` r
bceff$correc_fac <- bceff$plant_area/bceff$area_seed_trap 
bc.ses2 <- bc.ses1[,,]*bceff$correc_fac

cam.ses2 <- cam.ses1[,,]*2
```

#### 5.3. Data merging (mean)

``` r
SES<- (bc.ses2+cam.ses2)/2
```

``` r
write.csv(QC, file="Pistacia_QC.csv")
write.csv(QS, file="Pistacia_QS.csv")
write.csv(GTS, file="Pistacia_GTS.csv")
write.csv(MMS, file="Pistacia_MMS.csv")
write.csv(SES, file="Pistacia_SES.csv")
```

### Data distribution for initial adjanjency matrix and merging methods

All data is log-transformed due to 0 inflated matrices

![](pl_matrices_merge_files/figure-gfm/histogram%20-%20distribution%20frequency-1.png)<!-- -->
