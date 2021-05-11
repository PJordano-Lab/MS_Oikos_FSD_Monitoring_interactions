Pistacia correlations
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

Read data

``` r
bc <- read.csv("../Case_studies_datasets/pl_bc_clean.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
cam <- read.csv("../Case_studies_datasets/pl_cam_clean.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
QC <- read.csv("../Data_merging/Pistacia_QC.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
QS <- read.csv("../Data_merging/Pistacia_QS.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
GTS <- read.csv("../Data_merging/Pistacia_GTS.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
MMS <- read.csv("../Data_merging/Pistacia_MMS.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
SES <- read.csv("../Data_merging/Pistacia_SES.csv",header=T,sep=",",dec=".")%>% 
    column_to_rownames(var="X")
```

Apply long format to matrices

Combine all merging methods together in a single dataset and calculate a
Coefficient of Variation for ‘Standardizing’ methods

## COMPARE BETWEEN QUANTITATIVE METHODS

![](Pistacia_correlations_files/figure-gfm/comparing%20between%20methods-1.png)<!-- -->

Now with data log-transformed to control for 0 inflated matrices.

![](Pistacia_correlations_files/figure-gfm/comparing%20between%20methods%202-1.png)<!-- -->

-   **All correlations showed in the graphs are kendall’s tau (based in
    ranking for non-parametric data).**
-   Here we see how the different merging methods are (dis)similar to
    each other.
-   MMS and QS are the most similar ones.
-   SES method is most similar to a GTS, and less to QS.
-   GTS has also very similar results to MMS.

### METHODS CORRELATIONS

``` r
pearson <- cor(x=all.long[,c(3,4,6:9)], method = "pearson")

kendall <- cor(x=all.long[,c(3,4,6:9)], method = "kendall")
```

Correlation plot:

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
library(RColorBrewer)

col <- colorRampPalette(c("white", "white","white", "white","white", "white", "white",  "white", "white","#D9EF8B","#A6D96A","#66BD63"))

col2 <- colorRampPalette(c("white", "white","white", "white","white", "white", "white",  "white", "white","#edf8b1","#7fcdbb","#2c7fb8"))

corrplot(pearson,method = "color",type="upper",number.cex = .7,addCoef.col="black", tl.col = "black", col=col2(40), title="Pearson UP vs Kendall DOWN", tl.pos = "d",tl.srt = 45, cl.pos = "n")

corrplot(kendall, method = "color",type="lower",number.cex = .7,addCoef.col=T, tl.col = "black",  col=col2(40),diag = FALSE, tl.pos = "n", cl.pos = "n",add=T)
```

![](Pistacia_correlations_files/figure-gfm/cors%202-1.png)<!-- -->

### COMPARING METHODOLOGIES WITH MERGING METHODS

*Number of registers for each methodology*

DNA-Barcoding:

    ## [1] 1162

GoPro cameras:

    ## [1] 397

Here we observe the relationship between the 2 methodologies used in the
field: Barcoding and cams is very little correlated.

![](Pistacia_correlations_files/figure-gfm/compare%20cams%20and%20bc-1.png)<!-- -->

This plot trendline shows how the highest values for cameras are not so
high in barcoding, probably because cams detect in higher freq the most
common frugivores (E. rub and S. mel), the force the trend to go
upsteep. Rare species could also be pulling the trend towards the x axis
(since they are only detectable in barcoding), yet, these are much fewer
interactions, not likely affecting the final trend.

### How well each “merging method” values the different two methodologies

#### QUANTITATIVE SUM (QS)

``` r
QS_BC <- ggplot(data=all.long, aes(x=visits.qs, y=visits.bc))+geom_point()+
  theme_bw()+ylab("DNA-Barcoding")+xlab("QS")+
  geom_smooth(color="#0570b0", size=0.5)+
  #geom_smooth(method=lm, color="violetred1", size=0.5)+ 
  stat_cor(aes(label = ..r.label..), method="kendall")

QS_CAM <- ggplot(data=all.long, aes(x=visits.qs, y=visits.cam))+geom_point()+
  theme_bw()+ylab("GoPro")+xlab("QS")+
  geom_smooth(color="#0570b0", size=0.5)+
  #geom_smooth(method=lm, color="violetred1", size=0.5)+ 
  stat_cor(aes(label = ..r.label..), method="kendall")

grid.arrange(QS_BC, QS_CAM, ncol=2, top="How QS merging treats each metholodgy")
```

![](Pistacia_correlations_files/figure-gfm/comparing%20within%20methods%201-1.png)<!-- -->

#### Grand Total Standarization (GTS)

![](Pistacia_correlations_files/figure-gfm/comparing%20within%20methods%202-1.png)<!-- -->

#### MIN-MAX SCALING (MMS)

![](Pistacia_correlations_files/figure-gfm/comparing%20within%20methods%203-1.png)<!-- -->

#### Standarize base on SAMPLING EFFORT (SES)

![](Pistacia_correlations_files/figure-gfm/comparing%20within%20methods%204-1.png)<!-- -->

SUMMARY:

-   All methods show a higher Kendall’s correlation with the barcoding
    data, indicating a higher accordance with DNA-Barcoding (probably
    because it has many more interactions, 1162 vs. 397).

-   Yet, GTS and SES methods gave higher correlation to cameras than QS
    and MMS, decreasing the weight of the barcoding data; indicating
    more accordance with camera data. I think this might be an effect of
    GTS and SES putting both methods at the same level (giving them a
    similar weight to both) by considering a similar sampling effort for
    both methods.

-   GTS does this by referring interactions to the total of registers.
    And SES by referring all interactions to the same units, regardless
    they have been more or less sampled.

-   For SES standardization, regarding time, barcoding data is less
    valued, since they have been recording for around 1300 hours (and
    would need to divide the interaction by that big number of hours) vs
    cameras that just have recorded a mean of 2.11 hours. Yet, regarding
    area, DNA-barcoding is more valued, since it has recorded around 1%
    of the plant and the cameras have recorded a 50% (barcoding
    interactions would be multiplied by 100 to refer it to whole plant,
    while cameras only by 2). These differences in time and area
    correction, make in the end having higher correction factor for
    interactions in cameras than in barcoding.
