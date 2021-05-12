State Correlation Analyses
================

Markdown code for the survival analyses of the Port of Miami data
examining the main effects of channel (i.e. dredging impact), reef
habitat, and direction. Survival analyses used to tease apart species
specific differences as well as the impact of state (or condition)
specifically whether the corals were observed to be buried, bleached or
diseased. Each variable is coded as 0 (absence) or 1 (presence) as well
as in combination in the states column.

``` r
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE, error = FALSE, fig.path='Figures/', dev=c('png', 'pdf'))
```

All figures exported to Figures/folder as png and pdf

``` r
df=read.csv("9sp_corr.csv") #time by months; excludes missing corals
glimpse(df)
```

    ## Rows: 63
    ## Columns: 5
    ## $ Species           <chr> "DCLI", "DSTO", "DSTR", "MCAV", "MMEA", "PAST", "SBO…
    ## $ Condition         <chr> "Mortality", "Mortality", "Mortality", "Mortality", …
    ## $ Value             <int> 5, 49, 14, 11, 47, 7, 14, 0, 3, 4, 24, 8, 29, 24, 1,…
    ## $ Total.Individuals <int> 17, 74, 16, 93, 51, 96, 110, 41, 62, 17, 74, 16, 93,…
    ## $ Percentage        <dbl> 0.29411765, 0.66216216, 0.87500000, 0.11827957, 0.92…

``` r
df %>% filter(Condition %in% c("Bleaching","Disease","Buried", "Partial Burial","Partial Mortality","Mortality")) %>%
  mutate(prop = Value/Total.Individuals) %>%
  ggplot(aes(y=Species, x = prop)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~Condition)
```

![](Figures/bar%20plot%20all%20conditions%20facet-1.png)<!-- -->

``` r
df %>% filter(Condition %in% c("Bleaching","Disease","Buried", "Partial Burial","Partial Mortality","Mortality")) %>%
  mutate(prop = Value/Total.Individuals*100) %>%
  ggdotchart(x="Species", y = "prop",
             color = "Condition",
             rotate = TRUE,
             add = "segments",
             dot.size = 6,
             label = "Value",
             repel = FALSE,
             xlab = "Top 9 Coral Species",
             ylab = "Percentage of Corals with Condition",
             font.label = list(color = "white", size = 9, vjust = 0.5),
             facet.by = "Condition")
```

![](Figures/dot%20plot%20all%20conditions%20facet-1.png)<!-- -->

``` r
df %>% filter(Condition %in% c("Bleaching","Disease","Buried","Mortality")) %>%
  mutate(prop = Value/Total.Individuals*100) %>%
  ggdotchart(x="Species", y = "prop",
             color = "Condition",
             rotate = TRUE,
             add = "segments",
             dot.size = 6,
             label = "Value",
             repel = FALSE,
             xlab = "Top 9 Coral Species",
             ylab = "Percentage of Corals with Condition",
             font.label = list(color = "white", size = 9, vjust = 0.5),
             facet.by = "Condition")
```

![](Figures/dot%20plot%20major%20conditions%20facet-1.png)<!-- -->

``` r
df %>% filter(Condition %in% c("Bleaching","Disease","Buried","Mortality")) %>%
  mutate(prop = Value/Total.Individuals*100) %>%
  ggdotchart(x="Species", y = "prop",
             color = "Condition",
             rotate = TRUE,
             add = "segments",
             dot.size = 5,
             label = "Value",
             repel = FALSE,
             xlab = "Top 9 Coral Species",
             ylab = "Percentage of Corals with Condition",
             font.label = list(color = "white", size = 9, vjust = 0.5))
```

![](Figures/dot%20plot%20major%20conditions%20BEST-1.png)<!-- -->

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
df_w <- dcast(df, Species ~ Condition, value.var="Percentage")
kable(df_w)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Species
</th>
<th style="text-align:right;">
Bleaching
</th>
<th style="text-align:right;">
Buried
</th>
<th style="text-align:right;">
Disease
</th>
<th style="text-align:right;">
Mortality
</th>
<th style="text-align:right;">
Normal
</th>
<th style="text-align:right;">
Partial Burial
</th>
<th style="text-align:right;">
Partial Mortality
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
DCLI
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.2352941
</td>
<td style="text-align:right;">
0.2941176
</td>
<td style="text-align:right;">
0.0588235
</td>
<td style="text-align:right;">
0.7647059
</td>
<td style="text-align:right;">
0.6470588
</td>
</tr>
<tr>
<td style="text-align:left;">
DSTO
</td>
<td style="text-align:right;">
0.0135135
</td>
<td style="text-align:right;">
0.0405405
</td>
<td style="text-align:right;">
0.3243243
</td>
<td style="text-align:right;">
0.6621622
</td>
<td style="text-align:right;">
0.0405405
</td>
<td style="text-align:right;">
0.6621622
</td>
<td style="text-align:right;">
0.4324324
</td>
</tr>
<tr>
<td style="text-align:left;">
DSTR
</td>
<td style="text-align:right;">
0.0625000
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.5000000
</td>
<td style="text-align:right;">
0.8750000
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.2500000
</td>
<td style="text-align:right;">
0.2500000
</td>
</tr>
<tr>
<td style="text-align:left;">
MCAV
</td>
<td style="text-align:right;">
0.0752688
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.3118280
</td>
<td style="text-align:right;">
0.1182796
</td>
<td style="text-align:right;">
0.2473118
</td>
<td style="text-align:right;">
0.5268817
</td>
<td style="text-align:right;">
0.2580645
</td>
</tr>
<tr>
<td style="text-align:left;">
MMEA
</td>
<td style="text-align:right;">
0.2156863
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.4705882
</td>
<td style="text-align:right;">
0.9215686
</td>
<td style="text-align:right;">
0.0196078
</td>
<td style="text-align:right;">
0.5490196
</td>
<td style="text-align:right;">
0.3921569
</td>
</tr>
<tr>
<td style="text-align:left;">
PAST
</td>
<td style="text-align:right;">
0.1562500
</td>
<td style="text-align:right;">
0.0729167
</td>
<td style="text-align:right;">
0.0104167
</td>
<td style="text-align:right;">
0.0729167
</td>
<td style="text-align:right;">
0.3750000
</td>
<td style="text-align:right;">
0.4895833
</td>
<td style="text-align:right;">
0.2187500
</td>
</tr>
<tr>
<td style="text-align:left;">
SBOU
</td>
<td style="text-align:right;">
0.0181818
</td>
<td style="text-align:right;">
0.0636364
</td>
<td style="text-align:right;">
0.1272727
</td>
<td style="text-align:right;">
0.1272727
</td>
<td style="text-align:right;">
0.1090909
</td>
<td style="text-align:right;">
0.7727273
</td>
<td style="text-align:right;">
0.6454545
</td>
</tr>
<tr>
<td style="text-align:left;">
SINT
</td>
<td style="text-align:right;">
0.4634146
</td>
<td style="text-align:right;">
0.0975610
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0731707
</td>
<td style="text-align:right;">
0.8292683
</td>
<td style="text-align:right;">
0.4878049
</td>
</tr>
<tr>
<td style="text-align:left;">
SSID
</td>
<td style="text-align:right;">
0.1290323
</td>
<td style="text-align:right;">
0.0967742
</td>
<td style="text-align:right;">
0.0000000
</td>
<td style="text-align:right;">
0.0483871
</td>
<td style="text-align:right;">
0.3870968
</td>
<td style="text-align:right;">
0.4838710
</td>
<td style="text-align:right;">
0.3548387
</td>
</tr>
</tbody>
</table>

``` r
#Select 
df_corr <- df_w %>% select(Mortality, Buried, Disease, Bleaching, Normal, "Partial Mortality")
head(df_corr)
```

    ##    Mortality     Buried    Disease  Bleaching     Normal Partial Mortality
    ## 1 0.29411765 0.00000000 0.23529412 0.00000000 0.05882353         0.6470588
    ## 2 0.66216216 0.04054054 0.32432432 0.01351351 0.04054054         0.4324324
    ## 3 0.87500000 0.00000000 0.50000000 0.06250000 0.00000000         0.2500000
    ## 4 0.11827957 0.00000000 0.31182796 0.07526882 0.24731183         0.2580645
    ## 5 0.92156863 0.00000000 0.47058824 0.21568628 0.01960784         0.3921569
    ## 6 0.07291667 0.07291667 0.01041667 0.15625000 0.37500000         0.2187500

``` r
res <- cor(df_corr)
round(res,2)
```

    ##                   Mortality Buried Disease Bleaching Normal Partial Mortality
    ## Mortality              1.00  -0.66    0.90     -0.24  -0.68             -0.16
    ## Buried                -0.66   1.00   -0.89      0.48   0.51              0.08
    ## Disease                0.90  -0.89    1.00     -0.37  -0.66             -0.17
    ## Bleaching             -0.24   0.48   -0.37      1.00   0.02             -0.11
    ## Normal                -0.68   0.51   -0.66      0.02   1.00             -0.43
    ## Partial Mortality     -0.16   0.08   -0.17     -0.11  -0.43              1.00

``` r
library(Hmisc)
```

    ## Loading required package: lattice

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     src, summarize

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, units

``` r
res2 <-rcorr(as.matrix(df_corr))
res2
```

    ##                   Mortality Buried Disease Bleaching Normal Partial Mortality
    ## Mortality              1.00  -0.66    0.90     -0.24  -0.68             -0.16
    ## Buried                -0.66   1.00   -0.89      0.48   0.51              0.08
    ## Disease                0.90  -0.89    1.00     -0.37  -0.66             -0.17
    ## Bleaching             -0.24   0.48   -0.37      1.00   0.02             -0.11
    ## Normal                -0.68   0.51   -0.66      0.02   1.00             -0.43
    ## Partial Mortality     -0.16   0.08   -0.17     -0.11  -0.43              1.00
    ## 
    ## n= 9 
    ## 
    ## 
    ## P
    ##                   Mortality Buried Disease Bleaching Normal Partial Mortality
    ## Mortality                   0.0515 0.0011  0.5409    0.0459 0.6845           
    ## Buried            0.0515           0.0014  0.1878    0.1594 0.8397           
    ## Disease           0.0011    0.0014         0.3269    0.0536 0.6549           
    ## Bleaching         0.5409    0.1878 0.3269            0.9687 0.7717           
    ## Normal            0.0459    0.1594 0.0536  0.9687           0.2449           
    ## Partial Mortality 0.6845    0.8397 0.6549  0.7717    0.2449

``` r
library(corrplot)
```

    ## corrplot 0.84 loaded

``` r
corrplot(res, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black",
         p.mat = res2$P, sig.level = 0.05,
         diag = FALSE)
```

![](Figures/correlation%20plot-1.png)<!-- -->

``` r
corrplot(res2$r, type="upper", order="hclust", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "white",
         p.mat = res2$P, sig.level = 0.05, insig = "blank",
         diag = FALSE)
```

![](Figures/significant%20correlations%20BEST-1.png)<!-- -->

``` r
ggplot(data=df_w, mapping = aes(x = Mortality, y = Disease, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Figures/relationship%20between%20disease%20and%20mortality-1.png)<!-- -->
