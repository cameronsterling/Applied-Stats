Proj 3 II
================
Cameron Sterling
2024-12-06

------------------------------------------------------------------------

## The Question

What are the health consequences to outdoor workers’ exposure to extreme
heat in NYS?

## The Ideal Dataset

The ideal data set to analyze the health consequences of outdoor
workers’ exposure to extreme heat in New York State would combine
environmental, occupational, and health outcome data. It would include
granular temperature and heat index measurements across the state,
paired with occupational data identifying outdoor workers by industry,
geographic distribution, and demographics. This would be used to build a
spatiotemporal regression to quantify the relationship between
occupational heat exposure and health impacts while accounting for
regional differences.

## What dataset am I using?

For this project, I created my own dataset from two/three different
ones. 1. National Heat Vulnerability Index: this data set is a
cross-sectional. It was developed in 2022 originally (but updated to
2024) and includes ZCTA (zipcode tabulation area) level data nationally.
Some important variables from this dataset will be `PR_HRI` which comes
from NEMSIS, which is the ZCTA’s percentile rank of heat-related EMS
activation reported to NEMSIS and `P_NEHD` which is the number of
extreme heat days. 2. Dataset for Adamo, S.B. and Squires J. (2024) -
Outdoor Workers, heat, air, pollution, and tree cover in New York: a
rural-urban narrative: This data set is cross-sectional, drawing on ACS
5-year estimates of employment statistics, USDA data from 2011, 2016,
and 2021 describing local tree cover, and NYSERDA (2023) NYSERDA (2023).
DAC Distribution of Disadvantaged Communities (DAC). On a census tract
level, it has information about the number of outdoor workers, their
occupations, health statistics, and information about local
environmental conditions.

While this dataset will be be able to (somewhat) answer the question at
hand, there are some key differences from the ideal dataset. 1. This
dataset tells us where outdoor workers live, not where they work. While
in more rural census tracts, this will make less of a difference, in an
urban context it prevents us from gaining a full picture of work-related
exposure to heat and particulate matter. 2. We only have aggregate
information about health outcomes. That means that we are not actually
be able to quantify the effects of an individual’s exposure, but a
popuation’s exposure, making it difficult to establish a causal story.

This being said, we still are able to answer the question. If we examine
census tracts with comparable heat and environmental conditions, we
should be able to see that those with more people working outdoors will
have more heat-related ER visits.

## Critical Processes

The most important process that happened to this dataset was combining
the ZCTA-based data with the census tract based data. The idea was to
disaggregate the ZCTA data to the census tract; for temperature related
data, this meant that there was lower resolution. However, one could say
that the census tracts are \<<within an area>\> where a specific
environmental condition is present. This also complicates anything that
relates to population such as the Percentile rank of heat-related EMS
activation reported to NEMSIS, which is based on a gross statistic.

The way I processed this data was to use a crosswalk between 2020 ZCTA
and Census tract information, and a seperate crosswalk to go from 2020
census tracts to 2010. These transformations introduce some spatial
noise, however, it was not a large concern of mine because all of these
variables should be spatially correlated anyway.

The other important consideration in processing was figuring out who
counted as an outdoor worker. I decided to make an variable called
perc_outdoor that measured the percent of outdoor workers which I
defined as workers in construction, Agriculture, forestry, fishing,
hunting, Mining, quarrying, or oil and gas extraction.

Lastly, another important thing was making sure that the data was
normally distrubted. A lot of the variables appear in percentiles, which
can

## R Setup and Processing the Data

``` r
df_work$MI_Rates <- as.numeric(df_work$MI_Rates)
df_work$MI_Rates_prep <- df_work$MI_Rates / 100
df_work$normal_MI_Rates <- qnorm(df_work$MI_Rates_prep)
df_work$normal_MI_Rates <- df_work$normal_MI_Rates + 4
```

``` r
zcta_to_tract20 <- read.delim(
  "zctatotract2020.txt", header = TRUE, sep = "|", colClasses = c("character", "character", "character"))

twenty_to_10 <- read.delim("/Users/cameronsterling/Downloads/Applied Statistical Methods/tab20_tract20_tract10_st36.txt", header = TRUE, sep = "|", colClasses= "character")
```

``` r
df_heat_tract20 <- zcta_to_tract20 %>%
  right_join(heat_ny, by = c("GEOID_ZCTA5_20" = "ZCTA" ))

df_to2010 <- twenty_to_10 %>% 
  merge(df_heat_tract20, by ="GEOID_TRACT_20")

df_combined <- df_to2010 %>%
  merge(df_work, by.x = "GEOID_TRACT_10", by.y = "GEOID...1")

df_combined <- unique(df_combined)

df_combined <- df_combined %>%
  distinct(GEOID_TRACT_10, .keep_all = TRUE) 

rownames(df_combined) <- df_combined$GEOID_TRACT_10
```

## Informative Graph

``` r
 df_combined$PR_NEHD <- as.numeric(df_combined$PR_NEHD )
 df_combined$MI_Rates <- as.numeric(df_combined$MI_Rates )

Heat_MI <- lm(df_combined$MI_Rates ~ df_combined$PR_NEHD )
df_combined$P_NEHD[df_combined$P_NEHD == -999.0000] <- NA

df_combined$PR_NEHD <- rank(df_combined$PR_NEHD, na.last = "keep") / sum(!is.na(df_combined$P_NEHD))

Heat_MI <- lm(MI_Rates ~ PR_NEHD, data = df_combined)

ggplot(df_combined, aes(x = P_NEHD, y = MI_Rates, color = Urb_Rural)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Urb_Rural), linetype = "solid") + # Separate regression lines
  scale_color_manual(values = c("urban" = "blue", "rural" = "orange", "suburban" = "green")) +
  labs(
    title = "Census Tract Percentile of ER Visits for Heart Attack and of Days Above 90 Degrees",
    x = "Percentile Ranking Extreme Heat",
    y = "% ranking  age-adjusted hospitalizations for heart attacks",
    color = "Urban/Rural/Suburban"
  ) +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 22 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](TryII_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

About 5.624752 of people in NYS work outdoors. Where do they work?

``` r
ny_tracts <- tracts(state = "NY", cb = FALSE, year = 2010)
```

    ##   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |==========================                                            |  36%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |============================                                          |  41%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%

``` r
ny_tracts_mapped <- ny_tracts %>%
  merge(df_combined, by.x = "GEOID10", by.y = "GEOID_TRACT_10", all.x = FALSE)

ggplot(data = ny_tracts_mapped) +
  geom_sf(aes(fill = perc_outdoor), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "% Outdoor") +
  theme_minimal() +
  labs(
    title = "Percentage Outdoor Workers by Census Tract (2020)",
    subtitle = "New York State",
  )
```

![](TryII_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggplot(data = ny_tracts_mapped) +
  geom_sf(aes(fill = normal_MI_Rates), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "% Outdoor") +
  theme_minimal() +
  labs(
    title = "Relative extremity of heart attack ER visit rates",
    subtitle = "New York State",
  )
```

![](TryII_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
ggplot(data = ny_tracts_mapped) +
  geom_sf(aes(fill = P_NEHD), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "% Outdoor") +
  theme_minimal() +
  labs(
    title = "Number of Hot Days  (2020)",
    subtitle = "New York State",
  )
```

![](TryII_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
ggplot(data = ny_tracts_mapped) +
    geom_sf(aes(fill = P_NEHD), color = NA) +
   scale_fill_viridis_c(option = "viridis", name = "Percentile Days over 90 Degrees") +
    theme_minimal() +
    labs(       title = "Percentile Days over 90 degrees  by Census Tract (2010)",
 subtitle = "New York State",
  )
```

![](TryII_files/figure-gfm/unnamed-chunk-6-1.png)<!-- --> \## Analysis
What do do? \### First I am going to test for spatial autocorrelation

``` r
df_combined$normal_MI_Rates[!is.finite(df_combined$normal_MI_Rates)] <- NA


mixed_model <- lm(normal_MI_Rates ~ perc_outdoor + P_NEHD + TCC_2021 + SOCIODEM_SCORE + PM25*SOCIODEM_SCORE + TCC_2021*SOCIODEM_SCORE + TCC_2021*PM25 
                  , data = df_combined)

summary(mixed_model)
```

    ## 
    ## Call:
    ## lm(formula = normal_MI_Rates ~ perc_outdoor + P_NEHD + TCC_2021 + 
    ##     SOCIODEM_SCORE + PM25 * SOCIODEM_SCORE + TCC_2021 * SOCIODEM_SCORE + 
    ##     TCC_2021 * PM25, data = df_combined)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.8819 -0.6605 -0.0231  0.6218  3.3101 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              3.607e+00  2.507e-01  14.391  < 2e-16 ***
    ## perc_outdoor             1.579e-02  2.837e-03   5.568 2.72e-08 ***
    ## P_NEHD                   8.793e-02  8.906e-03   9.873  < 2e-16 ***
    ## TCC_2021                -2.682e-02  4.176e-03  -6.423 1.47e-10 ***
    ## SOCIODEM_SCORE          -6.775e-01  4.501e-01  -1.505 0.132311    
    ## PM25                    -3.454e-02  3.138e-03 -11.008  < 2e-16 ***
    ## SOCIODEM_SCORE:PM25      5.302e-02  6.124e-03   8.657  < 2e-16 ***
    ## TCC_2021:SOCIODEM_SCORE  3.039e-02  8.145e-03   3.731 0.000193 ***
    ## TCC_2021:PM25            1.833e-04  3.157e-05   5.805 6.87e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9128 on 4699 degrees of freedom
    ##   (174 observations deleted due to missingness)
    ## Multiple R-squared:  0.1595, Adjusted R-squared:  0.1581 
    ## F-statistic: 111.5 on 8 and 4699 DF,  p-value: < 2.2e-16

## Moran’s I Test

``` r
ny_tracts_mapped$normal_MI_Rates[!is.finite(ny_tracts_mapped$normal_MI_Rates)] <- NA

nb <- poly2nb(ny_tracts_mapped, snap = 0.001) 
```

    ## Warning in poly2nb(ny_tracts_mapped, snap = 0.001): some observations have no neighbours;
    ## if this seems unexpected, try increasing the snap argument.

    ## Warning in poly2nb(ny_tracts_mapped, snap = 0.001): neighbour object has 2 sub-graphs;
    ## if this sub-graph count seems unexpected, try increasing the snap argument.

``` r
weights <- nb2listw(nb, style = "W", zero.policy = TRUE)
coords <- st_centroid(st_geometry(ny_tracts_mapped))


moran_test <- moran.test(ny_tracts_mapped$normal_MI_Rates, weights, zero.policy = TRUE, na.action = na.pass)
```

    ## Warning in lag.listw(listw, z, zero.policy = zero.policy, NAOK = NAOK): NAs in
    ## lagged values

``` r
print(moran_test)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  ny_tracts_mapped$normal_MI_Rates  
    ## weights: weights  
    ## n reduced by no-neighbour observations  
    ## 
    ## Moran I statistic standard deviate = 78.714, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      6.616925e-01     -2.049180e-04      7.070949e-05

### Geographically Weighted Regression (GWR)

``` r
data_no_geom <- st_drop_geometry(ny_tracts_mapped) 

required_vars <- c("normal_MI_Rates", "perc_outdoor", "P_NEHD", "TCC_2021", "SOCIODEM_SCORE", "PM25")

complete_rows <- complete.cases(data_no_geom[, required_vars])
ny_tracts_mapped_clean <- ny_tracts_mapped[complete_rows, ]

nrow(ny_tracts_mapped_clean)
```

    ## [1] 4708

``` r
ny_tracts_mapped_clean <- cbind(ny_tracts_mapped_clean, 
                                st_coordinates(st_centroid(ny_tracts_mapped_clean)))
```

    ## Warning: st_centroid assumes attributes are constant over geometries

``` r
gwr_bandwidth <- bw.gwr(
  formula = normal_MI_Rates ~ perc_outdoor + P_NEHD + TCC_2021 + 
             SOCIODEM_SCORE + PM25 * SOCIODEM_SCORE + TCC_2021 * PM25,
  data = ny_tracts_mapped_clean,
  longlat = FALSE,  
  approach = "AICc",
  adapt = TRUE     
)
```

    ## Take a cup of tea and have a break, it will take a few minutes.
    ##           -----A kind suggestion from GWmodel development group
    ## Adaptive bandwidth (number of nearest neighbours): 2917 AICc value: 11784.31 
    ## Adaptive bandwidth (number of nearest neighbours): 1811 AICc value: 11314.24 
    ## Adaptive bandwidth (number of nearest neighbours): 1126 AICc value: 10892.55 
    ## Adaptive bandwidth (number of nearest neighbours): 704 AICc value: 10268.06 
    ## Adaptive bandwidth (number of nearest neighbours): 442 AICc value: 9480.217 
    ## Adaptive bandwidth (number of nearest neighbours): 281 AICc value: 8607.069 
    ## Adaptive bandwidth (number of nearest neighbours): 180 AICc value: 7865.815 
    ## Adaptive bandwidth (number of nearest neighbours): 119 AICc value: 10587.96 
    ## Adaptive bandwidth (number of nearest neighbours): 219 AICc value: 8147.578 
    ## Adaptive bandwidth (number of nearest neighbours): 157 AICc value: 7930.109 
    ## Adaptive bandwidth (number of nearest neighbours): 195 AICc value: 7976.302 
    ## Adaptive bandwidth (number of nearest neighbours): 171 AICc value: 7804.871 
    ## Adaptive bandwidth (number of nearest neighbours): 165 AICc value: 7763.777 
    ## Adaptive bandwidth (number of nearest neighbours): 162 AICc value: 7894.833 
    ## Adaptive bandwidth (number of nearest neighbours): 168 AICc value: 7811.706 
    ## Adaptive bandwidth (number of nearest neighbours): 164 AICc value: 8071.776 
    ## Adaptive bandwidth (number of nearest neighbours): 166 AICc value: 8728.429 
    ## Adaptive bandwidth (number of nearest neighbours): 164 AICc value: 8071.776 
    ## Adaptive bandwidth (number of nearest neighbours): 165 AICc value: 7763.777

``` r
print(gwr_bandwidth)
```

    ## [1] 165

``` r
gwr_model <- gwr.basic(
  formula = normal_MI_Rates ~ perc_outdoor + P_NEHD + TCC_2021 + 
            SOCIODEM_SCORE + PM25 * SOCIODEM_SCORE + TCC_2021 * PM25,
  data = ny_tracts_mapped_clean,
  bw = gwr_bandwidth,
  longlat = FALSE,  
  adapt = TRUE     
)
```

``` r
print(gwr_model)
```

    ##    ***********************************************************************
    ##    *                       Package   GWmodel                             *
    ##    ***********************************************************************
    ##    Program starts at: 2024-12-06 12:37:42.968463 
    ##    Call:
    ##    gwr.basic(formula = normal_MI_Rates ~ perc_outdoor + P_NEHD + 
    ##     TCC_2021 + SOCIODEM_SCORE + PM25 * SOCIODEM_SCORE + TCC_2021 * 
    ##     PM25, data = ny_tracts_mapped_clean, bw = gwr_bandwidth, 
    ##     adaptive = TRUE, longlat = FALSE)
    ## 
    ##    Dependent (y) variable:  normal_MI_Rates
    ##    Independent variables:  perc_outdoor P_NEHD TCC_2021 SOCIODEM_SCORE PM25
    ##    Number of data points: 4708
    ##    ***********************************************************************
    ##    *                    Results of Global Regression                     *
    ##    ***********************************************************************
    ## 
    ##    Call:
    ##     lm(formula = formula, data = data)
    ## 
    ##    Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.7839 -0.6649 -0.0189  0.6319  3.3215 
    ## 
    ##    Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ##    (Intercept)          2.939e+00  1.757e-01  16.727  < 2e-16 ***
    ##    perc_outdoor         1.619e-02  2.839e-03   5.702 1.26e-08 ***
    ##    P_NEHD               8.665e-02  8.911e-03   9.724  < 2e-16 ***
    ##    TCC_2021            -1.225e-02  1.482e-03  -8.269  < 2e-16 ***
    ##    SOCIODEM_SCORE       7.277e-01  2.468e-01   2.949   0.0032 ** 
    ##    PM25                -2.709e-02  2.423e-03 -11.178  < 2e-16 ***
    ##    SOCIODEM_SCORE:PM25  3.754e-02  4.512e-03   8.321  < 2e-16 ***
    ##    TCC_2021:PM25        1.728e-04  3.149e-05   5.487 4.29e-08 ***
    ## 
    ##    ---Significance stars
    ##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ##    Residual standard error: 0.9141 on 4700 degrees of freedom
    ##    Multiple R-squared: 0.1571
    ##    Adjusted R-squared: 0.1558 
    ##    F-statistic: 125.1 on 7 and 4700 DF,  p-value: < 2.2e-16 
    ##    ***Extra Diagnostic information
    ##    Residual sum of squares: 3927.048
    ##    Sigma(hat): 0.9134971
    ##    AIC:  12524.81
    ##    AICc:  12524.85
    ##    BIC:  7951.037
    ##    ***********************************************************************
    ##    *          Results of Geographically Weighted Regression              *
    ##    ***********************************************************************
    ## 
    ##    *********************Model calibration information*********************
    ##    Kernel function: bisquare 
    ##    Adaptive bandwidth: 165 (number of nearest neighbours)
    ##    Regression points: the same locations as observations are used.
    ##    Distance metric: Euclidean distance metric is used.
    ## 
    ##    ****************Summary of GWR coefficient estimates:******************
    ##                               Min.     1st Qu.      Median     3rd Qu.
    ##    Intercept           -5.6803e+02 -1.2112e+01 -6.7800e-01  8.5900e+00
    ##    perc_outdoor        -9.6446e-02 -4.2464e-03  7.6023e-03  2.1286e-02
    ##    P_NEHD              -1.0442e+02 -2.1847e-01  1.8770e-01  7.6316e-01
    ##    TCC_2021            -1.9857e+00 -8.9109e-02 -1.1293e-02  9.4847e-02
    ##    SOCIODEM_SCORE      -2.8539e+02 -5.2770e+00  3.8425e+00  1.8885e+01
    ##    PM25                -2.2067e+00 -1.2267e-01  2.9719e-02  1.9843e-01
    ##    SOCIODEM_SCORE:PM25 -5.6329e+00 -3.5938e-01 -7.1678e-02  2.0117e-01
    ##    TCC_2021:PM25       -3.7403e-02 -2.1929e-03 -1.5307e-04  1.9687e-03
    ##                             Max.
    ##    Intercept           1197.4534
    ##    perc_outdoor           0.0966
    ##    P_NEHD                47.3673
    ##    TCC_2021               3.6891
    ##    SOCIODEM_SCORE       401.9627
    ##    PM25                   3.6587
    ##    SOCIODEM_SCORE:PM25    3.2521
    ##    TCC_2021:PM25          0.0238
    ##    ************************Diagnostic information*************************
    ##    Number of data points: 4708 
    ##    Effective number of parameters (2trace(S) - trace(S'S)): 564.1646 
    ##    Effective degrees of freedom (n-2trace(S) + trace(S'S)): 4143.835 
    ##    AICc (GWR book, Fotheringham, et al. 2002, p. 61, eq 2.33): 7763.777 
    ##    AIC (GWR book, Fotheringham, et al. 2002,GWR p. 96, eq. 4.22): 7237.172 
    ##    BIC (GWR book, Fotheringham, et al. 2002,GWR p. 61, eq. 2.34): 5774.979 
    ##    Residual sum of squares: 1168.99 
    ##    R-square value:  0.7490761 
    ##    Adjusted R-square value:  0.7149056 
    ## 
    ##    ***********************************************************************
    ##    Program stops at: 2024-12-06 12:37:54.46447

The analysis demonstrates that the relationship between heart attack ER
visit severity (normalized severity percentile, normal_MI_Rates) and
environmental and socio-demographic factors varies significantly across
space. In the global regression model, perc_outdoor (percent outdoor
workers), P_NEHD (neighborhood vulnerability), and SOCIODEM_Score
(socio-demographic score) are positively associated with higher
severity, while TCC_2021 (tree cover) and PM25 (air quality, with lower
values indicating better air quality) are negatively associated.
Interaction terms reveal that poor air quality amplifies the effect of
socio-demographic disadvantages and reduces the protective effect of
tree cover. However, the global model explains only 15.6% of the
variance, underscoring its limitations in capturing spatial
heterogeneity. In contrast, the geographically weighted regression (GWR)
model shows significant spatial variation in the coefficients, with
local relationships ranging from strongly positive to strongly negative
for variables like perc_outdoor and SOCIODEM_Score.

For instance, the effect of perc_outdoor ranges from -0.096 (negative
impact) to 0.096 (positive impact), suggesting that outdoor workers face
higher risk in some areas but not others, potentially due to differences
in air quality or tree cover. Similarly, the interaction of
socio-demographic vulnerability and air quality (SOCIODEM_Score:PM25)
has a highly variable effect, amplifying risk in some locations while
mitigating it in others. The GWR model explains 71.5% of the variance,
far surpassing the global model, and highlights the importance of
considering localized environmental and socio-demographic dynamics in
understanding and addressing heart attack severity across regions.

``` r
ny_tracts_mapped_clean$gwr_coef_perc_outdoor <- gwr_model$SDF$perc_outdoor

ggplot(data = ny_tracts_mapped_clean) +
  geom_sf(aes(fill = gwr_coef_perc_outdoor)) +
  scale_fill_viridis_c() +
  labs(title = "GWR Coefficients for perc_outdoor",
       fill = "Coefficient") +
  theme_minimal()
```

![](TryII_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ny_tracts_mapped_clean$gwr_coef_perc_outdoor <- gwr_model$SDF$perc_outdoor

lm_model <- lm(gwr_coef_perc_outdoor ~ Urb_Rural, data = ny_tracts_mapped_clean)

summary(lm_model)
```

    ## 
    ## Call:
    ## lm(formula = gwr_coef_perc_outdoor ~ Urb_Rural, data = ny_tracts_mapped_clean)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.107810 -0.013470 -0.000926  0.012146  0.085232 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       0.0015270  0.0007301   2.092   0.0365 *  
    ## Urb_Ruralsuburban 0.0098369  0.0009129  10.776   <2e-16 ***
    ## Urb_Ruralurban    0.0082701  0.0008400   9.845   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.02069 on 4705 degrees of freedom
    ## Multiple R-squared:  0.02625,    Adjusted R-squared:  0.02584 
    ## F-statistic: 63.42 on 2 and 4705 DF,  p-value: < 2.2e-16

``` r
ggplot(ny_tracts_mapped_clean, aes(x = Urb_Rural, y = gwr_coef_perc_outdoor, fill = Urb_Rural)) +
  geom_boxplot() +
  labs(
    title = "GWR Coefficients by Urban, Rural, and Suburban Areas",
    x = "Urban/Rural/Suburban Classification",
    y = "GWR Coefficient for perc_outdoor"
  ) +
  theme_minimal()
```

![](TryII_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> Our analysis
sought to address the question: What are the health consequences to
outdoor workers’ exposure to extreme heat in New York State? By focusing
on the normalized severity of heart attack ER visits (normal_MI_Rates),
we explored how environmental factors such as outdoor worker percentage
(perc_outdoor), tree cover (TCC_2021), air quality (PM25), and
socio-demographic vulnerability (SOCIODEM_Score) influence health
outcomes across the state.

The results suggest that outdoor workers are indeed at higher risk in
areas with poor environmental conditions. Specifically, the global
regression showed that perc_outdoor is positively associated with higher
severity, while TCC_2021 (tree cover) and better air quality are
protective. However, the global model failed to account for spatial
heterogeneity, explaining only 15.6% of the variance in heart attack
severity. The geographically weighted regression (GWR) addressed this
limitation, revealing significant spatial variability in the
relationships. For example, the effect of perc_outdoor on severity
ranged from negative to strongly positive across regions, likely
reflecting localized variations in environmental conditions like tree
cover and air quality. The GWR model provided a much better fit,
explaining 71.5% of the variance, and highlighted that the combined
impacts of socio-demographic vulnerability and air quality
(SOCIODEM_Score:PM25) exacerbate health risks in some regions while
mitigating them in others.

While the results align with our expectations that environmental and
socio-demographic factors interact to affect outdoor workers’ health,
they also revealed unexpected complexity. For instance, the extreme
variability in coefficients, such as the negative effects of tree cover
in certain areas, suggests more nuanced interactions between
environmental factors that require further exploration. Additionally,
data constraints limited our ability to analyze at finer spatial or
temporal scales, which could provide more targeted insights.

Future work could extend this analysis by testing the robustness of
these findings through simulations that ignore spatial correlations or
alternative modeling approaches like spatial lag models. Temporal data
on heatwaves and health outcomes could also help identify short-term
impacts of extreme heat on outdoor workers. Finally, integrating
finer-resolution data, such as individual-level health outcomes or
workplace conditions, would allow for a more precise assessment of the
health consequences of heat exposure for outdoor workers in New York
State.
