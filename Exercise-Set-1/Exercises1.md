Part One
--------

Using the ABIA.csv dataset, my aim was to assess how departure delays
correspond to arrival delays per airline. Obviously we should expect a
relatively linear trend where a one-minute delay results in a one-minute
later arrival, but it’s also possible some airlines may have been
willing to expend more gas to “make-up” for departure delays. I was also
curious as to whether that was accentuated during different days of the
week for which flight travel is more utilized and thus planes weigh less
(allowing planes to speed up mid-flight if there was a delay of some
sort). I was also curious as to whether longer or shorter flights
generally stayed true to the expected linear trend and if that varied
per airline.

``` r
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.4
    ## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0
    ## ✓ purrr   0.3.3

    ## ── Conflicts ──────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
ABIA.csv <- read.csv("~/Documents/Master/Data Mining/ABIA.csv.txt")
data(ABIA.csv)
```

    ## Warning in data(ABIA.csv): data set 'ABIA.csv' not found

``` r
ggplot(data = ABIA.csv) + 
  geom_point(mapping = aes(DepDelay, ArrDelay, color = DayOfWeek)) +
  scale_color_gradient(low = "orange", high = "blue") +
  facet_wrap(~ UniqueCarrier, nrow = 4) + 
  labs(title = "Austin Bergstrom International Airport",
       subtitle = "Arrival Delays given Departure Delays per Carrier") +
  ylim(-100,500) +
  xlim(-100, 500)
```

    ## Warning: Removed 1607 rows containing missing values (geom_point).

![](Exercises1_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
ggplot(data = ABIA.csv) + 
  geom_point(mapping = aes(DepDelay, ArrDelay, color = CRSElapsedTime)) +
  scale_color_gradient(low = "orange", high = "blue") +
  facet_wrap(~ UniqueCarrier, nrow = 4) + 
  labs(title = "Austin Bergstrom International Airport",
       subtitle = "Arrival Delays given Departure Delays per Carrier per Estimated Elapsed Flight Time") +
  ylim(-100,500) +
  xlim(-100, 500)
```

    ## Warning: Removed 1607 rows containing missing values (geom_point).

![](Exercises1_files/figure-markdown_github/unnamed-chunk-1-2.png)

The first plot details each airline’s trend between Departure Delay
times and Arrival Delay times for any given day of the week. It explains
how each airline, per day, is able to make-up lost time given a
departure delay. We see that in the middle of the week, fewer flights
are delayed and thus fewer arrive delayed. This is likely because of
fewer people flying in the middle of the week because of work and school
commitments.

The second plot shows how each airline’s delays occur given their flight
lengths. For the most part, given a longer expected flight, there is a
much lower chance of an arrival delay. This is likely because it is
easier to make up lost time on a longer flight than it is on a shorter
flight.

Part Two
--------

Using the sclass.csv dataset, I was able to apply K-Nearest Neighbors
non-parametric modeling to the data after having split the data into
training and testing sets. I looked at a handful of different options
for K and discerned what seemed to accomodate for the most information,
accurately, and without over-generalizing.

``` r
library(tidyverse)
library(FNN)
library(mosaic)
```

    ## Loading required package: lattice

    ## Loading required package: ggformula

    ## Loading required package: ggstance

    ## 
    ## Attaching package: 'ggstance'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     geom_errorbarh, GeomErrorbarh

    ## 
    ## New to ggformula?  Try the tutorials: 
    ##  learnr::run_tutorial("introduction", package = "ggformula")
    ##  learnr::run_tutorial("refining", package = "ggformula")

    ## Loading required package: mosaicData

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## Registered S3 method overwritten by 'mosaic':
    ##   method                           from   
    ##   fortify.SpatialPolygonsDataFrame ggplot2

    ## 
    ## The 'mosaic' package masks several functions from core packages in order to add 
    ## additional features.  The original behavior of these functions should not be affected by this.
    ## 
    ## Note: If you use the Matrix package, be sure to load it BEFORE loading mosaic.

    ## 
    ## Attaching package: 'mosaic'

    ## The following object is masked from 'package:Matrix':
    ## 
    ##     mean

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     count, do, tally

    ## The following object is masked from 'package:purrr':
    ## 
    ##     cross

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     stat

    ## The following objects are masked from 'package:stats':
    ## 
    ##     binom.test, cor, cor.test, cov, fivenum, IQR, median, prop.test,
    ##     quantile, sd, t.test, var

    ## The following objects are masked from 'package:base':
    ## 
    ##     max, mean, min, prod, range, sample, sum

``` r
sclass.csv <- read.csv("~/Documents/Master/Data Mining/sclass.csv.txt")

data(sclass.csv)
```

    ## Warning in data(sclass.csv): data set 'sclass.csv' not found

``` r
sclass550 = subset(sclass.csv, trim == '350')
summary(sclass550)
```

    ##        id             trim       subTrim    condition  isOneOwner
    ##  Min.   :  282   350    :416   Hybrid:  0   CPO :131   f:310     
    ##  1st Qu.:14290   320    :  0   unsp  :416   New : 15   t:106     
    ##  Median :26658   400    :  0                Used:270             
    ##  Mean   :26520   420    :  0                                     
    ##  3rd Qu.:39599   430    :  0                                     
    ##  Max.   :52220   450    :  0                                     
    ##                  (Other):  0                                     
    ##     mileage            year          color      displacement       fuel    
    ##  Min.   :     6   Min.   :1994   Black  :219   3.0 L  :302   Diesel  :307  
    ##  1st Qu.: 19264   1st Qu.:2006   Silver : 77   3.7 L  :109   Gasoline:109  
    ##  Median : 29998   Median :2012   White  : 48   3.5 L  :  5   Hybrid  :  0  
    ##  Mean   : 42926   Mean   :2010   Gray   : 30   3.2 L  :  0   unsp    :  0  
    ##  3rd Qu.: 63479   3rd Qu.:2012   unsp   : 21   4.2 L  :  0                 
    ##  Max.   :173000   Max.   :2013   Blue   : 11   4.3 L  :  0                 
    ##                                  (Other): 10   (Other):  0                 
    ##      state         region             soundSystem    wheelType     wheelSize  
    ##  CA     : 57   SoA    :138   Alpine         :  0   Alloy  :225   unsp   :384  
    ##  FL     : 47   Pac    : 68   Bang Olufsen   :  5   Chrome :  1   18     : 12  
    ##  TX     : 41   Mid    : 52   Bose           : 18   Premium:  6   17     : 10  
    ##  IL     : 37   ENC    : 51   Boston Acoustic:  0   Steel  :  0   19     :  6  
    ##  VA     : 32   WSC    : 45   Harman Kardon  :106   unsp   :184   20     :  4  
    ##  NJ     : 27   New    : 24   Premium        :130                 16     :  0  
    ##  (Other):175   (Other): 38   unsp           :157                 (Other):  0  
    ##   featureCount        price       
    ##  Min.   :  0.00   Min.   :  6600  
    ##  1st Qu.: 31.75   1st Qu.: 19401  
    ##  Median : 54.00   Median : 52900  
    ##  Mean   : 49.22   Mean   : 46854  
    ##  3rd Qu.: 70.00   3rd Qu.: 61991  
    ##  Max.   :112.00   Max.   :106010  
    ## 

``` r
sclass65AMG = subset(sclass.csv, trim == '65 AMG')
summary(sclass65AMG)
```

    ##        id             trim       subTrim    condition  isOneOwner
    ##  Min.   : 1060   65 AMG :292   Hybrid:  0   CPO : 26   f:254     
    ##  1st Qu.:13977   320    :  0   unsp  :292   New : 91   t: 38     
    ##  Median :26557   350    :  0                Used:175             
    ##  Mean   :26444   400    :  0                                     
    ##  3rd Qu.:38687   420    :  0                                     
    ##  Max.   :52326   430    :  0                                     
    ##                  (Other):  0                                     
    ##     mileage            year          color      displacement       fuel    
    ##  Min.   :     1   Min.   :2006   Black  :182   6.0 L  :285   Diesel  :  0  
    ##  1st Qu.:    20   1st Qu.:2007   Silver : 39   unsp   :  7   Gasoline:279  
    ##  Median : 28803   Median :2010   White  : 27   3.0 L  :  0   Hybrid  :  0  
    ##  Mean   : 33700   Mean   :2010   Gray   : 22   3.2 L  :  0   unsp    : 13  
    ##  3rd Qu.: 58496   3rd Qu.:2015   unsp   : 10   3.5 L  :  0                 
    ##  Max.   :146975   Max.   :2015   Blue   :  7   3.7 L  :  0                 
    ##                                  (Other):  5   (Other):  0                 
    ##      state         region            soundSystem    wheelType     wheelSize  
    ##  CA     : 75   Pac    :79   Alpine         :  0   Alloy  :136   unsp   :252  
    ##  FL     : 44   SoA    :63   Bang Olufsen   : 31   Chrome :  2   20     : 39  
    ##  TX     : 27   Mid    :34   Bose           : 13   Premium:  9   18     :  1  
    ##  IL     : 18   ENC    :33   Boston Acoustic:  0   Steel  :  0   16     :  0  
    ##  NY     : 15   WSC    :32   Harman Kardon  : 42   unsp   :145   17     :  0  
    ##  NJ     : 12   Mtn    :17   Premium        : 92                 19     :  0  
    ##  (Other):101   (Other):34   unsp           :114                 (Other):  0  
    ##   featureCount        price       
    ##  Min.   :  0.00   Min.   : 18990  
    ##  1st Qu.: 17.00   1st Qu.: 48711  
    ##  Median : 58.00   Median : 79994  
    ##  Mean   : 48.09   Mean   :117121  
    ##  3rd Qu.: 72.00   3rd Qu.:225975  
    ##  Max.   :112.00   Max.   :247075  
    ## 

``` r
ggplot(data = sclass550) +
  geom_point(mapping = aes(x=mileage, y = price))
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
ggplot(data = sclass65AMG) +
  geom_point(mapping = aes(x = mileage, y = price))
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-2.png)

``` r
#Train/Test splitting

N550 = nrow(sclass550)
N65AMG = nrow(sclass65AMG)

N550_train = floor(.8*N550)
N550_test = N550 - N550_train
N65AMG_train = floor(.8*N65AMG)
N65AMG_test = N65AMG - N65AMG_train

N550_train_ind = sample.int(N550, N550_train, replace=FALSE)
N65AMG_train_ind = sample.int(N65AMG, N65AMG_train, replace=FALSE)


D550_train = sclass550[N550_train_ind,]
D550_test = sclass550[-N550_train_ind,]
D65AMG_train = sclass65AMG[N65AMG_train_ind,]
D65AMG_test = sclass65AMG[-N65AMG_train_ind,]

D550_test = arrange(D550_test, mileage)
D65AMG_test = arrange(D65AMG_test, mileage)


X_550_train = select(D550_train, mileage)
y_550_train = select(D550_train, price)
X_550_test = select(D550_test, mileage)
y_550_test = select(D550_test, price)

X_65AMG_train = select(D65AMG_train, mileage)
y_65AMG_train = select(D65AMG_train, price)
X_65AMG_test = select(D65AMG_test, mileage)
y_65AMG_test = select(D65AMG_test, price)


#RMSE

rmse = function(y, ypred) {
  sqrt(mean(data.matrix((y-ypred)^2)))
}

#KNN3
knn3_550 = knn.reg(train = X_550_train, test = X_550_test, y = y_550_train, k = 3)
knn3_65AMG = knn.reg(train = X_65AMG_train, test = X_65AMG_test, y = y_65AMG_train, k = 3)



y550pred_knn3 = knn3_550$pred
y65AMGpred_knn3 = knn3_65AMG$pred

rmse(y_550_test, y550pred_knn3)
```

    ## [1] 12247.05

``` r
rmse(y_65AMG_test, y65AMGpred_knn3)
```

    ## [1] 25580.86

``` r
#Testing the fit

D550_test$y550pred_knn3 = y550pred_knn3

p_test550 = ggplot(data = D550_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test550 + geom_path(aes(x = mileage, y = y550pred_knn3), color='blue')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-3.png)

``` r
D65AMG_test$y65AMGpred_knn3 = y65AMGpred_knn3

p_test65AMG = ggplot(data = D65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn3), color='blue')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-4.png)

``` r
#KNN10
knn10_550 = knn.reg(train = X_550_train, test = X_550_test, y = y_550_train, k = 10)
knn10_65AMG = knn.reg(train = X_65AMG_train, test = X_65AMG_test, y = y_65AMG_train, k = 10)



y550pred_knn10 = knn10_550$pred
y65AMGpred_knn10 = knn10_65AMG$pred

rmse(y_550_test, y550pred_knn10)
```

    ## [1] 10854.86

``` r
rmse(y_65AMG_test, y65AMGpred_knn10)
```

    ## [1] 23597.85

``` r
#Testing the fit

D550_test$y550pred_knn10 = y550pred_knn10

p_test550 = ggplot(data = D550_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test550 + geom_path(aes(x = mileage, y = y550pred_knn10), color='green')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-5.png)

``` r
D65AMG_test$y65AMGpred_knn10 = y65AMGpred_knn10

p_test65AMG = ggplot(data = D65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn10), color='green')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-6.png)

``` r
#KNN20
knn20_550 = knn.reg(train = X_550_train, test = X_550_test, y = y_550_train, k = 20)
knn20_65AMG = knn.reg(train = X_65AMG_train, test = X_65AMG_test, y = y_65AMG_train, k = 20)



y550pred_knn20 = knn20_550$pred
y65AMGpred_knn20 = knn20_65AMG$pred

rmse(y_550_test, y550pred_knn20)
```

    ## [1] 10741.31

``` r
rmse(y_65AMG_test, y65AMGpred_knn20)
```

    ## [1] 23246.09

``` r
#Testing the fit

D550_test$y550pred_knn20 = y550pred_knn20

p_test550 = ggplot(data = D550_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test550 + geom_path(aes(x = mileage, y = y550pred_knn20), color='orange')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-7.png)

``` r
D65AMG_test$y65AMGpred_knn20 = y65AMGpred_knn20

p_test65AMG = ggplot(data = D65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn20), color='orange')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-8.png)

``` r
#KNN30
knn30_550 = knn.reg(train = X_550_train, test = X_550_test, y = y_550_train, k = 30)
knn30_65AMG = knn.reg(train = X_65AMG_train, test = X_65AMG_test, y = y_65AMG_train, k = 30)



y550pred_knn30 = knn30_550$pred
y65AMGpred_knn30 = knn30_65AMG$pred

rmse(y_550_test, y550pred_knn30)
```

    ## [1] 10617.56

``` r
rmse(y_65AMG_test, y65AMGpred_knn30)
```

    ## [1] 24079.8

``` r
#Testing the fit

D550_test$y550pred_knn30 = y550pred_knn30

p_test550 = ggplot(data = D550_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test550 + geom_path(aes(x = mileage, y = y550pred_knn30), color='purple')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-9.png)

``` r
D65AMG_test$y65AMGpred_knn30 = y65AMGpred_knn30

p_test65AMG = ggplot(data = D65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn30), color='purple')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-10.png)

``` r
#KNN50
knn50_550 = knn.reg(train = X_550_train, test = X_550_test, y = y_550_train, k = 50)
knn50_65AMG = knn.reg(train = X_65AMG_train, test = X_65AMG_test, y = y_65AMG_train, k = 50)



y550pred_knn50 = knn50_550$pred
y65AMGpred_knn50 = knn50_65AMG$pred

rmse(y_550_test, y550pred_knn50)
```

    ## [1] 10775.3

``` r
rmse(y_65AMG_test, y65AMGpred_knn50)
```

    ## [1] 27316.45

``` r
#Testing the fit

D550_test$y550pred_knn50 = y550pred_knn50

p_test550 = ggplot(data = D550_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test550 + geom_path(aes(x = mileage, y = y550pred_knn50), color='red')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-11.png)

``` r
D65AMG_test$y65AMGpred_knn50 = y65AMGpred_knn50

p_test65AMG = ggplot(data = D65AMG_test) + 
  geom_point(mapping = aes(x = mileage, y = price)) + 
  theme_bw(base_size=18)

p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn50), color='red')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-12.png)

``` r
#550
p_test550 + geom_path(aes(x = mileage, y = y550pred_knn50), color='red') +
  geom_path(aes(x = mileage, y = y550pred_knn3), color='blue') +
  geom_path(aes(x = mileage, y = y550pred_knn10), color='green') +
  geom_path(aes(x = mileage, y = y550pred_knn20), color='orange') +
  geom_path(aes(x = mileage, y = y550pred_knn30), color = 'purple')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-13.png)

``` r
#65AMG
p_test65AMG + geom_path(aes(x = mileage, y = y65AMGpred_knn50), color='red') +
  geom_path(aes(x = mileage, y = y65AMGpred_knn3), color='blue') +
  geom_path(aes(x = mileage, y = y65AMGpred_knn10), color='green') +
  geom_path(aes(x = mileage, y = y65AMGpred_knn20), color='orange') +
  geom_path(aes(x = mileage, y = y65AMGpred_knn30), color='purple')
```

![](Exercises1_files/figure-markdown_github/unnamed-chunk-2-14.png)

I ran K-Nearest Neighbors for values of K equal to 3, 10, 20, 30, and
50. The optimum from these for the 550 trim was K equal to 50. The
optimum for the 65AMG trim was K equal to 30. The 550 trim yields a
larger optimum value of K because there were more observations in the
data set and thus a larger value of K is optimal to create a more
accurate non-parametric model.

The corresponding RMSE values are 10600.26 for 550 trim and 16049.87 for
65AMG trim. These values could be diminished but that accomodates a
significant portion of the sample size we would be dealing with. For 550
trim, the K equal to 50 only accounts for about 12% of the data at any
time to generate the model. For the 65AMG trim, the K equal to 30
accounts for about 10% of the data to generate the model.

Conclusion
----------

This assignment challenged me to think about what I was capable of doing
with data. I was able to apply what we’ve done in class. I had a few
things I wish that I knew how to do (create a heat map of all flights
and the destinations around the world out of Austin. I was able to
generate the map but couldn’t quite figure out how to extend my coding
to recognize locations of airports) but I am happy with what I was able
to do and demonstrate.
