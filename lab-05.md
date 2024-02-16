Lab 05 - La Quinta is Spanish for next to Denny’s, Pt. 2
================
Eric
2.14.24

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
data(dennys, package = "dsbox")
data(laquinta, package = "dsbox")
```

### Exercise 1 and 2

> Filter the Denny’s data frame for Alaska (AK) and save the result as
> dn_ak. How many Denny’s locations are there in Alaska?

> Filter the La Quinta data frame for Alaska (AK) and save the result as
> lq_ak. How many La Quinta locations are there in Alaska?

``` r
dn_ak <- dennys %>%
  filter(state == "AK")
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

``` r
nrow(lq_ak)
```

    ## [1] 2

There are 3 Alaska Dennys locations and 2 Alaska Laquinta locations.

### Exercise 3

> How many pairings are there between all Denny’s and all La Quinta
> locations in Alaska, i.e., how many distances do we need to calculate
> between the locations of these establishments in Alaska?

``` r
dn_lq_ak <- full_join(dn_ak, lq_ak, by = "state")
```

    ## Warning in full_join(dn_ak, lq_ak, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_ak
```

    ## # A tibble: 6 × 11
    ##   address.x     city.x state zip.x longitude.x latitude.x address.y city.y zip.y
    ##   <chr>         <chr>  <chr> <chr>       <dbl>      <dbl> <chr>     <chr>  <chr>
    ## 1 2900 Denali   Ancho… AK    99503       -150.       61.2 3501 Min… "\nAn… 99503
    ## 2 2900 Denali   Ancho… AK    99503       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 3 3850 Debarr … Ancho… AK    99508       -150.       61.2 3501 Min… "\nAn… 99503
    ## 4 3850 Debarr … Ancho… AK    99508       -150.       61.2 4920 Dal… "\nFa… 99709
    ## 5 1929 Airport… Fairb… AK    99701       -148.       64.8 3501 Min… "\nAn… 99503
    ## 6 1929 Airport… Fairb… AK    99701       -148.       64.8 4920 Dal… "\nFa… 99709
    ## # ℹ 2 more variables: longitude.y <dbl>, latitude.y <dbl>

This combined the two data sets. Note each observation is no longer
either dennys or laquinta establishments, but both are down twice. The
first two locations are Dennys \#1, the next two Denny’s \#2, etc. Rows
1, 3, and 5 are the first Laquinta location, and rows 2, 4, and 6 are
the second Laquinta location.

### Exercise 4

> How many observations are in the joined dn_lq_ak data frame? What are
> the names of the variables in this data frame.

There are now 6 rows, or observations, if you define “observation” as a
combination of Dennys and Laquintas. Column 3 is “State” –\> this only
appears once because it is the variable we joined by and is identical in
the two data sets. Columns 1, 2, 4, 5, and 6 are the address, city, zip,
longitude, and latitude for Dennys, respectively, and columns 7-11 are
the corresponding variables for the Laquinta establishment.

### Exercise 5

> What function from the tidyverse do we use the add a new variable to a
> data frame while keeping the existing variables?

I think it’s “mutate.” That’s the only function that I remember using to
create new variables!

### Exercise 6a

``` r
haversine <- function(long1, lat1, long2, lat2, round = 3) {
  # convert to radians
  long1 <- long1 * pi / 180
  lat1 <- lat1 * pi / 180
  long2 <- long2 * pi / 180
  lat2 <- lat2 * pi / 180

  R <- 6371 # Earth mean radius in km

  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((long2 - long1) / 2)^2
  d <- R * 2 * asin(sqrt(a))

  return(round(d, round)) # distance in km
}
```

The ability to create functions is very nice!

### Exercise 6b

> Calculate the distances between all pairs of Denny’s and La Quinta
> locations and save this variable as distance. Make sure to save this
> variable in THE dn_lq_ak data frame so that you can use it later.

``` r
dn_lq_ak_distance <- dn_lq_ak %>%
   mutate(distance = haversine (longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
```

Done. I saved it as a new data frame because I wasn’t sure how to save
it in the previous data frame, and I’m worried about unintentionally
making changes to the original data frame.

### Exercise 7

> Calculate the minimum distance between a Denny’s and La Quinta for
> each Denny’s location. To do so we group by Denny’s locations and
> calculate a new variable that stores the information for the minimum
> distance.

``` r
dn_lq_ak_mindist <- dn_lq_ak_distance %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

This worked, though it deleted the other variables from the data set.

### Exercise 8

> Describe the distribution of the distances Denny’s and the nearest La
> Quinta locations in Alaska. Also include an appripriate visualization
> and relevant summary statistics.

``` r
dn_lq_ak_mindist %>% summarise(mean = mean(closest), median = median(closest), sd = sd(closest))
```

    ## # A tibble: 1 × 3
    ##    mean median    sd
    ##   <dbl>  <dbl> <dbl>
    ## 1  4.41   5.20  2.10

``` r
ggplot(dn_lq_ak_mindist, aes(y = closest)) + 
  geom_boxplot()
```

![](lab-05_files/figure-gfm/distribution%20of%20minimum%20distance-1.png)<!-- -->

The data points are: 2.035, 5.197, and 5.998. I’m not sure how else to
describe the distribution of 3 data points! But I’ve computed a box-plot
graph and some summary statistics. I see the next problems have states
with more establishments, so these will be more useful then.

### Exercise 9

> Repeat the same analysis for North Carolina: (i) filter Denny’s and La
> Quinta Data Frames for NC, (ii) join these data frames to get a
> complete list of all possible pairings, (iii) calculate the distances
> between all possible pairings of Denny’s and La Quinta in NC, (iv)
> find the minimum distance between each Denny’s and La Quinta location,
> (v) visualize and describe the distribution of these shortest
> distances using appropriate summary statistics.

``` r
dn_nc <- dennys %>%
  filter(state == "NC")
lq_nc <- laquinta %>%
  filter(state == "NC")
dn_lq_nc <- full_join(dn_nc, lq_nc, by = "state")
```

    ## Warning in full_join(dn_nc, lq_nc, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_nc_distance <- dn_lq_nc %>%
   mutate(distance = haversine (longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
```

``` r
dn_lq_nc_mindist <- dn_lq_nc_distance %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

``` r
dn_lq_nc_mindist %>% summarise(mean = mean(closest), median = median(closest), sd = sd(closest))
```

    ## # A tibble: 1 × 3
    ##    mean median    sd
    ##   <dbl>  <dbl> <dbl>
    ## 1  65.4   53.5  53.4

Ok, so the graphical depiction here was harder. I wanted a grouped
histogram, which turned out to be much harder than I expected it to be.
I ended up constructing the following, with the help of ChatGPT. It
seems like there has to be an easier way. Leaving open the question of
whether a grouped histogram is a good graphical choice, I’d like to
discuss if there’s an easier way to make one Everything I tried had some
issues, which then had work-arounds, which produced more issues (e.g.,
when recoding, it then used those as labels, so that needed to be redone
… but then I (think) it wasn’t considering distance as a continuous
variable, so left off situations were n was 0, so that needed to be
fixed). Anyway, i’m glad I got it to work, but it would be worth
discussing if there’s a better choice for producing this type of
histogram.

``` r
dn_lq_nc_mindist <- dn_lq_nc_mindist %>%
  mutate(closest_grouped = case_when(
    closest > 0 & closest <= 20 ~ 1,
    closest > 20 & closest <= 40 ~ 2,
    closest > 40 & closest <= 60 ~ 3,
    closest > 60 & closest <= 80 ~ 4,
    closest > 80 & closest <= 100 ~ 5,
    closest > 100 & closest <= 120 ~ 6,
    closest > 120 & closest <= 140 ~ 7,
    closest > 140 & closest <= 160 ~ 8,
    closest > 160 & closest <= 180 ~ 9,
    closest > 180 & closest <= 200 ~ 10
  ))
dn_lq_nc_mindist$closest_grouped <- factor(dn_lq_nc_mindist$closest_grouped, 
                                           levels = 1:10,
                                           labels = c("0-20", "20-40", "40-60", "60-80", 
                                                      "80-100", "100-120", "120-140", 
                                                      "140-160", "160-180", "180-200"))
dn_lq_nc_mindist_complete <- dn_lq_nc_mindist %>%
  count(closest_grouped) %>%
  complete(closest_grouped, fill = list(n = 0))
ggplot(dn_lq_nc_mindist_complete, aes(x = closest_grouped, y=n)) + 
  geom_bar(stat="identity") +
  labs(
    title = "Minimum Distance between Dennys and Laquinta in NC",
    x = "minimum distance (Haversine)", y = "Count"
  )
```

![](lab-05_files/figure-gfm/constructing%20a%20grouped%20histogram%20for%20NC-1.png)<!-- -->

It’s difficult for me to evaluate this, not fully understanding what the
Haversine distance represents, but it looks like most are not
particularly close, at least not in terms of the joke of being right
next to each other. There’s also some right skew -\> once they get far
away, they may be quite far away.

### Exercise 10

> Repeat the same analysis for Texas.

``` r
dn_tx <- dennys %>%
  filter(state == "TX")
lq_tx <- laquinta %>%
  filter(state == "TX")
dn_lq_tx <- full_join(dn_tx, lq_tx, by = "state")
```

    ## Warning in full_join(dn_tx, lq_tx, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_tx_distance <- dn_lq_tx %>%
   mutate(distance = haversine (longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
```

``` r
dn_lq_tx_mindist <- dn_lq_tx_distance %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

``` r
dn_lq_tx_mindist %>% summarise(mean = mean(closest), median = median(closest), sd = sd(closest))
```

    ## # A tibble: 1 × 3
    ##    mean median    sd
    ##   <dbl>  <dbl> <dbl>
    ## 1  5.79   3.37  8.83

Now making the graph.

``` r
dn_lq_tx_mindist <- dn_lq_tx_mindist %>%
  mutate(closest_grouped = case_when(
    closest > 0 & closest <= 20 ~ 1,
    closest > 20 & closest <= 40 ~ 2,
    closest > 40 & closest <= 60 ~ 3,
    closest > 60 & closest <= 80 ~ 4,
    closest > 80 & closest <= 100 ~ 5,
    closest > 100 & closest <= 120 ~ 6,
    closest > 120 & closest <= 140 ~ 7,
    closest > 140 & closest <= 160 ~ 8,
    closest > 160 & closest <= 180 ~ 9,
    closest > 180 & closest <= 200 ~ 10
  ))
dn_lq_tx_mindist$closest_grouped <- factor(dn_lq_tx_mindist$closest_grouped, 
                                           levels = 1:10,
                                           labels = c("0-20", "20-40", "40-60", "60-80", 
                                                      "80-100", "100-120", "120-140", 
                                                      "140-160", "160-180", "180-200"))
dn_lq_tx_mindist_complete <- dn_lq_tx_mindist %>%
  count(closest_grouped) %>%
  complete(closest_grouped, fill = list(n = 0))
ggplot(dn_lq_tx_mindist_complete, aes(x = closest_grouped, y=n)) + 
  geom_bar(stat="identity") +
  labs(
    title = "Minimum Distance between Dennys and Laquinta in Texas",
    x = "minimum distance (Haversine)", y = "Count"
  )
```

![](lab-05_files/figure-gfm/constructing%20a%20grouped%20histogram%20for%20Texas-1.png)<!-- -->

Well, this is entirely different! The Dennys and Laquintas are much more
clustered together. The most obvious difference is the (much) smaller
central tendency. Seems to still be a bit of positive skew. (As an
aside, I tried to compute the skewness statistic, but couldn’t figure
out how to.)

### Exercise 12

> Repeat the same analysis for a state of your choosing, different than
> the ones we covered so far.

I’ll choose Connecticut

``` r
dn_ct <- dennys %>%
  filter(state == "CT")
lq_ct <- laquinta %>%
  filter(state == "CT")
dn_lq_ct <- full_join(dn_ct, lq_ct, by = "state")
```

    ## Warning in full_join(dn_ct, lq_ct, by = "state"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 1 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

``` r
dn_lq_ct_distance <- dn_lq_ct %>%
   mutate(distance = haversine (longitude.x, latitude.x, longitude.y, latitude.y, round = 3))
```

``` r
dn_lq_ct_mindist <- dn_lq_ct_distance %>%
  group_by(address.x) %>%
  summarize(closest = min(distance))
```

``` r
dn_lq_ct_mindist %>% summarise(mean = mean(closest), median = median(closest), sd = sd(closest))
```

    ## # A tibble: 1 × 3
    ##    mean median    sd
    ##   <dbl>  <dbl> <dbl>
    ## 1  21.3   21.5  13.1

Now making the graph.

``` r
dn_lq_ct_mindist <- dn_lq_ct_mindist %>%
  mutate(closest_grouped = case_when(
    closest > 0 & closest <= 20 ~ 1,
    closest > 20 & closest <= 40 ~ 2,
    closest > 40 & closest <= 60 ~ 3,
    closest > 60 & closest <= 80 ~ 4,
    closest > 80 & closest <= 100 ~ 5,
    closest > 100 & closest <= 120 ~ 6,
    closest > 120 & closest <= 140 ~ 7,
    closest > 140 & closest <= 160 ~ 8,
    closest > 160 & closest <= 180 ~ 9,
    closest > 180 & closest <= 200 ~ 10
  ))
dn_lq_ct_mindist$closest_grouped <- factor(dn_lq_ct_mindist$closest_grouped, 
                                           levels = 1:10,
                                           labels = c("0-20", "20-40", "40-60", "60-80", 
                                                      "80-100", "100-120", "120-140", 
                                                      "140-160", "160-180", "180-200"))
dn_lq_ct_mindist_complete <- dn_lq_ct_mindist %>%
  count(closest_grouped) %>%
  complete(closest_grouped, fill = list(n = 0))
ggplot(dn_lq_ct_mindist_complete, aes(x = closest_grouped, y=n)) + 
  geom_bar(stat="identity") +
  labs(
    title = "Minimum Distance between Dennys and Laquinta in Delaware",
    x = "minimum distance (Haversine)", y = "Count"
  )
```

![](lab-05_files/figure-gfm/constructing%20a%20grouped%20histogram%20for%20Delaware-1.png)<!-- -->

So, despite being a much smaller state, they are still (on average) more
spread out in Connecticut than in Texas.
