# Reproducible Research: Peer Assessment 1




## Loading and preprocessing the data

### Dataset processing steps

```r
#unzip
unzip("activity.zip")

#read file
activity<-read.csv("activity.csv", stringsAsFactors = F)

#convert date to POSIXct
activity$date<- as.POSIXct(activity$date)

#display summary
summary(activity)
```

```
##      steps             date                        interval     
##  Min.   :  0.00   Min.   :2012-10-01 00:00:00   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16 00:00:00   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31 00:00:00   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31 00:25:34   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15 00:00:00   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30 00:00:00   Max.   :2355.0  
##  NA's   :2304
```



## What is mean total number of steps taken per day?

###Total number of steps per day###

```r
#plot total number os steps by date
ggplot(activity, aes(x=date, y=steps)) + geom_histogram(stat = 'identity')
```

![](PA1_template_files/figure-html/TotalNrSteps-1.png) 


### Mean and Median of total daily number of steps###

```r
#calculate summaries by date
v<-activity %>% group_by(date) %>% summarise(n_steps = sum(steps, na.rm = T))
```

Dataset Mean: **9354.2295082** and  Median: **10395**

```r
#alternatively -- dispay mean/median in dataframe
data.frame(Measure = c("Mean", "Median"), Values = c(mean(v$n_steps), median(v$n_steps)))
```

```
##   Measure   Values
## 1    Mean  9354.23
## 2  Median 10395.00
```



## What is the average daily activity pattern?

### Daily Activity Pattern ###

```r
ggplot(activity, aes(x=interval, y=steps)) + stat_summary(fun.y="mean", geom="line")
```

![](PA1_template_files/figure-html/DailyActivityPattern-1.png) 

### Interval with Maximum Avg number of steps###

```r
v<-activity %>% group_by(interval) %>%summarise( n_steps = mean(steps, na.rm=TRUE))
#display interval with maximum number of steps
v[v$n_steps == max(v$n_steps), ]
```

```
## Source: local data frame [1 x 2]
## 
##   interval  n_steps
##      (int)    (dbl)
## 1      835 206.1698
```

```r
#alternatively -- display top 10 intervals
head( v[ order(desc(v$n_steps)), ], 5 )
```

```
## Source: local data frame [5 x 2]
## 
##   interval  n_steps
##      (int)    (dbl)
## 1      835 206.1698
## 2      840 195.9245
## 3      850 183.3962
## 4      845 179.5660
## 5      830 177.3019
```


## Imputing missing values
### Total Number of **NA** values in dataset is 2304

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
## alternatively,: summary(activity) reports number of NA's
```

### Imputing Strategy: use mean number of steps per interval to substitute for missing values

```r
#get means by interval
activity_sm<-activity %>% group_by(interval) %>% summarise_each(funs(mean(., na.rm =T)), steps)

#merge interval means with activity dataframe
activity_mrg<-merge(activity, activity_sm, by = "interval")

#substitute interval value if NA 
activity_mm<-transform(activity_mrg, steps = ifelse(is.na(steps.x), steps.y, steps.x))

#get activity imputed
activity_imputed<- select(activity_mm[order(activity_mm$date),], date, interval, steps)
```

###Total number of steps per day. Imputed Dataset###
![](PA1_template_files/figure-html/DailyNrStepsImputed-1.png) 

### Mean and Median of total daily number of steps in imputed dataset###

```r
#calculate summaries by date
v<-activity_imputed %>% group_by(date) %>% summarise(n_steps = sum(steps))
```
Dataset Mean: **1.0766189\times 10^{4}** and  Median: **1.0766189\times 10^{4}**


```r
#alternatively, display mean/median in data frame
data.frame(Measure = c("Mean", "Median"), Values = c(mean(v$n_steps), median(v$n_steps)))
```

```
##   Measure   Values
## 1    Mean 10766.19
## 2  Median 10766.19
```
Using imputed values in the dataset increases mean of total steps taken per day.

## Are there differences in activity patterns between weekdays and weekends?

```r
#create weekend/weekday factor
activity_wd<-transform(activity_imputed, d_type = ifelse( weekdays(date) == 'Saturday' | weekdays(date) == 'Sunday', 'weekend', 'weekday' ))
```

### Weekend/Weekday Activity pattern comparison
![](PA1_template_files/figure-html/WeekdayActivityComp-1.png) 

