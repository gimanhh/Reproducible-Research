setwd('C:/Users/gilma/Desktop/Data Science/Especialización/Data Science/Reproducible Research/Peer Assessment 1')


#Loading and preprocessing the data
#Show any code that is needed to
#1. Load the data (i.e. read.csv())
   data<-read.csv("activity.csv",header=T,sep=',')
#2. Process/transform the data (if necessary) into a format suitable for your analysis
   data[,2]<-as.Date(data$date)
   colnames(data)<-c('Steps','Date','Interval')
   attach(data)

#What is mean total number of steps taken per day?
#For this part of the assignment, you can ignore the missing values in the dataset.
#1. Calculate the total number of steps taken per day
   TotalSteps<-tapply(Steps,Date,sum,na.rm=T)

#2. If you do not understand the difference between a histogram and a barplot,
#research the difference between them. Make a histogram of the total number of steps taken each day
   hist(TotalSteps,col='grey',main="HISTOGRAM OF THE TOTAL NUMBER\nOF STEPS TAKEN EACH DAY",xlab='Steps')
   abline(v=c(mean(TotalSteps),median(TotalSteps)),col=c(2,4),lwd=2)
   text(5000,c(20,17.5),c('Mean = 9354.23','Median = 10395'),col=c(2,4))

#3. Calculate and report the mean and median of the total number of steps taken per day
   mean(TotalSteps)
   median(TotalSteps)

#What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
#   and the average number of steps taken, averaged across all days (y-axis)
   AverageSteps<-tapply(Steps,Interval,mean,na.rm=T)
   FiveMinInt<-as.numeric(names(AverageSteps))
   plot(FiveMinInt,AverageSteps,type='l',
      main='AVERAGE NUMBER OF STEPS TAKEN\nACROSS ALL DAYS BY 5-MINUTE INTERVAL',
      ylab='Average Number of Steps', xlab='5-minute Interval')

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
   names(AverageSteps[which(AverageSteps==max(AverageSteps))])

#Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as NA).
#The presence of missing days may introduce bias into some calculations or summaries of the data.
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
   table(is.na(Steps))[2]

#2. Devise a strategy for filling in all of the missing values in the dataset.
#The strategy does not need to be sophisticated. For example, you could use the mean/median
#for that day, or the mean for that 5-minute interval, etc.
   DataBoost<-function(Data)
   {
      set.seed(999)
      Medians<-tapply(Data[,1],Data[,3],median,na.rm=T);   
      interval<-as.numeric(names(Medians));
      for(i in 1:length(interval))
      {
         for(j in 1:length(Data[,1]))
         {
            if(is.na(Data[j,1])==T && Data[j,3]==interval[i])
            {
               Data[j,1]<-sample(rpois(1000,Medians[i]),1);
            }
         }
      }
      Data;
   }

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
   DataComplete<-DataBoost(data)
   head(data)
   head(DataComplete)

   detach(data)
   attach(DataComplete)
#4. Make a histogram of the total number of steps taken each day and Calculate and report
#the mean and median total number of steps taken per day. Do these values differ from
#the estimates from the first part of the assignment? What is the impact of imputing missing data
#on the estimates of the total daily number of steps?
TotalBootsSteps<-tapply(Steps,Date,sum)
mean(TotalBootsSteps)
median(TotalBootsSteps)

hist(TotalBootsSteps,col='grey',main="HISTOGRAM OF THE TOTAL NUMBER\nOF STEPS TAKEN EACH DAY (Booststrapped)",xlab='Steps')
abline(v=c(mean(TotalBootsSteps),median(TotalBootsSteps)),col=c(2,4),lwd=2)
text(5000,c(20,17.5),c('Mean = 9501.92','Median = 10395'),col=c(2,4))

#Are there differences in activity patterns between weekdays and weekends?
#For this part the weekdays() function may be of some help here.
#Use the dataset with the filled-in missing values for this part.
#1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend"
#   indicating whether a given date is a weekday or weekend day.
   DataComplete$WeekDay<-ifelse(strftime(Date,"%u")=="6" | strftime(Date,"%u")=="7",1,0)
   DataComplete$WeekDay<-as.factor(DataComplete$WeekDay)
   levels(DataComplete$WeekDay)<-c('weekday','weekend')

#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval
#   (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
   Weekday<-subset(DataComplete,WeekDay=='weekday')
   Weekend<-subset(DataComplete,WeekDay=='weekend')

   AverageBootsSteps<-tapply(Steps,Interval,mean)
   FiveMinIntTotal<-as.numeric(names(AverageBootsSteps))

   AverageWeekendSteps<-tapply(Weekend[,1],Weekend[,3],mean)
   AverageWeekdaySteps<-tapply(Weekday[,1],Weekday[,3],mean)

   par(mfrow=c(3,1))
   plot(FiveMinIntTotal,AverageBootsSteps,type='l',
        main='AVERAGE NUMBER OF STEPS TAKEN\nACROSS ALL DAYS BY 5-MINUTE INTERVAL (Bootstraped)',
        ylab='Average Number of Steps', xlab='5-minute Interval')
   grid(10,5,lwd=1,col='gray60')
   
   plot(FiveMinIntTotal,AverageWeekdaySteps,type='l',col=2,
        main='AVERAGE NUMBER OF STEPS TAKEN\nACROSS WEEKDAYS BY 5-MINUTE INTERVAL',
        ylab='Average Number of Steps', xlab='5-minute Interval')
   grid(10,5,lwd=1,col='gray60')
   
   plot(FiveMinIntTotal,AverageWeekendSteps,type='l',col=4,
        main='AVERAGE NUMBER OF STEPS TAKEN\nACROSS WEEKENDS BY 5-MINUTE INTERVAL',
        ylab='Average Number of Steps', xlab='5-minute Interval')
   grid(10,5,lwd=1,col='gray60')


