
rm(list = ls()) # clear the workspace

product <- "250m_16_days_NDVI" # set the product (and band)

#### 10 Outputs: 

# ---------
#### Load required packages. You may need to run install.packages('package') if you have not used these before.
library(ggplot2) #package for plotting
library(viridis) # package for nice color scheme

#### Read in data
stat_fname <- paste("../statistics_", product, ".csv", sep='')
subset_stats <- read.csv(stat_fname, head=TRUE, sep=",")
summary(subset_stats) # get basic summary information about the data

# Format dates and add to dataframe
dates <- as.Date(subset_stats$date)
years <- format(dates, format = "%Y")
n_yrs <- length(unique(years)) # calculate number of years in subset
months <- format(dates, format = "%m")
monthAbbrev <- format(dates, format = "%b")
DOY <- as.numeric(substr(subset_stats$date.YYYYDDD.,6,8)) #to find the day of year (DOY), take the last three digits of date.YYYYDDD. which is an eight digit string

# find the 68% confidence intervals around the mean
y1 <- subset_stats$mean + subset_stats$standard_deviation 
y2 <- subset_stats$mean - subset_stats$standard_deviation

# add to main data frame
subset_stats <- cbind(subset_stats, DOY, months, years, y1, y2)

# -------
#### Plotting

#### Figure 1: mean and variation timeseries: subset mean is a dark line and 68% CI is a shaded region  
F1 <- ggplot(subset_stats, aes(x=as.Date(date), y = mean)) + 
  geom_ribbon(aes(ymin=y2, ymax=y1), fill = "#99ccff") + # draw the shaded area for 68% CI
  geom_line(colour = "#000000", aes(y=mean)) + # draw the line for the mean value
  ggtitle("NDVI Mean and 68% Confidence Interval") + # add a title
  ylab("NDVI") + # add a y-axis label
  xlab("Date (16 Days)") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=15), axis.title.y = element_text(face="bold", size=15), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements

F1
ggsave(paste("R_MODIS_Plots_", product,"_Fig1_timeseries.png", sep=''), width=25, height=16, units='cm')

#### Figure 2: mean monthly values for the subset
F2 <- ggplot(subset_stats, aes(factor(months), mean)) +
  geom_boxplot(fill="#2D708EFF") + 
  geom_jitter(width = 0) + 
  ggtitle("NDVI Values by Month with boxplot") + 
  scale_x_discrete(breaks= months, labels=monthAbbrev) + 
  xlab("Month") + 
  ylab("NDVI values in different years of a month") + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), legend.position="none", axis.title.x = element_text(face="bold", size=15), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=15), axis.text.y = element_text(size=16)) +
  guides(fill=FALSE)

F2
ggsave(paste("R_MODIS_Plots_", product,"_Fig2_monthly.png", sep=''), width=25, height=16, units='cm')

#### Figure 3: mean yearly values for the subset
F3 <- ggplot(subset_stats, aes(factor(years), mean)) +
  geom_boxplot(fill="#2D708EFF") + 
  geom_jitter(width = 0) + 
  ggtitle("NDVI Values by Year with boxplot") + 
  scale_x_discrete(breaks= years, labels=years) + 
  xlab("Year") + 
  ylab("NDVI values in different months of a year") + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), legend.position="none", axis.title.x = element_text(face="bold", size=15), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=15), axis.text.y = element_text(size=16)) +
  guides(fill=FALSE)

F3
ggsave(paste("R_MODIS_Plots_", product,"_Fig3_yearly.png", sep=''), width=25, height=16, units='cm')

#### Figure 4: frequency histogram of all subset mean values 
F4 <- ggplot(subset_stats, aes(x=mean)) + 
  geom_histogram(bins=20, fill="#ffcc99", color="white") + 
  ggtitle("Distribution of Subset Mean Values") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
  ylab("Frequency in 494 observations ") + 
  xlab("NDVI values") + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), legend.position="none", axis.title.x = element_text(face="bold", size=15), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=15), axis.text.y = element_text(size=16))

F4
ggsave(paste("R_MODIS_Plots_", product,"_Fig4_hist.png", sep=''), width=25, height=16, units='cm')

#### Figure 5: timeseries with years stacked and a loess smoothed fit curve for each year
F5 <- ggplot(subset_stats, aes(x=DOY, y=mean, color=years)) + 
  scale_color_viridis(discrete=T) + 
  geom_point(size=3) + 
  geom_smooth(aes(factor=years), se=FALSE, size=1.2) + 
  ggtitle("NDVI Mean Value by Day of Year") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20)) + 
  xlab("Day of Year") + 
  ylab("NDVI values in different years") + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=15), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=15), axis.text.y = element_text(size=16))    

F5
ggsave(paste("R_MODIS_Plots_", product,"_Fig5_stackedYears.png", sep=''), width=25, height=16, units='cm')

#### Figure 6: subset area mean grouped by year and month
F6 <- ggplot(subset_stats, aes(factor(months), mean)) +
  geom_boxplot(fill="#2D708EFF") + 
  geom_jitter(width = 0) +  
  facet_wrap(~ years, ncol=3) +
  ggtitle("NDVI Values by Month for Each Year with Boxplot") + 
  scale_x_discrete(breaks=months, labels=monthAbbrev) + 
  xlab("Month") + 
  ylab("NDVI values") + 
  theme_bw() +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), legend.position="none", axis.title.x = element_text(face="bold", size=15), axis.text.x = element_text(angle=90, vjust=0.5, size=16), axis.title.y = element_text(face="bold", size=15), axis.text.y = element_text(size=5))
  
F6
ggsave(paste("R_MODIS_Plots_", product,"_Fig6_month_year.png", sep=''), width=25, height=16, units='cm')



#### Calculating regression, coefficient of determination and Plotting


#### Figure 7: linear regression data including each 16 day: NDVI mean is a dark line and dark points are mean NDVI values 
F7 <- ggplot(subset_stats, aes(x=as.Date(date), y = mean)) + 
  geom_point(size=1, color= "black") +
  geom_line(colour = "#2D708EFF", aes(y=mean)) + # draw the line for the mean value
  stat_smooth(method = "lm", formula = y ~ x, level= 0.95, col = "red") + # linear regression
  ggtitle("Simple Linear Regression Of NDVI with Date") + # add a title
  ylab("NDVI values") + # add a y-axis label
  xlab("Date (Every 16 days)") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=15), axis.title.y = element_text(face="bold", size=15), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements

F7
ggsave(paste("R_MODIS_Plots_", product,"_Fig7_linear_regression.png", sep=''), width=25, height=16, units='cm')


##################################################

monthfile <- read.csv(file = '../statistics_250m_16_days_NDVIMeanbyMonth.csv', head=TRUE, sep=",")
#### Figure 7: Monthly Linear Regression of NDVI (95% CI)
F8 <- ggplot(monthfile, aes(x=month, y = mean)) + 
  geom_line(colour = "#2D708EFF", size = 1, aes(y=mean)) + # draw the line for the mean value
  geom_point(size=3, color= "orange") +
  stat_smooth(method = "lm", formula = y ~ x, level= 0.95, col = "red") + # linear regression
  ggtitle("Monthly Linear Regression of NDVI (95% CI)") + # add a title
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec")) +
  ylab("Average NDVI over the years") + # add a y-axis label
  xlab("Month") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements
F8
ggsave(paste("R_MODIS_Plots_", product,"_Fig8_monthly_regression.png", sep=''), width=25, height=16, units='cm')


#### Figure 9: Simple Linear Regression of NDVI with Day of Year (95% CI)
F9 <- ggplot(subset_stats, aes(x=DOY, y = mean)) + 
  geom_point(size=3, color= "orange") +
  # geom_spline(aes(x = DOY, y = mean, color="red", size=1)) +
  stat_smooth(method = "lm", formula = y ~ x, level= 0.95, col = "red") + 
  geom_line(colour = "#2D708EFF", aes(y=mean)) + # draw the line for the mean value
  ggtitle("Simple Linear Regression of NDVI with Day of Year (95% CI)") + # add a title
  ylab("NDVI values in different years") + # add a y-axis label
  xlab("Day of Year") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=15), axis.title.y = element_text(face="bold", size=15), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements

F9
ggsave(paste("R_MODIS_Plots_", product,"_Fig9_linear_regression_DOY.png", sep=''), width=25, height=16, units='cm')

#### Figure 10: Cubic Spline Regression of NDVI with Day of Year  
F10 <- ggplot(subset_stats, aes(x=DOY, y = mean)) + 
  geom_point(size=3, color= "darkgray") +
  geom_spline(aes(x = DOY, y = mean, color="cubic spline", size=0.5)) +
  geom_line(colour = "#2D708EFF", aes(y=mean)) + # draw the line for the mean value
  ggtitle("Cubic Spline Regression of NDVI with Day of Year") + # add a title
  ylab("NDVI values") + # add a y-axis label
  xlab("Date (16 Days)") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=15), axis.title.y = element_text(face="bold", size=15), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements

F10
ggsave(paste("R_MODIS_Plots_", product,"_Fig10_cubicsplineDOY.png", sep=''), width=25, height=16, units='cm')


#calculation yearwise
lmNdvi = lm(mean~as.Date(date), data = subset_stats) #Create the linear regression
summary(lmNdvi) #Review the results

#calculation yearwise
lmNdvi2 = lm(mean~months, data = subset_stats) #Create the linear regression
summary(lmNdvi2) #Review the results


#calculation monthwise
lmNdvi3 = lm(mean~DOY, data = subset_stats) #Create the linear regression
summary(lmNdvi3) #Review the results


