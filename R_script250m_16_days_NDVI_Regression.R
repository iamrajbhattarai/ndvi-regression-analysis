
#### Script to plot MODIS collection 6 subset data
#### Alison Boyer - ORNL DAAC
#### April 2017

rm(list = ls()) # clear the workspace

#### Input: statistics file from the MODIS subsetting tool. The filename of the statistics file is structured as: stat_fname <- paste("statistics_", product, ".csv", sep=''). Note: this script must be run from the same directory that contains the statistics file.
#### product examples: "250m_16_days_NDVI", "250m_16_days_EVI", "Nadir_Reflectance_Band4", "shortwave_actual", "LST_Night_1km"
product <- "250m_16_days_NDVI" # set the product (and band)

#### Outputs: 
#### 1. plot of subset mean value and variation over time
#### 2. plot of subset mean values summarized by month
#### 3. plot of subset mean values summarized by year
#### 4. frequency histogram plot of all subset area mean values
#### 5. timeseries plot with years stacked and a less smoothed fit curve for each year
#### 6. plot subset area mean grouped by year and month

# ---------
#### Load required packages. You may need to run install.packages('package') if you have not used these before.
library(ggplot2) #package for plotting
library(viridis) # package for nice color scheme

#### Read in data

# summary(subset_stats) # get basic summary information about the data
head(monthfile)

#### Calculating regression, coefficient of determination and Plotting

monthfile <- read.csv(file = '../statistics_250m_16_days_NDVIMeanbyMonth.csv', head=TRUE, sep=",")
#### Figure 7: linear regression data including each 16 day: NDVI mean is a dark line and dark points are mean NDVI values 
F7 <- ggplot(monthfile, aes(x=month, y = mean)) + 
  geom_line(colour = "#2D708EFF", size = 1, aes(y=mean)) + # draw the line for the mean value
  geom_point(size=3, color= "orange") +
  stat_smooth(method = "lm", formula = y ~ x, level= 0.95, col = "red") + # linear regression
  ggtitle("Monthly Linear Regression of NDVI") + # add a title
  scale_x_discrete(limits=c("Jan","Feb","Mar","Apr","May", "Jun", "Jul", "Aug", "Sep","Oct", "Nov", "Dec")) +
  ylab(product) + # add a y-axis label
  xlab("Date") + # add a x-axis label
  theme_bw() + # set the plot theme
  theme(plot.title = element_text(lineheight=.8, face="bold", size=20), axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x = element_text(size=16), axis.text.y = element_text(size=16)) # set optional theme elements

F7
