---
output:
  pdf_document: default
---
---
title: "FA 582 Homework 1"
author: 'Shaheer Sidd (CWID: 20036003)'
output:
  pdf_document: default

```{r setup, include=FALSE}
getwd()
#setwd("C:/Users/hp/Desktop/Stevens/Semester 1/FA582 Data Science/Assignments/Assignment_1/HW1_S25_data")
knitr::opts_knit$set(root.dir = "C:/Users/hp/Desktop/Stevens/Semester 1/FA582 Data Science/Assignments/Assignment_1/HW1_S25_data")

library(openxlsx)
library(dplyr)
library(tidyr)
library(naniar)
library(ggplot2)
library(pheatmap)
library(lubridate)
library(doBy)
library(ggcorrplot)


```

## Problem 1

Use the datasets provided for Bronx, Brooklyn, Manhattan, Queens, and Staten Island. Do the following:

### 1.1) Load, clean, and merge the data in a single dataframe.

```{r}
df_bronx_21 <- read.xlsx("2021_bronx.xlsx", 1, startRow=7)
df_brooklyn_21 <- read.xlsx("2021_brooklyn.xlsx", 1, startRow=7)
df_manhattan_21 <- read.xlsx("2021_manhattan.xlsx", 1, startRow=7)
df_queens_21 <- read.xlsx("2021_queens.xlsx", 1, startRow=7)
df_staten_island_21 <- read.xlsx("2021_staten_island.xlsx", 1, startRow=7)

df_bronx_22 <- read.xlsx("2022_bronx.xlsx", 1, startRow=7)
df_brooklyn_22 <- read.xlsx("2022_brooklyn.xlsx", 1, startRow=7)
df_manhattan_22 <- read.xlsx("2022_manhattan.xlsx", 1, startRow=7)
df_queens_22 <- read.xlsx("2022_queens.xlsx", 1, startRow=7)
df_staten_island_22 <- read.xlsx("2022_staten_island.xlsx", 1, startRow=7)

#Merging
df_ny_21 <- rbind(df_bronx_21,df_brooklyn_21,df_manhattan_21,df_queens_21,df_staten_island_21)
df_ny_21$Year <- 2021 # Adding Year Column 

df_ny_22 <- rbind(df_bronx_22,df_brooklyn_22,df_manhattan_22,df_queens_22,df_staten_island_22)
df_ny_22$Year <- 2022 #Adding Year Column 

df_ny <- rbind(df_ny_21,df_ny_22)

#Cleaning
## Lowercase all Column Names
names(df_ny) <- tolower(names(df_ny))

## Converting BOROUGH codes to respective States
df_ny <- df_ny %>%
  mutate(borough_name = case_when(
    borough == 1 ~ "Manhattan",  
    borough == 2 ~ "Bronx",
    borough == 3 ~ "Brooklyn",
    borough == 4 ~ "Queens",
    borough == 5 ~ "Staten_Island",
    TRUE ~ "Unknown")) 

##Removing EASE-MENT due to 100% Missing Values
df_ny <- df_ny %>% select(-"ease-ment")

##Sales Date
df_ny$sale.date <- convertToDateTime(as.numeric(df_ny$sale.date))
```

### 1.2) Conduct exploratory data analysis in order to find out where there are outliers or missing values, decide how you will treat them, make sure the dates are formatted correctly, make sure values you think are numerical are being treated as such, etc.

#### Missing Values Heatmap
```{r}
suppressWarnings(vis_miss(df_ny,warn_large_data=FALSE)+
  scale_fill_manual(values=c("Navyblue","Orange3"),labels=c("Present","Missing"))+
  labs(fill="Legend")+ggtitle("Missing Values Heatmap"))

```



#### Comment
We can see in the heatmap above which illustrates missing values.Additionally, the outlier analysis is made part of the next section while converting certain columns into relevant formats was done as part of previous section 1.1.


### 1.3) Conduct exploratory data analysis to visualize and make comparisons for residential building category classes across boroughs and across time (analyze the following: 1-, 2-, and 3-family homes, coops, and condos). Use histograms, boxplots, scatterplots or other visual graphs. Provide summary statistics along with your conclusions.

```{r}
##1.3)Exploratory Data Analysis
df_eda <- df_ny[grepl("FAMILY|COOP|COOPS|CONDO|CONDOS", df_ny$building.class.category), ]
hist(df_eda$sale.price)
```



#### Identifying Outliers

```{r}
df_sale <- df_eda[df_eda$sale.price!=0,]
plot(df_sale$gross.square.feet,df_sale$sale.price)
```



#### Comment
The above plot looks very concentrated at a particular point. Taking log of both axis before plotting would be more informative in a visualization.

```{r}
plot(log(df_sale$gross.square.feet),log(df_sale$sale.price))
```


#### Comment
Outliers or garbage values can be identified as points on the plot that do not follow the pattern and make no sense. In the plot above sale prices of houses less than $6k is almost impossible. By removing these we can get a better idea of our dataset not impacted by outliers or garbage data.
```{r}
df_sale$outliers <- (log(df_sale$sale.price) <= 6) 
df_sale_1 <- df_sale[which(df_sale$outliers==0 & df_sale$gross.square.feet>0),]
#dim(df_sale_1)
plot(log(df_sale_1$gross.square.feet),log(df_sale_1$sale.price))
```




#### EDA by Boroughs (Boxplot)




```{r}
ggplot(df_sale_1,aes(x = borough_name,y=log(sale.price),fill=borough_name))+
  geom_boxplot(alpha = 0.8)+
  labs(title="Box Plot of Sale Prices by Borough",x="Borough",y="Log of Sale Prices")+
  theme_minimal()+theme(legend.position="none") 
```


#### Comment

Boxplots are an effective way to visualize data distribution. By grouping house prices by borough, we gain a clearer understanding of how they vary across different areas. The boxplot above illustrates the five-number summary for each borough: the lowest points represent the minimum sale prices, the lower edge of the box marks the first quartile, the middle line within the box indicates the median price, and the upper edge represents the third quartile. The maximum values, including outliers, are also displayed.  

From the visualization, it is evident that Manhattan is significantly more expensive compared to the other boroughs, which have relatively similar sale prices.


#### Time Series Analysis of Sale Prices by Building Categories




```{r}
df <- df_sale_1 %>%
  mutate(Month=floor_date(sale.date,"month"), Year=year(sale.date), MonthYear = format(Month,"%Y-%m")) %>%
  filter(building.class.category %in% c("01 ONE FAMILY DWELLINGS","02 TWO FAMILY DWELLINGS","03 THREE FAMILY DWELLINGS","11 SPECIAL CONDO BILLING LOTS")) 

sales_trends <- df %>%
  group_by(Month, building.class.category) %>%
  summarize(Average_Sale_Price=median(sale.price,na.rm=TRUE), .groups="drop")

ggplot(sales_trends,aes(Month,Average_Sale_Price, color=building.class.category)) +
  geom_line(linewidth=1.39)+ 
  labs(title = "Sales Price Trend by Building Category",x="Month",y="Median Sale Price",color = "Building Category")+theme_minimal()
```




#### Comment
The sale prices for all types of family dwellings have remained relatively stable, with little volatility. However, special condo prices have shown significant fluctuations, likely due to shifting demand for luxury housing during COVID. For real estate investors who can afford category three family dwellings, long-term investment in special condos could be a worthwhile consideration, especially since their prices converged toward the end of 2022.

## Problem 2
The datasets provided nyt1.csv, nyt2.csv, and nyt3.csv represents three (simulated) days of ads shown and clicks recorded on the New York Times homepage. Each row represents a single user. There are 5 columns: age, gender (0=female, 1=male), number impressions, number clicks, and logged-in. Use R to handle this data. Perform some exploratory data analysis

### Merging & Cleaning Data
```{r}
nyt_1 <- read.csv("nyt1.csv")
nyt_1$Day <- 1 
nyt_2 <- read.csv("nyt2.csv")
nyt_2$Day <- 2 
nyt_3 <- read.csv("nyt3.csv")
nyt_3$Day <- 3 
nyt <- rbind(nyt_1,nyt_2,nyt_3)  #Binding Data

nyt$Gender <- as.factor(nyt$Gender)
nyt$Day <-as.factor(nyt$Day)
nyt$Signed_In <- as.factor(nyt$Signed_In)
```

### 2.1) Create a new variable, age_group, that categorizes users as “<20”, “20-29”, “30-39”, “40-49”, “50-59”, “60-69”, and “70+”.

```{r}
nyt$age_group <- cut(nyt$Age,breaks = c(-Inf,19,29,39,49,59,69,Inf),labels =c("<20","20-29","30-39","40-49","50-59","60-69","70+"))
head(nyt)
```

### 2.2) For each day: a) Plot the distribution of number of impressions and click-through-rate (CTR =clicks /impressions) for these age categories

```{r}
nyt$CTR <- round(nyt$Clicks/nyt$Impressions,digits=3)  # Click through Rate
siterange <- function(x){c(length(x),min(x),mean(x),max(x))}
summaryBy(Impressions~age_group,data=nyt,FUN=siterange)
```


#### Distribution of Impressions by age category




```{r}
ggplot(nyt,aes(x=age_group,y=Impressions,fill=age_group))+geom_boxplot() +
  labs(title="Distribution of Impressions by Age Group",x="Age_Group",y="Impressions") +
  theme_minimal()+theme(legend.position="none")
```



#### Comment
The spread of impressions across each age group are almost similar. It seems the ads shown are not optimized to a particular age group and is used as a marketing treatment for all viewers.

#### Distribution of Click through Rate by age category




```{r}
nyt <- nyt[is.finite(nyt$CTR), ]

ggplot(nyt,aes(x= age_group,y=(CTR),fill=age_group))+geom_boxplot() +
  labs(title="Distribution of CTR by Age Group",x="Age Group",y="CTR")+
  theme_minimal()+theme(legend.position ="none")
```




#### Comment
The Click through rate(CTR) for all the age groups do not look very promising. Since number of impressions for each age group is similar, the clicks are not happening across all age groups resulting in a low CTR. We can explore the Clicks across dataset in the following sections to understand more regarding the efficacy of current NYT marketing treatments/ ads.


### b) Define a new variable to segment or categorize users based on their click behavior.

```{r}
nyt$Click_Behavior<-cut(nyt$CTR,breaks=c(-Inf,0,0.3,1,Inf),labels=c("No Clicks","Low Clicks","Moderate Clicks","High Clicks"))
head(nyt)
```


```{r}
ggplot(nyt,aes(x=Click_Behavior,fill=Click_Behavior)) +
  geom_bar()+labs(title ="Time Interval Between Extreme Events",x="Time Interval Category",y="Frequency") +
  theme_minimal()+theme(legend.position="none")
```

#### Comment
The above barplot shows that majority of the visiters do not find the ads enticing enough to click. We have very negligible activity in low clicks behaviour having CTR(0-0.3) & moderate click behavior having CTR(0.3-1).


### c) Explore the data and make visual and quantitative comparisons across user segments/demographics (<20-year-old males versus <20-year-old females or logged-in versus not, for example)

```{r}
under_20 <- nyt %>% filter(Age < 20)  #under 20 age people

gender_summary <- under_20 %>%
  group_by(Gender) %>%
  summarise(
    avg_impressions = mean(Impressions),avg_clicks = mean(Clicks),avg_ctr = mean(CTR,),count = n())
print(gender_summary)
```

#### Comment
It's interesting to note that females (Gender = 0) make up the majority of the dataset compared to males (Gender = 1). However, despite this difference, the average impressions, clicks, and CTR remain the same for both genders. Given that female viewers outnumber male viewers by approximately 10 times, ad optimization strategies could be tailored more effectively to engage this dominant audience segment.


#### CTR Distibution by gender and <20 peeps

```{r}
ggplot(under_20,aes(x=Gender,y=CTR,fill=Gender)) +
  geom_boxplot()+
  labs(title = "CTR Distribution by Gender (<20 years old)", y = "CTR")+theme_minimal()
```



#### Comment
The CTR for less than 20 years across both genders is negligible & similar which means that data is being not deployed for marketing ads effectively. Female gender has 10 times more frequency and should have a better CTR as discussed in previous problem section.

#### Impressions Distribution by Signed-In Status for <20 ppl

```{r}
ggplot(nyt,aes(x=Signed_In,y=Impressions,fill=Signed_In)) +
  geom_boxplot()+
  labs(title = "Impressions Distribution by Signed-In Status")+theme_minimal()
```



#### Comment
The impressions for less than 20 years across both Signed In vs anonymous is similar, which means that data is either not being captured for signed in users or not deployed for marketing ads effectively.


#### Correlation Heatmap for Quant Comparison

```{r}
cor_matrix <- cor(nyt %>% select(Impressions, Clicks, CTR), use = "complete.obs")
ggcorrplot(cor_matrix, lab = TRUE)
```


#### Comment
Clicks & CTR should have a high positive association by logic, however a zero association btw impressions and CTR tells us that other factors such as ad quality, targeting or engagement would play a better role in improving CTR.

### 2.3)Extend your analysis across days. Visualize some metrics and distributions over time.
```{r}
daily_metrics <- nyt %>%
  group_by(Day) %>%
  summarise(total_impressions= sum(Impressions),total_clicks= sum(Clicks),avg_CTR= mean(Clicks/Impressions))

ggplot(daily_metrics,aes(x=Day,y=total_impressions,group=1)) +
  geom_line(color="Blue",size=1.3) +
  geom_point(color="Red") +
  labs(title="Total Impressions Over Time",x="Day",y="No. of Impressions")+theme_minimal()

```



#### Clicks over time


```{r}
ggplot(daily_metrics,aes(x=Day,y=total_clicks,group=1)) +
  geom_line(color="Blue",size=1.3) +
  geom_point(color="Red")+
  labs(title="Total Clicks Over Time",x="Day",y="Number of Clicks")+theme_minimal()

```




#### CTR over time

```{r}
ggplot(daily_metrics,aes(x=Day,y=avg_CTR,group=1))+geom_line(color="Purple",size=1)+geom_point(color="Orange") +
  labs(title ="Average CTR Over Time",x="Day",y="Click-Through Rate (CTR)")+theme_minimal()
```



#### Comment
Over the three days, we observe a consistent decline in average impressions and clicks. However, CTR begins to rise from Day 2 onward. This increase in CTR is not necessarily a positive indicator, as the drop in impressions was significantly larger than the decline in clicks, causing a slight inflation in CTR on Day 3. The time series graph may be misleading due to the limited number of days analyzed.
