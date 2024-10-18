####  Data Cleaning  ####
d1 = read.csv("Beneficiary_Bank_Data.csv")[,-1]
d2 = read.csv("Remitter_Bank_Data.csv")[,-1]

head(d1)
head(d2)


d2[,5] = as.numeric(substr(d2[,5],1,5))
d2[,6] = as.numeric(substr(d2[,6],1,4))
d2[,7] = as.numeric(substr(d2[,7],1,4))
#d1[,8] = as.numeric(substr(d1[,8],1,4))
#head(d1)
d2[,4] = gsub(",","",d2[,4])
d2[,4] = as.numeric(d2[,4])

d2[,9]=as.numeric(gsub("%","",d2[,9]))


d2[,5] = d2[,5]*d2[,4]/100
d2[,6] = d2[,6]*d2[,4]/100
d2[,7] = d2[,7]*d2[,4]/100
d2[,9] = d2[,9]*d2[,8]/100
#d1[,8] = d1[,8]*d1[,4]/100
head(d2)
d2[49,]
write.csv(d2,"Remitter_Bank_Data.csv")

aggregate(d2[,5]~d2[,3],data=d2,length)
aggregate(d1[,5]~d1[,3],data=d1,length)

library(stringr)
head(d1)
d1[,3] = str_to_title(d1[,3])
d2[,3] = str_to_title(d2[,3])
d1 = read.csv("Beneficiary_Bank_Data.csv")[,-c(1:2)]
head(d1)
d2[which(d2[,3] == "Axis Bank Ltd."),3]="Axis Bank Ltd"
d2[which(d2[,3] == "Citi"),3]="Citibank"
d2[which(d2[,3] == "Citi Bank"),3] = "Citibank"
d2[which(d2[,3] == "Rbl"),3]="Rbl Bank"
d2[which(d2[,3] == "Tri O Tech Solutions Private Limited (Ppi)"),3]="Tri O Tech Solutions Private Limited"
d2[which(d2[,3] == "Fino Payments Bank Limited"),3] = "Fino Payments Bank"
d2[which(d2[,3] == "Fino Payments Bank Limited Fip"),3]="Fino Payments Bank"
d2[which(d2[,3] == "Equitas Bank"),3]="Equitas Small Finance Bank"
d2[which(d2[,3] == "Dbs Bank Ltd"),3] = "Dbs Bank India Limited"


df=data   ## regression data xlsx
### here we clean the vectors contains NA values 
## because pca dont work if there are NA values
vec <- ifelse(df$Total_Volume == "NA", NA, df$Total_Volume)   ###total volume
vec_numeric <- as.numeric(vec)
vec_numeric[is.na(vec_numeric)] <- mean(vec_numeric, na.rm = TRUE)
df$Total_Volume=vec_numeric
mean(vec_numeric)


vec <- ifelse(df$DRA == "NA", NA, df$DRA)   ### Debit reversal amount
vec_numeric <- as.numeric(vec)
vec_numeric[is.na(vec_numeric)] <- mean(vec_numeric, na.rm = TRUE)
df$DRA=vec_numeric
mean(vec_numeric)

###total volume
vec1 <- ifelse(new.transform.data$Total_Volume.ben == "NA", NA, new.transform.data$Total_Volume.ben)
vec_numeric2 <- as.numeric(vec1)
vec_numeric2[is.na(vec_numeric2)] <- mean(vec_numeric2, na.rm = TRUE)
new.transform.data$Total_Volume.ben=vec_numeric2
mean(vec_numeric2)



###### regression analysis

y=new_transform_data$Total_Volume.ben

Total_Volume=df$Total_Volume
DRA=df$DRA
Approve=df$Approve
BD=df$BD
DRS=df$DRS
BD.Ben=df$BD.Ben
Approve.Benefi=df$Approve.Benefi
reg=lm(y ~ 1+Total_Volume+DRA+Approve+BD+DRS+BD.Ben+Approve.Benefi)
summary(reg)


#Top 10 Banks

d11= aggregate(Sl_No.~UPI_Beneficiary_Bank,d1,mean)
d21 =aggregate(Sl_No.~UPI_Remitter_Bank,d2,mean)
colnames(d11)[1] = "Banks"
colnames(d21)[1] = "Banks"
#which(d11$UPI_Beneficiary_Bank %in% intersect(d11[,1],d21[,1]))
#which(d21$UPI_Remitter_Bank %in% intersect(d11[,1],d21[,1]))
fin = merge(d11, d21, by = "Banks", all = TRUE)
colnames(fin)[2:3] = c("Beneficiary_Rank","Remitter Rank") 
fin[,4] = fin[,2] + fin[,3]

top_banks = fin[order(fin[,4]),][1:10,]
colnames(top_banks)[4] = "Combined_Rank" 



#### PCA analysis #############################################################

data=regression_data
data=as.data.frame(regression_data)
Total_vol_rem=data$...5
DRA_rem=na.omit(as.numeric(data$...6))
Approve_rem=na.omit(as.numeric(data$...7))
BD_rem=na.omit(as.numeric(data$...8))
DRS_rem=na.omit(as.numeric(data$...9))
Total_volume_ben=na.omit(as.numeric(data$Response))
Approve_ben=na.omit(as.numeric(data$...11))
BD_ben=na.omit(as.numeric(data$...12))
Total_vol_rem=na.omit(as.numeric(Total_vol_rem))

pca_data=cbind(Total_vol_rem,Total_volume_ben,DRA_rem,Approve_rem,BD_rem,DRS_rem,Approve_ben,BD_ben)
dim(pca_data)
# Assuming your data is stored in a data frame called 'data'
# Remove any rows with missing values
pca_data <- pca_data[complete.cases(pca_data), ]

# Standardize the data
scaled_data <- scale(pca_data)

# Perform PCA
pca_result <- prcomp(scaled_data)

# Summary of PCA results
summary(pca_result)

# Extracting principal component scores
pc_scores <- pca_result$x

# Biplot (optional)
biplot(pca_result)
# Assuming pca_result is the result of PCA obtained using prcomp()

# Extracting standard deviations of each principal component
pc_std_dev <- pca_result$sdev

# Calculating variance explained by each principal component
var_explained <- (pc_std_dev^2) / sum(pc_std_dev^2) * 100
round(var_explained)
# Scree plot
plot(1:length(var_explained), var_explained, type = "b", 
     xlab = "Principal Component", ylab = "Percentage of Variance Explained",
     main = "Scree Plot")
write.xlsx(pca_data,file="pca_data.xlsx")
pca_data=as.data.frame(pca_data)

################################################################################
##### Time series analysis of ben####
df=Top_Beneficiary_Banks
a=unique(df$UPI_Beneficiary_Bank)
df2=df[which(df$UPI_Beneficiary_Bank==a[2]),]
df2=as.data.frame(df2)[,c(2,4,5)]
# Create a scatter plot using ggplot
ggplot(df2, aes(x = 1:30, y = df2$Total_Volume)) +
  geom_point() +  # Add points for the scatter plot
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Scatter Plot of Ben") +
  theme_minimal()  # Use a minimal theme for the plot
# Example time series data (replace this with your actual data)
ts_data_ben <- ts(c(as.numeric(df2$Total_Volume)),
              start = c(2021, 8), frequency = 12)

### decompose ts data
decompose_result_2=decompose(ts_data_ben)
plot(decompose_result_2)

# Perform Augmented Dickey-Fuller (ADF) Test
adf_test_result <- adf.test(decompose_result_2$random[7:24])
print(adf_test_result)
## as it is not stationary for alpha0.01 make it stationary.

# Assuming your original time series data is stored in 'ts_data_ben'
# You can create a new column for the differenced values

# Create a new column for differenced data
ts_data_ben_diff <- c(NA, diff(ts_data_ben))
# Remove rows with NA values from ts_data$differenced
ts_data_ben_diff <- ts_data_ben_diff[!is.na(ts_data_ben_diff)]

# Plot the differenced data
plot(ts_data_ben_diff, type = "l", xlab = "Time", ylab = "Differenced Total Volume", main = "Differenced Time Series of Ben")

# Check stationarity using ADF test
adf_result <- ur.df(ts_data_ben_diff, type = "trend", lags = 10)
summary(adf_result)


# Plot ACF and PACF
ggtsdisplay(ts_data_ben_diff,main="pacf of ben")

plot(decompose_result_2$seasonal)
plot(ts_data_ben_diff)




# Combine plots using grid.arrange (assuming you have the gridExtra package installed)
# Perform time series decomposition
decomposed_ts <- decompose(ts_data_ben)

# Extract decomposed components
trend <- decomposed_ts$trend
seasonal <- decomposed_ts$seasonal
random <- decomposed_ts$random

# Create separate plots for each component and original time series in ggplot 
plot_original <- ggplot(data.frame(Date = time(ts_data_ben), Original = ts_data_ben), aes(x = Date, y = Original)) +
  geom_line() +
  labs(title = "Original Time Series of Ben", x = "Date", y = "Value") +
  theme_minimal()

plot_trend <- ggplot(data.frame(Date = time(ts_data_ben), Trend = trend), aes(x = Date, y = Trend)) +
  geom_line() +
  labs(title = "Trend Component", x = "Date", y = "Value") +
  theme_minimal()

plot_seasonal <- ggplot(data.frame(Date = time(ts_data_ben), Seasonal = seasonal), aes(x = Date, y = Seasonal)) +
  geom_line() +
  labs(title = "Seasonal Component", x = "Date", y = "Value") +
  theme_minimal()

plot_random <- ggplot(data.frame(Date = time(ts_data_ben), Random = random), aes(x = Date, y = Random)) +
  geom_line() +
  labs(title = "Random Component", x = "Date", y = "Value") +
  theme_minimal()


grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             ncol = 2, nrow = 2)

write.xlsx(ts_data_ben,file="time_series_data_ben.xlsx")
#######################################################

##### Time series of rem ####
library(tseries)
library(forecast)
library(ggplot2)
library(urca)
library(lubridate) ### for dates
library(gridExtra)# Combine plots using grid.arrange (assuming you have the gridExtra package installed)



df= as.data.frame(Remitter_Bank_Data)
df2=df[,c(2,4,5)]
a=unique(df$UPI_Remitter_Bank)
sbi_data= df2[which(df2$UPI_Remitter_Bank == a[1]),]

as.data.frame(sbi_data)

# Create a scatter plot using ggplot
ggplot(sbi_data, aes(x = 1:30, y = sbi_data$Total_Volume)) +
  geom_point() +  # Add points for the scatter plot
  labs(x = "X-axis Label", y = "Y-axis Label", title = "Scatter plot of rem") +
  theme_minimal()  # Use a minimal theme for the plot



# Example time series data (replace this with your actual data)
ts_data <- ts(c(1018.15, 1036.45, 1193.24, 1176.24, 1296.56, 1291.88, 1242.45, 1482.61, 1540.38, 1644.91,
                1625.09, 1709.44, 1793.55, 1865.75, 1972.78, 1951.85, 2128.94, 2153.20, 1987.68, 2260.05,
                2298.06, 2427.45, 2415.09, 2574.67, 2707.09, 2730.35, 2930.95, 2861.65, 3073.18, 3128.97),
              start = c(2021, 8), frequency = 12)



### decompose ts data
decompose_result=decompose(ts_data)
plot(decompose_result)
# Perform Augmented Dickey-Fuller (ADF) Test
adf_test_result <- adf.test(decompose_result$random[7:24])
print(adf_test_result)
## as it is not stationary make it stationary.



# Assuming your original time series data is stored in 'ts_data'
# You can create a new column for the differenced values

# Create a new column for differenced data
ts_data_diff <- c(NA, diff(ts_data))
# Remove rows with NA values from ts_data$differenced
ts_data_diff <- ts_data_diff[!is.na(ts_data_diff)]

# Plot the differenced data
plot(ts_data_diff, type = "l", xlab = "Time", ylab = "Differenced Total Volume", main = "Differenced Time Series")

# Check stationarity using ADF test
adf_result <- ur.df(ts_data_diff, type = "trend", lags = 10)
summary(adf_result)


# Plot ACF and PACF
ggtsdisplay(ts_data_diff,main="PACF of remitter")


# Perform time series decomposition
decomposed_ts <- decompose(ts_data)

# Extract decomposed components
trend <- decomposed_ts$trend
seasonal <- decomposed_ts$seasonal
random <- decomposed_ts$random

# Create separate plots for each component and original time series in ggplot 
plot_original <- ggplot(data.frame(Date = time(ts_data), Original = ts_data), aes(x = Date, y = Original)) +
  geom_line() +
  labs(title = "Original Time Series of Rem", x = "Date", y = "Value") +
  theme_minimal()

plot_trend <- ggplot(data.frame(Date = time(ts_data), Trend = trend), aes(x = Date, y = Trend)) +
  geom_line() +
  labs(title = "Trend Component", x = "Date", y = "Value") +
  theme_minimal()

plot_seasonal <- ggplot(data.frame(Date = time(ts_data), Seasonal = seasonal), aes(x = Date, y = Seasonal)) +
  geom_line() +
  labs(title = "Seasonal Component", x = "Date", y = "Value") +
  theme_minimal()

plot_random <- ggplot(data.frame(Date = time(ts_data), Random = random), aes(x = Date, y = Random)) +
  geom_line() +
  labs(title = "Random Component", x = "Date", y = "Value") +
  theme_minimal()

grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             +              ncol = 2, nrow = 2)

grid.arrange(plot_original,plot_trend, plot_seasonal, plot_random,
             ncol = 2, nrow = 2)


write.xlsx(ts_data,file="time_series_remitter_data.xlsx")

##############################################################

#####   K-Mean Clustering   ######

d1 = read.csv("Beneficiary_Bank_Data.csv")
library(factoextra)
head(d1)

unique(d1$UPI_Beneficiary_Bank) # To extract total beneficiary banks names

ben_mean <- aggregate(d1$Total_Volume~d1$UPI_Beneficiary_Bank,d1,mean)

d2 <- read.csv("Remitter_Bank_Data.csv")

rem_mean <-  aggregate(d2$Total_Volume~d2$UPI_Remitter_Bank,d2,mean)

ben_mean <-  as.data.frame(ben_mean)

rem_mean <-  as.data.frame(rem_mean)

cb <- intersect(ben_mean[,1],rem_mean[,1])

bindex <-  numeric(length=length(cb))

rindex <-  bindex

for(i in 1:length(cb))
{
bindex[i] = which(ben_mean[,1] == cb[i])
rindex[i] = which(rem_mean[,1] == cb[i])
}

t = cbind(ben_mean[bindex,],rem_mean[rindex,])

df = cbind(t[,2],t[,4])

rownames(df) = t[,1]

colnames(df) = c("Beneficiary Volume","Remitter Volume")

clusters <- kmeans(df, 5)

fviz_cluster(clusters, df)

######################################################################################

#######     Asymptotic Distribution     #######

######   Remitter Data  ######

rem_data <- read.csv("Top_Remitter_Banks.csv")  # To load the data set

x1 <- rem_data$Time # To extract the column of time 

x2 <- rem_data$Total_Volume

x3 <- rem_data$UPI_Remitter_Bank

data1 <- cbind(x1,x2,x3) 

df <- as.data.frame(data1) 

colnames(df) <- c("Month", "Amount", "Bank")

a <- unique(df$Bank) # To get total Banks

sbi_rem <- df[which(df$Bank == a[1]),] # This will give rows in first two columns which corresponds to the bank a[1]

sbi_rem_volume <- as.numeric(sbi_rem[,2]) # This will give volumes corresponds to bank - a[1]

###################

hdfc_rem <- df[which(df$Bank == a[2]),]

hdfc_rem_volume <- as.numeric(hdfc_rem[,2])

###################

paytm_rem <- df[which(df$Bank == a[6]),]

paytm_rem_volume <- as.numeric(paytm_rem[,2])

#################

Baroda_rem <- df[which(df$Bank == a[4]),]

Baroda_rem_volume <- as.numeric(Baroda_rem[,2])

##################

icici_rem <- df[which(df$Bank == a[5]),]

icici_rem_volume <- as.numeric(icici_rem[,2])

##############  Beneficiary Data  ##################

Ben_data <- read.csv("Top_Beneficiary_Banks.csv")

y1 <- Ben_data$Time

y2 <- Ben_data$UPI_Beneficiary_Bank

y3 <- Ben_data$Total_Volume

data2 <- cbind(y1,y2,y3)

dataframe <- as.data.frame(data2)

colnames(dataframe) <- c("month","Bank","Volume")

u <- unique(dataframe$Bank)

sbi_Ben <- dataframe[which(dataframe$Bank == u[2]),]

sbi_Ben_volume <- as.numeric(sbi_Ben[,3])

##############

hdfc_Ben <- dataframe[which(dataframe$Bank == u[5]),]

hdfc_Ben_volume <- as.numeric(hdfc_Ben[,3])

###############

paytm_Ben <- dataframe[which(dataframe$Bank == u[1]),]

paytm_Ben_volume <- as.numeric(paytm_Ben[,3])

################

Baroda_Ben <- dataframe[which(dataframe$Bank == u[6]),]

Baroda_Ben_volume <- as.numeric(Baroda_Ben[,3])

################

icici_Ben <- dataframe[which(dataframe$Bank == u[3]),]

icici_Ben_volume <- as.numeric(icici_Ben[,3])

################

t1 = cbind(c(u[2],u[5],u[1],u[6],u[3]),c(shapiro.test(sbi_rem_volume)$p,shapiro.test(hdfc_rem_volume)$p,shapiro.test(paytm_rem_volume)$p,shapiro.test(Baroda_rem_volume)$p,shapiro.test(icici_rem_volume)$p))

t1[,2] = round(as.numeric(t1[,2]),2)

t2 = cbind(c(u[2],u[5],u[1],u[6],u[3]),c(shapiro.test(sbi_Ben_volume)$p,shapiro.test(hdfc_Ben_volume)$p,shapiro.test(paytm_Ben_volume)$p,shapiro.test(Baroda_Ben_volume)$p,shapiro.test(icici_Ben_volume)$p))

t2[,2] = round(as.numeric(t2[,2]),2)

shapiro = cbind(t1,t2[,2])

colnames(shapiro) = c("Banks","Remitter p-value","Benif p-value")
shapiro

write.csv(shapiro,"shapiro test.csv")

####################################################

### Mean and Variance of volumes ###

# Remitter 

rem <-  cbind(sbi_rem_volume,hdfc_rem_volume,paytm_rem_volume,Baroda_rem_volume,icici_rem_volume)

rem <-  rem/1000

a <- rbind(apply(rem,2,mean),apply(rem,2,var))

# Beneficiary

Ben <- cbind(sbi_Ben_volume,hdfc_Ben_volume,paytm_Ben_volume,Baroda_Ben_volume,icici_Ben_volume)

Ben <- Ben/1000

b <-  rbind(apply(Ben,2,mean),apply(Ben,2,var))

write.csv(rbind(a,b),"Top 5 Banks Summary.csv")



