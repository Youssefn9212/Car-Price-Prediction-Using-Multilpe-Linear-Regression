# List of required packages
required_packages <- c("tidyverse", "car", "caret", "MASS", "ggplot2", "psych", "dplyr", "olsrr", "fpc")

# Check and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
library(dplyr)
library(tidyverse)
library(car)
library(caret)
library(MASS)
library(ggplot2)
library(psych)
library(olsrr)
library(fpc)

#Loading the dataset
file_path <- "D:/youssef/Desktop/AUC/Fall 2024/Regression/Project/Dataset/CarPrice_Assignment.csv"
data <- read_csv(file_path)

# Inspect the data
str(data)
summary(data)


#Preprocessing the data
#Dropping Car ID
data <- data[, !colnames(data) %in% "car_ID"]

#Dealing with Car Name
data$car_brand <- sapply(strsplit(as.character(data$CarName), " "), `[`, 1)
data$car_brand <- gsub("-", "_", data$car_brand)
data$car_brand <- tolower(data$car_brand)
brand_counts <- table(data$car_brand)
# Replace infrequent brands with "Other"
data$car_brand <- ifelse(data$car_brand %in% names(brand_counts[brand_counts > 1]),
                         data$car_brand, "Other")
# View unique categories after fixing
unique(data$car_brand)
# Create a mapping for redundant categories
brand_mapping <- list(
  maxda = "mazda",
  vw = "volkswagen"
)
# Replace redundant categories
data$car_brand <- ifelse(data$car_brand %in% names(brand_mapping),
                         unlist(brand_mapping[data$car_brand]),
                         data$car_brand)
# View the updated data
head(data$car_brand)


#dropping car name
data <- data[, !colnames(data) %in% "CarName"]


unique(data$fuelsystem)
#correcting the typos
data$fuelsystem[which(data$fuelsystem=="spfi")]<-"spdi"
data$fuelsystem[which(data$fuelsystem=="mfi")]<-"mpfi"

head(data$cylindernumber)
data<-data%>%
  mutate(cylindernumber=case_when(
    cylindernumber=="eight" ~ 8,
    cylindernumber=="twelve" ~ 12,
    cylindernumber=="five" ~ 5,
    cylindernumber=="four" ~ 4,
    cylindernumber=="two" ~ 2,
    cylindernumber=="three" ~ 3,
    cylindernumber=="six"~6
  ))
head(data$doornumber)
data<-data%>%
  mutate(doornumber=case_when(
    doornumber=="two" ~ 2,
    doornumber=="four" ~ 4,
  ))

#Graphs before fitting
# Boxplot of price
boxplot(data$price, horizontal = TRUE, main = "Boxplot of Price", xlab = "Price")

# Histogram of price
hist(data$price, breaks = 30, main = "Histogram of Price", xlab = "Price", col = "lightblue", border = "black")

# Statistical summary of the price variable
summary(data$price)

windows() 
pairs(data[, sapply(data, is.numeric)],
      main = "Pairs Plot of Numerical Columns",
      pch = 19, col = "blue")


#Preparing for Linear regression
column_types <- sapply(data, class)
print(column_types)

#converting the categorical variables into factor type
data$fueltype<-as.factor(data$fueltype)
data$aspiration<-as.factor(data$aspiration)
data$carbody<-as.factor(data$carbody)
data$drivewheel<-as.factor(data$drivewheel)
data$enginelocation<-as.factor(data$enginelocation)
data$enginetype<-as.factor(data$enginetype)
data$fuelsystem<-as.factor(data$fuelsystem)
data$car_brand<-as.factor(data$car_brand)

#creating dummy variables for for each categorical variable in the dataset
data[sapply(data,is.factor)]<-data.matrix(data[sapply(data,is.factor)])



#Model 1
# Fit an initial linear regression model
reg1 <- lm(price ~ ., data = data)

# Summary of the regression model
summary(reg1)

#checking assumptions
n=nrow(data)        # Sample size
model=reg1$call    # Fitted model
betahat=reg1$coef  # Estimated regression coefficients
yhat=reg1$fitted   # Fitted values
yhat=fitted(reg1)  # Fitted values
e=reg1$resid       # Ordinary residuals
IStudRes=rstandard(reg1) # Internally studentized residuals
EStudRes=rstudent(reg1)  # Externally Studentized Residuals
p=reg1$rank-1      # Number of predictors
d.f=reg1$data      # Residuals degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg1),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
plot(reg1$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(ols_leverage(reg1),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg1)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)

# Print unique values of the car_brand column
unique(data$car_brand)



#converting the categorical variables into binary variables
df_new=cat2bin(data,categorical=c(2,3,5,6,7,13,16,25))$data
df_new=as.data.frame(df_new)

# List of car brands
car_brands <- c("alfa_romero", "audi", "bmw", "chevrolet", "dodge", "honda", "isuzu", 
                "jaguar", "volkswagen", "mazda", "buick", "Other", "mitsubishi", 
                "nissan", "peugeot", "plymouth", "porsche", "renault", "saab", 
                "subaru", "toyota", "volvo")

# Updated names vector
names(df_new) <- c("symboling", "diesel", "gas", "std", "turbo", "doornumber", 
                   "convertible", "hardtop", "hatchback", "sedan", "wagon", "fourwd", 
                   "fwd", "rwd", "front", "rear", "wheelbase", "carlength", "carwidth", 
                   "carheight", "curbweight", "dohc", "dohcv", "l", "ohc", "ohcf", 
                   "ohcv", "rotor", "cylindernumber", "enginesize", "1bbl", "twobbl", 
                   "fourbbl", "idi", "mpfi", "spdi", "boreratio", "stroke", 
                   "compressionratio", "horsepower", "peakrpm", "citympg", "highwaympg", 
                   "price", car_brands)

# Drop the specified baseline variables
df_new <- df_new[, -which(names(df_new) %in% c("gas", "std", "doornumber", "sedan", "fwd", "front", "ohc", "mpfi", "subaru"))]

# Confirm the updated names
print(names(df_new))

#Model 2
reg2=lm(df_new$price~.,data=df_new)
summary(reg2)

# Drop the columns "idi" and "saab"
df_new <- df_new[, !colnames(df_new) %in% c("idi", "saab")]

reg2=lm(df_new$price~.,data=df_new)
summary(reg2)


#checking assumptions
n=nrow(df_new)        # Sample size
model=reg2$call    # Fitted model
betahat=reg2$coef  # Estimated regression coefficients
yhat=reg2$fitted   # Fitted values
yhat=fitted(reg2)  # Fitted values
e=reg2$resid       # Ordinary residuals
IStudRes=rstandard(reg2) # Internally studentized residuals
EStudRes=rstudent(reg2)  # Externally Studentized Residuals
p=reg2$rank-1      # Number of predictors
d.f=reg2$df_new      # Residusla degrees of freedom = n - p - 1

op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(EStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(rstandard(reg2),pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
plot(reg2$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)

op <- par(mfrow = c(2,2)) 
plot(ols_leverage(reg2),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg2)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)


# Define lambda values for the ladder of transformation
lambda <- c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)

# Function to apply the ladder of transformation and plot
power.transf2 <- function(df_new, lambda) {
  op <- par(mfrow = c(3, 3))  # Arrange plots in a 3x3 grid
  for (i in seq_along(lambda)) {
    df1 <- df_new
    # Apply the transformation for each lambda
    if (lambda[i] == 0) {
      df1$price <- log(df_new$price)  # Log transformation for lambda = 0
    } else {
      df1$price <- df_new$price^lambda[i]
    }
    # Fit the regression model for transformed price
    reg <- lm(price ~ ., data = df1)
    IStudRes <- rstandard(reg)  # Internally studentized residuals
    # Plot residuals vs transformed price
    plot(df1$price, IStudRes,
         pch = 19, 
         main = paste("Lambda =", lambda[i]),
         xlab = "Transformed Price", 
         ylab = "IStudRes")
  }
  par(op)  # Reset plot settings
}

# Apply the ladder of transformation and generate plots
power.transf2(df_new, lambda)

dev.new(width = 12, height = 12)  # Opens a new plotting window

df_transformed=df_new
df_transformed$price=log(df_transformed$price)

reg3=lm(df_transformed$price ~.,data=df_transformed)
summary(reg3)
# Fit the regression model with log-transformed price
reg3 <- lm(df_transformed$price ~ ., data = df_transformed)

#Checking assumptions
n <- nrow(df_transformed)         # Sample size
model <- reg3$call                # Fitted model
betahat <- reg3$coef              # Estimated regression coefficients
yhat <- reg3$fitted               # Fitted values
e <- reg3$resid                   # Ordinary residuals
IStudRes <- rstandard(reg3)       # Internally studentized residuals
EStudRes <- rstudent(reg3)        # Externally studentized residuals
p <- reg3$rank - 1                # Number of predictors
d.f <- reg3$df.residual           # Residual degrees of freedom = n - p - 1

op <- par(mfrow = c(2, 2)) 
hist(IStudRes, xlab = "IStudRes", main = "Histogram of IStudRes")
qqnorm(IStudRes, ylab = "IStudRes", pch = 19, main = "Normal Q-Q Plot")
qqline(IStudRes, col = "red")
plot(rstandard(reg3), pch = 19, xlab = "Index", ylab = "IStudRes", main = "Index Plot of Residuals")
plot(reg3$fitted, IStudRes, pch = 19, xlab = "Fitted Values", ylab = "IStudRes", main = "Residuals vs Fitted Values")
par(op)  

for(j in 1:p){ 
  if (is.numeric(df_transformed[, j + 1])) {
    windows()
    plot(df_transformed[,j+1],IStudRes,pch=19,xlab = names(df_transformed)[j+1],main=paste("Plot of IStudRes v.",names(df_transformed)[j+1]),ylab = "IStudRes")
  }
}

# Plot Cook's Distance
#IDENTIFY NOT WORKING WALLAHI
cooks_d <- cooks.distance(reg3)
plot(cooks_d, pch = 19, xlab = "Index", ylab = "Cook's Distance", main = "Cook's Distance")
abline(h = 4 / nrow(df_transformed), col = "red", lty = 2)

# Add labels with jitter to avoid overlap
text(x = jitter(seq_along(cooks_d)), 
     y = jitter(cooks_d), 
     labels = ifelse(cooks_d > 4 / nrow(df_transformed), seq_along(cooks_d), ""), 
     pos = 4, cex = 0.7, col = "blue")


# Calculate Hadi's influence using olsrr package
Hadi_influence <- Hinf$hadi  # Assuming Hinf is already calculated

# Plot Hadi's influence
plot(Hadi_influence, pch = 19, ylab = "Hadi's Influence", main = "Hadi's Influence Plot")
abline(h = 2 * mean(Hadi_influence), col = "red", lty = 2)  # Add a reference line for context

# Annotate each point with its observation number
text(x = seq_along(Hadi_influence), y = Hadi_influence, 
     labels = seq_along(Hadi_influence), pos = 4, cex = 0.7, col = "blue")

op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg3),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg3),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg3)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)



#Dropping outliers
df_new2=df_transformed[-c(3, 126, 127, 17, 50, 135, 75),]
reg4=lm(df_new2$price~.,data=df_new2)
summary(reg4)


#Checking assumptions for reg4
n <- nrow(df_transformed)         # Sample size
model <- reg4$call                # Fitted model
betahat <- reg4$coef              # Estimated regression coefficients
yhat <- reg4$fitted               # Fitted values
e <- reg4$resid                   # Ordinary residuals
IStudRes <- rstandard(reg4)       # Internally studentized residuals
EStudRes <- rstudent(reg4)        # Externally studentized residuals
p <- reg4$rank - 1                # Number of predictors
d.f <- reg4$df.residual           # Residual degrees of freedom = n - p - 1

# Create diagnostic plots
op <- par(mfrow = c(2, 2)) 
hist(IStudRes, xlab = "IStudRes", main = "Histogram of IStudRes for reg4")
qqnorm(IStudRes, ylab = "IStudRes", pch = 19, main = "Normal Q-Q Plot for reg4")
qqline(IStudRes, col = "red")
plot(rstandard(reg4), pch = 19, xlab = "Index", ylab = "IStudRes", main = "Index Plot of Residuals for reg4")
plot(reg4$fitted, IStudRes, pch = 19, xlab = "Fitted Values", ylab = "IStudRes", main = "Residuals vs Fitted Values for reg4")
par(op)



#Scaling data
df_new2=scale(df_new2)  
df_new2=as.data.frame(df_new2)

vif(reg3)

X = df_new2[, !colnames(df_new2) %in% "price"]
X=as.matrix(X)
R=cor(X)
e=eigen(R)
L=e$val
V=e$vec
kappa=sqrt(L[1]/L)
kappa


W=X%*%V  # Principal components
reg_pcr <- lm(df_new2$price ~ W)
summary_pcr <- summary(reg_pcr)



pc_pvalues <- summary_pcr$coefficients[-1, 4]  # Exclude intercept, extract p-values
significant_pcs <- which(pc_pvalues <= 0.05)  # Identify indices of significant PCs
print("Significant PCs:")
print(significant_pcs)


alpha <- as.matrix(reg_pcr$coef[-1])  # Remove intercept
beta_pc <- V[, significant_pcs] %*% alpha[significant_pcs]  # Use significant PCs only

y_hat_pcr <- X %*% beta_pc

plot(y_hat_pcr, df_new2$price, pch = 19,
     xlab = "Fitted Values (PCR - Significant PCs)", 
     ylab = "Price",
     main = "Price vs. Fitted Values - PCR (Significant PCs)")
abline(0, 1, col = "red")  # Reference line y = x

W_significant <- W[, significant_pcs]
reg_pcr_refit <- lm(df_new2$price ~ W_significant)
summary_pcr_refit <- summary(reg_pcr_refit)
summary_pcr_refit
