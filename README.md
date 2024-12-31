# Car Price Prediction

This project aims to develop a robust regression model to predict car prices based on various features such as engine size, horsepower, mileage, and more. By addressing challenges like multicollinearity, non-linearity, and outliers, the study explores different regression techniques to identify the most effective model for car price prediction.

---

## Project Contents

1. [Problem Statement](#problem-statement)
2. [Dataset](#dataset)
   - [Source & Background](#source--background)
   - [Preprocessing](#preprocessing)
3. [Regression Models](#regression-models)
   - [Model 1](#model-1)
   - [Model 2](#model-2)
   - [Model 3](#model-3)
   - [Model 4: PCA](#model-4-pca)
4. [Conclusion](#conclusion)

---

## Problem Statement

Predicting car prices accurately is a challenging task due to the wide range of car features and variations across models. This project focuses on determining the most influential attributes that affect car prices and building a reliable regression model to predict prices based on these attributes. The ultimate goal is to assist manufacturers and buyers in evaluating car prices effectively.

---

## Dataset

### Source & Background

The dataset was obtained from [Kaggle](https://www.kaggle.com/datasets/hellbuoy/car-price-prediction) and includes data from a Chinese car company aiming to enter the US market. The dataset contains 26 variables and 205 observations, with 1 response variable (price) and 25 predictor variables.


### Preprocessing

- **Handling Categorical Variables**: Encoded variables like `fueltype` and `carbody` into binary indicators.
- **Feature Engineering**: Extracted car brand from `CarName` and corrected typos.
- **Addressing Multicollinearity**: Applied Principal Component Analysis (PCA) to reduce redundancy among highly correlated predictors.

---

## Regression Models

### Model 1
- Included all predictor variables.
- Results:
  - Adjusted R²: 88.6%
  - Significant issues with normality, homoscedasticity, and multicollinearity.

### Model 2
- Addressed multicollinearity by dropping redundant predictors.
- Results:
  - Adjusted R²: 96.18%
  - Improved residual plots but retained minor deviations from normality.

### Model 3
- Applied power transformation (log) to response variable.
- Results:
  - Adjusted R²: 96.33%
  - Assumptions satisfied, but multicollinearity persisted.

### Model 4: PCA
- Used Principal Component Regression to address multicollinearity.
- Results:
  - Adjusted R²: 88.2%
  - Used only 3 principal components, significantly reducing model complexity.

---

## Conclusion

Principal Component Regression (PCR) proved to be an effective solution to mitigate multicollinearity while maintaining strong predictive power. Despite a slight reduction in R² compared to other models, PCR achieved better stability and interpretability. This project highlights the importance of addressing regression assumptions to build reliable and robust predictive models.
