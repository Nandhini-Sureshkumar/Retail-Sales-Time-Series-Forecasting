rm(list = ls())

options(spipen = 999)
library(fredr)
library(fpp3)
library(plotly)
library(scales)
library(latex2exp)
library(quantmod)
library(dplyr)
library(prophet)
#Fred HW


#RSAFSNA

# Setting FRED API key
fredr_set_key("9c0b4e09225a5896568a90948fb2eb54")  


################ DATA Extraction #####################

# Retrieving E-commerce sales are included in the total monthly sales estimates.
# Retail Sales: Retail Trade and Food Services (MRTSSM44X72USN)
us_rs_food_serv <- fredr(
  series_id = "MRTSSM44X72USN",  
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)

us_CPI <- fredr(
  series_id = "CPIAUCNS",  
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)


#Retail Sales: Clothing and Clothing Accessory Stores (MRTSSM448USN)
us_rs_clth_acc_stores <- fredr(
  series_id = "MRTSSM448USN",  
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)

#Retail Sales: Beer, Wine, and Liquor Stores (MRTSSM4453USN)
us_rs_liquor_stores <- fredr(
  series_id = "MRTSSM4453USN", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)


#Retail Sales: Food and Beverage Stores (MRTSSM445USN)
us_rs_beverage_stores <- fredr(
  series_id = "MRTSSM445USN", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)


#Retail Sales: Pharmacies and Drug Stores (MRTSSM44611USN)
us_rs_drug_stores <- fredr(
  series_id = "MRTSSM44611USN", 
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)


# Retail Sales: Furniture and Home Furnishings Stores (MRTSSM442USN)
us_rs_furniture_stores <- fredr(
  series_id = "MRTSSM442USN",  
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2024-12-01")
)


google_trend_restaurants <- read.csv("C:/Nandhini/SFSU MSBA/Spring 2025/ECON 855/Project/google_trend.csv")



############### DATA Cleaning #################
us_rs_food_serv_cleaned <- us_rs_food_serv %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% #billion conversion
  select(Month, sales)

google_trend_restaurants_cleaned <- google_trend_restaurants %>%
  mutate(Month = yearmonth(Month), iok_restaurants = restaurants) %>% 
  select(Month, iok_restaurants)

us_CPI_cleaned <- us_CPI %>%
  mutate(Month = yearmonth(date), cpi = value) %>% 
  select(Month, cpi)

us_rs_clth_acc_stores_cleaned <- us_rs_clth_acc_stores %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% 
  select(Month, sales)

us_rs_liquor_stores_cleaned <- us_rs_liquor_stores %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% 
  select(Month, sales)

us_rs_beverage_stores_cleaned <- us_rs_beverage_stores %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% 
  select(Month, sales)

us_rs_drug_stores_cleaned <- us_rs_drug_stores %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% 
  select(Month, sales)

us_rs_furniture_stores_cleaned <- us_rs_furniture_stores %>%
  mutate(Month = yearmonth(date), sales = value/1000) %>% 
  select(Month, sales)




########### Arranging the data for visualization #############
us_rs_food_serv_cleaned <- us_rs_food_serv_cleaned %>%
  as_tsibble(index = Month)

google_trend_restaurants_cleaned <- google_trend_restaurants_cleaned %>%
  as_tsibble(index = Month)

us_CPI_cleaned <- us_CPI_cleaned %>%
  as_tsibble(index = Month)

us_rs_food_serv_cleaned <- us_rs_food_serv_cleaned %>%
  as_tsibble(index = Month)

us_rs_clth_acc_stores_cleaned <- us_rs_clth_acc_stores_cleaned %>%
  as_tsibble(index = Month)

us_rs_liquor_stores_cleaned <- us_rs_liquor_stores_cleaned %>%
  as_tsibble(index = Month)

us_rs_beverage_stores_cleaned <- us_rs_beverage_stores_cleaned %>%
  as_tsibble(index = Month)

us_rs_drug_stores_cleaned <- us_rs_drug_stores_cleaned %>%
  as_tsibble(index = Month)

us_rs_furniture_stores_cleaned <- us_rs_furniture_stores_cleaned %>%
  as_tsibble(index = Month)


########## Plotting Data ###############

us_rs_food_serv_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Retail Trade and Food Services",
       y="Sales (in billions)")


us_rs_clth_acc_stores_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Clothing and Clothing Accessory Stores",
       y="Sales (in billions)")


us_rs_liquor_stores_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Beer, Wine, and Liquor Stores",
       y="Sales (in billions)")


us_rs_beverage_stores_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Food and Beverage Stores",
       y="Sales (in billions)")


us_rs_drug_stores_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Pharmacies and Drug Stores",
       y="Sales (in billions)")


us_rs_furniture_stores_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Sales: Furniture and Home Furnishings Stores",
       y="Sales (in billions)")


########## plotting together ##########


combined_sales <- bind_rows(
  us_rs_food_serv_cleaned %>% as_tibble() %>% mutate(category = "Food Services"),
  us_rs_clth_acc_stores_cleaned %>% as_tibble() %>% mutate(category = "Clothing & Accessories"),
  us_rs_liquor_stores_cleaned %>% as_tibble() %>% mutate(category = "Liquor Stores"),
  us_rs_beverage_stores_cleaned %>% as_tibble() %>% mutate(category = "Food & Beverage"),
  us_rs_drug_stores_cleaned %>% as_tibble() %>% mutate(category = "Pharmacies & Drug Stores"),
  us_rs_furniture_stores_cleaned %>% as_tibble() %>% mutate(category = "Furniture & Home")
)

ggplot(combined_sales, aes(x = Month, y = sales, color = category)) +
    geom_line() +
  facet_grid(category ~ ., scales = "free_y")+
  labs(title = "US Retail Sales by Category",
      x = "Month",
      y = "Sales (in billions)",
      color = "Retail Category")

combined_sales_ts <- combined_sales %>%
  as_tsibble(index = Month, key = category)

combined_sales_ts %>%
  gg_season(sales) +
  labs(title = "Seasonality in US Retail Sales by Category",
       x = "Month",
       y = "Sales (in billions)",
       color = "Retail Category")

combined_sales_ts %>%
  gg_subseries(sales) +
  labs(title = "Seasonality in US Retail Sales by Category",
       x = "Month",
       y = "Sales (in billions)",
       color = "Retail Category")


############ Seperate for each individuaL CHARTS###########
############ STL decomposition ###############

us_rs_food_serv_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

us_rs_clth_acc_stores_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

us_rs_liquor_stores_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

us_rs_beverage_stores_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

us_rs_drug_stores_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

us_rs_furniture_stores_cleaned %>%
  model(STL(sales)) %>%
  components()%>%
  autoplot()

############## Just seesing Seasonally adjusted#############

dcmp <- us_rs_food_serv_cleaned %>%
  model(STL(sales)) %>%
  components()


dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Retail Trade and Food Services - Seasonally Adjusted")


dcmp <- us_rs_clth_acc_stores_cleaned %>%
  model(STL(sales)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Clothing and Clothing Accessory Stores - Seasonally Adjusted")



dcmp <- us_rs_liquor_stores_cleaned %>%
  model(STL(sales)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Beer, Wine, and Liquor Stores - Seasonally Adjusted")



dcmp <- us_rs_beverage_stores_cleaned %>%
  model(STL(sales)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Food and Beverage Stores - Seasonally Adjusted")



dcmp <- us_rs_drug_stores_cleaned %>%
  model(STL(sales)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Pharmacies and Drug Stores - Seasonally Adjusted")



dcmp <- us_rs_furniture_stores_cleaned %>%
  model(STL(sales)) %>%
  components()

dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title= "Retail Sales: Furniture and Home Furnishings Stores - Seasonally Adjusted")

############# checking histogram, to see if log transformation is needed ##############




hist(as.numeric(us_rs_clth_acc_stores_cleaned$sales),
     main = "Clothing and Clothing Accessory Stores - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

### for clothing log is not needed, as it skews the result more
hist(as.numeric(log(us_rs_clth_acc_stores_cleaned$sales)),
     main = "Clothing and Clothing Accessory Stores - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(us_rs_liquor_stores_cleaned$sales),
     main = "Beer, Wine, and Liquor Stores - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(log(us_rs_liquor_stores_cleaned$sales)),
     main = "Beer, Wine, and Liquor Stores - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(us_rs_beverage_stores_cleaned$sales),
     main = "Food and Beverage Stores - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(log(us_rs_beverage_stores_cleaned$sales)),
     main = "Food and Beverage Stores - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(us_rs_drug_stores_cleaned$sales),
     main = "Pharmacies and Drug Stores - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(log(us_rs_drug_stores_cleaned$sales)),
     main = "Pharmacies and Drug Stores - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")


hist(as.numeric(us_rs_furniture_stores_cleaned$sales),
     main = "Furniture and Home Furnishings Stores - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")


### for furniture log is not needed, as it skews the result more
hist(as.numeric(log(us_rs_furniture_stores_cleaned$sales)),
     main = "Furniture and Home Furnishings Stores - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")


############## Retail Trade and Food Services #####################

hist(as.numeric(us_rs_food_serv_cleaned$sales),
     main = "Retail Trade and Food Services - Histogram of Sales",    
     xlab = "Sales Amount",         
     ylab = "Frequency")

hist(as.numeric(log(us_rs_food_serv_cleaned$sales)),
     main = "Retail Trade and Food Services - Histogram of Sales - After log transformation",    
     xlab = "Sales Amount",         
     ylab = "Frequency")


us_rs_food_serv_cleaned_trans <- us_rs_food_serv_cleaned %>%
  mutate(sales = (sales)) 

us_rs_food_serv_cleaned %>%
  autoplot(sales)+
  labs(title = "Retail Trade and Food Services")

us_rs_food_serv_cleaned_trans %>%
  autoplot(sales)+
  labs(title = "Retail Trade and Food Services - Log Transformed")

# Yes, The data are clearly non-stationary, with seasonality and a nonlinear trend, so we will first take a seasonal difference

us_rs_food_serv_cleaned_trans %>%
  gg_tsdisplay(difference(sales, 12) ,
               plot_type='partial', lag_max = 36)+
  labs(title="Seasonally differenced", y="")

# These are also clearly non-stationary, so we take a further first difference

us_rs_food_serv_cleaned_trans %>%
  gg_tsdisplay(difference((sales), 12) %>%
                 difference() ,
               plot_type='partial', lag_max = 36)+
  labs(title = "Double differenced", y="")

'''
The appropriate ARIMA model based on the ACF and PACF:

First Model : ARIMA(0,1,2)(0,1,1)12

• The significant spike at lag 2 in the ACF suggests a non-seasonal MA(2) component.

• The significant spike at lag 12 in the ACF suggests a seasonal MA(1) component.

• So, we begin with an ARIMA(0,1,2)(0,1,1)12 model, indicating a first difference, a seasonal difference, and non-seasonal MA(2) and seasonal MA(1) component.

Second Model : ARIMA(2,1,0)(0,1,1)12

• With PACF, we may have an ARIMA(2,1,0)(0,1,1) 12 model — using the PACF to select the non-seasonal part of the model and the ACF to select the seasonal part of the model.

Third Model : ARIMA(2,1,0)(3,1,0)12

• The significant spike at lag 2 in the PACF suggests a non-seasonal AR(2) component.

• The significant spike at lag 36 in the ACF suggests a seasonal AR(3) component.

• So, an ARIMA(2,1,0)(3,1,0)12 model, indicating a first difference, a seasonal difference, and non-seasonal AR(2) and seasonal AR(3) component.

'''

us_rs_food_serv_cleaned_trans %>%
  distinct(Month)

###########Train Test split ###################
us_food_cpi_joined <- left_join(
  us_rs_food_serv_cleaned,
  us_CPI_cleaned,
  by = "Month"
)

us_food_gtr_joined <- left_join(
  us_food_cpi_joined,
  google_trend_restaurants_cleaned,
  by = "Month"
)


us_rs_food_serv_train <- us_food_gtr_joined %>%
  filter(Month < yearmonth("2022-01-01"))

us_rs_food_serv_test <- us_food_gtr_joined %>%
  filter(Month >= yearmonth("2022-01-01"))



us_rs_food_serv_train %>%
  pivot_longer(-Month) %>%
  mutate(name = recode(name,
                       cpi = "Consumer Price Index (CPI)",
                       iok_restaurants = 'Google Search Trend: "restaurants"',
                       sales = "Retail Sales: Retail Trade and Food Services")) |>
  ggplot(aes(x = Month, y = value, colour = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Retail Indicators Over Time",
       y = "Value", x = "Month", colour = "Indicator")

############ Model #####################
STLF <- decomposition_model(
  STL((sales) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)


us_rs_food_serv_fit <- us_rs_food_serv_train%>%
  model(
    fourier6 = TSLM(sales ~ trend() + fourier(K = 6)),
    ets = ETS(sales),
    arima012011 = ARIMA(sales ~ pdq(0,1,2) + PDQ(0,1,1) ),
    arima210011 = ARIMA(sales ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima210110 = ARIMA(sales ~ pdq(2,1,0) + PDQ(1,1,0)),
    stepwise = ARIMA(sales),
    arima1 = ARIMA(sales, stepwise=FALSE, approx = FALSE),
    arima_cpi = ARIMA(sales ~ cpi ),
    arima_google = ARIMA(sales ~ iok_restaurants ),
    arima_fourier6 = ARIMA(sales ~ fourier(K=6) + PDQ(0,0,0)),
    # stlf = STLF
  )

us_rs_food_serv_fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

############### AIC #################
glance(us_rs_food_serv_fit) |> arrange(AICc) |> 
  select(.model:BIC,-r_squared,-adj_r_squared, -statistic, -p_value, -df)

 ################## graphs #################

# Step 1: Extract top 6 models by AICc
top_models_ordered <- glance(us_rs_food_serv_fit) %>%
  arrange(AICc) %>%
  slice(1:6) %>%
  pull(.model)

# Step 2: Forecast 3 years ahead and plot
forecast_data <- us_rs_food_serv_fit %>%
  select(all_of(top_models_ordered)) %>%
  forecast(new_data = us_rs_food_serv_test) %>%
  mutate(.model = factor(.model, levels = top_models_ordered))

forecast_data %>%
  autoplot(us_rs_food_serv_cleaned, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(
      x = yearmonth("2018 Jan"),  # <-- Move label to the left
      y = max(us_rs_food_serv_cleaned$sales, na.rm = TRUE),
      label = paste0("AICc = ", round(AICc, 1))
    ),
    data = glance(us_rs_food_serv_fit) %>%
      filter(.model %in% top_models_ordered) %>%
      mutate(.model = factor(.model, levels = top_models_ordered))
  )

########Training set ################
us_rs_food_serv_fit  %>% 
  accuracy() %>%
  filter(!is.nan(RMSE)) %>%
  select(-ME, -MPE, -ACF1,-MASE, -RMSSE) %>%
  arrange(RMSE)

########### forecast based on Training RMSE #########

# Step 1: Extract top 6 models based on training RMSE
top_models_rmse <- us_rs_food_serv_fit %>%
  accuracy() %>%
  filter(.type == "Training", !is.nan(RMSE)) %>%
  arrange(RMSE) %>%
  slice(1:6) %>%
  pull(.model)

# Step 2: Get RMSE and AICc values for labels
label_data <- glance(us_rs_food_serv_fit) %>%
  filter(.model %in% top_models_rmse) %>%
  left_join(
    us_rs_food_serv_fit %>%
      accuracy() %>%
      filter(.type == "Training") %>%
      select(.model, RMSE),
    by = ".model"
  ) %>%
  mutate(
    .model = factor(.model, levels = top_models_rmse),
    label_text = paste0("AICc = ", round(AICc, 1), "\nRMSE = ", round(RMSE, 1))
  )

# Step 3: Forecast and reorder for plotting
forecast_data <- us_rs_food_serv_fit %>%
  select(all_of(top_models_rmse)) %>%
  forecast(new_data = us_rs_food_serv_test) %>%
  mutate(.model = factor(.model, levels = top_models_rmse))



# Step 4: Final plot
forecast_data %>%
  autoplot(us_rs_food_serv_cleaned, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    data = label_data,
    aes(
      x = min(us_rs_food_serv_cleaned$Month),  # Far left
      y = max(us_rs_food_serv_cleaned$sales, na.rm = TRUE) * 0.98,  # Near top
      label = label_text
    ),
    inherit.aes = FALSE,
    size = 3.5,
    hjust = 0  # Align left within the label box
  )

############# Test Set ###############
bind_rows(
  us_rs_food_serv_fit %>%
    select(-arima_cpi, -arima_google) %>%   
    forecast(h = "3 years") %>%
    accuracy(us_rs_food_serv_test) %>%
    filter(!is.nan(RMSE)) 
  ,
  us_rs_food_serv_fit %>%
    select(arima_cpi, arima_google) %>%   
    forecast(new_data = us_rs_food_serv_test) %>%
    accuracy(us_rs_food_serv_test) %>%
    filter(!is.nan(RMSE)) 
)%>%
  select(.model, .type, RMSE, MAE, MAPE) %>%
  arrange(RMSE)

########### forecast based on test set #################

#  Top 6 models by test RMSE
test_accuracy <- bind_rows(
  us_rs_food_serv_fit %>%
    select(-arima_cpi, -arima_google) %>%   
    forecast(h = "3 years") %>%
    accuracy(us_rs_food_serv_test) %>%
    filter(!is.nan(RMSE)),
  us_rs_food_serv_fit %>%
    select(arima_cpi, arima_google) %>%   
    forecast(new_data = us_rs_food_serv_test) %>%
    accuracy(us_rs_food_serv_test) %>%
    filter(!is.nan(RMSE))
)

top_rmse_models <- test_accuracy %>%
  select(.model, RMSE) %>%
  arrange(RMSE) %>%
  slice(1:6) %>%
  pull(.model)

#  Forecast and plot
forecast_data <- us_rs_food_serv_fit %>%
  select(all_of(top_rmse_models)) %>%
  forecast(new_data = us_rs_food_serv_test) %>%
  mutate(.model = factor(.model, levels = top_rmse_models))

#  AICc values
aic_labels <- glance(us_rs_food_serv_fit) %>%
  filter(.model %in% top_rmse_models) %>%
  select(.model, AICc)

# Merged RMSE with AICc for annotation
labels_df <- test_accuracy %>%
  filter(.model %in% top_rmse_models) %>%
  left_join(aic_labels, by = ".model") %>%
  left_join(
    us_rs_food_serv_fit %>%
      accuracy() %>%
      filter(.type == "Training") %>%
      select(.model, train_RMSE = RMSE),
    by = ".model"
  ) %>%
  mutate(
    .model = factor(.model, levels = top_rmse_models),
    label_text = paste0("AICc = ", round(AICc, 1),
                        "\nTrain RMSE = ", round(train_RMSE, 1),
                        "\nTest RMSE = ", round(RMSE, 1))
  )

# Step 3: Plot
forecast_data %>%
  autoplot(us_rs_food_serv_cleaned, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    data = labels_df,
    aes(
      x = yearmonth("2001 Jan"),  # moved far left
      y = max(us_rs_food_serv_cleaned$sales, na.rm = TRUE),  # top
      label  = label_text
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,   # top alignment
    size = 3
  ) +
  labs(
    title = "3-Year Forecast: Top 6 Models by Test RMSE",
    y = "Sales (in billions)",
    x = "Month"
  )

# clearly arima_google trend seems to be a better model with low AICc, and Training and Test RMSE

#######################

us_rs_food_serv_fcst_arima <- us_rs_food_serv_fit %>% 
  select(arima_google) %>%
  forecast(new_data = us_rs_food_serv_test) 
  

autoplot(us_rs_food_serv_fcst_arima) +
  autolayer(us_rs_food_serv_cleaned_trans) +
  labs(
    title = "Retail Sales Forecast with ARIMA: Influence of Google Trends on Restaurant Activity",
    y = "Sales (in billions)", x = "Month"
  )

us_rs_food_serv_fcst_arima1 <- us_rs_food_serv_fit %>% 
  select(arima1) %>%
  forecast(h = "3 years") 


autoplot(us_rs_food_serv_fcst_arima1) +
  autolayer(us_rs_food_serv_cleaned_trans) +
  labs(
    title = "Retail Sales Forecast with ARIMA",
    y = "Sales (in billions)", x = "Month"
  )

#########residual analysis###########

us_rs_food_serv_fit |> select(arima_google) |> gg_tsresiduals(lag=36)

#########forecast###########
bind_rows(
  us_rs_food_serv_fit %>% select(arima_google) %>%  accuracy(),
  us_rs_food_serv_fit %>% select(arima1) %>%  accuracy(),
  us_rs_food_serv_fit %>% select(arima_google) %>% forecast(new_data = us_rs_food_serv_test) %>% accuracy(us_rs_food_serv_test),
  us_rs_food_serv_fit %>% select(arima1) %>% forecast(h = "3 years") %>% accuracy(us_rs_food_serv_test)
) %>%
  select(-ME, -MPE, -ACF1,-MASE, -RMSSE)


#########ljung_box###########

bind_rows(
  augment(us_rs_food_serv_fit %>% select(arima_google)) |> features(.innov, ljung_box, lag = 8),
  augment(us_rs_food_serv_fit %>% select(arima1)) |> features(.innov, ljung_box, dof = 3, lag = 8)
)


################## clothing ##############################

us_rs_clth_acc_stores_cleaned_trans <- us_rs_clth_acc_stores_cleaned %>%
  mutate(sales = (sales)) 

us_rs_clth_acc_stores_cleaned_trans %>%
  gg_tsdisplay(difference(sales, 12) ,
               plot_type='partial', lag_max = 36)+
  labs(title="Seasonally differenced", y="")

us_rs_clth_acc_stores_cleaned_trans %>%
  gg_tsdisplay(difference((sales), 12) %>%
                 difference() ,
               plot_type='partial', lag_max = 36)+
  labs(title = "Double differenced", y="")


'''
The appropriate ARIMA model based on the ACF and PACF:

First Model : ARIMA(0,1,2)(0,1,1)12

• The significant spike at lag 2 in the ACF suggests a non-seasonal MA(2) component.

• The significant spike at lag 12 in the ACF suggests a seasonal MA(1) component.

• So, we begin with an ARIMA(0,1,2)(0,1,1)12 model, indicating a first difference, a seasonal difference, and non-seasonal MA(2) and seasonal MA(1) component.

Second Model : ARIMA(2,1,0)(0,1,1)12

• With PACF, we may have an ARIMA(2,1,0)(0,1,1) 12 model — using the PACF to select the non-seasonal part of the model and the ACF to select the seasonal part of the model.

Third Model : ARIMA(2,1,0)(3,1,0)12

• The significant spike at lag 2 in the PACF suggests a non-seasonal AR(2) component.

• The significant spike at lag 36 in the ACF suggests a seasonal AR(3) component.

• So, an ARIMA(2,1,0)(3,1,0)12 model, indicating a first difference, a seasonal difference, and non-seasonal AR(2) and seasonal AR(3) component.

'''


us_rs_clth_acc_fit <- us_rs_clth_acc_train %>%
  model(
    fourier6 = TSLM(sales ~ trend() + fourier(K = 6)),
    ets = ETS(sales),
    arima012011 = ARIMA(sales ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(sales ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima210110 = ARIMA(sales ~ pdq(2,1,0) + PDQ(1,1,0)),
    stepwise = ARIMA(sales),
    arima1 = ARIMA(sales, stepwise = FALSE, approx = FALSE),
    arima_cpi = ARIMA(sales ~ cpi),
    arima_google = ARIMA(sales ~ iok_clothing),  # Replace if your column name differs
    arima_fourier6 = ARIMA(sales ~ fourier(K = 6) + PDQ(0,0,0))
  )

us_rs_clth_acc_fit |> 
  pivot_longer(everything(), names_to = "Model name", values_to = "Orders")



############### AIC - Clothing and Accessories #################
glance(us_rs_clth_acc_fit) |> 
  arrange(AICc) |> 
  select(.model:BIC, -r_squared, -adj_r_squared, -statistic, -p_value, -df)


# Step 1: Extract top 6 models by AICc
top_models_clth <- glance(us_rs_clth_acc_fit) %>%
  arrange(AICc) %>%
  slice(1:6) %>%
  pull(.model)

# Step 2: Forecast and plot
forecast_clth <- us_rs_clth_acc_fit %>%
  select(all_of(top_models_clth)) %>%
  forecast(new_data = us_rs_clth_acc_test) %>%
  mutate(.model = factor(.model, levels = top_models_clth))

forecast_clth %>%
  autoplot(us_rs_clth_acc_stores_cleaned_trans, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(
      x = yearmonth("2018 Jan"),
      y = max(us_rs_clth_acc_stores_cleaned_trans$sales, na.rm = TRUE),
      label = paste0("AICc = ", round(AICc, 1))
    ),
    data = glance(us_rs_clth_acc_fit) %>%
      filter(.model %in% top_models_clth) %>%
      mutate(.model = factor(.model, levels = top_models_clth))
  )

############### RMSE - Clothing and Accessories #################
us_rs_clth_acc_fit %>% 
  accuracy() %>%
  filter(!is.nan(RMSE)) %>%
  select(-ME, -MPE, -ACF1, -MASE, -RMSSE) %>%
  arrange(RMSE)


########### Forecast Based on Training RMSE - Clothing and Accessories #########

# Step 1: Top 6 models based on training RMSE
top_models_rmse_clth <- us_rs_clth_acc_fit %>%
  accuracy() %>%
  filter(.type == "Training", !is.nan(RMSE)) %>%
  arrange(RMSE) %>%
  slice(1:6) %>%
  pull(.model)

# Step 2: Combine AICc and RMSE for labels
label_data_clth <- glance(us_rs_clth_acc_fit) %>%
  filter(.model %in% top_models_rmse_clth) %>%
  left_join(
    us_rs_clth_acc_fit %>%
      accuracy() %>%
      filter(.type == "Training") %>%
      select(.model, RMSE),
    by = ".model"
  ) %>%
  mutate(
    .model = factor(.model, levels = top_models_rmse_clth),
    label_text = paste0("AICc = ", round(AICc, 1), "\nRMSE = ", round(RMSE, 1))
  )

# Step 3: Forecast and reorder
forecast_data_clth <- us_rs_clth_acc_fit %>%
  select(all_of(top_models_rmse_clth)) %>%
  forecast(new_data = us_rs_clth_acc_test) %>%
  mutate(.model = factor(.model, levels = top_models_rmse_clth))

# Step 4: Final plot
forecast_data_clth %>%
  autoplot(us_rs_clth_acc_stores_cleaned_trans, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    data = label_data_clth,
    aes(
      x = min(us_rs_clth_acc_stores_cleaned_trans$Month),
      y = max(us_rs_clth_acc_stores_cleaned_trans$sales, na.rm = TRUE) * 0.98,
      label = label_text
    ),
    inherit.aes = FALSE,
    size = 3.5,
    hjust = 0
  )


bind_rows(
  us_rs_clth_acc_fit %>%
    select(-arima_cpi, -arima_google) %>%
    forecast(h = "3 years") %>%
    accuracy(us_rs_clth_acc_test) %>%
    filter(!is.nan(RMSE)),
  
  us_rs_clth_acc_fit %>%
    select(arima_cpi, arima_google) %>%
    forecast(new_data = us_rs_clth_acc_test) %>%
    accuracy(us_rs_clth_acc_test) %>%
    filter(!is.nan(RMSE))
) %>%
  select(.model, .type, RMSE, MAE, MAPE) %>%
  arrange(RMSE)


# Step 1: Extract top 6 models based on Test RMSE
top_models_test_rmse_clth <- us_rs_clth_acc_fit %>%
  accuracy(us_rs_clth_acc_test) %>%
  filter(.type == "Test", !is.nan(RMSE)) %>%
  arrange(RMSE) %>%
  slice(1:6) %>%
  pull(.model)

# Step 2: Create label data with AICc and Test RMSE
label_data_clth_test <- glance(us_rs_clth_acc_fit) %>%
  filter(.model %in% top_models_test_rmse_clth) %>%
  left_join(
    us_rs_clth_acc_fit %>%
      accuracy(us_rs_clth_acc_test) %>%
      filter(.type == "Test") %>%
      select(.model, RMSE),
    by = ".model"
  ) %>%
  mutate(
    .model = factor(.model, levels = top_models_test_rmse_clth),
    label_text = paste0("AICc = ", round(AICc, 1), "\nTest RMSE = ", round(RMSE, 2))
  )

# Step 3: Forecast on test set
forecast_data_clth_test <- us_rs_clth_acc_fit %>%
  select(all_of(top_models_test_rmse_clth)) %>%
  forecast(new_data = us_rs_clth_acc_test) %>%
  mutate(.model = factor(.model, levels = top_models_test_rmse_clth))

# Step 4: Plot
forecast_data_clth_test %>%
  autoplot(us_rs_clth_acc_stores_cleaned_trans, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    data = label_data_clth_test,
    aes(
      x = min(us_rs_clth_acc_stores_cleaned_trans$Month),
      y = max(us_rs_clth_acc_stores_cleaned_trans$sales, na.rm = TRUE) * 0.98,
      label = label_text
    ),
    inherit.aes = FALSE,
    size = 3.5,
    hjust = 0
  )


#################### Furniture ##############

us_rs_furniture_stores_cleaned_trans <- us_rs_furniture_stores_cleaned %>%
  mutate(sales = (sales)) 

us_rs_furniture_stores_cleaned_trans %>%
  gg_tsdisplay(difference(sales, 12) ,
               plot_type='partial', lag_max = 36)+
  labs(title="Seasonally differenced", y="")

us_rs_furniture_stores_cleaned_trans %>%
  gg_tsdisplay(difference((sales), 12) %>%
                 difference() ,
               plot_type='partial', lag_max = 36)+
  labs(title = "Double differenced", y="")

'''
The appropriate ARIMA model based on the ACF and PACF:

First Model : ARIMA(0,1,2)(0,1,1)12

• The significant spike at lag 2 in the ACF suggests a non-seasonal MA(2) component.

• The significant spike at lag 12 in the ACF suggests a seasonal MA(1) component.

• So, we begin with an ARIMA(0,1,2)(0,1,1)12 model, indicating a first difference, a seasonal difference, and non-seasonal MA(2) and seasonal MA(1) component.

Second Model : ARIMA(2,1,0)(0,1,1)12

• With PACF, we may have an ARIMA(3,1,0)(0,1,1) 12 model — using the PACF to select the non-seasonal part of the model and the ACF to select the seasonal part of the model.

Third Model : ARIMA(2,1,0)(3,1,0)12

• The significant spike at lag 2 in the PACF suggests a non-seasonal AR(2) component.

• The significant spike at lag 36 in the ACF suggests a seasonal AR(3) component.

• So, an ARIMA(2,1,0)(3,1,0)12 model, indicating a first difference, a seasonal difference, and non-seasonal AR(2) and seasonal AR(3) component.

'''