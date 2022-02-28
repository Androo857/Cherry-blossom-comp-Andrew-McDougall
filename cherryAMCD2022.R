rm(list = ls())
library(tidyverse)
library(mgcv)
library(gratia)
library(MuMIn)
#read the data
cherry <- read.csv("washingtondc1.csv") %>% 
  bind_rows(read.csv("liestal1.csv")) %>% 
  bind_rows(read.csv("kyoto.csv"))

#Visualize as time series with linear trend
#Note that Liestal and Washington d.c. observations begin well after Kyoto
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_smooth(method = lm) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")+
  theme(panel.grid = element_blank())

#Visualize as time series with gam smoothing
#Note any deviation from linearity - e.g. Liestal, or Washington, D.C.
cherry %>% 
  filter(year >= 1880) %>%
  ggplot(aes(x = year, y = bloom_doy)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")+
  theme(panel.grid = element_blank())

#Non linear models seems appropriate

#read climate data file with Japan, Kyoto, Liestal, and Washington bloom_doy, and all other Japan sites
Wdata <- read.csv("siteclimatedata.csv", header = TRUE, sep = ",")


# Fit gam model for climate
gam_fit <- gam(bloom_doy ~ s(tmax2) + s(tmax1, k = 3), data = Wdata, subset = year >= 1880, method = "REML")

#Model summary
summary(gam_fit)

appraise(gam_fit)

draw(gam_fit)


#Read Vancouver data
Vdata <- read.csv("vancouver.csv", header = TRUE, sep = ",")

vancouver_doy <- predict(gam_fit, newdata = Vdata)

vc_doy <- as.integer(vancouver_doy)

vc_doy

Vdata1 <- Vdata %>% 
  mutate(vc_doy)

Vdata2 <- Vdata1 %>%
  rename(bloom_doy = vc_doy)

cherry1 <- cherry %>%
  bind_rows(Vdata2)


#FACTORS
#Make location a factor
cherry1$location <- as.factor(cherry1$location)
#Check
class(cherry1$location)


# Fit GAM  models for each site and test whether the mean doy of each site is different.
# and select 'best' model
gam_fit2 <- gam(bloom_doy ~ s(year, by = location) + (location),
                data = cherry1, subset = year >= 1880)
gam_fit5 <- gam(bloom_doy ~ s(year) + (location),
                data = cherry1, subset = year>= 1880)
gam_fit6 <- gam(bloom_doy ~ s(year, by = alt) + (alt),
               data = cherry1, subset = year >= 1880)
gam_fit7 <- gam(bloom_doy ~ s(year) + (alt),
                data = cherry1, subset = year>= 1880)
gam_fit8 <- gam(bloom_doy ~ s(year, by = location) + (location) + (alt),
                data = cherry1, subset = year>= 1880)

model.sel(gam_fit2, gam_fit5, gam_fit6, gam_fit7, gam_fit8)
#summarize
summary(gam_fit2)

## Assess residual diagnostics and model fit
appraise(gam_fit2)

## Visualise estimated relationships
draw(gam_fit2, nrow = 2)

# Compute the predictions for all 3 sites
predictions_gam <- expand_grid(location = unique(cherry1$location),
                               alt = unique(cherry1$alt),
                               year = 1880:2031) %>% 
  bind_cols(predicted_doy = predict(gam_fit2, newdata = .))



# Plot the predictions alongside the actual observations for 1990 up to 2032.
cherry1 %>% 
  right_join(predictions_gam, by = c('year', 'location')) %>%
  filter(year >= 1880) %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line(aes(color = year > 2021), size = 1) +
  geom_point(aes(y = bloom_doy)) +
  scale_color_manual(values = c('FALSE' = 'gray50', 'TRUE' = 'blue'),
                     guide = 'none') +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")


#view 10 year predictions for each site
gampred <- predictions_gam %>% 
  group_by(location) %>% 
  slice_tail(n = 10)

print(gampred [1:10, ])
print(gampred [11:20, ])
print(gampred [21:30, ])
print(gampred [31:40, ])

#read 2022 climate data for all four sites
#compiled from NOAA + 'topped-up' with forecasts, e.g. BBC weather
# and short history observations from accuweather, accesses on 25th and 26th February
#Full references in methods

CL22 <- read.csv("Climate2022predprecip.csv", header = TRUE, sep = ",")

#Read worldwide bloom_doy WITH hindcast Vancouver data added on
W1data <- read.csv("siteclimatedatawv.csv", header = TRUE, sep = ",")

#FACTORS
#Make location a factor
CL22$location <- as.factor(CL22$location)
#Check
class(CL22$location)

#Make location a factor
W1data$location <- as.factor(W1data$location)
#Check
class(W1data$location)




#Flowering time relationships data exploration and visualization
#What are the relationships and are they informative?
gam_fitA <- gam(bloom_doy ~ s(lat), data = W1data)
summary(gam_fitA)
draw(gam_fitA)
gam_fitB <- gam(bloom_doy ~ s(long), data = W1data)
summary(gam_fitB)
draw(gam_fitB)
gam_fitC <- gam(bloom_doy ~ s(precip1), data = W1data)
summary(gam_fitC)
draw(gam_fitC)
gam_fitD <- gam(bloom_doy ~ s(precip2), data = W1data)
summary(gam_fitD)
draw(gam_fitD)
gam_fitE <- gam(bloom_doy ~ s(alt), data = W1data)
summary(gam_fitE)
draw(gam_fitE)
gam_fitF <- gam(bloom_doy ~ te(long, lat), data = W1data)
summary(gam_fitF)
draw(gam_fitF)
#fit models for prediction
gam_fit_H <- gam(bloom_doy ~ s(tmax2) + s(tmax1, k = 3) + te(long, lat), data = W1data)
summary(gam_fit_H)
draw(gam_fit_H)
appraise(gam_fit_H)

gam_fit_I <- gam(bloom_doy ~ s(tmax2, by = location) + s(tmax1, k = 3), data = W1data)
summary(gam_fit_I)
draw(gam_fit_I)
appraise(gam_fit_I)

gam_fit_J <- gam(bloom_doy ~ s(tmax2, by = location) + s(tmax1, k = 3) +(location), data = W1data)
summary(gam_fit_J)
draw(gam_fit_J)
appraise(gam_fit_J)

#Select 'best' model
model.sel(gam_fit_H, gam_fit_I, gam_fit_J)
#Use best model for predicting 2022 bloom_doy
# the prediction only for 2022 for all four sites
predictions_gam1 <- expand_grid(location = unique(CL22$location),
                               year = 2022) %>% 
  bind_cols(predicted_doy1 = predict(gam_fit_I, newdata = CL22))

predictions_gam1

#view 2022 prediction for each site
gampred1 <- predictions_gam1 %>% 
  group_by(year,location) %>% 
  slice_tail(n = 1)

gampred1

print(gampred1 [1:4, ])


#Submission of predictions table
submission_predictions <- gampred %>%
  filter(year>2022)%>%
  mutate(predicted_doy = round(predicted_doy))%>%
  pivot_wider(names_from = 'location', values_from = 'predicted_doy')%>%
  select(year, kyoto, liestal, washingtondc, vancouver)

submission_predictions



#subtract values from columns to bring estimates to the regionally appropriate phase
#Replace Washington D.C. and Vancouver 2022 values (80% flowers open) with
#regionally used peak (70% flowers open) by indexing with
#bloom_doy - 1
#Explanation: this peak is estimated at one(1) day prior
#to the regression value peak, i.e. 80% of flowers that are possible to
#be open concurrently
submission_predictions[4:5] <- submission_predictions[4:5] - 1
submission_predictions
#Replace Liestal 2022 value (80% flowers open) with
#regionally used peak (25% flowers open) by indexing with
#bloom_doy - 5
#Explanation: this peak is estimated at five (5) days prior
#to the regression value peak, i.e. 80% of flowers that are possible to
#be open concurrently
submission_predictions[3] <- submission_predictions[3] - 5
submission_predictions



submission_predictions1 <- gampred1%>%
  filter(year == 2022) %>%
  mutate(predicted_doy1 = round(predicted_doy1))%>%
  pivot_wider(names_from = 'location', values_from = 'predicted_doy1')%>%
  select(year, kyoto, liestal, washingtondc, vancouver)

submission_predictions1

#Replace Liestal 2022 value (80% flowers open) with
#regionally used peak (25% flowers open) by indexing with
#bloom_doy - 5
#Explanation: this peak is estimated at five (5) days prior
#to the peak used in regression, i.e. 80% of flowers that are possible to
#be open concurrently
submission_predictions1[3] <- submission_predictions1[3] - 5
submission_predictions1

#Replace Washington D.C. and Vancouver 2022 values (80% flowers open) with
#regionally used peak (70% flowers open) by indexing with
#bloom_doy - 2
#Explanation: this peak is estimated at one (1) days prior
#to the peak used in regression, i.e. 80% of flowers that are possible to
#be open concurrently
submission_predictions1[4:5] <- submission_predictions1[4:5] - 1
submission_predictions1


submission_predictions2 <- submission_predictions1 %>%
  bind_rows(submission_predictions)

submission_predictions2

write.csv(submission_predictions2, file = "cherry-predictions-A-E-McD.csv",
          row.names = FALSE)

