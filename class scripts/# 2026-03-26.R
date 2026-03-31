# 2026-03-26
library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")
#logistic regression is constrained between 0 and 1
    # you can use it for binary (no or yes). or you could do probability

#new function alert - using `build_collapse_table.R` which we need to source
source("build_collapse_table.R")
    #with a source function, you can take the entire library of ANOTHER script and run it all for you
    #popular if you have functions you want to use again and again, you can use this for user defined functions

glimpse(collapse)
#looking at what characteristic makes it more likely for a fishery to collapse

#has a given stock ever collapsed?
model_data = collapse %>%
  group_by(stockid, stocklong) %>% #what indicates a unique stock. if you dont want something to go away, you need to work it in here
  summarize(ever_collapsed = any(current_collapse)) %>% #summarize current variable (is there EVER a true). any tells you if even one is true
  ungroup()%>%
  left_join(metadata, by = c("stockid", "stocklong")) %>% #metadata
  mutate(FisheryType = as.factor(FisheryType)) #changing it from a character to a factor. gets rid of quotes (acknowledges all fishery types to have the same element in them)

glimpse(model_data)

#model: is ever collapse predicted by fishery type (using GLM- generalized linear model)

model_l = glm(data = model_data, ever_collapsed ~ FisheryType, family = "binomial")
summary(model_l)  

model_data %>% distinct(FisheryType)
#flatfish is missing in our model, the intercept is flatfish
    #each of the things after "intercept" is the correction RELATIVE to flatfish
    #e.g., rockfish are significantly more likely to collapse than flatfish (because it is larger than the estimate)

#generate predictions to make it really in your face about what it means
    #predict likelyhood of a stock collapsing based on fishery type
    #generate new data for each of the 8 fishery types
newdata = model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
model_l_predict = predict(model_l, newdata = newdata, type = "response", se.fit = T) #behind the scenes, it will use predict.glm (rather than the predict.lm that we used for penguins)
                                    #type can be link, response, or terms. because link is first in the vector, it is the default
                                    #you need to figure out which you want!! link is messy. if you care about probablity, you want the one with the binary
                                    #use "response" for predictions
#connect to og data
collapse_fishery_type_predictions = cbind(newdata, model_l_predict)
# plotting yay :D
ggplot(data = collapse_fishery_type_predictions) +
  geom_bar(aes(x = FisheryType, y = fit, fill = FisheryType), stat = "identity", show.legend = F) + #if it was a continuous variable, we would do a line. this is categorical (based on type of fishery)
  geom_errorbar(aes(x = FisheryType, ymin = (fit-se.fit), ymax = (fit+se.fit)), width = .2) + #width is outside of aes because it is not data driven (so everything in AES is based on data)
  coord_flip() + 
  theme_minimal()


### poisson model. poisson d'avril! yay! (but evil maybe)
    #just like a logistical regression
#asking how many years this stock has been in the collapsed state
tsmetrics%>% filter(tsshort == "BdivBmgtpref")
tsmetrics%>% filter (tsshort == "UdivUmsypref")

#build dataset
u_summary = timeseries_values_views %>%
  filter(!is.na(BdivBmgtpref),
          !is.na(UdivUmgtpref)) %>%
  group_by(stockid, stocklong) %>%
  summarize(yrs_data = n(), # collapsing time series into how many years it's been collapsed and how often the biomass is above or below one (biomass above one good, pressure above one bad)
            ratio_yrs_overfished = sum((UdivUmsypref > 1)/yrs_data),#how often is it overfished and depleted
            ratio_yrs_low_stock = sum((BdivBmgtpref < 1)/yrs_data)) %>% #ratio of years where hte biomass is less than its supposed to be for maximum sustainable yield
  select(-yrs_data) %>%
  ungroup()%>%
  left_join(metadata %>% select(stockid, FisheryType)) #only grabbing the two columns from meta data you care about. you NEED stockid to join them together because they're common, but you want the info from fisherytype
glimpse(u_summary)

## join it with the collapse table
collapse_summary = collapse %>%
  group_by(stockid, stocklong) %>% #summarize how much time stock hjas been in a collapsed state
  summarize(yrs_data = n(), #how many years we have data for if the stock was collapsed (how many years was the stock in a collapsed state)
  yrs_collapsed = sum(current_collapse)) %>%
  ungroup() %>%
  inner_join(u_summary, by = c("stockid", "stocklong")) #inner join used because you ONLY want a row where you have all the data you need (that way you don't have to filter out NAs)

glimpse(collapse_summary)
#now we can do the poisson model?
table(collapse_summary$yrs_collapsed)
hist(collapse_summary$yrs_collapsed)
    #problem: most of the fish in the table are NEVER collapsed. 
      #this means it is 0 inflated. 
      #only for our fisheries that HAVE collapsed, what can predict how long they can collapse

#0 truncate our data
collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed > 0)
table(collapse_summary_zero_trunc$years_collapsed)

model_p = glm(yrs_collapsed ~FisheryType +ratio_yrs_overfished + ratio_yrs_low_stock, 
  offset(log(yrs_data)), #if they were monitored for the same amount of time, there wouldnt need to be an offset.
  data = collapse_summary_zero_trunc, 
  family = "poisson")
summary(model_p)
#we havent dealt with dispersion

install.packages("AER")
AER::dispersiontest(model_p)
    #testing for overdispersion. since our p value is really small, that means we have overdispersion.

model_qp = glm(yrs_collapsed ~FisheryType +ratio_yrs_overfished + ratio_yrs_low_stock, 
  offset(log(yrs_data)), #if they were monitored for the same amount of time, there wouldnt need to be an offset.
  data = collapse_summary_zero_trunc, 
  family = "quasipoisson") #quaisipoisson is a better way to get rid of overdispersion

#plotting
median_ratio_yrs_low_stock = median(collapse_summary_zero_trunc$ratio_yrs_low_stock)
FisheryType = collapse_summary_zero_trunc %>% distinct(FisheryType) %>% pull()
newdata = expand.grid(FisheryType = FisheryType,
                      ratio_yrs_low_stock = median_ratio_yrs_low_stock,
                    ratio_yrs_overfished = seq(0,1, by = .1))

###### 03/26/26 end, plotting poisson next

model_qp_predict = predict(model_qp, newdata = newdata, type = "response", se.fit = T) #want to arrange it so y looks like a predicted count
collapse_time_predictions = cbind(newdata, model_qp_predict) #what is the difference between cbind and joints?

# visualize with a pretty plot
ggplot(data = collapse_time_predictions) +
  geom_line(aes(x = ratio_yrs_overfished, y = fit, color = FisheryType)) + #why do some things go in aes and what determines what goes there
  geom_ribbon(aes(x = ratio_yrs_overfished, ymax = fit+se.fit, ymin = fit-se.fit, fill = FisheryType), alpha = 0.3) +#fill fills the color in, color just does the lines
  geom_point(aes(x = ratio_yrs_overfished, y = yrs_collapsed, color = FisheryType), data = collapse_summary_zero_trunc) +
  theme_minimal()




