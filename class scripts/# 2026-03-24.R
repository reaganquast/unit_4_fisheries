# 2026-03-24
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")
head(tsmetrics)
head(timeseries)

timeseries_tsmetrics = left_join(
  timeseries, 
  tsmetrics, 
  by = c("tsid" = "tsunique"))#this is in case you have multiple columns you're binding by. you say these things are equal
dim(timeseries)
dim(timeseries_tsmetrics)
    #use dim to make sure they have the same number of rows!

############### END 2026 - 03 - 18 #################

# this subset gives us the best data if there are repeats. just makes it more manageable
head(timeseries_values_views)

# lets join metadata about this stuff
      # using taxonomy table
head(taxonomy) #info about each species
glimpse(stock) #management about species
    #tsn - taxonomic serial number just as a btw
fish = timeseries_values_views%>%
  left_join(stock, #again, join is just the data you are combining the first data with 
            by = c("stockid", "stocklong")) #stockid is short hand, stocklong spells it out. if you join by both, it takes both alongside without creating any other columns
                          #since there are two common columns, we just join both in here.
glimpse(fish)

#now we have stock data. lets join with taxonomy
      #we needed to do an intermediary joining of tables because taxonomy doesn't exist in timeseries. this just allows us to have a joint where we can connect them

fish = timeseries_values_views%>%
  left_join(stock, by = c("stockid", "stocklong"))%>%
  left_join(taxonomy, by = c("tsn", "scientificname"))%>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)
      #this last line is only picking the columns we care about (so that we dont have to deal with all 69 columns)
glimpse(fish)

    ############# TODAY #############
    # overfishing and stock collapse

#lets start by seeing how many fish are caught over time

fish%>% arrange(desc(TCbest)) #resorting data so highest TCbest is printed first
      #TC best shows us what fish has been caught the most 

ggplot() +
  geom_line(data = fish, aes(x = year, y = TCbest, color = stockid)) +
  theme(legend.position = "none") #this tells it to not put the legend in at all. THAT IS BECAUSE THERE ARE WAY TOO MANY STOCKS
      #basically shows us that fishing is dominated by a few stocks

#you could stubset the data about the few highest stock
ggplot() +
  geom_line(data = fish %>% filter (TCbest >3e6), 
  aes(x = year, y = TCbest, color = stocklong))

#lets use this to answer a fun question. maybe...

fish %>% 
  filter(scientificname == "Gadus morhua") %>% #looking at cod
  distinct(region) #where are the stock's corresponding to this scientific name (looking at distinct regions)

cod_can = fish %>% #can for canada
  filter(scientificname == "Gadus morhua", 
        region == "Canada East Coast",
        !is.na(TCbest))
head(cod_can)
      #this cleaned up the data set and made it easy to understand... perhaps... do this for your research... 

#REMINDER: TC is the total catch each year

ggplot(data = cod_can) +
  geom_line(aes(x = year, y = TCbest, color = stocklong)) +
  theme_minimal() +
  ylab("Total Catch in MT")
# lets take all of the cod in eastern canada and add them together to make one stock.
      #new data frame to add all cod stocks together

cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest)) #anything that is not grouped or is not named in the summarize row is dropped in the data

ggplot(data = cod_can_total) +
  geom_line(aes(x = year, y = total_catch), col = "orchid3") +
  theme_minimal() +
  ylab("Total Catch in MT")

#lets take a break from cod and look at cumulative functions (in dplyr)
      #making a fake data set for this too
dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
      #from this point, examining all the rows proceeding, what is the max number that has been encountered in data so far
dat_sum = cumsum(dat)
      #steps through each row and adds things together
#test our cummulative functions
test_cum = data.frame(dat, dat_max, dat_sum)
#this would be useful to see if the current number is less than the max (like for seeing if theres a collapse...)

# using Boris Worm (2006) stock collapse definition - has cod collapsed?? :3

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch)) #to create a new column
#want to know if for any given year the number is less than the max catch...
cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch))%>%
  mutate(collapse = total_catch <= 0.1 * historical_max_catch) #true false (think of this for your research)
        #tells you if something is less than 10% of the historical match

#what NOT to do: open up the data and scroll through the data to see when the change happens. BAD.
#what TO do:
cod_collapse_year = cod_collapse %>%
  filter(collapse == T) %>%
  summarize(year = min(year))%>%
  pull() #pulls vector out of the data frame so it can be used as a number

ggplot() +
  geom_line(data = cod_collapse, aes(x = year, y = total_catch, color = collapse)) + #color draws line as red for false and blue for true
  geom_vline(xintercept = cod_collapse_year) + #creating a vertical line where the collapse happened. vline is for vertical
  theme_classic() +
  ylab("Total Catch in MT")

#taking what we've done with cod and applying it to our gigantic evil dataset
      #if you have big data, you should subset it, and subset it again
        #play in a small space until you are confiddent
#apply collapse to full data set
collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>% #makes sure the cummulative maxes are separate for each stock
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest <= 0.1 * historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>% #tells you if the fishery is collapsed yet! sometimes, the stock rebounds (which makes them no longer in a state of collapse)
              # ^^^ was there ever a period in the history of this stock that it was collapsed? (not focusing on if it's collapsed right now)
  ungroup() #this is for the future, doesn't change our data set at all

#lets find out when every stock experienced collapse
collapse_year = collapse %>%
  group_by(stockid, stocklong, region) %>% #this doesnt change anything, but if you dont group them, then when you summarize it will go missing since it wasnt mentioned
  filter(collapsed_yet == T) %>%
  summarize(first_collapse_yr = min(year)) %>%
  ungroup() #just a reminder that this is for plotting or modelling
head(collapse_year)  

# timeseries of shame!!!!

n_stocks = length(unique(collapse$stockid))
#calculate a collapse rate
collapse_ts = collapse_year %>%
  group_by(first_collapse_yr) %>%
  summarize(n = n())%>%
  mutate(cum_first_collapse_yr = cumsum(n),
        ratio_collapsed_yet = cum_first_collapse_yr / n_stocks)
head(collapse_ts)

ggplot(data = collapse_ts) +
  geom_line(aes(x = first_collapse_yr, y = ratio_collapsed_yet)) #more than 40% of known stocks have collapsed at some point in history

#exc 2.1 is good for homework practice



