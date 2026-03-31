# 2026-03-19

#joings - taking two tables and joining them together when they have the same data somehwere
    #mutating joints: adds columns (makes it wider, more variables)
    #filtering joints: taking 

library(tidyverse)

data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2")) #these are just for demonstration.
data2 = data.frame(ID = c(2,3), X2 = c("b1", "b2"))
#these only have one row in common - ID. lets bring them together!

############# left join #################
#left join: taking some data set (data1), you read from left to right and join with data two.. or something

data12_left = left_join(data1, data2) #preserves data 1, but adds data 2 (where possible) - 
                                      #you are adding new columns, and if x2 CAN go into data table 1, it goes ther
                                      #if there is no data, it just fills it with NA

#you can also do piping.
data12_left = data1 %>%
  left_join(data2) #this way, you can do other things, like filtering, adding new data, etc

########### right join ###############
data12_right = data1 %>%
  right_join(data2)
#it is probably easier to just left join and start with data 2. like below
                      demonstration = data2 %>%
                        left_join(data1)

############## inner join ###############
#tbh you could just use a filter but lets see
data12_inner = data1 %>%
  inner_join(data2)
#only keeps rows where the id exists in both data tables. 
      # data1 has id (1 and 2), data2 has id (2 and 3). they only have id (2) in common.
    #if you want to do this, you can do a left join and then filter out NAs

############ full join ##############
data12_full = data1 %>%
  full_join(data2)

#this joins... everything together. thinks that all things are useful. wow
    #if you really care about data2 and think it's all helpful, you'd use this.

########### semi join ###############
data12_semi = data1 %>%
  semi_join(data2)
#no new data is added, but it is filtered 
    #gets rid of any data that 
    #you CAN explicitly say which column you want to join by (which is useful when columns dont have the same name)

############ anti join ##############
data12_anti = data1 %>%
  anti_join(data2, by = "ID")
  #basically, give me all the stuff that's not available in data 2 but IS in data 1
  #good if you need to find things that are NOT available

#### NOTE ####
 # there is often row duplication... lets.. examine
data1 = data.frame(ID = c(1,2), X1 = c("a1", "a2")) 
data2 = data.frame(ID = c(2,2,3), X2 = c("b1", "b2", "b2"))

test = data1 %>%
  left_join(data2)

#problem - it duplicates your counts in the first column. this can be problematic if you need counts!

# ALWAYS test the dimensions of your data frame before and after you change this. you NEED to know the number of rows in case data was duplicated


######################################################
############### data pivots ##########################
######################################################

survey = data.frame(quadrat_id = c(101,102,103,104), 
                    barnacle = c(2, 11, 8, 27),
                    chiton = c(1, 0, 0, 2),
                    mussel = c(0,1,1,4))
 #this is the WIDE format. lets change it to the long format and then the wide format again
#MODELS really like WIDE formats
#PLOTTING likes LONG data formats

############ make it long ##############

long = survey %>%
  pivot_longer(cols = c("barnacle", "chiton", "mussel"), names_to = "taxa", values_to = "count")


############ make it wide ##############
wide = long %>%
  pivot_wider(names_from = taxa, values_from = count)

##exercise 1.2

ggplot(data = wide) +
  geom_point(aes(x = quadrat_id, y = barnacle), color = "orange")+
  geom_point(aes(x = quadrat_id, y = chiton), color = "orchid")+
  geom_point(aes(x = quadrat_id, y = mussel), color = "slateblue")

ggplot(data = long) +
  geom_point(aes(x = quadrat_id, y = count, color = taxa))
