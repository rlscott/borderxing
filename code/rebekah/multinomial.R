# trying to look at the multinomial distribution of each port's counts

library(tidyverse)
load("data/border_clean.Rdata")

border %>% # filter(Border == "US-Mexico Border") %>% 
  ggplot(aes(x = reorder(Port_name, as.numeric(Measure), na.rm = TRUE), fill = Measure)) + 
  geom_bar(position = "fill") + coord_flip() + xlab("Port Name") + 
  ylab("Count Precentage by Measure")

# get the aggregate multinomial distributions of each port 
border_multinom_long <- border %>% 
  # first get the sum of counts by measure and port 
  mutate(measure_count_by_port = sum(Value), .by = c(Measure, Port_code)) %>%
  # next get the sum of all measure  counts by port
  mutate(total_count_by_port = sum(Value), .by = Port_code) %>% 
  # create a proportion of the two 
  mutate(measure_porp = measure_count_by_port / total_count_by_port) %>% 
  # select what you need 
  select(Port_code, Measure, measure_porp, measure_count_by_port, total_count_by_port) %>% 
  # get the unique values and arrange by port code (to look pretty)
  unique() %>% arrange(Port_code, Measure) %>% 
  # select what you need 
  select(Port_code, Measure, measure_porp)

# quick check to make sure proportions add up to 1
border_multinom_long %>% group_by(Port_code) %>% summarise(equals1 = sum(measure_porp))

# get the average and median mult distn... (should we use median or average? hard to tell)
aggregate_multinom_long <- border %>% 
  # calculate the average and median count by each measure
  mutate(average_measures = mean(Value), middle_measures = median(Value), .by = Measure) %>% 
  # select important variables and select the unique values
  select(Measure, average_measures, middle_measures) %>% unique() %>% 
  # calculate the porportions of each measure (for both the average and median measures)
  mutate(total_average_measures = sum(average_measures), 
         total_middle_measures = sum(middle_measures), 
         avg_measure_porp = average_measures / total_average_measures, 
         med_measure_porp = middle_measures / total_middle_measures) %>% 
  arrange(Measure) %>% select(Measure, avg_measure_porp, med_measure_porp)

# another quick check 
aggregate_multinom_long %>% summarise(equals1 = sum(avg_measure_porp), 
                                        equals1_agian = sum(med_measure_porp))

# both data sets have similar structure 
border_multinom_long 
aggregate_multinom_long 

# pivot them wider so you can compare distributions with the chi squared test  
border_multinom_wide <- border_multinom %>%
  pivot_wider(names_from = Measure, values_from = measure_porp)

avg_aggregate_multinom_wide <- border_aggregate_multinom %>% select(Measure, avg_measure_porp) %>% 
  pivot_wider(names_from = Measure, values_from = avg_measure_porp)

med_aggregate_multinom_wide <- border_aggregate_multinom %>% select(Measure, med_measure_porp) %>% 
  pivot_wider(names_from = Measure, values_from = med_measure_porp)


# write this as a function maybe??? 

for (i in seq_len(nrow(border_multinom_wide))){
  print(i)
}



apply(border_multinom_wide, 1, function(x) chisq.test(x[-1], border_aggregate_multinom$avg_measure_porp)
      )

sapply(nrow(border_multinom_wide), function(x) {chisq.test(border_multinom_wide[x, -1], 
                                                           p = border_aggregate_multinom$avg_measure_porp, 
                                                           simulate.p.value = TRUE)$p.value})

chisq.test(border_multinom_wide[2, -1], p = border_aggregate_multinom$avg_measure_porp, simulate.p.value = TRUE)


try <- border_multinom %>%  select(Port_code, Measure, measure_porp) %>% filter(Port_code == "0101")

all_multinom_distn[2, -1]
avg_multinom_distn
med_multinom_distn

yes <- all_multinom_distn %>% select(-Port_code) %>% slice(3) %>% 
  unlist(., use.names=FALSE)

test <- chisq.test(try$measure_porp, border_aggregate_multinom$avg_measure_porp)
test$p.value
chisq.test(yes,  border_aggregate_multinom$avg_measure_porp)$p.value

# goodness of fit test 
# H0 p_i = pi_i where pi_i is the average porportion of the mean
# Ha: p_i neq pi_i for some i, i = 1, ... 12





















border %>% filter(Port_name == "Eastport")

border2 <- border %>% mutate(Port_name2 = as.factor(case_when(
  Port_name == "Eastport" & Port_code == "3302" ~ "Eastport ID", 
  Port_name == "Eastport" & Port_code == "0103" ~ "Eastport ME", 
  .default = Port_name)))

border %>% select(Port_name, Port_code, State) %>% 
  filter(Port_name == "Eastport") %>% unique() %>% arrange(Port_name) 

border2 %>% select(Port_name, Port_name2, Port_code, State) %>% 
  filter(Port_code %in% c("3302","0103")) %>% unique()

border2 %>% select(Port_name2, Port_name, Port_code, State) %>% unique()
levels(border2$Port_name2)
