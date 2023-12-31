# trying to look at the multinomial distribution of each port's counts

library(tidyverse)
load("data/border_clean.Rdata")

# stream lined multinomimal stuff -----


do_chi_sq <- function(df){
  median_distn <-  df %>% 
    mutate(med_measures = mean(Value), .by = Measure) %>% 
    select(Measure, med_measures) %>% unique() %>% 
    mutate(tot_med_measures = sum(med_measures), 
           porp_med_measures = med_measures / tot_med_measures) %>% 
    arrange(Measure) %>% select(Measure, porp_med_measures)
  
  new_df <- df %>% 
    mutate(measure_count_by_port = sum(Value), .by = c(Measure, Port_code)) %>%
    select(Port_name, Measure, measure_count_by_port) %>% 
    unique() %>% arrange(Port_name, Measure) %>% 
    pivot_wider(names_from = Measure, values_from = measure_count_by_port) %>% 
    replace(is.na(.), 0) %>% mutate( # using . means the current data frame :) 
      Test_stat = apply(.[-1], 1, function(x) 
        chisq.test(x, p =median_distn$porp_med_measures)$statistic)) %>% 
    select(Port_name, Test_stat) # %>% left_join(border, ., by = join_by(Port_name))
  
  return(new_df)
  
}


both_borders <- rbind(
  do_chi_sq(border %>% filter(Border == "US-Mexico Border")), 
  do_chi_sq(border %>% filter(Border == "US-Canada Border")))

border_test_stat <- border %>% left_join(both_borders, by = join_by(Port_name)) 

make_graph <- function(df){
  biggest_porp <- c("Personal Vehicles", "Personal Vehicle Passengers", 
                    "Pedestrians")
  
  graph <- df %>% get_porp() %>% 
    mutate(Measure = fct_relevel(Measure, biggest_porp, after = 0)) %>% 
    ggplot(aes(x = reorder(Port_name, Test_stat), y = measure_porp, 
               fill = Measure)) + 
    geom_col() + coord_flip() + 
    theme(plot.title = element_text(hjust=0.5)) + # legend.position = "bottom"
    xlab("") + ylab("Porportion by Measure") 
  return(graph)
}

get_porp <- function(df){
  df2 <- df %>% 
    mutate(measure_count_by_port = sum(Value), .by = c(Measure, Port_name)) %>%
    mutate(total_count_by_port = sum(Value), .by = Port_name) %>% 
    mutate(measure_porp = measure_count_by_port / total_count_by_port) %>% 
    select(Port_name, Test_stat, measure_porp, Measure) %>% unique()
  return(df2)
}

can <- make_graph(border_test_stat %>% filter(Border == "US-Canada Border")) + 
  ggtitle("US-Canada Border")

mex <- make_graph(border_test_stat %>% filter(Border == "US-Mexico Border")) + 
  ggtitle("US-Mexico Border")


mex
can

# re do witih the porportions (not the totals)
do_chi_sq2 <- function(df){
  median_distn <-  df %>% 
    mutate(med_measures = mean(Value), .by = Measure) %>% 
    select(Measure, med_measures) %>% unique() %>% 
    mutate(tot_med_measures = sum(med_measures), 
           porp_med_measures = med_measures / tot_med_measures) %>% 
    arrange(Measure) %>% select(Measure, porp_med_measures)
  
  new_df <- df %>% 
    mutate(measure_count_by_port = sum(Value), .by = c(Measure, Port_name)) %>%
    mutate(total_count_by_port = sum(Value), .by = Port_name) %>% 
    mutate(measure_porp = measure_count_by_port / total_count_by_port) %>% 
    select(Port_name, Measure, measure_porp) %>% 
    unique() %>% arrange(Port_name, Measure) %>% 
    pivot_wider(names_from = Measure, values_from = measure_porp) %>% 
    replace(is.na(.), 0) %>% mutate( # using . means the current data frame :) 
      Test_stat = apply(.[-1], 1, function(x) 
        chisq.test(x, p =median_distn$porp_med_measures)$statistic), 
      simulate.p.value = TRUE) %>% 
    select(Port_name, Test_stat) # %>% left_join(border, ., by = join_by(Port_name))
  
  return(new_df)
  
}


both_borders2 <- rbind(
  do_chi_sq2(border %>% filter(Border == "US-Mexico Border")), 
  do_chi_sq2(border %>% filter(Border == "US-Canada Border")))

border_test_stat2 <- border %>% left_join(both_borders2, by = join_by(Port_name)) 

can1 <- make_graph(border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
                     filter(Test_stat > quantile(Test_stat, .7))) + 
  ggtitle("US-Canada Border (1)") + theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


can2 <- make_graph(border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
                     filter(Test_stat >= quantile(Test_stat, .37) & 
                              Test_stat <= quantile(Test_stat, .7))) + 
  ggtitle("US-Canada Border (2)") + theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


can3 <- make_graph(border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
                     filter(Test_stat < quantile(Test_stat, .37))) + 
  ggtitle("US-Canada Border (3)") + theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


mex1 <- make_graph(border_test_stat2 %>% filter(Border == "US-Mexico Border")) + 
  ggtitle("US-Mexico Border") + theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))


leg <- ggpubr::as_ggplot(ggpubr::get_legend(make_graph(border_test_stat2) + 
                                              theme(legend.position = "bottom")))


four_grid <- gridExtra::grid.arrange(mex1, can1, can2, can3, nrow = 2, ncol = 2)

gridExtra::grid.arrange(four_grid, leg, heights = c(5,1), padding = 0.5)


all <- make_graph(border_test_stat2) + 
  ggtitle("All Ports of Entry")

border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
  filter(Test_stat < quantile(Test_stat, .37)) %>% 
  select(Port_name, Test_stat) %>% unique() 

border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
  filter(Test_stat > quantile(Test_stat, .7)) %>% 
  select(Port_name) %>% unique() 


border_test_stat2 %>% filter(Border == "US-Canada Border") %>% 
  filter(Test_stat >= quantile(Test_stat, .37) & Test_stat <= quantile(Test_stat, .7)) %>% 
  select(Port_name) %>% unique()

can2
can22
mex2
all

border_test_stat %>% arrange(-Test_stat) %>%  select(Port_name, Test_stat, Border) %>% 
  unique() %>% filter(Border == "US-Mexico Border")

border_test_stat2 %>% arrange(Test_stat) %>%  select(Port_name, Test_stat, Border) %>% 
  unique() %>% filter(Border == "US-Mexico Border")

# border_test_stat2 %>% arrange(Test_stat) %>%  select(Port_name, Test_stat, Border) %>% 
#   unique() %>% filter(Border == "US-Mexico Border")

# older revamped multinomail stuff --------------

# old plots 
mex <- border_test_stat %>% filter(Border == "US-Mexico Border") %>% 
  ggplot(aes(x = reorder(Port_name, Test_stat), fill = Measure)) + 
  geom_bar(position = "identity") + coord_flip() + 
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) +
  xlab("") + ylab("") + ggtitle("US-Mexico Border")

can <- border_test_stat %>% filter(Border == "US-Canada Border") %>% 
  ggplot(aes(x = reorder(Port_name, Test_stat), fill = Measure)) + 
  geom_bar(position = "fill") + coord_flip() + 
  theme(legend.position = "none", plot.title = element_text(hjust=0.5)) +
  xlab("") + ylab("Porportion by Measure") + ggtitle("US-Canada Border") 


# overal_median_distn <-  border %>% 
#   # filter(Border == "US-Mexico Border", Type == "People") %>% 
#   mutate(med_measures = mean(Value), .by = Measure) %>% 
#   select(Measure, med_measures) %>% unique() %>% 
#   mutate(tot_med_measures = sum(med_measures), 
#          porp_med_measures = med_measures / tot_med_measures) %>% 
#   arrange(Measure) %>% select(Measure, porp_med_measures)
# 
# 
# # get the total counts of each measure by each port 
# border2 <- border %>% 
#   # filter(Border == "US-Mexico Border", Type == "People") %>% 
#   mutate(measure_count_by_port = sum(Value), .by = c(Measure, Port_code)) %>%
#   select(Port_name, Measure, measure_count_by_port) %>% 
#   unique() %>% arrange(Port_name, Measure) %>% 
#   pivot_wider(names_from = Measure, values_from = measure_count_by_port) %>% 
#   replace(is.na(.), 0) %>% mutate( # using . means the current data frame :) 
#     Test_stat = apply(.[-1], 1, function(x) 
#       chisq.test(x, p =overal_median_distn$porp_med_measures)$statistic)) %>% 
#   select(Port_name, Test_stat) %>% left_join(border, ., by = join_by(Port_name))


# get the total counts of each measure by each port 
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
  select(Port_code, Measure, measure_count_by_port)


# make it wide 
border_multinom_wide <- border_multinom_long %>%
  pivot_wider(names_from = Measure, values_from = measure_count_by_port)

# get the average and median mult distn... 
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

# individual test 
chisq.test(border_multinom_wide[2, -1], p = aggregate_multinom_long$avg_measure_porp)$p.value

# dosen't work since they don't have complete cases :(
apply(border_multinom_wide[-1], 1, function(x) chisq.test(x, p =aggregate_multinom_long$avg_measure_porp)$p.value)

# 4 ports with incomplete cases 
border_multinom_wide %>% filter(!complete.cases(border_multinom_wide)) 

# get the complete cases 
complete_border_multinom_wide <- border_multinom_wide %>% filter(complete.cases(border_multinom_wide))

apply(complete_border_multinom_wide[-1], 1, function(x) 
  chisq.test(x, p =aggregate_multinom_long$med_measure_porp, simulate.p.value = TRUE)$p.value)

# old work --------------
# the giant graph hehe
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




# looking at the states --------------
border %>% select(Port_name, State) %>% unique() %>% count(State) %>% arrange(-n)

border %>% select(Port_name, State, Border) %>% unique() %>% 
  ggplot(aes(x = fct_infreq(State), 
             #fill = Border
             )) + 
  geom_bar(stat = "count") + coord_flip() + 
  ylab("Ports of Entry Count") + xlab("State") + 
  ggtitle("Port of Entry Counts by State")

border %>% select(Port_name, State, Border) %>% unique() %>% 
  ggplot(aes(x = fct_rev(fct_infreq(State)))) + geom_bar(stat = "count") + coord_flip() + 
  ylab("Ports of Entry Count")


# border %>% select(Port_name, State) %>% unique() %>% filter(State == "North Dakota")


