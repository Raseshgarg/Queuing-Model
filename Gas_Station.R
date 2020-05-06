#install.packages('simmer')
#install.packages('simmer.plot')
#install.packages('xlsx')
#install.packages('parallel')
library(simmer.plot)
library(simmer)
library(dplyr)
library(tidyr)
library(parallel)
library(xlsx)

### Setting initial parameters of the model
max_wait_time = 15
simulation_time  = 60*7 # 7 hours, say friday 5-12 pm
mean_service_time = 4
customer_inter_arrival_time = 2/3 # i.e 3 cars coming every 2 minutes
no_of_simulations = 500
output_file_name = "Model_C.xlsx"

### Setting up the system (customer trajectory and environment)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(gas)}) %>%
  
  renege_in(max_wait_time,
            out = trajectory("Reneging customer") %>%
              log_(function() {
                paste("Waited", now(gas) - get_attribute(gas, "start_time"), "I am off")
              })) %>%
  
  simmer::select(c("counter1", "counter2","counter1"), policy = "shortest-queue") %>%
  seize_selected() %>%
  renege_abort() %>% 
  log_(function() {paste("Waited: ", now(gas) - get_attribute(gas, "start_time"))}) %>%
  timeout(function() {rexp(1,1/mean_service_time)}) %>%
  release_selected() %>%
  log_(function() {paste("Finished: ", now(gas))})

####

gas <-
  simmer("gas") %>%
  ### Adding resources (counters with respective number of servers)
  add_resource("counter1",2) %>%  ### 2 is for number of servers per queue
  add_resource("counter2", 2) %>%
  add_resource("counter3", 1) %>%
  ## generating customers
  ## If number of customers follow poisson distribution, then the interarrival time follows a exponential distribution. 
  add_generator("Customer", customer, function() {c(0, rexp(1000, 1/customer_inter_arrival_time), -1)})

## WSimulating above set up many times

envs <- mclapply(1:no_of_simulations, function(i) {
  gas %>% run(until = simulation_time) %>%
    wrap()
})

#======================================================

## Estimating Target Parameters
# ---- waiting time
waiting_time <- function(x) {
  a = get_mon_arrivals(x) %>% filter(finished == TRUE, activity_time > 0 ) %>%
    mutate(waiting_time = end_time - start_time - activity_time)
  return(mean(a$waiting_time))
}
x  = lapply(envs, waiting_time )

waiting = c('Waiting_time', mean(unlist(x)),mean(unlist(x)) - 1.96*sd(unlist(x)),mean(unlist(x)) + 1.96*sd(unlist(x)))

# ----- % customers served

customers_served <- function(x) {
  a = get_mon_arrivals(x) %>% filter(finished == TRUE)
  y = (length(which(a$activity_time > 0))/nrow(a))*100
  return(y)
}
x  = lapply(envs, customers_served )

srvd_cust = c('Percentage_of_customers_served', mean(unlist(x)),mean(unlist(x)) - 1.96*sd(unlist(x)),mean(unlist(x)) + 1.96*sd(unlist(x)))

# -------- mean time in system

system_time <- function(x) {
  a = get_mon_arrivals(x) %>% filter(finished == TRUE, activity_time > 0) %>%
    mutate(sys_time = end_time - start_time) %>%
    filter(finished == TRUE, activity_time > 0)
  return(mean(a$sys_time))
}
x  = lapply(envs, system_time )
system= c('Time_spent_in_system', mean(unlist(x)),mean(unlist(x)) - 1.96*sd(unlist(x)),mean(unlist(x)) + 1.96*sd(unlist(x)))

#### -------- Queue Length
qu_length <- function(x) {
  a = get_mon_resources(x) 
  return(mean(a$queue))
}
x  = lapply(envs, qu_length )

qu = c('Queue_length', mean(unlist(x)),mean(unlist(x)) - 1.96*sd(unlist(x)),mean(unlist(x)) + 1.96*sd(unlist(x)))

#### -------- utilisation 

utilisation <- function(x) {
  a = get_mon_resources(x) 
  return((sum(a$server)/sum(a$capacity))*100)
}
x  = lapply(envs, utilisation)

ifelse for start time, ifelse for end time

util_res = c('resource_utilization', mean(unlist(x)),mean(unlist(x)) - 1.96*sd(unlist(x)),min(100, mean(unlist(x)) + 1.96*sd(unlist(x))))

##### Combining all metrices into a dataframe

final = data.frame(rbind(waiting, system,srvd_cust, qu , util_res ))
colnames(final) = c('Parameter', 'Expected mean', "95%_CI_lower_bound", "95%_upper_bound")
final[,-1] = apply(final[,-1], 2,as.numeric)
final[,-1] = round(final[,-1],1) 
rownames(final) = NULL

##### Writing output to a Excel File
customer_arrival_monitor <- envs[1] %>% get_mon_arrivals()
customer_arrival_monitor <- customer_arrival_monitor[order(customer_arrival_monitor$start_time),]
resource_monitor <- envs[1] %>% get_mon_resources() %>% select(-c(queue_size,limit))

### Writing monitor data for a single simulation
require(openxlsx)
list_of_datasets <- list("customer_arrival_monitor" =customer_arrival_monitor, "resource_monitor" = resource_monitor, "summary_post_1000_simulation" = final)
write.xlsx(list_of_datasets, file = output_file_name  )

##########   Evolution of waiting time plot

arrivals <- get_mon_arrivals(envs)
plot(arrivals, metric = "waiting_time")+
  labs(subtitle="Includes all customers (both served and not served)", 
       y="Waiting Time (mins)", 
       x="Simulation Time (mins)", 
       title="Waiting Time Evolution")
