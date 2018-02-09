library(tidyverse)
source('analyse.R')
load('result_data.RData')

# --  Helper Functions
tidylatency <- function(test_log){
  data.frame(
    latency = test_log %>% pull(latency_sec),
    users = test_log %>% pull(tar_sess) %>% unique(),
    stringsAsFactors = FALSE
  )
}

prepare <- function(test_logs, ratio = 1) {
  all <- map_df(test_logs, tidylatency)
  if (ratio > 1) {
    all$r_procs <- all$users / ratio
  } else {
    all$r_procs <- 1
  }
  grouping <- as.symbol(ifelse(ratio > 1, 'r_procs', 'users'))
  
  all %>% 
    group_by(!!grouping) %>% 
    summarise(avg = mean(latency), 
              med = median(latency),
              upper = quantile(latency, 0.75),
              lower = quantile(latency, 0.25)) %>% 
    right_join(all)
}

latency_plot <- function(prepped, title, grouping) {
  #facet <- paste0(grouping, '~ .')
  facet <- grouping
  ggplot(prepped) + 
    geom_density(aes_string(group = grouping, x = "latency")) +
    geom_point(aes_string(group = grouping, x = "avg"), y = 0.5) +
    geom_point(aes_string(group = grouping, x = "med"), y = 0.5, color = 'red') +
    facet_wrap(facet, 
      labeller = function(l){map_df(l, ~paste0(as.character(.x), ' ', grouping))},
      ncol = 1
    ) +
    scale_y_continuous(breaks = NULL) + 
    labs(
      title = NULL,
      caption = 'Red - Median; Black - Mean', 
      x = 'Latency (Secs)', 
      y = NULL
    ) +
   theme(
       text = element_text(size = 20)
   )
}


# -- First, look at the app without Caching

# Number of Users for 1 R process
# o_40_1 <- analyse('old_tests/40_1', '40', 'old_tests/30min.txt', 39)
# o_30_1 <- analyse('old_tests/30_1', '30', 'old_tests/30min.txt', 28)
# o_20_1 <- analyse('old_tests/20_1', '20', 'old_tests/30min.txt', 20)

nocache_single <- prepare(list(o_20_1, o_30_1,o_40_1))

latency_plot(nocache_single, 'Latency vs # of Users for 1 R process', 'users')

# Linear Scaling 
# o_60_2     <- analyse('old_tests/60_2', '60', 'old_tests/30min.txt', 60 )
# o_90_3     <- analyse('old_tests/90_3', '90', 'old_tests/30min.txt', 90 )
# o_120_4    <- analyse('old_tests/120_4', '120', 'old_tests/30min.txt', 120 )
# o_150_5    <- analyse('old_tests/150_5', '150', 'old_tests/30min.txt', 150)
# o_180_6    <- analyse('old_tests/180_6', '180', 'old_tests/30min.txt', 180)
# o_210_7    <- analyse('old_tests/210_7', '210', 'old_tests/30min.txt', 210)
# o_240_8    <- analyse('old_tests/240_8', '240', 'old_tests/30min.txt', 240)


nocache_many <- prepare(list(o_30_1, o_60_2,o_90_3,o_120_4,o_150_5, o_180_6, o_210_7, o_240_8), ratio = 30)

latency_plot(nocache_many, 'Nearly Linear Scaling: Latency vs # of R Procs (30 Users / Proc)', 'users')

# Some linear scaling plots
nocache_many %>% 
  select(users, Median = med, Average = avg, `75th Percentile` = upper, `25th Percentile` = lower) %>% 
  unique() %>% 
  ggplot() + 
    geom_line(aes(users, Median), color = 'red') +
    geom_line(aes(users, Average), color = 'black') +
    geom_errorbar(aes(users, ymax = `75th Percentile`, ymin = `25th Percentile`), alpha = 0.35) +
    labs(
      title = NULL,
      x = 'Users',
      y = 'Latency (Secs)'
    ) + 
    theme_minimal() +
    theme(text = element_text(size = 20)) 

data.frame(
  r_processes = 1:8,
  users = 540*1:8
) %>% 
  ggplot() + 
    geom_line(aes(r_processes, users))  +
    labs(
      title = NULL,
      x = 'R Processes',
      y = 'Users'
    ) +
    theme_minimal() +
    theme(text = element_text(size = 20)) 


# -- Next, look at the same metrics, but for the cache optimized app

# single process
# c_90_1  <- analyse('90_1', '90', '15min.log', 90)     
# c_60_1  <- analyse('60_1', '60', '15min.log', 60)
# c_120_1 <- analyse('120_1', '120', '15min.log', 120 )

cache_single <- prepare(list(c_60_1,c_90_1,c_120_1))

latency_plot(cache_single, 'Latency vs # of Users for 1 R process, 3x on Cache Optimized App', 'users')

# across processess
# c_180_2 <- analyse('180_2', '180', '15min.log',170)
# c_360_4 <- analyse('360_4', '360', '15min.log',360)
# c_540_6 <- analyse('540_6-2', '540', '15min.log',540)
# c_630_7 <- analyse('630_7', '630', '15min.log',600)
# c_720_8 <- analyse('720_8', '720', '15min.log',720)

cache_many <- prepare(list(c_180_2, c_360_4, c_540_6, c_630_7, c_720_8), 90)

latency_plot(cache_many, 'Not-so Linear Scaling: Latency vs # of R Procs (90 Users / Proc)', 'r_procs')

# across nodes
# c_2700_30 <- analyse('2700_30', '2700', '15min.log', 2700)
# c_1080_12 <- analyse('1080_12', '1080', '15min.log', 1080) 

cache_nodes <- prepare(list(c_540_6, c_1080_12, c_2700_30), 90)

latency_plot(cache_nodes, 'Linear Scaling Across Nodes (540 Users / Node)', 'users')                       
