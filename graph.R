library(tidyverse)
library(ggrepel)

df <- readxl::read_xlsx('data/Start_Specialisationg_Peak_Age.xlsx', sheet = 'Sheet2') %>% 
  janitor::clean_names() %>% 
  select(sport:average_peak_age) %>% 
  drop_na()

df %>% 
  mutate(age_range = specialisation_age - start_age)

df %>% 
  ggplot(aes(x=start_age, y = specialisation_age, label = sport))+
  geom_point(size = 1) +
  geom_text_repel(size = 2) +
  geom_vline(xintercept = 6, linetype = 'longdash', alpha = 0.5) +
  geom_vline(xintercept = 12, linetype = 'longdash', alpha = 0.5)+ 
  geom_hline(yintercept = 12, linetype = 'longdash', alpha = 0.5)+ 
  geom_hline(yintercept = 16, linetype = 'longdash', alpha = 0.5) +
  scale_x_continuous(limits = c(3, 20), breaks = 3:20) +
  scale_y_continuous(limits = c(10, 23),breaks = 10:20)+
  annotate(geom="text", x=4, y=23, label="Early Start",
           color="black", size = 2.5) +
  annotate(geom="text", x=9, y=23, label="Intermediate Start",
           color="black", size = 2.5) +
  annotate(geom="text", x=14, y=23, label="Late Start",
           color="black", size = 2.5)+
  annotate(geom="text", x=19, y=11, label="Early Specialisation",
           color="black", size = 2.5) +
  annotate(geom="text", x=19, y=14, label="Intermediate Specialisation",
           color="black", size = 2.5)+
  annotate(geom="text", x=19, y=17, label="Late Specialisation",
           color="black", size = 2.5) +
  theme_minimal()+
  labs(x = 'mean starting age', y = 'mean specialisation age')
        
## extra code 
# df %>% 
#   ggplot()+
#   geom_segment(aes(y=sport, yend = sport, x = specialisation_age, xend = average_peak_age), size = 4, color = 'skyblue2' ) +
#   geom_point(aes(y = sport, x = start_age), colour = 'skyblue4') +
#   xlab('age') +
#   scale_x_continuous(breaks=c(0,5,10, 15, 20, 25, 30, 35, 40, 45))
# 
# 
# df %>% 
#   ggplot()+
#   geom_segment(aes(x=average_peak_age, xend = average_peak_age, y = specialisation_age, yend = average_peak_age), size = 4, color = 'skyblue2' ) +
#   geom_label_repel(aes(y = start_age, x = average_peak_age, label = sport)) + 
#   coord_flip() +
#   xlab('age') +
#   scale_x_continuous(breaks=c(0,5,10, 15, 20, 25, 30, 35, 40, 45))

