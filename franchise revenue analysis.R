library(tidyr)
library(tidyverse)
theme_set(theme_light())

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

view(media_franchises)

#how many distinct franchise are0 there

media_franchises %>% 
  count(franchise, sort = TRUE)

distinct_franchise <- media_franchises %>% 
  distinct(franchise, .keep_all = TRUE) #this will get all distinc franchse and keep all the rows belonging to them 


 franchise <- media_franchises %>% 
   group_by(franchise, original_media, year_created, creators, owners) %>% 
   summarise(categories = n(),
             total_revenue = sum(revenue),
             most_profitable = revenue_category[which.max(revenue)]) %>% # what this will do is since it has already been grouped it will look at the once with 
 #the highest value in the revenue and pull out the category 
   ungroup()
 
 view(franchise)
 
 #franchise level 
 
 #what are the biggest franchise 
 
 franchise %>% 
   arrange(desc(revenue)) %>% 
   head(16) %>% 
   mutate(franchise = fct_reorder(franchise, revenue)) %>% 
   ggplot(aes(franchise, revenue))+
   geom_col()+
   coord_flip()+
   labs(title = "What are the most profitable franchise of all time")

 
library(glue) # to make pasting easier
 
 top_frnachise <- franchise %>% 
   top_n(20, total_revenue)
 
 media_franchises %>% 
   semi_join(franchise %>% top_n(16, total_revenue), by = "franchise") %>% 
   mutate(franchise = glue("{franchise} ( {original_media}, {year_created})")) %>% 
   mutate(franchise = fct_reorder(franchise, revenue, sum),
          revenue_category  = fct_reorder(revenue_category, revenue, sum)) %>% 
   ggplot(aes(franchise, revenue, fill = revenue_category))+
   geom_col()+
   scale_y_continuous(labels = scales::dollar)+
   coord_flip()+
   guides(fill = guide_legend(reverse = TRUE))
   labs(title = "What are the most profitable franchise of all time",
        fill = "category",
        y = "Revenue(dollar)")
 
   
#what are the most profitable owners 
   
media_franchises %>% 
  group_by(owners) %>% 
  filter(n_distinct(franchise)>2) %>% 
  ungroup() %>% 
  mutate(franchise = fct_reorder(franchise, revenue, sum),
         owners = fct_reorder(owners, -revenue, sum),
         revenue_category  = fct_reorder(revenue_category, revenue, sum)) %>% 
  ggplot(aes(franchise, revenue, fill = revenue_category))+
  geom_col()+
  facet_wrap(~owners, scales = "free_y")+
  guides(fill = guide_legend(reverse = TRUE))+
  coord_flip()+
  labs(title = "What owners owns at least 3 franchises ",
       fill = "category",
       y = "Revenue(dollar)")

# visualization based on the year created 

franchise %>% 
  ggplot(aes(year_created, total_revenue))+
  geom_point(aes(size = total_revenue, color = original_media))+
  geom_text(aes(label = franchise), check_overlap = TRUE, vjust = 1, hjust = 1)+
  expand_limits(x = 1910)+
  labs(title = "when were the great franchise created?")+
  theme(legend.position = "none")


media_franchises %>%
  group_by(original_media, revenue_category) %>% 
  summarise(revenue = sum(revenue)) 


#What kind of media leads to what type of revenue
media_franchises %>%
  group_by(original_media) %>% 
  filter(sum(revenue) >= 45) %>% 
  ungroup() %>% 
  mutate(revenue_category = fct_reorder(revenue_category, revenue, sum),
         original_media = fct_reorder(original_media, -revenue, sum)) %>% 
  ggplot(aes(revenue_category, revenue))+
  geom_col()+
  scale_y_continuous(labels = scales::dollar)+
  coord_flip()+
  facet_wrap(~original_media)+
  labs(x = "Revenue Category",
       y = "Revenue (Billions)",
       title = "What kind of media leads to what type of revenue")


#creating a heat map


original_media_revenue_categories <- media_franchises %>%
  group_by(original_media) %>% 
  filter(sum(revenue) >= 45) %>% 
  group_by(original_media, revenue_category) %>% 
  summarise(revenue = sum(revenue)) %>% 
  ungroup() %>% 
  mutate(revenue_category = fct_reorder(revenue_category, revenue, sum),
         original_media = fct_reorder(original_media, -revenue, sum)) 

original_media_revenue_categories %>% 
  ggplot(aes(revenue_category, revenue))+
  geom_col()+
  scale_y_continuous(labels = scales::dollar)+
  coord_flip()+
  facet_wrap(~original_media)+
  labs(x = "Revenue Category",
       y = "Revenue (Billions)",
       title = "What kind of media leads to what type of revenue")


original_media_revenue_categories %>% 
  mutate(revenue_category = fct_reorder(revenue_category, -revenue, sum)) %>% 
  ggplot(aes( revenue_category, original_media, fill = revenue))+
  geom_tile()+
  scale_fill_gradient2(low = "white", high = "red")+
  theme(panel.grid = element_blank())# to remove black grid ins from graph


#usng the fct_rev function 
original_media_revenue_categories %>% 
  mutate(revenue_category = fct_rev(revenue_category),
         original_media = fct_rev(original_media)) %>% 
  ggplot(aes( revenue_category, original_media, fill = revenue))+
  geom_tile()+
  scale_fill_gradient2(low = "white", high = "red", label = scales::dollar)+
  theme(panel.grid = element_blank(),# to remove black grid ins from graph  
        axis.text.x = element_text(angle = 90, hjust = 1))+ #this is a trick to beautify lable on axis that are overlapping 
  labs(fill = "Revenue in Bilions")


#creating a plot tht has labels on the barchat 

library(glue) # to make pasting easier

top_frnachise <- franchise %>% 
  mutate(franchise = glue("{franchise} ( {original_media}, {year_created})")) %>% 
    top_n(20, total_revenue)

media_franchises %>% 
  mutate(franchise = glue("{franchise} ( {original_media}, {year_created})")) %>% 
  semi_join(top_frnachise, by = "franchise") %>% 
  mutate(franchise = fct_reorder(franchise, revenue, sum),
         revenue_category  = fct_reorder(revenue_category, revenue, sum)) %>% 
  ggplot(aes(franchise, revenue))+
  geom_col(aes(fill = revenue_category))+
  geom_text(aes(y =total_revenue, 
                label = paste0(scales::dollar(total_revenue, accuracy = 1), 'B')), 
            data = top_frnachise, #the data needs to be seperate from the one u r using 
            hjust = 0)+ 
  scale_y_continuous(labels = scales::dollar)+
  expand_limits(y = 100)+
  coord_flip()+
  theme(panel.grid.major.y =element_blank())
  guides(fill = guide_legend(reverse = TRUE))
 labs(title = "What are the most profitable franchise of all time",
     fill = "category",
     y = "Revenue(dollar)")
