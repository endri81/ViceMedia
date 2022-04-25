Posting by year

```{r}
vice_data_cl %>%  
  mutate(year = lubridate::year(post_created)) %>% 
  group_by(year) %>% summarise(freq = n()) -> year_freqs 
ggplot(year_freqs, aes(x=year, y=freq)) +
  geom_bar(fill = 'green', stat='identity') 
```

Posting by month


```{r}
vice_data_cl %>%  
  mutate(year = lubridate::year(post_created)) %>% 
  mutate(month = lubridate::month(post_created, label=TRUE)) %>%   
  group_by(year, month) %>% 
  summarise(freq = n())  -> month_freqs 
# subset 2 months around flood
month_freqs %>%
  ggplot(aes(x = month, y = freq)) +
  geom_bar(stat = "identity", fill = "darkorchid4") +
  facet_wrap(~ year, ncol = 1) +
  labs(title = "Monthly Video Postings",
       subtitle = "Data plotted by year",
       y = "Monthly Postings",
       x = "Month")
```

Posting by day


```{r}
vice_data_cl %>%  
  mutate(year = lubridate::year(post_created)) %>% 
  mutate(day = lubridate::date(post_created)) %>% 
  group_by(year, day) %>% 
  summarise(freq = n())  -> day_freqs
ggplot(day_freqs, aes(x = day, y = freq)) + 
  geom_line(aes(color = factor(year))) 

```


Video Share Status – owned (originally released by that page) vs crosspost (reposted from a different page);


```{r}
vice_data_cl %>%  select(post_created, video_share_status) %>% 
  mutate(day = lubridate::date(post_created)) %>% 
  group_by(day, video_share_status) %>% 
  summarise(freq = n()) %>%
  subset(!is.na(video_share_status))   %>%
  ggplot(aes(
    x = day,
    y = video_share_status,
    height = freq,
    fill = video_share_status
  )) +
  geom_density_ridges(
    show.legend = FALSE,
    stat = "identity",
    scale = 3,
    alpha = 0.5
  ) +
  theme_bw() +
  scale_fill_manual(values = rep(c(
    "#005666",
    "#60BCC4",
    "#8E0000",
    "#F48B0B",
    "#11F6CD"
  ), 4)) +
  labs(title = "A Ridgeline Plot of Video Share Status ", 
       x = NULL,
       y = NULL)
```