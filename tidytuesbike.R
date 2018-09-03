
a <- read.csv("2016_07.csv")

files <- list.files(full.names = T) %>%
  map(read_csv, skip = 1)

fnames <- list.files()
csv <- lapply(fnames, read.csv)
result <- do.call(rbind, csv)

result2 <- result %>% separate(Duration, c("Ho", "M", "S"), ":")

result2$Ho <- as.numeric(as.character(result2$Ho))
result2$M <- as.numeric(as.character(result2$M))
result2$S <- as.numeric(as.character(result2$S))

result3 <- result2 %>% mutate(tottime = (Ho*60+M+(S/60))/60) %>%
                      mutate(speed = Distance_Miles/tottime) %>%
                      filter(Distance_Miles < 25) %>%
                      filter(Distance_Miles > 1) %>%
                      filter(speed < 100) %>%
                      filter(TripType == "") %>%
                      group_by(Distance_Miles, TripType, PaymentPlan) %>%
                      summarise(maxspeed = max(speed))
cols <- c("Casual" = "#FFBA08", "Subscriber" = "#3F88C5")

ggplot(result3, aes(x = Distance_Miles, y = maxspeed, col =PaymentPlan)) + geom_smooth(alpha = 0.3, size = 3) +
  labs(x = "Distance (Miles)", y = "Top Speed MPH", title = "Camparison of top speeds of Causal and Subscribers") +
  scale_color_manual(values = cols)+
  guides(col = guide_legend(title = "Payment Plan")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.8))

result4 <- result %>% group_by(PaymentPlan) %>%
                      summarise(tot = n())

ggplot(result4, aes(x = PaymentPlan, y = tot, fill = PaymentPlan)) + geom_col() +
            labs(x = "Payment Plan", y = "Total") + theme_minimal()

over <- result %>% group_by(PaymentPlan) %>%
                    summarise(tot = n())


result5 <- result %>% filter(PaymentPlan != "" ) %>%
                        group_by(PaymentPlan, TripType) %>%
                              summarise(tot = n())

result6 <- result5 %>% filter(PaymentPlan == "Casual") %>%
                          filter(TripType != "")  %>%
                            arrange(tot) %>%
                                mutate(TripType = factor(TripType, levels = .$TripType))

ggplot(result6, aes(x = TripType, y = tot)) + 
  geom_segment(aes(x = TripType, y = 0, xend = TripType, yend = tot)) +
  geom_point(size = 8, col = "#FFBA08") + 
  coord_flip() +
  labs(x = "Trip Type", y = "Total", title = "Casual Users breakdown of Trips") +
  theme_minimal()
                  
result7 <- result5 %>% filter(PaymentPlan == "Subscriber") %>%
             filter(TripType != "")  %>%
               arrange(tot) %>%
                 mutate(TripType = factor(TripType, levels = .$TripType))
                        

ggplot(result7, aes(x = TripType, y = tot)) + 
  geom_segment(aes(x = TripType, y = 0, xend = TripType, yend = tot)) +
  geom_point(size = 8,col ="#3F88C5") + 
  coord_flip() +
  labs(x = "Trip Type", y = "Total", title = "Subscriber Users breakdown of Trips") +
  theme_minimal()


result8 <- result %>% filter(PaymentPlan != "") %>%
                        group_by(PaymentPlan) %>%
                          select(PaymentPlan, Distance_Miles) %>%
                            filter(Distance_Miles <20)


ggplot(result8 , aes(x = Distance_Miles, fill = PaymentPlan, col = PaymentPlan)) + geom_density(alpha= 0.5) +
scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(x = "Distance (Miles)", y = "Density") +
  guides(fill = guide_legend(title = "Payment Plan"), col = FALSE) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.8))


result9 <- result %>% filter(PaymentPlan != "") %>%
                        group_by(PaymentPlan, RentalAccessPath) %>%
                          summarise(tot = n())


map5 <- get_map(location=c(lon=-122.67,  lat=45.52), zoom=14, maptype='roadmap')

ggplot(result9, aes(x = PaymentPlan, y = tot, fill = as.factor(RentalAccessPath))) + 
  geom_bar(stat = "identity") +
    labs(x = "Payment Plan", y = "Total") +
     guides(fill = guide_legend(title = "Payment Method"))+
      theme_minimal()
  
result10 <- result %>% filter(PaymentPlan == "Casual") %>% 
                                select(StartLatitude, StartLongitude)

map5 <- get_map(location=c(lon=-122.67,  lat=45.52), zoom=14, maptype = 'roadmap', color = 'bw')

map1 <- ggmap(map5) +
 geom_density2d(data = result10, aes(x = StartLongitude, y = StartLatitude)) +
  stat_density2d(data = result10, aes(x = StartLongitude, y = StartLatitude,fill = ..level.. , alpha = ..level..), size = 0.5, bins = 100, geom = "polygon") +
  scale_fill_gradient(low = "white", high = "#FFBA08") +
    scale_alpha(range = c(0, 1), guide = FALSE) +
    labs(title = "Casual Start Locations") +
    guides(fill = guide_legend(title= "Number of People")) +
    theme(legend.position = c(0.85,0.95),
axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y =element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

result12 <- result %>% filter(PaymentPlan == "Casual") %>% 
                        select(EndLatitude, EndLongitude)

map2 <- ggmap(map5) +
  geom_density2d(data = result12, aes(x = EndLongitude, y = EndLatitude)) +
  stat_density2d(data = result12, aes(x = EndLongitude, y = EndLatitude, fill = ..level.., alpha = ..level..), size = 0.5, bins = 100, geom = "polygon") +
  scale_fill_gradient(low = "white", high = "#FFBA08") +
  scale_alpha(range = c(0, 1), guide = FALSE)  +
  labs(title = "Casual End Locations") +
  guides(fill = guide_legend(title= "Number of People")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.85,0.95))
  
grid.arrange(map1, map2, ncol = 2)

result11 <- result %>% filter(PaymentPlan == "Subscriber") %>% 
  select(StartLatitude, StartLongitude)

map3 <- ggmap(map5) +
  geom_density2d(data = result11, aes(x = StartLongitude, y = StartLatitude)) +
  stat_density2d(data = result11, aes(x = StartLongitude, y = StartLatitude, fill = ..level.., alpha = ..level..), size = 0.5, bins = 50, geom = "polygon") +
  scale_fill_gradient(low = "white", high = "#3F88C5") +
  scale_alpha(range = c(0, 1), guide = FALSE)  +
  labs(title = "Subscriber Start Locations") +
  guides(fill = guide_legend(title= "Number of People")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.85,0.95))



result13 <- result %>% filter(PaymentPlan == "Subscriber") %>% 
  select(EndLatitude, EndLongitude)

map4 <- ggmap(map5) +
  geom_density2d(data = result13, aes(x = EndLongitude, y = EndLatitude)) +
  stat_density2d(data = result13, aes(x = EndLongitude, y = EndLatitude, fill = ..level.., alpha = ..level..), size = 0.5, bins = 50, geom = "polygon") +
  scale_fill_gradient(low = "white", high = "#3F88C5") +
  scale_alpha(range = c(0, 1), guide = FALSE)  +
  labs(title = "Subscriber End Locations") +
  guides(fill = guide_legend(title= "Number of People")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.85,0.95))

grid.arrange(map3, map4, ncol = 2)

result14 <- result %>% separate(StartTime, c("Ho", "M", "S"), ":") %>%
                          filter(PaymentPlan != "") %>%
                            group_by(PaymentPlan, Ho) %>%
                              summarise(tot = n())

result14$Ho <- as.numeric(as.character(result14$Ho))

ggplot(result14, aes(x = Ho,y = tot, col = PaymentPlan, fill= PaymentPlan)) + 
                                  geom_line(size = 2) +
                                      scale_color_manual(values = cols) +
                                        labs(x= "Hour", y = "Total Users") +
                                        guides(col = guide_legend(title = "Payment Plan")) +
                                            theme_minimal()



