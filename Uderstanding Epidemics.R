library(IDDA)
library(plotly)
library(tidyverse)
library(scales)


# Loading the dataset

df <- IDDA::state.long |> 
  filter(State=="Iowa") |> 
  arrange(DATE) |> 
  mutate(Y_infected =Infected-lag(Infected)) |> 
  filter(!is.na(Y_infected))



 # Time series plot of disease cases

p <- df |> 
  ggplot(aes(DATE,Y_infected)) +
  geom_line(linewidth=1.5, color="firebrick") +
  labs(x="Date",y="Count",title = "Daily New Infected Cases in Iowa")+
  theme(legend.position = "bottom") + theme_light()


ci <- IDDA::fore[c("mean","lower","upper")] |> as.data.frame()

names(ci) <- c("mean","lower","upper")

ci$date <- tail(df$DATE,1) + c(1:length(IDDA::fore$mean))
ci


p  + geom_line(data = ci,aes(date, mean), linewidth=0.8,
               color="darkgreen", 
               key_glyph="timeseries") +
  scale_x_date(limits =as.Date(c("2020-10-10","2021-01-15")),date_breaks =  "7 days")+
  scale_color_manual(" ",values = "red")

cases_1000 <- IDDA::state.long |> 
  # select(-Y_infected) |> 
  group_by(State) |> 
  summarise(total_pop=sum(pop, na.rm = TRUE),
            total_infected=sum( Infected, na.rm = TRUE),
            total_Deaths=sum(Death,na.rm = TRUE))





cases <- IDDA::state.long |> 
   group_by(DATE) |> 
  summarise(total_cases =sum(Infected, na.rm=TRUE)) |> 
  arrange(DATE)
  
ggplot(data = cases, aes(x = DATE)) +
  theme_bw() +
  geom_histogram(binwidth = 7, colour = "gray", fill = "dark blue", size = 0.1) +
  geom_density(aes(y = after_stat(density) * (nrow(cases) * 7)), colour = "red") +
  scale_x_date(breaks = date_breaks("2 weeks"), labels = date_format("%d %b"), 
               name = "Date") +
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 5), limits = c(0,30), name = "Number of cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

