#Loading Data

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Loading Packages

library(ggplot2)

library(usmap)

library(stringr)

library(dplyr)


counties_with_state <- incarceration %>%
  select(year,state,county_name,total_jail_pop, total_jail_adm, latinx_pop_15to64, black_pop_15to64, white_pop_15to64, black_jail_pop, latinx_jail_pop, white_jail_pop)%>%
  mutate(location=paste0(county_name, ",", state))


counties_with_state <- na.omit(counties_with_state)


counties_with_state <- subset(counties_with_state, year!= "1970" & year!= "1971" & year!= "1972" & year!="1973" & year!= "1974" & year!="1975" & year!= "1976" & year!= "1977" & year!= "1978" & year!= "1979" & year!="1980" & year!="1981" & year!="1982" & year!="1983" & year!="1984" & year!="1985" & year!= "1986" & year!="1987" & year!="1988" & year!="1989")

#5 Values of Interest
highest_adm_rate <- counties_with_state%>%
  filter(total_jail_adm == max(total_jail_adm))%>%
  pull(total_jail_adm, location)

highest_black_jail_pop <- max(counties_with_state$black_jail_pop, na.rm=TRUE)


highest_white_jail_pop <- max(counties_with_state_$white_jail_pop, na.rm = TRUE)

highest_native_jail_pop <-incarceration%>%
  filter(native_jail_pop == max(native_jail_pop, na.rm=TRUE))%>%
  pull(native_jail_pop, county_name)

latinx_highest_current <- counties_with_state%>%
  filter(year=="2018")%>%
  filter(latinx_jail_pop == max(latinx_jail_pop))%>%
  pull(latinx_jail_pop, location)


black_most_incarcerated <- counties_with_state%>%
  filter(black_jail_pop == max(black_jail_pop, na.rm=TRUE))%>%
  pull(black_jail_pop, year)

latinx_most_incarcerated <- counties_with_state%>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm=TRUE))%>%
  pull(latinx_jail_pop, year)

white_most_incarcerated <- counties_with_state%>%
  filter(white_jail_pop == max(white_jail_pop, na.rm=TRUE))%>%
  pull(white_jail_pop, year)




#Refining data for time trends chart


LA_county_df <- counties_with_state%>%
  filter(location == "Los Angeles County,CA")




# Basic line plot with points

 ggplot(LA_county_df, aes(x =year)) +
  geom_line(aes(y = black_jail_pop, color = "black_jail_pop")) +
  geom_line(aes(y = latinx_jail_pop, color = "latinx_jail_pop")) +
  geom_line(aes(y = white_jail_pop, color = "white_jail_pop")) +
  ggtitle("Incarceration Population for Black, Latinx, and White people in LA County Jail from 1993-2018") +
  scale_color_manual(name = "Race",
                     labels = c("Black jail pop ", "Latinx jail pop", "White jail pop"),
                     values = c("red", "green", "blue"))+
  xlab("")+
  scale_y_continuous("Incarceration Number")




#Variable Comparison Chart
library(ggplot2)
library(tidyverse)


race_jail_df <- incarceration %>%
select(year, black_jail_pop, white_jail_pop, latinx_jail_pop)%>%
  group_by(year)%>%
  summarise(n=n(), across(where(is.numeric), ~ sum(.x, na.rm=TRUE)))%>%
  gather("key", "value", -c(year,n))

variable_comparison <- ggplot(race_prison_df, aes(x=year, y=value, group=key, fill=key))+
  geom_col()+
  ggtitle("Black, Latinx and White Jail Population Comparison")+
  scale_fill_discrete(labels=c("Black Jail Population", "White Jail Population", "Latinx Jail Population"))







#Map Chart

library(usmap)
library(ggplot2)

black_pop_df <- counties_with_state %>%
   select(year, state, county_name, black_jail_pop, location)%>%
filter(year== "2018")


summary_table <- black_pop_df %>%
  group_by(state) %>%
  summarize("total_black_jail_pop" = sum(black_jail_pop), na.rm=TRUE)

 plot_usmap(data= summary_table, values = "total_black_jail_pop")+
  scale_fill_continuous(low="white", high="red", name = "incarceration rate")+
  ggtitle("Black Incarceration Population by U.S. State, 2018")


