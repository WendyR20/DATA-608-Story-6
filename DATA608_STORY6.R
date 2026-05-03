library(ipumsr)
library(tidyverse)
library(haven)
library(scales)
library(ggrepel)
library(tidycensus)

#reading in the data and the codebook provided by ipumsr
ddi <- read_ipums_ddi("cps_00003.xml")
data <- read_ipums_micro(ddi)


#reading in child mortality data
url <- "https://raw.githubusercontent.com/WendyR20/DATA-608-Story-6/refs/heads/main/data-table.csv"

infant_mortality <- read_csv(url)

#taking a look at the ipumsr data
head(data)
tail(data)
colnames(data)

#need to check the data types of each column
sapply(data, class)
class(data)

#need to know what the food insecuirty scale is, and what each number means
unique(data$FSSTATUS)
unique(data$FSSTATUSA)
unique(data$FSSTATUSC)


#cleaning some of the data
data2 <- data %>%
  mutate(
    STATEFIP = as_factor(STATEFIP),
    SEX = as_factor(SEX),
    FSSTATUS = as.numeric(FSSTATUS),
    FSSTATUSA = as.numeric(FSSTATUSA),
    FSSTATUSC = as.numeric(FSSTATUSC),
    AGE = as.numeric(AGE)
  )
#creating binary results for food insecurity columns and renaming the columns
data2 <- data2 %>%
  filter(
    FSSTATUS <= 3,
    FSSTATUSA <= 4,
    FSSTATUSC <= 3
  ) %>%
  mutate(
    food_insecure = ifelse(FSSTATUS >= 2, 1, 0),
    food_insecure_adult = ifelse(FSSTATUSA >= 3, 1, 0),
    food_insecure_child = ifelse(FSSTATUSC >= 2, 1, 0)
  )

data2 <- data2 %>%
  mutate(
    age_group = case_when(
      AGE < 18 ~ "Children",
      AGE >= 18 ~ "Adult"
    )
  )


#-----------------------BY STATE-----------------------------------------

#the wtfinl column holds person-level weight as in how many people
#the sampled person represents
#we will use the food_insecure column and the person-level weight column
#to find the proportion of people who are food insecure in each state

state_data <- data2 %>%
  group_by(STATEFIP) %>%
  summarize(rate = weighted.mean(food_insecure, WTFINL, na.rm = TRUE))%>%
  mutate(rate = rate *100)


state_data2 <- state_data %>%
  arrange(rate) %>%
  mutate(state = factor(STATEFIP, levels = STATEFIP)) %>%
  mutate(highlight = case_when(
    row_number() <= 5 ~ "Low",
    row_number() > (n() - 5) ~ "High",
    TRUE ~ "Normal"
  ))


label_extremes <- case_when(
  state_data2$highlight == "Low" ~ "steelblue",  
  state_data2$highlight == "High" ~ "#B22222", 
  TRUE ~ "gray40"                            
)

#lollipop chart
ggplot(state_data2, aes(x = rate, y = state)) +
  geom_segment(aes(x = 0, xend = rate, y = state, yend = state), 
               color = "gray80", size = 0.5) +
  geom_point(aes(color = highlight), size = 3) +
  scale_color_manual(values = c("High" = "#B22222", "Low" = "steelblue", "Normal" = "gray80")) +
  scale_x_continuous(labels = label_percent(scale = 1))+
  labs(x = "Food Insecurity Percentage")+
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",            
    axis.title.y = element_blank(), 
    axis.text.y = element_text(color = label_extremes, 
                               face = ifelse(state_data2$highlight == "Normal", "plain", "bold")),
    plot.title = element_text(size = 18, face = "bold", color= "gray20"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 15), color = "gray40"),
    panel.grid.major = element_blank(), 
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15), color = "gray40"),
  ) +
  labs(
    title = "Across Most States More Than 1 in 10 Americans Face Food Insecurity",
    subtitle = "Highlighting states with highest and lowest food insecurity rates",
    x = "Food Insecurity Rate",
    caption = "Source: Integrated Public Use Microdata Series, Current Population Survey"
  )


#--------------------GENDER DATA----------------------------------------

#creating a dataframe for gender data by state
#and finding the weighted average 
gender_state <- data2 %>%
  group_by(STATEFIP, SEX) %>%
  summarize(rate = round(weighted.mean(food_insecure, WTFINL, na.rm = TRUE)*100, 2),
            .groups = "drop")

#pivoting wider so there is a male and female column per state
#also arranging the dataframe by the difference between male and female rates in each state
gender_df <- gender_state %>%
  pivot_wider(names_from = SEX, values_from = rate) %>%
  mutate(diff = abs(Male - Female)) %>%
  arrange(diff) %>%
  mutate(state = factor(STATEFIP, levels = STATEFIP))

#highlighting rates that have a difference greater than 1%
gender_df <- gender_df %>%
  mutate(highlight = case_when(
    diff >= 1 ~ "Large Gap",
    TRUE ~ "Normal"
  ))


#using geom_segment to create a line connecting the male and female rates
ggplot(gender_df, aes(y = state)) +
  geom_segment(aes(x = Male, xend = Female, y = state, yend = state), 
               color = "gray90", linewidth = 1.2) +
  geom_point(aes(x = Male, color = "Male"), size = 3) + 
  geom_point(aes(x = Female, color = "Female"), size = 3) +
  scale_color_manual(name = "Gender", 
                     values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    axis.title.y = element_blank(),
    axis.text.y = element_text(
      face = ifelse(gender_df$highlight == "Large Gap", "bold", "plain"),
      color = ifelse(gender_df$highlight == "Large Gap", "black", "gray50")),
    plot.title = element_text(size = 18, face = "bold", color = "#e74c3c"),
    plot.subtitle = element_text(size = 10, margin = margin(b = 15), color = "gray40"),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15), color = "gray40")
  ) +
  labs(
    title = "Women Experience Higher Food Insecurity Than Men in Most States",
    subtitle = "Gap represents absolute difference between Male and Female rates",
    x = "Food Insecurity Rate (%)",
    caption = "Source: Integrated Public Use Microdata Series, Current Population Survey"
  )





#----------------------------AGE DATA ------------------------------------


age_state <- data2 %>%
  group_by(STATEFIP, age_group) %>%
  summarize(rate = round((weighted.mean(food_insecure, WTFINL, na.rm = TRUE)*100),2),
            .groups = "drop") %>%
  mutate(age_group = factor(age_group, levels = unique(age_group)))


#creating a heatmap of 
ggplot(age_state, aes(x = age_group, y = STATEFIP, fill = rate)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = function(x) paste0(x, "%")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 9, color = "black"), 
    legend.position = "right",
    legend.title = element_text(size = 9),
    plot.title = element_text(size = 15, face = "bold", color = "darkred"),
    plot.subtitle = element_text(size = 9, margin = margin(b = 15), color = "gray30"),
    plot.caption = element_text(size = 9, hjust = 0, margin = margin(t = 15), color = "gray30")
  ) +
  labs(
    title = "Young Adults and Seniors Have The Most Unstable Food Insecurity Rates Across The US",
    subtitle = "Food Insecurity Rates by State and Age Group",
    fill = "Rate (%)",
    caption = "Source: 2023 Integrated Public Use Microdata Series, Current Population Survey"
  )




#-------------------------CHILDREN--------------------------------

#creating a new dataframe beacuse we only want to lookn at the food 
#insecurity rates or young children for now

data3 <- data2 %>%
  filter(
    FSSTATUS <= 3,
    FSSTATUSA <= 4,
    FSSTATUSC <= 3
  ) %>%
  mutate(
    food_insecure = ifelse(FSSTATUS >= 2, 1, 0),
    food_insecure_adult = ifelse(FSSTATUSA >= 3, 1, 0),
    food_insecure_child = ifelse(FSSTATUSC >= 2, 1, 0)
  )

#filtering for children under 3 then grouping by state
child_df <- data3 %>%
  filter(AGE < 3) %>%
  group_by(STATEFIP) %>%
  summarize(
    rate = round(weighted.mean(food_insecure, WTFINL, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(state_label = as.character(as_factor(STATEFIP)))

#started off making a hexagonal tile chart but there were many 
#diffculties with the mapping of states
child_df <- child_df %>%
  mutate(state = case_when(
    state_label == "District of Columbia" ~ "DC",
    TRUE ~ state.abb[match(state_label, state.name)]
  )) %>%
  filter(!is.na(state))



hex_grid <- geofacet::us_state_grid1
hex_map_df <- hex_grid %>%
  left_join(child_df, by = c("code" = "state"))

#making square tiles and not using facet geo was less error prone
ggplot(hex_map_df, aes(x = col, y = -row, fill = rate)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = code), size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#fff5f0", "#fcbba1", "#fb6a4a", "#cb181d", "#67000d")
  )+
  coord_equal() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, hjust= 0.5,face = "bold",  color = "darkred"),
    plot.subtitle = element_text(size = 9, margin = margin(b = 15), color = "gray30"),
    plot.caption = element_text(size = 9, margin = margin(t = 15), color = "gray30")
  ) +
  
  labs(
    title = "Over 10% of Infants and Toddlers Face Food Insecurity In Most States",
    subtitle = "Darker states have higher food insecurity rates",
    fill = "Food Insecurity Rates (%)",
    caption = "Source: 2023 Integrated Public Use Microdata Series, Current Population Survey"
  )

#-----------------------ADULT VS CHILD RATES BY STATE-----------------------


#grouping by state and finding the child food insecurity rates and
#the adult food insecurity rates and their difference
#finding total children for use in the next graph
child_adult <- data2 %>%
  group_by(STATEFIP) %>%
  summarize(
    child_rate = round((weighted.mean(food_insecure_child, WTFINL, na.rm = TRUE) * 100), 2),
    adult_rate = round((weighted.mean(food_insecure_adult, WTFINL, na.rm = TRUE) * 100), 2),
    total_children = sum(WTFINL, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    gap = child_rate - adult_rate
  )


ggplot(child_adult, aes(x = child_rate, y = adult_rate)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
  geom_point(aes(color = gap), size = 3, alpha = 0.7) +
  #top 10% outliers
  geom_text_repel(
    data = filter(child_adult, child_rate > quantile(child_rate, 0.90) | 
                    adult_rate > quantile(adult_rate, 0.90)),
    aes(label = STATEFIP),
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_distiller(palette = "RdBu", direction = -1) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray30", size = 10),
    legend.position = "none" 
  ) +
  labs(
    title = "Higher Child Food Insecurity Leads to Higher Adult Food Insecruity Across States",
    subtitle = "Adult vs Child Food Insecurity: Each point represent a state, only states with the highest rates highlighted \nDashed line represents equal rates and points above the line show higher adult food insecurity.",
    x = "Child Food Insecurity (%)",
    y = "Adult Food Insecurity (%)",
    caption = "Source: 2023 IPUMS CPS Data"
  )


#--------------------POVERTY RATES------------------------------------------
#using tidycensus to get state-level poverty status data
child_poverty <- get_acs(
  geography = "state",
  table = "S1701",
  year = 2023,
  survey = "acs5"
)

#S1701_C03_002 represents population under 18 years
#creating a child poverty rate by state
child_poverty_clean <- child_poverty %>%
  filter(variable == "S1701_C03_002") %>%
  select(NAME, estimate) %>%
  rename(child_poverty_rate = estimate)


unique(child_poverty_clean$NAME)


child_poverty_clean <- child_poverty_clean %>%
  filter(NAME != "Puerto Rico" & NAME != "District of Columbia")

#joining child food insecurity rates and child poverty rates by state
poverty_fss <-left_join(child_adult, 
                        child_poverty_clean, by = c("STATEFIP" = "NAME"))


#checking whether there actually is a correlation
cor(poverty_fss$child_poverty_rate,
    poverty_fss$child_rate,
    use = "complete.obs")

model <- lm(child_rate ~ child_poverty_rate, data = poverty_fss)

r2_value <- summary(model)$r.squared

#creating a bubble plot where each bubble's size is related to the total number
#of children in each state
ggplot(poverty_fss, aes(x = child_poverty_rate, y = child_rate)) +
  geom_point(aes(size = total_children), color = "#2c3e50", alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black")+
  scale_size_continuous(range = c(2, 15), labels = scales::comma) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  #top 10% outliers
  geom_text_repel(
    data = filter(poverty_fss, 
                  child_poverty_rate > quantile(child_poverty_rate, 0.90, na.rm = TRUE) | 
                    child_rate > quantile(child_rate, 0.90, na.rm = TRUE)),
    aes(label = STATEFIP),
    size = 3,
    fontface = "bold", 
    color = "red"
  ) + 
  #adding our r-squared for our model since it indicates that the relationship is weak
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "R² = 0.024, \nPoverty explains 2% \nof variation in food insecurity",
    hjust = 1.1, vjust = 1.5,
    size = 3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
      plot.title = element_text(face = "bold", size = 14, color = "red"),
      plot.subtitle = element_text(color = "gray30"),
      plot.caption = element_text(color = "gray30") 
    
  ) +
  labs(
    title = "State Policy Matters More: Child Poverty Rates Do Not Fully Explain Child Food Insecurity Rates",
    subtitle = "Bubble size reflects total child population in each state; \nDashed line shows direction, not strength of relationship.",
    x = "Child Poverty Rate",
    y = "Child Food  Insecurity Rate",
    size = "Child Population",
    caption = "Source: 2023 IPUMS CPS Data"
  )

#-----------------CHILD POVERTY VS INFANT MORTALITY------------------------

#exploring infant mortality dataframe
head(infant_mortality)
tail(infant_mortality)
colnames(infant_mortality)
sapply(infant_mortality, class)

#filtering infant mortality data to include only 2023
#and dropping year url, deaths
infant_mortality2 <- infant_mortality %>%
  filter(YEAR == 2023) %>%
  select(Location, `Infant mortality rate`, Deaths) 

infant_mortality2 <- infant_mortality2 %>%
  filter(Location != "District of Columbia") %>%
  mutate(
    state_name = state.name[match(Location, state.abb)],
 )


#joining infant mortality rate data to data frame with child fss rate per state
child_infant<- left_join(child_adult, infant_mortality2, by = c("STATEFIP" = "state_name"))
colnames(child_infant)

child_infant <- child_infant %>%
  rename(infant_mortality_rate="Infant mortality rate") 

child_infant$infant_mortality_rate <- as.numeric(child_infant$infant_mortality_rate)

#checking whether there actually is a correlation

cor(child_infant$child_rate,
    child_infant$infant_mortality_rate,
    use = "complete.obs")

model2 <- lm(infant_mortality_rate ~ child_rate, data = child_infant)
summary(model2)

r2_value2 <- summary(model2)$r.squared

#making another bubble plot but this time it's for child fss rates vs infant mortality rates
ggplot(child_infant, aes(
  x = child_rate,
  y = infant_mortality_rate
)) +
  geom_point(aes(size = Deaths), fill = "gray40", color = "white", stroke = 0.5, alpha = 0.5, shape = 21) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#2c3e50", size= 0.8)+
  scale_size_continuous(range = c(2, 15), labels = scales::comma) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  #top 10% outliers
  geom_text_repel(
    data = filter(child_infant, 
                  child_rate > quantile(child_rate, 0.90, na.rm = TRUE) | 
                    infant_mortality_rate > quantile(infant_mortality_rate, 0.90, na.rm = TRUE)),
    aes(label = STATEFIP),
    size = 3,
    fontface = "bold", 
    color = "darkblue"
  ) + 
  #adding our r-squared from out model since it indicates the strength of the relationship 
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "R² = 0.062, \nChild food insecurity explains 6% \nof variation in infant mortality",
    hjust = 1.1, vjust = 1.5,
    size = 3,
    fontface = "bold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "darkblue"),
    plot.subtitle = element_text(color = "gray30"),
    plot.caption = element_text(color = "gray30") 
  ) +
  labs(
    title = "Child Food Insecurity More Closely Aligned To Infant Mortality Rates than Child Poverty Rates",
    subtitle = "Bubble size reflects total infant deaths in each state; \nDashed line shows direction, not strength of relationship.",
    x = "Child Food Insecurity Rate",
    y = "Infant Mortality Rate",
    caption = "Source: 2023 IPUMS CPS Data"
  )






