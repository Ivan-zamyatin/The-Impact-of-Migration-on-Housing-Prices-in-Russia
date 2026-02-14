install.packages('modelsummary')
install.packages('data.table')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages("xtsum")
install.packages("plm")
install.packages("lmtest")
install.packages("sandwich")
install.packages('readxl')
install.packages('raster')
install.packages('sf')
install.packages("geodata")
install.packages('rmapshaper')
install.packages('terra')
install.packages('gplots')
install.packages('stringdist')
install.packages("plm")
library(geodata)
library(stringr)
library(xtsum)
library(dplyr)
library(modelsummary)
library(stringdist)
library(scales)
library(sf)
library(raster)
library(rmapshaper)
library(readxl)
library(fixest)
library(gplots)
library(sandwich)
library(lmtest)
library(readxl)
library(plm)
library(fixest)
library(xtsum)
library(tidyverse)
library(ggplot2)


#Load dataset with housing prices
ind_vtor_4kv_2021 <- read.csv('ind_vtor_4kv_2021.csv')
ind_vtor_4kv_2022 <- read.csv('ind_vtor_4kv_2022.csv')
ind_vtor_4kv_2023 <- read.csv('ind_vtor_4kv_2023.csv')

ind_perv_4kv_2021 <- read.csv('ind_perv_4kv_2021.csv')
ind_perv_4kv_2022 <- read.csv('ind_perv_4kv_2022.csv')
ind_perv_4kv_2023 <- read.csv('ind_perv_4kv_2023.csv')


#Delete duplicates
ind_vtor_4kv_2021 <- ind_vtor_4kv_2021[!duplicated(ind_vtor_4kv_2021$Регион), ]
ind_vtor_4kv_2022 <- ind_vtor_4kv_2022[!duplicated(ind_vtor_4kv_2022$Регион), ]
ind_vtor_4kv_2023 <- ind_vtor_4kv_2023[!duplicated(ind_vtor_4kv_2023$Регион), ]

ind_perv_4kv_2021 <- ind_perv_4kv_2021[!duplicated(ind_perv_4kv_2021$Регион), ]
ind_perv_4kv_2022 <- ind_perv_4kv_2022[!duplicated(ind_perv_4kv_2022$Регион), ]
ind_perv_4kv_2023 <- ind_perv_4kv_2023[!duplicated(ind_perv_4kv_2023$Регион), ]


#Add constatnt year
vtor_4kv_2021 <- ind_vtor_4kv_2021 %>% mutate(year = 2021)
vtor_4kv_2022 <- ind_vtor_4kv_2022 %>% mutate(year = 2022)
vtor_4kv_2023 <- ind_vtor_4kv_2023 %>% mutate(year = 2023)

perv_4kv_2021 <- ind_perv_4kv_2021 %>% mutate(year = 2021)
perv_4kv_2022 <- ind_perv_4kv_2022 %>% mutate(year = 2022)
perv_4kv_2023 <- ind_perv_4kv_2023 %>% mutate(year = 2023)


#Vertical join in one table
result_vtor <- rbind(vtor_4kv_2021,vtor_4kv_2022,vtor_4kv_2023)

result_perv <- rbind(perv_4kv_2021,perv_4kv_2022,perv_4kv_2023)

#Change NA -> 0
result_vtor[is.na(result_vtor)] <- 0

result_perv[is.na(result_perv)] <- 0

#Rename coloms
colnames(result_vtor)[1] <- "region"
colnames(result_vtor)[2] <- "all_1k"
colnames(result_vtor)[3] <- "low_1k"
colnames(result_vtor)[4] <- "med_1k"
colnames(result_vtor)[5] <- "improve_1k"
colnames(result_vtor)[6] <- "vip_1k"

colnames(result_vtor)[7] <- "all_2k"
colnames(result_vtor)[8] <- "low_2k"
colnames(result_vtor)[9] <- "med_2k"
colnames(result_vtor)[10] <- "improve_2k"
colnames(result_vtor)[11] <- "vip_2k"

colnames(result_vtor)[12] <- "all_4k"
colnames(result_vtor)[13] <- "low_4k"
colnames(result_vtor)[14] <- "med_4k"
colnames(result_vtor)[15] <- "improve_4k"
colnames(result_vtor)[16] <- "vip_4k"

colnames(result_vtor)[17] <- "all_begin"
colnames(result_vtor)[18] <- "low_begin"
colnames(result_vtor)[19] <- "med_begin"
colnames(result_vtor)[20] <- "improve_begin"
colnames(result_vtor)[21] <- "vip_begin"

colnames(result_perv)[1] <- "region"
colnames(result_perv)[2] <- "all_1k"
colnames(result_perv)[3] <- "low_1k"
colnames(result_perv)[4] <- "med_1k"
colnames(result_perv)[5] <- "improve_1k"
colnames(result_perv)[6] <- "vip_1k"

colnames(result_perv)[7] <- "all_2k"
colnames(result_perv)[8] <- "low_2k"
colnames(result_perv)[9] <- "med_2k"
colnames(result_perv)[10] <- "improve_2k"
colnames(result_perv)[11] <- "vip_2k"

colnames(result_perv)[12] <- "all_4k"
colnames(result_perv)[13] <- "low_4k"
colnames(result_perv)[14] <- "med_4k"
colnames(result_perv)[15] <- "improve_4k"
colnames(result_perv)[16] <- "vip_4k"

colnames(result_perv)[17] <- "all_begin"

#Load dataset with migration joint with previous one by using Power Query
data <- read_excel('data_project_econ3.xlsx')

#Descriptive statistics
datasummary((`Price index` =all_begin) + 
              (`Migration` = migr) + 
              (`Migration Russia` = migr_rf) +
              (`Migration CIS` = migr_sng) +
              (`Migration other` = migr_other) +
              (`Migration Higher` = migr_high) +
              (`Migration Secondary special` = migr_start_prof) +
              (`Migration Initial professional` = migr_mid_spec) +
              (`Investment` = investment) + 
              (`Income` = income) ~ N + Mean + SD + Min + Max, data = data, output = 'latex')

#Graphs

# Filter null values
filtered_data <- result_vtor %>% filter(all_4k != 0)

filtered_data <- filtered_data %>%
  mutate(year = factor(year, levels = c("2021", "2022", "2023")))

ggplot(data = filtered_data, aes(x = all_begin, group = year, fill = year)) +
  geom_density(alpha = 0.5) + 
  scale_fill_manual(
    values = c("2021" = "#FF7F50", "2022" = "#EE6A50", "2023" = "#8B3E2F"),
    name = "Year"
  ) +
  labs(
    title = "Density function", 
    x = "Price index since the beginning of the year", 
    y = "Density"
  ) + 
  theme_minimal()

ggplot(data = filtered_data, aes(x = all_4k, group = year, fill = year)) +
  labs(
    title = "Distribution histogram", 
    x = "Price index since the beginning of the year", 
    y = "Frequency"
  ) +
  geom_histogram() + 
  scale_fill_manual(
    values = c("2021" = "#FF7F50", "2022" = "#EE6A50", "2023" = "#8B3E2F"), 
    name = "Year"
  ) +
  theme_minimal()


ggplot(data = filtered_data, aes(x = region, y = all_4k, group = year)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE, color = "#8B3E2F") +  
  labs(title = "Scatter plot with smoothed trend lines", 
       x = "Region", 
       y = "Price index since the beginning of the year") + 
  facet_wrap(~year) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank())

#Count mean value and CI for each region
summary_data_year <- result_vtor %>%
  group_by(year) %>%
  summarise(
    mean_all_4k = mean(all_4k, na.rm = TRUE),
    sd_all_4k = sd(all_4k, na.rm = TRUE),
    n = n(),
    se = sd_all_4k / sqrt(n)
  )

ggplot(summary_data_year, aes(x = factor(year), y = mean_all_4k)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  geom_errorbar(aes(ymin = mean_all_4k - se, ymax = mean_all_4k + se), 
                width = 0.2, color = "#8B3E2F") +
  labs(
    title = "Heterogeneity across years",
    x = "Year",
    y = "Price index since the beginning of the year"
  ) +
  geom_text(aes(label = paste0("n=", n)), vjust = 1.5) +
  theme_minimal()

#Count mean value and CI for each region
summary_data_region <- filtered_data %>%
  group_by(region) %>%
  summarise(
    mean_all_4k = mean(all_4k, na.rm = TRUE),
    sd_all_4k = sd(all_4k, na.rm = TRUE),
    n = n(),
    se = sd_all_4k / sqrt(n)
  )


ggplot(summary_data_region, aes(x = region, y = mean_all_4k)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  geom_errorbar(aes(ymin = mean_all_4k - se, ymax = mean_all_4k + se), 
                width = 0.2, color = "#8B3E2F") +
  labs(
    title = "Heterogeneity across regions",
    x = "Regions", 
    y = "Price index since the beginning of the year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank() 
  )

#Graphs with map
map <- gadm(country = "RUS", level = 1, path = tempdir())

#Long compilation about few seconds
map_geom <- st_as_sf(map) %>%
  st_set_crs("EPSG:4326") %>%
  st_transform("EPSG:20017") %>%
  ms_simplify(keep = 0.01) %>%
  dplyr::select(NL_NAME_1, geometry) %>%
  rename("name" = "NL_NAME_1") %>%
  mutate(name = ifelse(is.na(name), "Москва", name))  

# Bringing the names of regions in the map to a single format
map_geom <- map_geom %>%
  mutate(name = str_trim(name),              
         name = str_to_title(name))          


data21 <- read_excel('map21.xlsx')
colnames(data21)[1] <- "name"  


data21 <- data21 %>%
  mutate(name = str_trim(name),              
         name = str_to_title(name))          

# Using fuzzy matching to find the most similar strings
distances <- stringdist::stringdistmatrix(data21$name, map_geom$name, method = "jaccard")

# Find the smallest distance for each region
closest_match <- apply(distances, 1, which.min)

#Let's update the values in data21 with the most similar regions from map_geometry
data21$name <- map_geom$name[closest_match]

data_map21 <- left_join(map_geom, data21, by = "name")

data_map21$migr[60] <- -310
data_map21$year[60] <- 2021
data_map21$migr_high[60] <- -1003

data22 <- read_excel('map22.xlsx')
colnames(data22)[1] <- "name"  

data22 <- data22 %>%
  mutate(name = str_trim(name),              
         name = str_to_title(name))         

distances <- stringdist::stringdistmatrix(data21$name, map_geom$name, method = "jaccard")

closest_match <- apply(distances, 1, which.min)

data22$name <- map_geom$name[closest_match]

data_map22 <- left_join(map_geom, data22, by = "name")

data_map22$migr[60] <- -10512
data_map22$year[60] <- 2022
data_map22$migr_high[60] <- -1218

data23 <- read_excel('map23.xlsx')
colnames(data23)[1] <- "name"  

data23 <- data23 %>%
  mutate(name = str_trim(name),              
         name = str_to_title(name))          

distances <- stringdist::stringdistmatrix(data21$name, map_geom$name, method = "jaccard")

closest_match <- apply(distances, 1, which.min)

data23$name <- map_geom$name[closest_match]

data_map23 <- left_join(map_geom, data23, by = "name")

data_map23$migr[60] <- -13683
data_map23$year[60] <- 2023
data_map23$migr_high[60] <- -3566

#Plot graphs and setting visualisation
#For 2021
plot <- data_map21 %>%
  ggplot(aes(fill = migr)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill <- plot +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    
    guide = guide_colorbar(
      title.position = "top", 
      title.hjust = 0.5,       
      barwidth = 0.6,        
      barheight = 15  
    )
  )

plot_labs <- plot_fill +
  labs (title = "Migration by region",
        subtitle = "for 2021 year",
        caption = "Sourse:rosstat")


plot_theme <- plot_labs +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )

plot1 <- data_map21 %>%
  ggplot(aes(fill = migr_high)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill1 <- plot1 +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = 0.6,         
      barheight = 15  
    )
  )


plot_labs1 <- plot_fill1 +
  labs (title = "Migration of citizens with higher education",
        subtitle = "for 2021 year",
        caption = "Sourse:rosstat")


plot_theme1 <- plot_labs1 +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )

#For 2022

plot2 <- data_map22 %>%
  ggplot(aes(fill = migr)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill2 <- plot2 +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = 0.6,         
      barheight = 15  
    )
  )

plot_labs2 <- plot_fill2 +
  labs (title = "Migration by region",
        subtitle = "for 2022 year",
        caption = "Sourse:rosstat")


plot_theme2 <- plot_labs2 +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )

plot3 <- data_map22 %>%
  ggplot(aes(fill = migr_high)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill3 <- plot3 +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = 0.6,         
      barheight = 15  
    )
  )


plot_labs3 <- plot_fill3 +
  labs (title = "Migration of citizens with higher education",
        subtitle = "for 2022 year",
        caption = "Sourse:rosstat")


plot_theme3 <- plot_labs3 +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )

#For 2023

plot4 <- data_map23 %>%
  ggplot(aes(fill = migr)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill4 <- plot4 +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = 0.6,         
      barheight = 15  
    )
  )

plot_labs4 <- plot_fill4 +
  labs (title = "Migration by region",
        subtitle = "for 2023 year",
        caption = "Sourse:rosstat")


plot_theme4 <- plot_labs4 +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )

plot5 <- data_map23 %>%
  ggplot(aes(fill = migr_high)) +
  geom_sf(color = "white",
          alpha = 0.8,
          lwd = 0) +
  theme_void()


plot_fill5 <- plot5 +
  scale_fill_continuous(
    low = "#F7EDE2",
    high = "#EE6352",
    name = NULL,
    guide = guide_colorbar(
      title.position = "top",  
      title.hjust = 0.5,       
      barwidth = 0.6,         
      barheight = 15  
    )
  )


plot_labs5 <- plot_fill5 +
  labs (title = "Migration of citizens with higher education",
        subtitle = "for 2023 year",
        caption = "Sourse:rosstat")


plot_theme5 <- plot_labs5 +
  theme(
    text = element_text(colour = "grey40",
                        size = 14),
    legend.position = "right",
    legend.justification = "right",
    legend.direction = "vertical"
  )


plot_theme
plot_theme1
plot_theme2
plot_theme3
plot_theme4
plot_theme5


#Regression analysis
library(plm)
library(fixest)
library(xtsum)
library(dplyr)
library(modelsummary)
library(gplots)
data <- read_excel('data_project_econ3.xlsx')
data <- data %>%
  mutate(migr_lag = lag(migr, n = 1),
         migr = migr/1000,
         migr_high = migr_high/1000,
         migr_mid_spec = migr_mid_spec/1000,
         migr_start_prof = migr_start_prof/1000,
         migr_start_gen = migr_start_gen/1000, 
         migr_rf = migr_rf/1000,
         migr_sng = migr_sng/1000,
         migr_nf_citizenship = migr_nf_citizenship/1000,
         migr_other = migr_other/1000,
         income = income/1000,
         investment = investment/1000)

pan_data <- as.data.frame(data)
pan_data <- panel(data, ~ region + year)

pdim(pan_data)
is.pbalanced(pan_data)

#Base model classification 
ols0 <- feols(all_begin ~ -1 + migr + year + income , data = pan_data, vcov = 'hetero')

ols1 <- feols(all_begin ~ -1 + year|migr ~ investment, data = pan_data, vcov = 'hetero')


ols2 <- feols(all_begin ~ -1 + year + income|migr ~ investment, data = pan_data, vcov = 'hetero')

ols3 <- feols(all_begin ~  -1 + year + income |migr ~ investment + migr_lag, data = pan_data, vcov = 'hetero')


list_ols <- list(ols0, ols1, ols2)
etable(list_ols, stage = 1:2, fitstat = ~ . + ivfall + ivwaldall.p, digits = 3, digits.stats = 3, tex = TRUE)

#Models varying level of education.
ols4 <- feols(all_begin ~  -1 + year + income |migr_high ~ investment, data = pan_data, vcov = 'hetero')

ols5 <- feols(all_begin ~  -1 + year + income|migr_mid_spec ~ investment, data = pan_data, vcov = 'hetero')

ols6 <- feols(all_begin ~  -1 + year + income|migr_start_prof  ~ investment, data = pan_data, vcov = 'hetero')

ols7 <- feols(all_begin ~  -1 + year + income|migr_start_gen ~ investment, data = pan_data, vcov = 'hetero')

list_ols2 <- list(ols4, ols5, ols6)
etable(list_ols2, digits = 3, stage = 1:2, fitstat = ~ . + ivfall + ivwaldall.p, digits.stats = 3, tex = TRUE)

#Models varying type of citizenship.
ols8 <- feols(all_begin ~  -1 + year + income|migr_rf ~ investment, data = pan_data, vcov = 'hetero')

ols9 <- feols(all_begin ~  -1 + year + income|migr_sng ~ investment, data = pan_data, vcov = 'hetero')

ols10 <- feols(all_begin ~  -1 + year + income|migr_other  ~ investment, data = pan_data, vcov = 'hetero')

ols11 <- feols(all_begin ~  -1 + year + income|migr_nf_citizenship ~ investment, data = pan_data, vcov = 'hetero')

list_ols3 <- list(ols8, ols9, ols10)
etable(list_ols3, digits = 3, stage = 1:2, fitstat = ~ . + ivfall+ ivwaldall.p, digits.stats = 3, tex = TRUE)

#Model for different types of housing
ols12 <- feols(low_4k ~  -1 + year + income|migr_rf ~ investment, data = pan_data, vcov = 'hetero')

ols13 <- feols(low_4k ~  -1 + year + income|migr_high ~ investment, data = pan_data, vcov = 'hetero')

ols14 <- feols(med_4k ~  -1 + year + income|migr_rf ~ investment, data = pan_data, vcov = 'hetero')

ols15 <- feols(med_4k ~  -1 + year + income|migr_high ~ investment, data = pan_data, vcov = 'hetero')

ols16 <- feols(improve_4k ~  -1 + year + income|migr_rf ~ investment, data = pan_data, vcov = 'hetero')

ols17 <- feols(improve_4k ~  -1 + year + income|migr_high ~ investment, data = pan_data, vcov = 'hetero')

ols18 <- feols(vip_4k ~  -1 + year + income|migr_rf ~ investment, data = pan_data, vcov = 'hetero')

ols19 <- feols(vip_4k ~  -1 + year + income|migr_high ~ investment, data = pan_data, vcov = 'hetero')

list_ols4 <- list(ols12, ols13, ols14, ols15, ols16, ols17, ols18, ols19)
etable(list_ols4, digits = 3, digits.stats = 3, stage = 1:2, fitstat = ~ . + ivfall, tex = TRUE)


#Appendix

#Additional regression analisys with random effects
data[is.na(data)] <- 0

pdata <- pdata.frame(data, index = c("region",'year'))

model_fe <- plm(all_begin ~ income + migr + investment + migr_lag, data = pdata, model = "within")
summary(model_fe)

model_re <- plm(all_begin ~ income + migr + investment + migr_lag, data = pdata, model = "random")
summary(model_re)

model_iv <- plm(
  all_begin ~ migr + investment, 
  data = pdata,
  model = "random",                     
  inst.form = migr ~ income + migr_lag 
)

#Results and model classification test
summary(model_iv)

coeftest(model_re, vcovHC)
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(model_re, type = x)))))

hausman_test <- phtest(model_fe, model_re)
print(hausman_test)
#p>0.05 H0 is not rejected (the hypothesis of the absence of endogeneity is not rejected), 
#that is, the estimates of the two models are asymptotically equivalent

#Adding regional effect in migration
dataa <- read_excel('panp.xlsx')

pan <- dataa %>%
  mutate(migr_lag = lag(migr, n = 1),
         migr = migr/1000,
         migr_high = migr_high/1000,
         migr_mid_spec = migr_mid_spec/1000,
         migr_start_prof = migr_start_prof/1000,
         migr_start_gen = migr_start_gen/1000, 
         migr_rf = migr_rf/1000,
         migr_sng = migr_sng/1000,
         all_begin = as.numeric(all_begin),
         migr_nf_citizenship = migr_nf_citizenship/1000,
         migr_other = migr_other/1000,
         income = as.numeric(income)/1000,
         av_pop = av_pop/1000,
         investment = as.numeric(investment)/1000,
         low_4k = as.numeric(low_4k),
         med_4k = as.numeric(med_4k),
         improve_4k = as.numeric(improve_4k),
         vip_4k = as.numeric(vip_4k)
         )
pan <- pan %>%
  mutate(
         migr_high = migr_high/av_pop,
         migr_mid_spec = migr_mid_spec/av_pop,
         migr_start_prof = migr_start_prof/av_pop,
         migr_start_gen = migr_start_gen/av_pop, 
         migr_rf = migr_rf/av_pop,
         migr_sng = migr_sng/av_pop
         )

pan <- transform(pan, new =  as.numeric(migr/av_pop))

panda <- panel(pan, ~ region + year)

#Base model classification
ols20 <- feols(all_begin ~ -1 + new + year + income , data = panda, vcov = 'hetero')

ols21 <- feols(all_begin ~ -1 + year|new ~ investment, data = panda, vcov = 'hetero')


ols22 <- feols(all_begin ~ -1 + year + income|new ~ investment, data = panda, vcov = 'hetero')

ols23 <- feols(all_begin ~  -1 + year + income |new ~ investment + migr_lag, data = panda, vcov = 'hetero')


list_ols <- list(ols20, ols21, ols22)
etable(list_ols, stage = 1:2, fitstat = ~ . + ivfall + ivwaldall.p, digits = 3, digits.stats = 3, tex = TRUE)


#Models varying level of education.
ols24 <- feols(all_begin ~  -1 + year + income |migr_high ~ investment, data = panda, vcov = 'hetero')

ols25 <- feols(all_begin ~  -1 + year + income|migr_mid_spec ~ investment, data = panda, vcov = 'hetero')

ols26 <- feols(all_begin ~  -1 + year + income|migr_start_prof  ~ investment, data = panda, vcov = 'hetero')

ols27 <- feols(all_begin ~  -1 + year + income|migr_start_gen ~ investment, data = panda, vcov = 'hetero')

list_ols2 <- list(ols24, ols25, ols26)
etable(list_ols2, digits = 3, stage = 1:2, fitstat = ~ . + ivfall + ivwaldall.p, digits.stats = 3, tex = TRUE)



#Models varying type of citizenship.
ols28 <- feols(all_begin ~  -1 + year + income|migr_rf ~ investment, data = panda, vcov = 'hetero')

ols29 <- feols(all_begin ~  -1 + year + income|migr_sng ~ investment, data = panda, vcov = 'hetero')

ols30 <- feols(all_begin ~  -1 + year + income|migr_other  ~ investment, data = panda, vcov = 'hetero')

ols31 <- feols(all_begin ~  -1 + year + income|migr_nf_citizenship ~ investment, data = panda, vcov = 'hetero')

list_ols3 <- list(ols28, ols29, ols30)
etable(list_ols3, digits = 3, stage = 1:2, fitstat = ~ . + ivfall+ ivwaldall.p, digits.stats = 3, tex = TRUE)

#Model for different types of housing
ols32 <- feols(low_4k ~  -1 + year + income|migr_sng ~ investment, data = panda, vcov = 'hetero')

ols33 <- feols(low_4k ~  -1 + year + income|migr_start_prof ~ investment, data = panda, vcov = 'hetero')

ols34 <- feols(med_4k ~  -1 + year + income|migr_sng ~ investment, data = panda, vcov = 'hetero')

ols35 <- feols(med_4k ~  -1 + year + income|migr_start_prof ~ investment, data = panda, vcov = 'hetero')

ols36 <- feols(improve_4k ~  -1 + year + income|migr_sng ~ investment, data = panda, vcov = 'hetero')

ols37 <- feols(improve_4k ~  -1 + year + income|migr_start_prof ~ investment, data = panda, vcov = 'hetero')

ols38 <- feols(vip_4k ~  -1 + year + income|migr_sng ~ investment, data = panda, vcov = 'hetero')

ols39 <- feols(vip_4k ~  -1 + year + income|migr_start_prof ~ investment, data = panda, vcov = 'hetero')


list_ols4 <- list(ols32, ols33, ols34, ols35, ols36, ols37, ols38, ols39)
etable(list_ols4, digits = 3, digits.stats = 3, stage = 1:2, fitstat = ~ . + ivfall, tex = TRUE)