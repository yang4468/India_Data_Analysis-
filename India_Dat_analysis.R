source("clean_india_data.R")   # Run code that creates india04.Rdata
load("india02.Rdata")
library(tidyverse)
install.packages("stargazer")
library("stargazer")
library(modelr)
options(na.action = na.warn)

#manipulations
india02 <- india02 %>%
  filter(edattain != "Unknown") %>%
  filter(incwage != "NA")

view(india02)

#dummy 
india02 <-  india02 %>%
  mutate(female_dummy = ifelse(sex == "Female", 1, 0)) %>%
  mutate(urban_dummy = ifelse(urban == "Urban" , 1, 0))
india02 <- india02 %>%
  mutate(lessprimary_dummy = ifelse(edattain == "<primary", 1, 0))
summarise(india02, age)
attach(india02)

summary(india04)
view(india04)

india02 <- india02 %>%
  select(age, incwage, female_dummy, urban_dummy, lessprimary_dummy, perwt, edattain, geo1_in1999) 


india01 <- filter(india01, age <= 65, age >= 22)

view(india02)

stargazer(as.data.frame(india01), type = "text", summary = FALSE)
view(india01)

lessprimary_dummy
female_dummy
lessprimary_dummy
urban_dummy

row.names(geo1_in1999)
rownames(geo1_in1999)
india02 %>% count(geo1_in1999)

india02 <- summarise(india02, incwage_avg = weighted.mean(incwage, w = perwt, na.rm = TRUE))
summary(india04)
attach(india02)

ggplot(india02)

geo1
#Regressions
lm_a <- lm(log(incwage) ~ edattain, data = india02, weights = perwt)
lm_b <- lm(log(incwage) ~ female_dummy, data = india02, weights = perwt)
lm_c <- lm(log(incwage) ~ urban_dummy, data = india02, weights = perwt)

lm_b1 <- lm(log(incwage) ~ female_dummy + edattain,  data = india02, weights = perwt)
lm_c1 <- lm(log(incwage) ~ urban_dummy + edattain, data = india02, weights = perwt)

lm_d <- lm(log(incwage) ~ female_dummy + urban_dummy + edattain #add age #add location #Age*edu, data = india01, weights = perwt)
           
           lm_e <- lm(log(incwage) ~ geo1_in1999, data = india02, weights = perwt)           
           
           #regressions
           stargazer(lm_a, align = TRUE, title="Regression results on Educational Attainment",type = "text", out = "edat2.htm")
           stargazer(lm_b, align = TRUE, title="Regression results on Sex",type = "text", out = "sex2.htm")
           stargazer(lm_c, align = TRUE, title="Regression results on Location",type = "text", out = "loc2.htm")
           stargazer(lm_b1, align = TRUE, title="Regression results on Educational Attainment and Sex",type = "text", out = "sex3.htm")
           stargazer(lm_c1, align = TRUE, title="Regression results on Educational Attainment and Location",type = "text", out = "loc3.htm")
           
           stargazer(lm_d, align = TRUE, title="Regression results on Sex, Educational Attainment, and Location",type = "text", out = "d3.htm")
           stargazer(lm_e, align = TRUE, title="Regression results on Location",type = "text", out = "state.htm")
           
           
           stargazer(lm_a, lm_b, lm_c, lm_b1, lm_c1,lm_d, title = "Results", align = TRUE, type = "text", out = "Dec4.htm", column.labels=c("EdAttain","Sex", "Location", "EdAttain and Sex", "EdAttain and Loc", "EdAttain, Loc, and Sex"))
           rownames(geo1_in1999)
           stargazer(india04, type = "text")
           summarise(india02)
           colnames(geo1_in1999)
           #Residuals
           aba <- india04 %>%
             add_residuals(lm_a)
           aba
           
           #Rural vs Urban Wages per year average by edattain
           india01_3 <- india01 %>%
             mutate(incwage_adj = ifelse(incwage > 4500, NA, incwage )) 
           
           
           ggplot(data = india02, mapping = aes(x = geom1_in1999, y = incwage)) +
             geom_boxplot()
           
           attach(india02)
           ggplot(india02) +
             geom_boxplot(mapping = aes(x= "edattain", y = "incwage"))
           
           india01_sec5 %>% ggplot(mapping = aes(x = edattain, y = avg_indwage_year)) +
             geom_bar(stat = "identity") +
             facet_wrap(~urban)
           india04_sec5 <- mutate(india04, indwage_year = (incwage/4)*52) %>%
             group_by(occisco, urban) %>%
             summarise(avg_indwage_year = weighted.mean(indwage_year, w= perwt, na.rm = TRUE)) %>%
             arrange(desc(avg_indwage_year)) %>%
             filter(occisco != "Unknown")
           view(india04_sec5)
           india04_sec5
           
           count(india02, edattain)
           count(india02, urban)
           #unusual values
           
           ggplot(data = india04, mapping = aes(x = urban)) +
             geom_bar()
           
           india04_2 <- india04 %>% 
             mutate(incwage_adj = ifelse(incwage > 10000, NA, incwage))
           
           ggplot(data = smaller, mapping = aes(x = incwage, colour = edattain)) +
             geom_freqpoly(binwidth = 100)
           
           ggplot(data = india04_2, mapping = aes(x = incwage_adj, y = ..density..)) +
             geom_freqpoly(mapping = aes(colour = edattain), binwidth = 500)
           
           ggplot(data = india04_2) +
             geom_count(mapping = aes(x = sex, y = edattain))
           
           count(india02, geo1_in1999)
           names(geo1_in1999)
           colnames(india02, geo1_in1999)
           