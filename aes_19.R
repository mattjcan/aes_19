# PREAMBLE ---------------------------------------------------------------

library(tidyverse)
library(rgdal) 
library(leaflet)
library(knitr)
library(xaringan)
library(rmarkdown)
library(gridExtra)
library(widgetframe)
library(kableExtra)
library(ggthemes)
library(zoo)
library(readxl)
library(lubridate)
library(htmltools)

# PLOT FORMATS ----

background <- c("#e5e5df")

theme_mc <- theme_economist() + 
  theme(legend.position="none") + 
  theme(plot.title = element_text(size = 10, face = "bold")) +
  theme(axis.text = element_text(size = 10, vjust = 0.3, hjust = 0.5)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(axis.line = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(plot.caption = element_text(hjust = 0, size = 9)) +
  theme(plot.background = element_rect(fill = background)) +  
  theme(panel.background = element_rect(fill = background)) +   
  theme(panel.grid.major.y =  element_line(color = "#b3b3b3", size = 0.2))

stroke_size <- 0.75

line_color <- "#2166ac"

# IMPORT ------------------------------------------------------------------

d <- "C:/Users/matt/Dropbox/2a. Election/data/aes_19/" # parent directory for the data

aes <- read_csv(paste0(d,"data/aes19_unrestricted.csv"), skip = 0)

d1_list <- read_csv(paste0(d,"data/d1_list.csv"), skip = 0)


# TIDY -----

aes <- aes %>% 
  mutate(party = ifelse(B1 == 3, "Nationals", ifelse(B1 == 1, "Liberals", "Non-LNP")))


# TRANSFORM ---- 

aes_d1 <- aes %>% 
  select(D1_1, D1_2, D1_3, D1_4, D1_5, D1_6, D1_7, D1_8, D1_9, D1_10, party) %>% 
  gather(key = qn, value = ans, -party)

aes_d1 <- aes_d1 %>%
  filter(ans != 999) %>% 
  count(party, qn, ans) %>%
  group_by(party, qn) %>% 
  mutate(prop = prop.table(n))

aes_d1 <- left_join(aes_d1, d1_list, by = "qn")

aes_d1 <- aes_d1 %>% 
  mutate(ans_text = ifelse(ans == 1, "Extremely important", ifelse(ans == 2, "Quite important", "Not very important")))

aes_d1$party <- factor(aes_d1$party, levels = c("Nationals", "Liberals", "Non-LNP"))

aes_d1$ans_text <- factor(aes_d1$ans_text, levels = c("Extremely important", "Quite important", "Not very important"))

# d1 total 

aes_d1_tot <- aes %>% 
  select(D1_1, D1_2, D1_3, D1_4, D1_5, D1_6, D1_7, D1_8, D1_9, D1_10, party) %>% 
  gather(key = qn, value = ans)

aes_d1_tot <- aes_d1_tot %>%
  filter(ans != 999) %>% 
  count(qn, ans) %>%
  group_by(qn) %>% 
  mutate(prop = prop.table(n))

aes_d1_tot <- left_join(aes_d1_tot, d1_list, by = "qn")

aes_d1_tot <- aes_d1_tot %>% 
  mutate(ans_text = ifelse(ans == 1, "Extremely important", ifelse(ans == 2, "Quite important", "Not very important")))

aes_d1_tot$ans_text <- factor(aes_d1_tot$ans_text, levels = c("Extremely important", "Quite important", "Not very important"))



# PLOT ---- 

# D1_1 

f_aes_d1 <- function(qu) {

aes_d1 %>% 
  filter(qn_text == qu) %>% 
  ggplot(aes(fill = party, x = ans_text, y = prop * 100)) +   
  geom_bar(position = "dodge", stat="identity") +
  theme_mc +
  scale_fill_manual(values = c("Nationals" = "dark green", "Liberals" = "blue", "Non-LNP" = "red")) +
  labs(title = paste0("How important was ",  qu," when deciding your vote?"), subtitle = "%", caption = "Source: ANU, Australian Election Study, 2019", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  theme(legend.position = "bottom", legend.text = element_text(size=9), legend.background = element_rect(fill = background), legend.key = element_rect(fill = background), legend.title = element_blank()) +
  geom_text(aes(label = round(prop * 100,1)), position= position_dodge(0.9), vjust = -0.8, size = 3) +
  ylim(0, 100)
  
}

p_aes_d1 <- map(unique(aes_d1$qn_text), f_aes_d1)

names(p_aes_d1) <- unique(aes_d1$qn_text)

f_aes_d1_pa <- function(pa) {

aes_d1 %>% 
  filter(party == pa & ans_text == "Extremely important") %>% 
  ggplot(aes(x = reorder(qn_text, prop), y = prop * 100)) + 
  geom_bar(stat = "identity", fill = "dark green") +
  coord_flip() +
  theme_mc +
  labs(title = paste("Proportion of", pa, "voters saying issue is extremely important"), subtitle = "%", caption = "Source: ANU, Australian Election Study, 2019", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(prop * 100,1))), vjust = 0.2, hjust = -0.4, size=3) + 
    ylim(0, 100) 
}

p_aes_d1_pa <- map(unique(aes_d1$party), f_aes_d1_pa)

names(p_aes_d1_pa) <- unique(aes_d1$party)






aes_d1_tot %>% 
  filter(ans_text == "Extremely important") %>% 
  ggplot(aes(x = reorder(qn_text, prop), y = prop * 100)) + 
  geom_bar(stat = "identity", fill = line_color) +
  coord_flip() +
  theme_mc +
  labs(title = paste("Proportion of voters saying issue is extremely important"), subtitle = "%", caption = "Source: ANU, Australian Election Study, 2019", x = "", y = "") +
  theme(panel.grid.major = element_blank()) +
  geom_text(aes(label = paste(round(prop * 100,1))), vjust = 0.2, hjust = -0.4, size=3) + 
  ylim(0, 100) 
