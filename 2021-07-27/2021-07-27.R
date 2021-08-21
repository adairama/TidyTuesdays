#########################
## Setup and load data ##
#########################

setwd("c:/Users/aramasamy/Documents/TidyTuesdaysProject/Week31/")

pacman::p_load(tidyverse, janitor, broom, glue, ggpubr,
               gridExtra, ggrepel, tidytuesdayR)
rm(list=ls())

tmp <- tt_load(2021, week=31)

raw <- tmp$olympics %>%
  mutate(sex       = recode(sex, "M"="males", "F"="females"),
         event = recode(event, "Luge Mixed (Men)'s Doubles"="Luge Men's Doubles"),
         event_sex = str_extract(event, 'Men|Women|Mixed'))

range(raw$year) # 1896 to 2016

region <- tmp$regions %>%
  clean_names() %>%
  mutate(noc = gsub("SIN", "SGP", noc))

rm(tmp)


#############################
## Sex informative summary ##
#############################

## summary by year, season and sex (combine all countries)
summ_pa <- raw %>%
  group_by(year, season, sex) %>%
  summarize(n = n())

## summary by year, season, sex and noc
score_medals <- function(x){
  
  sum.na <- function(...) sum(..., na.rm=T)
  
  total <- 3*sum.na(x=="Gold") + 2*sum.na(x=="Silver") + 1*sum.na(x=="Bronze")
  return(total)
}

summ_pa_noc <- raw %>%
  group_by(year, season, noc, sex) %>%
  summarize(n     = n(),
            mrank = score_medals(medal))

save(raw, region, summ_pa, summ_pa_noc, file="Week31.rda", compress=T)


###############################
## Gender gap over the years ##
###############################

rm(list=ls())
load("Week31.rda", verbose=TRUE)


## Number of athletes participating ##
g1 <- summ_pa %>% 
  mutate(season = paste("# athletes in", season, "Olympics")) %>% 
  ggplot(aes(x=year, y=n, col=sex)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x=NULL, y=NULL, col=NULL) +
  facet_wrap(~ season, scale="free") +
  theme(legend.position="bottom",
        strip.text=element_text(size=12, face="bold")) +
  scale_color_manual(values=c("females"="red", "males"="blue"))


## Participation rate of female athletes
tmp <- summ_pa %>% 
  pivot_wider(names_from = sex, values_from = n, values_fill = 0) %>% 
  mutate(perc_females = 100*females/(males + females))

g2 <- ggplot(tmp, aes(x=year, y=perc_females, pch=season)) +
  geom_point() +
  geom_line(aes(lty=season)) +
  labs(x=NULL, y=NULL, subtitle="Percentage of female athletes") +
  ylim(0, 50) +
  geom_hline(yintercept=50, lwd=1, lty=2) +
  theme_minimal() +
  theme(legend.position="bottom",
        plot.subtitle=element_text(hjust=0.5, size=12, face="bold"))

rm(tmp)


##############################################################
## Female participation and ranking in Summer Olympics 2016 ##
##############################################################

Summer2016 <- summ_pa_noc %>% 
  filter(year==2016, season=="Summer") %>% 
  ungroup() %>% 
  select(-year, -season) %>% 
  complete(noc, sex, fill=list(n=0, mrank=0)) %>% 
  mutate(sex = recode(sex, "F"="females", "M"="males"))

length(unique(Summer2016$noc)) # 207 countries

## Only countries with > 10 athletes, Add region.
keep_min10 <- Summer2016 %>% 
  group_by(noc) %>% 
  summarize( total = sum(n) ) %>% 
  filter(total > 10) %>% 
  pull(noc)

Summer2016 <- Summer2016 %>% 
  filter(noc %in% keep_min10) %>% 
  left_join(region) 

length(unique(Summer2016$noc)) # 112 countries

## Check region duplicity
tb <- table(Summer2016$region)
Summer2016 %>% filter(region %in% names(which(tb > 2))) # Hong Kong classified as China

toFix <- which(Summer2016$noc=="HKG")
Summer2016[ toFix, "region" ] <- "Hong Kong, China"


## Countries with highest and lowest female athlete participation
female_rates <- Summer2016 %>% 
  group_by(noc, region) %>% 
  summarize(n_athletes = sum(n),
            perc_female = 100*n[sex=="females"]/n_athletes) %>% 
  arrange(perc_female) %>% 
  ungroup()


## Participation rates
n <- 15
mdf <- rbind( head(female_rates, n), tail(female_rates, n) ) %>% 
  mutate(region  = factor(region, levels=region),
         participation = ifelse(perc_female > 50, "High", "Low"))

g3 <- ggplot(mdf, aes(x=perc_female, y=region, col=participation)) +
  geom_segment(aes(x=50, y=region, xend=perc_female, yend=region)) +
  geom_point(aes(size=n_athletes)) +
  labs(x=str_wrap("Percentage of female athletes"), y=NULL,
       title    = "Female athlete participation rates in 2016 Summer Olymptics",
       subtitle = paste(n, "countries with highest and lowest percentage *"),
       caption  = "* Only countries with > 10 athletes included",
       col      = "Female athlete\nparticipation level",
       size     = "Number of athletes") +
  theme_minimal() +
  theme(plot.title = element_text(face="bold"),
        axis.text.y = element_text(size=12))

## Medal ranking vs number of athletes females
min_150_athletes <- Summer2016 %>% 
  filter(n > 150 | mrank > 30)

g4 <- ggplot(Summer2016, aes(x=n, y=mrank, col=sex)) +
  geom_point() +
  geom_smooth() +
  geom_text_repel(data=min_150_athletes, aes(label=region), max.overlaps=100) +
  theme_minimal() +
  labs(x = "Number of athletes participating",
       y = NULL,
       col = NULL,
       title = "Sum of weighted medal scores* in 2016 Summer Olympic",
       subtitle = str_wrap("Female athletes tend to score more medal points per participant than male athletes.
                           Some countries (e.g USA, Russia, UK) win more medal points per athlete while some (e.g. Brazil, Australia, Canada, Japan) win less medal points per participant.", width=125),
       caption = "* Weights: Gold = 3 points, Silver = 2 points, Bronze = 1 point") +
  theme(plot.title = element_text(face="bold"), legend.position = "right") +
  scale_color_manual(values=c("females"="red", "males"="blue"))

mat <- rbind( c(1, 1, 1, 2),
              c(3, 3, 4, 4),
              c(3, 3, 4, 4))


x <- glue("More atheletes are participating in the Olympics over the years.",
          "The rate of increase is greater with female athletes, especially in Summer Olympics,",
          "helping to close the gender gap in participation.", .sep=" ")

g <- grid.arrange(grobs=list(g1, g2, g3, g4),
                  layout_matrix = mat,
                  top = text_grob(x, size=15, face="bold"))


ggsave('2021-07-27.pdf', width = 20, height = 12, device = cairo_pdf, g)

pdftools::pdf_convert(
  '2021-07-27.pdf',
  filenames = '2021-07-27.png',
  format = 'png',
  dpi = 450
)