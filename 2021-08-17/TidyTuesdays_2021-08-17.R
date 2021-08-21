###########
## Setup ##
###########

setwd("~/TidyTuesdays/2021-08-17")
pacman::p_load(tidyverse, janitor, tidytext, pheatmap, ggplotify)
rm(list=ls())

##################
## Read in data ##
##################

responses <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


#############################
## Cleanup character names ##
#############################

responses <- responses %>%
  mutate( char2 = ifelse(grepl("Com", char), "Computer", char),
          char2 = gsub("Young ", "", char2),
          char2 = gsub("Mrs. Troi", "Lwaxana", char2),
          char2 = gsub(" .*", "", char2))

## Look into character's responses
char_responses <- responses %>% 
  filter(char2!="Computer") %>% 
  mutate(char3 = fct_lump_min(char2, min=10, other_level="Others"),
         char3 = fct_infreq(char3),
         char3 = fct_relevel(char3, "Others", after = Inf))

char_responses %>% count(char3)

char_responses %>% 
  summarize(n(), n_distinct(char2), n_distinct(char3))

g_interact <- ggplot(char_responses, aes(y=fct_rev(char3), fill=char3)) + 
  geom_bar() +
  geom_text(aes(label = ..count..), 
            size = 5, stat = "count", hjust = 1.1, colour = "black") +
  labs(x=NULL, y=NULL, 
       title = "Who interacts with the computer most?",
       subtitle = "13 of the 49 characters listed in the database contribute 92% of the interactions" ) +
  theme_minimal() +
  theme(legend.position="none", 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        plot.title  = element_text(size=20, face="bold", colour="royalblue"),
        plot.subtitle = element_text(size=16))

g_interact
# The 13 most talkative characters (out of 49) account for 92% (1-117/1506) of the conversations


######################################
## Cleanup primary interaction type ##
######################################

char_responses %>% count(pri_type, sort=T)

char_responses <- char_responses %>% 
  mutate(pri_type2 = fct_lump_min(pri_type, min=10),
         pri_type2 = fct_infreq(pri_type2),
         pri_type2 = fct_relevel(pri_type2, "Other", after = Inf))

char_responses %>% count(pri_type2, sort=T)


######################
## Cross-tabulation ##
######################

char_responses %>% 
  tabyl(char3, pri_type2) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>% 
  adorn_pct_formatting(digits=0) %>% 
  adorn_ns(position = "front") %>% 
  knitr::kable()

# |char3    |Command   |Statement |Question  |Other   |Total       |
# |:--------|:---------|:---------|:---------|:-------|:-----------|
# |Geordi   |185 (57%) |39 (12%)  |96 (30%)  |4 (1%)  |324 (100%)  |
# |Picard   |178 (64%) |53 (19%)  |42 (15%)  |4 (1%)  |277 (100%)  |
# |Data     |173 (73%) |24 (10%)  |38 (16%)  |3 (1%)  |238 (100%)  |
# |Riker    |89 (59%)  |49 (32%)  |5  (3%)   |8 (5%)  |151 (100%)  |
# |Beverly  |54 (45%)  |32 (26%)  |35 (29%)  |0 (0%)  |121 (100%)  |
# |Troi     |56 (74%)  |15 (20%)  |3  (4%)   |2 (3%)  |76 (100%)   |
# |Worf     |48 (71%)  |9 (13%)   |10 (15%)  |1 (1%)  |68 (100%)   |
# |Barclay  |31 (76%)  |7 (17%)   |2  (5%)   |1 (2%)  |41 (100%)   |
# |Wesley   |24 (80%)  |6 (20%)   |0  (0%)   |0 (0%)  |30 (100%)   |
# |K'Ehleyr |11 (55%)  |3 (15%)   |6 (30%)   |0 (0%)  |20 (100%)   |
# |Moriarty |9 (47%)   |9 (47%)   |1  (5%)   |0 (0%)  |19 (100%)   |
# |Krag     |11 (92%)  |0  (0%)   |0  (0%)   |1 (8%)  |12 (100%)   |
# |Lwaxana  |2 (17%)   |6 (50%)   |4 (33%)   |0 (0%)  |12 (100%)   |
# |Others   |71 (61%)  |35 (30%)  |9  (8%)   |2 (2%)  |117 (100%)  |
# |Total    |942 (63%) |287 (19%) |251 (17%) |26 (2%) |1506 (100%) |


## Find the average response pattern
type_means <- char_responses %>% 
  tabyl(pri_type2) %>% 
  select(type=pri_type2, percent) %>% 
  mutate(percent = 100*percent) 

## Heatmap version
mdf <- char_responses %>% 
  tabyl(char3, pri_type2) %>% 
  adorn_totals("col") %>% 
  mutate(char3 = paste0(char3, " (n=", Total, ")")) %>% 
  adorn_percentages() %>% 
  select(-Total) 

mat <- mdf %>% 
  column_to_rownames("char3") %>% 
  as.matrix()

identical(colnames(mat), as.character(type_means$type))

mat_excess <- sweep(100*mat, 2, type_means$percent, FUN="-")

ph <- pheatmap(mat_excess,
               cluster_cols = FALSE,
               display_numbers = round(100*mat),
               number_color = "black",
               fontsize = 15,
               angle_col = "0",
               legend = FALSE)

tmp <- "Table below shows the percentage of each character's conversation that can be allocated to the primary interaction types. 

Three user groups emerge: 1) Give command instructions (Krag, Wesley, Troi, Barclay, Picard, Data, Worf); 2) Seek answers to questions (Beverly, Geordi, K'Ehleyr) and 3) Make statements (Moriarty, Riker). Lwaxana aka Mrs. Troi shows a mixed pattern of interaction."

ph2 <- as.ggplot(ph) + 
  labs(title = "How do they interact with the computers?", 
       subtitle=str_wrap(tmp, width=100)) +
  theme(plot.title=element_text(size=20, face="bold", colour="royalblue"),
        plot.subtitle = element_text(size=16))

g <- grid.arrange(grobs=list(g_interact, ph2), nrow=1)

ggsave('2021-08-17.pdf', width = 20, height = 12, device = cairo_pdf, g)

pdftools::pdf_convert(
  '2021-08-17.pdf',
  filenames = '2021-08-17.png',
  format = 'png',
  dpi = 450
)

################################
## Dotplot version - not used ##
################################

mdf <- mdf %>% 
  select(-Other) %>% 
  pivot_longer(cols = -1, names_to = "type", values_to = "percent") %>% 
  mutate(percent = 100*percent)

type_means <- type_means %>% 
  filter(type != "Other")

mdf %>% 
  ggplot(aes(x = percent, 
             y = reorder_within(char3, percent, type),
             col = type)) +
  geom_point(size = 5) +
  facet_wrap( ~ type, scale="free") + 
  scale_y_reordered() +
  geom_vline(data=type_means, aes(xintercept=percent)) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none") +
  labs(x=NULL, y=NULL, 
       title = "The type of primary interaction with the computer",
       caption = "The vertical line represents the average percentage for each primary interaction type")

ggsave('2021-08-17_alt.pdf', width = 20, height = 12, device = cairo_pdf, g)

pdftools::pdf_convert(
  '2021-08-17_alt.pdf',
  filenames = '2021-08-17_alt.png',
  format = 'png',
  dpi = 450
)


###################################
## Computer responses - not used ##
###################################

computer_responses <- responses %>% 
  filter(char2=="Computer") %>% 
  mutate(pri_type2 = fct_infreq(pri_type)) 

g <- ggplot(computer_responses, aes(y=fct_rev(pri_type2), fill=pri_type2)) +
  geom_bar() +
  geom_text(aes(label = ..count..), 
          size = 5, stat = "count", hjust = 1, colour = "black") +
  labs(x=NULL, y=NULL, 
       title = "Computer conversation types") +
  theme_minimal() +
  theme(legend.position="none", 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        plot.title  = element_text(size=20, face="bold", colour="royalblue"),
        plot.subtitle = element_text(size=16))

ggsave('2021-08-17_computer_response.pdf', width = 20, height = 12, device = cairo_pdf, g)

pdftools::pdf_convert(
  '2021-08-17_computer_response.pdf',
  filenames = '2021-08-17_computer_response.png',
  format = 'png',
  dpi = 450
)
