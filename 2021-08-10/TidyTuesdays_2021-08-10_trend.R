###########
## Setup ##
###########

setwd("~/TidyTuesdays/2021-08-10")
pacman::p_load(readxl, tidyverse, readr, janitor, gridExtra, ggpubr)
rm(list=ls())


#################################
## Investment, chained to 2012 ##
#################################

ci <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv') %>% 
  rename(gross = gross_inv_chain) %>% 
  mutate(gross = gross/1000)  ## Change to billions of chained 2012 dollars


## Total infrastructure investment by type 
df <- ci %>% 
  
  filter(group_num %in% c(4, 17, 22)) %>% 
  
  mutate(category = gsub("in NAICS 515, 517, 518, and 519", "", category),
         category = gsub("communications", "comms", category),
         meta_cat = gsub("Total basic infrastructure", "Basic", meta_cat),
         category = paste0(meta_cat, ": ", category)) %>%
  
  group_by(year) %>% 
  
  # add proportion of the total spent per year
  summarize(meta_cat, category, gross, prop=gross/sum(gross)) %>% 
  
  ungroup()


## Sanity check to ensure total budget matches
cs1 <- df %>% 
  group_by(year) %>% 
  summarize(sum(gross)) %>% 
  deframe()

cs2 <- ci %>% 
  filter(group_num==1) %>% 
  group_by(year) %>% 
  summarize(sum(gross)) %>% 
  deframe()

max(abs(cs1 - cs2))
rm(cs1, cs2)

df %>% group_by(year) %>% summarize(sp=sum(prop)) %>% pull(sp) %>% table()


#########################################
## Trend in infrastructure investiment ##
#########################################

## By infrastructure category ##
p1 <- df %>% 
  group_by(meta_cat, year) %>% 
  summarize(total=sum(gross)) %>% 
  ggplot(aes(x=year, y=total, col=meta_cat)) +
  geom_line() +
  geom_point() +
  labs(x=NULL, y=NULL, col=NULL, title="By infrastructure category") +
  theme_classic() +
  theme(legend.position = "top")

## By source of funding ##
p2 <- ci %>% 
  filter(group_num==2) %>% 
  group_by(category, year) %>% 
  summarize(total=sum(gross)) %>% 
  ggplot(aes(x=year, y=total, col=category)) +
  geom_line() +
  geom_point() +
  labs(x=NULL, y=NULL, col=NULL, title="By funding source") +
  theme_classic() +
  theme(legend.position = "top")


## Proportion allocated to the major categories ##
tmp <- df %>% 
  group_by(category) %>% 
  summarize(gross_peak = max(gross),
            gross_2017 = gross[year==2017],
            d = (gross_2017 - gross_peak)/gross_2017) %>%
  arrange(d, gross_peak) 

p3 <- df %>% 
  mutate(category = factor(category, levels=tmp$category)) %>% 
  
  ggplot(aes(x=year, y=gross, col=category, alpha=0.5)) +
  geom_line(lwd=1) +
  geom_point(size=2) +
  facet_wrap( ~ category, scale="free_y") +
  labs(x=NULL, y=NULL, col=NULL,
       title = "Investment trend for 12 major subcategories",
       caption = "* Panels arranged by ratio of peak to 2017 investment") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text = element_text(size = 8, face=4, hjust = 0, colour = "brown")) 


## By source of funding for Health ##
p4 <- ci %>% 
  filter(group_num==18) %>% 
  mutate(category = recode(category,
                           "S&L other health structures" = "All S&L",
                           "S&L equipment" = "All S&L",
                           "S&L hospitals" = "All S&L")) %>% 
  group_by(category, year) %>% 
  summarize(total=sum(gross)) %>%
  ungroup() %>% 
  ggplot(aes(x=year, y=total, col=category)) +
  geom_line(lwd=1, alpha=0.5) +
  geom_point(size=2) +
  labs(x=NULL, y=NULL, col=NULL, title="Subcategories of Social: Health") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(col=guide_legend(ncol=1))


p5 <- ci %>% 
  filter(group_num == 8) %>% 
  group_by(category, year) %>% 
  summarize(total=sum(gross)) %>%
  ungroup() %>% 
  ggplot(aes(x=year, y=total, col=category)) +
  geom_line(lwd=1, alpha=0.5) +
  geom_point(size=2) +
  labs(x=NULL, y=NULL, col=NULL, title="Funding source for Basic: Power") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(col=guide_legend(ncol=1))

##########
## PLOT ##
##########

mat <- rbind( c(1, 3, 5),
              c(1, 3, 5),
              c(2, 3, 4),
              c(2, 3, 4))


x <- glue::glue("Trends in infrastructure investment (billions of chained 2012 dollars).", 
          "General increase in infrastructure investment over the years, especially private sector funding since 2000.",
          "\n Digital and Health sector increased the most, again due to private sector funding.", 
          "Public safety, conservation & development appears to be experiencing decline in investment growth.",
          .sep = " ")
          
g <- grid.arrange(grobs=list(p1, p2, p3, p4, p5),
             layout_matrix = mat, 
             widths=c(0.25, 0.6, 0.15),
             top = text_grob(str_wrap(x, width = 185), 
                             size=15, face="bold"))

ggsave('2021-08-10_trend.pdf', width = 20, height = 12, device = cairo_pdf, g)

pdftools::pdf_convert(
  '2021-08-10_trend.pdf',
  filenames = '2021-08-10_trend.png',
  format = 'png',
  dpi = 450
)
