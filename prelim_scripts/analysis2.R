library(tidyverse)
library(fs)
View(d)
d <- map_df(dir_ls("data"), read_csv, .id = "content_area") %>% 
  janitor::clean_names() %>% 
  mutate(content_area = str_extract(content_area, "RL|MA"),
         gndr = tolower(gndr)) %>% 
  select(rptchkdigitstdntid,
         attnddistinstid,
         attndschlinstid,
         content_area, 
         gndr, 
         ethniccd,
         enrlgrd,
         pl5b_tot,
         rit_tot) %>% 
  rename(ssid   = rptchkdigitstdntid,
         distid = attnddistinstid,
         scid   = attndschlinstid,
         plc    = pl5b_tot,
         rit    = rit_tot)

library(esvis)
safe_v <- safely(v)
safe_d <- safely(coh_d)




binned_plot(rit ~ ethniccd, es$data[[1]], ref_group = "W")

# Calculate and visualize actual effect sizes
library(esvis)

## Overall
binned_plot(rit ~ ethniccd, d, ref_group = "W")

## Collapse groups
# Select for cases with >= 5 in each group
gndr <- d %>% 
  group_by(scid, content_area) %>% 
  count(gndr) %>% 
  filter(n > 5) %>% 
  mutate(n = n()) %>% 
  filter(n == 2) %>% 
  semi_join(d, .)

coh_d(rit ~ gndr, gndr, ref_group = "m")

# gender gap by school: full sample model
gndr_fs <- gndr %>% 
  group_by(scid, content_area) %>% 
  nest() %>% 
  mutate(coh_d = map_dbl(data, 
                         ~coh_d(rit ~ gndr, ., ref_group = "m")$estimate),
         n     = map_dbl(data, nrow)) %>% 
  select(-data)

theme_set(theme_minimal())

gndr_fs %>% 
  group_by(scid) %>% 
  summarize(mean_n = mean(n, na.rm = TRUE)) %>% 
  left_join(gndr_fs) %>% 
  select(-n) %>% 
  spread(content_area, coh_d) %>% 
ggplot(aes(MA, RL)) +
  geom_point(aes(size = mean_n), alpha = 0.3, color = "cornflowerblue") +
  geom_density2d(color = "gray70") +
  geom_smooth(color = "magenta")
  
# gender gap with coarsened data
gndr_count <- gndr %>% 
  group_by(content_area, scid, gndr, plc) %>% 
  count() %>% 
  group_by(scid, gndr, content_area) %>% 
  mutate(pct = n/sum(n)) %>% 
  select(-n) %>% 
  spread(plc, pct) %>% 
  mutate(`-1` = 0) %>% 
  gather(plc, pct, -1:-3, convert = TRUE) %>% 
  arrange(scid, gndr, plc) %>% 
  mutate(cum_pct = cumsum(ifelse(is.na(pct), 0, pct))) %>% 
  select(-pct) %>% 
  spread(gndr, cum_pct)

gndr_ces <- gndr_count %>% 
  group_by(scid, content_area) %>% 
  summarize(auc = integrate.xy(f, m, use.spline = FALSE), 
            v   = sqrt(2)*qnorm(auc))

gndr_es <- left_join(gndr_fs, gndr_ces) %>% 
  mutate(content_area = ifelse(content_area == "MA", 
                                "Mathematics", 
                                "Reading & Language Arts"))

ggplot(gndr_es, aes(v, coh_d)) +
  geom_point(alpha = 0.2) +
  geom_density2d(color = "cornflowerblue") +
  facet_wrap(~content_area) +
  labs(x = "V",
       y = "Cohen's D",
       title = "Achievement Gap Estimates",
       subtitle = "Estimates from coarsened data plotted against estimates from the full sample")
ggsave("vd_gender_scatter.pdf", width = 6.5, height = 7)

-4 - -4

gndr_es %>% 
  mutate(diff = v - coh_d) %>% 
  ggplot(aes(diff)) +
    geom_histogram(alpha = 0.7) +
    geom_vline(xintercept = 0, color = "cornflowerblue", lwd = 1.5) +
    facet_wrap(~content_area) +
    labs(x = expression(delta["VD"]), 
         y = "Count",
         title = "Differences in V and D",
         subtitle = "All differences (over/under-estimates) are relative to V")
ggsave("vd_gender_hist.pdf", width = 6.5, height = 7)

gndr_es %>% 
  mutate(diff = v - coh_d) %>% 
  group_by(content_area) %>% 
  summarize(mean = mean(diff),
            sd = sd(diff))

