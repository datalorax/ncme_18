library(tidyverse)
ca <- rio::import("sb_ca2017_all_csv_v2.txt", 
                  setclass = "tbl_df",
                  na = "*") %>% 
  janitor::clean_names()
ca %>% 
  group_by(subgroup_id) %>% 
  summarize(min_n = min(students_with_scores, na.rm = TRUE)) %>% 
  count(min_n)

lookup <- rio::import("Subgroups.txt", setclass = "tbl_df")
names(lookup)[2:4] <- c("subgroup_id", "subgroup_cat", "subgroup")

ca <- left_join(ca, lookup[ ,-1]) %>% 
  mutate(test_id = ifelse(test_id == "1", "ELA", "Math")) %>% 
  select(county_code:school_code, subgroup_id, subgroup_cat, grade, test_id,
         percentage_standard_exceeded,
         percentage_standard_met,
         percentage_standard_nearly_met, 
         percentage_standard_not_met) %>% 
  gather(plc, percent, starts_with("percentage")) %>% 
  mutate(plc = str_replace(plc, "percentage_standard_", ""))
ca %>% 
  group_by(subgroup_cat)
ca %>% 
  filter(subgroup_id == "74" |
         subgroup_id == "80",
         test_id == "Math") %>% 
  mutate(percent = as.numeric(percent),
           plc = case_when(plc == "exceeded" ~ 4,
                           plc == "met" ~ 3,
                           plc == "nearly_met" ~ 2,
                           plc == "not_met" ~ 1,
                           TRUE ~ 999)) %>% 
  filter(!is.na(percent), 
         school_code != "0000000") %>% 
  spread(plc, percent) %>% 
  count(is.na(`1`))

ca_props <- ca %>% 
  filter(subgroup_id == "74" |
         subgroup_id == "80") %>% 
  mutate(percent = as.numeric(percent)/100,
           plc = case_when(plc == "exceeded" ~ 4,
                           plc == "met" ~ 3,
                           plc == "nearly_met" ~ 2,
                           plc == "not_met" ~ 1,
                           TRUE ~ 999)) %>% 
  filter(!is.na(percent), 
         school_code != "0000000") %>% 
  spread(plc, percent) %>% 
  mutate(`-1` = 0) %>% 
  gather(plc, percent, `1`:`-1`, convert = TRUE) %>% 
  arrange(school_code, subgroup_id, grade, test_id, plc) %>% 
  group_by(school_code, subgroup_id, grade, test_id) %>% 
  mutate(cumsum = cumsum(percent)) %>% 
  ungroup() %>% 
  select(-subgroup_id, -percent) %>% 
  spread(subgroup_cat, cumsum) %>% 
  na.omit() %>% 
  rename("black" = `Black or African American`,
         "white" = "White")

ca_aucs <- ca_props %>% 
  group_by(school_code, test_id, grade) %>% 
  nest() %>% 
  mutate(auc = map_dbl(data, 
                       ~sfsmisc::integrate.xy(.$white, 
                                              .$black, 
                                              use.spline = FALSE)), 
         v   = sqrt(2)*qnorm(auc))

theme_set(theme_minimal())
ggplot(filter(ca_aucs, grade == 13), aes(v)) +
  geom_histogram(alpha = 0.7, bins = 50) +
  geom_vline(xintercept = 0, color = "cornflowerblue", lwd = 1.5) +
  facet_grid(grade~test_id) 

ca_aucs <- ca_aucs %>% 
  mutate(county = map_chr(data, ~unique(.$county_code)))

sch_names <- rio::import("sb_ca2017entities_csv.txt", setclass = "tbl_df") %>% 
  janitor::clean_names() %>%  
  select(school_code, school_name, county_name)

ca_aucs2 <- left_join(ca_aucs, sch_names) %>% 
  mutate(school_name = tolower(school_name))

addies <- read_csv("pubschls.csv") %>% 
  janitor::clean_names() %>% 
  unite(address, street, city, state, zip, sep = " ") %>% 
  select(school, address, county, latitude, longitude) %>% 
  mutate(school = tolower(school)) %>% 
  rename(school_name = school,
         county_name = county)

ca_aucs2 <- left_join(ca_aucs2, addies)
alameda <- ca_aucs2 %>% 
  filter(county == "01")

# gps <- ca_aucs %>% 
#   filter(county_name == "Alameda") %>% 
#   select(school_code, address) %>% 
#   distinct() %>% 
#   na.omit()
# 
# lat_long <- function(address) {
#   url <- "http://maps.google.com/maps/api/geocode/json?address="
#   url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
#   x <- RJSONIO::fromJSON(url, simplify = FALSE)
#   if (x$status == "OK") {
#     out <- c(x$results[[1]]$geometry$location$lng,
#              x$results[[1]]$geometry$location$lat)
#   } else {
#     out <- NA
#   }
#   Sys.sleep(0.2)  # API only allows 5 requests per second
#   out
# }
# 
# geo_df <- function(address) {
# 	geo <- lat_long(address)
# 	data.frame(longitude = geo[1], lattitude = geo[2])	
# }
# full_gps <- map_df(gps$address, geo_df)
# gps <- bind_cols(gps, full_gps) 
# 
# # Some ended up missing. Try again
# missed <- filter(gps, is.na(longitude))
# missed_gps <- map_df(missed$address, geo_df)
# 
# gps_missed1 <- bind_cols(missed[ ,1:2], missed_gps)
# 
# # Still missed some... Try again.
# missed2 <- filter(gps_missed1, is.na(longitude))
# missed_gps2 <- map_df(missed2$address, geo_df)
# 
# gps_missed2 <- bind_cols(missed2[ ,1:2], missed_gps2)
# 
# # only 4 left. Try one last time
# missed3 <- filter(gps_missed2, is.na(longitude))
# missed_gps3 <- map_df(missed3$address, geo_df)
# 
# gps_missed3 <- bind_cols(missed3[ ,1:2], missed_gps3)
# 
# # Still missing three. Call it good for now.
# gps_full <- bind_rows(na.omit(gps), 
#           na.omit(gps_missed1),
#           na.omit(gps_missed2),
#           na.omit(gps_missed3))
# write_csv(gps_full, "gps_schools_alameda.csv")
# 
# alameda_gps <- left_join(gps_full, ca_aucs)

## Map it
library(leaflet)
library(tidycensus)
library(sf)
Sys.getenv("CENSUS_API_KEY")
alameda_acs <- get_acs(geography = "tract", 
                     variables = "B25077_001", 
                     state = "CA",
                     county = "Alameda",
                     geometry = TRUE) %>% 
  filter(estimate > 0)

pal <- colorNumeric(palette = "viridis", 
                    domain = alameda_acs$estimate)

pal_col <- function(df) {
  sapply(df[["v"]], function(rating) {
    if(rating < -0.3) {
      "blue"
    } else if(rating >= -0.3 & rating < 0.3) {
      "white"
    } else if(rating >= 0.3 & rating < 0.9) {
      "lightred"
    } else if(rating >= 0.9 & rating < 1.5) {
      "red"
    } else {
      "darkred"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = pal_col(filter(na.omit(alameda), test_id == "Math"))
)

alameda_acs %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ estimate,
            title = "Median Home Value",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>% 
  addAwesomeMarkers(data = filter(na.omit(alameda), test_id == "Math"), 
                    ~longitude, 
                    ~latitude, 
                    icon = icons,
                    popup= ~school_name) %>% 
  addLegend("bottomright", 
            colors = rev(c("#4EA1E1", "fff", "#F27B7B", "#D53B3B", "#9C0E0E")),
            labels = rev(c("< -0.3",
                       "-0.3 to 0.3", 
                       "0.3 to 0.9", 
                       "0.9 to 1.5", 
                       "> 1.5")),
            title = "Achievement gap effect size (V)",
            opacity = 0.8)
