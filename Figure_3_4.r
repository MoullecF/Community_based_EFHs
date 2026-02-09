
rm(list = ls())

source("./0-Load libraries.R")
source("./ggplot_theme.R")

# Load Emergent Hot Spot Analysis outputs per life stage
ehsa_juvenile_90 <- readRDS("./Outputs/EHSA/EHSA_juvenile_90.rds")
ehsa_adult_90 <- readRDS("./Outputs/EHSA/EHSA_adult_90.rds")

# Define custom order
custom_order <- c("intensifying hotspot","persistent hotspot","diminishing hotspot", "sporadic hotspot","consecutive hotspot", "new hotspot",  
                  "no pattern detected", "new coldspot", "consecutive coldspot", "sporadic coldspot", "diminishing colspot",
                  "persistent coldspot", "intensifying coldspot")

# Reorder the data frames
ehsa_juvenile_90 <- ehsa_juvenile_90 %>%
  as.data.frame() %>%  
  mutate(classification = str_to_title(classification),
         classification = factor(classification, levels = str_to_title(custom_order))) %>%
  arrange(classification) %>%
  st_as_sf()

ehsa_adult_90 <- ehsa_adult_90 %>%
  as.data.frame() %>%  
  mutate(classification = str_to_title(classification),
         classification = factor(classification, levels = str_to_title(custom_order))) %>%
  arrange(classification) %>%
  st_as_sf()

# Different color palettes
BrBG_pal <- rev(colorRampPalette(brewer.pal(11, "BrBG"))(13))

custom_palette <- c(
  "Intensifying Hotspot" = "#003c30",  
  "Persistent Hotspot" = "#005F56",     
  "Diminishing Hotspot" = "#23867E",    
  "Sporadic Hotspot" = "#5AB2A8",       
  "Consecutive Hotspot" = "#97D6CD",    
  "New Hotspot" = "#CEEBE7",  
  "No Pattern Detected" = "#F5F5F5",  
  "New Coldspot" = "#F5EACB",
  "Consecutive Coldspot" = "#E6CE94",    
  "Sporadic Coldspot" = "#CFA154",      
  "Diminishing Coldspot" = "#AE7121",   
  "Persistent Coldspot" = "#824B09",     
  "Intensifying Coldspot" = "#543005")


# Plot results
# Project with a new projection system : Albers equal-area conic projection

# Get significant coordinates
ehsa_juvenile_90_points <- ehsa_juvenile_90 %>%
  st_centroid() %>%
  mutate(longitude = st_coordinates(.)[, "X"],latitude  = st_coordinates(.)[, "Y"])

ehsa_adult_90_points <- ehsa_adult_90 %>%
  st_centroid() %>%
  mutate(longitude = st_coordinates(.)[, "X"],latitude  = st_coordinates(.)[, "Y"])

ehsa_juv_90 = ggplot(data = world) +
  geom_sf(data = ehsa_juvenile_90, aes(fill = classification), color="white", lwd = 0.05, show.legend = TRUE) +
  scale_fill_manual(values = custom_palette) +
  #geom_point(data = ehsa_juvenile_90_points[ehsa_juvenile_90_points$p_value<0.05,], aes(x = longitude, y = latitude), stroke =.1, size = .1, alpha = 1)+
  geom_sf(fill = "grey90", color = "grey20") +
  coord_sf(xlim = c(st_bbox(ehsa_juvenile_90)[1]-0.1, st_bbox(ehsa_juvenile_90)[3]+0.1 ), ylim = c(st_bbox(ehsa_juvenile_90)[2]-0.1, st_bbox(ehsa_juvenile_90)[4]+0.1), expand = FALSE) +
  labs(fill = "Pattern", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2)))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.position = "right",
        legend.key = element_rect(color = "white"))#+
  #geom_text(data = data.frame(label = "Juveniles", x = -3.4, y = 44),aes(label = label, x = x, y = y),size = 5, fontface = "bold")
ehsa_juv_90
ehsa_adu_90 = ggplot(data = world) +
  geom_sf(data = ehsa_adult_90, aes(fill = classification),color="white", lwd = 0.05, show.legend = FALSE) +
  scale_fill_manual(values = custom_palette) +
  #geom_point(data = ehsa_adult_90_points[ehsa_adult_90_points$p_value<0.05,], aes(x = longitude, y = latitude), size = .1,stroke =.1, alpha = 1)+
  geom_sf(fill = "grey90", color = "grey20") +
  coord_sf(xlim = c(st_bbox(ehsa_adult_90)[1]-0.1, st_bbox(ehsa_adult_90)[3]+0.1), ylim = c(st_bbox(ehsa_adult_90)[2]-0.1, st_bbox(ehsa_adult_90)[4]+0.1), expand = FALSE) +
  labs(fill = "Pattern", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2)))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.position = "right",
        legend.key = element_rect(color = "white"))#+
  #geom_text(data = data.frame(label = "Adults", x = -3.4, y = 44),aes(label = label, x = x, y = y),size = 5, fontface = "bold")

# Combined plot
map_ehsa <- ehsa_juv_90 / ehsa_adu_90 + 
  plot_layout(guides = "collect") &  # Collects legends
  theme(legend.position = "right")

ggplot2::ggsave(map_ehsa, filename = "C:/Users/fabie/Documents/Communications/Publications/28-HMSC_Med_onto/Figures/ehsa_per_life_stage.png", width = 20, height = 15, units = "cm", dpi = 400)

#########################################################
# Stacked barplot of the surface od each classification
med_albers <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ehsa_juvenile_90_proj <- st_transform(ehsa_juvenile_90, crs = med_albers)
ehsa_adult_90_proj <- st_transform(ehsa_adult_90, crs = med_albers)

# Calculate total surface area in km²
surface_area_km2 <- ehsa_juvenile_90_proj %>% 
  st_area() %>% 
  sum() %>% 
  units::set_units("km^2") %>% 
  as.numeric()

# Calculate surface per onto stage and spots' classification
areas_by_classification_juv <- ehsa_juvenile_90_proj %>%
  # Convert each polygon's area to km² immediately
  mutate(area_km2 = st_area(geometry) %>% 
           units::set_units("km^2") %>% 
           as.numeric(), onto = "Juveniles") %>%
  # Remove geometry and group
  st_drop_geometry() %>%
  group_by(classification, onto) %>%
  summarise(total_area_km2 = sum(area_km2), n_polygons = n(), .groups = "drop") %>%
  # Calculate percentage
  mutate(pct_total = (total_area_km2 / surface_area_km2) * 100,
         total_area_km2 = round(total_area_km2, 3), pct_total = round(pct_total, 1)) %>%
  arrange(desc(total_area_km2))

areas_by_classification_juv <- areas_by_classification_juv %>%
  # Create a flag for rows to be aggregated
  mutate(classification_group = ifelse(pct_total < 1, "Others (< 1%)", as.character(classification))) %>%
  # Group by this new classification
  group_by(classification_group, onto) %>%
  # Summarize the data
  summarize(
    total_area_km2 = sum(total_area_km2),
    n_polygons = sum(n_polygons),
    pct_total = sum(pct_total),
    .groups = "drop"
  ) %>%
  # Rename the classification column back
  rename(classification = classification_group) %>%
  # Convert Classification back to factor if needed
  mutate(classification = as.factor(classification)) %>%
  # Reorder rows if needed (optional)
  arrange(desc(pct_total))

areas_by_classification_adu <- ehsa_adult_90_proj %>%
  # Convert each polygon's area to km² immediately
  mutate(area_km2 = st_area(geometry) %>% 
           units::set_units("km^2") %>% 
           as.numeric(), onto = "Adults") %>%
  # Remove geometry and group
  st_drop_geometry() %>%
  group_by(classification, onto) %>%
  summarise(total_area_km2 = sum(area_km2), n_polygons = n(), .groups = "drop") %>%
  # Calculate percentage
  mutate(pct_total = (total_area_km2 / surface_area_km2) * 100,
         total_area_km2 = round(total_area_km2, 3), pct_total = round(pct_total, 1)) %>%
  arrange(desc(total_area_km2))

areas_by_classification_adu <- areas_by_classification_adu %>%
  # Create a flag for rows to be aggregated
  mutate(classification_group = ifelse(pct_total < 1, "Others (< 1%)", as.character(classification))) %>%
  # Group by this new classification
  group_by(classification_group, onto) %>%
  # Summarize the data
  summarize(
    total_area_km2 = sum(total_area_km2),
    n_polygons = sum(n_polygons),
    pct_total = sum(pct_total),
    .groups = "drop"
  ) %>%
  # Rename the classification column back
  rename(classification = classification_group) %>%
  # Convert Classification back to factor if needed
  mutate(classification = as.factor(classification)) %>%
  # Reorder rows if needed (optional)
  arrange(desc(pct_total))

tab_surf <- bind_rows(areas_by_classification_juv, areas_by_classification_adu) %>%
  mutate(onto = factor(onto, levels = c("Juveniles", "Adults"))) %>%
  mutate(classification = factor(classification, levels = c("Intensifying Hotspot","Persistent Hotspot",
                                                            "Diminishing Hotspot","Sporadic Hotspot","Consecutive Hotspot",
                                                            "New Hotspot", "No Pattern Detected", "New Coldspot","Consecutive Coldspot",
                                                            "Sporadic Coldspot","Diminishing Coldspot","Persistent Coldspot","Intensifying Coldspot", "Others (< 1%)")))

hotspot_summary <- tab_surf %>%
  filter(classification %in% c("Persistent Hotspot", "Sporadic Hotspot", 
                               "Intensifying Hotspot", "Diminishing Hotspot",
                               "New Hotspot", "Consecutive Hotspot")) %>%
  group_by(onto) %>%
  summarise(total_hotspot_pct = sum(pct_total),
            total_area_km2 = sum(total_area_km2),
            total_polygons = sum(n_polygons)) %>%
  mutate(total_hotspot_pct = round(total_hotspot_pct, 1))
print(hotspot_summary)

coldspot_summary <- tab_surf %>%
  filter(classification %in% c("Persistent Coldspot", "Sporadic Coldspot", 
                               "Intensifying Coldspot", "Diminishing Coldspot",
                               "New Coldspot", "Consecutive Coldspot")) %>%
  group_by(onto) %>%
  summarise(total_Coldspot_pct = sum(pct_total),
            total_area_km2 = sum(total_area_km2),
            total_polygons = sum(n_polygons)) %>%
  mutate(total_Coldspot_pct = round(total_Coldspot_pct, 1))
print(coldspot_summary)

custom_ylabels <- c(
  "<span style='font-size:12pt'>0%</span>",
  "<span style='font-size:12pt'>25%</span>",
  "<span style='font-size:10pt'>33.5%</span>",
  "<span style='font-size:10pt'>38.4%</span>",
  "<span style='font-size:12pt'>50%</span>",
  "<span style='font-size:12pt'>75%</span>",
  "<span style='font-size:12pt'>100%</span>")

custom_palette_2 <- c(
  "Intensifying Hotspot" = "#003c30",  
  "Persistent Hotspot" = "#005F56",     
  "Diminishing Hotspot" = "#23867E",    
  "Sporadic Hotspot" = "#5AB2A8",       
  "Consecutive Hotspot" = "#97D6CD",    
  "New Hotspot" = "#CEEBE7",  
  "No Pattern Detected" = "#F5F5F5",  
  "New Coldspot" = "#F5EACB",
  "Consecutive Coldspot" = "#E6CE94",    
  "Sporadic Coldspot" = "#CFA154",      
  "Diminishing Coldspot" = "#AE7121",   
  "Persistent Coldspot" = "#824B09",     
  "Intensifying Coldspot" = "#543005",
  "Others (< 1%)" = "grey")


gg_surf_spot <- ggplot(tab_surf, aes(x = onto, y = pct_total, fill = classification)) +
  geom_col(position = position_stack(reverse = TRUE)) +  # Stacked bars
  scale_fill_manual(values = custom_palette_2,guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, 100), breaks=c(0, 25, 33.5,38.4, 50, 75, 100), labels = custom_ylabels, expand = c(0,0))+
  scale_x_discrete(limits = c("Juveniles","Adults"),expand = c(0, 0)) +
  labs(x = "", y = "Proportion of total surface", fill = "Pattern") +
  geom_segment(aes(x = 0.55, xend = 1.45, y =33.5, yend = 33.5), linetype = "dashed", color = "black", linewidth = .7) +
  geom_segment(aes(x = 0.55, xend = 2.45, y =38.4, yend = 38.4), linetype = "dashed", color = "black", linewidth = .7) +
  HMSC.theme +
  theme(plot.margin = unit(c(1,1,1,1), units = , "cm"),axis.text.y = element_markdown())

ggplot2::ggsave(gg_surf_spot, filename = "C:/Users/fabie/Documents/Communications/Publications/28-HMSC_Med_onto/Figures/prop_ehsa.png", width = 20, height = 15, units = "cm", dpi = 400)
