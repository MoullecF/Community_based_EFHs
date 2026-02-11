library(tidyr)
library(biscale)
library(sf)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(patchwork)
library(raster)
library(ggnewscale)
library(forcats)

rm(list = ls())

source("./0-Load libraries.R")
source("./ggplot_theme.R")
med_albers <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=15 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Load Emergent Hot Spot Analysis outputs per life stage
ehsa_juvenile_90 <- readRDS("./Outputs/EHSA/EHSA_juvenile_90.rds")
ehsa_adult_90 <- readRDS("./Outputs/EHSA/EHSA_adult_90.rds")

# Define custom order
custom_order <- c("new hotspot", "consecutive hotspot", "intensifying hotspot",
                  "persistent hotspot", "diminishing hotspot", "sporadic hotspot",
                  "no pattern detected", "sporadic coldspot", "diminishing colspot",
                  "persistent coldspot", "intensifying coldspot", "consecutive coldspot","new coldspot")

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

# Total surface of predicted area (in km²) / same for juveniles and adults
ehsa_juvenile_90_proj <- st_transform(ehsa_juvenile_90, crs = med_albers)
total_surface_area_km2 <- ehsa_juvenile_90_proj  %>% 
  distinct(geometry, .keep_all = TRUE) %>%  # Keep only unique geometries
  st_area() %>% 
  sum() %>% 
  units::set_units("km^2") %>% 
  as.numeric()

# ehsa_adult_90_proj <- st_transform(ehsa_adult_90, crs = med_albers)
# ehsa_adult_90_surface_area_km2 <- ehsa_adult_90_proj %>% 
#   st_area() %>% 
#   sum() %>% 
#   units::set_units("km^2") %>% 
#   as.numeric()

vec_HS <- c("New Hotspot", "Consecutive Hotspot", "Intensifying Hotspot", "Persistent Hotspot",
            "Diminishing Hotspot", "Sporadic Hotspot") #, "No Pattern Detected"

vec_CS <- c("New Coldspot", "Consecutive Coldspot", "Intensifying Coldspot", "Persistent Coldspot",
            "Diminishing Coldspot", "Sporadic Coldspot") # , "No Pattern Detected"

HS_data_juv = ehsa_juvenile_90[ehsa_juvenile_90$classification %in% vec_HS, ]
HS_data_adu = ehsa_adult_90[ehsa_adult_90$classification %in% vec_HS, ]
CS_data_juv = ehsa_juvenile_90[ehsa_juvenile_90$classification %in% vec_CS, ]
CS_data_adu = ehsa_adult_90[ehsa_adult_90$classification %in% vec_CS, ]


HS_data_juv$classification <- str_replace(HS_data_juv$classification, "No Pattern Detected", "J.Npd")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "New Hotspot", "J.New")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "Consecutive Hotspot", "J.Con")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "Intensifying Hotspot", "J.Int")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "Persistent Hotspot", "J.Per")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "Diminishing Hotspot", "J.dim")
HS_data_juv$classification <- str_replace(HS_data_juv$classification, "Sporadic Hotspot", "J.Spo")

CS_data_juv$classification <- str_replace(CS_data_juv$classification, "No Pattern Detected", "J.Npd")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "New Coldspot", "J.New")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "Consecutive Coldspot", "J.Con")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "Intensifying Coldspot", "J.Int")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "Persistent Coldspot", "J.Per")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "Diminishing Coldspot", "J.dim")
CS_data_juv$classification <- str_replace(CS_data_juv$classification, "Sporadic Coldspot", "J.Spo")

HS_data_adu$classification <- str_replace(HS_data_adu$classification, "No Pattern Detected", "A.Npd")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "New Hotspot", "A.New")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "Consecutive Hotspot", "A.Con")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "Intensifying Hotspot", "A.Int")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "Persistent Hotspot", "A.Per")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "Diminishing Hotspot", "A.Dim")
HS_data_adu$classification <- str_replace(HS_data_adu$classification, "Sporadic Hotspot", "A.Spo")

CS_data_adu$classification <- str_replace(CS_data_adu$classification, "No Pattern Detected", "A.Npd")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "New Coldspot", "A.New")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "Consecutive Coldspot", "A.Con")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "Intensifying Coldspot", "A.Int")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "Persistent Coldspot", "A.Per")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "Diminishing Coldspot", "A.Dim")
CS_data_adu$classification <- str_replace(CS_data_adu$classification, "Sporadic Coldspot", "A.Spo")

HS_df <- st_join(HS_data_juv, HS_data_adu, largest = TRUE)
HS_df$Classification <- paste(HS_df$classification.x, HS_df$classification.y, sep = " - ")
CS_df <- st_join(CS_data_juv, CS_data_adu,  largest = TRUE)
CS_df$Classification <- paste(CS_df$classification.x, CS_df$classification.y, sep = " - ")

HS_df <- HS_df[!grepl("- NA", HS_df$Classification),]
CS_df <- CS_df[!grepl("- NA", CS_df$Classification),]

# Remove spot associations with "no pattern detected"
npd_remove <- c("J.New - A.Npd", "J.Int - A.Npd", "J.Per - A.Npd", "J.dim - A.Npd", "J.Spo - A.Npd", "J.Npd - A.Spo", "J.Npd - A.Per", "J.Npd - A.Int",
                "J.Npd - A.New", "J.Npd - A.Con","J.Con - A.Npd")

HS_df <- HS_df[!HS_df$Classification %in% npd_remove,]
HS_df$Classification <- as.factor(HS_df$Classification)
CS_df <- CS_df[!CS_df$Classification %in% npd_remove,]
CS_df$Classification <- as.factor(CS_df$Classification)


unique(HS_df$Classification)
unique(CS_df$Classification)

HS_legend_col <- colorRampPalette(brewer.pal(9, "YlGn"))(13)
#HS_legend_col <- c(HS_legend_col[1:length(HS_legend_col)%%2==0], "grey80")
CS_legend_col <- colorRampPalette(brewer.pal(9, "BuPu"))(22)
#CS_legend_col <- c(CS_legend_col[1:length(CS_legend_col)%%2==0], "grey80")

# Join the sf objects with the legend colors
HS_legend_col <- c(
  "J.Int - A.Int" = HS_legend_col[13],  
  "J.Int - A.Per" = HS_legend_col[12],  
  "J.Int - A.Spo" = HS_legend_col[11],
  "J.Per - A.Int" = HS_legend_col[10],  
  "J.Per - A.Per" = HS_legend_col[9],  
  "J.Per - A.Spo" = HS_legend_col[8],
  ###"J.dim - A.Int" = HS_legend_col[10],  
  ###"J.dim - A.Per" = HS_legend_col[9],  
  "J.dim - A.Spo" = HS_legend_col[7],
  "J.Spo - A.Int" = HS_legend_col[6],  
  "J.Spo - A.Per" = HS_legend_col[5],  
  "J.Spo - A.Spo" = HS_legend_col[4],
  "J.Con - A.Per" = HS_legend_col[3],  
  "J.New - A.Int" = HS_legend_col[2],  
  ###"J.New - A.Per" = HS_legend_col[2],  
  "J.New - A.Spo" = HS_legend_col[1])
  ###"J.Npd - A.Npd" = "grey80")

CS_legend_col <- c(
  "J.Int - A.Int" = CS_legend_col[22],
  "J.Int - A.Per" = CS_legend_col[21],
  "J.Int - A.Spo" = CS_legend_col[20],
  "J.Int - A.Con" = CS_legend_col[19],
  "J.Int - A.New" = CS_legend_col[18],
  "J.Per - A.Int" = CS_legend_col[17], 
  "J.Per - A.Per" = CS_legend_col[16],
  "J.Per - A.Spo" = CS_legend_col[15],
  "J.Per - A.Con" = CS_legend_col[14],
  "J.Per - A.New" = CS_legend_col[13],
  "J.Spo - A.Int" = CS_legend_col[12],
  "J.Spo - A.Per" = CS_legend_col[11],
  "J.Spo - A.Spo" = CS_legend_col[10],
  "J.Spo - A.Con" = CS_legend_col[9],
  "J.Spo - A.New" = CS_legend_col[8],
  "J.Con - A.Int" = CS_legend_col[7],  
  "J.Con - A.Per" = CS_legend_col[6],  
  "J.Con - A.Spo" = CS_legend_col[5],
  "J.Con - A.Con" = CS_legend_col[4],  
  ###"J.Con - A.New" = CS_legend_col[6],
  "J.New - A.Int" = CS_legend_col[3], 
  "J.New - A.Per" = CS_legend_col[2],
  "J.New - A.Spo" = CS_legend_col[1])
  ###"J.New - A.Con" = CS_legend_col[2],
  ###"J.New - A.New" = CS_legend_col[1],
  ###"J.Npd - A.Npd" = "grey80")

levels(HS_df$Classification)
levels(CS_df$Classification)

#specify the factor levels in the order you want
HS_df$Classification <- factor(HS_df$Classification, levels = c("J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
                                                                "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
                                                                "J.dim - A.Int", "J.dim - A.Per", "J.dim - A.Spo",
                                                                "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo",
                                                                "J.Con - A.Per",
                                                                "J.New - A.Int", "J.New - A.Per", "J.New - A.Spo",
                                                                "J.Npd - A.Npd"))


CS_df$Classification <- factor(CS_df$Classification, levels = c("J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo", "J.Int - A.Con", "J.Int - A.New",
                                                                  "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo", "J.Per - A.Con", "J.Per - A.New",
                                                                  "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo", "J.Spo - A.Con", "J.Spo - A.New",
                                                                  "J.Con - A.Int", "J.Con - A.Per", "J.Con - A.Spo", "J.Con - A.Con", "J.Con - A.New",
                                                                  "J.New - A.Int", "J.New - A.Per", "J.New - A.Spo", "J.New - A.Con", "J.New - A.New",
                                                                  "J.Npd - A.Npd"))


# ###########
# HS_df$Spot <- "Hotspots"
# CS_df$Spot <- "Coldspots"
# combinedspot <- rbind(HS_df, CS_df)
# saveRDS(combinedspot, file = "./Outputs/Hotspots_analysis/Combined_HS_CS.RDS")
# ###########

# Hotspot congruence
gg.HS <- ggplot(data = world) +
  geom_sf(data = HS_df, aes(fill = Classification), color="white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual(values = HS_legend_col)+
  geom_contour(data = bathy_df, aes(x = x, y = y, z = Depth),
               color = "black", linewidth = 0.3, linetype = 2, breaks = c(-200)) +
  geom_sf(fill = "grey90", color = "grey20") +
  coord_sf(xlim = c(st_bbox(HS_df)[1]-0.1, st_bbox(HS_df)[3]+0.1 ), ylim = c(st_bbox(HS_df)[2]-0.1, st_bbox(HS_df)[4]+0.1), expand = FALSE) +
  labs(fill = "Hotspots\nClassification", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2)), fill=guide_legend(ncol= 2))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.position = "right",
        legend.key = element_rect(color = "white"))+
  geom_text(data = data.frame(label = "Hotspots", x = -3.4, y = 44),aes(label = label, x = x, y = y),size = 5, fontface = "bold")


# Coldspot congruence
gg.CS <- ggplot(data = world) +
  geom_sf(data = CS_df, aes(fill = Classification), color="white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual(values = CS_legend_col)+
  geom_contour(data = bathy_df, aes(x = x, y = y, z = Depth),
               color = "black", linewidth = 0.3, linetype = 2, breaks = c(-200)) +
  geom_sf(fill = "grey90", color = "grey20") +
  coord_sf(xlim = c(st_bbox(CS_df)[1]-0.1, st_bbox(CS_df)[3]+0.1 ), ylim = c(st_bbox(CS_df)[2]-0.1, st_bbox(CS_df)[4]+0.1), expand = FALSE) +
  labs(fill = "Coldspots\nClassification", x = "Longitude", y = "Latitude") +
  guides(shape = guide_legend(override.aes = list(size = 0.2)), fill=guide_legend(ncol= 2))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10), 
        legend.position = "right",
        legend.key = element_rect(color = "white"))+
  geom_text(data = data.frame(label = "Coldspots", x = -3.4, y = 44),aes(label = label, x = x, y = y),size = 5, fontface = "bold")

map_combined_spots <- gg.HS / gg.CS 
#map_combined_spots

#### Final figure
gg.combined.spot <- ggplot(data = world) +
  geom_sf(data = HS_df, aes(fill = Classification), color="white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual("Hotspots Pattern", values = HS_legend_col) +
  guides(fill=guide_legend(ncol= 2))+
  new_scale_fill() +
  geom_sf(data = CS_df, aes(fill = Classification), color="white", lwd = 0.05, inherit.aes = TRUE) +
  scale_fill_manual("Coldspots Pattern", values = CS_legend_col)+
  geom_sf(fill = "grey90", color = "grey20") +
  #geom_contour(data = bathy_df, aes(x = x, y = y, z = Depth),
   #            color = "black", linewidth = 0.35, linetype = 2, breaks = c(-200)) +
  coord_sf(xlim = c(st_bbox(HS_df)[1]-0.1, st_bbox(HS_df)[3]+0.1 ), ylim = c(st_bbox(HS_df)[2]-0.1, st_bbox(HS_df)[4]+0.1), expand = FALSE) +
  labs(fill = "Classification", x = "Longitude", y = "Latitude") +
  guides(fill=guide_legend(ncol= 2))+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.background = element_rect(fill = "white"), 
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10), legend.text=element_text(size=9),
        legend.position = "right",
        legend.key = element_rect(color = "white"),
        legend.key.size = unit(.5, 'cm'),
        plot.margin = unit(c(0,0,0,0), units = , "cm"))
gg.combined.spot
ggplot2::ggsave(gg.combined.spot, filename = "C:/Users/fabie/Documents/Communications/Publications/28-HMSC_Med_onto/Figures/combined_ehsa.png", width = 20, height = 15, units = "cm", dpi = 400)

#########################################################
HS_df_proj <- st_transform(HS_df, crs = med_albers)
CS_df_proj <- st_transform(CS_df, crs = med_albers)

# Stacked barplot of the surface of each classification
# Calculate total surface area in km²
total_surface_area_km2 <- ehsa_juvenile_90_proj %>% 
  distinct(geometry, .keep_all = TRUE) %>%  # Keep only unique geometries
  st_area() %>% 
  sum() %>% 
  units::set_units("km^2") %>% 
  as.numeric()

# Calculate surface per classification
areas_by_classification_HS <-HS_df_proj %>%
  # Convert each polygon's area to km² immediately
  mutate(area_km2 = st_area(geometry) %>% 
           units::set_units("km^2") %>% 
           as.numeric(), Spot = "Hotspots") %>%
  # Remove geometry and group
  st_drop_geometry() %>%
  group_by(Classification, Spot) %>%
  summarise(total_area_km2 = sum(area_km2), n_polygons = n(), .groups = "drop") %>%
  # Calculate percentage
  mutate(pct_total = (total_area_km2 / total_surface_area_km2) * 100,
         total_area_km2 = round(total_area_km2, 3), pct_total = round(pct_total, 3)) %>%
  arrange(desc(total_area_km2))

areas_by_classification_CS <-CS_df_proj %>%
  # Convert each polygon's area to km² immediately
  mutate(area_km2 = st_area(geometry) %>% 
           units::set_units("km^2") %>% 
           as.numeric(), Spot = "Coldspots") %>%
  # Remove geometry and group
  st_drop_geometry() %>%
  group_by(Classification, Spot) %>%
  summarise(total_area_km2 = sum(area_km2), n_polygons = n(), .groups = "drop") %>%
  # Calculate percentage
  mutate(pct_total = (total_area_km2 / total_surface_area_km2) * 100,
         total_area_km2 = round(total_area_km2, 3), pct_total = round(pct_total, 3)) %>%
  arrange(desc(total_area_km2))

# Sum grouped classification that have there surface representing less than 1%
# Group classification representing less than 1% of total surface
tab_HS <- areas_by_classification_HS %>%
  # Create a flag for rows to be aggregated
  mutate(classification_group = ifelse(pct_total < 1, "Others (< 1%)", as.character(Classification))) %>%
  # Group by this new classification
  group_by(classification_group, Spot) %>%
  # Summarize the data
  summarize(
    total_area_km2 = sum(total_area_km2),
    n_polygons = sum(n_polygons),
    pct_total = sum(pct_total),
    .groups = "drop"
  ) %>%
  # Rename the classification column back
  rename(Classification = classification_group) %>%
  # Convert Classification back to factor if needed
  mutate(Classification = as.factor(Classification))# %>%
  # Reorder rows if needed (optional)
  #arrange(desc(pct_total))

tab_CS <- areas_by_classification_CS %>%
  # Create a flag for rows to be aggregated
  mutate(classification_group = ifelse(pct_total < 1, "Others (< 1%)", as.character(Classification))) %>%
  # Group by this new classification
  group_by(classification_group, Spot) %>%
  # Summarize the data
  summarize(
    total_area_km2 = sum(total_area_km2),
    n_polygons = sum(n_polygons),
    pct_total = sum(pct_total),
    .groups = "drop"
  ) %>%
  # Rename the classification column back
  rename(Classification = classification_group) %>%
  # Convert Classification back to factor if needed
  mutate(Classification = as.factor(Classification))# %>%
  # Reorder rows if needed (optional)
  #arrange(desc(pct_total))

tab_surf <- bind_rows(tab_HS, tab_CS) %>%
  mutate(Classification = as.factor(Classification)) %>%
  mutate(fill_group = case_when(Spot == "Coldspots" ~ "Cold", Spot == "Hotspots" ~ "Hot", Spot == "Hotspots" ~ "Hot")) %>%
  mutate(class_interaction = interaction(Classification, fill_group))


# tab_surf$class_interaction <- factor(tab_surf$class_interaction, levels = c("J.Int - A.Int.Cold", "J.Int - A.Per.Cold", "J.Int - A.Spo.Cold",
#                                                                             "J.Per - A.Int.Cold", "J.Per - A.Per.Cold", "J.Per - A.Spo.Cold",
#                                                                             "J.Spo - A.Int.Cold", "J.Spo - A.Per.Cold", "J.Spo - A.Spo.Cold",
#                                                                             "Others (< 1%).Cold",
#                                                                             "J.Int - A.Per.Hot",  "J.Per - A.Per.Hot",  "J.Per - A.Spo.Hot",
#                                                                             "J.Spo - A.Per.Hot",  "J.Spo - A.Spo.Hot", "Others (< 1%).Hot"))

# Define separate color palettes
cold_palette <- c(
  "J.Int - A.Int" = "#4D004B",
  "J.Int - A.Per" = "#60055D",
  "J.Int - A.Spo" = "#740B70",
  "J.Per - A.Int" = "#873C99", 
  "J.Per - A.Per" = "#894DA2",
  "J.Per - A.Spo" = "#8A5DAA",
  "J.Spo - A.Int" = "#8C8DC2",
  "J.Spo - A.Per" = "#8F9DC9",
  "J.Spo - A.Spo" = "#96ABD1",
  "Others (< 1%)" = "grey")

hot_palette <- c(
  "J.Int - A.Per" = "#005C32",  
  "J.Per - A.Per" = "#379E54",  
  "J.Per - A.Spo" = "#53B466",
  "J.Spo - A.Per" = "#BBE395",  
  "J.Spo - A.Spo" = "#D9F0A3",
  "Others (< 1%)" = "grey")


gg.surf <- ggplot(tab_surf) +
  geom_col(data = subset(tab_surf, Spot == "Coldspots"), aes(x = Spot, y = pct_total, fill = fct_relevel(Classification, "Others (< 1%)", "J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
                                                                                                         "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
                                                                                                         "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo")),
    position = position_stack(reverse = FALSE)) +
  scale_fill_manual(values = cold_palette, name = "Combined Coldspots Pattern", breaks=c("J.Int - A.Int", "J.Int - A.Per", "J.Int - A.Spo",
                                                                               "J.Per - A.Int", "J.Per - A.Per", "J.Per - A.Spo",
                                                                               "J.Spo - A.Int", "J.Spo - A.Per", "J.Spo - A.Spo", "Others (< 1%)")) +
  guides(fill=guide_legend(ncol= 2))+
  new_scale_fill() +
  geom_col(data = subset(tab_surf, Spot == "Hotspots"), aes(x = Spot, y = pct_total, fill = fct_relevel(Classification, "Others (< 1%)", "J.Int - A.Per",  "J.Per - A.Per",  "J.Per - A.Spo",
                                                                                                        "J.Spo - A.Per",  "J.Spo - A.Spo")),
    position = position_stack(reverse = FALSE)) +
  scale_fill_manual(values = hot_palette, name = "Combined Hotspots Pattern", breaks = c("J.Int - A.Per",  "J.Per - A.Per",  "J.Per - A.Spo",
                                                                                "J.Spo - A.Per",  "J.Spo - A.Spo", "Others (< 1%)")) +
  scale_x_discrete(limits = c("Coldspots", "Hotspots"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 32), breaks=c(0, 10, 20, 30), labels = c("0%", "10%", "20%", "30%"), expand = c(0,0))+  labs(x = "", y = "Proportion of total surface") +
  HMSC.theme +
  guides(fill=guide_legend(ncol= 2))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

ggplot2::ggsave(gg.surf, filename = "C:/Users/fabie/Documents/Communications/Publications/28-HMSC_Med_onto/Figures/combined_ehsa_surface.png", width = 20, height = 15, units = "cm", dpi = 400)

