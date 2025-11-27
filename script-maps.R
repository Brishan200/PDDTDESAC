library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(marmap)
library(stars)
library(ncdf4)
library(ggspatial)
library(ggrepel)
library(ggforce)
library(tidyverse)
library(patchwork)
library(purrr)
library(grateful)
library(viridis)

#Data used to create plot was downloaded using 
# MyOcean Viewer (CMEMS)
# L4 satellite data

#constants-----
sa <- ne_countries(country = "south africa", scale = "large", returnclass = "sf")
# Download bathymetry for South Africa region (ETOPO1 ~ 1 arcmin resolution)
bathy <- getNOAA.bathy(lon1 = 10, lon2 = 50, lat1 = -50, lat2 = -20, resolution = 1)

# Convert bathy to dataframe for ggplot
bathy_df <- fortify.bathy(bathy)


stations <- tribble(
  ~site, ~station, ~date,        ~latitude, ~longitude,
  "AB",   1, "2023-10-26", -33.89616670, 25.70227778,
  "AB",   2, "2023-10-26", -33.82624000, 25.75429000,
  "AB",   3, "2023-10-26", -33.76805560, 25.90602778,
  # "AB",   4, "2023-10-26", -33.73205560, 26.06397222,
  # "AB",   5, "2023-10-26", -33.75666670, 26.23477778,
  # "AB",   6, "2023-10-26", -33.86869000, 26.29146000,
  # "AB",   7, "2023-10-26", -33.88177000, 25.98722000,
  # "AB",   8, "2023-10-26", -34.03411000, 25.72771000,
  "AB",   1, "2023-10-25", -33.89616670, 25.70227778,
  "AB",   2, "2023-10-25", -33.82624000, 25.75429000,
  "AB",   3, "2023-10-25", -33.76805560, 25.90602778,
  "SHB",  1, "2023-10-30", -32.7450333, 18.1221166,
  "SHB",  2, "2023-10-30", -32.7133333, 18.1033333,
  "SHB",  3, "2023-10-30", -32.6833330, 18.0883333,
  "SHB",  5, "2023-10-30", -32.6258330, 18.0591666,
  "SHB",  4, "2023-10-30", -32.6550000, 18.0733333,
  "SHB",  1, "2023-10-31", -32.7450333, 18.1221166,
  "SHB",  2, "2023-10-31", -32.7133333, 18.1033333,
  "SHB",  3, "2023-10-31", -32.6833330, 18.0883333,
  "SHB",  5, "2023-10-31", -32.6258330, 18.0591666,
  "SHB",  4, "2023-10-31", -32.6550000, 18.0733333,
  "WB",   6, "2023-11-08", -34.45991667, 19.25573333,
  "WB",   5, "2023-11-08", -34.47508333, 19.28253333,
  "WB",   4, "2023-11-08", -34.49386667, 19.30015000,
  "WB",   3, "2023-11-08", -34.51581667, 19.30928333,
  "WB",   2, "2023-11-08", -34.53833333, 19.31871667,
  "WB",   1, "2023-11-08", -34.56038333, 19.33003330,
  "WB",   1, "2023-11-09", -34.56038333, 19.33003330,
  "WB",   2, "2023-11-09", -34.53833333, 19.31871667,
  "WB",   3, "2023-11-09", -34.51581667, 19.30928333,
  "WB",   4, "2023-11-09", -34.49386667, 19.30015000
) %>%
  mutate(date = as.Date(date))



bays <- data.frame(
  site = c("Algoa Bay", "St Helena Bay", "Walker Bay"),
  lon  = c(25.8, 18.65, 19.65),
  lat  = c(-33.8, -33.05, -34.49),
  r    = c(0.35, 0.25, 0.25)  # radius in degrees, adjust as needed
)

bay_boxes <- bays %>%
  mutate(
    xmin = lon - r,
    xmax = lon + r,
    ymin = lat - r,
    ymax = lat + r
  )

##


# Bioscape daily maps  ----------------------------------------------------
# File list
files <- list(
  AB  = "maps/ab_METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1758214700943.nc",
  SHB = "maps/shb_METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1758214826799.nc",
  WB  = "maps/wb_METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2_1758214762662.nc"
)
files_chla <- list(
  AB  = "maps/ab_cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D_1758225802782.nc",
  SHB = "maps/shb_cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D_1758225845256.nc",
  WB  = "maps/wb_cmems_obs-oc_glo_bgc-plankton_my_l4-gapfree-multi-4km_P1D_1758225763677.nc"
)

# Helper: read one file
read_sst_file <- function(file, site_name) {
  nc <- nc_open(file)
  
  lon  <- ncvar_get(nc, "longitude")
  lat  <- ncvar_get(nc, "latitude")
  sst  <- ncvar_get(nc, "analysed_sst") # check variable name in file
  time <- ncvar_get(nc, "time")        # usually hours since 1981-01-01
  tunit <- ncatt_get(nc, "time", "units")$value
  nc_close(nc)
  
  # Convert time to Date
  time_origin <- sub("days since ", "", tunit)
  dates <- as.POSIXct(time, origin = "1970-01-01", tz = "UTC")
  
  # Put into long dataframe
  df_list <- list()
  for (t in seq_along(dates)) {
    temp_df <- expand.grid(lon = lon, lat = lat) %>%
      mutate(temp = as.vector(sst[,,t]),
             date = dates[t],
             site = site_name)
    df_list[[t]] <- temp_df
  }
  
  bind_rows(df_list)
}
read_chla_file <- function(file, site_name) {
  nc <- nc_open(file)
  
  lon  <- ncvar_get(nc, "longitude")
  lat  <- ncvar_get(nc, "latitude")
  chla <- ncvar_get(nc, "CHL")          # chlorophyll-a variable
  time <- ncvar_get(nc, "time")         # seconds since 1970-01-01
  nc_close(nc)
  
  # Convert time to Date
  dates <- as.Date(as.POSIXct(time, origin = "1970-01-01", tz = "UTC"))
  
  # Put into long dataframe
  df_list <- list()
  for (t in seq_along(dates)) {
    temp_df <- expand.grid(lon = lon, lat = lat) %>%
      mutate(chla = as.vector(chla[,,t]),
             date = dates[t],
             site = site_name)
    df_list[[t]] <- temp_df
  }
  
  bind_rows(df_list)
}


# Load all sites
sst_all <- bind_rows(
  read_sst_file(files$AB, "Algoa Bay"),
  read_sst_file(files$SHB, "St Helena Bay"),
  read_sst_file(files$WB, "Walker Bay")
) %>% 
  mutate(temp = temp - 273.15)

chla_all <- bind_rows(
  read_chla_file(files_chla$AB, "Algoa Bay"),
  read_chla_file(files_chla$SHB, "St Helena Bay"),
  read_chla_file(files_chla$WB, "Walker Bay")
)


bay_extents <- list(
  "Algoa Bay" = list(
    xlim = c(25.6, 26.4), ylim = c(-34, -33.5),
    xbreaks = seq(25, 26.4, 0.1), ybreaks = seq(-34, -33.5, 0.1)
  ),
  "St Helena Bay" = list(
    xlim = c(17.8, 18.3), ylim = c(-32.9, -32.4),
    xbreaks = seq(17.8, 18.3, 0.1), ybreaks = seq(-32.9, -32.6, 0.1)
  ),
  "Walker Bay" = list(
    xlim = c(19.1, 19.4), ylim = c(-34.7, -34.4),
    xbreaks = seq(19.1, 19.4, 0.1), ybreaks = seq(-34.7, -34.4, 0.2)
  )
)

stations <- stations %>%
  mutate(site = case_when(
    site == "AB"  ~ "Algoa Bay",
    site == "SHB" ~ "St Helena Bay",
    site == "WB"  ~ "Walker Bay",
    TRUE ~ site
  ))


# Plots
plots <- map(names(bay_extents), function(bay) {
  ext <- bay_extents[[bay]]
  
  # bathy_sub <- bathy_df %>%
  #   filter(x >= ext$xlim[1] - 1, x <= ext$xlim[2] + 1,
  #          y >= ext$ylim[1] - 1, y <= ext$ylim[2] + 1)
  
  ggplot() +
    geom_raster(data = filter(sst_all, site == bay),
                aes(x = lon, y = lat, fill = temp)) +
    scale_fill_viridis_c(option = "turbo", name = "SST (°C)") +
    geom_sf(data = sa, fill = "grey70", color = "black") +
    # geom_contour(data = bathy_df, aes(x = x, y = y, z = z),
    #              breaks = c(-10, -30),
    #              color = "black", linetype = "dashed") +
    geom_point(data = filter(stations, site == bay),
               aes(x = longitude, y = latitude),
               shape = 21, size = 2, fill = "red", color = "black") +
    # geom_text(data = filter(stations, site == bay),
    #           aes(x = longitude, y = latitude, label = station),
    #           vjust = -1, size = 3, color = "red") +
    coord_sf(xlim = ext$xlim, ylim = ext$ylim, expand = FALSE) +
    scale_x_continuous(breaks = head(ext$xbreaks,-1)) +
    scale_y_continuous(breaks = head(ext$ybreaks,-1)) +
    facet_grid(~date) +
    labs(title = "",
         x = "", y = "") +
    theme(legend.position = "right",
          panel.grid = element_blank(), 
          legend.key.size = unit(0.8,"cm"),
          legend.text = element_text(size = 14, color = "black"),
          legend.title = element_text(size = 14, color = "black"),
          axis.text = element_text(color = "black", size = 14),
          
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          strip.text = element_text(size = 14, color = "black"),
          strip.background = element_blank())
})

plots_chla <- map(names(bay_extents), function(bay) {
  ext <- bay_extents[[bay]]
  
  # bathy_sub <- bathy_df %>%
  #   filter(x >= ext$xlim[1] - 1, x <= ext$xlim[2] + 1,
  #          y >= ext$ylim[1] - 1, y <= ext$ylim[2] + 1)
  
  ggplot() +
    geom_raster(data = filter(chla_all, site == bay),
                aes(x = lon, y = lat, fill = chla)) +
    scale_fill_viridis_c(
      option = "viridis",
      trans = "log10",                      # log scale
      name = expression(log[10]~"(Chl-a, mg m"^-3*")"),
      breaks = c(0.1, 0.3, 1, 3, 10, 30)        # nice log ticks
    ) +
    geom_sf(data = sa, fill = "grey70", color = "black") +
    # geom_contour(data = bathy_df, aes(x = x, y = y, z = z),
    #              breaks = c(-10, -30),
    #              color = "black", linetype = "dashed") +
    geom_point(data = filter(stations, site == bay),
               aes(x = longitude, y = latitude),
               shape = 21, size = 2, fill = "red", color = "black") +
    # geom_text(data = filter(stations, site == bay),
    #           aes(x = longitude, y = latitude, label = station),
    #           vjust = -1, size = 3, color = "red") +
    coord_sf(xlim = ext$xlim, ylim = ext$ylim, expand = FALSE) +
    scale_x_continuous(breaks = head(ext$xbreaks,-1)) +
    scale_y_continuous(breaks = head(ext$ybreaks,-1)) +
    facet_grid(~date) +
    labs(title = "",
         x = "", y = "") +
    theme(legend.position = "right",
          panel.grid = element_blank(), 
          legend.key.size = unit(0.8,"cm"),
          legend.text = element_text(size = 14, color = "black"),
          legend.title = element_text(size = 14, color = "black"),
          axis.text = element_text(color = "black", size = 14),
          axis.text.x = element_text(angle = 90),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          strip.text = element_text(size = 14, color = "black"),
          strip.background = element_blank())
})

(plots[[1]] /  plots_chla[[1]] ) +
  plot_layout(axes = "collect", guides = "collect")


# ggsave("plots/AB_bioscape_map.png",
#        width = 12, height = 8, dpi = 300, units = "in")


(plots[[2]] +  plots_chla[[2]] ) +
  plot_layout(ncol = 1, nrow = 2, 
              axes = "collect", 
              guides = "collect")


# ggsave("plots/SHB_bioscape_map.png",
#        width = 12, height = 8, dpi = 300, units = "in")


(plots[[3]] +  plots_chla[[3]] ) +
  plot_layout(ncol = 1, nrow = 2, 
              axes = "collect",
              guides = "collect")

# ggsave("plots/WB_bioscape_map.png",
#        width = 12, height = 8, dpi = 300, units = "in")


####