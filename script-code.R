library(tidyverse)
library(readxl)
library(htmlTable)
library(lubridate)
library(ggvenn)
library(patchwork)
library(vegan)
library(oce)
library(ggrepel)
library(purrr)
library(grateful)
library(broom)
library(FSA)


#
# phyto data function--------------------------------------------------------------
lookup_table <- tibble::tibble(
  taxon = c(
    "Amphora", "Asterionellopsis", "Bacillaria", "Bacteriastrum", "Bellerochae", "Biddulphia",
    "centric", "Cerataulina", "C.debilis", "C.decipiens", "C.curvistus", "C.radicans", "C.didymus",
    "C.radiatus", "C.socialis", "C.lauderi", "Chaetoceros", "C.diadema", "C.densus", "C.simplex","C.danicus",
    "C.teres", "C.lorenzianus", "C.eibenii", "C.criophilus", "Chaetognath", "Ciliate",
    "C.frauenfeldianum", "Copepod nauplii", "Corethron", "Coscinodiscus", "C.wailesii",
    "C.granii", "C.coninnus", "C.asteromphalus", "C.closterium", "Cyst", "D.surirella",
    "Detonula", "Dinoflagellate", "Dinophysis", "D.caudata", "Ditylum", "Eucampia", "E.turris",
    "Flagellate", "Guinardia", "Gymnodinium", "P.bipes", "Helicotheca", "Hemiaulus",
    "Heterocapsa", "Lauderia", "Leptocylindrus", "L.minimus", "L.danicus", "G.polyedra",
    "Lithodesmium", "Melosira", "Navicula", "N.scintillans", "Odontella", "Pennate",
    "Planktoniella", "Pleurosigma", "Proboscia", "Prorocentrum", "P.ovatum", "P.divergens",
    "Protoperidinium", "P.depressum", "P.grande", "P.pellucidum", "P.claudicans",
    "P.minutum", "P.pallidum", "P.denticulatum", "P.conicum", "P.conicoides", "P.steinii",
    "P.elegans", "P.nitzschia", "P.pseudonoctiluca", "P.fusiformis", "R.clevei",
    "Rhizosolenia", "Scrippsiella", "Silicoflagellate", "Skeletonema", "S.palmeriana",
    "Stephanopyxis", "Thalassionema", "T.anguste-lineata", "T.rotula", "Thalassiosira",
    "T.constricta", "T.subtilis", "Tintinnid", "Triceratium", "Trieres",
    "T.furca", "T.fusus", "Tripos", "T.lineatum", "T.trichoceros", "T.symmetricus",
    "T.tripos", "T.horridum", "T.massiliense", "T.longirostrum", "T.longpipes",
    "T.macroceros", "T.candelabrum", "T.pentagonus", "T.platycorne",
    "D.tripos", "Zooplankton", 
    "Gonyaulax","Pyrocystis","Cylindrotheca","Noctiluca","Pseudo-nitzschia","Eupyxidicula",
    "Probosica", "C.cf.compressus", "D.brightwellii","E.zondicus", "T.frauenfeldii", "A.glacialis",
    "C.contortus", "C.pelagica", "C.protuberans","C.peruvianus","D.pumila","Asteroplanus",
    "P.horologium", "D.lenticula", "G.spirale", "Katodinium", "P.pyriforme",
    "D.norvegica","Pyrophacus", "P.reticulatum", "Actinocyclus","Cryptophaceae", "D.fragilissimus",
    "L.undulatum","Porosira", "C.affinis", "Alexandrium", "A.ostenfeldii","G.undulans",
    "Dictyocha","P.decipiens", "Asteromphalus","Surirella","M.rubrum",
    NA
  ),
  genus = c(
    "Amphora", "Asterionellopsis", "Bacillaria", "Bacteriastrum", "Bellerochae", "Biddulphia",
    "centric", "Cerataulina", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros",
    "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros","Chaetoceros",
    "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetoceros", "Chaetognath", "Ciliate",
    "Chaetoceros", "Copepod", "Corethron", "Coscinodiscus", "Coscinodiscus",
    "Coscinodiscus", "Coscinodiscus", "Coscinodiscus", "Cylindrotheca", "Cyst", "Delphineis",
    "Detonula", "Dinoflagellate", "Dinophysis", "Dinophysis", "Ditylum", "Eucampia", "Eupyxidicula",
    "Flagellate", "Guinardia", "Gymnodinium", "Protoperidinium", "Helicotheca", "Hemiaulus",
    "Heterocapsa", "Lauderia", "Leptocylindrus", "Leptocylindrus", "Leptocylindrus","Gonyaulax",
    "Lithodesmium", "Melosira", "Navicula", "Noctiluca", "Odontella", "Pennate",
    "Planktoniella", "Pleurosigma", "Proboscia", "Prorocentrum", "Protoperidinium", "Protoperidinium",
    "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium",
    "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium", "Protoperidinium","Protoperidinium",
    "Protoperidinium", "Pseudo-nitzschia", "Pyrocystis", "Pyrocystis", "Rhizosolenia",
    "Rhizosolenia", "Scrippsiella", "Silicoflagellate", "Skeletonema", "Skeletonema",
    "Stephanopyxis", "Thalassionema", "Thalassiosira", "Thalassiosira", "Thalassiosira",
    "Thalassiosira", "Thalassiosira", "Tintinnid", "Triceratium", "Trieres",
    "Tripos", "Tripos", "Tripos", "Tripos", "Tripos", "Tripos",
    "Tripos", "Tripos", "Tripos", "Tripos", "Tripos",
    "Tripos", "Tripos", "Tripos", "Tripos",
    "Dinophysis", "Zooplankton", 
    "Gonyaulax","Pyrocystis","Cylindrotheca","Noctiluca","Pseudo-nitzschia","Eupyxidicula",
    "Probosica","Chaetoceros", "Ditylum", "Eucampia","Thalassionema", "Asterionellopsis",
    "Chaetoceros","Cerataulina", "Chaetoceros","Chaetoceros","Detonula","Asteroplanus",
    "Pyrophacus", "Diplopsalis", "Gyrodinium", "Katodinium", "Protoperidinium",
    "Dinophysis", "Pyrophacus", "Protoceratium","Actinocyclus", "Cryptophaceae", "Dactyliosolen",
    "Lithodesmium", "Porosira","Chaetoceros", "Alexandrium", "Alexandrium","Gyrodinium",
    "Dictyocha","Protoperidinium","Asteromphalus","Surirella","Mesodinium",
    NA
  ),
  functional_type =c(
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom", "Diatom","Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Zooplankton", "Ciliate",
    "Diatom", "Zooplankton", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Cyst", "Diatom",
    "Diatom", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Diatom", "Diatom", "Diatom",
    "Flagellate", "Diatom", "Dinoflagellate", "Dinoflagellate", "Diatom", "Diatom",
    "Dinoflagellate", "Diatom", "Diatom", "Diatom", "Diatom","Dinoflagellate",
    "Diatom", "Diatom", "Diatom", "Dinoflagellate", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate","Dinoflagellate",
    "Dinoflagellate", "Diatom", "Dinoflagellate", "Dinoflagellate", "Diatom",
    "Diatom", "Dinoflagellate", "Silicoflagellate", "Diatom", "Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom",
    "Diatom", "Diatom", "Zooplankton", "Diatom", "Diatom",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Dinoflagellate",
    "Dinoflagellate", "Zooplankton", 
    "Dinoflagellate","Dinoflagellate","Diatom","Dinoflagellate","Diatom","Diatom",
    "Diatom", "Diatom", "Diatom", "Diatom", "Diatom","Diatom",
    "Diatom","Diatom","Diatom", "Diatom","Diatom","Diatom",
    "Dinoflagellate","Dinoflagellate", "Dinoflagellate", "Dinoflagellate","Dinoflagellate",
    "Dinoflagellate", "Dinoflagellate", "Dinoflagellate", "Diatom","Flagellate","Diatom",
    "Diatom", "Diatom","Diatom","Dinoflagellate","Dinoflagellate","Dinoflagellate",
    "Silicoflagellate","Dinoflagellate","Diatom","Diatom","Ciliate",
    NA
  )
)

add_taxonomic_info <- function(data, lookup_table) {
  data %>%
    left_join(lookup_table, by = "taxon") %>% 
    mutate(
      genus = ifelse(is.na(genus), "Unknown", genus),
      functional_type = ifelse(is.na(functional_type), "Unknown", functional_type)
    )
}





# Labels ------------------------------------------------------------------
custom_labels <- c(
  "2023-10-25" = "AB Day 1",
  "2023-10-26" = "AB Day 2",
  "2023-11-15" = "AB Day 3",
  "2023-10-30" = "SHB Day 1",
  "2023-10-31" = "SHB Day 2",
  "2023-11-08" = "WB Day 1",
  "2023-11-09" = "WB Day 2"
)

ordered_labels <- c(
  "AB Day 1", "AB Day 2", "AB Day 3",
  "WB Day 1", "WB Day 2",
  "SHB Day 1", "SHB Day 2"
)

custom_labels2 <- c(
  "2023-10-25" = "Day 1",
  "2023-10-26" = "Day 2",
  "2023-11-15" = "Day 3",
  "2023-10-30" = "Day 1",
  "2023-10-31" = "Day 2",
  "2023-11-08" = "Day 1",
  "2023-11-09" = "Day 2"
)

##

# general theme------
gtheme <- theme_bw() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_text(size = 16),
    strip.background = element_blank(),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.placement = "outside")
##
#  Phyto -----
phyto_raw <-  read_csv("data/Phyto_BioSCape-JGR_microcsope_raw.csv")

phyto_data <- phyto_raw %>% 
  mutate(taxon = case_when(
    taxon == "Centric" ~ "centric", 
    TRUE ~ taxon)) %>% 
  group_by(site,station,date_sampled, tow_size,tow_type,taxon) %>% 
  ###For the calculation it is reported as cells per liter
  reframe(abs_abun = case_when(
    tow_type == "U" ~ ((total_cells  * area_chamber) / area_counted * (vol_collected/sample_vol)),
    tow_type == "H" ~ ((total_cells * sample_vol * area_chamber) / area_counted * (vol_collected)) / dilution,
    tow_type == "V" ~ ((total_cells * sample_vol * area_chamber) / area_counted * (vol_collected)) / dilution)
  ) %>% 
  left_join(phyto_raw %>% 
              select(date_sampled,site, station, tow_size, tow_type, taxon, total_cells), 
            by = join_by("date_sampled","site", "station", "tow_size", "tow_type","taxon")) %>% 
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels),
         day_label = factor(day_label, levels = ordered_labels)) %>% 
  add_taxonomic_info(lookup_table)



phyto_data %>%
  mutate(functional_type = case_when(
    functional_type %in% c("Cyst", "Zooplankton") ~ "Other",
    TRUE ~ functional_type
  )) %>%
  group_by(site) %>%
  summarise(taxa_list = paste(sort(unique(taxon)), collapse = ", ")) %>%
  ungroup() ->taxa_by_site_collapsed


write.csv(taxa_by_site_collapsed, "taxa_by_site.csv", row.names = FALSE)


ggplot(phyto_data %>% 
         mutate(functional_type = case_when(
           functional_type %in% c("Cyst", "Zooplankton") ~ "Other",
           TRUE ~ functional_type
         )) %>%
         # filter(!functional_type %in% c("Diatom", "Dinoflagellate")) %>% 
         mutate(log_abun = log10(abs_abun + 1),
                date_sampled= factor(date_sampled),
                genus = factor(genus)),
       # group_by(taxon, site, functional_type) %>%
       # summarise(total_abun = sum(abs_abun, na.rm = TRUE), .groups = "drop") %>%
       # mutate(log_abun = log10(total_abun + 1))  ,
       aes(x = log_abun, y = fct_rev(genus), 
           fill = functional_type, group = interaction(date_sampled,genus))) +
  geom_boxplot() +
  # geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~day_label, ncol = 2) +
  labs(
    title = "",
    x = expression(paste(log[10]~"(Absolute Abundance)")),
    y = "",
    fill = ""
  ) +
  gtheme +
  coord_flip() +
  theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.2),
        axis.text = element_text(color = "black"),
        legend.position = "top")

# ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/phyto_absabun.jpg",width = 18, height = 14, dpi = 300)

#End phyto


#  CTD ---------------------------------------------------------------------
ctd_raw <- read_csv("data/BioSCape-JGR_CTD_raw.csv") %>%  select(-1)

str(ctd_raw)
unique(ctd_raw$date)

ggplot(data = ctd_raw,
       aes(x = longitude, y = latitude)) + 
  geom_point() +
  geom_text(aes(label = station),  nudge_x = 0.5) +
  facet_wrap(~date, scales = "free")

#temp
ggplot(ctd_raw %>% 
         filter(depth > 2,
                date != "2023-11-15") %>% 
         mutate(station = factor(station),
                site = case_when(
                  site == "AlgoaBay" ~ "AB",
                  site == "St Helena Bay" ~ "SHB",
                  site == "Walker Bay" ~ "WB",
                  TRUE ~ site
                ),
                day_label = recode(as.character(date), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)),
       aes(x = station, y = depth)) +
  geom_point(aes(color = temperature), size = 3) +
  geom_path(aes(color = temperature, group = station)) +
  facet_grid(~day_label, scales = "free_x", 
             labeller = labeller(date = custom_labels)) +
  labs(x = "", y = "Depth (m)", color = "Temp.(°C)") +
  scale_y_reverse() +  
  scale_color_viridis_c(option = "H") +
  gtheme +
  ggtitle("a - temperature") ->  ctdtempplot


ctd_raw %>% 
  filter(depth >2) %>% 
  group_by(site) %>% 
  reframe(range_temp = range(temperature, na.rm = TRUE))

ctd_raw %>%
  filter(depth >2) %>% 
  group_by(site,date) %>%
  reframe(
    mean_temp = mean(temperature, na.rm = TRUE),
    sd_temp   = sd(temperature, na.rm = TRUE),
    n         = n())


site_diffs <- ctd_raw %>%
  filter(depth >2) %>% 
  group_by(site) %>%
  do({
    # Run one-way ANOVA per site
    model <- aov(temperature ~ date, data = .)
    tidy(model)})

site_diffs

#salininty
ggplot(ctd_raw %>% 
         filter(depth > 2,
                date != "2023-11-15") %>% 
         mutate(station = factor(station),
                site = case_when(
                  site == "AlgoaBay" ~ "AB",
                  site == "St Helena Bay" ~ "SHB",
                  site == "Walker Bay" ~ "WB",
                  TRUE ~ site
                ),
                day_label = recode(as.character(date), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)),
       aes(x = station, y = depth)) +
  geom_point(aes(color = salinity), size = 3) +
  geom_path(aes(color = salinity, group = station)) +
  facet_grid(~day_label, scales = "free_x", 
             labeller = labeller(date = custom_labels)) +
  labs(x = "", y = "Depth (m)", color = "Sali.(PSU)") +
  scale_y_reverse() +  
  scale_color_viridis_c(option = "D") + 
  gtheme +
  ggtitle("c - salinity") -> ctdsaliplot


ctd_raw %>% 
  filter(depth >2) %>% 
  group_by(site) %>% 
  reframe(range_sali = range(salinity, na.rm = TRUE))


ctd_raw %>% 
  filter(depth >2, 
         site == "WB") %>% 
  group_by(site, depth) %>% 
  reframe(range_sali = range(salinity, na.rm = TRUE)) %>% 
  arrange(desc(depth))

site_diffs <- ctd_raw %>%
  filter(depth >2) %>% 
  group_by(site) %>%
  do({
    # Run one-way ANOVA per site
    model <- aov(salinity ~ date, data = .)
    tidy(model)})

site_diffs

#oxygen
ggplot(ctd_raw %>% 
         filter(depth > 2) %>% 
         mutate(station = factor(station),
                site = case_when(
                  site == "AlgoaBay" ~ "AB",
                  site == "St Helena Bay" ~ "SHB",
                  site == "Walker Bay" ~ "WB",
                  TRUE ~ site
                ),
                day_label = recode(as.character(date), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)),
       aes(x = station, y = depth)) +
  geom_point(aes(color = oxygen), size = 3) +
  geom_path(aes(color = oxygen, group = station)) +
  facet_grid(~day_label, scales = "free_x", 
             labeller = labeller(date = custom_labels)) +
  labs(x = "", y = "Depth (m)", color = "DO(mg/L)") +
  scale_y_reverse() +  
  scale_color_viridis_c(option = "B") +
  gtheme +
  ggtitle("d - oxygen") -> ctdoxyplot



ctd_raw %>% 
  filter(depth >2) %>% 
  group_by(site) %>% 
  reframe(range_ox = range(oxygen, na.rm = TRUE))


ggplot(ctd_raw %>% 
         filter(depth > 2, 
                date != "2023-11-15") %>% 
         mutate(station = factor(station),
                site = case_when(
                  site == "AlgoaBay" ~ "AB",
                  site == "St Helena Bay" ~ "SHB",
                  site == "Walker Bay" ~ "WB",
                  TRUE ~ site
                ),
                day_label = recode(as.character(date), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)),
       aes(x = station, y = depth)) +
  geom_point(aes(color = pH), size = 3) +
  geom_path(aes(color = pH, group = station)) +
  facet_grid(~day_label, scales = "free_x", 
             labeller = labeller(date = custom_labels)) +
  labs(x = "", y = "Depth (m)", color = "pH") +
  scale_y_reverse() +  
  scale_color_viridis_c(option = "E") +
  gtheme +
  ggtitle("b - pH") -> ctdphplot

ctd_raw %>% 
  filter(depth > 2) %>% 
  group_by(site,date) %>% 
  reframe(range_pH = range(pH, na.rm = TRUE))


ggplot(ctd_raw %>% 
         filter(depth > 2,
                depth <= max(depth, na.rm = TRUE) - 2,
                date != "2023-11-15") %>% 
         mutate(station = factor(station),
                site = case_when(
                  site == "AlgoaBay" ~ "AB",
                  site == "St Helena Bay" ~ "SHB",
                  site == "Walker Bay" ~ "WB",
                  TRUE ~ site
                ),
                day_label = recode(as.character(date), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)),
       aes(x = station, y = depth)) +
  geom_point(aes(color = turbidity), size = 3) +
  geom_path(aes(color = turbidity, group = station)) +
  facet_grid(~day_label, scales = "free_x", 
             labeller = labeller(date = custom_labels)) +
  labs(x = "station", y = "depth", color = "NTU") +
  scale_y_reverse() +  
  scale_color_viridis_c(option = "H") +
  gtheme +
  ggtitle("e - turbidity")


# Create T and S grid
T_vals <- seq(14, 25, 0.5)       # temperature range
S_vals <- seq(34, 36, 0.05)      # salinity range
grid <- expand.grid(temperature = T_vals, salinity = S_vals)

# Compute density (at surface pressure, p = 0 dbar)
grid$density <- swRho(salinity =  grid$salinity, temperature = grid$temperature, pressure = 0)

# Convert density to sigma-theta (σθ) by subtracting 1000
grid$sigma_theta <- grid$density - 1000

tsplotdata <- ctd_raw %>%
  filter(
    salinity > 30, pH > 5, pH < 9,
    !is.na(depth), !is.na(temperature),
    !is.na(latitude), !is.na(longitude)
  ) %>%
  group_by(site, station, date) %>%
  arrange(depth) %>%
  mutate(
    day_label = recode(as.character(date), !!!custom_labels),
    day_label = factor(day_label, levels = ordered_labels),
    pressure = gsw_p_from_z(z = -depth, lat = latitude),
    SA = gsw_SA_from_SP(SP = salinity, p = pressure,
                        longitude = longitude, latitude = latitude),
    CT = gsw_CT_from_t(SA = SA, t = temperature, p = pressure),
    sigma0 = gsw_sigma0(SA = SA, CT = CT),
    # compute vertical density gradient
    dens_grad = c(NA, diff(sigma0) / diff(depth))
  ) %>%
  ungroup()

turb_summary <- tsplotdata %>%
  group_by(site, day_label) %>%
  summarise(
    mean_grad = mean(abs(dens_grad), na.rm = TRUE),
    max_grad = max(abs(dens_grad), na.rm = TRUE),
    .groups = "drop"
  )

ggplot(tsplotdata, aes(x = sigma0, y = depth, color = day_label)) +
  geom_path(aes(group = interaction(site, station, date)), linewidth = 1) +
  scale_y_reverse() +
  scale_color_brewer(palette = "Set2") +
  labs(
    x = expression(paste("Potential Density Anomaly (", sigma[theta], " kg m"^-3, ")")),
    y = "Depth (m)",
    color = "",
    title = ""
  ) +
  gtheme +
  ggtitle("e - vertical density structure") -> tsplot


ctdplot <- ((ctdtempplot +ctdphplot + ctdsaliplot + ctdoxyplot) / tsplot ) +
  plot_layout(axes = "collect")


ctdplot

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/ctdplots_no-interpolation.jpg",
       width = 19, height = 22, dpi = 600)


##
#  Nuts ------
nuts_raw <- read_csv("data/bioSCape-JGR_nuts_reps.csv")

nuts_data <- nuts_raw %>% 
  group_by(date_sampled, site, station, depth) %>% 
  reframe(ave_PO4 = mean(PO4),
          ave_Si = mean(Si),
          ave_NOx = mean(NOx), 
          ave_NO2 = mean(NO2), 
          ave_NH4 = mean(NH4),
          sd_PO4 = sd(PO4), 
          sd_Si = sd(Si), 
          sd_NOx = sd(NOx), 
          sd_NO2 = sd(NO2), 
          sd_NH4 = sd(NH4))

# Step 1: Pivot the average columns
avg_long <- nuts_data %>%
  pivot_longer(cols = starts_with("ave_"),
               names_to = "nutrient",
               names_prefix = "ave_",
               values_to = "average") %>% 
  select(date_sampled, site, station , depth , nutrient, average)

# Step 2: Pivot the standard deviation columns
sd_long <- nuts_data %>%
  pivot_longer(cols = starts_with("sd_"),
               names_to = "nutrient",
               names_prefix = "sd_",
               values_to = "sd") %>% 
  select(date_sampled, site, station , depth , nutrient, sd)


nuts_long <- left_join(avg_long, sd_long,
                       by = c("date_sampled", "site", "station", "depth", "nutrient")) %>% 
  filter(date_sampled != as.Date("2023-11-15"))


nut_diff_kw <- nuts_long %>%
  group_by(site, nutrient) %>%
  do({
    test <- kruskal.test(average ~ date_sampled, data = .)
    tidy(test)
  }) %>%
  ungroup() %>%
  mutate(
    # Assign significance levels based on p-value thresholds
    sig_level = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(site, nutrient, statistic, p.value, sig_level)

nut_diff_kw


nut_diff_kw <- nuts_long %>%
  group_by(nutrient) %>%           # group by nutrient and depth
  do({
    test <- kruskal.test(average ~ site, data = .)   # compare sites
    tidy(test)
  }) %>%
  ungroup() %>%
  mutate(
    # Add significance levels based on p-values
    sig_level = case_when(
      p.value <= 0.001 ~ "***",
      p.value <= 0.01  ~ "**",
      p.value <= 0.05  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(nutrient, statistic, p.value, sig_level)

nut_diff_kw

nut_posthoc <- nuts_long %>%
  group_by(nutrient) %>%
  do({
    dunn <- dunnTest(average ~ site, data = ., method = "bh")$res
    as_tibble(dunn)
  }) %>%
  ungroup() %>% 
  mutate(
    # make readable site pairs (e.g., "AB–WB")
    Comparison = gsub(" - ", " vs ", Comparison),
    
    # add significance stars based on adjusted p-value
    sig_level = case_when(
      P.adj <= 0.001 ~ "***",
      P.adj <= 0.01  ~ "**",
      P.adj <= 0.05  ~ "*",
      TRUE           ~ "ns"
    )
  ) %>%
  select(nutrient, Comparison, Z, P.unadj, P.adj, sig_level)

nut_posthoc

ggplot(nuts_long %>% 
         mutate(day_label = recode(as.character(date_sampled), !!!custom_labels),
                day_label = factor(day_label, levels = ordered_labels)) %>% 
         mutate(date_sampled = as.Date(date_sampled),
                site = factor(site, levels = c("AlgoaBay","Walker Bay","St Helena Bay"))) %>% 
         filter(depth != 60),
       aes(x = day_label, y = average, fill = site)) +
  geom_boxplot(outlier.shape = 21, alpha = 0.7) +
  facet_wrap(depth~nutrient, scales = "free_y", ncol = 5,
             labeller = labeller(depth = c("0" = "0 m",
                                           "15" = "15 m",
                                           "30" = "30 m"))) +
  scale_fill_viridis_d(option = "H") +
  labs(x = "", y = "Concentration (µM)", fill = "Site") +
  theme(
    axis.text.x = element_text(size = 16, angle = 90,vjust=0.5,hjust=0.5, color = "black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_text(size = 16),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.placement = "outside")

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/nuts-plots.jpg",width = 18, height = 14, dpi = 600)


###
#  ordination -------------------------------------------------------------
ord_phyto_data <- phyto_data %>%
  filter(functional_type != "Zooplankton") %>% 
  mutate(sample_id = paste(site, station, date_sampled, tow_size, tow_type, sep = "_"))

# Pivot to wide: taxa as columns, abundance as values
comm_matrix <- ord_phyto_data %>%
  select(sample_id, taxon, abs_abun) %>%
  group_by(sample_id, taxon) %>%
  summarise(abundance = sum(abs_abun), .groups = "drop") %>%
  pivot_wider(names_from = taxon, values_from = abundance, values_fill = 0)

# Extract metadata
metadata <- comm_matrix %>%
  select(sample_id) %>%
  separate(sample_id, into = c("site","station","date_sampled","tow_size", "tow_type"), sep = "_") %>%
  mutate(date_sampled = as.Date(date_sampled)) %>% 
  mutate(sample_id = paste(site,station, date_sampled,tow_size, tow_type, sep = "_")) %>% 
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels))

# Community matrix only (drop metadata)
comm <- comm_matrix %>%
  select(-sample_id) %>%
  as.data.frame()
row.names(comm) <- comm_matrix$sample_id

comm <- comm[rowSums(comm) > 0, ]

set.seed(123)
nmds <- metaMDS(comm, distance = "bray", k = 2, trymax = 100)

# Extract site scores (samples)
sites_df <- as.data.frame(scores(nmds, display = "sites"))
sites_df$sample_id <- rownames(sites_df)
sites_df <- left_join(sites_df, metadata, by = "sample_id")

# Extract species scores (taxa)
species_df <- as.data.frame(scores(nmds, display = "species"))
species_df$taxon <- rownames(species_df)

hull_data <- sites_df %>%
  group_by(site) %>%
  slice(chull(NMDS1, NMDS2))

nmds$stress

ggplot() +
  # Hulls for each site
  stat_ellipse(data = sites_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  # Site points
  geom_jitter(data = sites_df, 
              aes(x = NMDS1, y = NMDS2, color = site, shape = day_label),
              size = 3) +
  geom_jitter(data = species_df, 
              aes(x = NMDS1, y = NMDS2), color = "black") +
  geom_text_repel(data = species_df, 
                  aes(x = NMDS1, y = NMDS2,label = taxon),
                  size = 4, max.overlaps = 50) +
  scale_color_manual(values =  c("gold","steelblue","firebrick")) +
  labs(title = "",
       subtitle = "",
       shape ="", color = "",
       x = "NMDS1", y = "NMDS2") +
  theme(
    axis.text.x = element_text(size = 16, angle = 0,vjust=0.5,hjust=0.5, color = "black"),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.placement = "outside")

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/nmds.jpg",
       width = 18, height = 14, dpi = 300)



adonis2(comm ~ site * date_sampled, data = metadata, method = "bray")


##

#  Renyi indicies  ---------------------------------------------------------
# 1. Build community matrix including tow_type
phyto_matrix <- phyto_data %>%
  filter(functional_type != "Zooplankton") %>% 
  unite(sample_id, site, station,tow_size, tow_type, date_sampled, remove = FALSE) %>%
  select(sample_id, site, station, date_sampled, tow_size, tow_type, taxon, abs_abun) %>%
  group_by(sample_id, site, station, date_sampled,tow_size, tow_type, taxon) %>%
  summarise(abs_abun = sum(abs_abun), .groups = "drop") %>%
  pivot_wider(
    names_from = taxon,
    values_from = abs_abun,
    values_fill = 0
  )

# Define alpha values
alpha_vals <- c(0, 0.25, 0.5, 1, 2, 4, 8)

# Function to compute Renyi profile
compute_renyi <- function(abun_vector, alpha_vals) {
  renyi_values <- renyi(abun_vector, scales = alpha_vals, hill = TRUE)
  data.frame(alpha = alpha_vals, renyi = renyi_values)
}

# 2. Compute Renyi diversity per sample_id
renyi_list <- phyto_matrix %>%
  group_split() %>%   # split tibble into list of samples
  lapply(function(df) {
    mat <- df %>% select(where(is.numeric)) %>% as.matrix()
    if (ncol(mat) == 0) return(NULL)
    
    purrr::map_dfr(1:nrow(mat), function(i) {
      row_vec <- mat[i, ]
      meta <- df[i, c("sample_id", "site", "station", 
                      "tow_size","tow_type","date_sampled")]
      
      renyi_out <- compute_renyi(row_vec, alpha_vals)
      cbind(renyi_out, meta)
    })
  })

renyi_df <- bind_rows(renyi_list)

# 3. Summarise (mean + sd) per site × tow_type × alpha
renyi_summary <- renyi_df %>%
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels2)) %>% 
  group_by(site, day_label, alpha) %>%
  summarise(
    mean_renyi = mean(renyi),
    sd_renyi   = sd(renyi),
    .groups = "drop"
  )

# 4. Plot: facetted by site and tow_type
ggplot(renyi_summary %>% 
         mutate(site = factor(site, levels = c("AB", "WB", "SHB"))),
       aes(x = alpha, y = mean_renyi, 
           group = day_label)) +
  geom_line(aes(linetype= day_label, color = day_label), size = 1) +
  geom_ribbon(aes(ymin = mean_renyi - sd_renyi,
                  ymax = mean_renyi + sd_renyi,
                  fill = day_label),
              alpha = 0.2, color = NA) +
  facet_wrap(~ site,
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay",
                                          "WB" = "Walker Bay"))) +
  scale_x_continuous(breaks = alpha_vals) +
  scale_color_manual(values =  c("steelblue",
                                  "firebrick")) +
  scale_fill_manual(values =  c( "steelblue",
                                 "firebrick")) +
  labs(
    title = "",
    x = "Renyi alpha (α)",
    y = "Diversity (Hill numbers)",
    color = "",
    linetype  = ""
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 16),
    legend.position = "top"
  )

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/renyi.jpg",
       width = 18, height = 14, dpi = 600)

##

#  fine scale spatial-temporal -----------------------------------------------------
# Prepare community matrix (site-station-day as rows, taxa as columns)
comm_data <- phyto_data %>%
  filter(functional_type != "Zooplankton") %>% 
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels2)) %>% 
  group_by(site, station,tow_size, tow_type,date_sampled,day_label, taxon) %>%
  summarise(abundance = sum(abs_abun), .groups = "drop") %>%
  pivot_wider(names_from = taxon, values_from = abundance, values_fill = 0)

# Ensure comm is a matrix with rownames
comm_mat <- comm_data %>%
  unite("sample_id", site, station,tow_size,tow_type,date_sampled, day_label, remove = FALSE) %>%
  column_to_rownames("sample_id") %>%
  select(-site, -station,-date_sampled,-tow_type, -day_label)

metadata <- comm_data %>%
  unite("sample_id", site, station,tow_size,tow_type,date_sampled, day_label, remove = FALSE) %>%
  select(sample_id, site, station,tow_size,tow_type,date_sampled, day_label)

# Run NMDS
nmds <- metaMDS(comm_mat, distance = "bray", k = 2, trymax = 100)

# Extract scores and align with metadata
scores_nmds <- as.data.frame(scores(nmds, display = "sites"))
scores_nmds$sample_id <- rownames(scores_nmds)

# Merge with metadata
scores_nmds <- left_join(scores_nmds, metadata, by = "sample_id")


ggplot(scores_nmds %>% 
         mutate(station = as.character(station)), 
       aes(x = NMDS1, y = NMDS2, 
           color = day_label, shape = station)) +
  geom_point(size = 3) +
  guides(shape = "none") +
  theme_minimal() +
  facet_wrap(~ site,
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay", 
                                          "WB" = "Walker Bay"))) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 12),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    strip.placement = "outside")


adonis_results <- adonis2(comm_mat ~ site * station* day_label, data = metadata, method = "bray", permutations = 999)
print(adonis_results)

# Within-bay test
adonis_by_bay <- metadata %>%
  group_split(site) %>%
  map(~ adonis2(comm_mat[metadata$site == .x$site[1], ] ~ day_label * station, 
                data = .x, method = "bray", permutations = 999))

adonis_by_bay


dist_mat <- vegdist(comm_mat, method = "bray")
dispersion <- betadisper(dist_mat, metadata$day_label)
anova(dispersion)


# 1. Standardize site names
site_lookup <- tibble(
  site_phyto = c("AB", "SHB", "WB"),
  site_env   = c("AlgoaBay", "StHelenaBay", "WalkerBay")
)

# Apply to nuts_data and chla_data
nuts_clean <- nuts_data %>%
  left_join(site_lookup, by = c("site" = "site_env")) %>%
  rename(siteName = site_phyto)

# chla_clean <- chla_data %>%
#   left_join(site_lookup, by = c("site" = "site_env")) %>%
#   rename(siteName = site_phyto)

# 2. Standardize station IDs
# phyto_data has "St1", "St2" etc, convert to numeric
metadata <- metadata %>%
  mutate(station = as.numeric(str_remove(station, "St")))

# Nutrients at surface (depth = 0)
nuts_surface <- nuts_clean %>%
  filter(depth == 0) %>%
  group_by(site, station, date_sampled) %>%
  summarise(across(starts_with("ave_"), mean, na.rm = TRUE), .groups = "drop")

# CTD at surface
ctd_surface <- ctd_raw %>%
  filter(depth < 1) %>%  # take shallowest values as "surface"
  group_by(site, station, date) %>%
  summarise(across(c(temperature, salinity, oxygen, pH, turbidity, fluorescence),
                   mean, na.rm = TRUE), .groups = "drop") %>%
  rename(date_sampled = date)

# Merge all env data
env_data <- nuts_surface %>%
  # full_join(chla_surface, by = c("site", "station", "date_sampled")) %>%
  # mutate(date_sampled = as.Date(date_sampled)) %>% 
  left_join(site_lookup, by = c("site" = "site_env")) %>%
  select(!site) %>% 
  rename("site" = "site_phyto") %>% 
  full_join(ctd_surface, by = c("site", "station", "date_sampled")) %>% 
  filter(date_sampled != "2023-11-15") %>% 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

merged <- metadata %>%
  left_join(env_data, by = c("site", "station", "date_sampled"))

unique(merged$date_sampled)

dbrda_model <- capscale(comm_mat ~ ave_PO4 + ave_Si + ave_NOx + ave_NH4  +
                          temperature + salinity  + oxygen + pH + turbidity, 
                        data = merged, distance = "bray")

anova(dbrda_model, by = "terms", permutations = 999)
# plot(dbrda_model)  

scores_dbrda <- scores(dbrda_model, display = "sites") %>%
  as.data.frame() %>%
  bind_cols(merged)

# Function: run dbRDA per site
run_dbrda <- function(site_name) {
  
  # Subset data
  comm_sub <- comm_mat[merged$site == site_name, ]
  meta_sub <- merged %>% filter(site == site_name)
  
  # Run dbRDA
  dbrda_sub <- capscale(comm_sub ~ ave_PO4 + ave_Si + ave_NOx + ave_NH4 +
                          temperature + salinity + oxygen + pH + turbidity,
                        data = meta_sub, distance = "bray")
  
  # Extract variance explained (% per axis)
  var_exp <- summary(dbrda_sub)$cont$importance
  axis_labels <- c(
    paste0("dbRDA1 (", round(var_exp[2,1] * 100, 1), "%)"),
    paste0("dbRDA2 (", round(var_exp[2,2] * 100, 1), "%)")
  )
  
  # Extract adjusted R²
  adjR2 <- RsquareAdj(dbrda_sub)$adj.r.squared
  
  # Site scores
  site_scores <- scores(dbrda_sub, display = "sites") %>%
    as.data.frame() %>%
    bind_cols(meta_sub) %>%
    mutate(site = site_name,
           axis1_label = axis_labels[1],
           axis2_label = axis_labels[2],
           adjR2 = round(adjR2, 2))  # keep 2 decimals
  
  # Environmental fit
  ef <- envfit(dbrda_sub,
               meta_sub %>% 
                 select(ave_PO4, ave_Si, ave_NOx, ave_NH4,
                        temperature, salinity, oxygen, pH, turbidity),
               permutations = 999)
  
  ef_scores <- as.data.frame(scores(ef, "vectors"))
  ef_scores$var <- rownames(ef_scores)
  ef_scores$site <- site_name
  ef_scores$pval <- ef$vectors$pvals
  
  # Keep only significant vars
  ef_scores <- ef_scores %>%
    filter(pval < 0.05) %>%
    mutate(CAP1 = CAP1,
           CAP2 = CAP2,
           axis1_label = axis_labels[1],
           axis2_label = axis_labels[2],
           adjR2 = round(adjR2, 2))
  
  list(sites = site_scores, env = ef_scores)
}

# Run for all sites
results <- map(unique(merged$site), run_dbrda)

# Combine
scores_all <- bind_rows(map(results, "sites"))
ef_all     <- bind_rows(map(results, "env")) %>% 
  mutate(var = recode(var,
                      "ave_Si"  = "Si",
                      "ave_NOx" = "NOxN",
                      "ave_NH4" = "NH4",
                      "temperature"  = "temp",
                      "turbidity" = "turb",
                      "oxygen" = "DO"))


# Plot with adjusted R² in facet titles
ggplot(scores_all %>% 
         mutate(site = factor(site, levels = c("AB", "WB", "SHB"))),
       aes(x = CAP1, y = CAP2, color = day_label)) +
  geom_point(aes(shape = day_label), size = 3) +
  stat_ellipse(aes(group = day_label), linetype = 2) +
  
  # Significant environmental arrows
  geom_segment(data = ef_all %>% 
                 mutate(site = factor(site, levels = c("AB", "WB", "SHB"))), 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.25, "cm")),
               inherit.aes = FALSE, color = "black") +
  geom_text(data = ef_all %>% 
              mutate(site = factor(site, levels = c("AB", "WB", "SHB"))), 
            aes(x = CAP1, y = CAP2, label = var),
            inherit.aes = FALSE, size = 6, vjust = -0.5) +
  
  facet_wrap(~ site,
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay", 
                                          "WB" = "Walker Bay"))) +
  theme_minimal() +
  labs(
    title = "",
    x = unique(scores_all$axis1_label),
    y = unique(scores_all$axis2_label)
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.placement = "outside")

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/dbRDA_perBAy.jpg",
       width = 18, height = 14, dpi = 600)

###

##


#  bubble plot --------
# Prepare community matrix (site-station-day as rows, taxa as columns)
comm_data <- phyto_data %>%
  filter(functional_type != "Zooplankton") %>% 
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels2)) %>% 
  group_by(site, station, tow_size,tow_type,date_sampled,day_label, genus) %>%
  summarise(cell_counts = sum(total_cells), .groups = "drop") %>%
  pivot_wider(names_from = genus, values_from = cell_counts, values_fill = 0)

# Ensure comm is a matrix with rownames
comm_mat <- comm_data %>%
  unite("sample_id", site, station,tow_size,tow_type,date_sampled, day_label, remove = FALSE) %>%
  column_to_rownames("sample_id") %>%
  select(-site, -station,-date_sampled,-tow_size,-tow_type, -day_label)

metadata <- comm_data %>%
  unite("sample_id", site, station,tow_size,tow_type,date_sampled, day_label, remove = FALSE) %>%
  select(sample_id, site, station,tow_size,tow_type,date_sampled, day_label)


bubble_data <- comm_mat %>%
  as.data.frame() %>%
  rownames_to_column("sample_id") %>%
  pivot_longer(-sample_id, names_to = "taxon", values_to = "abundance") %>%
  left_join(metadata, by = "sample_id")

bubble_summary <- bubble_data %>%
  group_by(site, day_label, taxon) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE),
            .groups = "drop")

bubble_diff <- bubble_summary %>%
  pivot_wider(names_from = day_label, values_from = mean_abundance, values_fill = 0) %>%
  mutate(
    ratio = (`Day 2` + 1) / (`Day 1` + 1),   # add 1 to avoid divide-by-zero
    log_ratio = log2(ratio)                  # log2 fold-change (↑ or ↓)
  )


top_taxa <- bubble_summary %>%
  group_by(site, taxon) %>%
  summarise(total_abundance = sum(mean_abundance), .groups = "drop") %>%
  group_by(site) %>%
  # slice_max(total_abundance, n = 50) %>%
  select(site,taxon)


bubble_diff <- bubble_diff %>%
  inner_join(top_taxa, by = c("site", "taxon"))


ggplot(bubble_diff %>% 
         mutate(site = factor(site, levels = c("AB", "WB", "SHB")),
                taxon = factor (taxon)), 
       aes(x = site, y = fct_rev(taxon), size = abs(log_ratio), color = log_ratio)) +
  geom_point() +
  scale_size_continuous(range = c(1, 10)) +
  scale_color_viridis_c(option = "E",name = "Day2 vs Day1 (log2 fold-change)") +
  facet_wrap(~ site, scales = "free_x",
             labeller = labeller(site = c("AB" = "Algoa Bay",
                                          "SHB" = "St. Helena Bay", 
                                          "WB" = "Walker Bay"))) +
  labs(title = "",
       x = "",
       y = "",
       size = "Effect size (|log2 ratio|)") +
  gtheme +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey80"),
    legend.position = "right",
    axis.title = element_text(size = 16),
    legend.key = element_rect(fill = "transparent"),
    panel.grid.major.y = element_line(color = "grey75"),
    strip.background = element_blank(),
    strip.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.placement = "outside")


ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/day-to-day_phyto.jpg",
       width = 18, height = 14, dpi = 600)

##

#  margalef mandala --------------------------------------------------------
# --- 1. stratification index (ΔT = surface–bottom temperature difference)
strat_index <- ctd_raw %>%
  mutate(day_label = recode(as.character(date), !!!custom_labels)) %>% 
  filter(!is.na(salinity), !is.na(temperature), !is.na(depth),
         !is.na(latitude), !is.na(longitude),
         !(site == "WB" & station == 5)) %>%
  group_by(day_label) %>%
  arrange(depth, .by_group = TRUE) %>%
  mutate(
    pressure = gsw_p_from_z(z = -depth, lat = latitude),
    SA = gsw_SA_from_SP(SP = salinity, p = pressure, longitude = longitude, latitude = latitude),
    CT = gsw_CT_from_t(SA = SA, t = temperature, p = pressure),
    sigma0 = gsw_sigma0(SA = SA, CT = CT)
  ) %>%
  summarise(
    delta_sigma = max(sigma0, na.rm = TRUE) - min(sigma0, na.rm = TRUE),  # density difference (stratification proxy)
    mean_sigma  = mean(sigma0, na.rm = TRUE),                             # mean potential density anomaly
    .groups = "drop"
  )


# --- 2. average nutrient availability (mean surface + mid + deep)
nuts_summary <- nuts_data %>%
  mutate(day_label = recode(as.character(date_sampled), !!!custom_labels)) %>% 
  mutate(site = recode(site,
                       "AlgoaBay" = "AB",
                       "Walker Bay" = "WB",
                       "St Helena Bay" = "SHB"
  )) %>%
  group_by(day_label) %>%
  reframe(
    DIN = mean(ave_NOx+ave_NH4, na.rm = TRUE),
    mean_NOx = mean(ave_NOx, na.rm = TRUE),
    mean_NH4 = mean(ave_NH4, na.rm = TRUE),
    mean_PO4 = mean(ave_PO4, na.rm = TRUE),
    mean_Si  = mean(ave_Si,  na.rm = TRUE),
    dindip = DIN /mean_PO4,
    dipsi = mean_PO4 / mean_Si,
    dinsi = DIN / mean_Si,
  )

# --- 3. phytoplankton functional groups (diatoms vs dinoflagellates vs others)
phyto_func <- phyto_data %>%
  mutate(
    day_label = recode(as.character(date_sampled), !!!custom_labels),
    station = readr::parse_number(station)
  ) %>%
  # filter(!(site == "WB" & station == 5)) %>% 
  group_by(day_label) %>%
  mutate(
    ttax = sum(total_cells, na.rm = TRUE),             # total cell count per station/day
    rel_abun = total_cells / ttax                      # relative abundance for each taxon
  ) %>%
  ungroup() %>%
  group_by(day_label, station, taxon) %>%
  summarise(
    rel_abun = sum(rel_abun, na.rm = TRUE),            # sum in case taxa repeat
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = taxon,
    values_from = rel_abun,
    values_fill = 0
  )

mandala_data <- strat_index %>%
  left_join(nuts_summary, by = c("day_label")) %>%
  left_join(phyto_func, by = c("day_label"))

mandala_data_long <- mandala_data %>%
  pivot_longer(
    cols = 13:ncol(mandala_data),    # adjust to include all genus columns
    names_to = "taxon",
    values_to = "rel_abun"
  ) %>%
  add_taxonomic_info(lookup_table) %>% 
  filter(!functional_type %in% c("Zooplankton", "Cyst"))

#try combine on one plot with sec axis 
# Find approximate scale ratio between the two nutrient ratios
# Compute scale ratio once
ratio_data <- mandala_data_long %>%
  group_by(day_label) %>%
  summarise(
    ratio_scale = max(dindip, na.rm = TRUE) / max(dinsi, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(mandala_data_long,
       aes(x = delta_sigma, y = dindip)) +
  
  # --- Points (DIN:DIP)
  geom_jitter(
    aes(size = rel_abun, color = functional_type),
    alpha = 0.8, width = 0.45, height = 50
  ) +
  
  # --- Labels
  geom_text_repel(
    data = mandala_data_long %>%
      distinct(day_label, station, .keep_all = TRUE),
    aes(label = interaction(day_label, station)),
    size = 5, fontface = "bold", color = "black"
  ) +
  
  # --- Secondary axis (scaled locally)
  scale_y_continuous(
    name = "DIN : DIP",
    sec.axis = sec_axis(
      ~ . / mean(ratio_data$ratio_scale, na.rm = TRUE),  # average scale used for axis ticks
      name = "DIN : Si"
    )
  ) +
  
  # facet_wrap(~site) +
  scale_size(range = c(1, 10), name = "Relative abundance") +
  scale_color_brewer(palette = "Dark2", name = "Functional type") +
  labs(
    x = expression(paste("Potential Density Anomaly (", sigma[theta], " kg m"^-3, ")")),
    title = "Phytoplankton succession"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y.left  = element_text(color = "grey10", face = "bold"),
    axis.title.y.right = element_text(color = "grey30", face = "bold"),
    legend.position = "bottom"
  )




mandala_summary <- mandala_data_long %>%
  group_by(day_label, delta_sigma, dindip, dinsi, functional_type) %>%
  summarise(rel_abun = sum(rel_abun, na.rm = TRUE), .groups = "drop")

mandala_scaled <- mandala_summary %>%
  left_join(ratio_data, by = c("day_label")) %>%
  mutate(dinsi_scaled = dinsi * ratio_scale)

ggplot(mandala_scaled,
       aes(x = delta_sigma, y = dindip)) +
  geom_point(
    aes(size = rel_abun, fill = functional_type),
    shape = 21, color = "black",
    alpha = 0.9,
    position = position_jitterdodge(jitter.width  = 0.1, jitter.height = 0.4,dodge.width = 0.4)
  ) +
  geom_text_repel(
    data = mandala_scaled %>%
      distinct(day_label, .keep_all = TRUE),
    aes(label = interaction(day_label)),
    size = 5, fontface = "bold"
  ) +
  scale_fill_brewer(palette = "Dark2", name = "") +
  scale_size(range = c(2, 10), name = "Relative abundance") +
  scale_y_continuous(
    name = "DIN : DIP",
    sec.axis = sec_axis(~ . / mean(ratio_data$ratio_scale, na.rm = TRUE),
                        name = "DIN : Si")
  ) +
  labs(
    x = expression(paste("Potential Density Anomaly (", sigma[theta], " kg m"^-3, ")")),
    title = "", fill = ""
  ) +
  gtheme +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.key.size = unit(0.2,"cm")
  ) +
  guides(
    fill = guide_legend(override.aes = list(size = 8))
  )

##

ggsave("C:/Resources/Campus Work/PhD/DATA/BioSCape/figures/margalef.jpg",
       width = 18, height = 14, dpi = 300)
# citation ----------------------------------------------------------------

cite_packages(out.dir = getwd(),
              citation.style = "peerj",
              pkgs = "Session",
              out.format = "html")

