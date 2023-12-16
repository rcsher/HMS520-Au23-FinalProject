# Install necessary packages and tools for 4-parameter logistic regression (4PL) analysis
install.packages("devtools")
devtools::install_bitbucket("dittmerlab/dr4pl")

# Loading libraries, specifically tidyr, dplyr, and ggplot2 from tidyverse
library(tidyverse)
library(dr4pl)
library(stats)
library(readxl)

# Reading in the data
restrict <- read_excel("/Users/Rachel/Documents/School/Graduate/HMS520-Au23-FinalProject/RESTRICT Template_121123_AS.xlsx", sheet = 'Combined Data')

# Subset data to eliminate NA Results
patient <- restrict %>%
  filter(!is.na(Result)) %>%
  mutate(Result = as.numeric(Result),
         compound = if_else(Dilution %in% c('1.17E-6', '1.22E-8', '1.3500000000000001E-9', '1.3E-7', '3.6500000000000003E-8', '3.9099999999999999E-7', '7.3000000000000005E-8', 'Blood -RT', 'Blood +RT'), 'compound1', as.character(Dilution))) %>%
  filter(Dilution != 'Water +RT DD4')

# Format to distinguish different patients (PTA/PTB) and conditions (1/2)
patient$Rowgroup <- str_sub(patient$`Row Group`, 1, 4) # Use stringr to select string values by index since each patient sample run in 4 replicates

# Positive and negative control need to be different colors -> add group to separate
patient$Newgroup <- ifelse(patient$Dilution == 'Blood +RT', 'Blood +RT',
                             ifelse(patient$Dilution == 'Blood -RT', 'Blood -RT', 'Replicate'))

# Subset to plotting variables and clean column names
colnames(patient)
pat_graphs <- pat_graphs %>%
  rename(Replicates = `Row Group`) %>%
  rename(RFU = Result) %>%
  dplyr::select(c(Date, `Run number`, RFU, Patient, `Concentration (M)`, Replicates, Rowgroup, Well, compound, Dilution, Newgroup)) 
  # Specify dplyr to prevent confusion with MASS::select

# Creating Concentration (Molar) variable based on code provided by colleague
pat_graphs <- pat_graphs %>%
  mutate(`Concentration (M)` = case_when(
    Dilution == 'Blood -RT' ~ '1e-4',
    Dilution == 'Blood +RT' ~ '1e-11',
    Dilution %in% c('H2O -RT', 'H2O +RT') ~ NA_character_,
    TRUE ~ as.character(Dilution)
  )) %>%
  mutate(`Concentration (M)` = as.numeric(`Concentration (M)`))

# Subset into separate runs, up to 4 per day
split_runs <- pat_graphs %>% group_split(`Run number`)
for (i in seq_along(split_runs)) {
  assign(paste0("Run", i), as.data.frame(split_runs[[i]]))
}

# Narrow run data to plotable variables
colnames(Run1)
Run1 <- Run1 %>%
  dplyr::select(Date, `Run number`, RFU, Patient, `Concentration (M)`, Replicates, Rowgroup, Well, Compound, Dilution, Newgroup)

Run2 <- Run2 %>%
  dplyr::select(Date, `Run number`, RFU, Patient, `Concentration (M)`, Replicates, Rowgroup, Well, Compound, Dilution, Newgroup)

# Separate runs by patient and create new subset
split_pat <- split(Run1, f=list(Run1$Rowgroup))
for (i in seq_along(split_pat)) {
  assign(paste0("Patient", i), as.data.frame(split_pat[[i]]))
}

# Graphs based on mean result values from original data set
# Patient1
Patient1 <- Patient1 %>%
  mutate(
    conc_uM = `Concentration (M)`,
    avg_negb = mean(RFU[Dilution == "Blood -RT"]),
    avg_posb = mean(RFU[Dilution == "Blood +RT"]),
    avg_negw = mean(RFU[Dilution == "Water -RT"]),
    avg_posw = mean(RFU[Dilution == "Water +RT"]),
    norm_fluor = (RFU - avg_negb) / (avg_posw - avg_negw)
  )

min_dose <- min(Patient1$conc_uM, na.rm = TRUE)
max_dose <- max(Patient1$conc_uM, na.rm = TRUE)
min_res <- min(Patient1$norm_fluor, na.rm = TRUE)
max_res <- max(Patient1$norm_fluor, na.rm = TRUE)
# Check dose/response maxes and mins
print(min_dose)
print(max_dose) 
print(min_res) 
print(max_res)

# Patient2
Patient2 <- Patient2 %>%
  mutate(
    conc_uM = `Concentration (M)`,
    avg_negb = mean(RFU[Dilution == "Blood -RT"]),
    avg_posb = mean(RFU[Dilution == "Blood +RT"]),
    avg_negw = mean(RFU[Dilution == "Water -RT"]),
    avg_posw = mean(RFU[Dilution == "Water +RT"]),
    norm_fluor = (RFU - avg_negb) / (avg_posw - avg_negw)
  )

min_dose <- min(Patient2$conc_uM, na.rm = TRUE)
max_dose <- max(Patient2$conc_uM, na.rm = TRUE)
min_res <- min(Patient2$norm_fluor, na.rm = TRUE)
max_res <- max(Patient2$norm_fluor, na.rm = TRUE)
# Check dose/response maxes and mins
print(min_dose)
print(max_dose) 
print(min_res) 
print(max_res)

# Removing Water controls from plotting data
# Patient 1
pat1_graph <- Patient1 %>%
  filter(!(Compound == "Water -RT" | Compound == "Water +RT")) 

obs1 <- Patient1 %>% # Observations used for plot
  group_by(Compound, avg_negb, avg_posb, avg_negw, avg_posw) %>%
  filter(!(Dilution %in% c('Blood -RT', 'Blood +RT', 'Water +RT', 'Water -RT')))

#Patient 2
pat2_graph <- Patient2 %>%
  filter(!(Compound == "Water -RT" | Compound == "Water +RT")) 

obs2 <- Patient2 %>%
  group_by(Compound, avg_negb, avg_posb, avg_negw, avg_posw) %>%
  filter(!(Dilution %in% c('Blood -RT', 'Blood +RT', 'Water +RT', 'Water -RT')))

# Set parameters for 4PL regression
# Attempted to complete 4PL regression several times. Error: object 'parm.constraints' not found
# Searched documentation, Stack, and ran full debug. Could not resolve error. Code included as comments for context. 

# Patient 1
#drpl1 <- dr4pl(
  #norm_fluor ~ conc_uM,
  #data = obs1,
  #method.init = "logistic",
  #dr4pl_theta(
    #theta_1 = 1,
    #theta_2 = NA,
    #theta_3 = 1,
    #theta_4 = 0
 # ))

# Patient 2
#drpl2 <- dr4pl(
  #norm_fluor ~ conc_uM,
  #data = obs2,
  #method.init = "logistic",
  #dr4pl_theta(
    #theta_1 = 1,
    #theta_2 = NA,
    #theta_3 = 1,
    #theta_4 = 0
 # ))

# Graphing the dilution points with pos/neg Blood controls in different colors
# Patient 1
pat1_graph <- ggplot(data = obs1, aes(x = conc_uM, y = norm_fluor)) +
  geom_point(aes(colour = as.factor(Newgroup)), # separate controls by color
             alpha = 0.5,
             size = 4) +
  scale_color_manual(values = c("blue", "red", "darkgrey")) +
  scale_x_continuous(
    trans = scales::log10_trans(),
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 12),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x)),
    expand = c(0, 0),
    limits = c(1e-11, 1e-4) #limits = c(min_dose/2.5, max_dose*4)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 8),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x)), #label log scale as exponents (formatting request)
    expand = c(0, 0),
    limits = c(min_res - 0.2, max_res + 0.2)
  ) +
  xlab("TFV-DP Concentration (M)") +
  ylab("Normalized Fluorescence") +
  ggtitle("Patient 1, Run 1, 12-11-23") +
  theme( #aesthetics
    axis.text.x = element_text(margin = margin(10, 0, 0, 0, "pt")),
    text = element_text(size = 16),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  ) + 
  guides(color = guide_legend(title = "Well Type"))

pat1_graph 

# Save plot as .png
ggsave("111223_PTA1 Graph.png", plot = pat1_graph, width = 20, height = 15)

# Patient 2
pat2_graph <- ggplot(data = obs2, aes(x = conc_uM, y = norm_fluor)) +
  geom_point(aes(colour = as.factor(Newgroup)), 
             alpha = 0.5,
             size = 4) +
  scale_color_manual(values = c("blue", "red", "darkgrey")) +
  scale_x_continuous(
    trans = scales::log10_trans(),
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 12),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x)),
    expand = c(0, 0),
    limits = c(1e-12, 1e-3)
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x, n = 8),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x)),
    expand = c(0, 0),
    limits = c(min_res - 0.2, max_res + 0.2)
  ) +
  xlab("TFV-DP Concentration (M)") +
  ylab("Normalized Fluorescence") +
  ggtitle("Patient 2, Run 1, 12-11-23") +
  theme(
    axis.text.x = element_text(margin = margin(10, 0, 0, 0, "pt")),
    text = element_text(size = 16),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  ) + 
  guides(color = guide_legend(title = "Well Type"))

pat2_graph

# Save plot as .png
ggsave("111223_PTA2 Graph.png", plot = pat2_graph, width = 20, height = 15)
