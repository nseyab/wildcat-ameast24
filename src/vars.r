# src/vars.r

#---------------------------------
# Tidying
#---------------------------------

YEARS <- c(
  "2022",
  "2023",
  "2024"
)

# Conference Teams
TEAMS <- c(
  "Albany",
  "Binghamton",
  "Bryant",
  "Maine",
  "New-Hampshire",
  "NJIT",
  "UMASS-Lowell",
  "UMBC",
  "Vermont"
)

ATK_COLS <- c(
  "Date",
  "Match",
  "Competition",
  "Duration",
  "Team",
  "Scheme",
  "xG",
  "Shots",
  "SoT", #Shots on Target
  "SoTP", #Shots on Target Percentage
  "Positional_Attacks",
  "PAwS", #Positional Attacks with Shot
  "PAwSP",
  "Counterattacks",
  "CAwS",
  "CAwSP",
  "Corners",
  "CwS",
  "CwSP",
  "Free_Kicks",
  "FKwS",
  "FKwSP",
  "Penalties",
  "Penalties_Converted",
  "PCP",
  "Crosses",
  "CA", #Crosses Accurate
  "CAP",
  "Offensive_Duels",
  "ODW", #Offensive Duels Won
  "ODWP",
  "Offsides"
)

DEF_COLS <- c(
  "Date",
  "Match",
  "Competition",
  "Duration",
  "Team",
  "Scheme",
  "Conceded_Goals",
  "Shots_Against",
  "SAoT", #Shots Against on Target
  "SAoTP",
  "Defensive_Duels",
  "DDW", #Defensive Duels Won
  "DDWP",
  "Aerial_Duels",
  "ADW", #Aerial Duels Won
  "ADWP",
  "Sliding_Tackles",
  "STS", #Sliding Tackles Successful
  "STSP",
  "Interceptions",
  "Clearances",
  "Fouls",
  "Yellow_Cards",
  "Red_Cards"
)

GEN_COLS <- c(
  "Date",
  "Match",
  "Competition",
  "Duration",
  "Team",
  "Scheme",
  "Goals",
  "xG",
  "Shots",
  "SoT",
  "SoTP",
  "Passes",
  "PA", #Passes Accurate
  "PAP",
  "Possession",
  "Losses",
  "LLow",
  "LMid",
  "LHigh",
  "Recoveries",
  "RLow",
  "RMid",
  "RHigh",
  "Duels",
  "DW", #Duels Won
  "DWP"
)

IDX_COLS <- c(
  "Date",
  "Match",
  "Competition",
  "Duration",
  "Team",
  "Scheme",
  "Tempo",
  "AvgPP", #Average Passes per Possession
  "LongPP", #Long Pass Percentage
  "PPDA",
  "ASD", #Average Shot Distance
  "APL" #Average Pass Length
)

PASS_COLS <- c(
  "Date",
  "Match",
  "Competition",
  "Duration",
  "Team",
  "Scheme",
  "Passes",
  "PA",
  "PAP",
  "Forward_Passes",
  "FwdPA",
  "FwdPAP",
  "Back_Passes",
  "BackPA",
  "BackPAP",
  "Lateral_Passes",
  "LatPA",
  "LatPAP",
  "Long_Passes",
  "LongPA",
  "LongPAP",
  "Passes_to_Final_Third",
  "PtFTA",
  "PtFTAP",
  "Progressive_Passes",
  "ProgPA",
  "ProgPAP",
  "Smart_Passes",
  "SmPA",
  "SmPAP",
  "Throw_Ins",
  "TIA",
  "TIAP",
  "Goal_Kicks"
)

# Tournament Teams 2022 - 2024
AEWSOCC <- c(
  "ME22", "NJIT22", "ALBY22", "UML22", "BING22", "UNH22", 
  "UVM23", "UNH23", "UML23", "NJIT23", "ME23", "BING23", 
  "BING24", "UVM24", "UML24", "BRY24", "ME24", "UNH24")

DROP <- c(
  "Goals",
  "Conceded_Goals",
  "SoT", #Shots on Target
  "PAwS", #Positional Attacks with Shot
  "Counterattacks",
  "CAwS", #Counterattack with Shot
  "CAwSP", #Counterattack with Shot Percentage
  "CwS", #Corner with Shot
  "Free_Kicks",
  "FKwS", #Free Kick with Shot
  "FKwSP", #Free Kick with Shot Percentage
  "CA", #Crosses Accurate
  "ODW", #Offensive Duels Won
  "PA", #Passes Accurate
  "DW", #Duels Won
  "SAoT", #Shots Against on Target
  "DDW", #Defensive Duels Won
  "ADW", #Aerial Duels Won
  "STS", #Sliding Tackles Successful
  "FwdPA", #Forward Passes Accurate
  "BackPA", #Back Passes Accurate
  "LatPA", #Lateral Passes Accurate
  "LongPA", #Long Passes Accurate
  "PtFTA", #Passes to Final Third Accurate
  "ProgPA", #Progressive Passes Accurate
  "Smart_Passes",
  "SmPA", #Smart Passes Accurate
  "SmPAP", #Smart Passes Accurate Percentage
  "TIA", #Throw in Accurate
  "Yellow_Cards",
  "Penalties",
  "Penalties_Converted",
  "PCP", #Penalties Converted Percentage
  "Red_Cards"
)


#---------------------------------
# Visualizations
#---------------------------------

library(showtext)
library(ggpubr)
font_add_google("Source Sans Pro", "ssp")
font_add_google("Lora", "lora")
showtext_auto()

theme_set(
  theme_classic(base_size = 20, base_family = "ssp") +
  theme(
    legend.position = "None",
    axis.line = element_line(color="#DCDEDF"),
    axis.text = element_text(family = "ssp", size = 18, color="#263645"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "lora", size = 40, color = "#263645"),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "lora", size = 48, face = "bold", color = "#263645"),
    plot.subtitle = element_text(family = "ssp", size = 40, face = "bold", color = "#263645"),
    plot.caption = element_text(family = "ssp", size = 40, face = "bold", color = "#263645"),
    panel.grid = element_blank()
  )
)

PAL_PRIMARY <- c(
  "ALBY" = "#46166B", # #46166B; # #EEB211
  "BING" = "#005A43", # #005A43; # #6CC24A
  "BRY" =  "#A98F42", # #A98F42; # #826E39
  "ME" = "#082E58", # #082E58; # #79BDE8
  "NJIT" = "#D22630", # #D22630; # #FFFFFF
  "UMBC" = "#000000", # #000000; # #FDB515
  "UML" = "#0067B1", # #0067B1; # #000000
  "UNH" = "#003591", # #003591; # #FFFFFF
  "UVM" = "#154734" # #154734; # #FFD100
)

PAL_SECONDARY <- c(
  "ALBY" = "#EEB211", # #46166B; # #EEB211
  "BING" = "#6CC24A", # #005A43; # #6CC24A
  "BRY" =  "#826E39", # #A98F42; # #826E39
  "ME" = "#79BDE8", # #082E58; # #79BDE8
  "NJIT" = "#FFFFFF", # #D22630; # #FFFFFF
  "UMBC" = "#A67A05", # #000000; # #DA2128
  "UML" = "#000000", # #0067B1; # #000000
  "UNH" = "#FFFFFF", # #003591; # #FFFFFF
  "UVM" = "#FFD100" # #154734; # #FFD100
)