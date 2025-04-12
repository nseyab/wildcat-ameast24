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


#---------------------------------
# Data Split
#---------------------------------
ID <- c("ID", "Team", "Year", "Date", "Opp")

RESPONSE <- c("Points", "Result", "PResult")

EFFICIENCY <- c(
  "SoTP", #Shots on Target %
  "PAwSP", #Positional Attacks with Shot %
  "CAwSP", #Counterattacks with Shots %
  "CwSP", #Corners with Shot %
  "FKwSP", #Free Kicks with Shot %
  "CAP", #Crosses Accurate %
  "ODWP", #Offensive Duels Won %
  "SAoTP", #Shots Against on Target %
  "DDWP", #Defensive Duels Won %
  "ADWP", #Aerial Duels Won %
  "STSP", #Sliding Tackles Successful %
  "PAP", #Passes Accurate % 
  "Possession",
  "DWP", #Duels Won %
  "LongPP", #Long Pass Percentage
  "FwdPAP", #Forward Passes Accurate %
  "BackPAP", #Back Passes Accurate %
  "LatPAP", #Lateral Passes Accurate %
  "LongPAP", #Long Passes Accurate %
  "PtFTAP", #Passes to Final Third Accurate %
  "ProgPAP", #Progressive Passes Accurate %
  "SmPAP", #Smart Passes Accurate %
  "TIAP" #Throw Ins Accurate %
)

VOLUME <- c(
  "Shots",
  "SoT", #Shots on Target
  "Positional_Attacks",
  "PAwS", #Positional Attacks with Shot
  "Counterattacks",
  "CAwS", #Counterattacks with Shots
  "Corners",
  "CwS", #Corners with Shot
  "Free_Kicks",
  "FKwS", #Free Kicks with Shot
  "Crosses",
  "CA", #Crosses Accurate
  "Offensive_Duels",
  "ODW", #Offensive Duels Won
  "Offsides",
  "Shots_Against",
  "SAoT", #Shots Against on Target
  "Defensive_Duels",
  "DDW", #Defensive Duels Won
  "Aerial_Duels",
  "ADW", #Aerial Duels Won
  "Sliding_Tackles",
  "STS", #Sliding Tackles Successful
  "Interceptions",
  "Clearances",
  "Fouls",
  "Yellow_Cards",
  "LLow", #Losses
  "LMid",
  "LHigh",
  "RLow", #Recoveries
  "RMid",
  "RHigh",
  "Forward_Passes",
  "FwdPA", #Forward Passes Accurate
  "Back_Passes",
  "BackPA", #Back Passes Accurate
  "Lateral_Passes",
  "LatPA", #Lateral Passes Accurate
  "Long_Passes",
  "LongPA", #Long Passes Accurate
  "Passes_to_Final_Third",
  "PtFTA", #Passes to Final Third Accurate
  "Progressive_Passes",
  "ProgPA", #Progressive Passes Accurate
  "Smart_Passes",
  "SmPA", #Smart Passes Accurate
  "Throw_Ins",
  "TIA", #Throw Ins Accurate
  "Goal_Kicks"
)

IDENTITY <- c(
  "xG",
  "xGA",
  "Tempo",
  "AvgPP", #Average Passes per Possession
  "PPDA",
  "ASD", #Average Shot Distance
  "APL" #Average Pass Length
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
    axis.text = element_text(family = "ssp", size = 40, color="#263645"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_text(family = "lora", size = 54, color = "#263645"),
    axis.ticks = element_blank(),
    plot.caption = ggtext::element_markdown(family = "ssp", size = 54, face = "bold", color = "#263645"),
    plot.title = element_text(family = "lora", size = 72, face = "bold", color = "#263645"),
    plot.subtitle = element_text(family = "lora", size = 54, face = "bold", color = "#263645"),
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