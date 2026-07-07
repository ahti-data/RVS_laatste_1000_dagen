#' AHTI Branding Metadata
#' Bron: Kleurgebruik Stijlgids

ahti_branding <- list(

  # Hoofdkleuren
  colors = list(
    fris_rood     = "#EE3124",
    helder_blauw  = "#009DDC",
    grijs_blauw   = "#336A88",

    # Tintvarianten
    rood_dark_25  = "#82241A",
    rood_dark_75  = "#380C09",
    blauw_light_75 = "#CCDAEI",
    blauw_dark_25  = "#0075A4",
    blauw_dark_75  = "#002737",
    grijs_blauw_dark_25 = "#264F65",
    grijs_blauw_dark_75 = "#0C1A22",

    # Steunkleuren
    fris_groen    = "#00A55D",
    diep_paars    = "#20153E",

    # Grijstinten
    donker_grijs  = "#272727",
    midden_grijs  = "#524F50",
    licht_grijs   = "#F4F4F4"
  ),

  # Aanbevolen Kleurverdeling
  distribution = c(
    primary   = 0.50,
    secondary = 0.20,
    accent_1  = 0.10,
    accent_2  = 0.10,
    accent_3  = 0.10
  ),

  # Metadata voor plotting (bijv. ggplot2)
  scale_discrete = c("#EE3124", "#009DDC", "#336A88", "#00A55D", "#20153E")
)
