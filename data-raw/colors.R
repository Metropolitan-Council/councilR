## code to prepare `colors` dataset goes here


colors <- list(

  ## Council and Metro Transit Colors
  councilBlue        = "#0054A4", # Council blue
  cdGreen            = "#78A22F", # Community Development green
  mtsRed             = "#EE3124", # MTS red
  esBlue             = "#009AC7", # Environmental Services blue
  transitBlue        = "#0053A0", # Metro Transit
  transitRed         = "#ED1B2E",
  transitYellow      = "#FFD200",
  suppYellow         = "#FFD200",
  suppBlack          = "#000000",
  suppGray           = "#666666",
  suppWhite          = "#FFFFFF",

  ## Transit line colors
  metroGreen        = "#008144", # Green Line
  metroOrange       = "#F68A1E", # Orange Line
  transitGold       = "#7A8690", # Gold Line
  metroGray         = "#7A8690", # Lettered lines

  ## Play Features colors
  playGreen         = "#7BA529",
  playBlue          = "#005AAD",
  playDaBlue        = "#00295A",
  playLiBlue        = "#009CCE",
  playSalmon        = "#E78452",
  playYellow        = "#C6DE29",

  ## Metro Stats colors
  metrostatsBlue    = "#0875C3",
  metrostatsBrown   = "#A16C4C",
  metrostatsRed     = "#A14D5D",
  metrostatsDaPurp  = "#643967",
  metrostatsMePurp  = "#AC74A6",
  metrostatsLiPurp  = "#D8B5D6",
  metrostatsPink    = "#F6BD9C",
  metrostatsTan     = "#EAE6C8"
)

usethis::use_data(colors, overwrite = TRUE, compress = "xz")
