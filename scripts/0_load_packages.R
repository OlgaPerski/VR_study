if (!require("pacman")) install.packages("pacman")

pkgs <- c("here",
          "tidyverse",
          "zen4R",
          "readxl",
          "gtsummary",
          "arsenal")

pacman::p_load(pkgs, character.only=T)
