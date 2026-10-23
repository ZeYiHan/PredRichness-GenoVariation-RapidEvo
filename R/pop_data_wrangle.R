# merge raw flowcytometry readings 

library(tidyverse)

# merge raw flowcytometer readings
merge_pop_csv <- function(pop_data_folder){

  merge_files_from_folder(pop_data_folder)|>
  rename_with(\(x) str_replace_all(x, fixed("-"), "_")) |>
  mutate(Day = readr::parse_number(word(id, 2, sep = "_")),
        Jar = readr::parse_integer(word(id, 3, sep = "_")),
        PredTrt = word(id, 4, sep="_")|>
            replace_values( "control" ~ "Control", "tetra" ~ "Tetra","glauc" ~ "Glauc"),
        GV = readr::parse_integer(word(id, 5, sep = "_")),
        Rep = str_sub(word(id, 6, sep="_"),start = 1, end = 1),
        PredNum =case_when(PredTrt=="Control"~ 0, PredTrt %in% c("Glauc","Tetra","P.caud") ~ 1,
        PredTrt%in% c("TG","GP","TP") ~ 2, PredTrt=="TGP" ~3),
        lgFSC_H = log(FSC_H)) |>
  select(-id)
  
}

# remove outliers and additional features
clean_aggregate_pop <- function(pop_data_merged){
  pop_remove_outlier <- pop_data_merged |> 
    group_by(Day,PredTrt,GV,Rep,PredNum,Jar) |>
    mutate(
      mlgFSC_H = mean(lgFSC_H),
      sdlgFSC_H = sd(lgFSC_H),
      z_lgFSC_H = (lgFSC_H - mlgFSC_H)/sdlgFSC_H) |>
    filter(z_lgFSC_H > -2 & z_lgFSC_H < 2)

  pop_count <- pop_remove_outlier |>
    group_by(Day, GV, PredNum, PredTrt, Jar, Rep) |>
    summarise(count = n(), .groups = "drop")

  pop_aggregate <- pop_count |>
    bind_rows( # day-0 inoculum, same for all jars
      pop_count |> distinct(GV, PredNum, PredTrt, Jar, Rep) |> mutate(Day = 0, count = 180)
    ) |>
    mutate(
      Dens   = (count / 30) * 1000,  # density: cells per mL
      lnDens = log(Dens + 1), 
      GV_f   = factor(GV, levels = c(10, 100, 1000)), 
      PredNum_f = factor(PredNum, levels = c(0, 1, 2, 3)), 
      Day_f  = factor(Day, levels = c(0, 7, 14, 21)),
      PredTrt = factor(PredTrt, levels = c("Control", "Glauc", "Tetra", "P.caud", "TG", "GP", "TP", "TGP")),
      Tetra  = as.integer(PredTrt %in% c("Tetra", "TG", "TP", "TGP")),
      Glauc  = as.integer(PredTrt %in% c("Glauc", "TG", "GP", "TGP")),
      Pcaud  = as.integer(PredTrt %in% c("P.caud", "TP", "GP", "TGP"))
  ) |>
    filter(!Jar == 42) # abnormal dynamics, likely contamination or innoculation error
  pop_aggregate
}


