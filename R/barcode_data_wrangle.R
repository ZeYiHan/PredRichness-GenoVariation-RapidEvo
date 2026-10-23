merge_bc_data <- function(bc_shepherd_folder, jar_ids){
  merge_files_from_folder(bc_shepherd_folder) |>
  mutate(Jar=readr::parse_integer(str_sub(word(id, 1, sep="_"),start =4)),
         IB=readr::parse_integer(word(id, 3, sep="_"))) |>
  rename(day0=time_point_1, day21=time_point_2)|> 
  left_join(jar_ids, by = "Jar")
}

clean_bc_data <- function(bc_data_merged){
  bad_sample <- c(42)    # abnormal dynamics, removed for population dynamics as well
  IB = 3                 # this analysis use insertion barcode on 3' 
  bc_data_merged |>
    filter(IB==IB,
           !Jar %in% bad_sample, 
           day0 > 20 # remove barcodes with low reading counts from initial population, Sequencing depth was scaled proportionally to the number of variants per pool
    )|> 
    rename(PredTrt = Pred)|>
    arrange(Jar)|>
    group_by(Jar)|>
    mutate(
      total_d0 = sum(day0),
      total_d21 = sum(day21),
      freq_d0 = day0/total_d0,
      freq_d21 = day21/total_d21,
      dlt_freq=freq_d21-freq_d0, 
      JarID=paste0("Jar",stringr::str_pad(Jar, width = 2, side = 'left', pad = '0')),
      Tetra=if_else(PredTrt %in% c("tetra","TG","TP","TGP"),1,0), 
      Glauc=if_else(PredTrt %in% c("glauc","TG","GP","TGP"),1,0), 
      Pcaud=if_else(PredTrt %in% c("P.caud","TP","GP","TGP"),1,0), 
      rank_d0= rank(-freq_d0), 
      rank_d21=rank(-freq_d21),
      rank_dlt_freq = rank(-dlt_freq),
      PredTrt= factor(PredTrt, levels =c("Control", "Glauc","Tetra","P.caud","TG","GP","TP","TGP")),
      PredNum_f = factor(PredNum, levels = c(0, 1,2,3)),
      GV_f = factor(GV, levels = c(10, 100, 1000))
    ) |>
    ungroup() |>
    select(-id)
}

aggregate_bc_data <- function(bc_data_cleaned){
  bc_data_cleaned |>
    group_by(Jar,JarID,PredNum_f,PredTrt,GV_f,Rep,Tetra,Glauc,Pcaud)|>
    summarise(richness = sum(day21 > 0),
              abundance = sum(day21), 
              d0_rich = sum(day0 > 0),
              Shannon = vegan::diversity(day21, index = "shannon"),
              .groups = "drop")|>
    mutate(perctLoss=(d0_rich-richness)/d0_rich)
}