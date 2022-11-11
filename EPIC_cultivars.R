# ***************
# EPIC-IIASA (CZ) 
# ***************

# preparing cultivars tables for EPIC IIASA CZ calibration



# PACKAGES ----------------------------------------------------------------



# install.packages("tidyverse", dependencies = T)
require(tidyverse)



# BASE --------------------------------------------------------------------



path_tab <- "c:/Users/krizovak/Documents/__EPIC__/R/_tables/"
path_out <- "c:/Users/krizovak/Documents/__EPIC__/R/_nitroRESULTS/"



# LOOP --------------------------------------------------------------------



# *** READ *** READ *** READ ** READ ***
jul_cal <- read.table(paste0(path_tab, "jul_cal.csv"), header = TRUE, sep = ";")
# *** READ *** READ *** READ ** READ ***

croplist <- c("BARL", "CORN", "WWHT")
# croplist <- c("CORN")


# v4 cultivars - PO specific

for(i in 1:length(croplist)) {  
  crop <- croplist[i]
  if(crop == "BARL"){ # new approach in creating cultivars - production zones
    SEAS <- "SPG"
    CROPID <- 14
    PLN_JUL <- c(84, 91, 100, 100) 
    LVP <- c(120, 120, 125, 130) # growth period
    ida <- c("k", "r", "b", "h")
    idb <- rep(c("1"), times = 4)
    CULT <- paste(ida, idb, sep="") # CULT = planting scenario
  } else if(crop == "CORN"){
    SEAS <- "SPG"
    CROPID <- 155
    PLN_JUL <- c(100, 115, 125, 130) 
    LVP <- c(175, 175, 150, 145) # growth period
    ida <- c("k", "r", "b", "h")
    idb <- rep(c("1"), times = 4)
    CULT <- paste(ida, idb, sep="") # CULT = planting scenario
  # } else if(crop == "CORN"){
  #   SEAS <- "SPG"
  #   PLN_JUL <- c(91,100,110, 105,115,125, 115,125,135, 121,130,140) 
  #   LVP <- rep(c(175, 175, 150, 145), each = 3) # growth period
  #   ida <- rep(c("k", "r", "b", "h"), each = 3)
  #   idb <- rep(c("1", "2", "3"), times = 4)
  #   CULT <- paste(ida, idb, sep="") # CULT = planting scenario
  } else if(crop == "WWHT"){
    SEAS <- "WIN"
    CROPID <- 10
    PLN_JUL <- c(293, 283, 268, 263) 
    LVP <- c(285, 280, 305, 315) # growth period
    ida <- c("k", "r", "b", "h")
    idb <- rep(c("1"), times = 4)
    CULT <- paste(ida, idb, sep="") # CULT = planting scenario
  } else {
    print("ZEROO")
  }
  if(SEAS == "SPG"){
    plant_scenarios <- data.frame(CULT, PLN_JUL, LVP) %>% 
      mutate(HRV_JUL = (PLN_JUL + LVP))   # harvest date
  } else if(SEAS == "WIN"){
        plant_scenarios <- data.frame(CULT, PLN_JUL, LVP) %>%
          mutate(HRV_JUL = (PLN_JUL + LVP)-365)   # harvest date
      } else {
        print("ZEROO")
      }
  cultivars <- plant_scenarios %>%
    merge(jul_cal, by.x = "PLN_JUL", by.y = "jul_seq", all.x = TRUE) %>%  
    merge(jul_cal, by.x = "HRV_JUL", by.y = "jul_seq", all.x = TRUE) %>% 
    mutate(CROP = crop) %>% 
    mutate(CROPID = CROPID)
  colnames(cultivars)[colnames(cultivars) == "dat_seq.x"] <- "sow_dat"
  colnames(cultivars)[colnames(cultivars) == "dat_mon.x"] <- "PLN_MON"
  colnames(cultivars)[colnames(cultivars) == "dat_day.x"] <- "PLN_DAY"
  colnames(cultivars)[colnames(cultivars) == "dat_seq.y"] <- "hrv_dat"
  colnames(cultivars)[colnames(cultivars) == "dat_mon.y"] <- "HRV_MON"
  colnames(cultivars)[colnames(cultivars) == "dat_day.y"] <- "HRV_DAY"
  cultivars <- cultivars %>% 
    select(CROP, CROPID, CULT, PLN_JUL, HRV_JUL, LVP, sow_dat, PLN_MON, PLN_DAY, 
           hrv_dat, HRV_MON, HRV_DAY) %>% 
    arrange(CULT)
  # write.table(cultivars, file = paste0(path_tab, "cultivars/", crop, "_cultivars.csv"), row.names=FALSE, sep = ";")
  dir.create (paste0(path_out, crop))
  write.csv(cultivars, file = paste0(path_out, crop, "/", crop, "_cultivars.csv"), 
            row.names=FALSE)
}


# v3 cultivars

# for(i in 1:length(croplist)) {  # FUNKCNI 
#   crop <- croplist[i]
#   if(crop == "BARL"){ # new approach in creating cultivars - production zones
#     seas <- "SPG"
#     PLN_JUL <- c(79,60, 69,69, 69,79, 79,91) 
#     HRV_JUL <- c(212,191, 201,191, 191,212, 222,222) 
#     ida <- rep(c("k", "r", "b", "h"), each = 2)
#     idb <- rep(c("1", "2"), times = 4)
#     runid <- paste(ida, idb, sep="") # runid = planting scenario
#   # if(crop == "BARL"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- c(79,84,89, 69,74,79, 100,105,110, 60,64,69, 91,94,100) 
#   #   HRV_JUL <- c(201,206,211, 181,186,191, 212,217,222, 181,186,191, 212,217,222) 
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 3)
#   #   idb <- rep(c("a", "b", "c"), times = 5)
#   #   runid <- paste(ida, idb, sep="") # runid = planting scenario
#   # } else if(crop == "CSIL"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(88,98,108,118,128,138,148,158,168), each = 5) # julday
#   #   LVP <- rep(c(120,130,140,150,160), times = 9) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 5)
#   #   idb <- rep(c("a", "b", "c", "d", "e"), times = 9)
#   #   runid <- paste(ida, idb, sep="")
#   } else if(crop == "CORN"){
#     seas <- "SPG"
#     PLN_JUL <- c(100,105,110, 100,105,110, 91,94,100) 
#     HRV_JUL <- c(253,258,263, 263,268,273, 244,248,253)
#     ida <- rep(c("s1", "s2", "s3"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 3)
#     runid <- paste(ida, idb, sep="")
#   # } else if(crop == "OATS"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 3) # julday
#   #   LVP <- rep(c(120, 130, 140), times = 9) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 3)
#   #   idb <- rep(c("a", "b", "c"), times = 9)
#   #   runid <- paste(ida, idb, sep="") 
#   # } else if(crop == "FPEA"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 3) # julday
#   #   LVP <- rep(c(130, 140, 150), times = 9) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 3)
#   #   idb <- rep(c("a", "b", "c"), times = 9)
#   #   runid <- paste(ida, idb, sep="") 
#   # } else if(crop == "POTA"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 4) # julday
#   #   LVP <- rep(c(130, 140, 150, 160), times = 9) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 4)
#   #   idb <- rep(c("a", "b", "c", "d"), times = 9)
#   #   runid <- paste(ida, idb, sep="") 
#   # } else if(crop == "RAPE"){
#   #   seas <- "WIN"
#   #   PLN_JUL <- rep(c(202, 212, 222, 232, 242), each = 6) # julday
#   #   LVP <- rep(c(280, 290, 300, 310, 320, 330), times = 5) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 6)
#   #   idb <- rep(c("a", "b", "c", "d", "e", "f"), times = 5)
#   #   runid <- paste(ida, idb, sep="")
#   # } else if(crop == "RYE"){
#   #   seas <- "WIN"
#   #   PLN_JUL <- rep(c(251,261,271,281,291), each = 5) # julday
#   #   LVP <- rep(c(280, 290, 300, 310, 320), times = 5) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#   #   idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#   #   runid <- paste(ida, idb, sep="")
#   # } else if(crop == "SGBT"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(89,99,109,119,129), each = 5) # julday
#   #   LVP <- rep(c(150,160,170,180,190), times = 5) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#   #   idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#   #   runid <- paste(ida, idb, sep="")
#   # } else if(crop == "SUNF"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(100,110,120,130,140), each = 3) # julday
#   #   LVP <- rep(c(100, 110, 120), times = 5) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 3)
#   #   idb <- rep(c("a", "b", "c"), times = 5)
#   #   runid <- paste(ida, idb, sep="")
#   # } else if(crop == "SOYB"){
#   #   seas <- "SPG"
#   #   PLN_JUL <- rep(c(100,110,120,130,140), each = 5) # julday
#   #   LVP <- rep(c(110,120,130,140,150), times = 5) # growth period
#   #   ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#   #   idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#   #   runid <- paste(ida, idb, sep="")
#   } else if(crop == "WWHT"){
#     seas <- "WIN"
#     PLN_JUL <- c(263,268,273, 263,268,273, 253,258,263, 253,258,263, 263,268,273)
#     HRV_JUL <- c(212,217,222, 201,206,211, 212,217,222, 222,227,232, 191,196,201)
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else {
#     print("ZEROO")
#   }
#   if(seas == "SPG"){
#     plant_scenarios <- data.frame(runid, PLN_JUL, HRV_JUL) %>% 
#       mutate(LVP = (HRV_JUL - PLN_JUL))   # calculate LVP: days between planting and harvest
#   } else if(seas == "WIN"){
#     plant_scenarios <- data.frame(runid, PLN_JUL, HRV_JUL) %>% 
#       mutate(LVP = ((365-PLN_JUL) + HRV_JUL))  # calculate LVP: days till end of the year + days till harvest
#   } else {
#     print("ZEROO")
#   }
#   cultivars <- plant_scenarios %>%
#     merge(jul_cal, by.x = "PLN_JUL", by.y = "jul_seq", all.x = TRUE) %>%  
#     merge(jul_cal, by.x = "HRV_JUL", by.y = "jul_seq", all.x = TRUE)
#   colnames(cultivars)[colnames(cultivars) == "dat_seq.x"] <- "sow_dat"
#   colnames(cultivars)[colnames(cultivars) == "dat_mon.x"] <- "PLN_MON"
#   colnames(cultivars)[colnames(cultivars) == "dat_day.x"] <- "PLN_DAY"
#   colnames(cultivars)[colnames(cultivars) == "dat_seq.y"] <- "hrv_dat"
#   colnames(cultivars)[colnames(cultivars) == "dat_mon.y"] <- "HRV_MON"
#   colnames(cultivars)[colnames(cultivars) == "dat_day.y"] <- "HRV_DAY"
#   cultivars <- cultivars %>% 
#     select(runid, PLN_JUL, HRV_JUL, LVP, sow_dat, PLN_MON, PLN_DAY, 
#            hrv_dat, HRV_MON, HRV_DAY) %>% 
#     arrange(runid)
#   # write.table(cultivars, file = paste0(path_tab, "cultivars/", crop, "_cultivars.csv"), row.names=FALSE, sep = ";")
#   dir.create (paste0(path_out, crop))
#   write.csv(cultivars, file = paste0(path_out, crop, "/", crop, "_cultivars.csv"), 
#               row.names=FALSE)
#   }



# v2 CULTIVARS BACKUP --------------------------------------------------------------------

# 
# # *** READ *** READ *** READ ** READ ***
# jul_cal <- read.table(paste0(path_tab, "jul_cal.csv"), header = TRUE, sep = ";")
# # *** READ *** READ *** READ ** READ ***
# 
# # chybi alfa !!!
# croplist <- c("BARL", "CSIL", "CORN", "OATS", "FPEA", "POTA",
#               "RAPE", "RYE", "SGBT", "SUNF", "SOYB", "WWHT")
# 
# 
# for(i in 1:length(croplist)) {  # FUNKCNI 
#   crop <- croplist[i]
#   if(crop == "BARL"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(38, 48, 58, 68, 78, 88, 98, 108, 118), each = 3) # julday
#     LVP <- rep(c(120, 130, 140), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 9)
#     runid <- paste(ida, idb, sep="") # runid = planting scenario
#   } else if(crop == "CSIL"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(88,98,108,118,128,138,148,158,168), each = 5) # julday
#     LVP <- rep(c(120,130,140,150,160), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 9)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "CORN"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(88,98,108,118,128,138,148,158,168), each = 5) # julday
#     LVP <- rep(c(140,150,160,170,180), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 9)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "OATS"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 3) # julday
#     LVP <- rep(c(120, 130, 140), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 9)
#     runid <- paste(ida, idb, sep="") 
#   } else if(crop == "FPEA"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 3) # julday
#     LVP <- rep(c(130, 140, 150), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 9)
#     runid <- paste(ida, idb, sep="") 
#   } else if(crop == "POTA"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(36,46,56,66,76,86,96,106,116), each = 4) # julday
#     LVP <- rep(c(130, 140, 150, 160), times = 9) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9"), each = 4)
#     idb <- rep(c("a", "b", "c", "d"), times = 9)
#     runid <- paste(ida, idb, sep="") 
#   } else if(crop == "RAPE"){
#     seas <- "WIN"
#     PLN_JUL <- rep(c(202, 212, 222, 232, 242), each = 6) # julday
#     LVP <- rep(c(280, 290, 300, 310, 320, 330), times = 5) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 6)
#     idb <- rep(c("a", "b", "c", "d", "e", "f"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "RYE"){
#     seas <- "WIN"
#     PLN_JUL <- rep(c(251,261,271,281,291), each = 5) # julday
#     LVP <- rep(c(280, 290, 300, 310, 320), times = 5) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "SGBT"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(89,99,109,119,129), each = 5) # julday
#     LVP <- rep(c(150,160,170,180,190), times = 5) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "SUNF"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(100,110,120,130,140), each = 3) # julday
#     LVP <- rep(c(100, 110, 120), times = 5) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 3)
#     idb <- rep(c("a", "b", "c"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "SOYB"){
#     seas <- "SPG"
#     PLN_JUL <- rep(c(100,110,120,130,140), each = 5) # julday
#     LVP <- rep(c(110,120,130,140,150), times = 5) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 5)
#     runid <- paste(ida, idb, sep="")
#   } else if(crop == "WWHT"){
#     seas <- "WIN"
#     PLN_JUL <- rep(c(246,256,266,276,286,296,306), each = 5) # julday
#     LVP <- rep(c(280,290,300,310,320), times = 7) # growth period
#     ida <- rep(c("s1", "s2", "s3", "s4", "s5", "s6", "s7"), each = 5)
#     idb <- rep(c("a", "b", "c", "d", "e"), times = 7)
#     runid <- paste(ida, idb, sep="")
#   } else {
#     print("ZEROO")
#   }
#   if(seas == "SPG"){
#     plant_scenarios <- data.frame(runid, PLN_JUL, LVP) %>% 
#       mutate(HRV_JUL = (PLN_JUL + LVP))   # harvest date
#   } else if(seas == "WIN"){
#     plant_scenarios <- data.frame(runid, PLN_JUL, LVP) %>% 
#       mutate(HRV_JUL = (PLN_JUL + LVP)-365)   # harvest date
#   } else {
#     print("ZEROO")
#   }
#   cultivars <- plant_scenarios %>%
#     merge(jul_cal, by.x = "PLN_JUL", by.y = "jul_seq", all.x = TRUE) %>%  
#     merge(jul_cal, by.x = "HRV_JUL", by.y = "jul_seq", all.x = TRUE)
#   colnames(cultivars)[colnames(cultivars) == "dat_seq.x"] <- "sow_dat"
#   colnames(cultivars)[colnames(cultivars) == "dat_mon.x"] <- "PLN_MON"
#   colnames(cultivars)[colnames(cultivars) == "dat_day.x"] <- "PLN_DAY"
#   colnames(cultivars)[colnames(cultivars) == "dat_seq.y"] <- "hrv_dat"
#   colnames(cultivars)[colnames(cultivars) == "dat_mon.y"] <- "HRV_MON"
#   colnames(cultivars)[colnames(cultivars) == "dat_day.y"] <- "HRV_DAY"
#   cultivars <- cultivars %>% 
#     select(runid, PLN_JUL, HRV_JUL, LVP, sow_dat, PLN_MON, PLN_DAY, 
#            hrv_dat, HRV_MON, HRV_DAY) %>% 
#     arrange(runid)
#   # write.table(cultivars, file = paste0(path_tab, "cultivars/", crop, "_cultivars.csv"), row.names=FALSE, sep = ";")
#   dir.create (paste0(path_out, crop))
#   write.table(cultivars, file = paste0(path_out, crop, "/", crop, "_cultivars.csv"), row.names=FALSE, sep = ";")
# }
