
require(dplyr)
require(xtable)
############################################################
############################################################
##############   1: clean data  ############################
############################################################
############################################################

#old variable gen
VariableGen <- function(data, Ucost) { #nolint

  ############################################################
  ###1: Convert table to panel data format. create entry/exit years, age
  #set na for values that can be verified
  #(if last year is last year data or first is first in data)
  min_year <- min(data$fyear, na.rm = TRUE)
  max_year <- max(data$fyear, na.rm = TRUE)

  tempdata <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      entry = min(fyear), #nolint
      exit = max(fyear),
      age = ifelse(entry == min_year, NA, fyear - entry), #nolint
      life = ifelse(exit == max_year, NA, exit - fyear) #nolint
    )
  ############################################################
  ###2: Merge and generate

  tempdata <- merge(tempdata, Ucost, by = "fyear", all = TRUE)

  #K
  tempdata <- tempdata %>%
    mutate(K = ppegt * usercost) #nolint


  #MU
  tempdata <- tempdata %>%
    mutate(MU = sale / (cogs + ppegt * usercost)) #nolint

  #MU -1
  tempdata <- tempdata %>%
    mutate(MU_1 = MU - 1) #nolint

  #Adr
  tempdata <- tempdata %>%
    mutate(Adr = xad / sale) #nolint

  #Adr MC
  tempdata <- tempdata %>%
    mutate(Adr_MC = xad / (cogs + ppegt * usercost)) #nolint

  #time untill 2022 (or newest year)
  #grab newest year
  maxyear <- max(tempdata$fyear, na.rm = TRUE)
  tempdata <- tempdata %>%
    mutate(time = fyear - maxyear) #nolint
  #make quadratic and cubic
  tempdata <- tempdata %>%
    mutate(time2 = -time * time)
  tempdata <- tempdata %>%
    mutate(time3 = time * time * time)

  #deflate with GDP deflator
  #get deflator for 2015
  norm <- Ucost$GDP_def[Ucost$fyear == 2010]

  #convert from millions to thousands
  norm <- norm * 1000

  tempdata <- tempdata %>%
    mutate(
      sale = sale * norm / GDP_def, #nolint
      cogs = cogs * norm / GDP_def, #nolint
      xsga = xsga * norm / GDP_def, #nolint
      xad = xad * norm / GDP_def,   #nolint
      ppegt = ppegt * norm / GDP_def #nolint
    ) #nolint


  tempdata

}


exit_gen <- function(data) { #nolint

  ############################################################
  ###1: Convert table to panel data format. create entry/exit years, age
  #set na for values that can be verified
  #check entry based on first available year
  #make sure not first year in data
  min_year <- min(data$fyear, na.rm = TRUE)

  #get exit date by varible dldte (gives date of exit)
  #turn to year
  #but only want to consider dlrsn = 2,3 (bankruptcy or liquidation)
  #   no merger or become public
  data <- data %>%
    mutate(
      dldte = as.Date(as.character(dldte), format = "%Y%m%d"), #nolint
      exit = ifelse(any(dlrsn %in% c(2, 3)), year(dldte), NA) #nolint
    )

  tempdata <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      entry = min(fyear), #nolint
      age = ifelse(entry == min_year, NA, fyear - entry), #nolint
      life = (exit - fyear), #nolint
      exit_ind = ifelse(life == 0, 1, 0)#nolint
    )

  tempdata
}


#new variable gen
vargen <- function(data, Ucost) { #nolint

  ############################################################
  ###1: Convert table to panel data format. create entry/exit years, age
  #set na for values that can be verified
  #(if last year is last year data or first is first in data)
  min_year <- min(data$fyear, na.rm = TRUE)
  max_year <- max(data$fyear, na.rm = TRUE)

  tempdata <- data %>%
    group_by(GVKEY) %>% #nolint
    mutate(
      entry = min(fyear), #nolint
      exit = max(fyear),
      age = ifelse(entry == min_year, NA, fyear - entry), #nolint
      life = ifelse(exit == max_year, NA, exit - fyear) #nolint
    )
  ############################################################
  ###2: Merge and generate

  tempdata <- merge(tempdata, Ucost, by = "fyear", all = TRUE)

  #MU
  tempdata <- tempdata %>%
    mutate(MU = sale / (cogs + ppegt * usercost)) #nolint

  #MU -1
  tempdata <- tempdata %>%
    mutate(MU_1 = MU - 1) #nolint

  #Adr MC
  tempdata <- tempdata %>%
    mutate(Adr_MC = xad / (cogs + ppegt * usercost)) #nolint

  #time untill 2022 (or newest year)
  #grab newest year
  maxyear <- max(tempdata$fyear, na.rm = TRUE)
  tempdata <- tempdata %>%
    mutate(time = fyear - maxyear) #nolint
  #make quadratic and cubic
  tempdata <- tempdata %>%
    mutate(time2 = -time * time)
  tempdata <- tempdata %>%
    mutate(time3 = time * time * time)
  tempdata
}


Cleanadv <- function(data) {  #nolint

  ############################################################
  ###2: Merge and then filter
  ############################################################

  tempdata <- data %>%
    filter(MU >= 0 & !is.na(MU)) %>% #nolint
    filter(Adr >= 0 & !is.na(Adr)) %>% #nolint
    filter(Adr_MC >= 0 & !is.na(Adr_MC)) %>% #nolint
    filter(sale > 0 & !is.na(sale)) %>% #nolint
    filter(cogs >= 0 & !is.na(cogs)) %>% #nolint
    filter(ppegt >= 0 & !is.na(ppegt)) %>% #nolint
    filter(usercost >= 0 & !is.na(usercost)) %>% #nolint
    filter(cogs + ppegt * usercost > 0)

  tempdata
}


clean_deu <- function(data) { #nolint

  #trim at 1%
  data <- alpha_xsag_trim(data, 1, 1)

  # Drop observations when duplicates exist
  data <- data[!(duplicated(data[, c("GVKEY", "fyear")]) |
                   duplicated(data[, c("GVKEY", "fyear")], fromLast = TRUE)), ]

  # Remove rows with NA values in the GVKEY, fyear, and accounting variables #nolint
  data <-
    data[!is.na(data$GVKEY) & !is.na(data$fyear) & !is.na(data$naics)
         & !is.na(data$industry)
     & !is.na(data$ppegt) &!is.na(data$xsga) &!is.na(data$sale) & !is.na(data$cogs), ] #nolint

  # Ensure that accounting variables are positive
  data <- data[data$sale > 0 & data$cogs > 0
               & data$ppegt > 0  & data$xsga > 0, ]

  data

}


clean <- function(data) { #nolint

  # Drop observations when duplicates exist
  data <- data[!(duplicated(data[, c("GVKEY", "fyear")]) |
                   duplicated(data[, c("GVKEY", "fyear")], fromLast = TRUE)), ]

  # Remove rows with NA values in the GVKEY, fyear, and accounting variables #nolint
  data <-
    data[!is.na(data$GVKEY) & !is.na(data$fyear) & !is.na(data$naics)
         & !is.na(data$industry)
     & !is.na(data$ppegt) &!is.na(data$xsga) &!is.na(data$sale) & !is.na(data$cogs), ] #nolint

  # Ensure that accounting variables are positive
  data <- data[data$sale > 0 & data$cogs > 0
               & data$ppegt > 0  & data$xsga > 0, ]

  data

}


clean_adv <- function(data) { #nolint

  # Drop observations when duplicates exist
  data <- data[!(duplicated(data[, c("GVKEY", "fyear")]) |
                   duplicated(data[, c("GVKEY", "fyear")], fromLast = TRUE)), ]

  # Remove rows with NA values in the GVKEY, fyear, and accounting variables (including Adv) #nolint
  data <-
    data[!is.na(data$GVKEY) & !is.na(data$fyear) & !is.na(data$naics)
         & !is.na(data$industry)
     & !is.na(data$ppegt) &!is.na(data$xsga) &!is.na(data$sale) & !is.na(data$cogs), ] #nolint
}

#generate alpha = cogs/sales and trim at 1% and 99% by year
alpha_trim <- function(data,p){ #nolint

  tempdata <- data %>%  #nolint
    mutate(alpha = cogs / sale) #nolint

  #get p% and 100-p% quantiles
  tempdata <- tempdata %>%
    group_by(fyear) %>% #nolint
    mutate(
      q1 = quantile(alpha, 0 +  p / 100, na.rm = TRUE), #nolint
      q99 = quantile(alpha, 1 - p / 100, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(alpha >= q1 & alpha <= q99) #nolint

  tempdata

}

#generate alpha = cogs/sales and alphax = xsga/sales
# and trim at 1% and 99% by year
alpha_xsag_trim <- function(data,p,q){ #nolint

  tempdata <- data %>%  #nolint
    mutate(alpha = cogs / sale) %>%  #nolint
    mutate(alphax = xsga / sale) #nolint

  #get p% and 100-p% quantiles
  tempdata <- tempdata %>%
    group_by(fyear) %>% #nolint
    mutate(
      q1 = quantile(alpha[is.finite(alpha)], 0 +  p / 100, #nolint
        na.rm = TRUE), #nolint
      q99 = quantile(alpha[is.finite(alpha)], 1 - p / 100,
        na.rm = TRUE), #nolint
      q1x = quantile(alphax[is.finite(alphax)], 0 +  q / 100, #nolint
        na.rm = TRUE), #nolint
      q99x = quantile(alphax[is.finite(alphax)], 1 - q / 100,
        na.rm = TRUE) #nolint
    ) %>%
    ungroup() %>%
    filter(alpha > q1 & alpha < q99) %>% #nolint
    filter(alphax > q1x & alphax < q99x) #nolint

  tempdata

}


#Trim following DEU replication
DEU_trim <- function(data,p,q){ #nolint

  tempdata <- data %>%  #nolint
    mutate(alpha = cogs / (cogs + K + xsga) )  #nolint

  #get p% and 100-p% quantiles of alpha
  tempdata <- tempdata %>%
    group_by(fyear) %>% #nolint
    mutate(
      q1 = quantile(alpha[is.finite(alpha)], 0 +  p / 100, #nolint
        na.rm = TRUE), #nolint
      q99 = quantile(alpha[is.finite(alpha)], 1 - p / 100,
        na.rm = TRUE), #nolint
    ) %>%
    ungroup() %>%
    filter(alpha > q1 & alpha < q99) #nolint

  #get p% and 100-p% quantiles of MU
  tempdata <- tempdata %>%
    group_by(fyear) %>% #nolint
    mutate(
      q1 = quantile( MU[is.finite(MU)], 0 +  q / 100, #nolint
        na.rm = TRUE), #nolint
      q99 = quantile( MU[is.finite(MU)], 1 - q / 100,
        na.rm = TRUE), #nolint
    ) %>%
    ungroup() %>%
    filter( MU > q1 & MU < q99) #nolint

  tempdata

}



#Trim xad/cogs+usercost at 1% and 99% by year
adv_trim <- function(data,p){ #nolint

  #get p% and 100-p% quantiles
  tempdata <- data %>%
    group_by(fyear) %>% #nolint
    mutate(
      aq1 = quantile(Adr, 0 +  p / 100, na.rm = TRUE), #nolint
      aq99 = quantile(Adr, 1 - p / 100, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(Adr >= aq1 & Adr <= aq99) #nolint

  tempdata <- tempdata %>%
    filter(MU >= 0 & !is.na(MU)) %>% #nolint
    filter(Adr >= 0 & !is.na(Adr)) %>% #nolint
    filter(Adr_MC >= 0 & !is.na(Adr_MC)) %>% #nolint
    filter(sale > 0 & !is.na(sale)) %>% #nolint
    filter(cogs >= 0 & !is.na(cogs)) %>% #nolint
    filter(ppegt >= 0 & !is.na(ppegt)) %>% #nolint
    filter(usercost >= 0 & !is.na(usercost)) %>% #nolint
    filter(cogs + ppegt * usercost > 0)  #nolint


  tempdata

}


#GDP deflate

GDPdef <- function(data, Ucost) { #nolint
  #deflate with GDP deflator
  #get deflator for 2010
  norm <- Ucost$GDP_def[Ucost$fyear == 2010]

  #convert from millions to thousands
  norm <- norm * 1000

  #merge
  tempdata <- merge(data, Ucost, by = "fyear", all = TRUE)

  tempdata <- tempdata %>%
    mutate(
      sale = sale * norm / GDP_def, #nolint
      cogs = cogs * norm / GDP_def, #nolint
      xsga = xsga * norm / GDP_def, #nolint
      xad = xad * norm / GDP_def,   #nolint
      ppegt = ppegt * norm / GDP_def #nolint
    ) #nolint


  tempdata

}

############################################################
############################################################
##############   2: NAiSC Codes  ###########################
############################################################
############################################################

############################################################
################## 2.a digit give names ####################
############################################################

industry_n_dig <- function(Clean_data, naics, n) { #nolint

  #keep only n level codes
  temp_naisc <- naics %>%
    filter(nchar(naics_n) == n) #nolint

  #grab n digit code level from data
  temp_data <- Clean_data %>%
    mutate(naics_n = ifelse(nchar(naics) >= n, substr(naics, 1, n), NA))

  temp_data <- merge(temp_data, temp_naisc, by = "naics_n", all = TRUE)

  temp_data <- temp_data %>%
    filter(!is.na(naics_n)) #nolint

  #get rid of annoying T's
  temp_data$industry <- paste(temp_data$industry, "")
  temp_data$industry <- gsub("T  ", "", temp_data$industry)
  temp_data$industry <- gsub("T ", "", temp_data$industry)
  temp_data$industry <- gsub("  ", "", temp_data$industry)
  #adding the space to end changes NA to string
  #replace so regression drops NA values
  temp_data$industry <- na_if(temp_data$industry, "NA ")

  temp_data

}


industry_n_dig_2 <- function(Clean_data, n) { #nolint

  #organize sector by n digit code level from data
  temp_data <- Clean_data %>%
    mutate(industry = ifelse(nchar(naics) >= n, substr(naics, 1, n), NA)) #nolint

  temp_data

}

############################################################
######### 2.b generate 2,3,4 digit market shares ###########
############################################################

market_share <- function(data) { #nolint

  #get market share (by 2 digit industry)
  data <- data %>%
    group_by(industry, fyear) %>% #nolint
    mutate(
      m = sale / sum(sale[sale >= 0], na.rm=TRUE) #nolint
    ) %>% #nolint
    ungroup()

  data <- data %>%
    mutate(naics_3digit = substr(naics, 1, 3)) %>% #nolint
    group_by(naics_3digit, fyear) %>% #nolint
    mutate(
      m3 = sale / sum(sale[sale > 0], na.rm=TRUE) #nolint
    ) %>%
    ungroup()

  data <- data %>%
    mutate(naics_4digit = substr(naics, 1, 4)) %>% #nolint
    group_by(naics_4digit, fyear) %>% #nolint
    mutate(
      m4 = sale / sum(sale[sale > 0], na.rm=TRUE) #nolint
    ) %>%
    ungroup()


  data.frame(data)

}


############################################################
############################################################
##########   2: Summary Stats Table  #######################
############################################################
############################################################

sum_stat_table <- function(full_samp, balanced) {

  table <- data.frame(
    "", "", "", "Full Sample", ""
  )

  table[2, ] <- c(
    "", "Accounting Variable", "Mean", "Median", "No. of Obs"
  )

  table[3, ] <- c(
    "Years Since IPO", "", round(mean(full_samp$age, na.rm = TRUE), 2),
    round(median(full_samp$age, na.rm = TRUE), 2),
    sum(!is.na(full_samp$age))
  )

  table[4, ] <- c(
    "Years to Exit", "", round(mean(full_samp$life, na.rm = TRUE), 2),
    round(median(full_samp$life, na.rm = TRUE), 2),
    sum(!is.na(full_samp$life))
  )

  table[5, ] <- c(
    "Cost of Goods Sold", "COGS", round(mean(full_samp$cogs, na.rm = TRUE), 0),
    round(median(full_samp$cogs, na.rm = TRUE), 0), sum(!is.na(full_samp$cogs))
  )

  table[6, ] <- c(
    "SG\\&A", "XSGA", round(mean(full_samp$xsga, na.rm = TRUE), 0),
    round(median(full_samp$xsga, na.rm = TRUE), 0), sum(!is.na(full_samp$xsga))
  )

  table[7, ] <- c(
    "Capital", "PPEGT", round(mean(full_samp$ppegt, na.rm = TRUE), 0),
    round(median(full_samp$ppegt, na.rm = TRUE), 0),
    sum(!is.na(full_samp$ppegt))
  )

  table[9, ] <- c(
    "Advertising", "XAD", round(mean(full_samp$xad, na.rm = TRUE), 0),
    round(median(full_samp$xad, na.rm = TRUE), 0),
    sum(!is.na(full_samp$xad))
  )

  table[8, ] <- c(
    "Revenue", "SALE", round(mean(full_samp$sale, na.rm = TRUE), 0),
    round(median(full_samp$sale, na.rm = TRUE), 0), sum(!is.na(full_samp$sale))
  )


  table[10, ] <- c(
    "Advertising Share", "$xad$",
    round(mean(full_samp$Adr[is.finite(full_samp$Adr)], na.rm = TRUE), 2),
    round(median(full_samp$Adr, na.rm = TRUE), 2),
    sum(!is.na(full_samp$Adr))
  )

  table[11, ] <- c(
    "Cost Accounting Markup", "$\\tilde{\\mu}-1$",
    round(mean(full_samp$MU_1[is.finite(full_samp$MU_1)], na.rm = TRUE), 2),
    round(median(full_samp$MU_1, na.rm = TRUE), 2), sum(!is.na(full_samp$MU_1))
  )

  table[12, ] <- data.frame(
    "", "", "", "Advertising Balanced Sample", ""
  )

  table[13, ] <- c(
    "", "Accounting Variable", "Mean", "Median", "No. of Obs"
  )

  table[14, ] <- c(
    "Years Since IPO", "", round(mean(balanced$age, na.rm = TRUE), 2),
    round(median(balanced$age, na.rm = TRUE), 2),
    sum(!is.na(balanced$age))
  )

  table[15, ] <- c(
    "Years to Exit", "", round(mean(balanced$life, na.rm = TRUE), 2),
    round(median(balanced$life, na.rm = TRUE), 2),
    sum(!is.na(balanced$life))
  )

  table[16, ] <- c(
    "Cost of Goods Sold", "COGS", round(mean(balanced$cogs, na.rm = TRUE), 0),
    round(median(balanced$cogs, na.rm = TRUE), 0),
    sum(!is.na(balanced$cogs))
  )

  table[17, ] <- c(
    "SG\\&A", "XSGA", round(mean(balanced$xsga, na.rm = TRUE), 0),
    round(median(balanced$xsga, na.rm = TRUE), 0),
    sum(!is.na(balanced$xsga))
  )

  table[18, ] <- c(
    "Capital", "PPEGT", round(mean(balanced$ppegt, na.rm = TRUE), 2),
    round(median(balanced$ppegt, na.rm = TRUE), 2),
    sum(!is.na(balanced$ppegt))
  )

  table[20, ] <- c(
    "Advertising", "XAD", round(mean(balanced$xad, na.rm = TRUE), 0),
    round(median(balanced$xad, na.rm = TRUE), 0),
    sum(!is.na(balanced$xad))
  )

  table[19, ] <- c(
    "Revenue", "SALE", round(mean(balanced$sale, na.rm = TRUE), 0),
    round(median(balanced$sale, na.rm = TRUE), 0),
    sum(!is.na(balanced$sale))
  )

  table[21, ] <- c(
    "Advertising Share", "$xad$",
    round(mean(balanced$Adr[is.finite(balanced$Adr)], na.rm = TRUE), 2),
    round(median(balanced$Adr, na.rm = TRUE), 2),
    sum(!is.na(balanced$Adr))
  )

  table[22, ] <- c(
    "Cost Accounting Markup", "$\\tilde{\\mu}-1$",
    round(mean(balanced$MU_1[is.finite(balanced$MU_1)], na.rm = TRUE), 2),
    round(median(balanced$MU_1, na.rm = TRUE), 2),
    sum(!is.na(balanced$MU_1))
  )

  names(table) <- NULL

  # Format the table
  table[5:9, 3:4] <- data.frame(
    sapply(
      table[5:9, 3:4],
      function(x) {
        formatC(
          as.numeric(x),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      }
    )
  )

  table[16:20, 3:4] <- data.frame(
    sapply(
      table[16:20, 3:4],
      function(x) {
        formatC(
          as.numeric(x),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      }
    )
  )

  table[3:11, 5] <- data.frame(
    sapply(
      table[3:11, 5],
      function(x) {
        formatC(
          as.numeric(x),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      }
    )
  )

  table[14:22, 5] <- data.frame(
    sapply(
      table[14:22, 5],
      function(x) {
        formatC(
          as.numeric(x),
          format = "f",
          big.mark = ",",
          digits = 0
        )
      }
    )
  )

  lign <- c("l", "l", "c", "c", "c", "c")

  # Convert the table to a LaTeX table using xtable
  latex_table <- xtable(table, align = lign)

  # Save the output of print(xtable(...)) as a string
  latex_string <-
    capture.output(print(latex_table,
                         hline.after = c(0, 2, 11, 13, nrow(table)),
                         include.rownames = FALSE,
                         sanitize.text.function = identity))

  #add horizontal lines and merge cells o sub titles
  latex_string <- gsub(" &  &  & Full Sample &  \\\\\\\\",
   "& \\\\multicolumn{1}{l}{} & \\\\multicolumn{3}{c} {Full Sample} \\\\\\\\ \\\\cline{3-5} " #nolint
   , latex_string)

  latex_string <- gsub(" &  &  & Advertising Balanced Sample &  \\\\\\\\",
   "& \\\\multicolumn{1}{l}{} & \\\\multicolumn{3}{c} {Advertising Balanced Sample} \\\\\\\\ \\\\cline{3-5} " #nolint
   , latex_string)

  # Print the edited version
  cat(latex_string, sep = "\n")

}