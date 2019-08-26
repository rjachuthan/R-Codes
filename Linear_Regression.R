if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, readxl, lubridate, pbapply, zoo, xlsx, DMwR)
Sys.setlocale(locale = "chs")
gc()




# Helper Functions ----------------------------------------------------------------------
normalize_data <- function(x) {
  min_x  <- min(x,na.rm = T)
  max_x  <- max(x,na.rm = T)
  norm_x <- (x - min_x)/(max_x - min_x)
  return(norm_x)
}

rem_outlier <- function(y) {
  y_dec <- quantile(x = y,probs = seq(from = 0,to = 1,by = 0.1),type = 5,na.rm = T)
  y_qnt <- quantile(x = y,na.rm = T)
  y[which(y > (y_qnt[4] + (1.5*IQR(x = y,na.rm = T))))] <- y_dec[10]
  y[which(y < (y_qnt[2] - (1.5*IQR(x = y,na.rm = T))))] <- y_dec[2]
  return(y)
}




# Mapping -------------------------------------------------------------------------------
mpng_Province <- read_excel("Datasets/Region_Province_Mapping.xlsx", sheet = "Primary Sales")

mpng_Region <- unique(mpng_Province[, c("Region", "Region_Eng")])
mpng_Province <- mpng_Province[, c("Province", "Province_Eng")]

temp <- fread("Datasets/01_tabular_data/primary_sales/primary_sales_wtn_data.csv", encoding = "UTF-8")
temp <- unique(temp[, .(Region, Province)])

temp <- merge(temp, mpng_Province, by = "Province")
temp <- merge(temp, mpng_Region, by = "Region")
temp <- temp[, .(Region_Eng, Province_Eng)]
names(temp) <- c("Region", "Province")

mpng_Province <- read_excel("Datasets/05_geo_data/China_Province_City_Mapping.xlsx", sheet = 1)
mpng_Province <- mpng_Province[!is.na(mpng_Province$`14 Markets map to province`),
  c("Province", "14 Markets map to province")]

names(mpng_Province) <- c("Province", "City")

temp$Province <- str_to_title(temp$Province)

mapping_region <- merge(temp, mpng_Province, by = "Province")
mapping_region <- mapping_region[order(Region, Province), .(Region, Province, City)]

rm(mpng_Province, mpng_Region, temp)




# Dataset -------------------------------------------------------------------------------
model_month <- 1

dt_month <- data.frame(
  City = str_replace_all(
    list.files(path = paste0("Datasets/03_model_data/version05/month", model_month, "/"),
      pattern = ".csv"), ".csv", ""
  )
)

dt_month <- dt_month %>%
  mutate(data = map(.x = City,
    .f = function(x) fread(paste0("Datasets/03_model_data/version05/month", model_month, "/", x, ".csv"))
  ))

dt_month <- dt_month %>%
  group_by(City) %>%
  mutate(
    data = map(data, function(df) {
      df <- setDT(df)
      
      df1 <- copy(df)[, `:=` (Month = NULL, Purchase_Price = NULL)]
      df1 <- df1[, lapply(X = .SD, FUN = rem_outlier), .SDcols = colnames(df1)]
      df1 <- df1[, lapply(X = .SD, FUN = normalize_data), .SDcols = colnames(df1)]
      
      df <- cbind(df[,.(Month, Purchase_Price)], df1)
      df
    })
  )

dt_month <- dt_month %>%
  mutate(cols = map(.x = City, .f = function(x) {
    
    quota_var <- paste0("qta_lag0_quota_" ,mapping_region[mapping_region$Province == x,]$Region)
    
    column_names <- read_excel(paste0("Datasets/03_model_data/03_Feat_Sel_Cor/All features_v5/Sel_Feat_",
      gsub("'", "",mapping_region[mapping_region$Province == x,]$City),".xlsx"),
      sheet = model_month)
    
    column_names <- column_names %>% arrange(-Purchase_Price) %>% head(25)
    column_names <- unique(c("Month", "Purchase_Price", "tgt_National_Target", quota_var, column_names$Variable))
    column_names <- str_replace_all(column_names, "'", "")
    column_names
    })
  )

dt_month <- dt_month %>%
  mutate(
    data = map2(.x = data, .y = cols, .f = function(x, y) {
      names(x) <- str_replace_all(names(x), "'", "")
      x <- x %>% select(y)
      x
    })
  )

dt_month <- dt_month[, c("City", "data")]




# Modelling -----------------------------------------------------------------------------
city_function <- function(df_name) {
  print(df_name)
  set.seed(100)
  
  # Full model data for the City
  model_dat <- dt_month[dt_month$City == df_name, ]$data[[1]]
  names(model_dat) <- str_replace_all(names(model_dat), "'", "")
  
  start_time <- Sys.time()
  print(paste("Preparing model data.", start_time))
  
  # Training Data
  dat <- model_dat[model_dat$Month < switch(model_month, "1" = 201900, "2" = 201812, "3" = 201811),]
  
  # Validation Data
  validation_2 <- model_dat %>% filter(Month > 201900) %>% select(-Month, -Purchase_Price)
  true_vals <- model_dat %>% filter(Month > 201900)
  
  # Columns after Feature selection
  quota_var    <- paste0("qta_lag0_quota_" ,mapping_region[mapping_region$Province == df_name,]$Region)
  column_names <- names(dat)
  column_names <- column_names[!column_names %in% c("Month", "Purchase_Price", "tgt_National_Target",
    quota_var)]
  
  # List of all possible combinations
  print(paste("Getting all possible combinations.", round(Sys.time() - start_time, 2)))
  comb_num <- ifelse(length(column_names) < 8, length(column_names), 8)
  
  temp_list <- list()
  for(cols in 1:comb_num) {
    temp <- combn(x = column_names, m = cols, simplify = FALSE)
    temp_list[[cols]] <- temp
  }
  
  # List of all possible Formulaes
  print(paste("Creating Formulae.", round(Sys.time() - start_time, 2)))
  
  all_models <- lapply(temp_list, function(dat) {
      rbindlist(pbapply::pblapply(dat, function(x) {
        data.frame(
          Formulae = paste("Purchase_Price ~ tgt_National_Target + ", quota_var, " + ",
            paste(x, collapse = " + "))
        )
      }))
  })
  all_models <- rbindlist(all_models)
  all_models[, Formulae := as.character(Formulae)]
  
  # Running a LR Model with all above Variables
  print(paste0("Total no. of Models : ", prettyNum(nrow(all_models), big.mark = ",")))
  print(paste("Running Models", round(Sys.time() - start_time, 2)))
  
  all_models <- all_models %>%
    group_by(Formulae) %>%
    do(model = lm(as.formula(.$Formulae), data = dat))
  
  print(paste("Collecting Model informations.", round(Sys.time() - start_time, 2)))
  all_models <- all_models %>%
    group_by(Formulae) %>%
    mutate(
      tidy   = map(model, broom::tidy),
      glance = map(model, broom::glance),
      
      ST_Coeff      = as.numeric(map(model, function(x) x$coefficients[[2]])),
      ST_pvalue     = as.numeric(map(tidy, function(x) x$p.value[[2]])),
      QT_Coeff      = as.numeric(map(model, function(x) x$coefficients[[3]])),
      QT_pvalue     = as.numeric(map(tidy, function(x) x$p.value[[2]])),
      adj_r_squared = glance %>% map_dbl("adj.r.squared"),
      
      preds  = map(model, function(x) predict(object = x, newdata = validation_2)),
      scores = map(preds, function(x) regr.eval(trues = true_vals$Purchase_Price, preds = x)),
      mae    = scores %>% map_dbl("mae"),
      mse    = scores %>% map_dbl("mse"),
      rmse   = scores %>% map_dbl("rmse"),
      mape   = scores %>% map_dbl("mape")
    )
  
  print(paste("Finalizing.", round(Sys.time() - start_time, 2)))
  all_models <- all_models %>% select(-scores) %>% mutate(City = df_name)
  all_models %>% arrange(mae)
}


# City-wise Models
city_names <- as.character(unique(dt_month$City))
city_models <- vector(length = length(as.character(unique(dt_month$City))), mode = "list")
names(city_models) <- city_names


library(parallel)
cl <- makeCluster(7)
clusterExport(cl = cl, c("dt_month", "model_month", "mapping_region"))
clusterEvalQ(cl, pacman::p_load(data.table, tidyverse, readxl, lubridate, pbapply, zoo, xlsx, DMwR))
city_models_1 <- parLapplyLB(cl = cl, city_names, city_function)
stopCluster(cl = cl)

city_models_1 <- lapply(city_names[1], city_function)

save(list = "city_models_1",
  file = "Datasets/04_model_results/02_LR_manual_stepwise/v03/20190824/city_models_1.RData")

Beijing <- city_models[[1]]
Beijing[Beijing$adj_r_squared > 0.6 & !is.na(Beijing$mae) & !Beijing$mae == 0, ] %>% arrange(mae)

bei_dat <- dt_month[dt_month == "Beijing", ]
trn_bei <- bei_dat[bei_dat$Month < 201811, ]
trn_bei <- trn_bei[, !colnames(trn_bei) %in% c("Month", "City")]

mod_1 <- lm(formula = as.formula(Beijing[Beijing$mae == min(Beijing$mae), ]$Formulae), data = trn_bei)
summary(mod_1)
city_models[[1]][city_models[[1]]$mae == min(city_models[[1]]$mae), ]



for(i in 1:14) {
  fwrite(city_models_1[[i]][, c("Formulae", "ST_Coeff", "ST_pvalue", "QT_Coeff", "QT_pvalue",
    "adj_r_squared", "mae", "mse", "rmse", "mape")],
    paste0("Datasets/04_model_results/02_LR_manual_stepwise/v03/20190824/",
      mapping_region[Province == unique(city_models_1[[i]]$City), ]$City, "_M1.csv"),
    row.names = F
  )
  print(mapping_region[Province == unique(city_models_1[[i]]$City), ]$City)
}

mod1 <- city_models_1
mod2 <- city_models_2
mod3 <- city_models_3

mod1 <- lapply(mod1, function(x) {
  x %>% mutate(
    qta_Coeff  = as.numeric(map(model, function(x) x$coefficients[[3]])),
    qta_pvalue = as.numeric(map(tidy, function(x) x$p.value[[3]]))
  )
})

mod2 <- lapply(mod2, function(x) {
  x %>% mutate(
    qta_Coeff  = as.numeric(map(model, function(x) x$coefficients[[3]])),
    qta_pvalue = as.numeric(map(tidy, function(x) x$p.value[[3]]))
  )
})

mod3 <- lapply(mod3, function(x) {
  x %>% mutate(
    qta_Coeff  = as.numeric(map(model, function(x) x$coefficients[[3]])),
    qta_pvalue = as.numeric(map(tidy, function(x) x$p.value[[3]]))
  )
})


### -------------------------- ROUGH WORK ----------------------------- ###
city_names <- c("Beijing", "Changsha", "Changshu", "Chengdu", "Guangzhou", "Kunming",
  "Linyi", "Shanghai", "Shenyang", "ShiJiazhuang", "Wuhan", "Xi'an", "Yiwu", "Zhengzhou")

load("Datasets/04_model_results/02_LR_manual_stepwise/v03/city_models_1.RData")
load("Datasets/04_model_results/02_LR_manual_stepwise/v03/city_models_2.RData")
load("Datasets/04_model_results/02_LR_manual_stepwise/v03/city_models_3.RData")

Beijing_1 <- city_models_1[[1]]
Beijing_2 <- city_models_2[[1]]
Beijing_3 <- city_models_3[[1]]

Beijing_1[Beijing_1$mae == min(Beijing_1$mae), ]
Beijing_2[Beijing_2$mae == min(Beijing_2$mae), ]
Beijing_3[Beijing_3$mae == min(Beijing_3$mae), ]

summary(Beijing_1[Beijing_1$mae == min(Beijing_1$mae), ]$model[[1]])
summary(Beijing_2[Beijing_2$mae == min(Beijing_2$mae), ]$model[[1]])
summary(Beijing_3[Beijing_3$mae == min(Beijing_3$mae), ]$model[[1]])


coeff_fn <- function (x) {
  print(unique(x$City))
  x <- x[x$mae != 0, ]
  
  if (nrow(x[x$ST_Coeff < 0 & x$qta_Coeff < 0, ]) > 0) {
    x <- x[x$ST_Coeff < 0 & x$qta_Coeff < 0, ]
  } else {
    x <- x[x$ST_Coeff < 0, ]
  }
  
  if (nrow(x[x$adj_r_squared > 0.6, ]) > 0) {
    x <- x[x$adj_r_squared > 0.6, ]
  }
  x <- x[order(x$mae), ]
  
  print(paste(
    "MAE :", round(x[x$mae == min(x$mae), ][1,]$mae, 2),
    "AR2 :", round(x[x$mae == min(x$mae), ][1,]$adj_r_squared, 2)
  ))
  
  print(paste(
    "ST Coeff :", round(x[x$mae == min(x$mae), ][1,]$ST_Coeff, 2),
    "QT Coeff :", round(x[x$mae == min(x$mae), ][1,]$qta_Coeff, 2)
  ))
  
  cbind(City = unique(x$City), data.frame(x[x$mae == min(x$mae), ][1,]$tidy))
}

preds_fn <- function (x) {
  
  print(unique(x$City))
  x <- x[x$mae != 0, ]
  
  if (nrow(x[x$ST_Coeff < 0 & x$qta_Coeff < 0, ]) > 0) {
    x <- x[x$ST_Coeff < 0 & x$qta_Coeff < 0, ]
  } else {
    x <- x[x$ST_Coeff < 0, ]
  }
  
  if (nrow(x[x$adj_r_squared > 0.6, ]) > 0) {
    x <- x[x$adj_r_squared > 0.6, ]
  }
  x <- x[order(x$mae), ]
  
  data.frame(
    City = mapping_region[Province == unique(x$City), ]$City,
    Prediction = x[x$mae == min(x$mae), ]$preds[[1]]
  )
}


# Coefficients ----
coeff_1 <- lapply(mod1, coeff_fn)
coeff_1 <- rbindlist(coeff_1)
setcolorder(x = coeff_1, neworder = "City")

coeff_2 <- lapply(mod2, coeff_fn)
coeff_2 <- rbindlist(coeff_2)
setcolorder(x = coeff_2, neworder = "City")

coeff_3 <- lapply(mod3, coeff_fn)
coeff_3 <- rbindlist(coeff_3)
setcolorder(x = coeff_3, neworder = "City")


# Predictions ----
preds_1 <- lapply(mod1, preds_fn)
preds_1 <- rbindlist(preds_1)

preds_2 <- lapply(mod2, preds_fn)
preds_2 <- rbindlist(preds_2)

preds_3 <- lapply(mod3, preds_fn)
preds_3 <- rbindlist(preds_3)
