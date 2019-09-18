library(data.table)

GA_Lin<-function(Data,Target,Norm = T,Keep = NULL,Ban = NULL,Trn = 24,Converge_MAE = Inf,Par = T, ...){
  
  suppressMessages({
    suppressWarnings({
      suppressPackageStartupMessages({
        require(data.table)
        require(DMwR)
        require(GA)
      })
    })
  })
  
  dt_sub<-Data[Month > 201700]
  dt_sub<-dt_sub[order(Month,decreasing = F)]
  dt_sub[,Month := NULL]
  dt_sub<-dt_sub[,which(colnames(dt_sub) %in% Ban == F),with = F]

  rep_out<-function(y){
    y_dec<-quantile(x = y,probs = seq(from = 0,to = 1,by = 0.1),type = 5,na.rm = T)
    y_qnt<-quantile(x = y,na.rm = T)
    y[which(y > (y_qnt[4] + (1.5*IQR(x = y,na.rm = T))))]<-y_dec[10]
    y[which(y < (y_qnt[2] - (1.5*IQR(x = y,na.rm = T))))]<-y_dec[2]
    return(y)
  }
  
  dt_out<-dt_sub[,lapply(X = .SD,FUN = rep_out),.SDcols = colnames(dt_sub)[-which(colnames(dt_sub) == Target)]]
  if(Norm == T){
    dt_out<-dt_out[,lapply(X = .SD,FUN = function(x){
      min_x<-min(x,na.rm = T)
      max_x<-max(x,na.rm = T)
      norm_x<-(x - min_x)/(max_x - min_x)
      return(norm_x)
    }),.SDcols = colnames(dt_out)]
  }
  
  dt_out<-cbind.data.frame(dt_out,dt_sub[[Target]])
  colnames(dt_out)[ncol(dt_out)]<-Target
  
  if(is.null(Keep) == F){
    dt_out<-setcolorder(x = dt_out,neworder = Keep)
  }
  
  dt_trn<-dt_out[1:Trn,]
  dt_tst<-dt_out[(Trn + 1):nrow(dt_out)]
  
  fit_func<-function(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,Keep = Keep, ...){
    res_mae<-tryCatch({
      sel_var<-unique(colnames(dt_out)[as.integer(c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10))])
      lm_form<-as.formula(paste(Target,paste(sel_var,collapse = " + "),sep = " ~ "))
      lm_mod<-lm(formula = lm_form,data = dt_trn)
      lm_sum<-summary(lm_mod)
      adj_rsq<-lm_sum$adj.r.squared
      
      if(is.null(Keep) == F){
        keep_coef<-lm_sum$coefficients[,1][which(rownames(lm_sum$coefficients) %in% Keep)]
        keep_pval<-lm_sum$coefficients[,4][which(rownames(lm_sum$coefficients) %in% Keep)]
        if(keep_coef >= 0 | keep_pval > 0.1 | adj_rsq < 0.6){
          res_mae<-100
        }
        
        else {
          res_mae<-regr.eval(trues = dt_tst[[Target]],
                             preds = predict(object = lm_mod,newdata = dt_tst),stats = "mae")
        }
      }
      
      else {
        if(adj_rsq < 0.6){
          res_mae<-100
        }
        
        else {
          res_mae<-regr.eval(trues = dt_tst[[Target]],
                             preds = predict(object = lm_mod,newdata = dt_tst),stats = "mae")
        }
      }
    },error = function(e){
      return(1000)
    })
    
    return(res_mae)
  }
  
  if(is.null(Keep) == T){
    ga_mod<-ga(type = "real-valued",
               fitness = function(x){-fit_func(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],
                                               Keep = Keep)},lower = rep(0,times = 10),
               upper = rep(ncol(dt_out),times = 10),popSize = 100,pcrossover = 0.8,pmutation = 0.2,
               maxiter = 3000,run = 1000,maxFitness = Converge_MAE,monitor = T,seed = 666,parallel = Par,
               keepBest = T)
  }
  
  else {
    ga_mod<-ga(type = "real-valued",
               fitness = function(x){-fit_func(x[1],x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],
                                               Keep = Keep)},lower = c(1,rep(0,times = 9)),
               upper = c(1,rep(ncol(dt_out),times = 9)),popSize = 100,pcrossover = 0.8,pmutation = 0.2,
               maxiter = 3000,run = 1000,maxFitness = Converge_MAE,monitor = T,seed = 666,parallel = Par,
               keepBest = T)
  }
  
  sol<-as.integer(ga_mod@solution[1,])
  sol_var<-unique(colnames(dt_out)[sol])
  sol_form<-as.formula(paste(Target,paste(sol_var,collapse = " + "),sep = " ~ "))
  sol_mod<-lm(formula = sol_form,data = dt_trn)
  sol_sum<-summary(sol_mod)
  sol_pred<-predict(object = sol_mod,newdata = dt_tst)
  sol_mae<-regr.eval(trues = dt_tst[[Target]],preds = sol_pred,stats = "mae")
  sol_cor<-numeric(length = length(sol_var))
  names(sol_cor)<-sol_var
  for(i in 1:length(sol_var)){
    sol_cor<-c(sol_cor,round(cor(x = dt_trn[[sol_var[i]]],y = dt_trn[[Target]]),digits = 2))
  }
  
  return(list("Formula" = sol_form,"Coef" = sol_sum$coefficients,"Predictions" = sol_pred,
              "Actual" = dt_tst[[Target]],"MAE" = sol_mae,"Correlation" = sol_cor,
              "Model" = sol_mod,"GA_Model" = ga_mod))
}

setwd("E:/China CRM (19-AIM-2810)/WS_market_price_prediction/Datasets/03_model_data/version06")
loc_dt<-list.files(path = choose.dir(getwd()),full.names = T,recursive = F)
names(loc_dt)<-gsub(pattern = ".csv",replacement = "",x = list.files(path = choose.dir(getwd()),full.names = F,
                                                                     recursive = F),fixed = T)
ls_dt<-lapply(X = loc_dt,FUN = function(x){
  fread(input = x,sep = ",",header = T,encoding = "UTF-8")
})

lapply(X = ls_dt,FUN = dim)

dt_map<-fread(input = choose.files(),sep = ",",header = T,encoding = "UTF-8")

ls_dt_N<-ls_dt[which(names(ls_dt) %in% dt_map[Region == "North"]$Province)]
ls_dt_E<-ls_dt[which(names(ls_dt) %in% dt_map[Region == "East"]$Province)]
ls_dt_S<-ls_dt[which(names(ls_dt) %in% dt_map[Region == "South"]$Province)]
ls_dt_W<-ls_dt[which(names(ls_dt) %in% dt_map[Region == "West"]$Province)]

start_time<-Sys.time()

res_N<-lapply(X = ls_dt_N,FUN = function(x){
  colnames(x)<-gsub(pattern = "'",replacement = "",x = colnames(x),fixed = T)
  res<-GA_Lin(Data = x,Target = "Purchase_Price",Norm = T,Keep = "qta_lag0_quota_North",
              Ban = c(paste("qta_lag0_quota",c("East","West","South"),sep = "_"),
                      paste("qta_lag1_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag2_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag3_quota",c("East","West","South","North"),sep = "_")),Trn = 24,Par = F)
  
  return(res)
})

res_E<-lapply(X = ls_dt_E,FUN = function(x){
  colnames(x)<-gsub(pattern = "'",replacement = "",x = colnames(x),fixed = T)
  res<-GA_Lin(Data = x,Target = "Purchase_Price",Norm = T,Keep = "qta_lag0_quota_East",
              Ban = c(paste("qta_lag0_quota",c("West","South"),sep = "_"),
                      paste("qta_lag1_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag2_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag3_quota",c("East","West","South","North"),sep = "_")),Trn = 24,Par = F)
  
  return(res)
})

res_S<-lapply(X = ls_dt_S,FUN = function(x){
  colnames(x)<-gsub(pattern = "'",replacement = "",x = colnames(x),fixed = T)
  res<-GA_Lin(Data = x,Target = "Purchase_Price",Norm = T,Keep = "qta_lag0_quota_South",
              Ban = c(paste("qta_lag0_quota",c("West","East"),sep = "_"),
                      paste("qta_lag1_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag2_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag3_quota",c("East","West","South","North"),sep = "_")),Trn = 24,Par = F)
  
  return(res)
})

res_W<-lapply(X = ls_dt_W,FUN = function(x){
  colnames(x)<-gsub(pattern = "'",replacement = "",x = colnames(x),fixed = T)
  res<-GA_Lin(Data = x,Target = "Purchase_Price",Norm = T,Keep = "qta_lag0_quota_West",
              Ban = c(paste("qta_lag0_quota",c("East","South"),sep = "_"),
                      paste("qta_lag1_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag2_quota",c("East","West","South","North"),sep = "_"),
                      paste("qta_lag3_quota",c("East","West","South","North"),sep = "_")),Trn = 24,Par = F)
  
  return(res)
})

res_fin<-c(res_N,res_E,res_S,res_W)

end_time<-Sys.time()

save(list = "res_fin",file = "GA_Lin_Res_M2.RData")
