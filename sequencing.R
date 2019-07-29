sequencing_ge <- function(Qload){
  geresult <- c(0,0,0,0,0,0,0) 
  open_status <- c(0,0,0,0,0,0)
  ge_plr <- c(0,0,0,0,0,0)
  for (i in 1:3) { 
    obj.function <- function(plr1, plr2,plr3,plr4,plr5,plr6) {
      #predict_rf_cl 为预测的coldingload
      if(sum(Qload * c((plr1/10), (plr2/10),(plr3/10) ,(plr4/10),(plr5/10) ,(plr6/10))) >= predict_rf_cl[i]) {pen_1 <- 0} 
      else {pen_1 <- 100000} 
      #PLR状态
      PLRStatus <- fun.check.status(c(plr1, plr2,plr3,plr4,plr5,plr6))
      #约束：开启的冷机2h内不能关闭,因为数据为每5分钟一次，所以从第25个数据开始考虑第i-24个数据。
      if(i < 25) {
        if(i == 1) {pen_2 <- 0} else {
          if(length(which(fun.check.status(c(plr1, plr2,plr3,plr4,plr5,plr6)) - open_status[i,] < 0)) >= 1) {pen_2 <- 100000} else {pen_2 <- 0}
        }
      } else 
      { if(length(which(fun.check.status(c(plr1, plr2,plr3,plr4,plr5,plr6)) - open_status[i-24,]<0)) >= 1) {pen_2 <- 100000} else {pen_2 <- 0}}
      
      
     
      
      
      cop1 <- predict(lm.cop1.drop, newdata = data.frame(PLR1 = (plr1/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      cop2 <- predict(lm.cop2.drop, newdata = data.frame(PLR2 = (plr2/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      cop3 <- predict(lm.cop3.drop, newdata = data.frame(PLR3 = (plr3/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      cop4 <- predict(lm.cop4.drop, newdata = data.frame(PLR4 = (plr4/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      cop5 <- predict(lm.cop5.drop, newdata = data.frame(PLR5 = (plr5/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      cop6 <- predict(lm.cop6.drop, newdata = data.frame(PLR6 = (plr6/10), stdTdb = df_cl[i, "stdTdb"], stdRH = df_cl[i,"stdRH"]))
      
      if (cop6 >= 0) {
        cop6 = cop6
      } else {cop6 = 0}
      
      pwr_1 <- Qload[1] * (plr1/10) / cop1
      pwr_2 <- Qload[2] * (plr2/10) / cop2
      pwr_3 <- Qload[3] * (plr3/10) / cop3
      pwr_4 <- Qload[4] * (plr4/10) / cop4
      pwr_5 <- Qload[5] * (plr5/10) / cop5
      pwr_6 <- Qload[6] * (plr6/10) / cop6
      
      result <- pwr_1 + pwr_2 + pwr_3 +pwr_4 +pwr_5 +pwr_6 + pen_1 + pen_2
      return(as.numeric(result))
    }
    ge <- genoud(function(x) obj.function(x[1],x[2],x[3],x[4],x[5],x[6]),data.type.int = TRUE, nvars = 6, max = FALSE, Domains = boundary_matrix)
    geresult <- rbind(geresult,c(ge$par,ge$value))
    ge_plr <- rbind(ge_plr, ge$par)
    open_status <- rbind(open_status, fun.check.status(ge_plr[i+1,]))
  }
  return(data.frame(geresult,ge_plr,open_status))
}
