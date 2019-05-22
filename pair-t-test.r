


data1=read.table("for_R/language_pretest_attitude.csv"  , sep=',',header = T)
data2=read.table("for_R/language_posttest_attitude.csv" , sep=',',header = T)

##########(pair t test)
###(6?غA??)
pvalue = array(NA, 18)
for(i in 4:21){
  if(i==18 || i==21){
    pair_t1=t.test(data1[-5,i],data2[-5,i], paired=TRUE)
    pvalue[i-3] = pair_t1$p.value
  }else{
    pair_t1=t.test(data1[,i], data2[,i], paired=TRUE)
    pvalue[i-3] = pair_t1$p.value
  }
}
pair_t1=data.frame(pvalue)

###(??????)
data1_domC=matrix(c(data1[,4],data1[,10],data1[,16]),ncol = 1)
data1_domT=matrix(c(data1[,5],data1[,11],data1[,17]),ncol = 1)
data1_domE=matrix(c(data1[,6],data1[,12],data1[-5,18]),ncol = 1)
data1_recC=matrix(c(data1[,7],data1[,13],data1[,19]),ncol = 1)
data1_recT=matrix(c(data1[,8],data1[,14],data1[,20]),ncol = 1)
data1_recE=matrix(c(data1[,9],data1[,15],data1[-5,21]),ncol = 1)

data2_domC=matrix(c(data2[,4],data2[,10],data2[,16]),ncol = 1)
data2_domT=matrix(c(data2[,5],data2[,11],data2[,17]),ncol = 1)
data2_domE=matrix(c(data2[,6],data2[,12],data2[-5,18]),ncol = 1)
data2_recC=matrix(c(data2[,7],data2[,13],data2[,19]),ncol = 1)
data2_recT=matrix(c(data2[,8],data2[,14],data2[,20]),ncol = 1)
data2_recE=matrix(c(data2[,9],data2[,15],data2[-5,21]),ncol = 1)

pair_t_domC=t.test(data1_domC, data2_domC, paired=TRUE)
pair_t_domT=t.test(data1_domT, data2_domT, paired=TRUE)
pair_t_domE=t.test(data1_domE, data2_domE, paired=TRUE)
pair_t_recC=t.test(data1_recC, data2_recC, paired=TRUE)
pair_t_recT=t.test(data1_recT, data2_recT, paired=TRUE)
pair_t_recE=t.test(data1_recE, data2_recE, paired=TRUE)

palue_domC=pair_t_domC$p.value
palue_domT=pair_t_domT$p.value
palue_domE=pair_t_domE$p.value
palue_recC=pair_t_recC$p.value
palue_recT=pair_t_recT$p.value
palue_recE=pair_t_recE$p.value

pair_t2=data.frame(c(palue_domC,palue_domT,palue_domE,palue_recC,palue_recT,palue_recE))

###(3?ػy??)
data1_C=matrix(c(data1[,4],data1[,7],data1[,10],data1[,13],data1[,16],data1[,19]),ncol = 1)
data1_T=matrix(c(data1[,5],data1[,8],data1[,11],data1[,14],data1[,17],data1[,20]),ncol = 1)
data1_E=matrix(c(data1[,6],data1[,9],data1[,12],data1[,15],data1[-5,18],data1[-5,21]),ncol = 1)

data2_C=matrix(c(data2[,4],data2[,7],data2[,10],data2[,13],data2[,16],data2[,19]),ncol = 1)
data2_T=matrix(c(data2[,5],data2[,8],data2[,11],data2[,14],data2[,17],data2[,20]),ncol = 1)
data2_E=matrix(c(data2[,6],data2[,9],data2[,12],data2[,15],data2[-5,18],data2[-5,21]),ncol = 1)

pair_t_C=t.test(data1_C, data2_C, paired=TRUE)
pair_t_T=t.test(data1_T, data2_T, paired=TRUE)
pair_t_E=t.test(data1_E, data2_E, paired=TRUE)

palue_C=pair_t_C$p.value
palue_T=pair_t_T$p.value
palue_E=pair_t_E$p.value

pair_t3=data.frame(c(palue_C,palue_T,palue_E))
