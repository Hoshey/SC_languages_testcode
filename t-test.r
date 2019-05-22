########(????????)
###(6?غA??)
#t-test
data1=read.table("for_R/language_pretest_attitude2.csv" , sep=',',header = T)
data2=read.table("for_R/language_posttest_attitude2.csv" , sep=',',header = T)


pvalue1 = array(NA, 18)
pvalue2 = array(NA, 18)
for(i in 4:21){
  if(i==17 || i==18 || i==20 || i==21){
    t1=t.test(data1[-5,i], alternative = "two.sided", mu=0)
    pvalue1[i-3] =t1$p.value
  }else{
    t1=t.test(data1[,i],alternative = "two.sided", mu=0)
    pvalue1[i-3] =t1$p.value
  }
}


for(j in 4:21){
  if(j==17 || j==18 || j==20 || j==21){
    t2=t.test(data2[-5,j], alternative = "two.sided", mu=0)
    pvalue2[j-3] = t2$p.value
  }else{
    t2=t.test(data2[,j],alternative = "two.sided", mu=0)
    pvalue2[j-3] = t2$p.value
  }
}

t.test1=data.frame(pvalue1,pvalue2)

#pair t
data1_abs=abs(data1[,4:21])
data2_abs=abs(data2[,4:21])
pvalue = array(NA, 18)
for(i in 1:18){
  if(i==14 || i==15 || i==17 || i==18){
    pair_t1=t.test(data1_abs[-5,i],data2_abs[-5,i], paired=TRUE)
    pvalue[i] = pair_t1$p.value
  }else{
    pair_t1=t.test(data1_abs[-5,i],data2_abs[-5,i], paired=TRUE)
    pvalue[i] = pair_t1$p.value
  }
}
pair_t1=data.frame(pvalue)


###(??????)
#t-test
data1_domC=matrix(c(data1[,4],data1[,10],data1[,16]),ncol = 1)
data1_domT=matrix(c(data1[,5],data1[,11],data1[-5,17]),ncol = 1)
data1_domE=matrix(c(data1[,6],data1[,12],data1[-5,18]),ncol = 1)
data1_recC=matrix(c(data1[,7],data1[,13],data1[,19]),ncol = 1)
data1_recT=matrix(c(data1[,8],data1[,14],data1[-5,20]),ncol = 1)
data1_recE=matrix(c(data1[,9],data1[,15],data1[-5,21]),ncol = 1)

data2_domC=matrix(c(data2[,4],data2[,10],data2[,16]),ncol = 1)
data2_domT=matrix(c(data2[,5],data2[,11],data2[-5,17]),ncol = 1)
data2_domE=matrix(c(data2[,6],data2[,12],data2[-5,18]),ncol = 1)
data2_recC=matrix(c(data2[,7],data2[,13],data2[,19]),ncol = 1)
data2_recT=matrix(c(data2[,8],data2[,14],data2[-5,20]),ncol = 1)
data2_recE=matrix(c(data2[,9],data2[,15],data2[-5,21]),ncol = 1)

t1_domC=t.test(data1_domC, alternative = "two.sided", mu=0)
t1_domT=t.test(data1_domT, alternative = "two.sided", mu=0)
t1_domE=t.test(data1_domE, alternative = "two.sided", mu=0)
t1_recC=t.test(data1_recC, alternative = "two.sided", mu=0)
t1_recT=t.test(data1_recT, alternative = "two.sided", mu=0)
t1_recE=t.test(data1_recE, alternative = "two.sided", mu=0)

t2_domC=t.test(data2_domC, alternative = "two.sided", mu=0)
t2_domT=t.test(data2_domT, alternative = "two.sided", mu=0)
t2_domE=t.test(data2_domE, alternative = "two.sided", mu=0)
t2_recC=t.test(data2_recC, alternative = "two.sided", mu=0)
t2_recT=t.test(data2_recT, alternative = "two.sided", mu=0)
t2_recE=t.test(data2_recE, alternative = "two.sided", mu=0)

palue1_domC=t1_domC$p.value
palue1_domT=t1_domT$p.value
palue1_domE=t1_domE$p.value
palue1_recC=t1_recC$p.value
palue1_recT=t1_recT$p.value
palue1_recE=t1_recE$p.value

palue2_domC=t2_domC$p.value
palue2_domT=t2_domT$p.value
palue2_domE=t2_domE$p.value
palue2_recC=t2_recC$p.value
palue2_recT=t2_recT$p.value
palue2_recE=t2_recE$p.value

t.test2=data.frame(c(palue1_domC,palue1_domT,palue1_domE,palue1_recC,palue1_recT,palue1_recE),
                  c(palue2_domC,palue2_domT,palue2_domE,palue2_recC,palue2_recT,palue2_recE))

#pair t
data1_domC=matrix(c(data1_abs[,1],data1_abs[,7],data1_abs[,13]),ncol = 1)
data1_domT=matrix(c(data1_abs[,2],data1_abs[,8],data1_abs[-5,14]),ncol = 1)
data1_domE=matrix(c(data1_abs[,3],data1_abs[,9],data1_abs[-5,15]),ncol = 1)
data1_recC=matrix(c(data1_abs[,4],data1_abs[,10],data1_abs[,16]),ncol = 1)
data1_recT=matrix(c(data1_abs[,5],data1_abs[,11],data1_abs[-5,17]),ncol = 1)
data1_recE=matrix(c(data1_abs[,6],data1_abs[,12],data1_abs[-5,18]),ncol = 1)

data2_domC=matrix(c(data2_abs[,1],data2_abs[,7],data2_abs[,13]),ncol = 1)
data2_domT=matrix(c(data2_abs[,2],data2_abs[,8],data2_abs[-5,14]),ncol = 1)
data2_domE=matrix(c(data2_abs[,3],data2_abs[,9],data2_abs[-5,15]),ncol = 1)
data2_recC=matrix(c(data2_abs[,4],data2_abs[,10],data2_abs[,16]),ncol = 1)
data2_recT=matrix(c(data2_abs[,5],data2_abs[,11],data2_abs[-5,17]),ncol = 1)
data2_recE=matrix(c(data2_abs[,6],data2_abs[,12],data2_abs[-5,18]),ncol = 1)

t_domC=t.test(data1_domC,data2_domC, paired=TRUE)
t_domT=t.test(data1_domT,data2_domT, paired=TRUE)
t_domE=t.test(data1_domE,data2_domE, paired=TRUE)
t_recC=t.test(data1_recC,data2_recC, paired=TRUE)
t_recT=t.test(data1_recT,data2_recT, paired=TRUE)
t_recE=t.test(data1_recE,data2_recE, paired=TRUE)

palue_domC=t_domC$p.value
palue_domT=t_domT$p.value
palue_domE=t_domE$p.value
palue_recC=t_recC$p.value
palue_recT=t_recT$p.value
palue_recE=t_recE$p.value

pair_t2=data.frame(c(palue_domC,palue_domT,palue_domE,palue_recC,palue_recT,palue_recE))

###(3?ػy??)
#t-test
data1_C=matrix(c(data1[,4],data1[,7],data1[,10],data1[,13],data1[,16],data1[,19]),ncol = 1)
data1_T=matrix(c(data1[,5],data1[,8],data1[,11],data1[,14],data1[-5,17],data1[-5,20]),ncol = 1)
data1_E=matrix(c(data1[,6],data1[,9],data1[,12],data1[,15],data1[-5,18],data1[-5,21]),ncol = 1)

data2_C=matrix(c(data2[,4],data2[,7],data2[,10],data2[,13],data2[,16],data2[,19]),ncol = 1)
data2_T=matrix(c(data2[,5],data2[,8],data2[,11],data2[,14],data2[-5,17],data2[-5,20]),ncol = 1)
data2_E=matrix(c(data2[,6],data2[,9],data2[,12],data2[,15],data2[-5,18],data2[-5,21]),ncol = 1)

pair_t1_C=t.test(data1_C, alternative = "two.sided", mu=0)
pair_t1_T=t.test(data1_T, alternative = "two.sided", mu=0)
pair_t1_E=t.test(data1_E, alternative = "two.sided", mu=0)

pair_t2_C=t.test(data2_C, alternative = "two.sided", mu=0)
pair_t2_T=t.test(data2_T, alternative = "two.sided", mu=0)
pair_t2_E=t.test(data2_E, alternative = "two.sided", mu=0)


palue1_C=pair_t1_C$p.value
palue1_T=pair_t1_T$p.value
palue1_E=pair_t1_E$p.value
palue2_C=pair_t2_C$p.value
palue2_T=pair_t2_T$p.value
palue2_E=pair_t2_E$p.value

t.test3=data.frame(c(palue1_C,palue1_T,palue1_E),c(palue2_C,palue2_T,palue2_E))

#pair t
data1_abs=abs(data1[,4:21])
data2_abs=abs(data2[,4:21])

data1_C=matrix(c(data1_abs[,1],data1_abs[,4],data1_abs[,7],data1_abs[,10],data1_abs[,13],data1_abs[,16]),ncol = 1)
data1_T=matrix(c(data1_abs[,2],data1_abs[,5],data1_abs[,8],data1_abs[,11],data1_abs[-5,14],data1_abs[-5,17]),ncol = 1)
data1_E=matrix(c(data1_abs[,3],data1_abs[,6],data1_abs[,9],data1_abs[,12],data1_abs[-5,15],data1_abs[-5,18]),ncol = 1)

data2_C=matrix(c(data2_abs[,1],data2_abs[,4],data2_abs[,7],data2_abs[,10],data2_abs[,13],data2_abs[,16]),ncol = 1)
data2_T=matrix(c(data2_abs[,2],data2_abs[,5],data2_abs[,8],data2_abs[,11],data2_abs[-5,14],data2_abs[-5,17]),ncol = 1)
data2_E=matrix(c(data2_abs[,3],data2_abs[,6],data2_abs[,9],data2_abs[,12],data2_abs[-5,15],data2_abs[-5,18]),ncol = 1)

t_C=t.test(data1_C,data2_C, paired=TRUE)
t_T=t.test(data1_T,data2_T, paired=TRUE)
t_E=t.test(data1_E,data2_E, paired=TRUE)

palue_C=t_C$p.value
palue_T=t_T$p.value
palue_E=t_E$p.value

pair_t3=data.frame(c(palue_C,palue_T,palue_E))