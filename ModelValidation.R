
### model control for elastic net with alpha=0.75
residElasTrain<-ytrain- Y_hatTrain075


resid_elas_M<-as.matrix(residElasTrain)
residMeltElas<-melt(resid_elas_M)
Resid20MElas<-residMeltElas
Resid20MElas<-residMeltElas[1:1240,]


BB<-as.data.frame(residElasTrain)

Resid20MElas$Var2<- factor(Resid20MElas$Var2, 
                           levels = c("DX1", "DX3",  "DX4",  "DX5",  "DX6",  "DX7",
                                      "DX8",  "DX9",  "DX10", "DX11", "DX12", "DX13",
                                      "DX14", "DX15","DX16", "DX17" ,"DX18", "DX19",
                                      "DX20", "DX21","DX22", "DX23", "DX24", "DX25","DX26" ,"DX27", "DX28" ,
                                      "DX29", "DX30" ,"DX31" ,"DX32" ,"DX35", "DX36", "DX37", "DX38",
                                      "DX39","DX40" ,"DX41" ,"DX42", "DX43", "DX45", "DX46" ,"DX47", 
                                      "DX48" ,"DX49" ,"DX50" ,"DX51", "DX52", "DX53" ,"DX54", "DX55" ,
                                      "DX56" ,"DX57", "DX58", "DX59", "DX60", "DX61", "DX62", "DX63"), ordered = TRUE, 
                           labels=c(expression(widehat(epsilon)[1],
                                               widehat(epsilon)[2], widehat(epsilon)[3], widehat(epsilon)[4],
                                               widehat(epsilon)[5], widehat(epsilon)[6], widehat(epsilon)[7],
                                               widehat(epsilon)[8],widehat(epsilon)[9], widehat(epsilon)[10],
                                               widehat(epsilon)[11], widehat(epsilon)[12],widehat(epsilon)[13],
                                               widehat(epsilon)[14], widehat(epsilon)[15], widehat(epsilon)[16],
                                               widehat(epsilon)[17], widehat(epsilon)[18], widehat(epsilon)[19],
                                               widehat(epsilon)[20],widehat(epsilon)[21],
                                               widehat(epsilon)[22], widehat(epsilon)[23], widehat(epsilon)[24],
                                               widehat(epsilon)[25], widehat(epsilon)[26], widehat(epsilon)[27],
                                               widehat(epsilon)[28],widehat(epsilon)[29], widehat(epsilon)[30],
                                               widehat(epsilon)[31], widehat(epsilon)[32],widehat(epsilon)[33],
                                               widehat(epsilon)[34], widehat(epsilon)[35], widehat(epsilon)[36],
                                               widehat(epsilon)[37], widehat(epsilon)[38], widehat(epsilon)[39],
                                               widehat(epsilon)[40],widehat(epsilon)[41],
                                               widehat(epsilon)[42], widehat(epsilon)[43], widehat(epsilon)[44],
                                               widehat(epsilon)[45], widehat(epsilon)[46], widehat(epsilon)[47],
                                               widehat(epsilon)[48],widehat(epsilon)[49], widehat(epsilon)[50],
                                               widehat(epsilon)[51], widehat(epsilon)[52],widehat(epsilon)[53],
                                               widehat(epsilon)[54], widehat(epsilon)[55], widehat(epsilon)[56],
                                               widehat(epsilon)[57], widehat(epsilon)[58], widehat(epsilon)[59])))

##  All histograms
HHall<-ggplot(data=Resid20MElas, aes(x=value)) +
  geom_histogram(color="gray", fill="skyblue2", binwidth = 1) +
  stat_theodensity(aes(y = after_stat(count))) +
  scale_x_continuous( breaks=c(-15,0,15))+
  scale_y_continuous( breaks=c(0,10,20))+
  facet_wrap( ~Var2,labeller = label_parsed, ncol=7)+
  theme_bw()+xlab("")
HHall


# 
qqALL<-ggplot(data=Resid20MElas, aes(sample=value)) +
      stat_qq(size=0.75) + stat_qq_line(size=0.5,color="blue")+
        facet_wrap( ~Var2,labeller = label_parsed, ncol=7)+
        theme_bw()+xlab("")+ylab("")
                         
qqALL                           
                       
                           
                           
                  #### the residuals from elastic net 0.75
                           residElasTrain<-ytrain- Y_hatTrain075
                           resid_elas_M<-as.matrix(residElasTrain)
                           ## wilcox.test 
                           p_value<-rep(NA,59)
                           for (i in 1:59){
                             p_value[i]<-(wilcox.test(resid_elas_M[,i]))$p.value
                           }
                           
                           p_values<-as.data.frame(p_value)
                           box1<-ggplot(data=p_values, aes(x=p_value)) +
                             geom_boxplot(fill = "skyblue2")+xlim(c(0,1))+xlab("P-values from Wilcoxon test")+theme_bw()
                           box1
                           
                           ## normal test shapiro.test
                           p_valueNorm<-rep(NA,)
                           for (i in 1:59){
                             p_valueNorm[i]<-(shapiro.test(resid_elas_M[,i]))$p.value
                           }
                           
                           p_valuesNorm<-as.data.frame(p_valueNorm)
                           box2<-ggplot(data=p_valuesNorm, aes(x=p_valueNorm)) +
                             geom_boxplot(fill = "skyblue2")+xlim(c(0,1))+xlab("P-values from Shapiro test")+theme_bw()
                           box2
                           min(p_valueNorm)
                           
                        
                           
                           grid.arrange(box1,box2,ncol=2)
                       
                           
                           
                           ######## try residuals plot with squared residuals
                           
                           a1<-ggplot(data=fit_plus_Resid,aes(x=X1,y=abs(DX1))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,1']))+
                             ylab(expression(paste('|',hat(epsilon)['t,1'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a2<-ggplot(data=fit_plus_Resid,aes(x=X2,y=abs(DX3))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,2']))+
                             ylab(expression(paste('|',hat(epsilon)['t,2'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a3<-ggplot(data=fit_plus_Resid,aes(x=X3,y=abs(DX4))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,3']))+
                             ylab(expression(paste('|',hat(epsilon)['t,3'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a4<-ggplot(data=fit_plus_Resid,aes(x=X4,y=abs(DX5))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,4']))+
                             ylab(expression(paste('|',hat(epsilon)['t,4'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a5<-ggplot(data=fit_plus_Resid,aes(x=X5,y=abs(DX6))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,5']))+
                             ylab(expression(paste('|',hat(epsilon)['t,5'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a6<-ggplot(data=fit_plus_Resid,aes(x=X6,y=abs(DX7))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,6']))+
                             ylab(expression(paste('|',hat(epsilon)['t,6'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a7<-ggplot(data=fit_plus_Resid,aes(x=X7,y=abs(DX8))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,7']))+
                             ylab(expression(paste('|',hat(epsilon)['t,7'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a8<-ggplot(data=fit_plus_Resid,aes(x=X8,y=abs(DX9))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,8']))+
                             ylab(expression(paste('|',hat(epsilon)['t,6'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a9<-ggplot(data=fit_plus_Resid,aes(x=X9,y=abs(DX10))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,9']))+
                             ylab(expression(paste('|',hat(epsilon)['t,9'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a10<-ggplot(data=fit_plus_Resid,aes(x=X10,y=abs(DX11))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,10']))+
                             ylab(expression(paste('|',hat(epsilon)['t,10'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a11<-ggplot(data=fit_plus_Resid,aes(x=X11,y=abs(DX12))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,11']))+
                             ylab(expression(paste('|',hat(epsilon)['t,11'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a12<-ggplot(data=fit_plus_Resid,aes(x=X12,y=abs(DX13))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,12']))+
                             ylab(expression(paste('|',hat(epsilon)['t,12'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a13<-ggplot(data=fit_plus_Resid,aes(x=X13,y=abs(DX14))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,13']))+
                             ylab(expression(paste('|',hat(epsilon)['t,13'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a14<-ggplot(data=fit_plus_Resid,aes(x=X14,y=abs(DX15))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,14']))+
                             ylab(expression(paste('|',hat(epsilon)['t,14'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a15<-ggplot(data=fit_plus_Resid,aes(x=X15,y=abs(DX16))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,15']))+
                             ylab(expression(paste('|',hat(epsilon)['t,15'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           
                           grid.arrange(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,ncol=5)
                           
                           
                           a16<-ggplot(data=fit_plus_Resid,aes(x=X16,y=abs(DX17))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,16']))+
                             ylab(expression(paste('|',hat(epsilon)['t,16'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a17<-ggplot(data=fit_plus_Resid,aes(x=X17,y=abs(DX18))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,17']))+
                             ylab(expression(paste('|',hat(epsilon)['t,17'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a18<-ggplot(data=fit_plus_Resid,aes(x=X18,y=abs(DX19))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,18']))+
                             ylab(expression(paste('|',hat(epsilon)['t,18'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a19<-ggplot(data=fit_plus_Resid,aes(x=X19,y=abs(DX20))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,19']))+
                             ylab(expression(paste('|',hat(epsilon)['t,19'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a20<-ggplot(data=fit_plus_Resid,aes(x=X20,y=abs(DX21))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,20']))+
                             ylab(expression(paste('|',hat(epsilon)['t,20'],'|')))+
                             scale_y_continuous(limits = c(0,10), breaks=c(0,2,4,6,8))+
                             geom_smooth(method = "lm", se = FALSE)
                           a21<-ggplot(data=fit_plus_Resid,aes(x=X21,y=abs(DX22))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,21']))+
                             ylab(expression(paste('|',hat(epsilon)['t,21'],'|')))+
                             scale_y_continuous(limits = c(0,13), breaks=c(0,3,6,9,12))+
                             geom_smooth(method = "lm", se = FALSE)
                           a22<-ggplot(data=fit_plus_Resid,aes(x=X22,y=abs(DX23))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,22']))+
                             ylab(expression(paste('|',hat(epsilon)['t,22'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a23<-ggplot(data=fit_plus_Resid,aes(x=X23,y=abs(DX24))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,23']))+
                             ylab(expression(paste('|',hat(epsilon)['t,23'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a24<-ggplot(data=fit_plus_Resid,aes(x=X24,y=abs(DX25))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,24']))+
                             ylab(expression(paste('|',hat(epsilon)['t,24'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a25<-ggplot(data=fit_plus_Resid,aes(x=X25,y=abs(DX26))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,25']))+
                             ylab(expression(paste('|',hat(epsilon)['t,25'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a26<-ggplot(data=fit_plus_Resid,aes(x=X26,y=abs(DX27))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,26']))+
                             ylab(expression(paste('|',hat(epsilon)['t,26'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a27<-ggplot(data=fit_plus_Resid,aes(x=X27,y=abs(DX28))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,27']))+
                             ylab(expression(paste('|',hat(epsilon)['t,27'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a28<-ggplot(data=fit_plus_Resid,aes(x=X28,y=abs(DX29))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,28']))+
                             ylab(expression(paste('|',hat(epsilon)['t,28'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a29<-ggplot(data=fit_plus_Resid,aes(x=X29,y=abs(DX30))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,29']))+
                             ylab(expression(paste('|',hat(epsilon)['t,29'],'|')))+
                             scale_y_continuous(limits = c(0,11), breaks=c(0,3,6,9,11))+
                             geom_smooth(method = "lm", se = FALSE)
                           a30<-ggplot(data=fit_plus_Resid,aes(x=X30,y=abs(DX31))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,30']))+
                             ylab(expression(paste('|',hat(epsilon)['t,30'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           
                           grid.arrange(a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,a26,a27,a28,a29,a30,ncol=5)
                           
                           
                           a31<-ggplot(data=fit_plus_Resid,aes(x=X31,y=abs(DX32))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,31']))+
                             ylab(expression(paste('|',hat(epsilon)['t,31'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a32<-ggplot(data=fit_plus_Resid,aes(x=X32,y=abs(DX35))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,32']))+
                             ylab(expression(paste('|',hat(epsilon)['t,32'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a33<-ggplot(data=fit_plus_Resid,aes(x=X33,y=abs(DX36))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,33']))+
                             ylab(expression(paste('|',hat(epsilon)['t,33'],'|')))+
                             scale_y_continuous(limits = c(0,9), breaks=c(0,2,4,6,8))+
                             geom_smooth(method = "lm", se = FALSE)
                           a34<-ggplot(data=fit_plus_Resid,aes(x=X34,y=abs(DX37))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,34']))+
                             ylab(expression(paste('|',hat(epsilon)['t,34'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a35<-ggplot(data=fit_plus_Resid,aes(x=X35,y=abs(DX38))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,35']))+
                             ylab(expression(paste('|',hat(epsilon)['t,35'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a36<-ggplot(data=fit_plus_Resid,aes(x=X36,y=abs(DX39))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,36']))+
                             ylab(expression(paste('|',hat(epsilon)['t,36'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a37<-ggplot(data=fit_plus_Resid,aes(x=X37,y=abs(DX40))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,37']))+
                             ylab(expression(paste('|',hat(epsilon)['t,37'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a38<-ggplot(data=fit_plus_Resid,aes(x=X38,y=abs(DX41))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,38']))+
                             ylab(expression(paste('|',hat(epsilon)['t,38'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a39<-ggplot(data=fit_plus_Resid,aes(x=X39,y=abs(DX42))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,39']))+
                             ylab(expression(paste('|',hat(epsilon)['t,39'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a40<-ggplot(data=fit_plus_Resid,aes(x=X40,y=abs(DX43))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,40']))+
                             ylab(expression(paste('|',hat(epsilon)['t,40'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a41<-ggplot(data=fit_plus_Resid,aes(x=X41,y=abs(DX45))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,41']))+
                             ylab(expression(paste('|',hat(epsilon)['t,41'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a42<-ggplot(data=fit_plus_Resid,aes(x=X42,y=abs(DX46))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,42']))+
                             ylab(expression(paste('|',hat(epsilon)['t,42'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a43<-ggplot(data=fit_plus_Resid,aes(x=X43,y=abs(DX47))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,43']))+
                             ylab(expression(paste('|',hat(epsilon)['t,43'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a44<-ggplot(data=fit_plus_Resid,aes(x=X44,y=abs(DX48))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,44']))+
                             ylab(expression(paste('|',hat(epsilon)['t,44'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a45<-ggplot(data=fit_plus_Resid,aes(x=X45,y=abs(DX49))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,45']))+
                             ylab(expression(paste('|',hat(epsilon)['t,45'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           
                           grid.arrange(a31,a32,a33,a34,a35,a36,a37,a38,a39,a40,a41,a42,a43,a44,a45,ncol=5)
                           
                           
                           a46<-ggplot(data=fit_plus_Resid,aes(x=X46,y=abs(DX50))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,46']))+
                             ylab(expression(paste('|',hat(epsilon)['t,46'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a47<-ggplot(data=fit_plus_Resid,aes(x=X47,y=abs(DX51))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,47']))+
                             ylab(expression(paste('|',hat(epsilon)['t,47'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a48<-ggplot(data=fit_plus_Resid,aes(x=X48,y=abs(DX52))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,48']))+
                             ylab(expression(paste('|',hat(epsilon)['t,48'],'|')))+
                             #scale_y_continuous(limits = c(0,9), breaks=c(0,2,4,6,8))+
                             geom_smooth(method = "lm", se = FALSE)
                           a49<-ggplot(data=fit_plus_Resid,aes(x=X49,y=abs(DX53))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,49']))+
                             ylab(expression(paste('|',hat(epsilon)['t,49'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a50<-ggplot(data=fit_plus_Resid,aes(x=X50,y=abs(DX54))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,50']))+
                             ylab(expression(paste('|',hat(epsilon)['t,50'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a51<-ggplot(data=fit_plus_Resid,aes(x=X51,y=abs(DX55))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,51']))+
                             ylab(expression(paste('|',hat(epsilon)['t,51'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a52<-ggplot(data=fit_plus_Resid,aes(x=X52,y=abs(DX56))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,52']))+
                             ylab(expression(paste('|',hat(epsilon)['t,52'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           a53<-ggplot(data=fit_plus_Resid,aes(x=X53,y=abs(DX57))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,53']))+
                             ylab(expression(paste('|',hat(epsilon)['t,53'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a54<-ggplot(data=fit_plus_Resid,aes(x=X54,y=abs(DX58))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,54']))+
                             ylab(expression(paste('|',hat(epsilon)['t,54'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a55<-ggplot(data=fit_plus_Resid,aes(x=X55,y=abs(DX59))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,55']))+
                             ylab(expression(paste('|',hat(epsilon)['t,55'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a56<-ggplot(data=fit_plus_Resid,aes(x=X56,y=abs(DX60))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,56']))+
                             ylab(expression(paste('|',hat(epsilon)['t,56'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a57<-ggplot(data=fit_plus_Resid,aes(x=X57,y=abs(DX61))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,57']))+
                             ylab(expression(paste('|',hat(epsilon)['t,57'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a58<-ggplot(data=fit_plus_Resid,aes(x=X58,y=abs(DX62))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,58']))+
                             ylab(expression(paste('|',hat(epsilon)['t,58'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           a59<-ggplot(data=fit_plus_Resid,aes(x=X59,y=abs(DX63))) + 
                             geom_point() +theme_bw()+xlab(expression(Delta~ hat(X)['t,59']))+
                             ylab(expression(paste('|',hat(epsilon)['t,59'],'|')))+
                             geom_smooth(method = "lm", se = FALSE)
                           
                           grid.arrange(a46,a47,a48,a49,a50,a51,a52,a53,a54,a55,a56,a57,a58,a59,ncol=5)
                           
                           