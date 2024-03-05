########################################################################################################################
## REVISING THE BORGATTI-EVERETT CORE-PERIPHERY MODEL
## (1) Figures
## R script written by Jose Luis Estevez (Vaestoliitto)
## Date: Mar 5th, 2024
########################################################################################################################

# R PACKAGES REQUIRED
library(ggplot2);library(data.table);library(ggpubr);library(igraph)

# CLEAN ENVIRONMENT
rm(list=ls())

########################################################################################################################

# The three networks 

# BEFIG1
BEfig1 <- matrix(data=c(1,1,1,1,1,0,0,0,0,0,
                        1,1,1,1,0,1,1,1,0,0,
                        1,1,1,1,0,0,0,1,1,0,
                        1,1,1,1,1,0,0,0,0,1,
                        1,0,0,1,0,0,0,0,0,0,
                        0,1,0,0,0,0,0,0,0,0,
                        0,1,0,0,0,0,0,0,0,0,
                        0,1,1,0,0,0,0,0,0,0,
                        0,0,1,0,0,0,0,0,0,0,
                        0,0,0,1,0,0,0,0,0,0),
                 nrow=10,ncol=10,dimnames=list(1:10,1:10),byrow = TRUE)
# Send the diagonal to NA
diag(BEfig1) <- NA

# BAKER 
baker <- read.table('baker.txt',header=TRUE,sep='\t')
baker <- as.matrix(baker[,-1])
rownames(baker) <- colnames(baker) <- toupper(colnames(baker))

# Send the diagonal to NA
diag(baker) <- NA

# GALTUNG NETWORK

names <- c(paste('c',1:4,sep=''),paste('p',c(11,12,21,22,31,32,33,41,42),sep=''))
galtung <- matrix(data=c(1,1,0,1,1,1,0,0,0,0,0,0,0,
                         1,1,1,0,0,0,1,1,0,0,0,0,0,
                         0,1,1,1,0,0,0,0,1,1,1,0,0,
                         1,0,1,1,0,0,0,0,0,0,0,1,1,
                         1,0,0,0,0,0,0,0,0,0,0,0,0,
                         1,0,0,0,0,0,0,0,0,0,0,0,0,
                         0,1,0,0,0,0,0,0,0,0,0,0,0,
                         0,1,0,0,0,0,0,0,0,0,0,0,0,
                         0,0,1,0,0,0,0,0,0,0,0,0,0,
                         0,0,1,0,0,0,0,0,0,0,0,0,0,
                         0,0,1,0,0,0,0,0,0,0,0,0,0,
                         0,0,0,1,0,0,0,0,0,0,0,0,0,
                         0,0,0,1,0,0,0,0,0,0,0,0,0),
                  nrow=13,ncol=13,dimnames=list(names,names),byrow = TRUE)
# Send the diagonal to NA
diag(galtung) <- NA

########################################################################################################################

# NETWORK VISUALIZATIONS

f1 <- graph_from_adjacency_matrix(BEfig1,mode='undirected')
f2 <- graph_from_adjacency_matrix(baker,mode='undirected')
f3 <- graph_from_adjacency_matrix(galtung,mode='undirected')

jpeg(filename="Fig1.jpeg",width=12,height=4.5,units='in',res=500)
par(mfrow=c(1,3))
set.seed(0708)
plot.igraph(f1,
            vertex.color='grey80',vertex.size=28,
            edge.color='grey20',edge.width=3,
            layout=layout_with_fr(f1),
            main='BEfig1 example')
plot.igraph(f2,
            vertex.color='grey80',vertex.size=28,
            edge.color='grey20',edge.width=3,
            layout=layout_with_kk(f2),
            main="Baker's binary example")
plot.igraph(f3,
            vertex.color='grey80',vertex.size=28,
            edge.color='grey20',edge.width=3,
            layout=layout_with_fr(f3),
            main="Galtung's feudal structure example")
dev.off()

rm(f1);rm(f2);rm(f3)
par(mfrow=c(1,1))

########################################################################################################################

# PARTITION VISUALIZATIONS

# BEFIG1

# Turn the matrix into data table
BEfig1dt <- as.data.table(reshape2::melt(BEfig1))

# Turn the values to a factor
BEfig1dt[,Var1 := factor(Var1)]
BEfig1dt[,Var2 := factor(Var2,levels=rev(1:10))]

# Visualizaton
# Set members of core, and members of periphery
core <- paste(1:4)
periphery <- paste(5:10)

p1 <- ggplot(data = BEfig1dt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(BEfig1dt$Var1 == BEfig1dt$Var2,'grey40',
                          ifelse(BEfig1dt$Var1 %in% core & BEfig1dt$Var2 %in% core,'white',
                                 ifelse(BEfig1dt$Var1 %in% periphery & BEfig1dt$Var2 %in% periphery,'white','grey90')))) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,10.5), color = 'black') +
  geom_hline(yintercept = c(0.5,10.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 4.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 6.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(BEfig1dt$value == 1,value,''))) +
  theme_minimal() +
  theme(legend.position = 'none')

ggsave("Fig2.png", p1, width = 4, height = 4)

#############################

# BAKER

# Turn the matrix into data table
bakerdt <- as.data.table(reshape2::melt(baker))

# Turn the values to a factor
bakerdt[,Var1 := factor(Var1)]
bakerdt[,Var2 := factor(Var2,levels=rev(levels(bakerdt$Var1)))]

# VisualizatiOn
# Set members of core, and members of periphery
core <- rownames(baker)[1:7]
periphery <- rownames(baker)[!(rownames(baker) %in% core)]

p2 <- ggplot(data = bakerdt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(bakerdt$Var1 == bakerdt$Var2,'grey40',
                          ifelse(bakerdt$Var1 %in% core & bakerdt$Var2 %in% core,'white',
                                 ifelse(bakerdt$Var1 %in% periphery & bakerdt$Var2 %in% periphery,'white','grey90')))) +  
  # borders of the matrix
  geom_vline(xintercept = c(0.5,20.5), color = 'black') +
  geom_hline(yintercept = c(0.5,20.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 7.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 13.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(bakerdt$value == 1,value,''))) +
  theme_minimal() +
  theme(legend.position = 'none') + 
  theme(axis.text.x = element_text(vjust = 1, angle = 90))  

#############################

# GALTUNG

# Turn the matrix into data table
galtungdt <- as.data.table(reshape2::melt(galtung))

# Turn the values to a factor
galtungdt[,Var1 := factor(Var1)]
galtungdt[,Var2 := factor(Var2,levels=rev(levels(galtungdt$Var1)))]

# Visualizaton
# Set members of core, and members of periphery
core <- rownames(galtung)[1:4]
periphery <- rownames(galtung)[!(rownames(galtung) %in% core)]

p3 <- ggplot(data = galtungdt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(galtungdt$Var1 == galtungdt$Var2,'grey40',
                          ifelse(galtungdt$Var1 %in% core & galtungdt$Var2 %in% core,'white',
                                 ifelse(galtungdt$Var1 %in% periphery & galtungdt$Var2 %in% periphery,'white','grey90')))) + 
  # borders of the matrix
  geom_vline(xintercept = c(0.5,13.5), color = 'black') +
  geom_hline(yintercept = c(0.5,13.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,12.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,12.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 4.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 9.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(galtungdt$value == 1,value,''))) +
  theme_minimal() +
  theme(legend.position = 'none')

ggsave("Fig3.png",
       ggarrange(p2,p3,ncol=2,labels=c('A','B')),
       width =12, height = 6)

########################################################################################################################

# ALTERNATIVE IDEAL PATTERNS
core <- paste(1:4)
periphery <- paste(5:10)

# BEfig2
BEfig1dt[,ideal1 := ifelse(Var1 == Var2,NA,
                           ifelse(Var1 %in% core & Var2 %in% core,1,
                                  ifelse(Var1 %in% periphery & Var2 %in% periphery,0,0)))]
BEfig1dt[,ideal2 := ifelse(Var1 == Var2,NA,
                           ifelse(Var1 %in% core & Var2 %in% core,1,
                                  ifelse(Var1 %in% periphery & Var2 %in% periphery,0,1)))]
BEfig1dt[,ideal3 := ifelse(Var1 == Var2,NA,
                           ifelse(Var1 %in% core & Var2 %in% core,1,
                                  ifelse(Var1 %in% periphery & Var2 %in% periphery,0,1/3)))]

# Visualization
i1 <- ggplot(data = BEfig1dt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(BEfig1dt$Var1 == BEfig1dt$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,10.5), color = 'black') +
  geom_hline(yintercept = c(0.5,10.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 4.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 6.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(BEfig1dt$ideal1 == 1,ideal1,''))) +
  theme_minimal() +
  theme(legend.position = 'none')

i2 <- ggplot(data = BEfig1dt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(BEfig1dt$Var1 == BEfig1dt$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,10.5), color = 'black') +
  geom_hline(yintercept = c(0.5,10.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 4.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 6.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(BEfig1dt$ideal2 == 1,ideal2,''))) +
  theme_minimal() +
  theme(legend.position = 'none')

i3 <- ggplot(data = BEfig1dt, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(BEfig1dt$Var1 == BEfig1dt$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,10.5), color = 'black') +
  geom_hline(yintercept = c(0.5,10.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,9.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 4.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 6.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(BEfig1dt$ideal3 == 1,'1',
                               ifelse(BEfig1dt$ideal3 == 1/3,'1/3','')))) +
  theme_minimal() +
  theme(legend.position = 'none')

ggsave("Fig4.png",
       ggarrange(i1,i2,i3,ncol=3,labels=c('A','B','C')),
       width =12, height = 4)

########################################################################################################################

# BLOCK DENSITIES

# Ideal Blockmodel
reducedblock <- data.table(Var1 = rep(c('core','periphery'),each=2),
                           Var2 = rep(c('core','periphery'),times=2),
                           value = c(1,NA,NA,0))

reducedblock[,Var1 := factor(Var1)]
reducedblock[,Var2 := factor(Var2,levels=rev(levels(reducedblock$Var1)))]

# Reduced BEfig1
reducedBEfig1 <- reducedblock

reducedBEfig1$value[1] <- round(mean(BEfig1[1:4,1:4],na.rm=TRUE),4) # core
reducedBEfig1$value[2] <- reducedBEfig1$value[3] <- round(mean(BEfig1[5:10,1:4]),4) # cp/pc
reducedBEfig1$value[4] <- mean(BEfig1[5:10,5:10],na.rm=TRUE) # periphery

p3 <- ggplot(data = reducedBEfig1, aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(reducedBEfig1$Var1 == reducedBEfig1$Var2,'white','grey90')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,2.5), color = 'black') +
  geom_hline(yintercept = c(0.5,2.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = reducedBEfig1$value)) +
  theme_minimal() +
  theme(legend.position = 'none')

#########################

# Reduced Baker
reducedbaker <- reducedblock

reducedbaker$value[1] <- round(mean(baker[1:7,1:7],na.rm=TRUE),2) # core
reducedbaker$value[2] <- reducedbaker$value[3] <- round(mean(baker[8:20,1:7]),4) # cp/pc
reducedbaker$value[4] <- round(mean(baker[8:20,8:20],na.rm=TRUE),2) # periphery

p4 <- ggplot(data = reducedbaker, aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(reducedbaker$Var1 == reducedbaker$Var2,'white','grey90')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,2.5), color = 'black') +
  geom_hline(yintercept = c(0.5,2.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = reducedbaker$value)) +
  theme_minimal() +
  theme(legend.position = 'none')

#########################

# Reduced Galtung
reducedgaltung <- reducedblock

reducedgaltung$value[1] <- round(mean(galtung[1:4,1:4],na.rm=TRUE),4) # core
reducedgaltung$value[2] <- reducedgaltung$value[3] <- mean(galtung[5:13,1:4]) # cp/pc
reducedgaltung$value[4] <- mean(galtung[5:13,5:13],na.rm=TRUE) # periphery

p5 <- ggplot(data = reducedgaltung, aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(reducedgaltung$Var1 == reducedgaltung$Var2,'white','grey90')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,2.5), color = 'black') +
  geom_hline(yintercept = c(0.5,2.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 1.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = reducedgaltung$value)) +
  theme_minimal() +
  theme(legend.position = 'none')

ggarrange(p3,p4,p5,ncol=3,labels=c('A','B','C'),width =10, height = 3)

########################################################################################################################

# ALTERNATIVE SOLUTIONS (BAKER)

# SECOND VERSION
neworder <- c(rownames(baker)[1:8],'PW','ASW',rownames(baker)[c(9:11,13,15:20)])

bakerdt2 <- bakerdt
bakerdt2[,Var1 := factor(Var1,levels=neworder)]
bakerdt2[,Var2 := factor(Var2,levels=rev(levels(bakerdt2$Var1)))]

# VisualizatiOn
core <- c(rownames(baker)[1:8],'PW','ASW')
periphery <- rownames(baker)[!(rownames(baker) %in% core)]

ggplot(data = bakerdt2, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(bakerdt2$Var1 == bakerdt2$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,20.5), color = 'black') +
  geom_hline(yintercept = c(0.5,20.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 10.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 10.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(bakerdt2$value == 1,value,''))) +
  theme_minimal() +
  theme(legend.position = 'none') + 
  theme(axis.text.x = element_text(vjust = 1, angle = 90))  

#########################

# THIRD VERSION
neworder2 <- c(rownames(baker)[2:5],'SW','CW','SWRA','CAN','FR','CSWJ','AMH','ASW','BJSW','PW',rownames(baker)[15:20])

bakerdt3 <- bakerdt
bakerdt3[,Var1 := factor(Var1,levels=neworder2)]
bakerdt3[,Var2 := factor(Var2,levels=rev(levels(bakerdt3$Var1)))]

# VisualizatiOn
core <- c('CYSR','JSWE','SSR','SCW','SW')
periphery <- rownames(baker)[!(rownames(baker) %in% core)]

ggplot(data = bakerdt3, aes(x = Var1, y = Var2)) +
  geom_tile(fill = ifelse(bakerdt3$Var1 == bakerdt3$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,20.5), color = 'black') +
  geom_hline(yintercept = c(0.5,20.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,19.5,by=1), color = 'grey80') +
  # partition of the blockmodel
  geom_vline(xintercept = 5.5, color = 'red',linetype='dashed',linewidth=1) +
  geom_hline(yintercept = 15.5, color = 'red',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(bakerdt3$value == 1,value,''))) +
  theme_minimal() +
  theme(legend.position = 'none') + 
  theme(axis.text.x = element_text(vjust = 1, angle = 90))  

########################################################################################################################

# FIGURE 5

# Ideal-pattern when inter-categorical ties are set to .2833 and .3833
BEfig1dt[,ideal4 := ifelse(ideal3 == 1/3,0.2833,ideal3)]
BEfig1dt[,ideal5 := ifelse(ideal3 == 1/3,0.3833,ideal3)]

# Corr test
cor.test(BEfig1dt$value,BEfig1dt$ideal3)
cor.test(BEfig1dt$value,BEfig1dt$ideal4)
cor.test(BEfig1dt$value,BEfig1dt$ideal5)

# To identify the block of each pair...
BEfig1dt[,block := ifelse(Var1 %in% 1:4 & Var2 %in% 1:4,'Core',
                 ifelse(Var1 %in% 5:10 & Var2 %in% 5:10,'Periphery','Inter-categorical'))]

ip1<- ggplot(data=BEfig1dt,aes(x=value,y=ideal3)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.3333")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 0.6686")) +  
  labs(x='',y='',color='Block') +
  theme_bw() + theme(legend.position = 'top')

ip2 <- ggplot(data=BEfig1dt,aes(x=value,y=ideal4)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.2833")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 0.6664")) +  
  labs(x='',y='',color='Block') +
  theme_bw() + theme(legend.position = 'top') 

ip3 <- ggplot(data=BEfig1dt,aes(x=value,y=ideal5)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.3833")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 0.6665")) +  
  labs(x='Observed values',y='',color='Block') +
  theme_bw() + theme(legend.position = 'top')

##########################

# EXACT-DENSITY BLOCK

BEfig1dt[,edb1 := value]
BEfig1dt[,edb2 := value]

# Remove a tie
BEfig1dt[Var1 == 1 & Var2 == 5]$edb1 <- BEfig1dt[Var1 == 5 & Var2 == 1]$edb1 <- 0
# Add a tie
BEfig1dt[Var1 == 1 & Var2 == 6]$edb2 <- BEfig1dt[Var1 == 6 & Var2 == 1]$edb2 <- 1

# Correlation
cor.test(BEfig1dt$value,BEfig1dt$value)
cor.test(BEfig1dt$value,BEfig1dt$edb1)
cor.test(BEfig1dt$value,BEfig1dt$edb2)

edb1 <- ggplot(data=BEfig1dt,aes(x=value,y=value)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.3333")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 1")) +  
  labs(x='',y='Values in the ideal pattern',color='Block') +
  theme_bw() + theme(legend.position = 'top')

edb2 <- ggplot(data=BEfig1dt,aes(x=value,y=edb1)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.2833")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 0.9484")) +  
  labs(x='',y='Values in the ideal pattern',color='Block') +
  theme_bw() + theme(legend.position = 'top')

edb3 <- ggplot(data=BEfig1dt,aes(x=value,y=edb2)) +
  geom_smooth(method='lm',se=FALSE,color='grey20') +
  geom_jitter(aes(color=block),width = 0.02, height = 0.02,alpha = 3/4) +
  annotate("text",x=.15,y=.9,label = parse(text = "d == 0.3833")) +
  annotate("text",x=.15,y=.8,label = parse(text = "rho == 0.9504")) +  
  labs(x='Observed values',y='Values in the ideal pattern',color='Block') +
  theme_bw() + theme(legend.position = 'top')

ggsave("Fig5.png",
       ggarrange(edb1,ip1,edb2,ip2,edb3,ip3,
                 nrow=3,ncol=2,labels=c('A','B','C','D','E','F'),common.legend = TRUE),
       width=6,height=9)

########################################################################################################################

# P-CORE

core <- data.table(Var1 = rep(1:5,time=5),
                   Var2 = rep(1:5,each=5),
                   value = c(NA,1,1,0,1,
                             0,NA,0,1,1,
                             1,1,NA,1,1,
                             1,0,0,NA,0,
                             0,1,0,1,NA))

core[,Var1 := factor(Var1)]
core[,Var2 := factor(Var2,levels=rev(1:5))]

p1 <- ggplot(data = core,
             aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(core$Var1 == core$Var2,'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,5.5), color = 'black') +
  geom_hline(yintercept = c(0.5,5.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(core$value == 1,1,''))) +
  theme_minimal() +
  theme(legend.position = 'none',axis.text.x=element_blank(),axis.text.y=element_blank())

# Rearranged the values
core[,value2 := c(1,1,1,0,NA,
                  1,1,0,0,NA,
                  1,1,1,1,NA,
                  1,0,0,0,NA,
                  1,1,0,0,NA)]

p2 <- ggplot(data = core,
             aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(is.na(core$value2),'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,5.5), color = 'black') +
  geom_hline(yintercept = c(0.5,5.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(core$value2 == 1,1,''))) +
  theme_minimal() +
  theme(legend.position = 'none',axis.text.x=element_blank(),axis.text.y=element_blank())

core[,value3 := c(1,1,1,1,1,
                  1,1,0,1,1,
                  0,1,0,1,1,
                  0,0,0,0,0,
                  NA,NA,NA,NA,NA)]

p3 <- ggplot(data = core,
             aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(is.na(core$value3),'grey40','white')) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,5.5), color = 'black') +
  geom_hline(yintercept = c(0.5,5.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(core$value3 == 1,1,''))) +
  theme_minimal() +
  theme(legend.position = 'none',axis.text.x=element_blank(),axis.text.y=element_blank())

# Ideal blocks
core[,ideal1 := c(1,1,1,0,NA,
                  1,1,0,0,NA,
                  1,1,1,1,NA,
                  1,1,0,0,NA,
                  1,1,0,0,NA)]

p4 <- ggplot(data = core,
       aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(is.na(core$value2),'grey40',
                        ifelse(core$Var1 %in% 1:2, 'darkseagreen1','white'))) +
  # borders of the matrix
  geom_vline(xintercept = c(0.5,5.5), color = 'black') +
  geom_hline(yintercept = c(0.5,5.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  # Cut-off point (0.75 p-core)
  geom_vline(xintercept = 2.5, color = 'darkseagreen4',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(core$ideal1 == 1,1,''))) +
  theme_minimal() +
  theme(legend.position = 'none',axis.text.x=element_blank(),axis.text.y=element_blank())

core[,ideal2 := c(1,1,1,1,1,
                  1,1,1,1,1,
                  0,1,0,1,1,
                  0,0,0,0,0,
                  NA,NA,NA,NA,NA)]

p5 <- ggplot(data = core,
       aes(x = Var1, y = Var2)) +
  geom_tile(fill=ifelse(is.na(core$value3),'grey40',
                        ifelse(core$Var2 %in% 1:2, 'darkseagreen1','white'))) +  # borders of the matrix
  geom_vline(xintercept = c(0.5,5.5), color = 'black') +
  geom_hline(yintercept = c(0.5,5.5), color = 'black') +
  # border of the cells
  geom_vline(xintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  geom_hline(yintercept = seq(1.5,4.5,by=1), color = 'grey80') +
  # Cut-off point (0.75 p-core)
  geom_hline(yintercept = 3.5, color = 'darkseagreen4',linetype='dashed',linewidth=1) +
  labs(x = '', y = '') +
  scale_x_discrete(position = "top") +
  geom_text(aes(label = ifelse(core$ideal2 == 1,1,''))) +
  theme_minimal() +
  theme(legend.position = 'none',axis.text.x=element_blank(),axis.text.y=element_blank())

# Visualization
empty_plot <- ggplot() + theme_void()

ggsave("Fig6.png",
       ggarrange(ggarrange(empty_plot,p1,empty_plot,ncol=1,labels=c('','A',''),heights = c(1,2,1)),
                 ggarrange(p2,p4,p3,p5,nrow=2,ncol=2,labels=c('B','D','C','E')),
                 ncol=2,widths = c(1,2)),
       width=6,height=4)

########################################################################################################################