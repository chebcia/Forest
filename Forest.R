library(knitr)
library(e1071)
library(mltools)
library(data.table)
library(corrplot)
library(ggplot2)
library(dplyr)
table = read.table("C:/Users/Natalia Cheba/Desktop/Forest/Forest_Project/data/covtype.data", sep = ",");
#read.table("C:\\pliki\\covtype.data", sep = ",");
colnames(table) = c('Elevation', 'Aspect', 'Slope', 'Horizontal_Distance_To_Hydrology','Vertical_Distance_To_Hydrology', 'Horizontal_Distance_To_Roadways', 'Hillshade_9am', 'Hillshade_Noon', 'Hillshade_3pm', 'Horizontal_Distance_To_Fire_Points', 'WA_1', 'WA_2', 'WA_3', 'WA_4', seq(1,40,1), 'Cover_Type')



summary(table)

basic_statistics <- function(v){
  mean <- mean(v)
  median <- median(v)
  mode <- mode(v)
  quantile1 <- as.numeric(quantile(v, probs = 0.25))
  quantile3 <- as.numeric(quantile(v, probs = 0.75))
  variation <- var(v)
  std_deviation <- sqrt(variation)
  kurtosis <- kurtosis(v)
  
  return(c(mean,median,mode,quantile1,quantile3,variation,std_deviation,kurtosis))
}

tmp = data.frame(basic_statistics(table$Elevation), basic_statistics(table$Aspect))
row.names(tmp) <- c('mean', 'median', 'mode', 'quantile1','quantile3','variation','std_deviation','kurtosis')
print(tmp)

p<-table %>% group_by(Cover_Type) %>% summarise(ilosc = n()) 
ggplot(p, aes(x=Cover_Type, y=ilosc, fill=Cover_Type)) +  geom_bar(stat="identity")+theme_minimal() + theme(legend.position="none") +
scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7")) +geom_text(aes(label=ilosc), vjust=-0.3, size=3.5) +theme(axis.title.x = element_text( face="bold"))+
theme(axis.title.y= element_text( face="bold")) +
theme(plot.title = element_text(size = 15,  face= 'bold' ))+ labs(title="Ilosc Cover_typów", x ="Kategoria", y = "Ilość") +  scale_y_continuous(breaks =  seq(0,300000, by=50000)) + 
theme( plot.title = element_text(hjust=0.5))

########################################################################
SOILS
#######################################################################
nazwy<-names(table[1, 15:54])
soil_type_sums = c(colSums(table[15:54]))
soils<-data.frame(soil_type_sums, nazwy)


ggplot(soils, aes(x=nazwy, y=soil_type_sums, fill=nazwy)) +  geom_bar(stat="identity")+theme_minimal() + theme(legend.position="none") +
  scale_x_discrete(limits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40")) +geom_text(aes(label=soil_type_sums), vjust=-0.3, size=3.5) +theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))+ labs(title="Ilosc soilsów", x ="Kategoria", y = "Ilość") +  scale_y_continuous(breaks =  seq(0,120000, by=10000)) + 
  theme( plot.title = element_text(hjust=0.5))

#######################################################################

WILDERNESS AREA
########################################################################
nazwy2<-names(table[1, 11:14])

wilderness_area_sums = c(colSums(table[11:14]))

wild<-data.frame(wilderness_area_sums, nazwy2)
ggplot(wild, aes(x=nazwy2, y=wilderness_area_sums, fill=nazwy2)) +  geom_bar(stat="identity")+theme_minimal() + theme(legend.position="none") +
  scale_x_discrete(limits=c("WA_1", "WA_2", "WA_3", "WA_4")) +geom_text(aes(label=wilderness_area_sums), vjust=-0.3, size=3.5) +theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' ))+ labs(title="Ilosc Wa_coś tam trzeba przeczytać", x ="Kategoria", y = "Ilość") +  scale_y_continuous(breaks =  seq(30000,300000, by=50000)) + 
  theme( plot.title = element_text(hjust=0.5))

########################################################################
COVER TYPE I SLOPE

#######################################################################

table$Cover_Type = factor(table$Cover_Type, levels=c(1,2,3,4,5,6,7), labels=c(1,2,3,4,5,6,7))
ggplot(table,  aes(x=Cover_Type, y=Slope, fill=Cover_Type)) + 
  geom_boxplot()  +
  xlab("Cover_Type") + ylab("Slope")+
  theme(axis.title.x = element_text( face="bold"))+
  theme(axis.title.y= element_text( face="bold")) +
  theme(plot.title = element_text(size = 15,  face= 'bold' )) +
  labs(title = "Boxplot Cover_Type and Slope") + 
  scale_fill_discrete(name = "", labels = c("Spruce/Fir", "Lodgepole Pine",  "Ponderosa Pine", "Cottonwood/Willow", "Aspen",
                                            "Douglas-fir", "Krummholz"))

pct <- round(Rawah$ilosc/sum(Rawah$ilosc)*100)
Rawah$Cover_Type<-as.character(Rawah$Cover_Type)
Rawah[1,1] <- "Spruce"
Rawah[2,1] <- "Lodgepole_Pine"
Rawah[3,1] <- "Aspen"
Rawah[4,1] <- "Krummholz"
lbls <- paste(Rawah$Cover_Type, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(Rawah$ilosc,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of las")

corrplot(table[1:10], method = "circle")
M<-cor(table[1:10])
corrplot(M, method = "number")

# one hot encoding cover type
table <- one_hot(as.data.table(table), cols = 'Cover_Type')

res1 <- cor.mtest(cor(table[1:10]), conf.level = .95)
res2 <- cor.mtest(cor(table[1:10]), conf.level = .99)
corrplot(cor(table[1:10]),p.mat = res1$p, sig.level = .1, method = "number")


binary_correlation <- function(v1, v2){
  vm = v1 * v2
  positive_match = sum(vm == 1) / length(v1)
  v = v1 - v2
  positive = sum(v == 1) / length(v1)
  negative = sum(v == -1) / length(v1)
  vm = v + vm
  negative_match = sum(vm == 0) / length(v1)
  return(c(positive_match,negative_match,positive,negative))
}

bin_cor_df <- data.frame(positive_match=double(),
                         negative_match=double(), 
                         positive=double(), 
                         negative=double()) 
names(bin_cor_df) = c('positive_match', 'negative_match', 'positive', 'negative')
for (c1 in 11:14){
  for (c2 in 55:61){
    v = data.frame(binary_correlation(table[c1], table[c2]))
    bin_cor_df = cbind(bin_cor_df, v)
  }
}


table2<-table %>% filter(WA_1==1) 
Rawah<-table2 %>% group_by(Cover_Type) %>% summarise(ilosc = n())
Rawah %>% ggplot(aes(x=Cover_Type, y=ilosc)) + geom_col()




ggplot(table, aes(x=Hillshade_9am,y= Hillshade_3pm)) +
  geom_point() +
  geom_smooth(method = lm)

simple.fit<-lm(Hillshade_9am~Hillshade_3pm,data=table)
summary(simple.fit)



