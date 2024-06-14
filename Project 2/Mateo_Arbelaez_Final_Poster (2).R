library(ggplot2)
library(vioplot)
library(usmap)
library(maps)


library(mapproj)
library(rworldmap)
library(plotrix)
library(rnaturalearth)
library(datasets)
library(rnaturalearthhires)
library(raster)
library(ggmap)
library(tmaptools)
library(maptools)

fname <- file.choose()
data <- read.csv(file = fname
                 ,header = TRUE
                 , stringsAsFactors = FALSE)

View(data)
dim(data)
str(data)
summary(data)


data$age<- as.numeric(data$age)



tbl <- with(data,table( data$gender, data$age_bucket))

barplot(tbl, beside = TRUE)



#data without US

data2 <- data[which(data$contry_citizenship != "United States"),]

tbl2 <- with(data2,table( data2$contry_citizenship))

df1 <- data.frame(tbl2)

df1 <- df1[with(df1,order(-Freq)),]
df1 <- df1[1:10,]



ggplot(df1, aes(factor(df1$Var1), Freq, fill = df1$Freq)) +     
  geom_col() + coord_flip() 

###TOP 10 NON -US Countries

ggplot(df1, aes(x=reorder(df1$Var1, df1$Freq), y = df1$Freq)) + geom_bar(stat = "identity") + coord_flip() +
  geom_text(aes(label = df1$Freq), hjust = 2)



#data for US states

data_us <- data[which(data$contry_citizenship == "United States"),]

tbl_us <- with(data_us,table( data_us$state))

df_us <- data.frame(tbl_us)

df_us <- df_us[with(df_us,order(-Freq)),]
df_us <- df_us[1:10,]


###TOP 10 NON -US Countries

ggplot(df_us, aes(x=reorder(df_us$Var1, df_us$Freq), y = df_us$Freq)) + geom_bar(stat = "identity") + coord_flip() +
  geom_text(aes(label = df_us$Freq), hjust = 1.5)



##### Total Finishers



tbl_total <- with(data,table( data$gender))

barplot(tbl_total, beside = FALSE)

barplot(as.matrix(tbl_total))



class(data$official_time)

#conver date fromstring to date

conversion.string <-"%r"


tmp <- strptime(data$official_time, conversion.string)

class(tmp)

tmp

is.na(tmp) 
any(is.na(tmp))  #are any NAs?

rm(tmp)
data$official_time <- strptime(data$official_time, conversion.string)


df_time <- aggregate(data$official_time, list(data$gender),mean)
colnames(df_time) <- c("Gender","avg_time")
df_time
barplot(df_time$avg_time, names.arg =df_time$Gender)



df_time <- aggregate(data$seconds , list(data$gender),mean)
colnames(df_time) <- c("Gender","avg_time")
df_time


vec<- c("all",'13981')


df_time[nrow(df_time) + 1,] = vec

str(df_time)



df_time$avg_time <- as.numeric(df_time$avg_time)


barplot(df_time$avg_time , names.arg =df_time$Gender)


barplot(df_time)


barplot(as.matrix(df_time), beside = TRUE)

######### for MAP

library(usmap)
library(ggplot2)
library(maps)

library(tidyverse)
library(sf)
library(plotly)
library(ggplot2)


### US map

plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())


map(database = "world")
m <-map("state")
map("state", fill = TRUE, col = c("red", "orange", "yellow"))


plot_usmap(data = df_us, values = "Freq",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Popularity", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) 





###### treeemap

library(RColorBrewer)
library(treemap)
str(data)


data_us2 <- data[which(data$contry_citizenship == "United States"),]

tbl_us2 <- with(data_us2,table( data_us2$state))

df_us2 <- data.frame(tbl_us2)

df_us2 <- df_us2[with(df_us2,order(-Freq)),]

treemap(df_us2, index =c("Var1")
        ,vSize ="Freq"
        ,vColor= "Freq"
        ,type = "dens"
        ,fontsize.labels = 10
        ,palette ="Greys")


### US map

plot_usmap(regions = "states") + 
  labs(title = "U.S. States",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())


map(database = "world")
m <-map("state")
map("state", fill = TRUE, col = c("red", "orange", "yellow"))


plot_usmap(df_us2$Var1, values = "Freq",   labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Popularity", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) 


######world map


tbl_map <- with(data,table( data$contry_citizenship))

df_map <- data.frame(tbl_map)



countries <- df_map

iso3.code <- tapply(countries$Var1
                    , 1:length(countries$Var1)
                    , rwmGetISO3)
df_iso <- data.frame(country=iso3.code, labels = countries$Var1
                 ,life=countries$Freq)
df_iso
df.map <- joinCountryData2Map(df_iso, joinCode = "ISO3"
                              ,nameJoinColumn = "country")
par(mar=c(0,0,1,0))
colourPalette <- RColorBrewer::brewer.pal(5,'PuBu')

mapCountryData(df.map
               ,nameColumnToPlot = "life"
               ,numCats = num.cat
               ,catMethod = 'catMethod'
               ,colourPalette = colourPalette
    
               ,oceanCol = "white"
              
)


######world map without US


data_nous <- data[which(data$contry_citizenship != "United States"),]


tbl_map3 <- with(data_nous,table( data_nous$contry_citizenship))

df_map3 <- data.frame(tbl_map3)


countries <- df_map3

iso3.code <- tapply(countries$Var1
                    , 1:length(countries$Var1)
                    , rwmGetISO3)

df_iso <- data.frame(country=iso3.code, labels = countries$Var1
                     ,life=countries$Freq)
df_iso
df.map <- joinCountryData2Map(df_iso, joinCode = "ISO3"
                              ,nameJoinColumn = "country")
par(mar=c(0,0,1,0))
colourPalette <- RColorBrewer::brewer.pal(5,'PuBu')

mapCountryData(df.map
               ,nameColumnToPlot = "life"
               ,numCats = num.cat
               ,catMethod = 'catMethod'
               ,colourPalette = colourPalette
               
               ,oceanCol = "white"
               
)

### elevation

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

my.dir <- "H:\\Desktop\\IST 719\\"
df <- read.csv(file=paste0(my.dir,"elevation.csv")
               ,header=TRUE
               ,stringsAsFactors = FALSE)


plot(df$Â.Elevation2,df$ï..Mileage)

names(df)[names(df) == "Â.Elevation2"] <- "elevation2"
names(df)[names(df) == "ï..Mileage"] <- "mile"
df$elevation2<- as.numeric(df$elevation2)
df$mile<- as.numeric(df$mile)
str(df)

class(df)


plot(df$elevation2, type="o")


ggplot( df, aes(x=df$mile, y=df$elevation2)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("bitcoin price ($)") + theme_ipsum()


nrlang::last_error()

#plots the area chart with theme, title and labels 
ggplot(df, aes(x=df$mile, y=df$elevation2))+
  geom_area(fill='black', alpha=.5)+
  geom_line(color='darkgray', size=1)+
  geom_point(size=2, color='black')+
  labs(x='Mile', y='Elevation') +
  theme(panel.grid =element_line(color="white", size=1))+
  theme(panel.grid.minor = element_line(color = "white", size = 1)) 
  

