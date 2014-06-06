setwd("/Users/jamesfisher/Documents/Blog/US World Cup Roster")

library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("RColorBrewer", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

usa <- read.xlsx("US World Cup Roster.xlsx",sheetIndex=1,header=TRUE)

### Graphs:
##      1) Age
##      2) Club Country
##      3) Caps
##      ** Histograms

Year <- usa$Year
Birth <- usa$Birth.Year
Age <- usa$Age
Caps <- usa$Caps
Country <- usa$ClubCountry

df <- data.frame(Year, Birth, Age, Caps, Country)

### Graph 1 :: Team Age

ggplot(df, aes(x=factor(Year), y=Age, fill=Year)) + 
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12),
          axis.text.y = element_text(size=12),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank(), # hides vertical major grid lines
          plot.title = element_text(face="bold")) +
    guides(fill=FALSE) +
    labs(x="Year", y= "Player Age", 
         title = "US World Cup Rosters: Team Ages")

### Graph 2 :: Caps

dfcap <- subset(df,Year > 1994 ,select=Year:Country) # Caps are more accurate in 1998 and beyond

ggplot(dfcap, aes(x=factor(Year), y=Caps, fill=Year)) + 
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12),
          axis.text.y = element_text(size=12),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank(), # hides vertical major grid lines
          plot.title = element_text(face="bold")) +
    guides(fill=FALSE) +
    labs(x="Year", y= "International Appearances", 
         title = "US World Cup Rosters: Caps \n  (International Appearances)")

### Graph 3 :: Club Country

# Adding a column to give non-USA club teams one value ("Foreign")
Club <- ifelse(df$Country == "USA", yes = "USA", no = "Foreign")
Club <- as.data.frame(Club)
dfclub <- cbind(df,Club)

ggplot(dfclub, aes(x=factor(Year), fill=Club)) +
    geom_bar() +
    scale_y_continuous(expand = c(0,0),
                       limits=c(0,25),
                       breaks=c(0, 5, 10, 15,20,22,23)) +
    scale_fill_manual(values=c("#56B4E9","red3")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, vjust=0.5, size=12),
          axis.text.y = element_text(size=12),
          panel.grid.minor=element_blank(), # hides minor gridliness
          panel.grid.major.x=element_blank(), # hides vertical major grid lines
          plot.title = element_text(face="bold")) +
    labs(x="Year", y= "Number of Players", 
         title = "US World Cup Rosters:\nClub Team Countries")
