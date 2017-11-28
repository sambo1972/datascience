library(ggplot2)

load.data.frame <- function(){
  # read and merge each file
  df1 <- read.csv('data/flare.data1', header = F, sep=" ", skip = 1)
  df2 <- read.csv('data/flare.data2', header = F, sep=" ", skip = 1)
  df <- rbind(df1, df2)
  
  # set column names from the data descriptor
  col_names <- c("zurich-class", "largest-spot-size", "spot-distribution",
                 "activity", "evolution", "prev-24hr-activity", "historically-complex",
                 "became-historically-complex", "area", "area-largest-spot",
                 "pred-common-c-class", "pred-moderate-m-class", "pred-severe-x-class")
  names(df) <- col_names
  # build frequency distribution for largest observed classification
  df.pd <- as.data.frame(prop.table(table(df$`largest-spot-size`)) * 100)
  names(df.pd) <- c('Classification', 'Frequency')
  return (df.pd)
}

df <- load.data.frame()

# build plot and save
svg("plots/solar_plot1.svg")

# initial plot
g <- ggplot(data=df, aes(x = Classification, y=Frequency, fill=Classification)) + 
  geom_bar(stat="identity", colour="black") + 
  ggtitle("Distribution of Solar Flare Spot Size Classification") +
  xlab("Spot Size Classification") +
  ylab("Percentage") +
  theme(plot.background = element_rect(fill="black"), 
        panel.background = element_rect(fill="grey"),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.grid.major = element_line(colour="black"),
        panel.grid.minor = element_line(colour="black"),
        plot.title = element_text(colour = "white", face = "bold", hjust = 0.5),
        axis.title = element_text(colour = "white", face = "bold"),
        axis.text = element_text(colour = "white", size = 10),
        legend.background = element_rect(fill="black", colour = "white"),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white", face = "bold"),
        legend.key = element_blank()
  )
g
dev.off()
g