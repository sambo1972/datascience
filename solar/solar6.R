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
svg("plots/solar_plot6.svg")

# sort by descending frequency and highlight grouping

# re-order the levels of the Classification factor by descreasing Frequency
# so ggplot shows them on x-axis same way
df$Classification <- factor(df$Classification, levels = df$Classification[order(-df$Frequency)])
  
# also re-order the dataframe by descreasing Frequency
df <- df[order(-df$Frequency),]
  
# group the ones we want to highlight and associate colours
df$Group <- factor(c(1,1,0,0,0,0))
group.colors <- c("1"="tomato1", "0"="#cccccc")  

g <- ggplot(data=df, aes(x = Classification, y=Frequency, fill=Group)) + 
    geom_bar(stat="identity") + 
    ggtitle("Solar flare class distribution") +
    geom_text(aes(label=paste(round(Frequency, digits=1), "%", sep = '')), vjust=1.1, size=8, colour="white") +
    xlab(NULL) + ylab(NULL) +
    theme(plot.background = element_blank(), 
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(colour = "#aaaaaa"),
          axis.text = element_text(colour = "#aaaaaa", size = 15),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none"
    )
g <- g + scale_fill_manual(values=group.colors)
g <- g + scale_y_discrete(expand = c(0,0))

g
dev.off()
g
