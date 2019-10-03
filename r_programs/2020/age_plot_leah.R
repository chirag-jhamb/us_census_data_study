
# load packages
require(ggplot2)
require(tidyverse)

# todays date
dateo <- paste(substr(Sys.Date(),1,4),substr(Sys.Date(),6,7),substr(Sys.Date(),9,10),sep="")
dateo


# multiplot function, to plot multiple graphs in a page. Taken from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

df <- read.csv("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/python_output/2020/20190918_age.csv") #read the data file

# add up totals for men and women
# average population total (average of two same values of Total gives the population)
dfg <- group_by(.data = df, FIP, year, age)
dfsum <- summarize(.data = dfg, age_mw = sum(Population),
      	 		        total_pop = mean(Total))

# make share by age group and year 
dfsum$age_share <- dfsum$age_mw/dfsum$total_pop
summary(dfsum$age_share)

# check that age share adds up to one
dfsum <- group_by(.data = dfsum, FIP, year)
dfsum <- mutate(.data = dfsum, age_share_total = sum(age_share))
summary(dfsum$age_share_total)

#### make one graph
## subset just to dc for one year 
dc1year <- dfsum[which(dfsum$FIP == "DC_001" & dfsum$year == "2008-2012"),]

# creat variable to sort age
dc1year$age_sort <- substr(dc1year$age,1,2)
dc1year$age_sort <- ifelse(dc1year$age_sort == "un",4,dc1year$age_sort)
table(dc1year$age_sort)
dc1year$age_sort_num <- as.numeric(dc1year$age_sort)

# make a graph
dc1 <- ggplot() +
       geom_bar(data = dc1year,
       		aes(x = reorder(age, age_sort_num), y = age_share),
		stat = "identity") +
       coord_flip()

# save the graph
fn <- paste0("/groups/brooksgrp/center_for_washington_area_studies/state_of_the_capitol_region/r_output/2020/20190920_meeting/tester/",dateo,"_DCexample.jpg")
ggsave(filename = fn,
       plot = dc1,
       device = "jpg",
       units = c("in"),
       width = 8,
       height = 11)

stop()

for (cnt in unique(df$FIP)){   # loop over each value of FIP (county)
  df3 <- subset(df, FIP==cnt)   # select only that county's data
  fname <- gsub(" ", "", cnt, fixed = TRUE)
  fname = paste("age_plots/20190918_" , cnt,'.pdf',sep = "")
  pdf(fname)
  plots <- list()
  x <- 1
  for (yr in sort(unique(df3$year))){   # loop over each year
    df2 <- subset(df3, year==yr)   
    df2 <- aggregate(Population ~ age + age_sort+year+Total, df, sum)   #add male and female population data together
    df2$population_share <- df2$Population/df2$Total   #calculate %age
    # sort the age in order by converting it into a factor
    df2 <- df2[order(df2$age_sort),]    
    df2$age = factor(df2$age, levels = unique(df2$age))
    # save the graph in plots list
    plots[[x]] <- ggplot(data = df2, aes(x = age, y = population_share)) + geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(size = 5)) + coord_flip() + labs(title=yr)
    x <- x+1
  }
  # save plots for this county in a file:
  multiplot(plotlist = plots, cols = 3)
  dev.off()
}

