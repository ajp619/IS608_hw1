## IS608
## Homework 1
## Aaron Palumbo

# load required packages
library(plyr)
library(ggplot2)
library(scales)
library(grid)

# set locations
data <- "../Data/"
figures <- "../Figures/"

# Make sure I'm in the right directory
if(file.exists("Code")){
  setwd("Code")
}
if(!file.exists("../Data")){
  stop("Not in the correct working directory!")
}

#load data
file <- "inc5000_data.csv"
raw.data <- read.csv(paste(data, file, sep=""), header=TRUE, sep=",", strip.white=TRUE)

# summarize data for figure 1
by.state <- ddply(raw.data, .(State), summarize, count = length(Rank))
by.state <- by.state[order(by.state$count),]
by.state$State <- factor(by.state$State, levels = by.state$State) #This controls ggplot ordering

# figure 1
figure1 <- ggplot(data=by.state, aes(x=State, y=count)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  ggtitle("Figure 1. Number of Fastest Growing Tech Companies per State") +
  theme(axis.title.x = element_blank(), plot.title = element_text(size = 12))
figure1
ggsave(figure1, file=paste(figures, "figure1.png", sep=""), height=10, width=8, dpi=100, scale=0.75)


# summarize data for figures 2 and 3
# Choose an outlier limit that minimizes the number of companies left out, but allows for important details to be shown
outlier = 300

# Filter on NY, discard companies greater than outlier limit
ny <- raw.data[raw.data$State == "NY" & raw.data$Employees < outlier,]

# Make sure to note how many companies are being discarded
print("Number of companies discarded")
nrow(raw.data[raw.data$State == "NY" & raw.data$Employees >= outlier,])
table(raw.data$Industry[raw.data$State == "NY" & raw.data$Employees >= outlier])

# Summarize average and total number of Employees by Industry
ny.ind <- ddply(ny, .(Industry), summarize, 
                avg.employeed = mean(Employees),
                total.employed = sum(Employees),
                num.companies = length(Employees),
                avg.revenue = mean(Revenue/Employees),
                sd.revenue = sd(Revenue/Employees),
                avg.growth = mean(Growth_Rate))
# Order data based on average number of employees (descending)
ny.ind <- ny.ind[order(ny.ind$avg.employeed, decreasing=TRUE),]

# Order factor levels in main data frame based on average number of employees
ny$Industry <- factor(ny$Industry, levels=ny.ind$Industry)

# Found cool function: http://stackoverflow.com/questions/13297155/add-floating-axis-labels-in-facet-wrap-plot
# Adds labels to second row
facetAdjust <- function(x, pos = c("up", "down"))
{
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p); dev.off()
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  panels <- sum(grepl("panel", names(gtable$grobs)))
  space <- ncol * nrow
  n <- space - panels
  if(panels != space){
    idx <- (space - ncol - n + 1):(space - ncol)
    gtable$grobs[paste0("axis_b",idx)] <- list(gtable$grobs[[paste0("axis_b",panels)]])
    if(pos == "down"){
      rows <- grep(paste0("axis_b\\-[", idx[1], "-", idx[n], "]"), 
                   gtable$layout$name)
      lastAxis <- grep(paste0("axis_b\\-", panels), gtable$layout$name)
      gtable$layout[rows, c("t","b")] <- gtable$layout[lastAxis, c("t")]
    }
  }
  class(gtable) <- c("facetAdjust", "gtable", "ggplot"); gtable
}

print.facetAdjust <- function(x, newpage = is.null(vp), vp = NULL) {
  if(newpage)
    grid.newpage()
  if(is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  }
  invisible(x)
}

# When facet_wrap populates a grid, it does it row wise.  
# This function orders factors such that a rxc grid will be ordered column wise.  
# 
# i.e.:  
#   Assuming a factor is ordered by rank [1, 2, 3, 4, 5, 6].  
# Default behavior with ncol=2 will produce  
# 1 2  
# 3 4  
# 5 6  
# 
# This function will order the factors such that facet_wrap with ncol=2 will produce
# 1 4  
# 2 5  
# 3 6  
# 
# It does this by ordering the factors as [1, 4, 2, 5, 3, 6]
facet.order <- function(level.order, ncols){
  ntotal <- length(level.order)
  nrows <- ceiling(ntotal / ncols)
  new.order <- level.order
  for(c in 1:ncols){
    index.start <- 1 + (c-1) * nrows
    index.end <- nrows + (c-1) * nrows
    index.end <- min(index.end, ntotal)
    new.order[seq(from=c, to=ntotal, by=ncols)] <- level.order[index.start:index.end]
  }
  return(new.order)
}


# figure 2, dotplot
# Order summary data based on average number of employees (descending)
ny.ind <- ny.ind[order(ny.ind$avg.employeed, decreasing=TRUE),]

# Order factors for plot
ny$Industry <- factor(ny$Industry, levels = facet.order(ny.ind$Industry, 2))

d <- ggplot(ny, aes(x=Employees)) + theme_bw() + 
  theme(strip.text.x = element_text(size=8)) +
  geom_dotplot(binwidth=5, dotsize=0.75, method="histodot") +
  facet_wrap(~ Industry, ncol=2) + 
  geom_vline(data=ny.ind, aes(xintercept=avg.employeed), color="red", size=1) +
  scale_y_continuous(name = "Each point represents one company", breaks = NULL) +
  scale_x_continuous(name = "Number of employees (average per industry in red)", 
                     breaks = seq(from=0, to=300, by=50),
                     minor_breaks = seq(from=0, to=300, by=10)) +
  ggtitle("Figure 2. Number of employees per company")

figure2 <- facetAdjust(d)
print(figure2)
# ggsave("name.pdf", p)
ggsave(figure2, 
       file=paste(figures, "figure2.png", sep=""), 
       height=12.5, width=11, dpi=100, scale=0.75)


# Figure 3
# Order summary data by avg revenue / employee
ny.ind <- ny.ind[order(ny.ind$avg.revenue, decreasing=TRUE),]

# Order factors for plot
ny$Industry <- factor(ny$Industry, levels = facet.order(ny.ind$Industry, 2))

figure3 <- ggplot(ny, aes(x=Revenue/Employees, y=Growth_Rate/100)) + theme_bw() +
  geom_point() + 
  scale_x_log10("Revenue per Employee (log scale)\nAverage show in red", labels = dollar) +
  # Something is funny here ...
  scale_y_log10("3 Year percentage growth (log scale)\nAverage shown in blue", 
                labels = percent) +
  # scale_y_log10("3 Year Percentage Growth (log scale)") +
  facet_wrap(~ Industry, ncol=2) +
  geom_vline(data=ny.ind, aes(xintercept=avg.revenue), color="red", size=1) +
  geom_hline(data=ny.ind, aes(yintercept=avg.growth/100), color="blue") +
  ggtitle("Figure 3. Revenue generated per employee")

figure3.adjusted <- facetAdjust(figure3)
print(figure3.adjusted)

ggsave(figure3.adjusted, 
       file=paste(figures, "figure3.png", sep=""), 
       height=12.5, width=11, dpi=100, scale=0.75)