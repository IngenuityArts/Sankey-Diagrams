# Library
library(networkD3)
library(dplyr)
library(htmlwidgets)
library(stringr)

#input csv file. Must have four columns; with source columns as CitiIQ Considerations.
d.links <- read.csv("Kenz_UMDF_Sankey_Data-2.csv", stringsAsFactors = F)
colnames(d.links) <- c("source", "target", "weight", "groupA")

#trim whitespace (at beginning or end of all entries), if it exists
d.links <- data.frame(apply(d.links, MARGIN = c(1,2), str_trim))

#add space as prefix to all citiIQ nodes, for Java/label adjustment function
tosub <- str_pad(d.links$source, 40, pad = " ")
tosub <- sub("\\s+", " ", tosub)
d.links <- cbind(tosub, d.links[,2:ncol(d.links)])
colnames(d.links)[1] <- "source"

#add the groups here for colours. The group 'targ' is for the target node colour.
d.nodes <- data.frame(
  name = c(as.character(d.links$source), as.character(d.links$target)),
  group = c(d.links$groupA, rep("targ", times = nrow(d.links))))
d.nodes <- unique(d.nodes)

d.links$IDsource <- match(d.links$source, d.nodes$name)-1 
d.links$IDtarget <- match(d.links$target, d.nodes$name)-1

###citiIQ hex codes:
#Destiny: pink - #f03b72
#Livability: green - #60c1b0
#Opportunity purple - #7f65ac
#Competitiveness: yellow - #fbce51
#Basic Needs: blue - #2aabe2

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["Destiny", "Livability", "Opportunity", "Competitiveness", "BasicNeeds", "targ"]) .range(["#f03b72", "#7f65ac", "#60c1b0", "#fbce51", "#2aabe2","#808080"])'

# Make the Network
p <- sankeyNetwork(Links = d.links, Nodes = d.nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "weight", NodeID = "name", 
                   colourScale=my_color, NodeGroup="group",
                   sinksRight = FALSE,iterations = 0, fontSize=15, nodeWidth = 20, nodePadding = 5)
p

# save the widget
# Add folder "HtmlWidget" to source file location then run below
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/HtmlWidget/SankeyBaseline.html"))
# convert html to .tiff here https://convertio.co/download/ce8f5af2cba2802d10e99a7ed2ba8fff39ef72/