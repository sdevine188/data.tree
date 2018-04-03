library(data.tree)
library(treemap)
library(DiagrammeR)
library(yaml)

# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
# http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#graphviz-layouts

# get data
data(GNI2014)
head(GNI2014)
glimpse(GNI2014)

# create variable pathString to parse into hierarchy
GNI2014$pathString <- paste("world", 
                            GNI2014$continent, 
                            GNI2014$country, 
                            sep = "/")

glimpse(GNI2014)

# create a tree from data.frame
population <- as.Node(GNI2014)

attributes(population)
names(population)
str(population)

population
population$fieldsAll
print(population)
print(population, "iso3", "population", "GNI", limit = 20)
print(population, "population", "GNI", limit = 20)
print(population, "GNI", limit = 20)

glimpse(population)

glimpse(as.data.frame(population))


# climb tree
population$`North America`


#######################################################



# create a tree programmatically
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

print(acme)

# climb tree
acme$IT
acme$IT$Outsource

acme$Research
acme$Research$`New Labs`

# navigate by fields
Climb(acme, name = "IT")
Climb(acme, name = "IT", position = 2)
Climb(acme, position = c(2, 1))
Climb(acme, name = "IT", name = "Outsource")
Climb(acme, name = "IT", name = "Outsource")$path


Climb(acme, position = c(2, 1))

# add variables to nodes
software$cost <- 1000000
standards$cost <- 500000
newProductLine$cost <- 2000000
newLabs$cost <- 750000
outsource$cost <- 400000
agile$cost <- 250000
goToR$cost <- 50000

software$p <- 0.5
standards$p <- 0.75
newProductLine$p <- 0.25
newLabs$p <- 0.9
outsource$p <- 0.2
agile$p <- 0.05
goToR$p <- 1
print(acme, "cost", "p")


# plot tree
plot(acme)

# add styling (using DiagrammR)
# http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#graphviz-layouts
SetGraphStyle(acme, rankdir = "TB")
SetEdgeStyle(acme, arrowhead = "vee", color = "grey35", penwidth = 2)
SetNodeStyle(acme, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow", 
             fontname = "helvetica", tooltip = GetDefaultTooltip)
SetNodeStyle(acme$IT, fillcolor = "LightBlue", penwidth = "5px")
plot(acme)


###################################################


# labeling tree plots

# get data
# despite odd data source, it's just a standard node object
fileName <- system.file("extdata", "jennylind.yaml", package="data.tree")
cat(readChar(fileName, file.info(fileName)$size))
lol <- yaml.load_file(fileName)
glimpse(lol)

jl <- as.Node(lol)
jl
jl$fieldsAll

print(jl, "type", "payoff", "p")
print(jl)


# basic plot
SetGraphStyle(jl, rankdir = "TB")
plot(jl)


# custom plot
GetNodeLabel <- function(node) switch(node$type, 
                                      terminal = paste0( '$ ', format(node$payoff, scientific = FALSE, big.mark = ",")),
                                      paste0('ER\n', '$ ', format(node$payoff, scientific = FALSE, big.mark = ",")))

GetEdgeLabel <- function(node) {
        if (!node$isRoot && node$parent$type == 'chance') {
                label = paste0(node$name, " (", node$p, ")")
        } else {
                label = node$name
        }
        return (label)
}

GetNodeShape <- function(node) switch(node$type, decision = "box", chance = "circle", terminal = "none")


SetEdgeStyle(jl, fontname = 'helvetica', label = GetEdgeLabel)
SetNodeStyle(jl, fontname = 'helvetica', label = GetNodeLabel, shape = GetNodeShape)

SetGraphStyle(jl, rankdir = "TB")
plot(jl)







