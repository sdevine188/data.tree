library(data.tree)
library(treemap)
library(DiagrammeR)
library(yaml)
library(rpart)
library(rpart.plot)

# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
# http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#graphviz-layouts
# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html

# setwd
setwd("H:/R/data.tree")

# get data
data(GNI2014)
head(GNI2014)
glimpse(GNI2014)

# create variable pathString to parse into hierarchy
GNI2014$pathString <- paste("world", GNI2014$continent, GNI2014$country, sep = "/")

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


# test converting df with NAs due to uneven hierarchy, update pathString
GNI2014_v2 <- GNI2014 %>% mutate(country = case_when(country %in% c("Bermuda", "Norway", "Qatar", "United States") ~ NA_character_, TRUE ~ country),
                                 country = ifelse(is.na(country), "", country),
                                 pathString = str_c("world", continent, country, sep = "/")) %>% slice(1:10)
GNI2014_v2


# convert to node
population_v2 <- as.Node(GNI2014_v2)
population_v2


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


############################################################
###########################################################
############################################################


# converting rpart object into node
head(mtcars)

# build rpart model
mtcars_rpart <- rpart(am ~ mpg + cyl + disp + hp + wt + gear + carb, data = mtcars, method = "class",
                      minsplit = 2, minbucket = 1, cp = -1)
mtcars_rpart
rpart.plot(mtcars_rpart, extra = 104, nn = TRUE)


########################################################


# reshape mtcars_rpart 
head(GNI2014)
glimpse(GNI2014)
GNI2014$pathString <- paste("world", GNI2014$continent, GNI2014$country, sep = "/")
glimpse(GNI2014)

# get clean model output of stats for each node
mtcars_rpart_output <- mtcars_rpart$frame
mtcars_rpart_output_part2 <- data.frame(mtcars_rpart$frame$yval2)
glimpse(mtcars_rpart_output)
glimpse(mtcars_rpart_output_part2)
mtcars_rpart_output <- cbind(mtcars_rpart_output, mtcars_rpart_output_part2)
glimpse(mtcars_rpart_output)
mtcars_rpart_output

# clean model output
# note that output dataframe has same stats for each node number as the rpart output object, 
# but the variable listed for each node number in the df is actually the variable listed for the subsequent node number on output object
# basically, if you just ignore the variable listed on the rpart output object, and refer only by node number, it's the same
# the output object is listing the variable that HAS been split on to get to current node, but
# the df is listing the variable that will be split on FROM that node

mtcars_rpart_output <- mtcars_rpart_output %>% select(-yval2, V1) %>%
        rename(obs_in_node = n, var_to_be_split_next = var, misclassified_count = dev, predicted_class = yval,
               class_1_obs_in_node = V2, class_2_obs_in_node = V3, prob_class_1 = V4, 
               prob_class_2 = V5, pct_obs_in_node = nodeprob) %>%
        mutate(terminal_node = rownames(mtcars_rpart_output), tree_depth = rpart:::tree.depth(as.numeric(terminal_node)))

mtcars_rpart_output
glimpse(mtcars_rpart_output)


##################################################


# call get_node_path function
source("get_node_paths.R")

mtcars_rpart_node_paths <- get_node_paths(model_fit = mtcars_rpart, output_terminal_nodes = mtcars_rpart_output)
mtcars_rpart_node_paths

# add node paths to output_terminal nodes
mtcars_rpart_output <- left_join(mtcars_rpart_output, mtcars_rpart_node_paths, by = c("terminal_node" = "terminal_node"))
mtcars_rpart_output


######################################################


# create get_hierarchy_from_node_path function
get_hierarchy_from_node_path <- function(df) {
        
}



# # create a tree from data.frame
# population <- as.Node(GNI2014)
# population$fieldsAll
# print(population)
# 
# GNI2014_v2 <- GNI2014 %>% mutate(country = case_when(country %in% c("Bermuda", "Norway", "Qatar", "United States") ~ NA_character_, TRUE ~ country),
#                                  country = ifelse(is.na(country), "", country),
#                                  pathString = str_c("world", continent, country, sep = "/")) %>% slice(1:10)








