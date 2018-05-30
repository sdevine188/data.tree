library(data.tree)
library(treemap)
library(DiagrammeR)
library(rpart)
library(rpart.plot)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)
library(tibble)
library(tidyr)
library(tidyselect)
library(scales)
library(janitor)
library(readr)
library(forcats)
library(DiagrammeRsvg)
library(RColorBrewer)

# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html
# http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#graphviz-layouts
# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html
# http://rich-iannone.github.io/DiagrammeR/graphs.html
# https://www.graphviz.org/doc/info/attrs.html

options(scipen=999)

# setwd
setwd("H:/RED/IBFA/analysis")


####################################################


# read model
model <- readRDS(file = "output/post_interview_wo_issue/output/post_interview/train/treatment_post_interview_train_rpart_caret_model_Case_OutcomeDenied.rds")

# inspect model
model
rpart.plot(model, extra = 104, nn = TRUE)


################################################################


# get clean model output of stats for each node
model_output <- model$frame
model_output_part2 <- data.frame(model$frame$yval2)
glimpse(model_output)
glimpse(model_output_part2)
model_output <- cbind(model_output, model_output_part2)
glimpse(model_output)
model_output

# clean model output
# note that output dataframe has same stats for each node number as the rpart output object, 
# but the variable listed for each node number in the df is actually the variable listed for the subsequent node number on output object
# basically, if you just ignore the variable listed on the rpart output object, and refer only by node number, it's the same
# the output object is listing the variable that HAS been split on to get to current node, but
# the df is listing the variable that will be split on FROM that node

model_output <- model_output %>% select(-yval2, V1) %>%
        mutate(terminal_node = rownames(model_output), tree_depth = rpart:::tree.depth(as.numeric(terminal_node))) %>%
        rename(obs_in_node = n, var_to_be_split_next = var, misclassified_count = dev, predicted_class = yval,
               class_1_obs_in_node = V2, class_2_obs_in_node = V3, prob_class_1 = V4, 
               prob_class_2 = V5, pct_obs_in_node = nodeprob)
        

model_output
glimpse(model_output)


###############################################################


# call get_node_path function
current_wd <- getwd()
setwd("H:/R/rpart")
source("get_node_paths.R")
setwd(current_wd)

model_output_node_paths <- get_node_paths(model_fit = model, output_terminal_nodes = model_output)
model_output_node_paths

# add node paths to output_terminal nodes
model_output <- left_join(model_output, model_output_node_paths, by = "terminal_node")
model_output


##############################################################


# get next split at node
get_next_split_at_node <- function(node_path_tbl) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " & ")
        last_split_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% tail(1) %>% pull(end))
        last_split_locations <- map(.x = last_split_locations, .f = clean_last_split_locations) %>% tibble(last_split_locations = .) %>%
                unnest() %>% mutate(last_split_locations = as.numeric(last_split_locations),
                                    last_split_locations = case_when(last_split_locations != 0 ~ last_split_locations + 1,
                                                                     TRUE ~ last_split_locations))
        next_split_at_node <- str_sub(string = node_path_tbl$node_path, start = last_split_locations$last_split_locations)
        str_c("  ", next_split_at_node, "     ")
}

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_last_split_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# run get_next_split_at_node function
node_path_tbl <- model_output %>% select(node_path)
next_split_at_node <- get_next_split_at_node(node_path_tbl)
model_output <- model_output %>% mutate(next_split_at_node = next_split_at_node)
model_output %>% data.frame()


###############################################################


# create new_data
new_data <- read_csv("treatment_post_interview_dummies_v2_20180508.csv")
glimpse(new_data)

# define outcome variable
outcome_variable <- "Case_OutcomeDenied"
positive_class_name <- "Denied"
negative_class_name <- "Not_Denied"
outcome_value_positive <- "Denied"
outcome_value_negative <- "Not_Denied"

# create outcome_variable_sym
outcome_variable_sym <- sym(outcome_variable)

# prepare new_data
new_data %>% select(!!outcome_variable_sym) %>% glimpse()
new_data <- new_data %>% 
        mutate(!!outcome_variable_sym := case_when((!!outcome_variable_sym) == 1 ~ outcome_label_positive, TRUE ~ outcome_label_negative)) %>%
        mutate(!!outcome_variable_sym := factor(!!outcome_variable_sym)) %>%
        mutate(!!outcome_variable_sym := fct_relevel(!!outcome_variable_sym, outcome_label_positive, outcome_label_negative))
new_data %>% select(!!outcome_variable_sym) %>% glimpse()
new_data %>% select(!!outcome_variable_sym) %>% str()


#######################################################################


# predict nodes for new_data
current_wd <- getwd()
setwd("H:/R/rpart")
source("predict_nodes.R")
setwd(current_wd)

new_data_pred_node <- predict_nodes(object = model, newdata = new_data)

# add new_data_pred_node to new_data
new_data <- new_data %>% mutate(terminal_node = as.character(new_data_pred_node))
new_data


#########################################################################


# get node_paths for new_data terminal nodes
current_wd <- getwd()
setwd("H:/R/rpart")
source("get_node_paths.R")
setwd(current_wd)

new_data_node_paths <- get_node_paths(model_fit = model, output_terminal_nodes = model_output)
new_data_node_paths

# add node paths to output_terminal nodes
new_data <- left_join(new_data, new_data_node_paths, by = "terminal_node")
glimpse(new_data)

# check 
model_output %>% filter(var_to_be_split_next == "<leaf>") %>% distinct(terminal_node, node_path) %>% arrange(terminal_node)
new_data %>% distinct(terminal_node, node_path) %>% arrange(terminal_node)


##############################################################


# create tree_output

# inspect outcome_variable
new_data %>% tabyl(!!outcome_variable_sym)

# create tree_output
tree_output <- new_data %>% mutate(tree_depth = str_count(string = node_path, pattern = " & ") + 1) %>%
        group_by(terminal_node) %>% summarize(obs_in_node = n(), pct_obs_in_node = obs_in_node / nrow(new_data),
                        positive_class_obs_in_node = sum(!!outcome_variable_sym == outcome_value_positive),
                        negative_class_obs_in_node = sum(!!outcome_variable_sym == outcome_value_negative),
                        prob_positive_class = positive_class_obs_in_node / obs_in_node,
                        prob_negative_class = negative_class_obs_in_node / obs_in_node,
                        predicted_class = ifelse(prob_positive_class > prob_negative_class, 1, 0),
                        predicted_class_name = ifelse(prob_positive_class > prob_negative_class, positive_class_name, negative_class_name),
                        node_path = unique(node_path), 
                        tree_depth = unique(tree_depth))

tree_output %>% data.frame()


#################################################################


# create function add_hierarchy_to_tree_output, then run on tree_output
add_hierarchy_to_tree_output <- function(tree_output, final_terminal_node) {
        
        # find max tree depth to know how many placeholder columns you need to build pathString of hierarchy 
        max_tree_depth <- max(tree_output$tree_depth) + 1
        
        # find how many terminal nodes in tree
        terminal_node_count <- nrow(tree_output)
        
        # create placeholder variable names for the hierarchy
        node_level_var_names <- unlist(map(.x = 1:max_tree_depth, .f = ~ str_c("node_level_", .x)))
        
        # create placeholder variable vectors for the hierarchy
        node_level_vectors <- map(.x = 1:max_tree_depth, .f = ~ rep(NA, times = terminal_node_count))
        
        # create placeholder variables for the hierarchy
        create_tbl <- function(.x, .y, ...){
                node_level_var_name_sym <- sym(.x)
                tibble(!!node_level_var_name_sym := .y)
        }
        node_level_data <- map2(.x = node_level_var_names, .y = node_level_vectors, .f = create_tbl) %>% bind_cols(.)
        
        # save node_level variable names, and create syms for use in get_node_hierarchy function
        node_level_data_variable_names <- names(node_level_data)
        node_level_data_variable_names_syms <- syms(node_level_data_variable_names )
        
        # add node_level_data placholder to tree_output_terminal_nodes
        tree_output_terminal_nodes <- tree_output  
        tree_output_terminal_nodes <- bind_cols(tree_output_terminal_nodes, node_level_data)
        
        # create get_node_hierarchy function 
        grouping_var_sym <- sym("terminal_node")
        
        # test get_node_hierarchy
        # data <- tree_output_terminal_nodes %>% mutate(grouping_var = !!grouping_var_sym) %>% group_by(grouping_var) %>%
        #                 nest() %>% slice(1) %>% unnest()
        
        get_node_hierarchy <- function(grouping_var, data, ...) {
                
                # split node_path into constituent variables
                node_path_split <- str_trim(unlist(str_split(string = data$node_path[1], pattern = "&")))
                
                # remove spaces in each node_path
                node_hierarchy <- str_replace_all(string = node_path_split, pattern = " ", replace = "")
                
                # add root to beginning of node_hierarchy
                node_hierarchy <- c("root_node", node_hierarchy)
                
                # pad node_hierarchy with NA as needed
                count_of_NAs_needed_for_padding <- length(node_level_data_variable_names) - length(node_hierarchy)
                node_hierarchy <- c(node_hierarchy, rep(NA, times = count_of_NAs_needed_for_padding))
                
                # map over the node_hierarchy values and node_level_data_variable_names to return row for terminal node with hierarchy
                map2_dfr(.x = node_level_data_variable_names_syms, .y = node_hierarchy, 
                         .f = ~ data %>% mutate(!!.x := .y)) %>%
                        fill(vars_select(names(data), starts_with("node_level_")), .direction = "up") %>% 
                        fill(vars_select(names(data), starts_with("node_level_")), .direction = "down") %>% slice(1)
        }
        
        # map get_node_hieararchy function over all terminal nodes
        tree_output_w_hierarchy <- tree_output_terminal_nodes %>% mutate(grouping_var = !!grouping_var_sym) %>% group_by(grouping_var) %>%
                nest() %>% pmap_dfr(.l = ., .f = get_node_hierarchy)
        
        # add pathString to tree_output_w_hierarchy
        tree_output_w_hierarchy <- tree_output_w_hierarchy %>% 
                mutate_at(.vars = vars_select(names(tree_output_w_hierarchy), starts_with("node_level_")), .funs = funs(ifelse(is.na(.), "", .))) %>%
                mutate(pathString = str_c(!!!node_level_data_variable_names_syms, sep = "/")) %>%
                mutate(pathString = str_replace(string = pathString, pattern = regex("/+$"), replacement = ""))
        
        # return tree_output_w_hierarchy
        tree_output_w_hierarchy
        
}

# call function and save output
tree_output_w_hierarchy <- add_hierarchy_to_tree_output(tree_output = tree_output, final_terminal_node = TRUE)
tree_output_w_hierarchy %>% data.frame()
tree_output_w_hierarchy %>% select(terminal_node, node_path, starts_with("node_level_"), pathString) %>% data.frame()


###################################################################


# get tree_output for non-terminal_nodes
tree_output_w_hierarchy %>% data.frame()
model_output %>% data.frame()

# create variable for final_terminal_node
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(final_terminal_node = 1)
model_output <- model_output %>% mutate(final_terminal_node = ifelse(var_to_be_split_next == "<leaf>", 1, 0))

# test functions
# target_depth <- 1
# node_path_tbl <- model_output %>% filter(tree_depth == target_depth, final_terminal_node == 0)
# node_path_tbl

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_target_depth_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# create get_node_path_to_target_depth function to be called in get_non_terminal_node_paths function
get_node_path_to_target_depth <- function(node_path_tbl, target_depth) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " &")
        target_depth_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% slice(target_depth) %>% pull(end))
        target_depth_locations <- map(.x = target_depth_locations, .f = clean_target_depth_locations) %>% tibble(target_depth_locations = .) %>%
                unnest() %>% mutate(target_depth_locations = case_when(target_depth_locations != 0 ~ target_depth_locations - 2,
                                                                       TRUE ~ 0))
        node_path_to_target_depth <- str_sub(string = node_path_tbl$node_path, start = 1,
                                             end = ifelse(target_depth_locations$target_depth_locations != 0, 
                                                          target_depth_locations$target_depth_locations,
                                                          nchar(node_path_tbl$node_path)))
        node_path_to_target_depth
}

# create get_non_terminal_node_paths function
get_non_terminal_node_paths <- function(.x) {
        model_output %>% filter(tree_depth == .x, final_terminal_node == 0) %>%
                get_node_path_to_target_depth(node_path_tbl = ., target_depth = .x)
}

# call get_non_terminal_node_paths on model_output
non_terminal_node_paths <- map(.x = 1:(max(model_output$tree_depth)-1), .f = get_non_terminal_node_paths) %>%
        tibble(node_path = .) %>% unnest()
non_terminal_node_paths

# create get_non_terminal_node_stats function
get_non_terminal_node_output <- function(.x) {
        current_non_terminal_node_children <- tree_output_w_hierarchy %>% 
                filter(str_sub(string = node_path, start = 1, end = nchar(.x)) == .x)
        
        current_non_terminal_node <- tibble( 
                obs_in_node = sum(current_non_terminal_node_children$obs_in_node),
                pct_obs_in_node = obs_in_node / sum(tree_output_w_hierarchy$obs_in_node),
                positive_class_obs_in_node = sum(current_non_terminal_node_children$positive_class_obs_in_node),
                negative_class_obs_in_node = sum(current_non_terminal_node_children$negative_class_obs_in_node),
                prob_positive_class = positive_class_obs_in_node / obs_in_node,
                prob_negative_class = negative_class_obs_in_node / obs_in_node,
                predicted_class = ifelse(prob_positive_class > prob_negative_class, 1, 0),
                predicted_class_name = ifelse(prob_positive_class > prob_negative_class, 
                                              positive_class_name, negative_class_name), 
                node_path = .x, final_terminal_node = 0)
        
        current_non_terminal_node
}

# call get_non_terminal_node_output on non_terminal_node_paths$node_path
non_terminal_node_output <- map(.x = non_terminal_node_paths$node_path, .f = get_non_terminal_node_output) %>% 
        tibble(non_terminal_node_output = .) %>% unnest()
non_terminal_node_output %>% data.frame()

# add terminal_node and tree_depth to non_terminal_node_stats 
non_terminal_node_output <- model_output %>% filter(node_path %in% non_terminal_node_output$node_path) %>%
        select(terminal_node, tree_depth, node_path) %>%
        left_join(non_terminal_node_output, ., by = "node_path")
non_terminal_node_output %>% data.frame()

# call add_hierarchy_to_tree_output on non_terminal_node_output
non_terminal_node_output_w_hierarchy <- add_hierarchy_to_tree_output(tree_output = non_terminal_node_output,
                                                                     final_terminal_node = FALSE)
non_terminal_node_output_w_hierarchy %>% data.frame()

# ensure non_terminal_node_output_w_hierarchy has the same node_level variables
tree_output_w_hierarchy_node_level_variables <- tree_output_w_hierarchy %>% select(starts_with("node_level")) %>%
        names()
non_terminal_node_output_w_hierarchy_node_level_variables <- non_terminal_node_output_w_hierarchy %>%
        select(starts_with("node_level")) %>%
        names()

omitted_node_level_variable <- tree_output_w_hierarchy_node_level_variables[!(tree_output_w_hierarchy_node_level_variables %in% 
                                                       non_terminal_node_output_w_hierarchy_node_level_variables)]

# add any omitted_node_level_variable
if(length(omitted_node_level_variable) > 0) {
        omitted_node_level_variable_sym <- sym(omitted_node_level_variable)
        non_terminal_node_output_w_hierarchy <- non_terminal_node_output_w_hierarchy %>% 
                mutate(!!omitted_node_level_variable_sym := NA_character_)
}


# compare non_terminal_node_output_w_hierarchy and tree_output
dim(non_terminal_node_output_w_hierarchy)
dim(tree_output_w_hierarchy)

tree_output_w_hierarchy %>% data.frame()
non_terminal_node_output_w_hierarchy %>% data.frame()

tree_output_w_hierarchy %>% select(-c(!!!syms(names(non_terminal_node_output_w_hierarchy))))
non_terminal_node_output_w_hierarchy %>% select(-c(!!!syms(names(tree_output_w_hierarchy))))

# create root node
new_data %>% tabyl(!!outcome_variable)

root_terminal_node <- "1"
root_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_obs_in_node = sum(obs_in_node)) %>% pull(root_obs_in_node)
root_positive_class_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_positive_class_obs_in_node = sum(positive_class_obs_in_node)) %>%
        pull(root_positive_class_obs_in_node)
root_negative_class_obs_in_node <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>%
        summarize(root_negative_class_obs_in_node = sum(negative_class_obs_in_node)) %>%
        pull(root_negative_class_obs_in_node) 
root_prob_positive_class <- root_positive_class_obs_in_node / root_obs_in_node
root_prob_negative_class <- root_negative_class_obs_in_node / root_obs_in_node
root_predicted_class <- ifelse(root_prob_positive_class > root_prob_negative_class, 1, 0)
root_predicted_class_name <- ifelse(root_prob_positive_class > root_prob_negative_class, positive_class_name, negative_class_name)
root_node_path <- "root_node"
root_tree_depth <- 0
root_final_terminal_node <- 0
root_pathString <- "root_node"
root_node_level_1 <- "root_node"

root_node <- tibble(terminal_node = root_terminal_node, obs_in_node = root_obs_in_node,
                    pct_obs_in_node = 1.00, positive_class_obs_in_node = root_positive_class_obs_in_node,
                    negative_class_obs_in_node = root_negative_class_obs_in_node,
                    prob_positive_class = root_prob_positive_class, prob_negative_class = root_prob_negative_class,
                    predicted_class = root_predicted_class, predicted_class_name = root_predicted_class_name,
                    node_path = root_node_path, tree_depth = root_tree_depth, final_terminal_node = root_final_terminal_node,
                    pathString = root_pathString, node_level_1 = root_node_level_1)

root_node %>% data.frame()
dim(root_node)

# get node_level variables for root and add to root_node, except node_level_1 which is always going to be "root"
node_level_names <- tree_output_w_hierarchy %>% select(starts_with("node_level")) %>% select(-node_level_1) %>% names()
node_level_names_syms <- syms(node_level_names)
root_node_node_levels <- map_dfc(.x = node_level_names_syms, .f = ~ root_node %>% mutate(!!.x := NA_character_) %>% select(!!.x))

root_node <- bind_cols(root_node, root_node_node_levels)
root_node %>% data.frame()

# add root_node to non_terminal_node_output_w_hierarchy
root_node %>% data.frame()
dim(root_node)
dim(non_terminal_node_output_w_hierarchy)
non_terminal_node_output_w_hierarchy %>% select(-c(!!!syms(names(root_node))))
root_node %>% select(-c(!!!syms(names(non_terminal_node_output_w_hierarchy))))

non_terminal_node_output_w_hierarchy <- bind_rows(non_terminal_node_output_w_hierarchy, root_node)
non_terminal_node_output_w_hierarchy %>% data.frame()

# combine tree_output_w_hierarchy with non_terminal_node_output_w_hierarchy
tree_output_w_hierarchy <- bind_rows(tree_output_w_hierarchy, non_terminal_node_output_w_hierarchy)


###################################################################


# get next split at node
get_split_leading_to_node <- function(node_path_tbl) {
        node_split_locations <- str_locate_all(string = node_path_tbl$node_path, pattern = " & ")
        last_split_locations <- map(.x = node_split_locations, .f = ~ data.frame(.x) %>% tail(1) %>% pull(end))
        last_split_locations <- map(.x = last_split_locations, .f = clean_last_split_locations) %>% tibble(last_split_locations = .) %>%
                unnest() %>% mutate(last_split_locations = as.numeric(last_split_locations),
                        last_split_locations = case_when(last_split_locations != 0 ~ last_split_locations + 1,
                                                                     TRUE ~ last_split_locations))
        split_leading_to_node <- str_sub(string = node_path_tbl$node_path, start = last_split_locations$last_split_locations)
        str_c("  ", split_leading_to_node, "     ")
}

# create clean_last_split_locations function to tidy up last split locations; called inside get_next_split_at_node
clean_last_split_locations <- function(.x) {
        split_location_length <- length(.x)
        if(split_location_length == 0) {
                return(0)
        }
        if(split_location_length != 0) {
                return(.x)
        }
}

# run get_next_split_at_node function on tree_output_w_hierarchy
tree_output_w_hierarchy %>% data.frame()

node_path_tbl <- tree_output_w_hierarchy %>% select(node_path)
split_leading_to_node <- get_split_leading_to_node(node_path_tbl)
tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(split_leading_to_node = split_leading_to_node)
tree_output_w_hierarchy %>% data.frame()


##############################################################################


# get node dummies for each individual observation 
# not necessary for plotting tree, just a useful function
tree_output_w_hierarchy %>% data.frame()
glimpse(new_data)


get_node_membership <- function(tree_output_w_hierarchy) {
        
        # get terminal nodes in tree
        terminal_node_list <- unique(tree_output_w_hierarchy$terminal_node)
        
        # create placeholder variable names for the hierarchy
        node_member_var_names <- unlist(map(.x = terminal_node_list, .f = ~ str_c("member_of_node_", .x)))
        
        # get final_terminal_nodes
        terminal_node_list <- tree_output_w_hierarchy %>% pull(terminal_node)
        
        # create placeholder variable vectors for the hierarchy
        node_level_vectors <- map(.x = node_member_var_names, .f = ~ rep(as.numeric(NA), times = length(terminal_node_list)))
        
        # create placeholder variables for the hierarchy
        create_tbl <- function(.x, .y, ...){
                node_level_var_name_sym <- sym(.x)
                tibble(!!node_level_var_name_sym := .y)
        }
        
        # run create_tbl
        node_member_data <- map2(.x = node_member_var_names, .y = node_level_vectors, .f = create_tbl) %>% bind_cols(.)
        
        # add terminal_nodes to node_member_data
        node_member_data <- node_member_data %>% mutate(terminal_node = terminal_node_list)
        
        # get pathString for final_terminal_nodes
        final_terminal_node_pathStrings <- tree_output_w_hierarchy %>% filter(final_terminal_node == 1) %>% pull(pathString)
        
        # create find_final_terminal_node_for_each_node function
        find_final_terminal_node_for_each_node <- function(.x) {
                terminal_node_tbl <- tree_output_w_hierarchy %>% filter(row_number() == .x)
                current_terminal_node <- terminal_node_tbl %>% pull(terminal_node)
                current_pathString <- terminal_node_tbl %>% pull(pathString)
                beginning_of_pathStrings <- str_sub(string = tree_output_w_hierarchy$pathString, start = 1, end = nchar(current_pathString))
                membership_terminal_nodes <- tree_output_w_hierarchy$terminal_node[beginning_of_pathStrings == current_pathString]
                map2_dfr(.x = membership_terminal_nodes, .y = current_terminal_node, .f = flag_node_membership) %>%
                fill(vars_select(names(node_member_data), starts_with("member_of_node")), .direction = "up") %>% 
                        fill(vars_select(names(node_member_data), starts_with("member_of_node")), .direction = "down") %>% slice(1)
        }
        
        # create flag_node_membership function
        flag_node_membership <- function(.x, .y) {
                current_terminal_node <- .y
                current_membership_terminal_node <- .x
                current_membership_terminal_node_var_name <- str_c("member_of_node_", current_membership_terminal_node)
                current_membership_terminal_node_var_name_sym <- sym(current_membership_terminal_node_var_name)
                current_node_member_data <- node_member_data %>% filter(terminal_node == current_terminal_node) %>%
                        mutate(!!current_membership_terminal_node_var_name_sym := 1)
                current_node_member_data
        }
        
        # call find_final_terminal_node_for_each_node function
        node_membership_tbl <- map_dfr(.x = 1:nrow(tree_output_w_hierarchy), .f = find_final_terminal_node_for_each_node)
        
        # pivot node_membership_tbl to get which nodes a given node traces up to, instead of which nodes a given node percolates down to
        node_membership_tbl <- node_membership_tbl %>% gather(key = percolates_down_to_node, value = value, -c(terminal_node)) %>% 
                mutate(percolates_down_to_node = str_replace(string = percolates_down_to_node, 
                                                              pattern = "member_of_node_", replacement = "")) %>%
                mutate(traces_up_to_node = str_c("traces_up_to_node_", terminal_node)) %>% 
                select(-terminal_node) %>% spread(key = traces_up_to_node, value = value) %>% 
                rename(terminal_node = percolates_down_to_node) %>%
                data.frame() %>% arrange(as.numeric(terminal_node))

        # join node_membership_tbl with new_data
        node_membership_tbl <- tree_output_w_hierarchy %>% select(terminal_node, final_terminal_node) %>%
                left_join(node_membership_tbl, ., by = "terminal_node")
}

node_membership_tbl <- get_node_membership(tree_output_w_hierarchy = tree_output_w_hierarchy)
node_membership_tbl %>% data.frame()


#############################################################################


# get all node statistics 
# because it needs to allow for weighting of individual observations, the stats must be calculated from individual obs instead of tree_output

# add node_membership_tbl to new_data
glimpse(new_data)
new_data <- new_data %>% left_join(., node_membership_tbl, by = "terminal_node")
new_data %>% distinct(terminal_node, final_terminal_node, traces_up_to_node_1, traces_up_to_node_2) %>% data.frame()

# add weights
new_data <- new_data %>% mutate(weight = case_when(BEN_COUNTRY_OF_BIRTHMEXIC == 1 ~ 1.24, 
                   BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXM == 1 ~ 1.13, 
                   BEN_COUNTRY_OF_BIRTHMEXIC == 0 & BEN_SEXF == 1 ~ 1.04, TRUE ~ 1)) %>%
        mutate(weighted_outcome = (!!outcome_variable_sym) * weight)

new_data %>% distinct(BEN_COUNTRY_OF_BIRTHMEXIC, BEN_SEXM, BEN_SEXF, weight, weighted_outcome)

# output new_data
write_csv(new_data, path = "output/post_interview_wo_issue/output/post_interview/full/treatment_post_interview_full_w_tree_output_Case_OutcomeDenied.csv")

# get variable names for node dummies
node_var_names <- new_data %>% select(starts_with("traces_up_to_node")) %>% names(.)

# create get_all_node_stats function
get_all_node_stats <- function(.x) {
        
        # assign current_node_var_name and current_node_var_name_sym
        current_node_var_name <- .x
        current_node_var_name_sym <- sym(current_node_var_name)
        
        # get current_terminal_node
        current_terminal_node <- str_replace(string = current_node_var_name, pattern = "traces_up_to_node_", replacement = "")
        
        
        ###################################################

        
        # get outcome_p_weighted
        current_node_outcome_stats <- current_data %>% filter(!!current_node_var_name_sym == 1) %>% 
                summarize(outcome_p_weighted = sum(weighted_outcome) / sum(weight), obs_in_node = n(),
                          outcome_denominator_weight_sum = sum(weight), outcome_numerator_weight_sum = sum(weighted_outcome), 
                          outcome_positive_sum = sum(!!outcome_variable_sym))

        # use wilson formula to get confidence interval for outcome_p_weighted
        n_weighted <- current_node_outcome_stats %>% pull(outcome_denominator_weight_sum)
        outcome_p_weighted <- current_node_outcome_stats %>% pull(outcome_p_weighted)
        z <- 1.96
        
        upper_limit_outcome_p_weighted_value <- (1 / (2 * (n_weighted   + z^2)) ) * ( (2 * n_weighted * outcome_p_weighted + z^2) + 
                                                                                      (z * sqrt(4 * n_weighted * outcome_p_weighted * (1 - outcome_p_weighted) + z^2) ) )
        
        lower_limit_outcome_p_weighted_value <- (1 / (2 * (n_weighted + z^2)) ) * ( (2 * n_weighted * outcome_p_weighted + z^2) - 
                                                                                    (z * sqrt(4 * n_weighted * outcome_p_weighted * (1 - outcome_p_weighted) + z^2) ) )
        
        # save upper/lower_limit_outcome_p_weighted
        current_node_outcome_stats <- current_node_outcome_stats %>% 
                mutate(upper_limit_outcome_p_weighted = upper_limit_outcome_p_weighted_value,
                       lower_limit_outcome_p_weighted = lower_limit_outcome_p_weighted_value)
        
        
        ###################################################
   
        
        # get share_p_weighted
        current_node_share_stats <- current_data %>% filter(!!current_node_var_name_sym == 1) %>% 
                summarize(share_p_weighted = sum(weight) / sum(current_data$weight), obs_in_node = n(),
                          share_denominator_weight_sum = sum(current_data$weight), share_numerator_weight_sum = sum(weight)) %>% 
                mutate(lower_limit_share_p_weighted = 0, upper_limit_share_p_weighted = 0)
        
        # use wilson formula to get confidence interval for share_p_weighted
        n_weighted <- current_node_share_stats %>% pull(share_denominator_weight_sum)
        share_p_weighted <- current_node_share_stats %>% pull(share_p_weighted)
        z <- 1.96
        
        upper_limit_share_p_weighted_value <- (1 / (2 * (n_weighted   + z^2)) ) * ( (2 * n_weighted * share_p_weighted + z^2) + 
                                                                                              (z * sqrt(4 * n_weighted * share_p_weighted * (1 - share_p_weighted) + z^2) ) )
        
        lower_limit_share_p_weighted_value <- (1 / (2 * (n_weighted + z^2)) ) * ( (2 * n_weighted * share_p_weighted + z^2) - 
                                                                                            (z * sqrt(4 * n_weighted * share_p_weighted * (1 - share_p_weighted) + z^2) ) )
        
        # save upper/lower_limit_share_p_weighted
        current_node_share_stats <- current_node_share_stats %>% 
                mutate(upper_limit_share_p_weighted = upper_limit_share_p_weighted_value,
                       lower_limit_share_p_weighted = lower_limit_share_p_weighted_value)
        
        
        #####################################################
        
        
        # combine current_node_outcome_stats and current_node_share_stats
        current_node_stats <- bind_cols(current_node_outcome_stats, current_node_share_stats) %>% 
                select(-obs_in_node1) %>% mutate(terminal_node = current_terminal_node) 
        
        current_node_stats
}

current_data <- new_data
all_node_stats <- map_dfr(.x = node_var_names, .f = get_all_node_stats)
all_node_stats %>% data.frame()

# add all_node_stats to tree_output_w_hierarchy
tree_output_w_hierarchy <- all_node_stats %>% select(-obs_in_node) %>% left_join(tree_output_w_hierarchy, ., by = "terminal_node")
tree_output_w_hierarchy %>% data.frame()


###############################################################################


# # get all_node_stats for control group
# control_data <- read_csv("output/pre_interview/full/control_pre_interview_full_w_terminal_nodes_Case_OutcomeDenied.csv")
# glimpse(control_data)
# 
# # predict nodes for new_data
# # current_wd <- getwd()
# # setwd("H:/R/rpart")
# # source("predict_nodes.R")
# # setwd(current_wd)
# # 
# # control_data_pred_node <- predict_nodes(object = model, newdata = control_data)
# 
# # add control_data_pred_node to control_data
# # control_data <- control_data %>% mutate(terminal_node = as.character(control_data_pred_node))
# # control_data
# 
# # ensure terminal_node is a character so it can merge with node_membership_tbl
# control_data <- control_data %>% mutate(terminal_node = as.character(terminal_node))
# 
# # overwrite NA values for control Case_OutcomeDenied 
# # these are the result of control variable "outcome" having value of "R" for rejected, which were converted to NA in cleaning
# control_data %>% count(!!outcome_variable_sym)
# control_data <- control_data %>% mutate(!!outcome_variable_sym := case_when(is.na(!!outcome_variable_sym) ~ as.integer(0), 
#                                                                             TRUE ~ !!outcome_variable_sym))
# control_data %>% count(!!outcome_variable_sym)
# 
# # add weights to control
# control_data <- control_data %>% mutate(weight = case_when(ben_country_of_birth_groupedMEXIC == 1 ~ 1, 
#                                                    ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXM == 1 ~ 1, 
#                                                    ben_country_of_birth_groupedMEXIC == 0 & BEN_SEXF == 1 ~ 1, TRUE ~ 1)) %>%
#         mutate(weight = case_when(is.na(weight) ~ 1, TRUE ~ weight)) %>%
#         mutate(weighted_outcome = (!!outcome_variable_sym) * weight)
# 
# control_data %>% distinct(ben_country_of_birth_groupedMEXIC, BEN_SEXM, BEN_SEXF, weight, weighted_outcome)
# 
# # add node_membership_tbl to control_data
# glimpse(control_data)
# control_data <- control_data %>% left_join(., node_membership_tbl, by = "terminal_node")
# control_data %>% distinct(terminal_node, final_terminal_node, traces_up_to_node_1, traces_up_to_node_2) %>% data.frame()
# 
# # output control_data
# write_csv(control_data, path = "output/pre_interview/full/control_pre_interview_full_w_tree_output_Case_OutcomeDenied.csv")
# 
# # get control_all_node_stats
# current_data <- control_data
# control_all_node_stats <- map_dfr(.x = node_var_names, .f = get_all_node_stats)
# control_all_node_stats %>% data.frame()
# 
# # rename control_all_node_stats variables
# control_all_node_stats_variables <- names(control_all_node_stats)
# control_all_node_stats_variables <- str_c("control_", control_all_node_stats_variables)
# names(control_all_node_stats) <- control_all_node_stats_variables
# 
# # add all_node_stats to tree_output_w_hierarchy
# tree_output_w_hierarchy <- control_all_node_stats %>% left_join(tree_output_w_hierarchy, ., by = c("terminal_node" = "control_terminal_node"))
# tree_output_w_hierarchy %>% data.frame()


##############################################################################


# create node from tree_output_w_hierarchy
tree_output_w_hierarchy %>% data.frame()

tree_node <- as.Node(tree_output_w_hierarchy)
tree_node

tree_node$fieldsAll
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

plot(tree_node)


#####################################################################


# reorder tree_output_w_hierarchy to match tree_node order
# this allows for adding variables to from tree_output_w_hierarchy directly to tree_node
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

tree_output_w_hierarchy %>% select(terminal_node)
print(tree_node, "terminal_node")

# get tree_node_terminal_node_order
tree_node_terminal_node_order <- tibble(terminal_node = unname(tree_node$Get("terminal_node")))
tree_node_terminal_node_order

# reorder tree_output_w_hierarchy
tree_output_w_hierarchy <- left_join(tree_node_terminal_node_order, tree_output_w_hierarchy, by = "terminal_node")

tree_output_w_hierarchy %>% select(terminal_node)
print(tree_node, "terminal_node")


####################################################################


# add custom node fillcolor and fontcolor variables to tree_output_w_hierarchy

# inspect colors
# display.brewer.all()
# brewer.pal(n = 9, name = "Blues")

tree_output_w_hierarchy <- tree_output_w_hierarchy %>% mutate(custom_node_fill_color = ifelse(final_terminal_node == 1, "#08306B", "#9ECAE1"),
                                                              custom_node_font_color = ifelse(final_terminal_node == 1, "white", "black"))
tree_output_w_hierarchy %>% data.frame()


########################################################################


# create custom_split_leading_to_node edge labels
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")


# check next_split_at_node
split_leading_to_node
length(split_leading_to_node)

print(tree_node, "terminal_node", "node_path")

# create custom_next_split_at_node
# custom_split_leading_to_node <- c("root_node", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")
      
custom_split_leading_to_node <- c("root_node", "Ben. and pet. live together", "Ben. was married before",
                                  "Time since present marriage is less than or equal to 5 months",
                                  "Time since present marriage is greater than 5 months",
                                  "Ben. was not married before", "Ben. and pet. do not live together",
                                  "Ben. has not Entered Without Inspection",
                                  "Ben. has Entered Without Inspection")
  
length(custom_split_leading_to_node)
nrow(tree_output_w_hierarchy)

# add leading and trailing spaces
custom_split_leading_to_node <- str_c("  ", custom_split_leading_to_node, "     ")

# add custom_next_split_at_node to tree_node
tree_node$Set(custom_split_leading_to_node = custom_split_leading_to_node)

# inspect
tree_node$fieldsAll
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")


###################################################################


# create custom_node_labels
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

print(tree_node, "terminal_node")

tree_output_w_hierarchy %>% select(terminal_node)


# create custom_node_labels
# note than tree_node does not keep the same order as tree_output_w_hierarchy
custom_node_labels <- c("Ben. was married before?",
                        "Ben. region of birth is not Latin America?",
                        "Time since present marriage is less than or equal to 7 months?",
                        "Ben. was married before &\n ben. region of birth is Latin America &\n time since present marriage is less than or equal to 7 months",
                        "Ben. was married before &\n ben. region of birth is Latin America &\n time since present marriage is greater than 7 months",
                        "Ben. was married before &\n ben. region of birth is not Latin America",
                        "Ben. is in removal proceedings?",
                        "Ben. was not married before &\n ben. is in removal proceedings",
                        "Ben. was not married before &\n ben. is not in removal proceedings")
                        
length(custom_node_labels)
nrow(tree_output_w_hierarchy)


# add leading and trailing spaces
# custom_split_leading_to_node <- str_c("  ", custom_split_leading_to_node, "     ")

# add custom_next_split_at_node to tree_node
tree_node$Set(custom_node_labels = custom_node_labels)

# inspect
tree_node$fieldsAll
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")


#########################################################################


# set custom node style
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# get node label
GetNodeLabel <- function(node) {
        # str_c("Node ", node$terminal_node, "\n",
        #       node$predicted_class_name, "\n", 
        #       node$positive_class_obs_in_node, " / ", node$negative_class_obs_in_node, "\n",
        #       percent(node$prob_positive_class), " / ", percent(node$prob_negative_class), "\n",
        #       percent(node$pct_obs_in_node))
        
        # node$custom_node_labels
        
        current_terminal_node <- unname(node$Get("terminal_node"))[1]
        
        if(current_terminal_node == 1) {
                str_c("Denial rate", "\n", 
                      "IBFA: ", percent(node$outcome_p_weighted), 
                      " (", percent(node$lower_limit_outcome_p_weighted), 
                      "-", percent(node$upper_limit_outcome_p_weighted), ")",
                      "\n",
                      "\n",
                      "% of All Petitions", "\n",
                      "IBFA: 100%")
        } else {
                str_c("Denial rate", "\n", 
                      "IBFA: ", percent(node$outcome_p_weighted), 
                      " (", percent(node$lower_limit_outcome_p_weighted), 
                      "-", percent(node$upper_limit_outcome_p_weighted), ")",
                      "\n",
                      "\n",
                      "% of All Petitions", "\n",
                      "IBFA: ", percent(node$share_p_weighted),
                      " (", percent(node$lower_limit_share_p_weighted), 
                      "-", percent(node$upper_limit_share_p_weighted), ")")
        }
        
        
}

# write get_node_color function
set_node_style <- function(x) {
        current_terminal_node <- unname(x$Get("terminal_node"))[1]
        # print(current_terminal_node)
        
        # set custom variables for current_node
        current_node_fill_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_fill_color)
        current_node_font_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_font_color)
        
        # run SetNodeStyle for current_node
        SetNodeStyle(node = x, style = "filled, rounded", fontname = 'helvetica', label = GetNodeLabel(x),
                     shape = "box", fillcolor = current_node_fill_color, fontcolor = current_node_font_color, inherit = FALSE)
}


##################################################################



# set custom edge style
tree_output_w_hierarchy %>% data.frame()
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# get/set edge label
GetEdgeLabel <- function(node) {
        # node$split_leading_to_node
        node$custom_split_leading_to_node
}

# write get_node_color function
set_edge_style <- function(x) {
        # current_terminal_node <- unname(x$Get("terminal_node"))[1]
        # print(current_terminal_node)
        
        # set custom variables for current_node
        # current_node_fill_color <- tree_output_w_hierarchy %>% filter(terminal_node == current_terminal_node) %>% pull(custom_node_fill_color)

        # run SetNodeStyle for current_node
        SetEdgeStyle(node = x, fontname = 'helvetica', label = GetEdgeLabel(x), inherit = FALSE)
}


#################################################################


# create custom tree plot
print(tree_node, "terminal_node", "obs_in_node", "positive_class_obs_in_node", 
      "negative_class_obs_in_node", "prob_positive_class", "prob_negative_class", "pct_obs_in_node", "node_path", 
      "final_terminal_node", "split_leading_to_node", "pathString", "outcome_p_weighted", "share_p_weighted")

# set graph style
SetGraphStyle(tree_node, rankdir = "TB")

# set node style
tree_node$Do(function(x) { set_node_style(x) })

# set edge style
tree_node$Do(function(x) { set_edge_style(x) })

# plot tree
plot(tree_node)

# save plot
tree_plot_title <- str_c("post_interview_tree_plot_", outcome_variable)
tree_plot_title

filename <- str_c("output/post_interview_wo_issue/output/post_interview/train/", tree_plot_title, ".pdf")
filename

export_graph(graph = ToDiagrammeRGraph(tree_node), file_name = filename)




