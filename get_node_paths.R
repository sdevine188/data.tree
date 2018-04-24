# model_fit <- treatment_pre_interview_train_rpart
# output_terminal_nodes <- treatment_pre_interview_train_rpart_output_terminal_nodes

# node_path <- path.rpart(m1_denial_treatment_pre_interview_train_rpart, 7)
# str(node_path)
# node_path[[1]]
# names(node_path)

get_node_paths <- function(model_fit, output_terminal_nodes) {
        
        # create placeholder df to store all terminal node paths
        node_paths <- data.frame()
        
        # loop through terminal nodes getting node paths
        for(i in 1:nrow(output_terminal_nodes)) {
                current_terminal_node <- output_terminal_nodes$terminal_node[i]

                # get node path
                path_rpart_output <- path.rpart(model_fit, current_terminal_node)
                
                # clean node path
                current_node_path_value <- str_c(path_rpart_output[[1]], collapse = " & ")
                current_node_path_value <- str_replace(current_node_path_value, pattern = "root & ", replacement = "")
                current_node_path_value <- str_replace_all(current_node_path_value, pattern = "<", replacement = " <")
                current_node_path_value <- str_replace_all(current_node_path_value, pattern = "<=", replacement = "<= ")
                current_node_path_value <- str_replace_all(current_node_path_value, pattern = ">", replacement = " >")
                current_node_path_value <- str_replace_all(current_node_path_value, pattern = ">=", replacement = ">= ")

                # create current_node_path_output with terminal node
                current_node_path_output <- data.frame(terminal_node = current_terminal_node, node_path = current_node_path_value)
                                
                # rbind current_node_path to node_paths output
                node_paths <- rbind(node_paths, current_node_path_output)
        }
        
        # clean and return node_paths
        node_paths <- node_paths %>% mutate(terminal_node = as.character(terminal_node), node_path = as.character(node_path))
        node_paths
}


# test get_node_paths
# test_node_paths <- get_node_paths(model_fit, output_terminal_nodes)
# test_node_paths
# glimpse(test_node_paths)
