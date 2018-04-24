# get terminal node # for  newdata 
# https://stackoverflow.com/questions/29304349/how-to-get-terminal-nodes-for-a-new-observation-from-an-rpart-object
predict_nodes <- function (object, newdata, na.action = na.pass) {
                where <-
                        if (missing(newdata)) 
                                object$where
                else {
                        if (is.null(attr(newdata, "terms"))) {
                                Terms <- delete.response(object$terms)
                                newdata <- model.frame(Terms, newdata, na.action = na.action, 
                                                       xlev = attr(object, "xlevels"))
                                if (!is.null(cl <- attr(Terms, "dataClasses"))) 
                                        .checkMFClasses(cl, newdata, TRUE)
                        }
                        rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
                }
                as.integer(row.names(object$frame))[where]
        }