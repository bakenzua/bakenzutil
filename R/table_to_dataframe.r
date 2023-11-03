#' table_to_dataframe Expand a table object of counts to a dataframe
#'
#' Expands a table object of counts to a dataframe, ie an inverse table() function.
#' Maybe should have worked harder on the google fu and stack overflow beforehand...
#'
#' @param t Table object
#'
#' @return dataframe of groups and outcomes
#' @export
table_to_dataframe <- function(t) {
    # setup
    group_name <- names(dimnames(t))[1]
    if(is.null(group_name)){ group_name <- "treatment"}
    groups <- dimnames(t)[[names(dimnames(t))[1]]]
    outcome_name <- names(dimnames(t))[2]
    if(is.null(outcome_name)){ outcome_name <- "outcome"}
    outcomes <- dimnames(t)[[names(dimnames(t))[2]]]
    margin_table <- addmargins(t)

    group_vec <- c()
    outcome_vec <- c()
    for (i in 1:length(groups)) {
        group_vec <- c(group_vec, rep(groups[i], margin_table[i,"Sum"]))
        for(j in 1:length(outcomes)){
            outcome_vec <- c(
                outcome_vec,
                rep(outcomes[j], margin_table[i, j])
            )
        }
    }
    result <- data.frame(
        group_name = group_vec,
        outcome_name = outcome_vec
    )
    colnames(result) <- c(group_name, outcome_name)
    return(result)
}