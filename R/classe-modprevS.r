# Classe abstrata de modelos com shapeshifting

new_modprevS <- function(target_pipe, data_pipe, data_list, tipo, ...) {
    target_pipe <- parse_pipes(target_pipe, data_list, parent.frame())
    data_pipe   <- parse_pipes(data_pipe, data_list, parent.frame())

    data   <- combine_pipes(eval_pipes(data_pipe, data_list, parent.frame()))
    target <- combine_pipes(eval_pipes(target_pipe, data_list, parent.frame()))
    colnames(target)[2] <- "target"

    fulldata <- merge(target, data, by = 1)

    serie    <- ts(fulldata$target)
    fulldata <- fulldata[, -seq(2), drop = FALSE]

    estimamodelo(serie, fulldata, tipo = tipo, ...)
}
