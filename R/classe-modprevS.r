####################################################################################################
# CLASSE ABSTRATA CONTENDO MODELOS COM SHAPESHIFTING
####################################################################################################

# CONSTRUTOR ---------------------------------------------------------------------------------------

#' Estimacao De Modelos Com Shapesifting
#' 
#' Wrapper para estimacao de modelos com shapeshifting
#' 
#' Esta funcao nao deve ser chamada diretamente pelo usuario, mas sim internamente por 
#' [`estimamodelo`] quando os argumentos `data_pipe`, `target_pipe` e `data_list` sao usados.
#' Descricoes mais detalhadas da estimacao de modelos, periodicos ou nao, devem ser buscadas em 
#' [`estimamodelo`] diretamente.
#' 
#' @param target_pipe pipes brutos indicando como extrair a serie alvo a ser modelada
#' @param data_pipe pipes brutos indicando como extrair as variaveis explicativas a serem usadas
#' @param data_list lista nomeada contendo os dados a serem usados na extracao via pipes
#' @param tipo tipo de modelo a ser ajustado. Ver [`estimamodelo`]
#' @param ... demais parâmetros passados para as funções de fit específicas de cada modelo
#' 
#' @return Objeto da classe `modprev_S`

estimamodelo_S <- function(target_pipe, data_pipe, data_list, tipo, ...) {
    target_pipe <- parse_pipes(target_pipe, data_list, parent.frame())
    data_pipe   <- parse_pipes(data_pipe, data_list, parent.frame())

    data   <- combine_pipes(eval_pipes(data_pipe, data_list, parent.frame()))
    target <- combine_pipes(eval_pipes(target_pipe, data_list, parent.frame()))
    colnames(target)[2] <- "target"

    fulldata <- merge(target, data, by.x = colnames(target)[1], by.y = colnames(data)[1])

    serie    <- ts(fulldata$target)
    fulldata <- fulldata[, -seq(2), drop = FALSE]

    mod <- estimamodelo(serie, regdata = fulldata, tipo = tipo, ...)

    new_modprevS(modelo = mod, data_pipe = data_pipe, target_pipe = target_pipe)
}

new_modprevS <- function(modelo, data_pipe, target_pipe) {
    structure(
        list(
            modelo      = modelo,
            data_pipe   = data_pipe,
            target_pipe = target_pipe
        ),
        class = c("modprevS", class(modelo))
    )
}

# METODOS ------------------------------------------------------------------------------------------

predict.modprevS <- function(object, n.ahead, newdata_list, feedback_fun = NULL, ...) {
    if (!is.null(feedback_fun)) {
        predict_recursive_modprevS(object, n.ahead, newdata_list, feedback_fun, ...)
    } else {
        predict_default_modprevS(object, n.ahead, newdata_list, ...)
    }
}

predict_default_modprevS <- function(object, n.ahead, newdata_list, ...) {
    data_pipe <- object$data_pipe
    data <- combine_pipes(eval_pipes(data_pipe, newdata_list, parent.frame()))

    prev <- predict(object$modelo, n.ahead = n.ahead, newdata = data, ...)

    return(prev)
}

predict_recursive_modprevS <- function(object, n.ahead, newdata_list, feedback_fun, ...) {
    data_pipe <- object$data_pipe
    modelo <- object$modelo

    pred_list <- list()

    for (i in seq_len(n.ahead)) {
        data <- combine_pipes(eval_pipes(data_pipe, newdata_list, parent.frame()))

        pred_list[i] <- list(predict(modelo, newdata = data, ...))

        feedback_data <- feedback_fun(c(pred_list[[i]][, 1]), newdata_list)
        newdata_list[names(feedback_data)] <- feedback_data
    }

    start <- start(pred_list[[1]])
    freq  <- frequency(pred_list[[1]])
    pred <- ts(do.call(rbind, pred_list), start = start, frequency = freq)
    colnames(pred) <- c("prev", "sd")

    return(pred)
}