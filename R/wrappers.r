#' @export
runConjoint <- function(dataset, fctCols, yCol) {
 prfls <- unique(dataset[,fctCols])
 prfls_int <- mutate(prfls, across(everything(), ~as.integer(as.factor(.))))
 dataset_chr <- dataset[,fctCols] %>% transmute(across(everything(), as.character))
 allLvls <- dataset_chr %>% as.matrix() %>% as.character() %>% unique()

 dataset$fctCombo <- match(x = as.character(tidyr::unite(dataset_chr,'')[[1]]),
                           table = as.character(tidyr::unite(prfls,'')[[1]]))
 col1nm <- names(dataset)[1]  # first col is assumed to be respondent's id
 datasetY <- dataset[, c(col1nm, 'fctCombo', yCol)]  # dplyr::pivot_wide requires unnese/unchop %>%
 #  group_by(!!as.name(col1nm)) %>% mutate(grouped_id = row_number()) %>%
 #  tidyr::spread(key = 'fctCombo', value = yCol, convert = TRUE) %>% dplyr::select(-grouped_id) %>%
 #  group_by(!!as.name(col1nm)) %>%
 #  summarise(across(everything(), ~ first(na.omit(.))))
 Y.wd <- matrix(nrow = length(unique(dataset[[col1nm]])), ncol = nrow(prfls),
                dimnames = list(unique(dataset[[col1nm]]), NULL))
 for (i in 1:nrow(datasetY)) Y.wd[datasetY[[col1nm]][i], datasetY$fctCombo[i]] <- datasetY[[yCol]][i]

 Conjoint(y = as.data.frame(Y.wd),
          x = as.data.frame(prfls_int),
          z = as.data.frame(allLvls, stringsAsFactors = TRUE))
}
