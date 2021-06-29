
#' For each level combination from two variables, calculate an index comparing grouped distribution to a total one in a dataset
#'
#' Implementation: tidyverse/dplyr. Used in EDA (exploratory data analysis) part of assignment.
#' Example. Grouping variable: s_gender. Index variable: s_age. Output: For each level in s_gender, calculate the index of each s_age level.
#' An index compares the grouped distribution compared to the total distribution. If for example, 50% of all males are between 24 and 35, but only 40% of the total population, the index would be 50/40 *100 = 125.
#' @param grpVar A character vector of length = 1, with a variable (column) name from \code{dataset}, values from which will be used for grouping index calculations
#' @param idxVar A character vector of length = 1, with a variable (column) name from \code{dataset}, for each value of which an index will be calculated per \code{grpVar}
#' @param dataset A tibble with tidy data (variables in columns and observations in rows). Factors are better suited for categorical variables (otherwise, may error due to missing records for some group combinations, e.g. Mixed race and Others aged 65+)
#'
#' @return A tibble with 3 columns: levels of \code{grpVar}, levels of \code{idxVar} for each, and index (idx column) for each of those level combinations
#'
#' @examples
#'
#' require(haven)
#' require(dplyr)
#' survey_data <- read_sav('experRrvey/extdata/survey_data.sav')  #  not needed for the included dataset
#' survey_data$s_age <- factor(survey_data$s_age, levels =
#'                              unique(survey_data$s_age)[
#'                                order(as.numeric(gsub("(^[[:digit:]]{1,2}).*",
#'                                                 "\\1", unique(survey_data$s_age))))])
#' survey_data$s_gender <- as_factor(survey_data$s_gender)
#' calcIdx("s_gender", "s_age", survey_data)
#'
#' @export
#'
#'
#'
calcIdx <- function(grpVar, idxVar, dataset, resp.na.rm = TRUE, group.na.rm = TRUE) {
  stopifnot(inherits(dataset, "tbl"))
  stopifnot(inherits(dataset[[idxVar]], "factor") && inherits(dataset[[grpVar]], "factor"))
  dataset <- if (resp.na.rm) na.omit(dataset[,c(grpVar, idxVar)]) else dataset[,c(grpVar, idxVar)]
  dataset %>%
    group_by(!!as.name(grpVar), !!as.name(idxVar), .drop = group.na.rm) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    mutate(rel.freq = n / sum(n) * 100,
           N = count(dataset,
                     !!as.name(idxVar), .drop = group.na.rm)[['n']],
           idx = rel.freq / (N / nrow(dataset) * 100) * 100) %>%
    ungroup() %>% dplyr::select(-rel.freq, -N, -n)
}

#'
#' @export
plotIdxPair <- function(dtset, idxtbl, h_var = NULL, v_var = NULL) {
 if (is.null(h_var)) h_var <- grep(inv = T, "idx", names(idxtbl), val = T)
 if (is.null(v_var)) {
  v_var <- grep(inv = T, paste("idx",sep="|",h_var[1]), names(idxtbl), val = T)
  h_var <- h_var[1]
 } else {
  h_var <- grep(inv = T, paste("idx",sep="|",v_var[1]), names(idxtbl), val = T)
 }

 dataPlot <-
   dtset[,c(h_var, v_var)] %>%
     group_by(!!as.name(h_var), !!as.name(v_var)) %>%
     count() %>%
     left_join(idxtbl)

 ggplot(dataPlot, aes(x = !!as.name(h_var), y = n)) +
  geom_col(aes(fill = !!as.name(v_var), alpha = idx))
}

#'
#' @export
plotLikertSorted <- function(savtibble, delim = " - ") {

 savtibble %<>% setNames(nm = sapply(., attr, "label"))
 topAvgLvl <- order(decreasing = TRUE, sapply(savtibble[,-1], function(x)
                             sum(as.integer(x) >= median(1:length(levels(x))),
                                 na.rm = TRUE) / sum(!is.na(x))))  # weight of +

 names(savtibble)[-c(1,topAvgLvl[1] + 1)] %<>% strsplit(split = " - ") %>% sapply(`[`, -1)

 survLP <- savtibble[,-1] %>%
           as.data.frame() %>%
           likert() %>% plot(ordered = T)  # crashed RStudio once, unstable?

 return(survLP)
}

#'
#' @export
calcIdxsAll <- function(dataset) {
 idxList <- vector(mode = 'list', length = ncol(dataset)) %>%
              setNames(names(dataset))  # omit response_id & task
 for (i in names(idxList)) {
   for (k in names(idxList)) {
     if (k == i) next
     if (!is.null(idxList[[k]][[i]])) next  # to avoid symmetric/double computations
     idxList[[i]][[k]] <-
      tryCatch(calcIdx(i, k, dataset, FALSE, FALSE), error = function (e1)
        tryCatch(calcIdx(k, i, dataset, FALSE, FALSE)[,c(i, k, 'idx')] %>%
                  arrange(!!as.name(i), !!as.name(k)),  # pre-cautions: i <-> k, na.rm
                 error = function(e2) calcIdx(i, k, dataset, resp.na.rm = TRUE, group.na.rm = FALSE)))
   }
 }
 return(idxList)
}

#'
#' @export
idxList2tbl <- function(idxL) {
  upperNms <- setNames(as.list(names(idxL)), names(idxL))
  lapply(upperNms, function (i_name)
        lapply(setNames(as.list(names(idxL[[i_name]])), names(idxL[[i_name]])),
               function(k_name) mutate(idxL[[i_name]][[k_name]],
                                       grpVar = i_name, lvlVar = k_name) %>%
                                rename(grpVal = !!as.name(i_name),
                                       lvlVal = !!as.name(k_name)))) %>%
  Reduce(f = rbind) %>%
  Reduce(f = rbind) %>%  # unfortunately this is very slow, optimize with map
  dplyr::select(grpVar, grpVal, lvlVar, lvlVal, idx) %>%
  mutate(grpVal = as.character(grpVal), lvlVal = as.character(lvlVal)) %>% distinct()
}



# currently only works with categorical data (still?)
# too slow (over 25 mins), test on smaller dataset
# or rewrite using calcIdx

#' @export
rateResponses <- function(sveyRes, idxset, weights = 1L, idxcol = 'idx', onlyIntensities = FALSE) {  # first col should be respondent
  stopifnot(identical(weights, 1L) || NROW(weights) == nrow(sveyRes))
  respondents <- NULL
  if (!class(sveyRes[[1]]) == 'factor') {
    respondents <- sveyRes[, 1, drop = FALSE]
    sveyRes <- sveyRes[,-1]
  }
  stopifnot(all(sapply(sveyRes, is.factor)))
  stopifnot(all(names(sveyRes) %in% c(idxset$grpVar, idxset$lvlVar)))
  uniqColPairs <- combn(1:ncol(sveyRes), m = 2)
  sveyRespIdxs <- sveyRes
  sveyRespIdxs[,] <- 0
  for (i in 1:ncol(uniqColPairs)) {
    sveyRespIdxs[,uniqColPairs[,i]] <- sveyRespIdxs[,uniqColPairs[,i]] +
     idxset %>% filter(grpVar == names(sveyRes[, uniqColPairs[,i]])[1] &
                       lvlVar == names(sveyRes[, uniqColPairs[,i]])[2]) %>%
     dplyr::select("grpVal", "lvlVal", !!as.name(idxcol)) %>%
     {.[match(tidyr::unite(sveyRes[, uniqColPairs[,i]], "")[[1]],
              tidyr::unite(.[,c('grpVal', 'lvlVal')], "")[[1]]),][[idxcol]]} %>%
     na.fill(0)
  }
  sveyRespIdxs <- sveyRespIdxs / (ncol(uniqColPairs) - 1)
  if (onlyIntensities) sveyRes[,] <- 1
  sveyRes %>% mutate(across(everything(), as.integer)) %>%
   {bind_cols(respondents, . * weights * sveyRespIdxs, rtng = rowMeans(sveyRespIdxs, na.rm = TRUE))}
}

#'
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
