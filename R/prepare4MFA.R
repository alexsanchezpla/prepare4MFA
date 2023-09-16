#' Build a table with information about samples in a data frame
#'
#' @description
#' May be useful when, given a dataset, one wishes to create a table
#' that will be later used to visualize individuals by groups
#' Notice that some columns of this table might be variables in the original dataset.
#'
#' The function returns a data frame with
#' - the index,
#' - name,
#' - Group of each individual in a data frame.
#' The type of a variable is determined based on whether it is a factor/character or not.
#'
#' @param x A data frame containing the variables to be analyzed.
#'
#' @return A data frame with the index, name and Group of each variable in the data frame.
#'
#' @examples
#' samplesTable(iris)
#'
#' @export
samplesTable <- function(x){
  index <- 1:nrow(x)
  sampleID <- rownames(x)
  df <- data.frame(index=index, IDs=sampleID,
                   Group="pendingDefinition")
  rownames(df) <- rownames(x)
  df
}

#' Build a table with information about variables in a data frame
#'
#' @description
#' May be useful when, given a dataset, one wishes to create a table
#' that will be later used to define subgroups
#'
#' The function returns a data frame with
#' - the index,
#' - name,
#' - class and
#' - type of each variable in a data frame.
#' The type of a variable is determined based on whether it is a factor/character or not.
#'
#' @param x A data frame containing the variables to be analyzed.
#'
#' @return A data frame with the index, name, class and type of each variable in the data frame.
#'
#' @examples
#' varsTable(iris)
#'
#' @export
varsTable <- function(x){
  index <- 1:ncol(x)
  vars <- colnames(x)
  clase <- sapply(x, class)
  tipo <- sapply(x, function (column)
    ifelse (is.factor(column) | is.character(column), "n", "c"))
  df <- data.frame(index=index, vars=vars,
                   clase=clase, tipo=tipo,
                   Group="pendingDefinition")
  rownames(df) <- colnames(x)
  df
}

#' Extract the subset of variables associated with a given label.
#'
#' @description
#' This function returns the variable names associated with a given label in a data frame.
#'
#' @param varsAndGroupsDF A data frame with the variables and their associated labels.
#' @param label The label for which we want to extract the variables.
#'
#' @return A character vector with the names of the variables associated with the given label.
#'
#' @examples
#' varsAndGroupsDF <- varsTable(iris)
#' varsAndGroupsDF$Group <- c(rep("Sepal",2),rep("Petal",2), "Species")
#' extractVarNames(varsAndGroupsDF, "Sepal")
#'
#' @export
extractVarNames <- function(varsAndGroupsDF, label){
  return(with(varsAndGroupsDF, vars[Group==label]))
}

#' MultMerge2
#'
#' @description
#' Combina varios data frames en un único data frame,
#' manteniendo todas las filas y columnas únicas de cada uno.
#'
#' @param lst Una lista de data frames que se fusionarán.
#' @param all.x Si TRUE, incluir todas las filas de los data frames en la lista original.
#' @param all.y Si TRUE, incluir todas las filas de los data frames en la lista nueva.
#' @param by Nombre de la columna por la que se unirán los data frames (opcional).
#'
#' @return Un nuevo data frame fusionado.
#'
#' @examples
#' df1 <- data.frame(matrix(rnorm(20), nrow=10))
#' df2 <- data.frame(min=letters[1:8], may=LETTERS[1:8])
#' dfList <- list(df1, df2)
#' MultMerge2(dfList)
#'
#' df1 <- data.frame(ID = 1:10, Value = runif(10))
#' df2 <- data.frame(ID = 6:15, Value = runif(10))
#' df3 <- data.frame(ID = 1:10, Value = runif(10))
#' dfList <- list(df1, df2, df3)
#' MultMerge2(dfList, all.x = FALSE, all.y = FALSE, by = "ID")
#'
#' @import DescTools
#'
#' @export
MultMerge2 <- function (lst, all.x = TRUE, all.y = TRUE, by = NULL)
{
  # lst <- list(...) # The original version had "..." instead of "list" as argument
  if (length(lst) == 1)
    return(lst[[1]])
  if (!is.null(by)) {
    for (i in seq_along(lst)) {
      rownames(lst[[i]]) <- lst[[i]][[by]]
      lst[[i]][by] <- NULL
    }
  }
  unames <- DescTools::SplitAt(make.unique(unlist(lapply(lst, colnames)),
                                           sep = "."),
                               cumsum(sapply(utils::head(lst, -1), ncol)) + 1)
  for (i in seq_along(unames)) colnames(lst[[i]]) <- unames[[i]]
  res <- Reduce(function(y, z) merge(y, z, all.x = all.x, all.y = all.x, sort=FALSE),
                lapply(lst, function(x) data.frame(x, rn = row.names(x))))
  rownames(res) <- res$rn
  res$rn <- NULL
  seq_ord <- function(xlst) {
    jj <- character(0)
    for (i in seq_along(xlst)) {
      jj <- c(jj, setdiff(xlst[[i]], jj))
    }
    return(jj)
  }
  ord <- seq_ord(lapply(lst, rownames))
  res[ord, ]
  if (!is.null(by)) {
    res <- data.frame(row.names(res), res)
    colnames(res)[1] <- by
    rownames(res) <- c()
  }
  return(res)
}

#' Extracts a subgroup of columns from a data matrix
#'
#' @description
#' This function extracts a subgroup of columns from a data matrix given the position of the subgroup to be extracted and a vector of sizes that defines the number of columns in each continuous subgroup in the data.
#'
#' @param x a data matrix
#' @param pos the position of the subgroup to be extracted
#' @param vecOfSizes a vector of sizes that defines the number of columns in each continuous subgroup in the data.
#'
#' @return a data frame containing the subgroup of columns
#'
extractGroup <- function (x, pos, vecOfSizes){
  if (pos==1){
    first<- 1
    last <-vecOfSizes[pos]
  }else{
    if (pos==length(vecOfSizes)){
      first<- sum(vecOfSizes[-pos])+1
      last <- sum(vecOfSizes)
    }else{
      first<- sum(vecOfSizes[1:(pos-1)])+1
      last <- sum(vecOfSizes[1:pos])
    }
  }
  groupData <- x |>
    as.data.frame.array() |>
    dplyr::select (first:last)
  return(groupData)
}

#' Muestra un texto en la consola con un formato especial.
#'
#' @param aText El texto que se mostrará en la consola.
#'
showText <- function (aText){
  cat("\n",aText,"\n")
  cat(paste(rep("=", nchar(aText)), collapse=""),"\n")
}

#' Comprueba la estructura factorial de los grupos creados.
#'
#' @param mylistOfGroups La lista de grupos de variables a analizar.
#'
#' @return Un marco de datos que muestra la estructura factorial de los grupos.
#'
#' @export
checkFactorialStructure <- function (mylistOfGroups)
{
  mylistOfDataSets <- mylistOfGroups$groupsData
  uniqueDataSet <- MultMerge2 (mylistOfDataSets)
  varsList <- colnames(uniqueDataSet)
  actualTypes <- sapply(uniqueDataSet[,varsList], class)
  grupos <- character()
  tipos <- character()
  for (i in 1:length(mylistOfDataSets)){
    groupNames <- mylistOfGroups$groupsNames[i]
    groupSizes <- mylistOfGroups$groupsSizes[i]
    groupTypes <- mylistOfGroups$groupTypes[i]
    grupos <- c(grupos, rep(groupNames, groupSizes))
    assignedTypes <- tipos <- c(tipos, rep(groupTypes, groupSizes))
  }
  showText("Global dimensions")
  print(dim(uniqueDataSet))
  showText("Structure of each group")
  showGroupsinList(mylistOfGroups$groupsVars)
  showText("Data type vs group label")
  print(table(actualTypes, assignedTypes))
  return(data.frame(Variable= varsList, TipoActual=actualTypes,
                    Grupo=grupos, TipoAsignado= tipos ))
}

#' Muestra información sobre los grupos en una lista de variables.
#'
#' @param alistOfVars La lista de variables a mostrar.
#'
#'
showGroupsinList <- function(alistOfVars){
  for (i in 1:length(alistOfVars)){
    cat(names(alistOfVars[i]), "\n")
    cat("\t", "numVars = ", alistOfVars[[i]][[2]], "\n")
    cat("\t", "groupName = ", alistOfVars[[i]][[3]], "\n")
    cat("\t", "groupType = ", alistOfVars[[i]][[4]], "\n")
  }
}

