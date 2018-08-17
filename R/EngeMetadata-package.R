#' A package for analyzing tissue spatiality in scRNAseq data.
#'
#' Description
#'
#' Handles R import of Google Drive based metadata for the Enge lab
#'
#' @name EngeMetadata
#' @docType package
#' @author Author: Jason T. Serviss
#' @keywords package
NULL
#' @importFrom utils globalVariables
## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", ".data"))
