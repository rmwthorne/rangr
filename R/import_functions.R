#' Reads a HOMER peak/position file.
#'
#' \code{read_pos} takes a HOMER peak file as input and tries to automatically find the columns that describe genomic ranges. It returns them as a GRanges object.
#'
#' @param file A HOMER-formatted peak file (PeakID, chr, start, stop ...)
#' @param keep_all If \code{TRUE}, will keep all metadata columns.
#' @param message Include messages for column parsing?
#' @return A GenomicRanges object
#' @export

read_pos <- function(file, keep_all = F, message = F) {
  # Read in the header
  header <- readr::read_lines(file, n_max = 50)
  # Grab the column names from line starting #PeakID...
  colnames <- header %>%
    subset(., grepl("^#PeakID", ., ignore.case = T)) %>%
    strsplit("\t")
  # Read in the file
  read_tsv(file, col_names = colnames[[1]], comment = "#", ) %>%
    makeGRangesFromDataFrame(keep.extra.columns = keep_all)
}

#' Truncates long column names matching pattern.
#'
#' @param df A data frame.
#' @param pattern character string containing a regular expression to trim.
#' @param replacement a replacement for matched pattern in sub and gsub.
#' @return A dataframe with truncated columns.
#' @export
#' @examples
#' gr <- GRanges(Rle(c("chr2", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
#' IRanges(1:10, width=10:1, names=head(letters, 10)),
#' Rle(strand(c("-", "+", "*", "+", "-")), c(1, 2, 2, 3, 2)),
#' score=1:10, GC=seq(1, 0, length=10))
#' df <- as.data.frame(gr)
#' df
#' df <- truncate_cols(df, pattern="re{0,}?.$")
#' df

truncate_cols <- function(df, pattern = pattern, replacement = replacement) {
  cnames <- colnames(df)
  colnames(df) <- gsub(pattern = pattern, replacement = replacement, x = cnames)
  return(df)
}

#' Truncates homer column names matching .Tag.Count(...)
#'
#' @param df A data frame.
#' @param pattern character string containing a regular expression to trim.
#' Defaults to ".Tag.Count.\{0,\}?.$".
#' @param replacement a replacement for matched pattern in sub and gsub.
#' Defaults to "".
#' @return A dataframe with truncated columns.

truncate_homer <- function(df, pattern = ".Tag.Count.{0,}?.$", replacement = "") {
  truncate_cols(df, pattern = pattern, replacement = replacement)
}
