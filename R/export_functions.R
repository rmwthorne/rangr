

#' Save a GRanges object to a bed file.
#'
#' @param gr A GRanges object.
#' @param path Path to write to.
#' @param ucsc_name if \code{TRUE}, will add a track name for UCSC.
#' @return writes an object to a specific path.
#' @examples
#' gr <- GRanges(Rle(c("chr2", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
#' IRanges(1:10, width=10:1, names=head(letters, 10)),
#' Rle(strand(c("-", "+", "*", "+", "-")), c(1, 2, 2, 3, 2)),
#' score=1:10, GC=seq(1, 0, length=10))
#' write_bed(gr, path = "output/file.ucsc.bed", ucsc = TRUE)
#' @export

write_bed <- function(gr, path = "output/", ucsc = F, ucsc.col = "",
                      name.col = "", score.col = "") {
  # Makes a bed file from a granges object
  # name.col and score.col not working yet.
  df <- gr %>% as.data.frame
  df <- df %>% select(seqnames, start, end)
  if (ucsc) {
    cat(paste0("track name=\"",file,"\"\n"), file = paste0(dir,file,".bed"))
  }
  write.table(df, file = paste0(dir,file,".bed"), append = T,
              quote = F, sep = "\t", row.names = F, col.names = F)
}

