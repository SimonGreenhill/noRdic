
#' read.nordic
#'
#' Reads a Nordic data CSV in this format:
#'
#'      AREA GENUSE       SPUSE
#'   1  SWE      F F_alcoholic
#'   2  ICE      F F_alcoholic
#'
#' ... and converts it to a long format table:
#'
#'      AREA   TYPE                USE
#'  1   SWE GENUSE                 F
#'  2   ICE GENUSE                 F
#'
#' @param filename - filename to load
#' @param ... - other arguments passed to read.csv
#'
#' @return a dataframe
#' @export
read.nordic <- function(filename) {
    data <- readr::read_csv(filename, show_col_types = FALSE)
    data <- as.data.frame(data)
    # check the format looks right
    if (!all(c("AREA", "GENUSE", "SPUSE") %in% colnames(data))) {
        stop(sprintf("File %s does not look like a Nordic CSV", filename))
    }
    tidyr::gather(data, 'TYPE', 'USE', c("GENUSE", "SPUSE"))
}


#' to.rayDISC
#'
#' Constructs a data frame for rayDISC, adding in missing taxa
#'
#'     AREA   TYPE          USE
#'  1  NORN GENUSE           IC
#'  2   SWE GENUSE           IC
#'  3   DEN GENUSE           IC
#'
#' ... and this command:
#'
#'   > augment(df, c("NORN", "SWE", "DEN", "A", "B"))
#'
#'     Taxon State
#'     AREA   TYPE          USE    STATE
#'  1  NORN GENUSE           IC       1
#'  2   SWE GENUSE           IC       1
#'  3   DEN GENUSE           IC       1
#'  4     A GENUSE           IC       0
#'  5     B GENUSE           IC       0
#'
#' @param df - a dataframe
#' @param taxa - a vector of taxa that should be present.
#' @param state - the state code to add (default= "0")
#'
#' @return a dataframe
#' @export
to.rayDISC <- function(df, taxa, state='0') {
    if ('STATE' %in% colnames(df) == FALSE) { df$STATE <- '1' }
    # remove duplicate rows if present.
    df <- df[!duplicated(df), ]

    if (!all(taxa %in% df$AREA)) {
        df <- rbind(df, data.frame(
            AREA = taxa[taxa %in% df$AREA == FALSE],
            TYPE = df$TYPE[[1]],
            USE = df$USE[[1]],
            STATE = state
        ))
    }
    df
}


plot_noRdic <- function(tree, data, result) {
    get_tip_color <- function(s) ifelse(s == '1', 'black', 'white')
    rownames(data) <- data$AREA
    colors <- sapply(data[tree$tip.label, 'STATE'], get_tip_color, simplify=TRUE, USE.NAMES=FALSE)

    corHMM::plotRECON(tree, result$states, pie.cex=1, font=1, cex=1.4)
    tiplabels(pch = 21, col = "#333333", bg = colors, adj = 0.45, cex = 2)
    nodelabels(round(result$states[, 2], 2), adj=-0.8, cex=0.8, frame="none", bg="none")
}



