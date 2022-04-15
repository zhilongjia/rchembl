#' get_properties
#'
#' query molecule information, including properties, structural representations and synonyms of drugs.
#' @param ChEMBLIDs a vector of ChEMBL IDs, such as c("CHEMBL4303288", "CHEMBL1380")
#' @param verbose verbose.
#' #' @import dplyr
#' @import jsonlite
#' @examples
#' ChEMBLIDs <- c("CHEMBL4303288", "CHEMBL1380", "CHEMBL345714")
#' res_tib <- get_properties(ChEMBLIDs)
#' dim(res_tib)
#' @export
#'
get_properties <- function(ChEMBLIDs, verbose=FALSE) {

    joined_res_list <- list()

    for (ChEMBLID in ChEMBLIDs ) {
        # ChEMBLID <- ChEMBLIDs[1]
        # ChEMBLID <- "CHEMBL1380"
        if (verbose) {print (ChEMBLID)}
        api_base <- "https://www.ebi.ac.uk/chembl/api/data/molecule.json?molecule_chembl_id="
        query_url <- paste0(api_base, ChEMBLID)
        res_list <- jsonlite::fromJSON(query_url)
        res_tibble <- res_list$molecules
        if (verbose) {print (dim(res_tibble))}

        # multi-pages
        while( !is.null(res_list$page_meta$"next") ) {
            print (res_list$page_meta$total_count)
            res_list <- fromJSON(paste0("https://www.ebi.ac.uk", res_list$page_meta$"next") )
            res_tibble <- dplyr::bind_rows(res_tibble, res_list$atc)
        }
        joined_res_list[[ChEMBLID]] <- res_tibble

    }

    joined_res_tibble_raw <- dplyr::bind_rows(joined_res_list)

    return(joined_res_tibble_raw)

}
