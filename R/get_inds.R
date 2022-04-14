#' Get Drug indications based on ChEMBL IDs
#'
#'
#'
#'
#' @param ChEMBLIDs a vector of ChEMBL IDs, such as c("CHEMBL4303288", "CHEMBL1380")
#' @param full FASLE (default) or TRUE. outputs molecule_chembl_id, mesh_heading, efo_term or all the related information
#' @param verbose verbose.
#' @return a tibble object with 3 columns, consisting of molecule_chembl_id, mesh_heading, efo_term.
#' @import dplyr
#' @import jsonlite
#' @examples
#'
#' ind_tib <- get_inds(c("CHEMBL4303288", "CHEMBL1380"))
#' dim(ind_tib)
#'
#'
#' @export
#'
get_inds <- function(ChEMBLIDs,full=FALSE, verbose=FALSE) {

    indications_list <- list()

    for (ChEMBLID in ChEMBLIDs ) {

        print (ChEMBLID)
        ind_api <- "https://www.ebi.ac.uk/chembl/api/data/drug_indication.json?molecule_chembl_id="
        ind_url <- paste0(ind_api, ChEMBLID)
        ind_list <- jsonlite::fromJSON(ind_url)
        ind_tibble <- ind_list$drug_indications
        if (verbose) {print (dim(ind_tibble))}

        # multi-pages
        while( !is.null(ind_list$page_meta$"next") ) {
            print (ind_list$page_meta$total_count)
            ind_list <- fromJSON(paste0("https://www.ebi.ac.uk", ind_list$page_meta$"next") )
            ind_tibble <- dplyr::bind_rows(ind_tibble, ind_list$drug_indications)
        }
        indications_list[[ChEMBLID]] <- ind_tibble

    }

    indications_tibble_raw <- dplyr::bind_rows(indications_list)

    if (full) {
        return(indications_tibble_raw)
    } else {
        # reformat to tibble
        indications_tibble <- data.frame(molecule_chembl_id=ChEMBLIDs)

        mesh_heading_tibble <-  dplyr::group_by(indications_tibble_raw, molecule_chembl_id) %>%
            dplyr::summarise(mesh_heading=pasteX(mesh_heading))
        efo_term_tibble <-  dplyr::group_by(indications_tibble_raw, molecule_chembl_id) %>%
            dplyr::summarise(efo_term=pasteX(efo_term))

        indications_tibble <- dplyr::left_join(indications_tibble, mesh_heading_tibble) %>%
            dplyr::left_join(efo_term_tibble)

        return (indications_tibble)
    }

}

