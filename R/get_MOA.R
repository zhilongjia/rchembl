#' Get Drug Mechanism and Targets based on ChEMBL IDs
#'
#'
#'
#'
#' @param ChEMBLIDs a vector of ChEMBL IDs, such as c("CHEMBL4303288", "CHEMBL1380")
#' @param full FASLE (default) or TRUE. outputs molecule_chembl_id, molecular_mechanism, target or all the related information
#' @param verbose verbose.
#' @return a tibble object with 3 columns, consisting of molecule_chembl_id, molecular_mechanism, target.
#' @import dplyr
#' @import jsonlite
#' @examples
#' ChEMBLIDs <- c("CHEMBL4303288", "CHEMBL1380")
#' res_tib <- get_MOA(ChEMBLIDs)
#' dim(res_tib)
#'
#'
#' @export
#'
get_MOA <- function(ChEMBLIDs, full=FALSE, verbose=FALSE) {

    MOA_list <- list()

    for (ChEMBLID in ChEMBLIDs ) {
        # ChEMBLID <- ChEMBLIDs[1]
        # ChEMBLID <- "CHEMBL1380"
        if (verbose) {print (ChEMBLID)}
        moa_api <- "https://www.ebi.ac.uk/chembl/api/data/mechanism.json?molecule_chembl_id="
        moa_url <- paste0(moa_api, ChEMBLID)
        moa_list <- jsonlite::fromJSON(moa_url)
        moa_tibble <- moa_list$mechanisms
        if (verbose) {print (dim(moa_tibble))}

        # multi-pages
        while( !is.null(moa_list$page_meta$"next") ) {
            print (moa_list$page_meta$total_count)
            moa_list <- fromJSON(paste0("https://www.ebi.ac.uk", moa_list$page_meta$"next") )
            moa_tibble <- dplyr::bind_rows(moa_tibble, moa_list$mechanisms)
        }
        MOA_list[[ChEMBLID]] <- moa_tibble

    }

    MOA_tibble_raw <- dplyr::bind_rows(MOA_list)

    if (full) {
        return(MOA_tibble_raw)
    } else {
        # reformat to tibble
        MOA_tibble <- data.frame(molecule_chembl_id=ChEMBLIDs)

        mechanism_of_action_tibble <-  dplyr::group_by(MOA_tibble_raw, molecule_chembl_id) %>%
            dplyr::summarise(mechanism_of_action=pasteX(mechanism_of_action))
        target_tibble <-  dplyr::group_by(MOA_tibble_raw, molecule_chembl_id) %>%
            dplyr::summarise(target_chembl_id=pasteX(target_chembl_id))

        MOA_tibble <- dplyr::left_join(MOA_tibble, mechanism_of_action_tibble) %>%
            dplyr::left_join(target_tibble)

        return (MOA_tibble)
    }

}

