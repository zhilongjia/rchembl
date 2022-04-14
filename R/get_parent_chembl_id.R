#' get parent chembl ID
#'
#' ChEMBL includes parent molecules and their salts (approved and investigational)
#' as well as other alternative forms such as hydrates and radioisotopes.
#' These alternative forms are linked to their parent compound through the
#' molecule hierarchy.
#' ref: http://chembl.blogspot.com/2020/09/molecule-hierarchy.html
#' @param ChEMBLIDs a vector of ChEMBL IDs, such as c("CHEMBL4303288", "CHEMBL1380")
#' @return a tibble object with 2 columns, molecule_chembl_id and parent_chembl_id
#' @examples
#' ChEMBLIDs <- c("CHEMBL4303288", "CHEMBL1380")
#' parent_CHEMBLID_tib <- get_parent_chembl_id(ChEMBLIDs)
#' dim(parent_CHEMBLID_tib)
#' @export
get_parent_chembl_id <- function(ChEMBLIDs) {

    parent_tibble <- data.frame(molecule_chembl_id=ChEMBLIDs)

    for (ChEMBLID_i in seq(length(ChEMBLIDs)) ) {
        ChEMBLID <- ChEMBLIDs[ChEMBLID_i]
        parent_api <- "https://www.ebi.ac.uk/chembl/api/data/molecule_form.json?molecule_chembl_id="
        parent_url <- paste0(parent_api, ChEMBLID)
        parent_list <- jsonlite::fromJSON(parent_url)
        parent_tibble[ChEMBLID_i,"parent_chembl_id"] <- parent_list$molecule_forms$parent_chembl_id
    }

    return(parent_tibble)
}
