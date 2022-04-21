#' get alternative_forms of molecules
#'
#' ChEMBL includes parent molecules and their salts (approved and investigational)
#' as well as other alternative forms such as hydrates and radioisotopes.
#' These alternative forms are linked to their parent compound through the
#' molecule hierarchy.
#' ref: http://chembl.blogspot.com/2020/09/molecule-hierarchy.html
#' @param ChEMBLIDs a vector of ChEMBL IDs, such as c("CHEMBL4303288", "CHEMBL1380")
#' @param verbose verbose.
#' @return a tibble object with 3 columns, molecule_chembl_id, parent_chembl_id and alternative_forms.
#' @examples
#' ChEMBLIDs <- c("CHEMBL4303288", "CHEMBL1380", "CHEMBL3040746")
#' res_tib <- get_alternative_forms(ChEMBLIDs)
#' @export
get_alternative_forms <- function(ChEMBLIDs, verbose=FALSE) {

    # get the parents ChEMBLIDs of the input and then obtain the alternative forms.
    alternative_tibble <- get_parent_chembl_id(ChEMBLIDs)
    rownames(alternative_tibble) <- alternative_tibble$molecule_chembl_id


    for (ChEMBLID_i in seq(length(ChEMBLIDs)) ) {
        ChEMBLID <- alternative_tibble[ChEMBLID_i, "parent_chembl_id"]
        parent_api <- "https://www.ebi.ac.uk/chembl/api/data/molecule_form.json?parent_chembl_id="
        parent_url <- paste0(parent_api, ChEMBLID)

        if (verbose) {print (parent_url)}
        parent_list <- jsonlite::fromJSON(parent_url)
        if (!is.null(parent_list$molecule_forms$molecule_chembl_id)) {
            alternative_tibble[ChEMBLIDs[ChEMBLID_i],"alternative_forms"] <- pasteX(parent_list$molecule_forms$molecule_chembl_id)
        }

    }

    return(alternative_tibble)
}
