# funkcia z tutorialu
my_query <- function(wordspace,query) colSums(wordspace[query,])/length(query)

# vypocita chybovost 
# prva matica je kosinusova vzdialenost dokumentov povodneho priestoru
# druha matica je kosinusova vzdialenost dokumentov LSA priestoru
# tretia matica je absolutna chybovost ako rozdiel prvej a druhej matice
lsa_doc_error<-function(doc_list = NULL, ds = docspace, os = my_tdm) {
  library(slam)
  cosine_sim_mat <- function(tdm) crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
  
  if(is.null(doc_list))
    doc_list <- colnames(ds)
  orig_dist_matrix <- cosine_sim_mat(os[,doc_list])
  lsa_dist_matrix <- cosine(ds[,doc_list])
  return(list(dist_dtm = orig_dist_matrix,lsa_dist = lsa_dist_matrix,abs_error = abs(orig_dist_matrix-lsa_dist_matrix)))
}
