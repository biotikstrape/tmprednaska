
# d1:  Romeo and Juliet.
# d2:  Juliet: O happy dagger!
# d3:  Romeo died by dagger.
# d4:  “Live free or die”, that’s the New-Hampshire’s motto.
# d5:  Did you know, New-Hampshire is in New-England.
library(lsa)
source('/lsa/my_lsa_functions.R')
source('/lsa/my_lsa_visual.R')

my_tdm<-matrix(c(1,1,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,1),ncol = 5)
colnames(my_tdm) <- c("d1","d2","d3","d4","d5")
rownames(my_tdm) <- c("romeo","juliet","happy","dagger","live","die","free","new-hampshire")
my_tdm<-as.TermDocumentMatrix(my_tdm,weighting = weightTf)
my_space <- lsa(my_tdm)

# my_tdm_w <- lw_bintf(my_tdm) * gw_idf(my_tdm)
# my_space <- lsa(my_tdm_w)

query(("romeo juliet"),rownames(my_space))


which(rownames(my_space$tk)=="dagger")
my_space$tk["dagger",]

sma <- my_space$tk
ut <- t(my_space$dk)
sigma <- diag(my_space$sk) # treba stvorcovu maticu spravit
docspace <- sigma %*% ut
wordspace <- sma %*% sigma

qr <- my_query(wordspace, c("romeo","die","new-hampshire"))

plot_euclidean_lsa_space(word_space = wordspace, doc_space = docspace, query_coords = qr,
                         doc_labels = T, word_labels = T)

wholespace <- as.textmatrix(my_space)
associate(wholespace, "romeo")
# segments(wordspace["die",][1],wordspace["die",][2],wordspace["dagger",][1],wordspace["dagger",][2])
#
# cosine(wholespace[,"d1"],wholespace[,"d2"]) je to isté ako  cosine(docspace[,"d1"],docspace[,"d2"])
#

mat_list<-lsa_doc_error(doc_list = NULL, ds = docspace, os = my_tdm)
avg_err <- sum(mat_list$abs_error)/(nrow(mat_list$abs_error)^2-nrow(mat_list$abs_error))
# pouzitie
lsa_doc_error(c("d1","d4","d5"))

# angle between two documents in degree
degree_angle <- function(v1,v2,ds=docspace) acos(cosine(ds[,v1],ds[,v2]))*180/pi


draw_cossim_lines(c("d5"))

draw_circle(T)
add_cosine_space_points(docspace,2,text_labels = T,pl_fcn = points)
