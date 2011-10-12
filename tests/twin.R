library(kinship2)
#
# Test some twins data from Curtis Oswold
#
twindat <- c(1,3,4,2,
             2,0,0,1,
             3,8,7,1,
             4,6,5,2,
             5,0,0,2,
             6,0,0,1,
             7,0,0,2,
             8,0,0,1,
             101,3,4,1,
             102,3,4,2,
             103,3,4,2,
             104,3,4,2,
             105,3,4,2,
             106,3,4,2,
             201,2,1,1,
             202,2,1,1,
             203,2,1,1,
             204,2,1,1)
twindat <- matrix(twindat, ncol=4, byrow=T)
dimnames(twindat) <- list(NULL, c('id', 'dadid', 'momid', 'sex'))
twindat <- data.frame(twindat)

tped <- with(twindat, pedigree(id, dadid, momid, sex,
                 relation=data.frame(id1=104, id2=105, code=2)))
