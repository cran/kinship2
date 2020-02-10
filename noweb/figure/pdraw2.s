#
# The second, more complex test pedigree
#
test2 <- data.frame(id=c(1:13, 21:41),
                    dad=c(0,0, 1,1,1, 0,0, 6,6,6, 0, 11,11,
                          0,0,0,0,0,0, 21,21,21,23,23,25, 28,28,28,28, 
                          32,32,32,32,33),
                    mom=c(0,0, 2,2,2, 0,0, 7,7,7, 0, 5, 9, 0,0,0,0,0,0, 
                          22,22,22,24,24,26, 31,31,31,31, 29,29,29,29,13),
                    sex=c(1,2, 1,2,2, 1,2, 1,2,1, 1, 1,2,
                          1,2,1,2,1,2, 1,1,2,2,2,1, 1,1,2,2, 1,2,1,1,2))
ped2 <- pedigree(test2$id, test2$dad, test2$mom, test2$sex)
ped2a<- ped2
ped2a$hints <- cbind(1:length(test2$id), 0)


itemp <- sort(unique(test2$id))
test3 <- data.frame(id=match(test2$id, itemp),
                    dad=match(test2$dad, itemp, nomatch=0),
                    mom=match(test2$mom, itemp, nomatch=0),
                    sex=test2$sex)
ped3 <- pedigree(test3$id, test3$dad, test3$mom, test3$sex)
ped3a <- ped3
ped3a$hints <- ped2a$hints



