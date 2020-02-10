#
# A simple pedigree to illustrate autohint's code
#
test3 <- data.frame(id=1:14, 
                    sex=c('m', 'f')[c(2, 1, 1, 1, 1, 1,2,2,1, 2, 1, 2, 2, 1)],
                    father=c(0,6,2,2,2,0,0,11,0,9,6,0,6,11),
                    mother=c(0,7,1,1,1,0,0,12,0,1,7,0,7,12),
                    affected=c(0,1,1,0,0,0,0,0,0,0,1,0,0,0))
ped3 <- with(test3, pedigree(id, father, mother, sex))
temp3 <- ped3
temp3$hints <- list(order=1:14)


test4 <- data.frame(id=1:17, 
                    sex=c('m', 'f')[c(2,1,1,2,2,1,2,1,2,2,1,2,2,1,1,2,2)],
                    father=c(0,8,6,6,0,0,0,0,11,0,0,6,0,0,0,11,14),
                    mother=c(0,9,5,5,0,0,0,0,10,0,0,7,0,0, 0,12,5))
ped4 <- with(test4, pedigree(id, father, mother, sex))
ped4$hints <- list(order=c(1:17))


ped3a <-ped3[1:12]
ped3a$hints <- list(order=1:12)

pdf("autohint3.pdf", width=10, height=8)
par(mfrow=c(1,2))
plot(ped4, symbolsize=.8, mar=c(4.1, 1, 4.1, 3))

plot(temp3, symbolsize=.8, mar=c(4.1, 3, 4.1, 1))
dev.off()
