#
# A simple pedigree to illustrate autohint's code
#
test1 <- data.frame(id=1:11, sex=c('m', 'f')[c(1,2,1,2,1, 1,2, 2,1,2, 1)],
                    father=c(0,0,1,1,1, 0,0,6,6,6, 9),
                    mother=c(0,0,2,2,2, 0,0,7,7,7, 4))
ped1 <-pedigree(test1$id, test1$father, test1$mother, test1$sex)
temp2 <- ped1
temp2$hints <- cbind(1:11, 0)

pdf.graph("autohint1.pdf", width=10, height=8)
par(mfrow=c(1,2))
plot(temp, symbolsize=.8, mar=c(4.1, 1, 4.1, 3))

plot(temp2, symbolsize=.8, mar=c(4.1, 3, 4.1, 1))
dev.off()
