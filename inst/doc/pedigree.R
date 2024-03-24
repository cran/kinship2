## ----echo = FALSE---------------------------------------------------------------------------------
options(width = 100)

## ----load, echo=TRUE------------------------------------------------------------------------------
require(kinship2)

## ----pedList--------------------------------------------------------------------------------------
data(sample.ped)
sample.ped[1:10,]
pedAll <- pedigree(id=sample.ped$id, 
                dadid=sample.ped$father, momid=sample.ped$mother, 
                sex=sample.ped$sex, famid=sample.ped$ped)
print(pedAll)

## ----plot12---------------------------------------------------------------------------------------
ped1basic <- pedAll['1']
ped2basic <- pedAll['2']
print(ped1basic)
print(ped2basic)
plot(ped2basic)
# plot(ped1basic)

## ----datped2--------------------------------------------------------------------------------------
datped2 <- sample.ped[sample.ped$ped %in% 2,]
datped2[datped2$id %in% 203, "sex"] <- 2
datped2 <- datped2[-which(datped2$id %in% 209),]

## ----fixped---------------------------------------------------------------------------------------
tryout <- try({
   ped2 <- with(datped2, pedigree(id, father, mother, sex))
})
fixped2 <- with(datped2, fixParents(id, father, mother, sex))
fixped2
ped2 <- with(fixped2, pedigree(id, dadid, momid, sex))
plot(ped2)

## ----calckinship----------------------------------------------------------------------------------
kin2 <- kinship(ped2basic)
kin2

## ----kinAll---------------------------------------------------------------------------------------
pedAll <- pedigree(id=sample.ped$id, 
                dadid=sample.ped$father, momid=sample.ped$mother, 
                sex=sample.ped$sex, famid=sample.ped$ped)
kinAll <- kinship(pedAll)
kinAll[1:14,1:14]
kinAll[40:43, 40:43]
kinAll[42:46, 42:46]

## ----kintwins-------------------------------------------------------------------------------------
reltwins <- rbind(c(206, 207, 1, 2), c(125, 126, 1, 1))
colnames(reltwins)=c("id1","id2","code","famid")
pedAll <- with(sample.ped, pedigree(id=id, dadid=father, momid=mother,
  sex=sex, famid=ped, relation=reltwins))
kinAll <- kinship(pedAll)
kinAll[21:31,21:31]
kinAll[42:50,42:50]

## ----censor---------------------------------------------------------------------------------------
df2 <- sample.ped[sample.ped$ped==2,]
names(df2)
df2$censor <- c(1,1, rep(0, 12))
ped2 <- pedigree(df2$id, df2$father, df2$mother, 
                 df2$sex, status=df2$censor)

## ----affected-------------------------------------------------------------------------------------
ped2 <- pedigree(df2$id, df2$father, df2$mother, 
                 df2$sex, affected=df2$affected,
                 status=df2$censor)
aff2 <- data.frame(blue=df2$affected, 
                   bald=c(0,0,0,0,1,0,0,0,0,1,1,0,0,1))
ped2 <- pedigree(df2$id, df2$father, df2$mother, 
                 df2$sex, affected=as.matrix(aff2),
                 status=df2$censor)

## ----twins----------------------------------------------------------------------------------------
## create twin relationships
relate2 <- matrix(c(210,211,1,
                   212,213,3), nrow=2, byrow=TRUE)
ped2 <- pedigree(df2$id, df2$father, df2$mother, 
                 df2$sex, affected=as.matrix(aff2),
                 status=df2$censor,
                 relation=relate2)

## ----inbreeding-----------------------------------------------------------------------------------
id <- 195:202
dadid <- c(0,0,0,196,196,0,197,199)
momid <- c(0,0,0,195,195,0,198,200)
sex <- c(2,1,1,2,1,2,1,2)
ped3 <- pedigree(id, dadid, momid, sex)
ped4df <- rbind.data.frame(as.data.frame(ped2)[-c(1,2),1:4], as.data.frame(ped3))
ped4 <- with(ped4df, pedigree(id, dadid, momid, sex))
plot(ped4, cex=.5)

## ----ped2update-----------------------------------------------------------------------------------
id2 <- paste(df2$id, c("John", "Linda", "Jack", "Rachel", "Joe", "Deb", 
                         "Lucy", "Ken", "Barb", "Mike", "Matt", 
                         "Mindy", "Mark", "George"), sep="\n")
plot(ped2, col=ifelse(df2$avail, 2, 1), id=id2, cex=.5)
pedigree.legend(ped2, location="topright", radius=.2) 

## ----plotped1-------------------------------------------------------------------------------------
df1<- sample.ped[sample.ped$ped==1,]
relate1 <- matrix(c(113, 114, 4), nrow=1)
ped1 <- pedigree(df1$id, df1$father, df1$mother, 
       df1$sex, affected=df1$affected, 
                 relation=relate1)
print(ped1)
plot(ped1, col=ifelse(df1$avail==1, "red","black"),cex=.7)

## ----ordering-------------------------------------------------------------------------------------
df1reord <- df1[c(35:41,1:34),]
ped1reord <- pedigree(df1reord$id, df1reord$father, df1reord$mother, 
       df1reord$sex, affected=df1reord$affected, relation=relate1)
plot(ped1reord, col=df1reord$avail+1, cex=.7)

## ----legendplot-----------------------------------------------------------------------------------
legendPlot(ped2, col=ifelse(df2$avail, "red", "black"),
   id=id2, symbolsize=.4,
   affected.label=c("blue eyes", "baldness"),
   col.label=c(`black`="no dna", `red`="dna"))

## ----legendplotindex------------------------------------------------------------------------------
colnames(ped2$affected)
legendPlot(ped2, col=ifelse(df2$avail, 2, 1),
   id=id2, cex=.5, symbolsize=.5,
   col.label=c("no dna", "dna"))

## ----ped2df---------------------------------------------------------------------------------------
dfped2 <- as.data.frame(ped2)
dfped2

## ----subset---------------------------------------------------------------------------------------
ped2.rm210 <- ped2[-10]
data.frame(ped2.rm210)
ped2.rm210$relation
ped2$relation

## ----trim-----------------------------------------------------------------------------------------
ped2.trim210 <- pedigree.trim(210, ped2)
ped2.trim210$id
ped2.trim210$relation
ped2.trim.more <- pedigree.trim(c(212,214), ped2.trim210)
ped2.trim.more$id
ped2.trim.more$relation

## ----shrink1, eval=FALSE--------------------------------------------------------------------------
#  set.seed(200)
#  shrink1.B30 <- pedigree.shrink(ped=ped1,
#                   avail=df1$avail, maxBits=30)
#  print(shrink1.B30)
#  #plot.pedigree.shrink(shrink1.B30, col=shrink1.B30$avail + 1, cex=.6)

## ----shrink2, eval=FALSE--------------------------------------------------------------------------
#  set.seed(10)
#  shrink1.B25 <- pedigree.shrink(ped=ped1, avail=df1$avail,
#                                 maxBits=25)
#  shrink1.B25
#  #plot.pedigree.shrink(shrink1.B25, col=shrink1.B25$avail+1, cex=.6)

## ----unrelateds-----------------------------------------------------------------------------------
df2<- sample.ped[sample.ped$ped==2,]
ped2 <- pedigree(df2$id, df2$father, df2$mother, 
       df2$sex, affected=df2$affected)
set.seed(10)
set1 <- pedigree.unrelated(ped2, avail=df2$avail)
set1
set2 <- pedigree.unrelated(ped2, avail=df2$avail)
set2

## ----unrelVerify----------------------------------------------------------------------------------
df2
kin2[df2$avail==1,df2$avail==1]

