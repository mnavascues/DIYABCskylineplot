library(adegenet)

# read genepop file and output Beast and VarEff format files

inputfile    <- "data/Requin.gen"
repeat_motif <- 4
repeat_motif <- c(2,2,2,2,2,2,4,2,2,2,2,2,2,2) #requin
repeat_motif <- c(2,4,4,4,2,2,2,2,2,2) #leatherback
output       <- "Leatherback50"
subsample    <- T

inputfile    <- "data/Scenario26.gen" # 8, 20, 26
repeat_motif <- 1
output       <- "Scenario26"

# read input, based on read.genepop funcion from adegenet (1 population only!)
txt       <- scan(inputfile, sep = "\n", what = "character", quiet = TRUE)
txt       <- txt[-1]
txt       <- gsub("\t", " ", txt)
nloc      <- min(grep("POP", toupper(txt))) - 1
loc.names <- txt[seq_len(nloc)]
txt       <- txt[-seq_len(nloc)]
txt       <- txt[-1]
temp      <- sapply(1:length(txt), function(i) strsplit(txt[i],","))
ind.names <- sapply(temp, function(e) e[1])
ind.names <- .rmspaces(ind.names)
vec.genot <- sapply(temp, function(e) e[2])
vec.genot <- .rmspaces(vec.genot)
rm(temp)
X         <- matrix( unlist(strsplit(vec.genot, "[[:space:]]+")), 
                     ncol = nloc,
                     byrow = TRUE)
if (any(duplicated(ind.names))) {
  rownames(X) <- .genlab("", nrow(X))
}else{
  rownames(X) <- ind.names
}
colnames(X) <- loc.names

microsat_data <- rbind(substr(X,1,3) , substr(X,4,6))
rm(X,vec.genot,txt)

microsat_data[which(microsat_data=="000")] <- NA

microsat_data <- matrix(as.integer(microsat_data),
                        ncol = nloc)
if(repeat_motif!=1){
  for (i in seq_len(nrow(microsat_data))){
    microsat_data[i,] <- round(microsat_data[i,]/repeat_motif) 
  }
}
max(microsat_data,na.rm=T)
min(microsat_data,na.rm=T)


if(subsample){
  sample_size<-50
  second_copy <- sort(sample(1:(nrow(microsat_data)/2), 50,replace=F))*2
  first_copy <- second_copy-1
  random_sample <- sort(c(first_copy,second_copy))
  microsat_data <- microsat_data[random_sample,]
}



# write 4 VarEff
library(plyr)   # for function count() that helps to explore allele size distribution
for (locus in seq_along(loc.names)){
  if (locus==1) append<-F
  locus_data <- count(as.data.frame(microsat_data), locus)
  alleles    <- seq(from = min(locus_data[,1], na.rm=T),
                    to   = max(locus_data[,1], na.rm=T),
                    by   = 1)
  count_of_alleles <- array(NA,length(alleles))
  for (al in seq_along(alleles)){
    if (length(which(locus_data[,1]==alleles[al]))==1){
      count_of_alleles[al] <- locus_data[which(locus_data[,1]==alleles[al]),2]
    }else{
      count_of_alleles[al] <- 0
    }
  }
  write(length(alleles),
        file     = paste0(output,".VarEff"),
        ncolumns = 1,
        append   = append)
  append <- T
  write(count_of_alleles,
        file     = paste0(output,".VarEff"),
        ncolumns = length(count_of_alleles),
        append   = T)
}

# write 4 Beauti 
write("#microsat",
      file     = paste0(output,".Beauti"),
      ncolumns = 1,
      append   = F)
write(paste("#name",output),
      file     = paste0(output,".Beauti"),
      ncolumns = 1,
      append   = T)
paste0()
write(c("id",paste0("locus",1:nloc)),
      file     = paste0(output,".Beauti"),
      ncolumns = nloc+1,
      append   = T)

write.table(microsat_data,
            file     = paste0(output,".Beauti"),
            col.names = F,
            append   = T)

