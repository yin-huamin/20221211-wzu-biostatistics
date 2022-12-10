rna<-fread("rna_tissue_consensus.tsv")

#install.packages("wordcloud")
library(wordcloud)

data <- data.frame(unique(rna$Tissue),1)

wordcloud(words = unique(rna$Tissue),freq = rep(1,54),max.words = 54,random.order=F,min.freq = 1,scale = c(1,1))
ggsave("./wordcloud_tissue.png",dpi = 300)