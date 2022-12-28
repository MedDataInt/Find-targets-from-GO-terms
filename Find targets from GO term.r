### Find interesting genes from GO terms
### the aim of this project is to find overlap between the most frequent genes in GO terms and our target genes
## In GO terms spreadsheet, geneID columns contain genes involved in the indicated GO terms, but sepearted by comma
# Read data in word cloud 
inscrpus <-corpus(c(data1a$geneID, data2a$geneID, data3a$geneID))
str(inscrpus)

paras <- inscrpus
paras <- tokens(paras, remove_punct = T, remove_numbers = T)
paras <- tokens_select(paras, selection = "remove", stopwords("english"))
webfile_dfm <- dfm(paras, tolower=FALSE )  # keep the capital letter 
str(webfile_dfm)

# Show the word cloud 
webfile_dfm2 <-dfm_trim(webfile_dfm, min_termfreq = 10)
textplot_wordcloud(webfile_dfm2, labelcolor ="red", labelsize = 3)

# Count gene frequency in the Gene Ontology term 
wordCounts <- textstat_frequency(webfile_dfm) # dataframe
head(wordCounts,50)

str(wordCounts)
barplot(wordCounts$frequency)
str(wordCounts)

# Focus on top 50 gene 
wordCounts50 <- wordCounts[1:50]

# Merge potential target gene with top 50 highest frequency genes in Gene Ontology term 
merged_gene_list <-inner_join(Target, wordCounts50)
str(merged_gene_list)

wordCounts50$feature <-factor(wordCounts50$feature, levels = wordCounts50$feature)

# data visulization 
target <- ggplot(data=wordCounts50, aes(y=feature, x = frequency)) +
    geom_bar(stat ="identity", fill="steelblue", width = 0.2)+
    geom_point(data = merged_gene_list, aes(x=frequency, y=feature), color = "darkred", size = 2)+
    scale_y_discrete(limits = rev(levels(wordCounts50$feature))) + 
    theme(aspect.ratio = 2/1)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=10, face = "bold"),
        axis.title=element_text(size=14,face="bold"))

target 