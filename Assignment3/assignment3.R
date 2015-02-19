### 0. Initialization

## Set working directory
#setwd(paste(getwd(),"data",sep="/"))
#setwd("C:/Users/sronnqvi/Downloads/assignment3/data")

### 1. Text statistics

# Read file
file = "transaction_management_system.txt"
text = Reduce(paste, readLines(file))

# Tokenize and normalize
tokens = unlist(lapply(strsplit(gsub("\\.|,|!|\\?|:|;|\\(|\\)|\\[|\\]|\\-\\-", "", text), " "), tolower))

## 1.1 Term statistics

# Calculate term frequencies
term_freqs = table(tokens)

# Plot histogram of term frequencies
hist(term_freqs, breaks=500, xlab="Term Frequency (TF)", ylab="TF count")

hist(term_freqs, breaks=500, xlab="Term Frequency (TF)", ylab="TF count", xlim=c(0,20))


# Print most frequent terms
term_freqs[order(term_freqs, decreasing=TRUE)][1:30]


## 1.2 N-gram statistics

# Make bigrams
bigrams = unlist(Map(function(i) paste(tokens[i],tokens[i+1]), 2:length(tokens)-1))

# Calculate bigram frequencies
bigram_freqs = table(bigrams)

# Print most frequent bigrams
bigram_freqs[order(bigram_freqs, decreasing=TRUE)][1:30]


## 1.3 Keyword extraction

# Read list of input files in data directory
files = list.files(".", "*.txt")

# Shuffle the alphabetical order of documents
set.seed(1) # Fixing the random seed will give consistent results between runs
files = sample(files)

## Loop through all files, and process the text
## Note: Run the entire for loop at once
for (file in files[1:100]){
	print(file)
	# Load file and split into terms
	text = scan(file, character(0))

	## Text preprocessing

	# Make terms lowercase
	text = unlist(lapply(text, tolower))

	# Remove punctuations (loop while stuff can be removed)
	repeat{
		new_text = unlist(lapply(text, function(x) gsub("\\.|,|!|\\?|:|;|\\(|\\)|\\[|\\]|\\-\\-", "", x)))
		if(Reduce(function(a,b) a && b, text == new_text)) break
		text = new_text
	}

	# Remove ' and spaces, plus everything after
	text = unlist(lapply(text, function(x) sub("(\\'| ).*$", "", x)))

	## End of preprocessing

	# Calculate term frequencies (TF) for document
	TFtable = data.frame(table(text))
	TF = data.frame(t(data.frame(TFtable$Freq, row.names=TFtable$text)))

	# Combine term frequencies for each document into data frame allTF
	if (file == files[1]){
		allTF = TF
	} else {
		# Pad TF with columns
		# terms in allTF and not in TF: add 0 to TF
		for(newterm in setdiff(names(allTF), names(TF))){
			TF[[newterm]] = 0
		}

		# Pad allTF with columns
		# terms in TF and not in allTF: add 0 vector to allTF
		for(newterm in setdiff(names(TF), names(allTF))){
			allTF[[newterm]] = rep(0, nrow(TF))
		}

		# Append TF to allTF
		allTF = rbind(allTF, TF)
	}
} ## End of for-loop


# Caluclate document frequencies (number of documents containing term)
DF = data.frame(
	Map(function(col) 
		Reduce(function(a,b) a+b, 
			Map(function(f) f > 0, col)
		), 
	allTF)
)


# Calculate TF-IDF score matrix
scores = allTF*matrix(ncol=1, data=log(nrow(allTF)/DF))


# Get top keywords for each document
keywords = Map(function(x) unlist(scores[x,order(scores[x,], decreasing=TRUE)][1:20]), 1:5)
names(keywords) = files[1:5]
keywords

## 2. Document clustering
## 2.1 Similarity

# Define cosine normalization function
cnorm = function(v) 1/sqrt(sum(unlist(Map(function(x) x^2, v))))
# Define cosine similarity function
sim = function(v1, v2) sum(array(unlist(v1))*array(unlist(v2)))*(cnorm(array(unlist(v1)))*cnorm(array(unlist(v2))))

# Calculate document similarity matrix
ndocs=30
simmx = matrix(ncol=ndocs, nrow=ndocs)
for(i in 1:ndocs){
      writeLines(paste("Comparing document",i))
      for(j in 1:ndocs){
      	    simmx[i,j] = sim(scores[i,], scores[j,])
	    }
}


## 2.2 Hierarchical clustering

# Calculate distance matrix (inverse of similarity/adjacency matrix)
distances = as.dist(1-simmx)

# Generate labels for documents (50 first characters of title)
labs=Map(function(f){substr(f, 1, 50)}, files[1:ndocs])

# Perform clustering
model = hclust(distances, method="ward")

# Plot dendrogram
plot(model)

# Plot dendrogram with filename labels
plot(model,labels=labs, cex=0.7)

## 2.3 Partitioning
# Partition hierarchy into k number of non-overlapping clusters
rect.hclust(model, k=20, border="blue") 


### Bonus code: Alternative visualization (not needed for assignment)
# Load/install package
if(!require(MASS)){ install.packages("MASS"); library(MASS) }

# Project data onto 2 dimensions using Sammon's mapping
projection = sammon(distances,k=2)$points

# Visualize projection
plot(projection,pch=19)
text(projection,labels=labs,cex=0.7,adj=c(0.5,2))
### 


### 3. Topic Modeling
if(!require(ggplot2)){ install.packages("ggplot2"); library(ggplot2) }
if(!require(reshape2)){ install.packages("reshape2"); library(reshape2) }
if(!require(lda)){ install.packages("lda"); library(lda) }

# Read all files and filter special characters
documents = concatenate.documents(Map(function(f){
  gsub("\\.|,|!|\\?|:|;|\\(|\\)|\\[|\\]|\\-\\-|\"|\'", "", readLines(f))
  }, files))

# Convert corpus for LDA
corpus = lexicalize(documents)

# Number of topics
K = 10

# Train topic model
result = lda.collapsed.gibbs.sampler(corpus$documents, K, corpus$vocab, 25, alpha=0.1, eta=0.1)

# Get the top words for topics
top.words = top.topic.words(result$topics, 8, by.score=TRUE)
top.words

# Get topic proportions
result$topic_sums/sum(result$topic_sums)

# Number of documents to visualize
N = 20

topic.proportions = t(result$document_sums) / colSums(result$document_sums)
topic.proportions = topic.proportions[sample(1:dim(topic.proportions)[1], N),]
topic.proportions[is.na(topic.proportions)] = 1/K
colnames(topic.proportions) = apply(top.words, 2, paste, collapse=" ")
topic.proportions.df = melt(cbind(data.frame(topic.proportions), document=factor(1:N)),
                            variable.name="topic", id.vars = "document")  

# Plot topic distributions for sample documents
qplot(topic, value, fill=document, ylab="proportion", data=topic.proportions.df, geom="bar") +
      opts(axis.text.x = theme_text(angle=90, hjust=1)) + coord_flip() + facet_wrap(~ document, ncol=5)

