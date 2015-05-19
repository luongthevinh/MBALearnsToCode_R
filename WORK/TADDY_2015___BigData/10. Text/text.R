#### Info retrieval

## the tm library (and related plugins) is R's ecosystem for text mining.
## for an intro see http://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
library(tm) 

## the way file input works with tm is you create a reader function,
## depending on document type.  Each of the reader functions
## have arguments elem, language, id (see ?readPlain,?readPDF,etc)
## I wrap another function around them to specify these arguments.

## for example, a reader to input plain text files 
## (Note: there are many other ways to do this)
readerPlain <- function(fname){
				readPlain()(elem=list(content=readLines(fname)), 
							id=fname, language='en') }
## test it on this script
## (the file name will change depending on where you store stuff).
rcode <- readerPlain("text.R")
rcode
## *** Reading PDFs ***

## from the tm docs: "Note that this PDF reader needs the 
## tool pdftotext installed and accessible on your system,
## available as command line utility in the Poppler PDF
## rendering library (see http://poppler.freedesktop.org/)."
## this appears to be the default on mac; may not work on windows

## we'll create a 'reader' function to interpret pdfs, 
## using tm's readPDF (see help(readPDF) examples)

readerPDF <- function(fname){
		txt <- readPDF(PdftotextOptions="-layout -enc UTF-8")(elem=list(uri=fname), 
															id=fname, language='en')
		Encoding(txt) <- "UTF-8" # text encoding can be very complicated and annoying...
		return(txt)
	}

## for the following to work, in your working directory
## you'll need to point to wherever you've stored the lectures
txt <- readerPDF("../../slides/01Data.pdf") ## test it
txt[1:6] # the cover slide

## apply to all the lectures
files <- Sys.glob("../../slides/*.pdf") 
# Sys.glob just expands file names from 'wildcards'
## takes time!  this would be easy to do 
## distributed via clusterApply or MapReduce
notes <- sapply(files, readerPDF) 

## unnecessary, but just some examples of string manipulation
names(notes)
substring(names(notes),first=14)
sub('.pdf', '', names(notes))
## putting it together to clean our slide names
names(notes) = sub('.pdf', '', substring(names(notes),first=14))
names(notes)

## once you have a bunch of docs in a vector, you 
## create a text mining 'corpus' with: 
docs <- Corpus(VectorSource(notes))
## you can then do some cleaning here
## tm_map just maps some function to every document in the corpus
docs <- tm_map(docs, tolower) ## make everything lowercase
docs <- tm_map(docs, removeNumbers) ## remove numbers
docs <- tm_map(docs, removePunctuation) ## remove punctuation
## remove stopword.  be careful with this: one's stopwords are anothers keywords.
docs <- tm_map(docs, removeWords, stopwords("SMART"))
# you could also do stemming; I don't bother here.
docs <- tm_map(docs, stripWhitespace) ## remove excess white-space

## create a doc-term-matrix
dtm <- DocumentTermMatrix(docs)
dtm # 11 documents, > 4K terms
## These are special sparse matrices.  
class(dtm)
## You can inspect them:
inspect(dtm[1:5,1:8])
## find words with greater than a min count
findFreqTerms(dtm,100)
## or grab words whose count correlates with given words
findAssocs(dtm, "lasso", .9) 

## Finally, drop those terms that only occur in one or two lectures
## This is a common step: you the noise of rare terms to overwhelm things,
##					and there is nothing to learn if a term occured once.
## Below removes those terms that have count 0 in >75% of docs.  
## this is way more harsh than you'd usually do (but we only have 11 docs here)
## .75*11 is 8.25, so this will remove those with zeros in 9+ docs.
## ie, it removes anything that doesn't occur in at least 3 docs
dtm <- removeSparseTerms(dtm, 0.75)
dtm # now near 700 terms


## consider of PCA on term frequencies.
## note that converting to a dense matrix would be infeasible for big corpora
## see the 'irlba' package for PCA on the sparse Matrices we've used with glmnet.
X <- as.matrix(dtm)
F <- X/rowSums(X) ## divide by row (doc totals)
classpca <- prcomp(F, scale=TRUE)
plot(classpca) 

## look at the big rotations (it does a pretty good job!)
classpca$rotation[order(abs(classpca$rotation[,1]),decreasing=TRUE),1][1:10]
classpca$rotation[order(abs(classpca$rotation[,2]),decreasing=TRUE),2][1:10]

## Plot the first two PCs..
plot(classpca$x[,1:2], col=0, xlab="PCA 1 direction", ylab="PCA 2 direction", bty="n")
text(x=classpca$x[,1], y=classpca$x[,2], labels=rownames(dtm),cex=3)


## **** a quick topic-modelling example **** ##
library(maptpx) ## you can give topics a few K, and it chooses the best by BIC
tpc <- topics(dtm, K=2:10) # log(BF) is basically -BIC
rownames(tpc$omega) ## remind ourselve what the documents are

## summary prints terms by 'lift': p(term|topic)/p(term)
summary(tpc, 10) #10 is number of top terms to print

## the topic-term probabilities are called 'theta', and each column is a topic
## we can use these to rank terms by probability within topics
rownames(tpc$theta)[order(tpc$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpc$theta)[order(tpc$theta[,2], decreasing=TRUE)[1:10]]

## plot the lectures another way (do them in order)
par(srt=-30, xpd=NA) ## rotate stings, and allow words outside plot
plot(tpc$omega[,1], type="l", col=8, xlab="", xlim=c(0.5,12),
	xaxt="n", ylab="topic 1 weight", bty="n")
text(x=1:nrow(tpc$omega), y=tpc$omega[,1], labels=rownames(dtm),cex=3)


## Back to our we8there.com reviews

library(textir)
data(we8there)

## Multinomial text Regression 

## cl=NULL instead implies a serial run. 
cl <- makeCluster(detectCores())
## small nlambda for a fast example
fits <- mnlm(cl, we8thereRatings, 
			we8thereCounts, bins=5,nlambda=10)
# stopCluster(cl)

## plot fits for a few individual terms
terms <- c("first date","chicken wing",
			"ate here", "good food",
			"food fabul","terribl servic")
par(mfrow=c(3,2))
for(j in terms)
{ 	plot(fits[[j]]); mtext(j,font=2,line=2) }
 
## extract coefficients
B <- coef(fits)
mean(B[-1,]==0) # sparsity in loadings
## some big loadings on `overall'
B[2,order(B[2,])[1:10]]
B[2,order(-B[2,])[1:10]]

## do MNIR projection onto factors
z <- srproj(B,we8thereCounts) 

## fit a fwd model to the factors
summary(fwd <- lm(we8thereRatings$Overall ~ z)) 

## truncate the fwd predictions to our known range
fwd$fitted[fwd$fitted<1] <- 1
fwd$fitted[fwd$fitted>5] <- 5
## plot the fitted rating by true rating
par(mfrow=c(1,1))
plot(fwd$fitted ~ factor(we8thereRatings$Overall), 
	varwidth=TRUE, col="lightslategrey")










