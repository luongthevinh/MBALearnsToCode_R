## nhl hockey analysis
source("hockey_start.R")

# [1]  
#  First, a simple gut-check point: the intercept.
#  This is the effect on odds that a goal is home rather than away,
#  regardless of any info about what teams are playing or who is on ice.
#  It's the home ice advantage!  
#  We find that home-ice increases odds you've scored by 8%
exp(coef(nhlreg)[1])
#  Now, lets look at the player effects.
#  The regression finds 646 significant player effects
sum(Baicc!=0)
# Here are the top 10 players
Baicc[order(Baicc, decreasing=TRUE)[1:10]]
# Here are the bottom 10 
Baicc[order(Baicc)[1:10]]
#  Specifically, the model says, e.g., that whenever a goal is scored,
#  Pittsburgh's odds of having scored (rather than scored on) 
#  increase by a 51% if Sidney Crosby is on the ice.  
exp(Baicc["SIDNEY_CROSBY"])
#  And the Blue Jackets (or Kings, pre 2011-12) odds of having scored 
#  drop by around 22% if Jack Johnson is on the ice.
exp(Baicc["JACK_JOHNSON"])
# hockey fans among you may feel free to comment in much more detail.

# [2] Without standardize=FALSE, you'd be multiplying the penalty for each
# coefficient (player effect) by that player's standard deviation in onice.
#
# The players with big SD in onice are guys who play a lot.  
# Players with small SD are those who play little (almost all zeros).  
#
# So weighting penalty by SD in this case is exactly what you don't want: 
# a bigger penalty for people with many minutes on ice, a smaller penalty
# for those who seldom play.  Indeed, running the regression without
# standardize=FALSE leads to a bunch of farm teamers coming up tops.  
nhlreg.std <-  gamlr(x, y, 
	free=1:(ncol(config)+ncol(team)), family="binomial")
Bstd <- coef(nhlreg.std)[colnames(player),]
Bstd[order(Bstd, decreasing=TRUE)[1:10]]

# NOTE: this is an exceptional case! You almost always want standardize=TRUE.

# [3]
## to run cv, just use cv.gamlr instead of gamlr
cv.nhlreg <- cv.gamlr(x, y, 
	free=1:(ncol(config)+ncol(team)),
	family="binomial", verb=TRUE, standardize=FALSE)

## plot them together
par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(cv.nhlreg$gamlr) ## cv.gamlr includes a gamlr object

## log lambdas selected under various criteria
log(nhlreg$lambda[which.min(AICc(nhlreg))])
log(nhlreg$lambda[which.min(AIC(nhlreg))])
log(nhlreg$lambda[which.min(BIC(nhlreg))])
log(cv.nhlreg$lambda.min)
log(cv.nhlreg$lambda.1se)
#  AIC and AICc give exactly the same answer here (n>>df)
#  and both are close to the cv.min answer.
Bcvmin <- coef(cv.nhlreg, select="min")[colnames(player),]
sum(Bcvmin!=0) # around 600
sort(Bcvmin,decreasing=TRUE)[1:10] # similar top 10
# Both AIC and AICc are trying to approximate the OOS deviance (MSE here).
# Thus the lambdas at minimum AIC and AICc values are estimates of the 
# lambda which minimizes OOS error -- the same thing targeted with the cv.min rule.
# Also, in this case, the degrees of  freedom are low enough relative to 'n' 
# that AIC works fine, and gives an answer close to AICc. 
#
# The 1se rule accounts for uncertainty about OOS error, and thus chooses a simpler model.
Bcv1se <- coef(cv.nhlreg)[colnames(player),]
# even though log lambdas are close, df drops by 1/2
sum(Bcv1se!=0) # only around 300
sort(Bcv1se,decreasing=TRUE)[1:10] # top 10 changes a bit
#
#  BIC is way more (overly I think) conservative than all these options.
Bbic <- coef(nhlreg,select=which.min(BIC(nhlreg)))[colnames(player),]
sum(Bbic!=0) # zero!  Nobody is different from average according to BIC
#
# The BIC is trying to find lambda with highest probability of having the minimum OOS error,
# which is subtly different than finding the lambda corresponding to lowest expected OOS error.
# For example, if there is more uncertainty about OOS error at the lambda with min expectation, 
# then it could be that another value with higher expected error but lower uncertainty around this
# value will have a higher probability of being best. 
#
# In this case, the BIC says there is much uncertainty at everything other than the
# null model, so that the null model ends up highest probability of being best.
# 
# as an aside: note that the null model here is not just an intercept, but rather
# includes onice configuration info along with information about the team and season.
# So the BIC is not saying that no players matter, but rather that it cannot confidently
# tell them apart from their team's average level of play in a given season.

## Finally, some plots to compare model selections
ll <- log(nhlreg$lambda) ## the sequence of lambdas
n <- nrow(goal)
par(mfrow=c(1,2))
plot(cv.nhlreg)
plot(ll, AIC(nhlreg)/n, 
	xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=3)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=3)
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=3)
points(ll, BIC(nhlreg)/n, pch=21, bg="green")
points(ll, AICc(nhlreg)/n, pch=21, bg="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))


## plot all the answers along the path
par(mfrow=c(1,1))
plot(nhlreg, col="grey")
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=2)
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=2)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=2)
abline(v=log(cv.nhlreg$lambda.min), col="blue", lty=2)
abline(v=log(cv.nhlreg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
	col=c("black","orange","green","blue","purple"),
	legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

# [4]
## to run player only models, drop 'free' and replace 'x' with 'player'
playeronly <- gamlr(player, y, 
	family="binomial", verb=TRUE, standardize=FALSE)
## Without controls, you select the most complicated model!
plot(playeronly)

## we can run the path to even smaller lambdas, 
## via smaller lambda.min.ratio (lamT/lam1; default is 0.01)
cv.playeronly <- cv.gamlr(player, y, lambda.min.ratio=1e-4,
	family="binomial", verb=TRUE, standardize=FALSE)
## now we select something inside the path
par(mfrow=c(1,2))
plot(cv.playeronly)
plot(cv.playeronly$gamlr)

## but it is still massively complicated
Bplayeronly <- coef(cv.playeronly, s="min")[-1,] 
sum(Bplayeronly!=0) # all but around 160 players matter

## Which is better?
## In OOS, the model with controls has a lower OOS deviance
min(cv.playeronly$cvm) ## cvm is OOS mean 
min(cv.nhlreg$cvm)
## In AICc, again the model with controls wins
min(AICc(cv.playeronly$gamlr))
min(AICc(cv.nhlreg$gamlr))

# [+] 
# This isn't the only way of thinking about it, but consider the situation
# where you have no information beyond the fact that player 'k' is on the ice.
# Then all other covariates (teams, onice, etc) would be zero.  
#
# The player effect translates into a `given a goal was scores, it was
# scored by his team' via a logit link as Pk = exp(Bk)/(1+exp(Bk)).
#
# Thus, in isolation, player k's effect is the number of goals he was on ice
# for, Nk, times Pk - (1-Pk) (i.e., prob(scored) - prob(scoredon)).  
#
# To put it another way, his expected `goals for' in isolation is Pk*Nk.
# Expected goals against (again, in isolation), is Nk(1-Pk).  
#
# So a `partial plus minus' (PPM) is NkPk - Nk(Pk-1) = Nk*(2*Pk-1).

# Here are some carreer PPM numbers
P <- exp(Baicc)/(1+exp(Baicc))
N <- colSums(abs(player)) # abs(onice) is 1 if any goal, 0 otherwise.
PPM <- N*(2*P-1)
sort(PPM, decreasing=TRUE)[1:20] # all studs
# get classic PM and compare
PM <- colSums(player*c(-1,1)[y+1]) # +1 for goal by your team, -1 by other team
names(PM) <- colnames(player)
sort(PM, decreasing=TRUE)[1:20] # all goalies
# plot a few
bigs <- which(abs(PM)>200|abs(PPM)>200)
plot(PM[bigs],PPM[bigs],type="n", xlim=range(PM)*1.05,
	xlab="plus minus", ylab="partial plus minus") # empty plot
text(PM[bigs],PPM[bigs],labels=colnames(player)[bigs]) # plot names
abline(a=0,b=1,col=8) # y=x line
# above the line means PM under-rates your performance
# below the line means you are over-rated (by PM)
