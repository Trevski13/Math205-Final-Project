
# x<-rnorm(100)
# # interquartile detection
# # if n < 15, normal and now outliers
# pValue <- shapiro.test(x)$p.value
# if(pValue > 0.05)
	# #t-test
	# iqr <- IQR(x)
	# quartiles <- quantile(x)
	# Q1 <- quartiles[2]
	# Q3 <- quartiles[4]
	# minAccept <- Q1-(1.5*iqr)
	# maxAccept <- Q3+(1.5*iqr)
	# #Q3+(1.5*iqr)
	# #Q1-(1.5*iqr)
	# for(i in 1:length(x)){
		# if(x[i] < minAccept || x[i] > maxAccept) {
			# print('Fail')
			# break
		# } else {
			# print('Pass')
		# }
		# #print(x[i] < minAccept || x[i] > maxAccept)
	# }
	# print(pValue)
	# print(iqr)
	# print(Q1)
	# print(Q3)
	
# } else {
 # #sign test
 
# }
# # if n < 40, no worse than mild skewness, no outliers [no need]

# # if n >= 40, allow strong skewness, no outliers [no need]


normality <- function(x){
	pValue <- shapiro.test(x)$p.value
	if(pValue > 0.05) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

noOutliers <- function(x){
	iqr <- IQR(x)
	quartiles <- quantile(x)
	Q1 <- quartiles[2]
	Q3 <- quartiles[4]
	minAccept <- Q1-(1.5*iqr)
	maxAccept <- Q3+(1.5*iqr)
	#Q3+(1.5*iqr)
	#Q1-(1.5*iqr)
	for(i in 1:length(x)){
		if(x[i] < minAccept || x[i] > maxAccept) {
			return(FALSE)
		}
	}
	return(TRUE)
}

independent <- function(x) {
	if(x[2]=="I"){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

equalVariances <- function() { #correctness?
	labx<-c(1:length_x)/c(1:length_x)
	laby<-c(1:length_y)/c(1:length_y)*2
	labxy<-c(labx,laby)
	pValue <- bartlett.test(c(array_x,array_y),labxy)$p.value
	if (pValue > 0.05){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

sufficentSuccessFailure <- function(x,neededSuccesses,neededFailure){
	Successes <- 0
	Failures <- 0
	for(i in 1:length(x)){
		if(x[i]==0){
			Failures <- Failures + 1
		} else (
			Successes <- Successes + 1
		)
		if (Successes >= neededSuccesses && Failures >= neededFailure){
			return(TRUE)
		}
	}
	return(FALSE)
}

countSuccesses <- function (x) {
	Successes <- 0
	for(i in 1:length(x)){
		if(x[i]==0){
			#merp
		} else (
			Successes <- Successes + 1
		)
	}
	return(Successes)
}

isCenter <- function(x){
	if(x[1]=="C"){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

isSpread <- function(x){
	if(x[1]=="S"){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

isCount <- function(x){
	if(x[1]=="K"){
		return(TRUE)
	} else {
		return(FALSE)
	}
}



#main
matrix<-read.csv(file=file.choose())
array_x<-matrix[,1]
array_y<-matrix[,2]
array_detail<-as.character(matrix[,3])
array_x<-array_x[!is.na(array_x)]
array_y<-array_y[!is.na(array_y)]
array_detail<-array_detail[!is.na(array_detail)]
length_x<-length(array_x)
length_y<-length(array_y)
length_detail<-length(array_detail)
#matrix # return value
cat('Analysing File...\n')
cat('\n')
#paired t #Center?
cat('Paired t-Test\n')
if(isCenter(array_detail)){
	if((length_x + length_y) < 15){
		if(normality(array_x) && normality(array_y)){
			if(noOutliers(array_x) && noOutliers(array_y)){
				print(t.test(array_x,array_y,PAIRED=TRUE))
			} else {
				cat('Failed Due to Outliers\n')
			}
		} else {
			cat('Failed Due to Lack of Normality\n')
		}
	} else {
		if (noOutliers(array_x) &&	noOutliers(array_y)) {
			print(t.test(array_x,array_y,PAIRED=TRUE))
		} else (
			cat('Failed Due to Outliers\n')
		)
	}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Center)\n')
}

cat('\n')
#sign #count
cat('Sign Test\n')
if(isCount(array_detail)){
		if(!normality(array_x) || !normality(array_y)){
			if(noOutliers(array_x) && noOutliers(array_y)){
				# do sign test
				print(binom.test(countSuccesses(array_x)+countSuccesses(array_y),length_x+length_y))
				# binom.test(successes, count)
			} else {
				cat('Failed Due to Outliers\n')
			}
		} else {
			cat('Failed Due to Normality\n')
		}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Count)\n')
}

cat('\n')
#Two-Sample t (non pooled) #center
cat('Two-Sample t-Test (Non-Pooled)\n')
if(isCenter(array_detail)){
	if(independent(array_detail)){
		if(normality(array_x) && normality(array_y)){
			if(noOutliers(array_x) && noOutliers(array_y)){
				print(t.test(array_x,array_y))
			} else {
				cat('Failed Due to Outliers\n')
			}
		} else {
			cat('Failed Due to Lack of Normality\n')
		}
	} else {
		cat('Failed Due to Lack of Independence\n')
	}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Center)\n')
}

cat('\n')
#Two-Sample t (pooled) #center
cat('Two-Sample t-Test (Pooled)\n')
if(isCenter(array_detail)){
	if(independent(array_detail)){
		if(normality(array_x) && normality(array_y)){
			if(noOutliers(array_x) && noOutliers(array_y)){
				if(	equalVariances() ){ #variances to be unknown but equal
					#two-sample t (pooled)
					print(t.test(array_x,array_y,var.equal=TRUE)) #correct?
				} else {
					cat('Failed Due to Unequal Variances\n')
				}
			} else {
				cat('Failed Due to Outliers\n')
			}
		} else {
			cat('Failed Due to Lack of Normality\n')
		}
	} else {
		cat('Failed Due to Lack of Independence\n')
	}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Center)\n')
}

cat('\n')
#F-test #spread
cat('F Test\n')
if(isSpread(array_detail)){ #maybe?
	if(independent(array_detail)){
		if(normality(array_x) && normality(array_y)){
			if(noOutliers(array_x) && noOutliers(array_y)){
				print(var.test(array_x, array_y))
			} else {
				cat('Failed Due to Outliers\n')
			}
		} else {
			cat('Failed Due to Lack of Normality\n')
		}
	} else {
		cat('Failed Due to Lack of Independence\n')
	}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Spread)\n')
}

cat('\n')
#two proportion test
cat('Two Proportion Test\n')
if(isCount(array_detail)){
	if(sufficentSuccessFailure(array_x,5,5) && sufficentSuccessFailure(array_y,5,5)){
		print(prop.test(c(countSuccesses(array_x),countSuccesses(array_y)),c(length_x,length_y)))
	} else {
		cat('Failed Due to Insufficent Successes or Failure Count\n')
	}
} else {
	cat('Failed Due to Incorrect Data Type (Must Be Count)\n')
}
cat('\n')
cat('Done\n')
#end
#"C:\Users\tbuttrey\Dropbox\Documents\School\MATH 205\Project\example stuff.r"