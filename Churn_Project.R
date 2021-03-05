## Churn Data
## IST 301
## Neel Kumtakar, Anthony Sofia, Alice Li, Connor Introna, and Micheal Bifulco
## 12/15/18
## Grp2




		churn<-read.csv(file.choose())  ## we read in the data

		str(churn) ## figure out the structure of the data

		summary(churn)  ##get the summary statistics, i.e min and max of each variable

		View(churn)  ## View the dataset instead of opening it 
		
		##get the number of values for numeric variables or the distrubtion of values for categorical variables
		
      		length(churn$RowNumber)
      		length(churn$CreditScore)
      		table(churn$Geography)
      		table(churn$Gender)
      		length(churn$Age)
      		length(churn$Tenure)
      		length(churn$Balance)
      		length(churn$NumOfProducts)
      		table(churn$HasCrCard)
      		table(churn$IsActiveMember)
      		length(churn$EstimatedSalary)
      		table(churn$Exited)
		
		
		

		## Data cleaning Step
		
		library(dplyr)

		## Dplyr's rename function comes in handy when renaming columns. 
		## We rename the column names to match the data dictionary names and for coding purposes.

						churn= rename(churn, 
						              Cust_ID= CustomerId,
						              Cust_Country= Geography,
						              Cust_Credit_Score= CreditScore,
						              Cust_Gender= Gender,
						              Cust_Age=Age,
						              Cust_Tenure=Tenure,
						              Cust_Balance= Balance,
						              Cust_Products=NumOfProducts,
						              Cust_Credit_Card=HasCrCard,
						              Cust_Activity=IsActiveMember,
						              Cust_Salary=EstimatedSalary,
						              Cust_Status=Exited )
		              
		  			names(churn) ## Verify the names have been changed
  
      	  ## Data Transformation- We remove Row Number, Surname and ID as they
		  ## are variables that do not relate to our target variable or are useful
		  ## in our model. Also because Cust_Status, Cust_Credit_Card, and 
		  ## Cust_activity are categorical variables, we set them as factor variables.

									churn$RowNumber=NULL
									churn$Surname= NULL
									churn$Cust_ID= NULL
									
									churn$Cust_Status <- as.factor(churn$Cust_Status )
									churn$Cust_Credit_Card<-as.factor(churn$Cust_Credit_Card)
									churn$Cust_Activity<-as.factor(churn$Cust_Activity)
									
									churn$Cust_Age <- as.numeric(churn$Cust_Age )
									churn$Cust_Products<-as.numeric(churn$Cust_Products)
									churn$Cust_Tenure<-as.numeric(churn$Cust_Tenure)
									churn$Cust_Credit_Score<-as.numeric(churn$Cust_Credit_Score)
			
			
			
			    	
			    
			    	 ## To get the correltion matrix, you need to filter out the categorical variables. 
			    	 ## Correlation matrix only meant for numeric variables.
			    
			    	library(dplyr)
			    
			    	churn_subset= select(churn, -c(Cust_Country, Cust_Gender, Cust_Status, Cust_Credit_Card, Cust_Activity))
			    
			    	## Setting up the Correlation Matrix

			    	library(corrplot)
					
					CHRN_corr<-cor(churn_subset)
					corrplot(CHRN_corr)
					
					## model the correlation between Balance and Products

					library(ggplot2)
					a<-ggplot(churn_subset, aes(y=Cust_Balance, x=Cust_Products)) + geom_point()
					
							

							## Naive Bayes Procedure
			
					    		set.seed(3)

					    	## generate testing and training data- 70% testing, 30% train 

						    	c <- sample(2, nrow(churn), prob=c(0.70,0.30), replace=TRUE)
							 
						    	churn_Train= churn[c==1,]
						    	churn_Test= churn[c==2,]
				 
						
			         			library(e1071)
			         			churn_naive= naiveBayes(Cust_Status~., data=churn_Train)
			         			churn_Predict=predict(churn_naive, churn_Test)
			         			churn_Predict
		         			
		         			## Generate Naive Baclassification statistics

			         			library(caret)
			         			confusionMatrix(table(churn_Predict, churn_Test$Cust_Status))



          
         					## Decision Tree Procedure
		         			
			         			library(rpart)
			         			library(rpart.plot)
			         			library(e1071)
			         			
			         			dt_churn <- rpart(Cust_Status~.,churn_Train, method= "class")
			         			summary(dt_churn)
			         			rpart.plot(dt_churn)
			         			
			         			churn_predict_dd<-predict(dt_churn,churn_Test, type = "class")
			         			churn_predict_dd
			         			
			         			library(caret)
			         			confusionMatrix(table(churn_predict_dd,churn_Test$Cust_Status))
			         			
			         			
			         			
			         			
			         			
			         			## Regression Procedure # Data cleaning churn$Exited <- as.factor(churn$Exited) churn$HasCrCard <- as.factor(churn$HasCrCard) churn$IsActiveMember <- as.factor(churn$IsActiveMember) colnames(churn) 
			         			
			         			# Create a subset that removes variables rowname, customer_id, and surname # These variables will not be useful in regression model b/c they are identifiers 
			         			
			         			subchurn <- churn[,c("CreditScore", "Geography", "Gender", "Age", "Tenure", "Balance", "NumOfProducts", "HasCrCard", "IsActiveMember", "EstimatedSalary", "Exited")] 
			         			head(subchurn) 
			         			str(subchurn) 
			         			
			         			# Splitting the data 
			         			install.packages("caret") 
			         			library(caret) 
			         			
			         			set.seed(99) 
			         			id <- createDataPartition(subchurn$Exited, p = 0.90, list = F) 
			         			id 
			         			subchurn_train <- subchurn[id,] 
			         			subchurn_test <- subchurn[-id,] 
			         			str(subchurn_train) 
			         			str(subchurn_test) 
			         			
			         			# Logistic Regression model 
			         			logreg <- glm(Exited~., data = subchurn_train, family = "binomial"(link = "logit")) 
			         			summary(logreg) 
			         			
			         			final_logreg <- glm(Exited ~ CreditScore + Gender +Age + Balance + IsActiveMember, data = subchurn_train, family = "binomial"(link = "logit")) 
			         			summary(final_logreg) 
			         			
			         			
			         		                                                                                            
			         			# Coefficients of final logistic regression model 
			         			invlogit <- function(x)    { 1 / (1 + exp(-x)) } invlogit(final_logreg$coefficients) 
			         			
			         			# Use test dataset for prediction
			         			logreg_pred <- predict(final_logreg, newdata = subchurn_test, type = "response") head(logreg_pred) 
			         			
			         			# Confusion Matrix # test different cutoff values to find one with best accuracy # cutoff value of 0.3 
			         			table(Actualvalues = subchurn_test$Exited, Predictedvalues = logreg_pred > 0.3) 
			         			
			         			# cutoff value of 0.4 
			         			table(Actualvalues = subchurn_test$Exited, Predictedvalues = logreg_pred > 0.4) 
			         			
			         			# cutoff value of 0.5 
			         			table(Actualvalues = subchurn_test$Exited, Predictedvalues = logreg_pred > 0.5) 
			         			
			         			# cutoff value of 0.6 
			         			table(Actualvalues = subchurn_test$Exited, Predictedvalues = logreg_pred > 0.6) 
			         			
			         			# cutoff value of 0.7
			         			table(Actualvalues = subchurn_test$Exited, Predictedvalues = logreg_pred > 0.7)
			         			
			         			
			         			
			         			
			         			
			         			
			         			
			         			
			         			
          
         		
			

			
