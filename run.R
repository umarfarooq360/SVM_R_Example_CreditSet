run =  function(percentTest = 1/3 ) {
  
  #import the required libraries
  library(e1071);
  library(rpart);
  
  #Import the dataset 
  dataSet=read.csv("creditset.csv");
  
  #Delete first column, named a in data which just has indices
  #dataSet = subset(dataSet , select= -c(a) );
  
  #Seperate indices for test and training data
  index = 1:nrow(dataSet);  #The whole dataset index
  
  #Sample randomly from the indices to get test and training sets
  testIndex = sample(index, trunc(length(index)* percentTest ));
  testSet = dataSet[testIndex ,  ];
  trainSet = dataSet[ -testIndex , ];
  
  #Creating the SVM. default10yr is the name of column with dataSet type
 
  SVM.Model  = svm(default10yr ~ age + LTI , data =trainSet,scale = TRUE ,cost =1000 ,gamma =1
                  );
  
  #-----------------
  #Optional : Tune the SVM
  #   tobj =  tune.svm( default10yr ~ LTI + age , data = trainSet
  #   ,gamma = 10^(-4:-1) ,cost =10^(1:2) )
  #----------------
  
  #Try printing and plotting the model 
  print(summary(SVM.Model));
  plot(SVM.Model , trainSet  , LTI~age );
  
  #Using the SVM to predict.We are taking away the results in the test set
  Answers =  subset(testSet , select= c(default10yr));
  testSet = subset(testSet ,  select= c(age,LTI)); #creating subset of testset
  SVM.Pred = predict(SVM.Model ,testSet );
  
  
  finalOut =  data.frame(Actual = Answers$default10yr,
                         Prediction = round(as.data.frame(SVM.Pred)$SVM.Pred  , 2) );
  
  print(judgeCloseness(finalOut$Actual , finalOut$Prediction,  0.25 ));
  
  
  return (list(out= finalOut,SVM = SVM.Model) );
}

judgeCloseness =  function(arr1 = NULL ,arr2 = NULL, threshold = 0.5){
  count = 0;
  for (i in 1:length(arr1)){
    if(abs(arr1[i] - arr2[i])< threshold ){
      count = count+1;      
    }
    
  }
  return (count/length(arr1));
}