import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from subprocess import check_output
print(check_output(["ls", "../input"]).decode("utf8"))

import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import train_test_split
from sklearn import model_selection
from sklearn import metrics
from sklearn import tree
from sklearn import svm
from sklearn import preprocessing
from sklearn.linear_model import SGDClassifier
from sklearn.naive_bayes import GaussianNB, BernoulliNB, MultinomialNB
from sklearn.cross_validation import cross_val_score
from statistics import mean
import seaborn as sns
import matplotlib.pyplot as plt
import warnings
from sklearn import datasets

#url = "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
#names = ["sepal_length","sepal_width","petal_length","petal_width","Species"]

data = pd.read_csv("D:/D/Hackathon/Iris/Iris.csv")

labels = []
labels += [0 for i in range(50)]
labels += [1 for i in range(50)]
labels += [2 for i in range(50)]

data.describe()

sns.FacetGrid(data, hue="Species", size=5) \
   .map(plt.scatter, "SepalWidthCm", "PetalLengthCm") \
   .add_legend()
   
sns.FacetGrid(data, hue="Species", size=5) \
   .map(plt.scatter, "SepalWidthCm", "SepalLengthCm") \
   .add_legend()
   
sns.FacetGrid(data, hue="Species", size=5) \
   .map(plt.scatter, "PetalLengthCm", "PetalWidthCm") \
   .add_legend()

sns.FacetGrid(data, hue="Species", size=5) \
   .map(plt.scatter, "PetalLengthCm", "SepalWidthCm") \
   .add_legend()

sns.pairplot(data.drop("Id", axis=1), hue="Species", size=3, diag_kind="kde")

# deleting these 2 columns from the dataframe
del data["Species"]
del data["Id"]

# spliting that data for training and testing with 80% data for training and 20% for testing
X_train,X_test,Y_train,Y_test = model_selection.train_test_split(data,labels, random_state = 22, test_size = 0.2)

clfModel1 = LogisticRegression(class_weight='balanced')

scores = cross_val_score(clfModel1,  X_train, Y_train, cv=5)
print("Accuracy for Logistic Regression : %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

clfModel3 = GaussianNB()
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clfModel3,  X_train, Y_train, cv=5)
print("Accuracy for Gaussian NB: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

clfModel4 = BernoulliNB()
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clfModel4,  X_train, Y_train, cv=5)
print("Accuracy for BernoulliNB: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

clfModel5 = tree.DecisionTreeClassifier(criterion='gini')
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clfModel5,  X_train, Y_train, cv=5)
print("Accuracy for Decision Tree : %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

clfModel6 = svm.SVC()
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clfModel6,  X_train, Y_train, cv=5)
print("Accuracy for SVM: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

from sklearn.linear_model import SGDClassifier
clfModel7 = SGDClassifier()
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clfModel7,  X_train, Y_train, cv=5)
print("Accuracy for SGD Classifier : %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)


from sklearn.neighbors import KNeighborsClassifier
# Create KNeighbors classifier object model 
clf8 = KNeighborsClassifier(n_neighbors=6) # default value for n_neighbors is 5
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clf8,  X_train, Y_train, cv=5)
print("Accuracy for K Neighbours : %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

from sklearn.ensemble import RandomForestClassifier
# Create Random Forest object
clf9 = RandomForestClassifier()
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clf9,  X_train, Y_train, cv=5)
print("Accuracy for Random Forest Clasifier: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

from sklearn.ensemble import GradientBoostingClassifier
warnings.filterwarnings("ignore")
clf10 = GradientBoostingClassifier(n_estimators=100, learning_rate=1.0, max_depth=1, random_state=0)
# check the accuracy
# the data is split 5 times
scores = cross_val_score(clf10,  X_train, Y_train, cv=5)
print("Accuracy for Gradient Boosting Classifier: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(scores)

classifierModel = svm.SVC()
classifierModel.fit(X_train,Y_train)
predictions = classifierModel.predict(X_test)

from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix

print(accuracy_score(Y_test,predictions))

# precision, recall, f1-score for setosa is perfect 
# because it is easily seperable from the other 2 species as seen in the plots 
print(classification_report(Y_test,predictions))

conf = confusion_matrix(Y_test,predictions)

label = ["Setosa","Versicolor","Virginica"]
sns.heatmap(conf, annot=True, xticklabels=label, yticklabels=label)