# View first 20 rows
from pandas import read_csv
from pandas import set_option
from matplotlib import pyplot
import numpy

filename = "pima-indians-diabetes.data.csv"
names = ['preg', 'plas', 'pres', 'skin', 'test', 'mass', 'pedi', 'age', 'class']
data = read_csv(filename, names = names)
peek = data.head(20)
print(peek)

# Shape of data
shape = data.shape
print(shape)

# Types
types = data.dtypes
print(types)

#  
set_option('display.width', 100)
set_option('precision', 3)
description = data.describe()
print(description)

# Class distributions
class_counts = data.groupby('class').size()
print(class_counts)

# Correlations
correlations = data.corr(method = 'pearson')
print(correlations)
fig = pyplot.figure()
ax = fig.add_subplot(111)
cax = ax.matshow(correlations, vmin=-1, vmax=1)
fig.colorbar(cax)
ticks = numpy.arange(0,9,1)
ax.set_xticks(ticks)
ax.set_yticks(ticks)
ax.set_xticklabels(names)
ax.set_yticklabels(names)
pyplot.show()

# Skew
skews = data.skew()
print(skews)

# Histograms
data.hist()

# Density plots
data.plot(kind='density', subplots=True, layout=(3,3), sharex=False)
pyplot.show()

# Box and Whisker plots
data.plot(kind='box', subplots=True, layout=(3,3), sharex=False, sharey=False)
pyplot.show()

# Rescale data
from sklearn.preprocessing import MinMaxScaler
from numpy import set_printoptions
dataframe = read_csv(filename, names = names)
array = dataframe.values
X = array[:, 0:8]
Y = array[:,8]

scaler = MinMaxScaler(feature_range = (0,1))
rescaledX = scaler.fit_transform(X)
set_printoptions(precision=3)
print(rescaledX[0:5,:])
print(rescaledX)

# Standardize data (0 mean, 1 stdev)
from sklearn.preprocessing import StandardScaler
# separate array into input and output components
scaler = StandardScaler().fit(X)
rescaledX = scaler.transform(X)
# summarize transformed data
set_printoptions(precision=3)
print(rescaledX[0:5,:])

# Normalize data (length of 1)
from sklearn.preprocessing import Normalizer
scaler = Normalizer().fit(X)
normalizedX = scaler.transform(X)
# summarize transformed data
set_printoptions(precision=3)
print(normalizedX[0:5,:])

# binarization
from sklearn.preprocessing import Binarizer
binarizer = Binarizer(threshold=0.0).fit(X)
binaryX = binarizer.transform(X)
# summarize transformed data
set_printoptions(precision=3)
print(binaryX[0:5,:])























