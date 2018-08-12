# -*- coding: utf-8 -*-
"""
Created on Mon Jan 22 19:25:57 2018

@author: Mrinmoy
"""
import pandas as pd
import numpy as np

data = pd.read_csv(r"E:\Studies\Data_Analytics_Courses/Jigsaw_Courses\Machine_Learning\NeuralNetwork\T3\Files\mnist_train.csv", header = None)
data.shape

print(data.head())

# Divide the dataset into training and testing data
numTrain = int(0.7*data.shape[0])
print (numTrain)

# Fix a seed for reproducibility 
np.random.seed(123)

# Randomly shuffle the original dataset
np.random.shuffle(data.values)

trainData = data[: numTrain]
testData = data[numTrain:]

print(trainData.shape)
print(testData.shape)


# Plot a sample image : to do that we have to convert each row of the data into 28x28 matrix
sample_row = trainData.iloc[0,1:]
np.shape(sample_row)

# convert this sample row into a 28x28 matrix
imageData = sample_row.reshape(28, 28)
print(sample_row.shape)

# Ploting the image
from matplotlib.pyplot import imshow
%pylab inline
plot()
title("label: " + str(trainData.iloc[0,0]))
imshow(imageData, cmap = 'Greys_r', interpolation = 'None')

# Let us plot some more images
figure(figsize = (10,10))
subplotArr = range(9)
for eachline in subplotArr :
    subplot(3,3, eachline+1)
    subplots_adjust(hspace = 0.5)
    title("label: " + str(trainData.iloc[eachline,0]))
    imshow(trainData.iloc[eachline, 1:].values.reshape(28,28), cmap = 'Greys_r', interpolation = 'None')

## Feature Reduction or dimensionality Technique:

## From a statistical point of view, if the data array is constant, it has 0 variance.
## Features that do not vary over the data give no information. Moreover, they can 
## hamper the performance of some classifiers pretty seriously.

# Remove the 0 variance features:

trainX = trainData.iloc[:,1:]

# Compute variance on each column:
variances = trainX.apply(np.var,axis = 0)

# proportion of columns that are constant
print(np.mean(variances==0))

# Columns with nonzero variance
print(trainData.columns[1:][variances > 0])

variances.hist()

variances.describe()

# --------------------------Modeling the Neural network:-----------------------

from keras.models import Sequential
from keras.layers import Dense, Activation

trainX = trainData.iloc[:,1:].values
type(trainX)

trainY = trainData.iloc[:,0].values

## One hot encoding of target variable:
from keras.utils import np_utils
encoded_Y = np_utils.to_categorical(trainY)

encoded_Y

# Initialize an empty model
model = Sequential()
inputDim = trainX.shape[1]

# add the first hidden layer
model.add(Dense(output_dim = 100,
                input_dim = inputDim,
                init = 'uniform',
                bias = True,
                activation = 'sigmoid'))

# add the output layer
model.add(Dense(output_dim = 10,
                init = 'uniform',
                bias = True,
                activation = 'sigmoid'))


model.summary()


## Compile the model:
model.compile(loss = 'categorical_crossentropy',
              optimizer = 'adam',
              metrics = ['accuracy'])

# Fit the model:
model.fit(trainX, encoded_Y, nb_epoch = 100, batch_size = 10)

prdictionsTestY = model.predict_classes(testData.iloc[:,1:].values)
np.mean(prdictionsTestY == testData.iloc[:,0])

prdictionsTestY
mistakes = [(i,j,k,l) for i,j,k,l in zip(testData.index, prdictionsTestY, testData.iloc[:,0], prdictionsTestY != testData.iloc[:,0])]



imshow(testData.loc[42043,1: ].values.reshape(28,28), cmap = 'Greys_r', interpolation = 'None')
print("predicted label for this image: ")
print(model.predict_classes(testData.loc[42043:42043,1:].values))



