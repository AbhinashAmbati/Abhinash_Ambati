# -*- coding: utf-8 -*-
"""ML_Lab1_v3.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1L_-upP_iVnAApI7ZA3n4b-ANH8SacOnI
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.rcParams['figure.figsize'] = (12.0, 9.0)
from IPython.display import display

def znormalize(df2):
  return (df2.iloc[:,0:] - df2.iloc[:,0:].mean()) / df2.iloc[:,0:].std()

# Previewing Datasets
concrete_data = pd.read_csv("concrete.csv")
housing_data = pd.read_csv("housing.csv")
yacht_data = pd.read_csv("yachtData.csv")
display(concrete_data.head())
display(housing_data.head())
display(yacht_data.head())

df2 = pd.read_csv('housing.csv')

df = znormalize(df2)

y = pd.DataFrame(df.iloc[:, -1])
X = df.iloc[:, 0:-1]

def mprint(x, y):
  print("x.head() is: \n{}\n\n".format(x.head()))
  print("y.head() is: \n{}\n\n".format(y.head()))
  print("x.shape: {}".format(x.shape))
  print("y.shape: {}".format(y.shape))

df.head()

mprint(X,y)

"""### Function to split data into train and test sets"""

def train_test_split(data_x, data_y, portion):
  order = np.random.permutation(len(data_x))
  test_x = data_x.iloc[order[:portion]]
  test_y = data_y.iloc[order[:portion]]
  train_x = data_x.iloc[order[portion:]]
  train_y = data_y.iloc[order[portion:]]
  return train_x, train_y, test_x, test_y



def compute_cost(x, y, w):
    y_estimate = np.dot(x, w)
    error = (y - y_estimate)
    #calculating sum squared error
    mse = (1.0/len(x))*np.sum(np.power(error, 2))
    # print(x.shape)
    # print(error.shape)
    gradient = -(1.0/len(x)) * np.dot(x.T, error)
    
    return gradient, mse

"""### Gradient descent algorithm"""

def gradient_descent(X, y, w, learning_rate=0.01, iterations=100, tolerance= 1e-5):
  train_x, train_y, test_x, test_y= train_test_split(X, y, portion=int(len(X)*0.2))
  i=0
  mse_old= None
  while i <= iterations:
      gradient, mse = compute_cost(train_x, train_y, w)
      new_w = w - learning_rate * gradient
    
      # print("w =",new_w)

      # Stopping Condition
      if (mse_old is not None) and (np.sqrt(np.sum(np.abs(mse-mse_old))) < tolerance):
          print("Converged.")
          break
      # print(i)
      # Print error every 100 iterations
      if i % 100 == 0:
          print("Iteration: %d - Error: %.4f" %(i, mse))
      
      i += 1
      w = new_w
      mse_old= mse
  print("w =",w)   
  print("Test Cost =", compute_cost(test_x, test_y, w)[1])



"""### Initializing weights to 0 with shape like X and adding 1's to X for bias term"""

W= np.zeros((14,1))
X_b= pd.DataFrame(np.c_[np.ones((len(X),1)), X])

mprint(X_b, pd.DataFrame(W))

gradient_descent(X_b, y, W, learning_rate=0.4/1000, iterations=50000, tolerance= 0.5/1000)

"""### It works for housing dataset, now generalizing it for all datasets"""

filenames = ["housing.csv", "yachtData.csv", "concrete.csv"]
learning_rates_list=[0.4/1000, 0.1/100, 0.7/1000]
tolerance_list= [0.5/100, 0.1/100, 0.1/1000]

metadata={}
i=0
for fn in filenames:
  metadata[fn]={'lr': learning_rates_list[i],'tr': tolerance_list[i]}
  i+=1
i=0

for fn in filenames:
  df2 = pd.read_csv(fn)
  df = znormalize(df2)
  Y = pd.DataFrame(df.iloc[:, -1])
  X = df.iloc[:, 0:-1]

  print("Data Head()")
  print(df2.head)
  
  X_b= pd.DataFrame(np.c_[np.ones((len(X),1)), X])
  W = np.zeros((X_b.shape[1], 1))

  gradient_descent(X_b, Y, W, learning_rate=metadata[fn]['lr'], iterations=50000, tolerance= metadata[fn]['tr'])



"""# *Experimenting With Different Tolerance & Learning Rates*"""

filenames = ["housing.csv", "housing.csv", "housing.csv"]
learning_rates_list=[0.4/1000, 0.4/1000, 0.4/1000]
tolerance_list= [0.1,0.01,0.05]

metadata={}
i=0
for fn in filenames:
  metadata[fn]={'lr': learning_rates_list[i],'tr': tolerance_list[i]}
  i+=1
i=0

for fn in filenames:
  df2 = pd.read_csv(fn)
  df = znormalize(df2)
  Y = pd.DataFrame(df.iloc[:, -1])
  X = df.iloc[:, 0:-1]
  
  X_b= pd.DataFrame(np.c_[np.ones((len(X),1)), X])
  W = np.zeros((X_b.shape[1], 1))

  gradient_descent(X_b, Y, W, learning_rate=metadata[fn]['lr'], iterations=50000, tolerance= metadata[fn]['tr'])

filenames = ["yachtData.csv", "yachtData.csv", "yachtData.csv"]
learning_rates_list=[0.1/100, 0.1/100, 0.1/100]
tolerance_list= [0.1,0.01,0.05]

metadata={}
i=0
for fn in filenames:
  metadata[fn]={'lr': learning_rates_list[i],'tr': tolerance_list[i]}
  i+=1
i=0

for fn in filenames:
  df2 = pd.read_csv(fn)
  df = znormalize(df2)
  Y = pd.DataFrame(df.iloc[:, -1])
  X = df.iloc[:, 0:-1]
  
  X_b= pd.DataFrame(np.c_[np.ones((len(X),1)), X])
  W = np.zeros((X_b.shape[1], 1))

  gradient_descent(X_b, Y, W, learning_rate=metadata[fn]['lr'], iterations=50000, tolerance= metadata[fn]['tr'])

filenames = ["concrete.csv", "concrete.csv", "concrete.csv"]
learning_rates_list=[0.7/1000, 0.7/1000, 0.7/1000]
tolerance_list= [0.1,0.01,0.05]

metadata={}
i=0
for fn in filenames:
  metadata[fn]={'lr': learning_rates_list[i],'tr': tolerance_list[i]}
  i+=1
i=0

for fn in filenames:
  df2 = pd.read_csv(fn)
  df = znormalize(df2)
  Y = pd.DataFrame(df.iloc[:, -1])
  X = df.iloc[:, 0:-1]
  
  X_b= pd.DataFrame(np.c_[np.ones((len(X),1)), X])
  W = np.zeros((X_b.shape[1], 1))

  gradient_descent(X_b, Y, W, learning_rate=metadata[fn]['lr'], iterations=50000, tolerance= metadata[fn]['tr'])

"""**The given torelance values to experiment are found to be very high and not even one iteration was occurred.**"""