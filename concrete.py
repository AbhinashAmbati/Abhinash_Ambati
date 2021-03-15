# -*- coding: utf-8 -*-
"""
Created on Sun Oct 11 01:46:52 2020

@author: z
"""

import numpy as np
import pandas as pd



data = pd.read_csv('concreteData-1.csv')
data['Intercept'] = 1

TrainingD = data.iloc[0:710,:]
TestD = data.iloc[710:1030,:]

#TrainingData = pd.DataFrame(data=TrainingData)

def z_score(df):
    df_std = df.copy()
    for column in df_std.columns:
        df_std[column] = (df_std[column] - df_std[column].mean()) / df_std[column].std()        
    return df_std
TrainingData = z_score(TrainingD)

X = TrainingData.iloc[:, 0:8]
X1 = TrainingData.iloc[:, 0]
X2 = TrainingData.iloc[:, 1]
X3 = TrainingData.iloc[:, 2]
X4 = TrainingData.iloc[:, 3]
X5 = TrainingData.iloc[:, 4]
X6 = TrainingData.iloc[:, 5]
X7 = TrainingData.iloc[:, 6]
X8 = TrainingData.iloc[:, 7]
Y = TrainingData.iloc[:, 8]

TrainingData['Intercept'] = 1
Intercept = TrainingData.iloc[:, 9]

TrainingData
#print(type(TrainingData))


w = [w1, w2, w3, w4, w5, w6, w7, w8] = [0, 0, 0, 0, 0, 0, 0, 0]

c = 0

#w = np.zeros((1,8))


L = 0.0007              # Learning Rate
iterations = 1000
tolerance = 0.0001    
n = float(len(X))
t1 = 10
t2 = 0

for i in range(iterations): 
    Y_pred = w1*X1 + w2*X2 + w3*X3 + w4*X4 + w5*X5 + w6*X6 + w7*X7 +w8*X8 + c         # The current predicted value of Y
    D_w1 = (-2/n) * sum(X1 * (Y - Y_pred))      # Derivative wrt w
    D_w2 = (-2/n) * sum(X2 * (Y - Y_pred))
    D_w3 = (-2/n) * sum(X3 * (Y - Y_pred))
    D_w4 = (-2/n) * sum(X4 * (Y - Y_pred))
    D_w5 = (-2/n) * sum(X5 * (Y - Y_pred))
    D_w6 = (-2/n) * sum(X6 * (Y - Y_pred))  
    D_w7 = (-2/n) * sum(X7 * (Y - Y_pred)) 
    D_w8 = (-2/n) * sum(X8 * (Y - Y_pred)) 
    
    D_c = (-2/n) * sum(Y - Y_pred)            # Derivative wrt c
    w1 = w1 - L * D_w1                           # Update w
    w2 = w2 - L * D_w2
    w3 = w3 - L * D_w3
    w4 = w4 - L * D_w4
    w5 = w5 - L * D_w5
    w6 = w6 - L * D_w6
    w7 = w7 - L * D_w7
    w8 = w8 - L * D_w8
    c = c - L * D_c                           # Update c
    #t2 = math.sqrt(sum((Y - Y_pred)**2)/n)    # calculate root mean squared error (RMSE)
    t2 = np.sqrt(np.mean((Y_pred - Y)**2))
    if abs(t2 - t1) < tolerance:
        break 
    else:                                     # update t_previous for next for loop
        t1 = t2
    #print(t2)
    #print(i)
    
print('w= \n')
print (w1, w2, w3, w4, w5, w6, w7, w8, c,'\n')

#####Test Data

def z_scoreTest(df1, df2):
    df_std1 = df1.copy()
    df_std2 = df2.copy()
    for column in df_std2.columns:
        df_std2[column] = (df_std2[column] - df_std1[column].mean()) / df_std1[column].std()        
    return df_std2

TestData = z_scoreTest(TrainingD, TestD)

InterceptTest = TestData.iloc[:, 9]
X_TestData = TestData.iloc[:, 0:8]
Y_TestData = TestData.iloc[:, 8]


y_GradientDescent_TestData = np.dot([w1, w2, w3, w4, w5, w6, w7, w8], np.transpose(X_TestData))
RMSE_GradientDescent_TestData = np.sqrt(np.mean((y_GradientDescent_TestData - Y_TestData)**2)) + c

print('RMSE test data:', RMSE_GradientDescent_TestData,'\n')

#################################
### Other Learning rates and Tolerance

print('learning rate')
L = float(input())
print('tolerance')
tolerance = float(input())
iterations = 1000


for i in range(iterations): 
    Y_pred = w1*X1 + w2*X2 + w3*X3 + w4*X4 + w5*X5 + w6*X6 + w7*X7 +w8*X8 + c         # The current predicted value of Y
    D_w1 = (-2/n) * sum(X1 * (Y - Y_pred))      # Derivative wrt w
    D_w2 = (-2/n) * sum(X2 * (Y - Y_pred))
    D_w3 = (-2/n) * sum(X3 * (Y - Y_pred))
    D_w4 = (-2/n) * sum(X4 * (Y - Y_pred))
    D_w5 = (-2/n) * sum(X5 * (Y - Y_pred))
    D_w6 = (-2/n) * sum(X6 * (Y - Y_pred))  
    D_w7 = (-2/n) * sum(X7 * (Y - Y_pred)) 
    D_w8 = (-2/n) * sum(X8 * (Y - Y_pred)) 
    
    D_c = (-2/n) * sum(Y - Y_pred)            # Derivative wrt c
    w1 = w1 - L * D_w1                           # Update w
    w2 = w2 - L * D_w2
    w3 = w3 - L * D_w3
    w4 = w4 - L * D_w4
    w5 = w5 - L * D_w5
    w6 = w6 - L * D_w6
    w7 = w7 - L * D_w7
    w8 = w8 - L * D_w8
    c = c - L * D_c                           # Update c
    #t2 = math.sqrt(sum((Y - Y_pred)**2)/n)    # calculate root mean squared error (RMSE)
    t2 = np.sqrt(np.mean((Y_pred - Y)**2))
    if abs(t2 - t1) < tolerance:
        break 
    else:                                     # update t_previous for next for loop
        t1 = t2
    #print(t2)
    print(i)
    
print (w1, w2, w3, w4, w5, w6, w7, w8, c)

y_GradientDescent_TestData = np.dot([w1, w2, w3, w4, w5, w6, w7, w8], np.transpose(X_TestData))
RMSE_GradientDescent_TestData = np.sqrt(np.mean((y_GradientDescent_TestData - Y_TestData)**2)) + c

print('RMSE test data: \n', RMSE_GradientDescent_TestData,'\n')


"""
Xt = np.transpose(X)
XtX = np.dot(Xt,X)
Xty = np.dot(Xt,Y)
#beta = np.linalg.solve(XtX,Xty)


theta = np.linalg.inv(XtX)@Xty

#print(beta)
print(theta)

y_NormalEquations = np.dot(theta,Xt) 
y_GradientDescent = w1*X1 + w2*X2 + w3*X3 + w4*X4 + w5*X5 + w6*X6 + c



RMSE_NormalEquations = np.sqrt(np.mean((y_NormalEquations - Y)**2))
RMSE_GradientDescent = np.sqrt(np.mean((y_GradientDescent - Y)**2))

print(RMSE_NormalEquations,'\n',RMSE_GradientDescent)

"""
