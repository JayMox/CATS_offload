"""
Created on Fri Aug  4 10:46:55 2017
main_2.py: single ANN, clusters are part of ANN features, no k-fold (only ANN run)
@author: Zac Yung-Chun Liu
"""
import numpy as np
import pandas as pd

# Read datasets and clusters
df = pd.read_csv('data_CC_CA_2017.csv')
data = df.iloc[:, [0, 1, 2, 3]].values # time, depth, VV, ODBA
#cluster = np.genfromtxt("y_kmeans_smooth.csv",delimiter="\t")

# stack depth into one dataset
dataset = np.vstack(data[:,1])
#dataset = np.vstack((data[:,1], data[:,2]))
#dataset = dataset.T
# Encoding categorical data
#from sklearn.preprocessing import LabelEncoder, OneHotEncoder
#labelencoder_dataset = LabelEncoder()
#dataset[:,0] = labelencoder_dataset.fit_transform(dataset[:,0])
#onehotencoder = OneHotEncoder(categorical_features = [0])
#dataset = onehotencoder.fit_transform(dataset).toarray()

# Setup parameters
i_start = 0
i_end = 48000
row = i_end - i_start
win_1 = 35 # window size for depth
#win_2 = 30 # window size for VV
#c = 7 # number of clusters
#feature_n = win_1 + win_2 # number of features
feature_n = win_1
L1 = 40 # ANN layer 1 number of neurons
L2 = 40 # ANN layer 2 number of neurons
L3 = 40 # ANN layer 3 number of neurons
batch_s = 20 # ANN batch
epoch_n = 20 # ANN epoch, number of runs
# Create training set with moving window
from window_size import mv_window_0
X = mv_window_0(row, feature_n, win_1, dataset)
# Create testing set
from smooth import sm
y = sm(data[i_start:i_end,3],30)

# Setup parameters for outside dataset/ dev set
i_start_a = 48000
i_end_a = 51600
row_a = i_end_a - i_start_a
# Create training set with moving window
from window_size import mv_window_00
X_a = mv_window_00(row_a, feature_n, i_start_a, win_1, dataset)
# Create testing set
from smooth import sm
y_a = sm(data[i_start_a:i_end_a,3],30)

# ANN Training, merit scores
#from ML_ANN_train import ANN_train
#[regressor, y_pred_all, y_pred_all_a, r_score, rmse, r_score_all, rmse_all, r_score_a, rmse_a] = ANN_train(X, y, L1, L2, L3, feature_n, batch_s, epoch_n, X_a, y_a)

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 0)
# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X = sc.fit_transform(X)
X_train = sc.transform(X_train)
X_test = sc.transform(X_test)
X_all = sc.transform(X)
X_all_a = sc.transform(X_a)

# Importing the Keras libraries and packages
import keras
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasRegressor
from sklearn.model_selection import cross_val_score
#from keras.layers import Dropout
# Building ANN
regressor = Sequential()
regressor.add(Dense(units = L1, kernel_initializer = 'normal', activation = 'relu', input_dim = feature_n))
regressor.add(Dense(units = L2, kernel_initializer = 'normal', activation = 'relu'))
regressor.add(Dense(units = L3, kernel_initializer = 'normal', activation = 'relu'))
regressor.add(Dense(units = 1, kernel_initializer = 'normal'))
regressor.compile(optimizer = 'adam', loss = 'mean_squared_error')

# Fit the ANN to training set
regressor.fit(X_train, y_train, batch_size = batch_s, epochs = epoch_n)
# Predicting the Test set results
y_pred = regressor.predict(X_test)
# Predicting the whole dataset
y_pred_all = regressor.predict(X_all)
# Predicting outside dataset/ dev set
y_pred_all_a = regressor.predict(X_all_a)

# Calculate rmse and r^2
from sklearn.metrics import r2_score
from sklearn.metrics import mean_squared_error
r_score = r2_score(y_test, y_pred) #0.4765
rmse = mean_squared_error(y_test, y_pred) #0.2905
r_score_all = r2_score(y, y_pred_all) #0.4721
rmse_all = mean_squared_error(y, y_pred_all) #0.2935
r_score_a = r2_score(y_a, y_pred_all_a) #0.4721
rmse_a = mean_squared_error(y_a, y_pred_all_a) #0.2935

# Print merit scores
print('window size = ', win_1)
print('neurons = ', L1, L2, L3)
print('batch and epoch = ', batch_s, epoch_n)
print('r2 = ', r_score_all)
print('rmse = ', rmse_all)
print('for dev set')
print('r2 = ', r_score_a)
print('rmse = ', rmse_a)
