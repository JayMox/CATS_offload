"""
Created on Wed Jul 26 15:56:24 2017
@author: Zac Yung-Chun Liu
"""
# Moving window size
def mv_window_0(row, col, win_1, dataset):
    import numpy as np
    #row = len(ind_0) # number of timestep
    #col = 52 # number of features (7 cluster + 30 VV + 15 VA = 52)
    #win_1 = 30 # moving window size for VV
    #win_2 = 15 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    # For D
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d] = dataset[i+d]
    return X

def mv_window(row, col, win_1, win_2, dataset):
    import numpy as np
    #row = len(ind_0) # number of timestep
    #col = 52 # number of features (7 cluster + 30 VV + 15 VA = 52)
    #win_1 = 30 # moving window size for VV
    #win_2 = 15 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    VV = dataset[:,0]
    VA = dataset[:,1]
    # For VV
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d] = VV[i+d]
    # For VA
    for d in range(0,win_2):
        for i in range(0,row):
            X[i][d+win_1] = VA[i+d]
    return X

def mv_window_00(row, col, i_start_a, win_1, dataset):
    import numpy as np
    #row = len(ind_0) # number of timestep
    #col = 52 # number of features (7 cluster + 30 VV + 15 VA = 52)
    #win_1 = 30 # moving window size for VV
    #win_2 = 15 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    # For D
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d] = dataset[i+i_start_a+d]
    return X

def mv_window_1(row, col, i_start_a, win_1, win_2, dataset):
    import numpy as np
    #row = len(ind_0) # number of timestep
    #col = 52 # number of features (7 cluster + 30 VV + 15 VA = 52)
    #win_1 = 30 # moving window size for VV
    #win_2 = 15 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    VV = dataset[:,0]
    VA = dataset[:,1]
    # For VV
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d] = VV[i+i_start_a+d]
    # For VA
    for d in range(0,win_2):
        for i in range(0,row):
            X[i][d+win_1] = VA[i+i_start_a+d]
    return X

def mv_window_2(row, col, win_1, win_2, c, data, dataset):
    import numpy as np
    #row = 48000 # number of timestep
    #col = 67 # number of features (7 cluster + 30 VV + 30 VA = 67)
    #win_1 = 30 # moving window size for VV
    #win_2 = 30 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    from smooth import sm
    VV = sm(data[:,2],30)
    VA = sm(data[:,3],30)
    # For cluster groups
    for g in range(0,c):
        for i in range(0,row):
            X[i][g] = dataset[i][g]
    # For VV
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d+c] = VV[i+d]
    # For VA
    for d in range(0,win_2):
        for i in range(0,row):
            X[i][d+c+win_1] = VA[i+d]
    return X

def mv_window_3(row, col, i_start_a, win_1, win_2, c, data, dataset):
    import numpy as np
    #row = 48000 # number of timestep
    #col = 67 # number of features (7 cluster + 30 VV + 30 VA = 67)
    #win_1 = 30 # moving window size for VV
    #win_2 = 30 # moving window size for VA
    #c = 7 # cluster group
    X = np.zeros((row, col))
    from smooth import sm
    VV = sm(data[:,2],30)
    VA = sm(data[:,3],30)
    # For cluster groups
    for g in range(0,c):
        for i in range(0,row):
            X[i][g] = dataset[i+i_start_a][g]
    # For VV
    for d in range(0,win_1):
        for i in range(0,row):
            X[i][d+c] = VV[i+i_start_a+d]
    # For VA
    for d in range(0,win_2):
        for i in range(0,row):
            X[i][d+c+win_1] = VA[i+i_start_a+d]
    return X