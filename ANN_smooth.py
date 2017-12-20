"""
Created on Wed Jul 26 15:35:56 2017
@author: Zac Yung-Chun Liu
"""
# Function to smooth the data
def sm(y, box_pts):
    import numpy as np
    box = np.ones(box_pts)/box_pts
    y_smooth = np.convolve(y, box, mode='same')
    return y_smooth
