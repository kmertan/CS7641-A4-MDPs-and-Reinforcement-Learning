# -*- coding: utf-8 -*-
"""
Created on Sun Apr 16 23:20:29 2017

@author: Jon
"""

import numpy as np

userMap = [[0,0,0,0,0,0,0,-5,0,1,0,0,0,-3,-3,-3,-3,-3,-3,0],
               [0,0,0,0,0,-5,-5,-5,-5,1,0,0,0,0,0,0,0,0,0,-3],
               [0,0,0,0,0,0,0,0,0,1,1,1,-1,1,1,1,1,1,1,0],
               [-3,1,1,-5,1,-5,1,0,-5,1,0,0,0,0,0,0,0,0,0,0],
               [-3,-3,1,0,0,0,0,0,0,1,0,0,1,1,1,1,1,1,1,1],
               [0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0],
               [0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0],
               [0,  0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0],
               [0,0,0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,1,0],
               [0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],
               [0,0,0,0,0,0,0,0,0,-3,1,0,0,1,0,0,0,1,0,0],
               [0,0,0,0,0,0,0,0,0,-3,-3,1,-1,-1,1,0,1,0,0,0],
               [0,0,0,0,0,0,0,0,0,-3,-3,1,-1,-1,-1,1,0,0,0,0],
               [0,1,1,1,1,0,0,0,0,-3,1,0,0,-1,-1,0,0,0,-5,-5],
               [0,1,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,-5,-5],
               [0,1,0,0,1,0,0,0,1,-5,0,0,0,0,0,0,0,1,1,-5],
               [0,1,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,1,-5,-5],
               [0,-5,-5,1,0,0,0,1,0,0,0,1,0,0,1,1,1,0,-5,-5],
               [0,1,-5,-5,1,-5,1,-5,0,0,0,0,1,1,1,0,0,0,0,0],
               [0,1,0,0,0,0,-5,-5,0,0,0,0,0,0,0,0,0,0,0,0]]
               
map2 = np.array(userMap)               