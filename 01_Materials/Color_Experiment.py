#!/usr/bin/env python
# -*- coding: utf-8 -*-

## import libraries

import time
import numpy as np
import pandas as pd
from psychopy import core, visual, event
import scipy

## Create Window

win = visual.Window(monitor="testMonitor",
                    size=[800, 600], units="pix", color=[0,0,0],
                    colorSpace="rgb")

fixation = visual.Circle(win, radius = 5)

fixation.draw()

win.flip()

"""
"""

## Parameters 
n_circles = 8
radius = 200   # radius of the big circle

## Colors
mean, std = 0.2, 0.05
a, b = (0 - mean) / std, (1 - mean) / std

C = scipy.stats.truncnorm.rvs(a,b, loc = mean, scale = std, size = 8)

## Positions arranged in a circle
angles = np.linspace(0, 2*np.pi, n_circles, endpoint=False)
xs = radius * np.cos(angles)
ys = radius * np.sin(angles)
positions = np.column_stack([xs, ys])
colors = np.stack([C, np.zeros(8), 1 - C], axis=1)

stim = visual.ElementArrayStim(
    win,
    nElements=n_circles,
    elementTex=None,          # solid shapes
    elementMask='circle',     # circle shape
    sizes=80,
    xys=positions,
    colors=colors,            # different color per element
    colorSpace = "rgb1"
)

## Draw once
fixation.draw()
stim.draw()
win.flip()

stim.colors = np.stack([C, np.zeros(8), 1 - C], axis=1)

fixation.draw()
win.flip()

win.close()
core.quit()
