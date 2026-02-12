#!/usr/bin/env python
# -*- coding: utf-8 -*-

## Import libraries

import time
import numpy as np
import pandas as pd
from psychopy.visual import Window
from psychopy.visual.circle import Circle
from psychopy.visual.textbox2 import TextBox2
from psychopy.core import Clock, quit, wait
from psychopy.visual.elementarray import ElementArrayStim
from scipy.stats import truncnorm
from psychopy.hardware.keyboard import Keyboard
from os.path import isfile

## Get experimenter input and create data file

participant_id = "test"
# participant_id = np.random.randint(1000, 9999)
# while isfile(f"./02_data/{participant_id}"):
#     participant_id = np.random.randint(1000, 9999)

path = f"./02_data/data_{participant_id}.csv"

## Create window

win = Window(monitor="testMonitor", screen=0, fullscr=True, 
                    units="pix", color=[0,0,0], colorSpace="rgb")

fixation = Circle(win, radius = 10, units = "pix")

## Initialize keyboard, dataframe and clocks

kb = Keyboard()
global_clock = Clock()
trial_clock = Clock()
experiment_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                      "section", "block", "trial",
                                      "mean_value", "std_value",
                                      "mean", "std", "bias"])



## Stimulus
n_circles = 8
outline_radius = 2.8      # radius of the circle arrangement
circle_size = 2.8 * 0.5   # radius of the colored circles

angles = np.linspace(0, 2*np.pi, n_circles, endpoint=False)
xs = outline_radius * np.cos(angles)
ys = outline_radius * np.sin(angles)
positions = np.column_stack([xs, ys])

stim = ElementArrayStim(
    win,
    nElements=n_circles,
    elementTex='none',              # solid shapes
    elementMask='circle',           # circle shape
    sizes=circle_size,
    xys=positions,
    units="deg",
    colors=np.zeros(shape=[8,3]),   # different color per element
    colorSpace = "rgb1"             # colors ranging from 0 to 1
)
# np.stack([np.zeros(8), np.zeros(8), np.zeros(8)], axis = 1) # to change colors

## Values and keys

color_dict = {"red": {"key": "c", "polarity": 1},
              "blue": {"key": "n", "polarity": -1}}

polarity_keys = {color_dict["red"]["polarity"]: color_dict["red"]["key"],
                 color_dict["blue"]["polarity"]: color_dict["blue"]["key"]}

confidence_values = ["1", "2", "3", "4", "8", "9", "0"]

## Numbers of blocks and trials

nt_introduction = 5     # Number of easy trials
nt_calibration = 64     # Number of trials in each staircase procedure block
nb_calibration = 6      # Minimum number of staircase procedure blocks
nt_testing = 64         # Number of trials in each testing block
nb_testing = 10         # Number of testing blocks

## Key guide
red_guide = TextBox2(win, f"{color_dict['red']['key']} = red", color="red", 
                     pos=(-0.5, -0.5), units = "norm", alignment="center")
blue_guide = TextBox2(win, f"{color_dict['blue']['key']} = blue", color="blue",
                      pos=(0.5,-0.5), units="norm", alignment="center")

## Trial function
def trial(color_values: list[float], stimulus_time: float = .16, 
          timeout: float = np.inf):
    """A single trial.

    Args:
        color_values (list[float]): List of red, green, and blue value (scale of 0 to 1).
        stimulus_time (float, optional): Time that stimulus is shown. Defaults to .16.
        timeout (float, optional): Maximum response time. Defaults to np.inf.

    Returns:
        _type_: _description_
    """    
    global color_dict
    color_keys = [color_dict["red"]["key"], 
                  color_dict["blue"]["key"]]
    
    # Start with fixation (autoDraw)
    win.flip()

    trial_clock.reset()
    kb.clock.reset()

    while trial_clock.getTime() < timeout:
        if trial_clock.getTime() < stimulus_time:
            stim.colors = color_values
            stim.draw()
       
        win.flip()

        keys = kb.getKeys(color_keys)

        if len(keys) > 0:
            response = keys[-1].name   # Only captures last response, might change later
            rt = keys[-1].rt
            return response, rt
    return "NA", "Inf"


## Trial feedback
correct_box = TextBox2(win, text="Correct", color="green",
                       fillColor="white", size=[None, 0.15],
                       units="norm", letterHeight=0.19, 
                       alignment="center", pos=(0,0.15))
incorrect_box = TextBox2(win, text="Incorrect", color="red",
                         fillColor="white", size=[None, 0.15],
                         units="norm", letterHeight=0.19, 
                         alignment="center", pos=(0,0.15))

def feedback(response: str, true_key: str):
    """Give feedback after trial

    Args:
        response (str): response in the preceding trial
        true_key (str): the key corresponding to the correct color in the \
preceding trial stimulus
    """    
    if response == true_key:
        correct_box.draw()
    else:
        incorrect_box.draw()
    win.flip()
    wait(1)
    return response == true_key

## Confidence rating
prompt = TextBox2(win, "How confident are you about your response?", 
                  pos=(0,0.5), units="norm")
left_cat = TextBox2(win, "Not confident at all", pos=(-0.5,-0.5),
                    units="norm")
right_cat = TextBox2(win, "Fully confident", pos=(0.5, -0.5), 
                     units="norm")

def confidence_rating():    
    prompt.draw()
    left_cat.draw()
    right_cat.draw()
    win.flip()


# block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
#                                  "section", "block", "trial",
#                                  "mean_value", "std_value",
#                                  "mean", "std", "bias"])

# easy_diff = .4
# easy_std = 0.1
# m = 0.5 + color_dict["red"]["polarity"] * easy_diff

# block_df.loc[0, "subjects"] = participant_id
# block_df.loc[0, "S"] = color_dict["red"]["key"]
# block_df.loc[0, "section"] = "test"
# block_df.loc[0, "block"] = 1
# block_df.loc[0, "trial"] = 1
# block_df.loc[0, "mean_value"] = m
# block_df.loc[0, "std_value"] = easy_std
# block_df.loc[0, "mean"] = "high"
# block_df.loc[0, "std"] = "low"
# block_df.loc[0, "bias"] = "none"

# block_df.loc[0, ["R"]], block_df.loc[0, ["rt"]] = \
#     trial(m, easy_std, np.inf, False)

# print(block_df)


## Introduction
block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                 "section", "block", "trial",
                                 "mean_values", "std_values",
                                 "mean", "std", "bias"])

instruction_text = ["In this experiment, you will perform a color judgement task, \
where you will judge the average of eight color hues around a fixation point \
ranging from red to blue.", 
f"If the average hue is more red, press <{color_dict['red']['key']}> with your left \
thumb, and if it is more blue, press <{color_dict['blue']['key']}> with your right thumb.",
"Keep your left pinky, ring, middle, and pointer finger on the 1, 2, 3, and 4 \
key respectively. Keep your right pointer, middle and ring finger on the 8, 9, \
and 0 key respectively.",
f"We will start with an easy training block. When you are ready press the \
<{color_dict['blue']['key']}> key with your fingers in position."]

continue_text = TextBox2(win, f"Press <{color_dict['blue']['key']}> to continue.",
                                pos=(0, -.5), units = "norm", alignment="center", 
                                letterHeight=.1, padding=0.02, autoDraw=True)

back_text = TextBox2(win, f"Press <{color_dict['red']['key']}> to go back.",
                     pos=(0, -.7), units = "norm", alignment="center", 
                     letterHeight=.1, padding=0.02)

# Go through instructions:
i = 0 
while i < len(instruction_text):
    if i > 0:
        back_text.draw()
    instruction = TextBox2(win, instruction_text[i], pos=(0, 0.25), 
                           units="norm", alignment="center",
                           letterHeight=.1, padding=0.02)
    instruction.draw()
    win.flip()
    keys = kb.waitKeys(keyList=[color_dict['blue']['key'],
                                color_dict['red']['key'],
                                "escape"])
    if keys[-1].name == "escape":
        win.close()
        quit()
    elif keys[-1].name == color_dict['red']['key']:
        i -= 1
    else:
        i += 1

continue_text.autoDraw = False

fixation.autoDraw = True
win.flip()

# Fill dataframe with stimulus information:
block_df["trial"] = list(range(nt_introduction))
block_df["subjects"] = participant_id
block_df["section"] = "Introduction"
block_df["block"] = 1
block_df["mean"] = "high"
block_df["std"] = "low"
block_df["bias"] = "none"
V_introduction = 0.033
block_df["std_values"] = V_introduction

# Sample stimulus type (red/blue) from a binomial
polarity = 2 * np.random.binomial(1, 0.5, nt_introduction) - 1
true_keys = list(map(polarity_keys.get, polarity))
block_df["S"] = true_keys

# Create average color values for each stimulus type
mean_values = 0.5 + polarity * 0.35
block_df["mean_values"] = mean_values

# Sample color values for each circle for each trial
color_values = np.transpose(
    truncnorm.rvs(a=0,b=1, loc = mean_values, scale = np.sqrt(V_introduction), 
                  size = (n_circles, nt_introduction))
)

# Stack Red values with complement (Blue values) and zeros (Green values) to 
# create RGB colors
colors = np.stack(
    [color_values, np.zeros((nt_introduction, n_circles)), 1 - color_values],
    axis = 2)

# Draw key guides
guides = red_guide.autoDraw = blue_guide.autoDraw = True
fixation.autoDraw = True
wait(2)

# Go through trials
t = 0
while t < nt_introduction:
    block_df.loc[i, "R"], block_df.loc[i, "rt"] = trial(colors[t], 
                                                        stimulus_time=np.inf,
                                                        timeout = np.inf)
    correct = feedback(str(block_df.loc[t, "R"]), 
                       true_key=str(polarity_keys[polarity[t]]))
    if not correct:
        nt_introduction += 1
    t += 1

block_df.to_csv(path)

# Turn off autodraw
guides = False
fixation.autoDraw = False

print(block_df)
win.close()
quit()

## Practice staircase blocks

# Instructions
instruction_text = [
    "In the next blocks, you will be trained on more difficult stimuli. \
The difficulty of these stimuli will be "
]

block_instruction = TextBox2(win, f"")

low_mean = .026
high_mean = .05
low_variance = .033
high_variance = .1

# 
b = 0
correct = []
while b < nb_calibration:
    # Create Stimuli
    # Fill dataframe with stimulus information:
    block_df["trial"] = list(range(nt_calibration))
    block_df["subjects"] = participant_id
    block_df["section"] = "adjustment"
    block_df["block"] = b
    mean = np.random.choice(["low", "high"], nt_calibration, 
                                        replace = True)
    block_df["mean"] = mean
    block_df["std"] = np.random.choice(["low", "high"], nt_calibration, 
                                        replace = True)
    block_df["bias"] = np.random.multinomial()
    V_introduction = 0.033
    block_df["std_values"] = V_introduction



    for t in range(nt_calibration):
        response, rt = trial(colors[t])
        correct.append(feedback(response, true_key[t]))
    block_df.to_csv(path)
    IES_LMHV = np.median(rt[LMHV]) / np.mean(correct[LMHV])
    IES_HMLV = np.median(rt[HMLV]) / np.mean(correct[LMHV])
    if abs(IES_HMLV - IES_LMHV) > .1:
        

## Experimental blocks

## Debriefing

win.close()
quit()
