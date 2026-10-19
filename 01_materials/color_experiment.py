#!/usr/bin/env python
# -*- coding: utf-8 -*-

## Import libraries

import numpy as np
import pandas as pd
from psychopy.visual import Window
from psychopy.visual.circle import Circle
from psychopy.visual.textbox2 import TextBox2
from psychopy.visual.slider import Slider
from psychopy.core import Clock, quit, wait
from psychopy.visual.elementarray import ElementArrayStim
from scipy.stats import truncnorm
from psychopy.hardware.keyboard import Keyboard
from psychopy.event import Mouse
from os.path import isfile

## Get experimenter input and create data file

participant_id = "test"
# participant_id = np.random.randint(1000, 9999)
# while isfile(f"./02_data/{participant_id}"):
#     participant_id = np.random.randint(1000, 9999)

path = f"./02_data/data_{participant_id}.csv"

## Counterbalancing

color_counterbalance_nl = np.random.choice(["rood", "blauw"], 2, replace = False)
conf_counterbalance_nl = np.random.choice(["rood", "blauw"], 2, replace = False)

color_counterbalance = np.random.choice(["red", "blue"], 2, replace = False)
conf_counterbalance = np.random.choice(["red", "blue"], 2, replace = False)

## Create window

win = Window(monitor="testMonitor", screen=0, fullscr=True, units="pix", 
             color=[0,0,0], colorSpace="rgb")

fixation = Circle(win, radius = 10, units = "pix")

refresh_rate = 120 # Monitor refresh rate in Hz

## Initialize keyboard, dataframe and clocks

kb = Keyboard()
global_clock = Clock()
trial_clock = Clock()
slider = Slider(win, ticks=(-1,1), labels=("Uncertain", "Certain"), 
                startValue=0.5, markerColor="White")
mouse = Mouse(visible=False)

## Stimulus
n_circles = 8
outline_radius = 2.8      # radius of the circle arrangement
circle_size = 2.8 * 0.5   # radius of the colored circles

# Create colored circle positions
angles = np.linspace(0, 2*np.pi, n_circles, endpoint=False)
xs = outline_radius * np.cos(angles)
ys = outline_radius * np.sin(angles)
positions = np.column_stack([xs, ys])

# `stim` is the main stimulus object that will be called throughout the 
# experiment, consisting of 8 circles layed out in a circle. The colors will 
# change using stim.colors.
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

bias = pd.Series([0.3, 0.5, 0.7], index=["blue", "none", "red"])

means = np.array([.026, .05])
stds = np.array([.033, .1])
names = np.array(["low", "high"])

## Numbers of blocks and trials

nt_introduction = 5     # Number of easy trials
nt_calibration = 5      # Number of trials in each staircase procedure block 64
nb_calibration = 1      # Minimum number of staircase procedure blocks 6
nt_testing = 5          # Number of trials in each testing block 64
nb_testing = 1          # Number of testing blocks 10

## Key guide
red_guide = TextBox2(win, f"{color_dict['red']['key']} = red", color="red", 
                     pos=(-0.5, -0.5), units = "norm", alignment="center")
blue_guide = TextBox2(win, f"{color_dict['blue']['key']} = blue", color="blue",
                      pos=(0.5,-0.5), units="norm", alignment="center")

## Trial functions

def sample_values(means, stds):
    """Will result in RecursionError when means are outside of or too 
close to 0 and 1, or when standard deviations are too high."""
    values = np.random.standard_normal((n_circles, len(means)))
    values = (values - values.mean(axis=0)) / values.std(axis = 0)
    values = means + values * stds
    cutoff = np.any((values > 1) | (values < 0), axis=0)
    if any(cutoff):
        values[:,cutoff] = sample_values(np.array(means)[cutoff], np.array(stds)[cutoff])
    return values

def sample(means, stds):
    values = sample_values(means, stds)
    return np.transpose(np.stack(
        [values, np.zeros((n_circles, len(means))), 1 - values]))

def trial(color_values: list[float], stimulus_time: float = .16, 
          timeout: float = np.inf):
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

def feedback(response, true_key):
    if response == true_key:
        correct_box.draw()
    else:
        incorrect_box.draw()
    win.flip()
    wait(1)
    return response == true_key

## Confidence rating

def confidence_rating(maxTime = np.inf):
    """Not the proper way to use the slider but it works"""
    slider.reset()
    slider.setAutoDraw(True)
    win.flip()
    trial_clock.reset()
    mouse.setPos((0,0))
    while trial_clock.getTime() < maxTime:
        slider.markerPos = 2 * mouse.getPos()[0] / slider.validArea.width # type: ignore
        win.flip()
        if mouse.getPressed()[0]: # type: ignore
            response = slider.markerPos
            rt = trial_clock.getTime()
            return response, rt

## 

response, rt = confidence_rating() # type: ignore
print("response = ", response, "\nRT = ", rt)

## Introduction
block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                 "section", "block", "trial",
                                 "mean_values", "std_values",
                                 "mean", "std", "bias", "conf","confrt"])

instruction_text = \
["Welkom bij dit experiment!",
"In dit experiment zal je gekleurde figuren op je scherm te zien krijgen.",
"We zijn geinteresseerd in hoe goed jij de gemiddelde kleur van de figuren kan bepalen,",
"en kan aangeven op elke trial of het gemiddelde eerder blauw of eerder rood is.",
    
    f"In this experiment, you will perform a color judgement task, where you will \
judge the average of eight color hues ranging from {color_counterbalance[0]} to\
 {color_counterbalance[1]}.", 
f"If the average hue is more {color_counterbalance[0]}, press <{color_dict[color_counterbalance[0]]['key']}> with your left \
thumb, and if it is more {color_counterbalance[1]}, press <{color_dict[color_counterbalance[1]]['key']}> with your right thumb.",
f"We will start with an easy training block. When you are ready press the \
<{color_dict['blue']['key']}> key with your fingers in position."]

continue_text = TextBox2(win, f"Press <{color_dict['blue']['key']}> to continue.",
                                pos=(0, -.5), units = "norm", alignment="center", 
                                letterHeight=.1, padding=0.02, autoDraw=True)

back_text = TextBox2(win, f"Press <{color_dict['red']['key']}> to go back.",
                     pos=(0, -.7), units = "norm", alignment="center", 
                     letterHeight=.1, padding=0.02)

intro = False
if intro == True:
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
    block_df["mean"] = names[1]
    block_df["std"] = names[0]
    block_df["bias"] = 0.5
    block_df["std_values"] = stds[0]

    # Sample stimulus type (red/blue) from a binomial
    polarity = 2 * np.random.binomial(1, 0.5, nt_introduction) - 1
    true_keys = list(map(polarity_keys.get, polarity))
    block_df["S"] = true_keys

    # Create average color values for each stimulus type
    mean_values = 0.5 + polarity * 0.35
    block_df["mean_values"] = mean_values

    # Sample color values for each circle for each trial
    color_values = np.transpose(
        truncnorm.rvs(a=0,b=1, loc = mean_values, scale = np.sqrt(stds[0]), 
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
        block_df.loc[t, "R"], block_df.loc[t, "rt"] = trial(colors[t], 
                                                            stimulus_time=np.inf,
                                                            timeout = np.inf)
        correct = feedback(str(block_df.loc[t, "R"]), 
                        true_key=str(true_keys[t]))
        if not correct:
            nt_introduction += 1
        t += 1

    block_df.to_csv(path)

    # Turn off autodraw
    guides = False
    fixation.autoDraw = False


## Practice staircase blocks

# Instructions
instruction_text = [
    "In the next blocks, you will be trained on more difficult stimuli. \
The difficulty of these stimuli will be "
]

block_instruction = TextBox2(win, f"")

staircase = False
if staircase == True:
    b = 0
    while b < nb_calibration:
        # Create dataframe:
        block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                        "section", "block", "trial",
                                        "mean_values", "std_values",
                                        "mean", "std", "bias"])
        # Fill dataframe with stimulus information:
        block_df["trial"] = list(range(nt_calibration))
        block_df["subjects"] = participant_id
        block_df["section"] = "adjustment"
        block_df["block"] = b
        index = np.random.binomial(1, 0.5, (2, nt_calibration))
        LMHV = np.logical_and(index[1], np.logical_not(index[0]))
        HMLV = np.logical_and(index[0], np.logical_not(index[1]))
        block_df["mean"] = names[index[0]]
        block_df["std"] = names[index[1]]
        block_df["bias"] = 0.5

        # Sample stimulus type (red/blue) from a binomial
        polarity = 2 * np.random.binomial(1, 0.5, nt_calibration) - 1
        true_keys = list(map(polarity_keys.get, polarity))
        block_df["S"] = true_keys

        mean_values = block_df["mean_values"] = 0.5 + polarity * means[index[0]]
        std_values = block_df["std_values"] = stds[index[1]]

        colors = sample(mean_values, std_values)
        correct = np.empty(nt_calibration)

        response = pd.Series()
        rt = pd.Series()
        for t in range(nt_calibration):
            response[t], rt[t] = trial(colors[t])
            correct[t] = feedback(response[t], true_keys[t])
        block_df["R"] = response
        block_df["rt"] = rt
        block_df.to_csv(path, mode = "a")

        
        #!!!
        print(block_df, "\n")
        print("LMHV = ", correct[LMHV], "\n")
        print("HMLV = ", correct[HMLV], "\n")
        print(correct, "\n")
        
        diff = np.mean(correct[LMHV]) / np.mean(correct[HMLV]) - 1
        print(diff)
        if abs(diff) > 0.15:
            if diff < 0:
                means[0] += 0.0025
            else:
                means[0] -= 0.0025
        elif abs(diff) > 0.1:
            if diff < 0:
                means[0] += 0.0012
            else:
                means[0] -= 0.0012
        elif abs(diff) > 0.06:
            if diff < 0:
                means[0] += 0.0005
            else:
                means[0] -= 0.0005

        if (b > 5) and (abs(diff) > 0.06):
            nb_calibration += 1
        b += 1

## Experimental blocks

## Debriefing

win.close()
quit()
