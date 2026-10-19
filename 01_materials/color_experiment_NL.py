#!/usr/bin/env python
# -*- coding: utf-8 -*-

## Import libraries

import numpy as np
import pandas as pd
from psychopy import prefs
import psychtoolbox.audio
prefs.hardware['audioDevice'] = [psychtoolbox.audio.get_devices()[-1]['DeviceName']] # type: ignore
print(prefs.hardware['audioDevice'])
from psychopy import core
from psychopy.sound import backend_ptb, audioclip
from psychopy.visual import Window
from psychopy.visual.circle import Circle
from psychopy.visual.textbox2 import TextBox2
from psychopy.visual.slider import Slider
from psychopy.visual.elementarray import ElementArrayStim
from scipy.stats import truncnorm
from psychopy.hardware.keyboard import Keyboard
from psychopy.event import Mouse
from os import remove
from os.path import isfile
from sys import exit


## Counterbalancing

# cb = np.random.choice([0,1], 2, replace=False) # Counterbalance index
cb = np.array([1, 0])
colors = np.array(["blue", "red"])[cb]
kleuren = np.array(["BLAUW", "rood"])[cb]
color_keys = np.array(["w", "d"]) # not indexed by cb to ensure counterbalancing
key_dict = dict(zip(color_keys, colors))
color_polarity = np.array([-1, 1])[cb]

## Color Values

means = np.array([.026, .05]) # Difference from 0.5, so high red is 0.55, low 
# high blue is 0.45. Low red is 0.526, low blue is 0.474
stds = np.array([.033, .1]) # standard deviations of the color values
low_high = np.array(["low", "high"]) # corresponding labels

## Get experimenter input and create data file

participant_id = input("ID: ")

path = f"./02_data/data_NL_{participant_id}.csv"

if participant_id == 'test':
    test = True
    if isfile(path):
        remove(path)
else:
    test = False

if isfile(path):
    exit("Participant ID already in use.")

df = pd.DataFrame(columns=["language", "subjects", "S", "R", "rt",
                           "section", "block", "trial",
                           "mean_values", "std_values",
                           "mean", "std", "bias", "conf","confrt"])

df.to_csv(path, header=True, index=False)

## Flags

intro = True
staircase = True
main = False
debrief = False

## Numbers of blocks and trials

if test is True:
    nt_calibration = 64     # Number of trials in each staircase procedure block
    nb_calibration = 2      # Minimum number of staircase procedure blocks
    nt_testing = 1          # Number of trials in each testing block
    nb_testing = 1          # Number of testing blocks
else:
    nt_introduction = 5     # Number of easy trials
    nt_calibration = 64     # Number of trials in each staircase procedure
    nb_calibration = 2      # Minimum number of staircase procedure blocks
    nt_testing = 64         # Number of trials in each testing block
    nb_testing = 15         # Number of testing blocks


## Create window

win = Window(monitor="testMonitor", screen=1, fullscr=True, units="pix", 
             color=[0,0,0], colorSpace="rgb")

refresh_rate = 120 # Monitor refresh rate in Hz

## Initialize keyboard, dataframe and clocks

kb = Keyboard()
global_clock = core.Clock()
trial_clock = core.Clock()
slider = Slider(win, ticks=(-1,1), labels=("Uncertain", "Certain"), 
                startValue=0.5, markerColor="White")
mouse = Mouse(visible=False)

def waitClick(max_time = 50):
    mouse.clickReset()
    trial_clock.reset()
    while True and trial_clock.getTime() < max_time:
        buttons = mouse.getPressed()
        escape = kb.getKeys(keyList=['escape'])
        if buttons == None:
            continue
        elif buttons[0] == 1:
            break
        elif len(escape) > 0:
            win.close()
            quit()
        else:
            continue
    if trial_clock.getTime() > max_time:
        return 1
    else:
        return 0

## Stimuli

## * Text

font = 'Noto Sans'

## * Colors
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

## * Shapes

fixation = Circle(win, radius = 0.3, units = "deg")

n_circles = 8
outline_radius = 2.8      # radius of the circle arrangement
circle_size = 2.8 * 0.5   # radius of the colored circles

# Create colored circle positions
angles = np.linspace(0, 2*np.pi, n_circles, endpoint=False)
xs = outline_radius * np.cos(angles)
ys = outline_radius * np.sin(angles)
positions = np.column_stack([xs, ys])

# Introduction stimuli
intro_outline_radius = 0.05      # radius of the circle arrangement
intro_circle_size = 0.05 * 0.5   # radius of the colored circles

# Create colored circle positions
intro_xs = intro_outline_radius * np.cos(angles)
intro_ys = intro_outline_radius * np.sin(angles)
intro_positions = np.column_stack([intro_xs, intro_ys])

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

intro_stim_left = ElementArrayStim(
    win,
    nElements=n_circles,
    elementTex='none',
    elementMask='circle',
    sizes=intro_circle_size,
    xys=intro_positions + [-0.25, -0.1],
    units='height',
    colors=sample([0.5 + color_polarity[0] * 0.026], [0.033])[0],
    colorSpace='rgb1'
)

intro_stim_right = ElementArrayStim(
    win,
    nElements=n_circles,
    elementTex='none',
    elementMask='circle',
    sizes=intro_circle_size,
    xys=intro_positions + [0.25, -0.1],
    units='height',
    colors=sample([0.5 + color_polarity[1] * 0.026], [0.033])[0],
    colorSpace='rgb1'
)

## Key guide
left_guide = TextBox2(win, f"{color_keys[0]} = {kleuren[0]}",
                      color=colors[0], pos=(-0.5, -0.5), units = "norm", alignment="center", letterHeight=0.05)

right_guide = TextBox2(win, f"{color_keys[1]} = {kleuren[1]}", color=colors[1],
                      pos=(0.5,-0.5), units="norm", alignment="center", 
                      letterHeight=0.05)

## Trial functions

def trial(color_values: list[float], stimulus_time: float = .16, 
          timeout: float = 1.5):
    global color_keys

    # Start with fixation (autoDraw)
    win.flip()

    trial_clock.reset()
    kb.clock.reset()

    while trial_clock.getTime() < timeout:
        if trial_clock.getTime() < stimulus_time:
            stim.colors = color_values
            stim.draw()
       
        win.flip()

        keys = kb.getKeys(keyList = ['w', 'd', 'escape'])

        if len(keys) > 0:
            if keys[-1].name == 'escape':
                win.close()
                core.quit()
            pressed_key = keys[-1].name
            response = key_dict[pressed_key]
            rt = keys[-1].rt
            return response, rt
    return "NA", "Inf"

## Auditory feedback

feedback_duration = 0.2
sample_rate = 48000
low_tone = backend_ptb.AudioClip.sine(duration=feedback_duration / 2, freqHz=400)
high_tone = backend_ptb.AudioClip.sine(duration=feedback_duration / 2, freqHz=800)
correct_tone = low_tone + high_tone
incorrect_tone = high_tone + low_tone

snd = backend_ptb.SoundPTB(secs=feedback_duration, sampleRate=sample_rate)

def auditory_feedback(correct):
    if correct:
        snd.setSound(correct_tone)
        snd.play()
        core.wait(feedback_duration)
    else:
        snd.setSound(incorrect_tone)
        snd.play()
        core.wait(feedback_duration)

## Confidence rating

def confidence_rating(stimuli = None, maxTime = np.inf):
    """Not the proper way to use the slider but it works"""
    slider.reset()
    if stimuli is not None:
        [stim.setAutoDraw(True) for stim in stimuli]
    slider.autoDraw = True
    win.flip()
    trial_clock.reset()
    mouse.setPos((0,0))
    while trial_clock.getTime() < maxTime:
        slider.markerPos = 2 * mouse.getPos()[0] / slider.validArea.width  # type: ignore
        escape = kb.getKeys(keyList = ['escape'])
        # type: ignore
        win.flip()
        if mouse.getPressed()[0]: # type: ignore
            slider.markerColor = 'red'
            win.flip()
            response = slider.markerPos
            rt = trial_clock.getTime()    
            slider.autoDraw = False
            if stimuli is not None:
                [stim.setAutoDraw(False) for stim in stimuli]
            slider.markerColor = 'white'
            return response, rt
        elif len(escape) > 0:
            win.close()
            quit()

## Introduction

block_df = pd.DataFrame(columns=["language", "subjects", "S", "R", "rt",
                                 "section", "block", "trial",
                                 "mean_values", "std_values",
                                 "mean", "std", "bias", "conf","confrt"])

# Instruction Text Boxes

text1 = TextBox2(win, 'Welkom bij dit experiment!', pos=(0, 0.25), 
                 units='height' ,alignment='center', letterHeight=0.03, 
                 font=font)

text2 = TextBox2(win, 'In dit experiment zal je gekleurde figuren op je scherm'\
                 ' te zien krijgen.', pos=(0, 0.125), units='height',
                 alignment='center', letterHeight=0.03, font=font)

text3 = TextBox2(win, f'''De taak is om te bepalen of de gemiddelde kleur van
deze figuren meer {kleuren[0]} of {kleuren[1]} is, op een schaal tussen volledig
{kleuren[0]} tot volledig {kleuren[1]}.''',
                 pos=(0, 0), units='height', alignment='center', letterHeight=0.03, font=font)

continue_text = TextBox2(win, 'Klik op de linkermuisknop om verder te gaan.',
                         pos=(0,-0.3), units='height', alignment='center',
                         letterHeight=0.03, font=font)

continue_time = 0.5 # Minimum time per introduction slide.

left_box_text = f'Links is een verzameling die gemiddeld {kleuren[0]} is:'

right_box_text = f'Rechts is een verzameling die gemiddeld {kleuren[1]} is:'

left_box = TextBox2(win, left_box_text, pos=(-0.25, 0), units='height', 
                    alignment='center', letterHeight=0.025, size=[0.4, None],
                    font=font)

right_box = TextBox2(win, right_box_text, pos=(0.25,0),
                    units='height', alignment='center', letterHeight=0.025, 
                    size=[0.4, None], font=font)
if intro == True:
    # Go through instructions
    ## Slide 1

    text1.draw()
    text2.draw()
    text3.draw()
    win.flip()
    core.wait(continue_time)
    text1.draw()
    text2.draw()
    text3.draw()
    continue_text.draw()
    win.flip()
    waitClick()

    ## Slide 2

    text1.text = 'Hieronder zie je enkele voorbeelden van stimuli die je zal' \
    ' zien:'

    intro_stim_left.draw()
    intro_stim_right.draw()
    left_box.draw()
    right_box.draw()
    win.flip()
    core.wait(continue_time)
    intro_stim_left.draw()
    intro_stim_right.draw()
    left_box.draw()
    right_box.draw()
    continue_text.draw()
    win.flip()
    waitClick()

    ## Slide 3

    text1.text = f'''Je kan antwoorden met de [{color_keys[0]}]-toets voor 
{kleuren[0]}, of de [{color_keys[1]}]-toets voor {kleuren[1]}'''
    left_box.text = f'{kleuren[0]} = {color_keys[0]}'
    left_box.color = colors[0]
    right_box.text = f'{kleuren[1]} = {color_keys[1]}'
    right_box.color = colors[1]

    text1.draw()
    left_box.draw()
    right_box.draw()
    win.flip()
    core.wait(continue_time)
    continue_text.draw()
    text1.draw()
    left_box.draw()
    right_box.draw()
    win.flip()
    waitClick()

    ## Slide 4

    text1.text = 'Zoals je kunt zien is de taak redelijk moeilijk, daarom zullen' \
    ' we starten met enkele oefentrials, om te wennen aan de taak.'
    text2.text = 'Je zal twee tonen horen: stijgend als je correct bent, en ' \
    'dalend als je incorrect bent'
    text3.text = 'Deze feedback kan je gebruiken om te leren hoe je de taak het' \
    ' best uit kunt voeren'

    text1.draw()
    text2.draw()
    text3.draw()
    win.flip()
    core.wait(continue_time)
    text1.draw()
    text2.draw()
    text3.draw()
    continue_text.draw()
    win.flip()
    waitClick()
    
    ## Slide 5
    text2.text = 'We gaan nu beginnen met de taak.'
    text3.text = f'''Houd je linker wijs- en middelvinger op de 
[{color_keys[0]}] en [{color_keys[1]}] toetsen'''

    text2.draw()
    text3.draw()
    continue_text.draw()
    left_guide.draw()
    right_guide.draw()
    win.flip()
    

    # ## Slide 5

    # text1.text = 'Je kan antwoorden met de response schaal die je hieronder ziet,' 
    # text2.text = f'''Door de cursor naar links te bewegen voor {kleuren[0]}, en naar rechts voor {kleuren[1]}.'''
    # continue_text.text = 'Druk vervolgens op de linkermuisknop om verder te gaan.'

    # text1.draw()
    # text2.draw()
    # slider.draw()
    # win.flip()
    # core.wait(continue_time)
    # confidence_rating([text1, text2, continue_text])

## Practice staircase blocks

lmlv_correct = np.array([])
lmlv_correct_block = np.array([])
lmlv_rt = np.array([])
lmlv_rt_block = np.array([])

hmhv_correct = np.array([])
hmhv_correct_block = np.array([])
hmhv_rt = np.array([])
hmhv_rt_block = np.array([])

block_d_ies = np.array([])

lmlv_evaluate = False
hmhv_evaluate = False

n = 1

matched = False

if staircase:
    conf_trial = False
    b = 0
    while b < nb_calibration:
        # Create dataframe:
        block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                        "section", "block", "trial",
                                        "mean_values", "std_values",
                                        "mean", "std", "bias", "conf"])
        # Fill dataframe with stimulus information:
        block_df["trial"] = list(range(nt_calibration))
        block_df["subjects"] = participant_id
        block_df["section"] = "staircase"
        block_df["block"] = b
        
        index_mean = np.random.binomial(1, 0.5, nt_calibration)
        index_match = np.random.binomial(1, 0.8, nt_calibration)
        index = np.array([index_mean,
                          np.where(index_match, index_mean, 1 - index_mean)])

        lmlv = (index[0] == index[1]) * (index[0] == 0)
        hmhv = (index[0] == index[1]) * (index[0] == 1)

        block_df["mean"] = low_high[index[0]]
        block_df["std"] = low_high[index[1]]
        block_df["bias"] = 0.5

        # Sample stimulus type (red/blue) from a binomial
        color_factor = np.random.binomial(1, 0.5, nt_calibration)
        polarities = color_polarity[color_factor]
        true_keys = color_keys[color_factor]
        S = block_df["S"] = colors[color_factor]

        mean_values = block_df["mean_values"] = 0.5 + polarities * means[index[0]]
        std_values = block_df["std_values"] = stds[index[1]]

        color_sample = sample(mean_values, std_values)
        correct = np.empty(nt_calibration)

        response = pd.Series()
        rt = pd.Series()
        conf = pd.Series()

        fixation.autoDraw = True
        left_guide.autoDraw = True
        right_guide.autoDraw = True
        
        win.flip()
        core.wait(2)

        for t in range(nt_calibration):
            response[t], rt[t] = trial(color_sample[t])
            correct[t] = (response[t] == S[t])

            if response[t] == "NA":
                # please respond in time
                auditory_feedback(False)
                core.wait(1)
            else:
                if lmlv[t]:
                    lmlv_correct = np.append(lmlv_correct, correct[t])[-32:]
                    lmlv_correct_block = np.append(lmlv_correct_block, correct[t])
                    lmlv_rt = np.append(lmlv_rt, rt[t] * 1000)[-32:]
                    lmlv_rt_block = np.append(lmlv_rt_block, rt[t] * 1000)
                    if np.sum(lmlv_correct) > 0:
                        lmlv_ies = np.median(lmlv_rt) / np.mean(lmlv_correct)
                        lmlv_evaluate = True

                if hmhv[t]:
                    hmhv_correct = np.append(hmhv_correct, correct[t])[-32:]
                    hmhv_correct_block = np.append(hmhv_correct_block, correct[t])
                    hmhv_rt = np.append(hmhv_rt, rt[t] * 1000)[-32:]
                    hmhv_rt_block = np.append(hmhv_rt_block, rt[t] * 1000)
                    if np.sum(hmhv_correct) > 0:
                        hmhv_ies = np.median(hmhv_rt) / np.mean(hmhv_correct)
                        hmhv_evaluate = True

                if lmlv_evaluate and hmhv_evaluate and (t + 1) % 16 == 0:
                    d_means = means[1] - means[0]
                    d_ies = lmlv_ies - hmhv_ies
                    matched = abs(d_ies) < 10
                    means[0] = max(0, min(means[1], 
                                          means[0] + (1 / n**0.6) * (d_ies * (1 - matched) / 5000) * d_means))
                    n += 1
                    print("Lower Mean :", means[0], "IES :", d_ies, "matched :", matched)

                if conf_trial and t > 5:
                    core.wait(.6)
                    conf[t] = confidence_rating()
                    auditory_feedback(correct[t])
                    core.wait(1)
                else:
                    conf[t] = pd.NA
                    auditory_feedback(correct[t])
                    core.wait(.6)

        block_df["R"] = response
        block_df["rt"] = rt
        block_df.to_csv(path, mode = "a", header=False, index=False)

        lmlv_ies_block = np.median(lmlv_rt_block) / np.mean(lmlv_correct_block)
        hmhv_ies_block = np.median(hmhv_rt_block) / np.mean(hmhv_correct_block)
        block_d_ies = np.append(block_d_ies, lmlv_ies_block - hmhv_ies_block)
        
        block_matched = abs(block_d_ies) < 10
        print("Block IES Difference: ", block_d_ies)
        
        lmlv_correct_block = np.array([])
        lmlv_rt_block = np.array([])
        hmhv_correct_block = np.array([])
        hmhv_rt_block = np.array([])        
        
        fixation.autoDraw = False
        left_guide.autoDraw = False
        right_guide.autoDraw = False

        # Block Summary

        text1.text = f'''Gemiddeld aantal fouten voor {kleuren[0]} was: 
        {1 - np.mean(correct[S == colors[0]]):.0%}'''
        
        text2.text = f'''Gemiddeld aantal fouten voor {kleuren[1]} was: 
        {1 - np.mean(correct[S == colors[1]]):.0%}'''

        text3.text = f'''{block_d_ies}'''

        text1.draw()
        text2.draw()
        text3.draw()
        continue_text.draw()
        win.flip()
        
        if b == 5:
            text1.text = 'In de volgende blokken willen we weten hoe zeker je' \
            ' bent dat je de juiste keuze gemaakt hebt.'
            text2.text = 'Na een aantal oefentrials'
            continue

        if not block_matched and nb_calibration < 6:
            nb_calibration += 1
        
        b += 1
        waitClick()

## Experimental blocks

if main:
    b = 0
    while b < nb_testing:
        # Create dataframe:
        block_df = pd.DataFrame(columns=["subjects", "S", "R", "rt",
                                        "section", "block", "trial",
                                        "mean_values", "std_values",
                                        "mean", "std", "bias", "conf"])
        # Fill dataframe with stimulus information:
        block_df["trial"] = list(range(nt_calibration))
        block_df["subjects"] = participant_id
        block_df["section"] = "staircase"
        block_df["block"] = b
        index = np.random.binomial(1, 0.5, (2, nt_calibration))
        LMHV = np.logical_and(index[1], np.logical_not(index[0]))
        HMLV = np.logical_and(index[0], np.logical_not(index[1]))
        block_df["mean"] = low_high[index[0]]
        block_df["std"] = low_high[index[1]]
        block_df["bias"] = 0.5

        # Sample stimulus type (red/blue) from a binomial
        color_factor = np.random.binomial(1, 0.5, nt_calibration)
        polarities = 2 * cb[color_factor] - 1
        true_keys = color_keys[color_factor]
        S = block_df["S"] = colors[color_factor]

        mean_values = block_df["mean_values"] = 0.5 + polarities * means[index[0]]
        std_values = block_df["std_values"] = stds[index[1]]

        color_sample = sample(mean_values, std_values)
        correct = np.empty(nt_calibration)

        response = pd.Series()
        rt = pd.Series()
        conf = pd.Series()

        fixation.autoDraw = True
        left_guide.autoDraw = True
        right_guide.autoDraw = True
        
        win.flip()
        core.wait(2)

        for t in range(nt_calibration):
            response[t], rt[t] = trial(color_sample[t])
            correct[t] = (response[t] == S[t])
            if t > 5:
                conf[t] = confidence_rating()
            else:
                auditory_feedback(correct[t])
                conf[t] = pd.NA
        block_df["R"] = response
        block_df["rt"] = rt
        block_df.to_csv(path, mode = "a")

        fixation.autoDraw = False
        left_guide.autoDraw = False
        right_guide.autoDraw = False

        # Block Summary

        text1.text = f'''Gemiddeld aantal fouten voor {kleuren[0]} was: 
        {1 - np.mean(correct[S == colors[0]]):.0%}'''
        
        text2.text = f'''Gemiddeld aantal fouten voor {kleuren[1]} was: 
        {1 - np.mean(correct[S == colors[1]]):.0%}'''

        text1.draw()
        text2.draw()
        continue_text.draw()
        win.flip()

        b += 1


## Debriefing

if debrief:
    text2.text = 'Je bent bij het einde van de taak beland.'
    text3.text = 'Hartelijk dank voor je participatie!'
    kb.waitKeys(maxWait=60, keyList=['escape'])

win.close()
core.quit()
