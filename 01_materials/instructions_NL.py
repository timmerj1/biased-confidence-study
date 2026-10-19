#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
from psychopy.visual import Window
from psychopy.visual.circle import Circle
from psychopy.visual.textbox2 import TextBox2
from psychopy.visual.slider import Slider
from psychopy.core import Clock, quit, wait
from psychopy.visual.elementarray import ElementArrayStim
from psychopy.hardware.keyboard import Keyboard
from psychopy.event import Mouse
from os.path import isfile

win = Window(monitor='testMonitor', screen=0, fullscr=True, units='norm', 
             color=[0,0,0], colorSpace='rgb')

fixation = Circle(win, radius = 0.1, units = 'height')

# refresh_rate = 120 # Monitor refresh rate in Hz

font = 'Helvetica'

continue_time = 1

## Flags

slide1 = True
slide2 = False
slide3 = True
slide4 = True

## Color Counterbalance

cb = np.random.choice([0,1], 2, replace=False)
colors= np.array(['red', 'blue'])[cb]
kleuren = np.array(['ROOD', 'BLAUW'])[cb]
polarity = np.array([1, -1])[cb]
color_keys = ['w', 'd']

means = np.array([.026, .05])
stds = np.array([.033, .1])
names = np.array(['low', 'high'])

## Initialize keyboard, dataframe and clocks

kb = Keyboard()
global_clock = Clock()
trial_clock = Clock()
slider = Slider(win, ticks=(-1,1), labels=('Zeker Fout','Zeker Juist'), 
                startValue=0, markerColor='White', size=[0.8, 0.1],
                units='norm')
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

## Stimulus
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

# color sampling
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

## Stimuli

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
    units='deg',
    colors=np.zeros(shape=[8,3]),   # different color per element
    colorSpace='rgb1'             # colors ranging from 0 to 1
)

intro_stim_left = ElementArrayStim(
    win,
    nElements=n_circles,
    elementTex='none',
    elementMask='circle',
    sizes=intro_circle_size,
    xys=intro_positions + [-0.25, -0.1],
    units='height',
    colors=sample([0.5 + polarity[0] * 0.026], [0.033])[0],
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
    colors=sample([0.5 + polarity[1] * 0.026], [0.033])[0],
    colorSpace='rgb1'
)

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


## Instruction Text Boxes

text1 = TextBox2(win, 'Welkom bij dit experiment!', pos=(0, 0.25), 
                 units='height' ,alignment='center', letterHeight=0.03, 
                 font=font)

text2 = TextBox2(win, 'In dit experiment zal je gekleurde figuren op je scherm'\
                 ' te zien krijgen.', pos=(0, 0.125), units='height', alignment='center', letterHeight=0.03, font=font)

text3 = TextBox2(win, 'Je kan deze beslissing op je eigen tempo maken, we zijn enkel geinteresseerd in hoe accuraat je deze keuze kunt maken.', pos=(0, 0), units='height', alignment='center', letterHeight=0.03, font=font)

continue_text = TextBox2(win, 'Klik op de linkermuisknop om verder te gaan.', pos=(0,-0.3), units='height', alignment='center', letterHeight=0.03, font=font)

## Slide 1

text1.draw()
text2.draw()
text3.draw()
win.flip()
wait(continue_time)
text1.draw()
text2.draw()
text3.draw()
continue_text.draw()
win.flip()
waitClick()

## Slide 2

text1.text = 'Hieronder zie je enkele voorbeelden van stimuli die je zal zien:'

left_box_text = f'Links is een verzameling die gemiddeld {kleuren[0]} is:'

right_box_text = f'Rechts is een verzameling die gemiddeld {kleuren[1]} is:'

left_box = TextBox2(win, left_box_text, pos=(-0.25, 0), units='height', alignment='center', letterHeight=0.025, size=[0.4, None], font=font)

right_box = TextBox2(win, right_box_text, pos=(0.25,0),
                     units='height', alignment='center', letterHeight=0.025, 
                     size=[0.4, None], font=font)

intro_stim_left.draw()
intro_stim_right.draw()
left_box.draw()
right_box.draw()
win.flip()
wait(continue_time)
intro_stim_left.draw()
intro_stim_right.draw()
left_box.draw()
right_box.draw()
continue_text.draw()
win.flip()
waitClick()

## Slide 3

text1.text = 'Je kan antwoorden met de response schaal die je hieronder ziet,' 
text2.text = f'''Door de cursor naar links te bewegen voor {kleuren[0]}, en naar rechts voor {kleuren[1]}.'''
continue_text.text = 'Druk vervolgens op de linkermuisknop om verder te gaan.'

text1.draw()
text2.draw()
slider.draw()
win.flip()
wait(continue_time)
confidence_rating([text1, text2, continue_text])

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
wait(continue_time)
text1.draw()
text2.draw()
text3.draw()
continue_text.draw()
win.flip()
waitClick()

win.close()
quit()
