#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
from psychopy import prefs
import psychtoolbox.audio
prefs.hardware['audioDevice'] = [psychtoolbox.audio.get_devices()[-1]['DeviceName']] # type: ignore
from psychopy import core
from psychopy.sound import backend_ptb

## Feedback Sounds

low_tone = backend_ptb.AudioClip.sine(duration=0.1, freqHz=400)
high_tone = backend_ptb.AudioClip.sine(duration=0.1, freqHz=800)
correct_tone = low_tone + high_tone
incorrect_tone = high_tone + low_tone

feedback = backend_ptb.SoundPTB(secs=0.2, sampleRate=48000)

feedback.setSound(correct_tone)
feedback.play()
core.wait(3)
feedback.setSound(incorrect_tone)
feedback.play()
core.wait(0.3)
core.quit()

