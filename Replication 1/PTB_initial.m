%------------------------------------------------------%
% Commands to initialize an experiment in Psychtoolbox %
%------------------------------------------------------%

% Clear the workspace
close all;
clear all;
sca

% Tell Psychtoolbox to use unified keyboard key names (helps with
% portability across different operating systems)
KbName('UnifyKeyNames');

% Here we call some default settings for setting up Psychtoolbox
PsychDefaultSetup(2);

% Define black and white (white will be 1 and black 0). This is because
% in general luminace values are defined between 0 and 1 with 255 steps in
% between. All values in Psychtoolbox are defined between 0 and 1
white = WhiteIndex(0);
black = BlackIndex(0);

% Open a screen window
[window, windowRect] = PsychImaging('OpenWindow', 0);

% Get the size of the on screen window
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

% Query the frame duration
ifi = Screen('GetFlipInterval', window);

% Set up alpha-blending for smooth (anti-aliased) lines
Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

% Get the centre coordinate of the window
[center(1), center(2)] = RectCenter(windowRect);
