%------------------------------------------------------%
% Commands to initialize an experiment in Psychtoolbox %
%------------------------------------------------------%

% Close any current screens
close all;
sca

% Tell Psychtoolbox to use unified keyboard key names (helps with
% portability across different operating systems)
KbName('UnifyKeyNames');

% Here we call some default settings for setting up Psychtoolbox
PsychDefaultSetup(2);

% Get the screen numbers. This gives us a number for each of the screens
% attached to our computer.
screens = Screen('Screens');

% To draw we select the maximum of these numbers. So in a situation where we
% have two screens attached to our monitor we will draw to the external
% screen.
screenNumber = max(screens);

% Define black and white (white will be 1 and black 0). This is because
% in general luminace values are defined between 0 and 1 with 255 steps in
% between. All values in Psychtoolbox are defined between 0 and 1
white = WhiteIndex(0);
black = BlackIndex(0);

% Open a screen window
[window, windowRect] = PsychImaging('OpenWindow', screenNumber);

% Get the size of the on screen window
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

% Query the frame duration
ifi = Screen('GetFlipInterval', window);

% Set up alpha-blending for smooth (anti-aliased) lines
Screen('BlendFunction', window, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

% Get the centre coordinate of the window
[center(1), center(2)] = RectCenter(windowRect);
