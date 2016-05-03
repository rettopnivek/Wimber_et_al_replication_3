%---------------------------------------------%
% Simplest example of Psychtoolbox experiment %
%---------------------------------------------%

% Purpose
% A simple example of using Psychtoolbox to make an experiment. Draws a red
% dot on the screen, then stops the experiment with a key press.

% Initialize Psychtoolbox
PTB_initial

% Draw a red dot in the center of the screen 
Screen('DrawDots', window, [0,0], 4, [1 0 0], center, 1);

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Now we have drawn to the screen we wait for a keyboard button press (any
% key) to terminate the demo.
KbStrokeWait;

% Clear the screen.
sca;
