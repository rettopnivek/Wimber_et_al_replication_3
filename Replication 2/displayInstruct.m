%------------------------------%
% Code to display instructions %
%------------------------------%

% Determine the size of the text on the y-axis 
[dim, extra] = Screen( window, 'TextBounds', char(instruct{1}) );
% Determine the y-axis coordinates for each line of text
yInstruct = ones(1,lenIns)*dim(4) + 4;
yInstruct = ( center(2) - sum(yInstruct)/2 ) + cumsum(yInstruct) - yInstruct(1);

% Display instructions
for ln = 1:(lenIns - 1)
    DrawFormattedText(window, char(instruct{ln}), 'center', yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Learning time
WaitSecs(4);

% Display the 'Press any key' statement
for ln = 1:(lenIns)
    DrawFormattedText(window, char(instruct{ln}), 'center', yInstruct(ln), [ 0 0 0 ] );
end;

% Flip to the screen (i.e. display stimuli)
Screen('Flip', window);

% Wait for a key press to continue
KbStrokeWait;
