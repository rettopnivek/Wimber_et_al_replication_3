function [ nx, ny, bbox ] = displayInstruct( window, string, center, lnSpace, waitTime, waitPos )
% Purpose:
% Displays a set of centered text (colored black), waits a period of time
% and then prompts the subject to move on with a keypress.
% Arguments:
% window   - A pointer to a PTB screen
% string   - A string with the instructions (line breaks are allowed)
% center   - A vector with the x and y-axis values for the screen center
%            in pixels
% lnSpace  - The spacing between lines (1.5 is recommended)
% waitTime - The amount of time to pause before allowing the subject to 
%            continue
% waitPos  - The offset for the 'Press any key to continue' statement
% Returns:
% The x and y-axis values and the dimensions of the bounding box

[ nx, ny, bbox ] = DrawFormattedText( window, string, 'center','center', [], [], [], [], lnSpace, [], [] );
Screen('Flip', window);
WaitSecs(waitTime);

[ nx, ny, bbox ] = DrawFormattedText( window, string, 'center','center', [], [], [], [], lnSpace, [], [] );
string = 'Press any key to continue';
[nx,ny,bbox] = DrawFormattedText( window, string,'center',center(2)+waitPos, [], [], [], [], lnSpace, [], [] );
Screen('Flip', window);
KbStrokeWait;

end

