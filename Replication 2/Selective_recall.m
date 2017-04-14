%------------------------------------------%
% Matlab script for selective recall phase %
%------------------------------------------%

stringChoice = 'F - O - S - ?'; % Define the characters to display
strlenC = length(stringChoice);
strPosChoice = ones( strlenC, 2 );

% Get the boundaries of the individual characters
for sp = 1:strlenC
    [ strDim, extra ] = Screen(window, 'TextBounds', stringChoice(sp) );
    strPosChoice(sp,1) = strDim(3);
    strPosChoice(sp,2) = strDim(4);
end;

% Define the x and y coordinates such that the choice options are centered
% and appear below where the images typically appear
xShft = cumsum( strPosChoice(1:strlenC,1) );
xShft = reshape(xShft,1,13);
xShft = xShft - xShft(1);
xCenter = ones( 1, strlenC ) * ( round( sum( strPosChoice(1:strlenC,1) )/2 ) );
strPosChoice( 1:strlenC, 1 ) = xCenter - xShft;

strPosChoice( 1:strlenC, 1) = center(1) - strPosChoice( 1:strlenC, 1 );
strPosChoice( 1:strlenC, 2) = center(2) + (imageHeight/2) + round( strPosChoice( 1:strlenC, 2 )/2 ) + 2;

colorIndex = [ 1 5 9 ];

% 
for trl = st_trials:end_trials
    
	i_name = char( images{ pairings( 2, ind2(trl) ) } );
    c_name = char( lures{ pairings( 3, ind2(trl) ) } );
    
    % Create the prime and target strings
    cueString = char( CueWords{1}( pairings( 1, ind2(trl) ) ) );
    % Determine the length of the target string
    strlen = length(cueString);
    
    % Determine the boundaries (in pixels) within which the initial character
    % will be drawn. The first vector (i.e. bounds1 or bounds 3) has four values, the last two
    % denote the x and y values.
    [bounds1, bounds2] = Screen(window, 'TextBounds', cueString);
    
    % Determine the x and y coordinates in order to center the strings
    strposCue = [ center(1)-round(bounds1(3)/2), center(2)-(imageHeight/2)-round(bounds1(4))-2 ];
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Display the cue word
    Screen('DrawText', window, cueString, strposCue(1), strposCue(2), [ 0 0 0 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Cue time
    WaitSecs(4);
    
    % Set the color of choice options to white
    choiceColor = zeros( strlenC, 3 );
        
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Display the cue word
    [ newX, newY ] = Screen('DrawText', window, cueString, strposCue(1), strposCue(2), [ 0 0 0 ]);
    % Display the choice options
    for sp = 1:strlenC
        [ newX, newY ] = Screen('DrawText', window, stringChoice(sp), strPosChoice(sp,1), strPosChoice(sp,2), choiceColor(sp,1:3));
    end;
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Record the subject's response
    [ RT, resp ] = getResponseMultiple(keyOptions,1:4, 1.5);
    
    if (resp > 0)
        % Feedback
        choiceColor( colorIndex( pairings( 5, ind2(trl) ) ), 2 ) = 2;
        
        % Display the cue word
        [ newX, newY ] = Screen('DrawText', window, cueString, strposCue(1), strposCue(2), [ 0 0 0 ]);
        % Display the choice options
        for sp = 1:strlenC
            [ newX, newY ] = Screen('DrawText', window, stringChoice(sp), strPosChoice(sp,1), strPosChoice(sp,2), choiceColor(sp,1:3));
        end;
        
        % Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);
        
        % Wait the remaining time in the 1.5 second interval
        WaitSecs(1.5006 - RT);
        
        % Interstimulus gap
        
        % Color the screen white
    	Screen('FillRect', window, [ 1 1 1 ]);
        
    	% Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);
        
        WaitSecs(1);
        
    else
        % Color the screen white
        Screen('FillRect', window, [ 1 1 1 ]);
        
        % Flip to the screen (i.e. display stimuli)
        Screen('Flip', window);

        [ RT, resp ] = getResponseMultiple(keyOptions,1:4, 0.9995);
        if (resp > 0)
            WaitSecs(1-RT);
        end;
        RT = RT + 1.5005;
        
    end;
    
	% Log file
    log_results
end;