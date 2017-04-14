%------------------------------------------------%
% Matlab script for recall and recognition tasks %
%------------------------------------------------%

% Change directory to where images and lures are located
orig_dir = cd('Images');

% Loop through trials
for trl = 1:length(ind2)
    
    % Extract the appropriate cue word
    cueString = char( CueWords{1}( pairings( 1, ind2(trl) ) ) );
    % Determine the length of the target string
    strlen = length(cueString);
    
    % Determine the boundaries (in pixels) within which the initial character
    % will be drawn. The first vector (i.e. bounds1 or bounds 3) has four values, the last two
    % denote the x and y values.
    [bounds1, bounds2] = Screen(window, 'TextBounds', cueString);
    
    % Determine the x and y coordinates in order to center the strings
    strposCue = [ center(1)-round(bounds1(3)/2), center(2)-round(bounds1(4)) ];

    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);

    % Display the cue word
    [ newX, newY ] = Screen('DrawText', window, cueString, strposCue(1), strposCue(2), [ 0 0 0 ]);

    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
        
    % Learning time
    WaitSecs(3);
    
    % Load in the image
    i_name = char( images{ pairings( image_type, ind2(trl) ) } );
    l_name = char( lures{ pairings( image_type, ind2(trl) ) } );
    theImage = imread( i_name );
    theLure = imread( l_name );
    
    % Get the size of the image
    [s1, s2, s3] = size(theImage);
    [s4, s5, s6] = size(theLure);
    
    % Get the aspect ratio of the image. We need this to maintain the aspect
    % ratio of the image when we draw it different sizes. Otherwise, if we
    % don't match the aspect ratio the image will appear warped / stretched
    aspectRatioImage = s2 / s1;
    aspectRatioLure = s5 / s4;

    % Scale the image
    imageWidth = imageHeight * aspectRatioImage;
    lureWidth = imageHeight * aspectRatioLure;
    
    % Make the image into a texture
    imageTexture = Screen('MakeTexture', window, theImage);
    lureTexture = Screen('MakeTexture', window, theLure);

    % Randomly determine which side of the screen the image will be
    % displayed on.
    Correct = randi(2);

    % Determine the coordinates for the image and lure
    picPos = -1 + (2*(Correct-1));

    imshft = center(1) + picPos*(imageWidth/2) + picPos*20;
    lushft = center(1) + picPos*(-1)*(lureWidth/2) + picPos*(-1)*20;

    baseRect = [0 0 imageWidth imageHeight];
    imrect = CenterRectOnPointd( baseRect, imshft, center(2));
    baseRect = [0 0 lureWidth imageHeight];
    lurect = CenterRectOnPointd( baseRect, lushft, center(2));
    
	% Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw a fixation cross
    Screen('DrawLines', window, fixAllCoords, fixLineWidthPix, [ 0 0 0 ], center);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Interstimulus gap
    WaitSecs(1);

    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw the image and lure
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    Screen('DrawTexture', window, lureTexture, [], lurect, 0);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Read in response and response time
    [ RT, resp ] = getResponseMultiple(keyOptions(1:2),1:2, 3.4995);
    % [ RT, resp ] = getResponse(leftKey, rightKey);
    
	% Log file
    log_results
    
    % Pen width for the frames
    penWidthPixels = 4;
    
    % Draw the rect to the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    Screen('FrameRect', window, [ 0 1 0 ], imrect, penWidthPixels);
    Screen('DrawTexture', window, lureTexture, [], lurect, 0);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Learning time
    WaitSecs(1);
    
    % Clean up textures to free memory
    Screen('Close', imageTexture);
    Screen('Close', lureTexture);

end;

% Return to the original directory
cd(orig_dir);