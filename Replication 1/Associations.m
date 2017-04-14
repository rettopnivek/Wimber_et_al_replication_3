%---------------------------------------------%
% Matlab script for word-picture associations %
%---------------------------------------------%

% Change directory to where images and lures are located
orig_dir = cd('Images');

% Loop through trials
for trl = st_trials:end_trials
    
    % Load in the image
    f_name = char( images{ pairings( image_type, ind(trl) ) } );
    theImage = imread( f_name );
    
    % Get the size of the image
    [s1, s2, s3] = size(theImage);
    
    % Get the aspect ratio of the image. We need this to maintain the aspect
    % ratio of the image when we draw it different sizes. Otherwise, if we
    % don't match the aspect ratio the image will appear warped / stretched
    aspectRatio = s2 / s1;
    
    % Scale the image
    imageWidth = imageHeight * aspectRatio;
    
    % Set coordinates to center image
    baseRect = [0 0 imageWidth imageHeight];
    imrect = CenterRectOnPointd( baseRect, center(1), center(2));
        
    % Make the image into a texture
    imageTexture = Screen('MakeTexture', window, theImage);
        
    % Create the prime and target strings
    cueString = char( CueWords{1}( pairings( 1, ind(trl) ) ) );
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
    
    % Draw a fixation cross
    Screen('DrawLines', window, fixAllCoords, fixLineWidthPix, [ 0 0 0 ], center);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Interstimulus gap
    WaitSecs(0.5);
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw the image to the screen to the center of the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    
    % Draw the cue word above the image
    [ newX, newY ] = Screen('DrawText', window, cueString, strposCue(1), strposCue(2), [ 0 0 0 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Learning time
    WaitSecs(4);
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Clean up textures to free memory
    Screen('Close', imageTexture);
    
end;

% Return to the original directory
cd(orig_dir);