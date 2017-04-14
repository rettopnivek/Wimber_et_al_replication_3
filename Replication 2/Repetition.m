%------------------------------------%
% Matlab script for repetition phase %
%------------------------------------%

% Change directory to where images and lures are located
orig_dir = cd('Images');

% Loop through trials
for trl = 1:length(repeat)
    
    % Load in the image
    f_name = char( all_type{ repeat(trl) } );
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
    labelString = char( Labels{1}( all_labels( repeat(trl) ) ) );
    % Determine the length of the target string
    strlen = length(labelString);
    
    % Determine the boundaries (in pixels) within which the initial character
    % will be drawn. The first vector (i.e. bounds1 or bounds 3) has four values, the last two
    % denote the x and y values.
    [bounds1, bounds2] = Screen(window, 'TextBounds', labelString);
    
    % Determine the x and y coordinates in order to center the strings
    strposLabel = [ center(1)-round(bounds1(3)/2), center(2)+(imageHeight/2)+round(bounds1(4))+2 ];
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw the image to the screen to the center of the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
        
    % Wait 1 second
    WaitSecs(1);
    
    % Draw the image to the screen to the center of the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    
    % Draw the label word below the image
    [ newX, newY ] = Screen('DrawText', window, labelString, strposLabel(1), strposLabel(2), [ 0 0 0 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
	% Length of time to display label
    WaitSecs(1.5);
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Clean up textures to free memory
    Screen('Close', imageTexture);

end;

% Return to the original directory
cd(orig_dir);