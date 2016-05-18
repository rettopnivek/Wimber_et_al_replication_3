%-----------------------------------------%
% Matlab script for familiarization phase %
%-----------------------------------------%

% Change directory to where images and lures are located
orig_dir = cd('Images');

% Create a temporary matrix to store responses and stimulus/condition info
curResp = zeros( totImage*2, 13 );
% Set increment for saving stimulus info
incStimInfo = 1;

% Loop through trials
for trl = st_trials:end_trials
    
    curTrial = curTrial + 1; % Increment trial index
    incStimInfo = incStimInfo + 1; % Increment stimulus info index
    
    % Load in the image
    f_name = char( all_type{ ind(trl) } );
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
    labelString = char( Labels{1}( all_labels( ind(trl) ) ) );
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
    WaitSecs(FamImageTime);
    
    % Draw the image to the screen to the center of the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    
    % Draw the label word below the image
    [ newX, newY ] = Screen('DrawText', window, labelString, strposLabel(1), strposLabel(2), [ 0 0 0 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
	% Length of time to display label
    WaitSecs(FamLabelTime);

    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw the image to the screen to the center of the screen
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Determine if person is familiar with image
    % Here 1 = Yes, 2 = No (i.e. left is yes, right is no)
    if debug == 0
        [ RT, resp ] = getResponseMultiple( keyOptions(1:2), 1:2, FamTimeOut );
    else
        [ RT, resp ] = Robot( 1, 1, robotParFam );
    end
    
    % If the person indicates that he/she is not familiar with the image,
    % set it to repeat at the end of the phase
    if resp == 2
        repeat = [ repeat, ind(trl) ];
    end;
    
    % Log the subject's responses for each trial
    % Trial,Cond,ImageNum,CueNum,Correct,Resp,Accuracy,RT,CueRep,ImageType,Baseline,Category,Block
    curResp(trl,:) = [ curTrial, expCond, ImageNum( ind(trl) ), NaN(1), ...
        NaN(1), resp, NaN(1), RT, NaN(1), ImageType( ind(trl) ), ...
        BaseType( ind(trl) ), CatType( ind(trl) ), 1 ];
    
    % Store the file names, cue words, and labels for each trial
    stimulusInfo{incStimInfo} = [ num2str(curTrial) ',' num2str(expCond) ...
        ',' '1,' f_name ',' labelString ',' 'NaN' ];
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Clean up textures to free memory
    Screen('Close', imageTexture);
end;

% Save current responses
allResp = [ allResp; curResp( st_trials:end_trials, : ) ];

% Return to the original directory
cd(orig_dir);