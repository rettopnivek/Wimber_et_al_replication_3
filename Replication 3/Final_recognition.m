%------------------------------------------%
% Matlab script for final recognition task %
%------------------------------------------%

% Change directory to where images and lures are located
orig_dir = cd('Images');

% Create a temporary matrix to store responses and stimulus/condition info
curResp = zeros( length(ind2), 13 );

% Loop through trials
for trl = st_trials:end_trials
    
    curTrial = curTrial + 1; % Increment trial index
    incStimInfo = incStimInfo + 1; % Increment stimulus info index
    
    % For complete records when logging the results
    cur_cue_num = pairings( 1, ind2(trl) );
    cueString = char( CueWords{1}( cur_cue_num ) );
    
    % Load in the image
    cur_i_num = pairings( image_type(trl), ind2(trl) );
    i_name = char( images{ cur_i_num } );
    l_name = char( lures{ cur_i_num } );
    theImage = imread( i_name );
    theLure = imread( l_name );
    
    % Extract the label of the image
    labelString = char( Labels{1}( all_labels( cur_i_num ) ) ); 
    
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
    WaitSecs(FRTInterStimGap);
    
    % Color the screen white
    Screen('FillRect', window, [ 1 1 1 ]);
    
    % Draw the image and lure
    Screen('DrawTexture', window, imageTexture, [], imrect, 0);
    Screen('DrawTexture', window, lureTexture, [], lurect, 0);
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
    % Record a subject's choice and response time
    % [ RT, resp ] = getResponseMultiple(keyOptions(1:2),1:2, 3.4995);
    if debug == 0
        [ RT, resp ] = getResponseMultiple(keyOptions(1:2),1:2,FRTTimeOut);
    else
        robotParFRT(trl) = robotAccParFRT(trl);
        [ RT, resp ] = Robot( Correct, 2, robotParFRT );
    end
    
    % Determine subject's accuracy
    Accuracy = resp == Correct;
    
    % Log the subject's responses for each trial
    % Trial,Cond,ImageNum,CueNum,Correct,Resp,Accuracy,RT,CueRep,ImageType,Baseline,Category,Block
    curResp(trl,:) = [ curTrial, expCond, cur_i_num, cur_cue_num, ...
        Correct, resp, Accuracy, RT, NaN(1), ImageType( trl ), ...
        BaseType( trl ), CatType( trl ), 1 ];
    
    % Store the file names, cue words, and labels for each trial
    stimulusInfo{incStimInfo} = [ num2str(curTrial) ',' num2str(expCond) ...
        ',1,' i_name '-' l_name ',' labelString ',' cueString ];
    
    % Flip to the screen (i.e. display stimuli)
    Screen('Flip', window);
    
	% Clean up textures to free memory
    Screen('Close', imageTexture);
    Screen('Close', lureTexture);

end;

% Save current responses
allResp = [ allResp; curResp ];

% Return to the original directory
cd(orig_dir);