%---------------------------------%
% Wimber et al (2015) replication %
% Kevin Potter and David Huber    %
% Updated 4/14/2015               %
%---------------------------------%

%{
Purpose:
A Matlab script to run a replication of the experimental design used in
Wimber, Alink, Charest, Kriegeskorte, and Anderson (2015).

Requirements:
Matlab R2013a
Psychtoolbox
Folder labeled 'Images' (with balanced number of image files)
Text file of cues; 'Cue_words.txt'
Text file of image labels; 'Labels.txt'

Outputs:
A .csv file with the stimuli and response information
A .mat file with the starting and ending time, and the stimuli ordering
info

Notes:
- We used a reduced stimulus set for this replication
- The experiment can be run using the batch file 'runexeperiment.bat'.
- To prematurely end the experiment, press shift + 2.
- linspace(a,b,n) generates a sequence of n values between a and b
- Thanks is extended to Will Hopper and Angela Nelson Lowe for help with
  the code in Matlab
%%% TO DO %%%
- Check stimulus ordering, randomization, and logging
- Complete annotations to Exp.m and subsidary matlab scripts
- Check issues with some types of keyboard input (e.g. the shift key)
%}

% Load in useful functions
% addpath('I:\Stuff\Kevin''s Stuff\Postdoc\Psychtoolbox\Memory_study_replication\Wimber_et_al_2015_replication')
addpath('F:\Stuff\Kevin''s Stuff\Postdoc\Wimber_et_al_2015_replication')
% addpath('I:\Stuff\Kevin''s Stuff\Postdoc\Psychtoolbox')
% addpath('F:\Stuff\Kevin''s Stuff\Postdoc\Psychtoolbox')
% addpath('C:\Users\lab\Documents\MATLAB\Wimber_et_al_2015_replication')

% Initialize Psychtoolbox
PTB_initial % Call separate Matlab script

%-------------------------%
% Index for code segments %
%-------------------------%

%{
To go to a specific segment, search for the matching 'Lookup - ...'

Variable declarations
Lookup - 00: Input subject ID number for later output files
Lookup - 01: Fixation point and image heights
Lookup - 02: Word picture association indices
Lookup - 03: Word cues and image labels
Lookup - 04: Key assignment
Lookup - 05: File names for images and lures

Stimulus display

Lookup - Log 0
Lookup - Instruct 0
Lookup - Instruct 1
Lookup - 06: Familiarization phase
Lookup - 07: Word-picture associations study/test for targets
Lookup - Instruct 2
Lookup - Instruct 3
Lookup - 08: Word-picture associations study/test for competitors
Lookup - Instruct 4
Lookup - Instruct 5
Lookup - Break
Lookup - Instruct 6
Lookup - 09: Selective retrieval
Lookup - Instruct 7
Lookup - 10: Final recognition task
Lookup - 11: End of experiment
Lookup - Instruct 8
%}

%-----------------------%
% Variable declarations %
%-----------------------%

%%% Input subject ID number for later output files %%%
% Lookup - 00
SubjectIDInput % Call separate Matlab script

% Time that the experiment began
startTime = clock;

%%% Fixation point and image heights %%%
% Lookup - 01

% Set the line length for the fixation cross (in pixels)
fixCrossDimPix = 20;
% Set the x and y coordinates for the fixation cross, where (0,0) is the
% center of the screen
fixAllCoords = ones(2,4);
fixAllCoords(1,1:4) = [-fixCrossDimPix fixCrossDimPix 0 0];
fixAllCoords(2,1:4) = [0 0 -fixCrossDimPix fixCrossDimPix];
% Set the line width for our fixation cross
fixLineWidthPix = 2;

% Scale images to all be the same height (25 percent of the screen height)
heightScale = 0.25;
imageHeight = screenYpixels * heightScale;

% Clean up workspace
clear fixCrossDimPix;

%%% Word picture association indices %%%
% Lookup - 02

% Set the total number of images for each category
nCat = 24;
% Set the total number of images (and therefore also lures)
totImage = nCat*3;
% Divide by two to get the number of target and competitor images
halfCat = nCat/2;

% The target and competitors need to be randomly paired such that the
% competitor comes from a separate category than the target

% First, randomly divide each category into first and second sets
% (i.e. targets and competitors)
sel = randperm(nCat);
first = sel( 1:(halfCat) );
second = sel( (halfCat+1):(nCat) );

sel = randperm(nCat);
first = [ first, sel( 1:(halfCat) ) + nCat ];
second = [ second, sel( (halfCat+1):(nCat) ) + nCat ];

sel = randperm(nCat);
first = [ first, sel( 1:(halfCat) ) + nCat*2 ];
second = [ second, sel( (halfCat+1):(nCat) ) + nCat*2 ];

% Next reorganize the second set so that the
% categories don't match between sets
sel = [ (halfCat + 1):(halfCat*2), (halfCat*2 + 1):(halfCat*3) ];
srt = randperm(halfCat*2);
sel = sel(srt);

temp = sel(1:halfCat);
sel = sel( (halfCat + 1):(halfCat*2) );
rem = sel( sel < (halfCat*2 + 1) );

sel = sel( sel > (halfCat*2) );
l = halfCat-length(sel);
sel2 = randperm(halfCat);
srt = randperm(halfCat);
sel = [ sel, sel2(1:l) ];
sel = sel(srt);
temp = [ temp, sel ];
sel = [ rem, sel2( (l+1):halfCat ) ];

srt = randperm(halfCat);
sel = sel(srt);
temp = [ temp, sel ];

% The total number of images to be shown for the associations
nAssoc = halfCat*3;
% Randomly define which images will be targets/competitors, and which will be part
% of the baseline condition
%%% NOTE %%%
% Unlike the original study, we'll set the baseline condition to have half
% of the images
nBaseline = nAssoc/2;
cond = [ ones(1,nAssoc-nBaseline), ones(1,nBaseline)*2 ];
cond = cond( randperm(length(cond)) );

% Create an index of which category to which each image belongs
correctCategory = [ ones(1,nCat), ones(1,nCat)*2, ones(1,nCat)*3 ];

% Combine all the indices into a single matrix for easy referencing
pairings = [ randperm(nAssoc); first; second(temp); cond; correctCategory(first); correctCategory(second(temp)) ];
% Row 1: Index for the word cue
% Row 2: Image number for targets
% Row 3: Image number for competitors
% Row 4: Conditions, where Target/Competitor = 1, Baseline  = 2
% Row 5: Categories for targets, where Faces = 1, Objects = 2, Scenes = 3
% Row 6: Categories for competitors, where Faces = 1, Objects = 2, Scenes = 3

% Clear up workspace
clear sel srt temp first second cond rem correctCategory l sel2;

%%% Word cues and image labels %%%
% Lookup - 03

% Open a link to the text file
fid = fopen('Cue_words.txt','r');
% Scan the text into a cell array
CueWords = textscan(fid,'%s','delimiter','\n');
% Close the link to the text file
fclose(fid);
% Access the first row of text
% CueWords{1}(1)

% Open a link to the text file
fid = fopen('Labels.txt','r');
% Scan the text into a cell array
Labels = textscan(fid,'%s','delimiter','\n');
% Close the link to the text file
fclose(fid);
% Access the first row of text
% Labels{1}(1)

% Clean up the workspace
clear fid;

%%% Key assignment %%%
% Lookup - 04

% Set the keys for picking the left or right picture during the
% recognition test
keyOptions = getKeyAssignments('jkl;',1,300);
leftKey = keyOptions(1);
rightKey = keyOptions(2);

%%% File names for images and lures %%%
% Lookup - 05

% Set the categories
category = { 'F_'; 'O_'; 'S_' };

% Create a cell string array for the image file names
images = num2str( ones(totImage,1) );
images = cellstr(images);

% Create a cell string array for the lure file names
lures = num2str( ones(totImage,1) );
lures = cellstr(lures);

% Fill cell string array with file names
for i = 1:totImage
    st = 1;
    if (i > nCat)
        st = 2;
    end;
    if (i > nCat*2)
        st = 3;
    end;
    images{i} = { [ category{st} 'Image_' num2str(i) '.bmp' ] };
    lures{i} = { [ category{st} 'Lure_' num2str(i) '.bmp' ] };
end;

%------------------%
% Stimulus display %
%------------------%

%{
Experiment design

Task 1: Familiarization phase with all images and lures

3 blocks of 12 images each
    Task 2: Word-picture association (Targets)
    Task 3: Recall and recognition (Targets; 2 cycles)
3 blocks of 12 images each
    Task 2: Word-picture association (Competitors)
    Task 3: Recall and recognition (Competitors; 1 cycle)

Brief rest period

Task 4: Selective recall (Non-baseline images only)

Task 5: Final recognition (All targets and competitors)

%}

%%% Initialize log file %%%
% Lookup - Log 0
log_state = 0;
log_results % Call separate Matlab script for logging results

%%% Instructions %%%
% Lookup - Instruct 0

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'For this experiment, you will complete several different ';
instruct{2} = 'memory tasks involving pictures and words. The experiment ';
instruct{3} = 'consists of 5 different tasks and should take about 45 ';
instruct{4} = 'minutes. The manner in which you give answers will change ';
instruct{5} = 'for different memory tests, so please read the detailed ';
instruct{6} = 'instructions shown on the screen before each test. ';
instruct{7} = 'Memorizing can be dull and difficult, but please ';
instruct{8} = 'try your best. We appreciate your effort! ';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Instructions %%%
% Lookup - Instruct 1

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Task 1: Familiarization';
instruct{2} = 'Next you will view a list of pictures one at a time. The ';
instruct{3} = 'goal of this task is to familiarize yourself with these ';
instruct{4} = 'pictures and their labels (there is no need to memorize ';
instruct{5} = 'the pictures at this time). Following each picture, an ';
instruct{6} = 'appropriate label will appear. Once the label disappears, ';
instruct{7} = 'hit the "j" key to start the next trial if you managed to ';
instruct{8} = 'get a good look at the picture. Otherwise, hit the "k" key ';
instruct{9} = 'and you will be given a second chance to look at the ';
instruct{10} = 'picture after the list is complete.';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Familiarization phase %%%
% Lookup - 06

% Determine stimulus information for later logging
ImageType = [ ones(1,totImage), zeros(1,totImage) ];
ImageType( pairings(3, 1:nAssoc) ) = 2;
BaseType = zeros(1,totImage*2);
BaseType( pairings(2, pairings(4,1:nAssoc)==2) ) = 1;
BaseType( pairings(3, pairings(4,1:nAssoc)==2) ) = 1;
BaseType( pairings(2, pairings(4,1:nAssoc)==2)+totImage ) = 1;
BaseType( pairings(3, pairings(4,1:nAssoc)==2)+totImage ) = 1;
CatType = [ ones(1,halfCat*2), ones(1,halfCat*2)*2, ones(1,halfCat*2)*3 ];
CatType = [ CatType, CatType ];

% Create a cell string array with all image and lure file names
all_type = num2str( ones(totImage*2,1) );
all_type = cellstr(all_type);
all_labels = [ 1:totImage, 1:totImage ];
for i = 1:totImage
    all_type{i} = images{i};
    all_type{i+totImage} = lures{i};
end;
% Randomly shuffle the order in which the images are displayed
ind = randperm(totImage*2);
% Set the trials to loop through (for ease of demonstration and debugging
% purposes)
st_trials = 1;
end_trials = totImage*2;
% end_trials = 3;
% Set the condition, block, and state for the results log
expCond = 1;
log_state = 1;

% Vector to store any images to be repeated
repeat = [];
Familiarization % Call separate Matlab script

DrawFormattedText(window, 'Re-study phase', 'center', 'center', [ 0 0 0 ] );
Screen('Flip', window);
WaitSecs(1);

% Reshow any images that the subject indicates they were not familiar with
Repetition % Call separate Matlab script

%%% Word-picture associations study/test for targets %%%
% Lookup - 07

% Randomly shuffle the order in which the associations are displayed
ind = randperm(nAssoc);
% Set the trials to loop through (for ease of demonstration and debugging
% purposes)
st_trials = 1;
end_trials = halfCat;
% end_trials = 3;
% Additional variable declarations for conditional statements and the log
image_type = 2;
ImageType = ones(1,nAssoc); % Only targets
BaseType = pairings(4, 1:nAssoc)-1;
CatType = pairings(5, 1:nAssoc); % Only targets
% blck = 1;
% Loop through blocks of halfCat trials each
for blck = 1:3
    
    if (blck==1)
        %%% Instructions %%%
        % Lookup - Instruct 2

        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'Task 2: Studying words and pictures';
        instruct{2} = 'Next you will learn to associate unrelated words with ';
        instruct{3} = 'some of the pictures you just viewed. A word will be ';
        instruct{4} = 'shown at the same time as a picture and your goal is ';
        instruct{5} = 'to study them together. This is in preparation for ';
        instruct{6} = 'a later memory test that will ask you to remember ';
        instruct{7} = 'the details of the picture when shown the word. To ';
        instruct{8} = 'help you memorize the association between word and ';
        instruct{9} = 'picture, imagine the word and picture in an interactive ';
        instruct{10} = 'way. For example, if the picture is a flag pole and the ';
        instruct{11} = 'word is ‘spoon’ imagine raising a spoon up the flag pole.';
        instruct{12} = 'Press any key to continue';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
    
    % Set the block number
    numBlock = blck;
    
    % Association training
    DrawFormattedText(window, ['Block ', num2str(blck), ': Task 2'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Associations % Call separate Matlab script
    
    if (blck==1)
        %%% Instructions %%%
        % Lookup - Instruct 3
        
        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        instruct{1} = 'Task 3: Remembering picture labels and picture details';
        instruct{2} = 'Next, your memory for the pictures will be tested in ';
        instruct{3} = 'two different ways. First, you will see one of the ';
        instruct{4} = 'unrelated words that was studied along with a picture. ';
        instruct{5} = 'Try to remember the picture and give the appropriate ';
        instruct{6} = 'label for that picture. If you can remember the ';
        instruct{7} = 'picture but forget its label, just come up with ';
        instruct{8} = 'your own label for the picture. Your answer ';
        instruct{9} = 'should be spoken clearly into the microphone. ';
        instruct{10} = 'Press any key to continue';
        lenIns = length(instruct);

        % Display the instructions
        displayInstruct % Call separate Matlab script

        % Define additional instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        instruct{1} = 'Second, your memory for the details of the picture will be ';
        instruct{2} = 'tested. Two very similar pictures will appear and you ';
        instruct{3} = 'should decide which one was studied with the word. For ';
        instruct{4} = 'instance, after seeing the word ‘spoon’ and attempting ';
        instruct{5} = 'to come up with the label ‘flag pole’, you would see ';
        instruct{6} = 'pictures of two different flag poles (the one that ';
        instruct{7} = 'was studied with ‘spoon’ versus a different flag pole). ';
        instruct{8} = 'Press "j" to pick the picture on the left, and "k" ';
        instruct{9} = 'to pick the image on the right . The correct choice ';
        instruct{10} = 'will then be outlined in green. Your memory for ';
        instruct{11} = 'each of the previously studied pictures will ';
        instruct{12} = 'be tested twice in this manner.';
        instruct{13} = 'Press any key to continue';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
        
    % First test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 2;
    ImageNum = pairings(2, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Task 3 (a)'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Training_test % Call separate Matlab script
    
    % Second test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 3;
    ImageNum = pairings(2, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Task 3 (b)'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Training_test % Call separate Matlab script
    
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%%% Word-picture associations study/test for competitors %%%
% Lookup - 08

% Randomly shuffle the order in which the associations are displayed
ind = randperm(nAssoc);
% Set the trials to loop through (for ease of demonstration and debugging
% purposes)
st_trials = 1;
end_trials = halfCat;
% end_trials = 3;
% Additional variable declarations for conditional statements and the log
image_type = 3;
ImageType = ones(1,nAssoc)*2; % Only competitors
BaseType = pairings(4, 1:nAssoc)-1;
CatType = pairings(6, 1:nAssoc); % Only competitors
% blck = 1;
% Loop through blocks of halfCat trials each
for blck = 1:3
    
	if (blck==1)
        %%% Instructions %%%
        % Lookup - Instruct 4

        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'Repeat of Task 2: Studying words and pictures';
        instruct{2} = 'You will now see a new list of unrelated words and ';
        instruct{3} = 'pictures. This list will use the same words as before, ';
        instruct{4} = 'but now those words will be paired with a new picture. ';
        instruct{5} = 'As before, your goal is to memorize the association ';
        instruct{6} = 'between the word and picture. For instance, if you ';
        instruct{7} = 'previously memorized the word ‘spoon’ and a picture of ';
        instruct{8} = 'of a flag pole, you might now be asked to memorize ';
        instruct{9} = 'the word ‘spoon’ and a picture of an airport. ';
        instruct{10} = 'Press any key to continue';
        lenIns = length(instruct);

        % Display the instructions
        displayInstruct % Call separate Matlab script

        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'As before, you should create an interactive image to aid';
        instruct{2} = 'your memory (e.g., imagine the airport runway littered ';
        instruct{3} = 'with spoons). It is important that you create a new ';
        instruct{4} = 'image that does not include the previous picture ';
        instruct{5} = '(e.g., one that does not include a flag pole) because ';
        instruct{6} = 'later on in the experiment you will be asked separately ';
        instruct{7} = 'about the first picture you learned versus the second ';
        instruct{8} = 'picture.';
        instruct{9} = 'Press any key to continue';
        lenIns = length(instruct);

        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
        
    % Set the block number
    numBlock = blck;
    
    % Association training
    DrawFormattedText(window, ['Block ', num2str(blck), ': Repeat of Task 2'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Associations % Call separate Matlab script
    
    if (blck==1)
        %%% Instructions %%%
        % Lookup - Instruct 5
        
        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        instruct{1} = 'Repeat of Task 3: Remembering picture labels and picture details';
        instruct{2} = 'As before, your memory will now be tested in two different ';
        instruct{3} = 'ways by first asking you to say out loud the label of ';
        instruct{4} = 'the picture that was studied with the word and then ';
        instruct{5} = 'asking you to indicate which of two very similar pictures ';
        instruct{6} = 'was the one that was studied. This memory test is exclusively ';
        instruct{7} = 'on the second set of pictures learned with each word ';
        instruct{8} = '(e.g., remember ‘airport’ in response to the word ''spoon'' ';
        instruct{9} = 'rather than ''flag pole''). Unlike the memory test for the ';
        instruct{10} = 'first set of pictures, your memory for this second set ';
        instruct{11} = 'of pictures will only be tested once.';
        instruct{12} = 'Press any key to continue';
        lenIns = length(instruct);

        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
        
    % Single test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 4;
    ImageNum = pairings(3, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Repeat of Task 3'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Training_test % Call separate Matlab script
        
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%%% Rest interval %%%
% Lookup - Break

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'You''ve finished 3 of the 5 tasks! If you want, ';
instruct{2} = 'take a few moments to stand up and stretch before ';
instruct{3} = 'continuing with the rest of the experiment.';
instruct{4} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Instructions %%%
% Lookup - Instruct 6

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Task 4: Imagination and classification of the first set of pictures';
instruct{2} = 'This task will test your memory for the FIRST set of ';
instruct{3} = 'pictures in a different manner than before. As before, the ';
instruct{4} = 'associated word will appear. However, before you give ';
instruct{5} = 'any response, your task is to create as strong a visual ';
instruct{6} = 'image as possible in your mind’s eye. Fill in all the ';
instruct{7} = 'specific visual details that you can (e.g., the size, color, ';
instruct{8} = 'angle, etc.) for this imagination of the picture. You ';
instruct{9} = 'will be given several seconds to imagine the picture. ';
instruct{10} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define additional instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'Immediately after this time to imagine, you will be asked ';
instruct{2} = 'to classify what kind of thing the picture is. You might ';
instruct{3} = 'not have realized it, but all of the pictures can be ';
instruct{4} = 'classified as being a Face, Object, or Scene. Sticking ';
instruct{5} = 'with our original example, the word ‘spoon’ was first studied ';
instruct{6} = 'with the picture of a flag pole and so the correct answer ';
instruct{7} = 'would be Object. You may be tempted to say Scene because ';
instruct{8} = '''spoon'' was also studied with a picture of an airport, ';
instruct{9} = 'but this would be an incorrect response. You can ';
instruct{10} = 'also indicate that you don’t remember.';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define additional instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'When it is time to classify the first picture associated ';
instruct{2} = 'with each word, you will see on the screen: ';
instruct{3} = 'F - O - S - ?';
instruct{4} = 'Which stands for Face, Object, Scene, and don’t remember. ';
instruct{5} = 'These 4 responses are indicated by the "j", "k", "l", and ";" ';
instruct{6} = 'to indicate your respective choice. Once you make your ';
instruct{7} = 'choice, the correct response will flash green. Please ';
instruct{8} = 'try to make these responses quickly so that you have ';
instruct{9} = 'time to learn the correct response, should you make ';
instruct{10} = 'an error.';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Selective retrieval %%%
% Lookup - 09

% Shuffle the presentation of the non-baseline
% cue words, and repeat each cue word 4 times
ind2 = 1:nAssoc;
sel = pairings(4,1:nAssoc) == 1;
ind2 = [ ind2(sel) ind2(sel) ind2(sel) ind2(sel) ];
ind2 = ind2(randperm((nAssoc-nBaseline)*4));

% Additional variable declarations for logging the results
ImageType = pairings(4,ind2);
ImageNum = pairings(2, ind2 ) + pairings(3, ind2 )/100;
BaseType = zeros(1,length(ind2));
CatType = pairings(5, ind2 ) + pairings(6, ind2 )/10;

% Create a record of how many times a cue word had already been
% repeated in a given trial
repetition = zeros(1,length(ind2));
temp = pairings(1, ind2);
temp2 = 1:nAssoc;
for i = temp2(sel)
    repetition( temp==pairings(1,i) ) = 1:4;
end;

% Additional variable declarations for the conditional statements
st_trials = 1;
end_trials = (nAssoc-nBaseline)*4;
% end_trials  = 3;
log_state = 3;
expCond = 5;

Selective_recall % Call separate Matlab script

%%% Instructions %%%
% Lookup - Instruct 7

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Task 5: Remembering picture details of both the first and second set of pictures';
instruct{2} = 'In this final phase, your memory for the picture details ';
instruct{3} = 'will be tested in the same manner as before, by choosing ';
instruct{4} = 'between two very similar pictures. Unlike before, you will ';
instruct{5} = 'not see the associated words and you will not be asked ';
instruct{6} = 'to say the label of the picture. Your memory for both the ';
instruct{7} = 'first and second set of pictures will be tested in ';
instruct{8} = 'this manner. You will only be given a short amount of ';
instruct{9} = 'time for each decision, so try to choose the correct ';
instruct{10} = 'picture quickly.';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Final recognition task %%%
% Lookup - 10

% Display all target and competitor images (for both the baseline and 
% non-baseline conditions), but shuffle the presentation order
ind = randperm(nAssoc*2);
image_type = [ ones(1,nAssoc)*2, ones(1,nAssoc)*3 ];
image_type = image_type(ind);
ind2 = [ 1:nAssoc, 1:nAssoc ];
ind2 = ind2(ind);

% Additional variable declarations for logging the results
ImageNum = ones(1,nAssoc*2);
ImageNum( image_type == 2 ) = pairings( 2, ind2( image_type == 2 ) );
ImageNum( image_type == 3 ) = pairings( 3, ind2( image_type == 3 ) );
BaseType = zeros(1,length(ind2));
BaseType( image_type == 2 ) = pairings( 4, ind2( image_type == 2 ) );
BaseType( image_type == 3 ) = pairings( 4, ind2( image_type == 3 ) );
BaseType = BaseType - 1;
CatType = zeros(1,length(ind2));
CatType( image_type == 2 ) = pairings( 5, ind2( image_type == 2 ) );
CatType( image_type == 3 ) = pairings( 6, ind2( image_type == 3 ) );

% Additional variable declarations for the conditional statements
st_trials = 1;
end_trials = nAssoc*2;
log_state = 4;
expCond = 6;

Final_recognition % Call separate Matlab script

%%% End of experiment %%%
% Lookup - 11

endTime = clock;

%%% Instructions %%%
% Lookup - Instruct 8

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Congratulations! You''ve finished the experiment. ';
instruct{2} = 'If you have any questions, feel free to contact the ';
instruct{3} = 'experimenter in room 202. Additional contact information ';
instruct{4} = 'is provided on the debriefing form. Thanks for your ';
instruct{5} = 'participation!';
instruct{6} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Write the log of the results to a .csv file
cd('Subjects')
fileID = fopen(csvOutputFile,'w');
for ln = 1:length(log_file)
    fprintf( fileID, [ char( log_file{ln} ), char(10) ] );
end;
fclose(fileID);
% Save key Matlab objects
save(matOutputFile,'pairings','log_file','startTime','endTime');

% Return to the original directory
cd(orig_dir);

% Clear the screen.
sca;

% Clear the workspace
% clear;
% exit