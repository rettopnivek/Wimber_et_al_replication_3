%-------------------------------------------%
% Wimber et al (2015) replication version 2 %
% Kevin Potter and David Huber              %
% Updated 06/16/2015                        %
%-------------------------------------------%

%{
Purpose:
A Matlab script to run a second version of a replication of the 
experimental design used in Wimber, Alink, Charest, Kriegeskorte, 
and Anderson (2015). This script runs the training part of the 
experiment, before the context change.

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
- We used the full stimulus set for this replication, but we balance
  the number of baseline and target items to be equal.
- The experiment can be run using the batch file 'runexeperiment.bat'.
- To prematurely end the experiment, press shift + 2.
- linspace(a,b,n) generates a sequence of n values between a and b
- Thanks is extended to Will Hopper and Angela Nelson Lowe for help with
  the code in Matlab
- Run the Test1a.m file first on certain machines to make sure Psychtoolbox
  is working.
- Use the key shortcut Win+P to change from dual to single monitor (Win is
  the key with the Windows logo).

%%% TO DO %%%
- Flesh out/correct indices
- Make sure directory pathways are correct (also for the .bat files)
%}

% Load in useful functions
addpath('F:\Stuff\Kevin''s Stuff\Postdoc\Wimber_et_al_2015_rep2')
% addpath('C:\Users\lab\Documents\MATLAB\Wimber_et_al_2015_rep2')
% cd('F:\Stuff\Kevin''s Stuff\Postdoc\Wimber_et_al_2015_rep2')

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
Lookup - 06: Familiarization phase
Lookup - 07: Word-picture associations study/test for targets
Lookup - 08: Word-picture associations study/test for competitors
Lookup - 11: End of experiment
Lookup - Instruct 8
%}

%-----------------------%
% Variable declarations %
%-----------------------%

%%% Input subject ID number for later output files %%%
% Lookup - 00
Session = '_train';
SubjectIDInput % Call separate Matlab script

% Convert the ID number from a string to a number for later indexing
SubjNum = [];
for i = 9:11
    if ( isempty(str2num(csvOutputFile(i)))==0 )
        if ( real( str2num(csvOutputFile(i) ) ) > 0 )
            SubjNum = [ SubjNum csvOutputFile(i) ];
        end;
    end;
end;
SubjNum = str2num( SubjNum );

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

% Load in a matrix of rng seeds in order to pair the training and test
% sessions
% load('RandomMat.mat')

% Set the total number of images for each category
nCat = 48;
% Set the total number of images (and therefore also lures)
totImage = nCat*3;
% Divide by two to get the number of target and competitor images
halfCat = nCat/2;

% The target and competitors need to be randomly paired such that the
% competitor comes from a separate category than the target

% First, randomly divide each category into first and second sets
% (i.e. targets and competitors)

% Set the permutations to be equal across training and test sessions
rng(666)
RandomMat = randi(1000,100,9);

rng( RandomMat(SubjNum,1) );
sel = randperm(nCat);
first = sel( 1:(halfCat) );
second = sel( (halfCat+1):(nCat) );

rng( RandomMat(SubjNum,2) )
sel = randperm(nCat);
first = [ first, sel( 1:(halfCat) ) + nCat ];
second = [ second, sel( (halfCat+1):(nCat) ) + nCat ];

rng( RandomMat(SubjNum,3) )
sel = randperm(nCat);
first = [ first, sel( 1:(halfCat) ) + nCat*2 ];
second = [ second, sel( (halfCat+1):(nCat) ) + nCat*2 ];

% Next reorganize the second set so that the
% categories don't match between sets
sel = [ (halfCat + 1):(halfCat*2), (halfCat*2 + 1):(halfCat*3) ];
rng( RandomMat(SubjNum,4) )
srt = randperm(halfCat*2);
sel = sel(srt);

temp = sel(1:halfCat);
sel = sel( (halfCat + 1):(halfCat*2) );
rem = sel( sel < (halfCat*2 + 1) );

sel = sel( sel > (halfCat*2) );
l = halfCat-length(sel);
rng( RandomMat(SubjNum,5) )
sel2 = randperm(halfCat);
rng( RandomMat(SubjNum,6) )
srt = randperm(halfCat);
sel = [ sel, sel2(1:l) ];
sel = sel(srt);
temp = [ temp, sel ];
sel = [ rem, sel2( (l+1):halfCat ) ];

rng( RandomMat(SubjNum,7) )
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
% rng( RandomMat(SubjNum,8) )
cond = cond( randperm(length(cond)) );

% Create an index of which category to which each image belongs
correctCategory = [ ones(1,nCat), ones(1,nCat)*2, ones(1,nCat)*3 ];

% Combine all the indices into a single matrix for easy referencing
rng( RandomMat(SubjNum,9) )
pairings = [ randperm(nAssoc); first; second(temp); cond; correctCategory(first); correctCategory(second(temp)) ];
% Row 1: Index for the word cue
% Row 2: Image number for targets
% Row 3: Image number for competitors
% Row 4: Conditions, where Target/Competitor = 1, Baseline  = 2
% Row 5: Categories for targets, where Faces = 1, Objects = 2, Scenes = 3
% Row 6: Categories for competitors, where Faces = 1, Objects = 2, Scenes = 3

% Clear up workspace
clear sel srt temp first second cond rem correctCategory l sel2;

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

3 blocks of 24 images each
    Task 2: Word-picture association (Targets)
    Task 3: Recall and recognition (Targets; 2 cycles)
3 blocks of 24 images each
    Task 2: Word-picture association (Competitors)
    Task 3: Recall and recognition (Competitors; 1 cycle)
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
instruct{1} = 'Building mental images of faces, places & objects';
instruct{2} = ' ';
instruct{3} = 'In this experiment, we are trying to replicate the ';
instruct{4} = 'behavioral results of a recent and innovative brain imaging ';
instruct{5} = 'study. You will be completing several different memory tasks ';
instruct{6} = 'involving pictures and words. There will be 5 different tasks. ';
instruct{7} = 'You will complete 3 tasks in this room, and then you will be ';
instruct{8} = 'taken upstairs to complete the final 2 tasks. ';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'The experiment should take a total of 2 hours. You will earn ';
instruct{2} = '5 dollars every half hour, for a total of 20 dollars by the end. ';
instruct{3} = 'The manner in which you give answers will change for different ';
instruct{4} = 'memory tests, so please read the detailed instructions shown ';
instruct{5} = 'on the screen before each test. Memorizing can be dull and ';
instruct{6} = 'difficult, but please try your best. We appreciate your effort!';
instruct{7} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Instructions %%%
% Lookup - Instruct 1

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Familiarization';
instruct{2} = ' ';
instruct{3} = 'The first part will take approximately 15 minutes. In this ';
instruct{4} = 'part, I will familiarize you with the different kinds of ';
instruct{5} = 'pictures that will later be used for the imagery part, and';
instruct{6} = 'make sure that you recognize each picture as what it is. ';
instruct{7} = 'Specifically, you will be shown pictures of well-known ';
instruct{8} = 'faces, places, and objects. Over the course of this ';
instruct{9} = 'familiarization phase, you will see two different versions';
instruct{10} = 'of each picture. ';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Each picture will first appear alone on the screen, followed ';
instruct{2} = 'by a label describing the content of the picture. Your task ';
instruct{3} = 'is to look at each picture carefully, and indicate with a ';
instruct{4} = 'button press whether you could have recognized the ';
instruct{5} = 'corresponding person, place, or object ';
instruct{6} = 'without the label. Please press the "j" key with';
instruct{7} = 'your index finger to indicate that you would have ';
instruct{8} = 'recognized the picture without the label, and the "k" ';
instruct{9} = 'key with your middle finger to indicate that you would ';
instruct{10} = 'not have recognized the picture without the label. ';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'You will likely recognize many of the images, but some might ';
instruct{2} = 'be unknown to you, or it might not be immediately clear what ';
instruct{3} = 'is being depicted. If you indicate that you cannot identify';
instruct{4} = 'a particular picture, it will be shown for a second time ';
instruct{5} = 'towards the end of the familiarization phase.';
instruct{6} = 'Press any key to continue';
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
WaitSecs(1.5);

% Reshow any images that the subject indicates they were not familiar with
Repetition % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Training of word-picture associations I';
instruct{2} = ' ';
instruct{3} = 'The main part of the experiment (approximately 1 hour) is ';
instruct{4} = 'dedicated to train you up for the final set of tasks that ';
instruct{5} = 'will be run upstairs. These tasks are typically used in ';
instruct{6} = 'brain imaging studies, in which participants visualize pictures, ';
instruct{7} = 'with the goal being to predict, merely based on their brain ';
instruct{8} = 'activity, which picture they have in mind. ';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'In the next ~60 min, we will therefore train you on ';
instruct{2} = 'associations between words and pictures, such that after ';
instruct{3} = 'the training, when you are presented with a specific word, ';
instruct{4} = 'you will be able to produce a vivid mental image of the ';
instruct{5} = 'corresponding picture. This phase is separated into several ';
instruct{6} = 'blocks, with each block training you on approximately 25 ';
instruct{7} = 'word-picture associations. Initially, you will see one word ';
instruct{8} = 'and one picture at a time, presented together for 4 sec each. ';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

instruct{1} = 'Your task is to form a visual association between the two so ';
instruct{2} = 'that when given the word, you can visualize the picture. ';
instruct{3} = 'Because later you will be asked to fully visualize the ';
instruct{4} = 'specific picture that goes together with the word, the ';
instruct{5} = 'strategy I am asking you to use is to take the picture on ';
instruct{6} = 'the screen, and use the meaning of the word to mentally ';
instruct{7} = 'transform the picture (e.g., make it move, or add something ';
instruct{8} = 'to it). For example, you might see the word “jump” and the ';
instruct{9} = 'face of the Dalai Lama, so you could imagine the Dalai Lama’s ';
instruct{10} = 'ears or his glasses jumping in the picture. ';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'After this initial training, you will be shown words alone, ';
instruct{2} = 'and your task is to visualize the corresponding picture and ';
instruct{3} = 'name it aloud as quickly as possible so I can verify that ';
instruct{4} = 'you have the correct image in mind. A few seconds later, two ';
instruct{5} = 'different versions of the correct picture will appear on ';
instruct{6} = 'the screen, and your task is to indicate, via button press, ';
instruct{7} = 'which of the two versions was paired with the word (index ';
instruct{8} = 'finger = left, middle finger = right). ';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'A green frame will then give you feedback about the correct ';
instruct{2} = 'choice. Use this feedback to memorize some specific detail ';
instruct{3} = 'that distinguishes the two pictures, because you will later ';
instruct{4} = 'be asked to visualize this specific picture, and I will ';
instruct{5} = 'also ask you, at the very end of the experiment, which one ';
instruct{6} = 'of the two similar pictures had been paired with a word. ';
instruct{7} = 'After each word-picture pair has been tested twice, you ';
instruct{8} = 'will be trained on a new set of word-picture pairs.';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

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
    
    % Set the block number
    numBlock = blck;
        
    % Association training
    DrawFormattedText(window, ['Block ', num2str(blck), ': Training'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1.5);
    
    if (blck==1)
        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'You will now see the set of words and images you need ';
        instruct{2} = 'to associate together. ';
        instruct{3} = 'Press any key to continue';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
    
    Associations % Call separate Matlab script
        
    % First test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 2;
    ImageNum = pairings(2, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Testing (a)'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1.5);
    
    if (blck==1)
        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'You will now be tested on the words and images you ';
        instruct{2} = 'were trained to associate together. First, you will ';
        instruct{3} = 'see the words and you must say out loud the label of ';
        instruct{4} = 'the associated image (during this, try to visualize ';
        instruct{5} = 'the image as much as possible). Then, you will see ';
        instruct{6} = 'two versions of each image, and you must pick the one ';
        instruct{7} = 'you associated with the word.';
        instruct{8} = 'Press any key to continue';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
    
    Training_test % Call separate Matlab script
    
    % Second test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 3;
    ImageNum = pairings(2, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Testing (b)'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1.5);
    Training_test % Call separate Matlab script
    
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%%% Word-picture associations study/test for competitors %%%
% Lookup - 08

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Training of word-picture associations II';
instruct{2} = ' ';
instruct{3} = 'The next part will take about 20 minutes, and is very similar ';
instruct{4} = 'to the one you have just finished. The task will, however, ';
instruct{5} = 'become slightly more challenging for two reasons. First, ';
instruct{6} = 'you will use words that you have already linked to a ';
instruct{7} = 'different picture before, and will link these to a new ';
instruct{8} = 'picture (one that hasn’t been linked to any word yet). ';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Even though you have previously linked the same words to a ';
instruct{2} = 'different picture, it is very important that you come up ';
instruct{3} = 'with a new, fresh mental image for each word-picture ';
instruct{4} = 'pairing. Please do not form any links between the new ';
instruct{5} = 'picture and the one that had been paired with the same word ';
instruct{6} = 'before! The new image should be treated separately from the ';
instruct{7} = 'first, and be a fresh new effort on your part. ';
instruct{8} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'After each series of 24 pairs, you will be assessed ';
instruct{2} = 'on your ability to visualize each picture in response to ';
instruct{3} = 'the hint word, and you will get feedback on the correct ';
instruct{4} = 'answer. Again, please use this feedback to find a detail ';
instruct{5} = 'that helps you to distinguish which of the two pictures was ';
instruct{6} = 'linked with the word, because you will need that information ';
instruct{7} = 'in the future tasks. Second, the task will also be slightly ';
instruct{8} = 'more challenging because there will only be one round ';
instruct{9} = '(instead of two rounds) of feedback for each set of ';
instruct{10} = 'word-picture pairs. ';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

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
    
    % Set the block number
    numBlock = blck;
    
    % Association training
    DrawFormattedText(window, ['Block ', num2str(blck), ': Training'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    Associations % Call separate Matlab script
            
    % Single test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    log_state = 2;
    expCond = 4;
    ImageNum = pairings(3, ind2);
    
    DrawFormattedText(window, ['Block ', num2str(blck), ': Testing'], 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    WaitSecs(1);
    
    if (blck==1)
        % Define the instructions
        instruct = num2str( ones(2,1) ); % Create a cell string array
        instruct = cellstr(instruct);
        % Fill the array with the individual lines of instructions
        instruct{1} = 'Once again, you will be tested on the words and ';
        instruct{2} = 'images you were trained to associate together. Recall ';
        instruct{4} = 'that you must say out loud the label of the associated ';
        instruct{5} = 'image and visualize it as well. Then, you will see ';
        instruct{6} = 'two versions of each image, and you must pick the one ';
        instruct{7} = 'you associated with the word.';
        instruct{8} = 'Press any key to continue';
        lenIns = length(instruct);
        
        % Display the instructions
        displayInstruct % Call separate Matlab script
    end;
    
    Training_test % Call separate Matlab script
        
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%%% End of Training %%%
% Lookup - End

endTime = clock;

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
instruct{1} = 'You''ve finished 3 of the 5 tasks! The experimenter will ';
instruct{2} = 'now take you upstairs to complete the remaining 2 tasks. ';
instruct{3} = 'Press any key to continue';
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

% Clear the screen
clear Screen;
% clear all;
sca;