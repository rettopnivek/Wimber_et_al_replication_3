%--------------------------------------------%
% Wimber et al (2015) replication version 3  %
% Kevin Potter, Luke Huszar, and David Huber %
% Updated 05/07/2016                         %
%--------------------------------------------%

%{
Purpose:
A Matlab script to run a third version of a replication of the
experimental design used in Wimber, Alink, Charest, Kriegeskorte,
and Anderson (2015).

Requirements:
Matlab R2013a
Psychtoolbox
Folder labeled 'Images' (with balanced number of image files)
Text file of cues; 'Cue_words.txt'
Text file of image labels; 'Labels.txt'

Outputs:
A .csv file with the response and condition information
A .csv file with the stimulus information
A .mat file with the starting and ending time, and the original responses
and stimulus information

Notes:
- The experiment can be run using the batch file 'runexeperiment.bat'.
- To prematurely end the experiment, press shift + 2.
- Thanks is extended to Will Hopper and Angela Nelson Lowe for help with
  the code in Matlab
- Thanks is extended to Maria Wimber for providing the stimuli and
  the instructions of the task
- Run the Test1a.m file first on certain machines to make sure Psychtoolbox
  is working.
- Use the key shortcut Win+P to change from dual to single monitor (Win is
  the key with the Windows logo).

%%% TO DO %%%
- Check key assignments in instructions
- Add pictures/notes to instructions?

Index
Lookup - 01:  Pre-experiment setup
Lookup - 02:  Initialize variables for stimulus presentation
Lookup - 03:  Image assignment, word cues, and file names
Lookup - 04:  Experiment design
Lookup - 05:  Familiarization phase
Lookup - 06:  Word-picture associations for targets
Lookup - 07:  Word-picture associations for competitors
Lookup - 08:  Selective retrieval
Lookup - 09:  Final recognition test
Lookup - 10:  End of experiment
%}

%----------------------%
% Pre-experiment setup %
%----------------------%
% Lookup - 01

% Set debug state (0 = normal subject, >= 1 for debugging )
debug = 1;
% Set the parameters to simulate responses in the debugging state
robotParFam = [ .95, 1, 1 ]; % Familiarization phase
robotParTest = [ .8, 1, 1 ]; % Training - test phase
robotParSR = [ .8, .1, .05, .6, .6 ]; % Selective retrieval phase
robotParFRT = [ .84, 1, 1 ]; % Final recognition test

% Save the current working directory
orig_dir = pwd;
% Add the current directory to the path to ensure internal functions can
% be called
pathToFunctions = orig_dir;
addpath(pathToFunctions);

% Record when the experiment begins
startTime = clock;

% Determine output filenames
if debug < 2
    Subject_ID_input
else
    csvOutputFile{1} = 'Subject_96.csv';
    matOutputFile{1} = 'Subject_96.mat';
    stimOutputFile{1} = 'Subject_96_Stim.csv';
end;

% Convert the ID number from a string to a number for later indexing
for i = 9:12
    if ( csvOutputFile{1}(i)=='.' )
        SubjNum = str2num( csvOutputFile{1}(9:(i-1)) );
    end
end;

if debug == 0
    % Determine demographics info based on NIH standards
    Demographics
    % Rename the demographics file
    movefile('Demographics.txt',demographicsFile{1});
    % Move the file to the subjects folder
    movefile(demographicsFile{1},'Subjects');
end;

% Read in the text file of pre-existing RNG seeds
fileID = fopen('RNG_seeds.txt','r');
RNG_seeds = fscanf(fileID,'%d' );
fclose(fileID);
rng( RNG_seeds(SubjNum) ); % Set seed based on current subject
% rng shuffle; % Random seed for rng

% Initialize Psychtoolbox
PTB_initial

% Hide the mouse curser
HideCursor();

%------------------------------------------------%
% Initialize variables for stimulus presentation %
%------------------------------------------------%
% Lookup - 02

% Set the line length for the fixation cross (in pixels)
fixCrossDimPix = 20;
% Set the x and y coordinates for the fixation cross, where (0,0) is the
% center of the screen
fixAllCoords = ones(2,4);
fixAllCoords(1,1:4) = [-fixCrossDimPix fixCrossDimPix 0 0];
fixAllCoords(2,1:4) = [0 0 -fixCrossDimPix fixCrossDimPix];
% Set the line width for our fixation cross
fixLineWidthPix = 2;

% Clean up workspace
clear fixCrossDimPix;

%---------------------------------------------%
% Image assignment, word cues, and file names %
%---------------------------------------------%
% Lookup - 03

% Scale images to all be the same height (25 percent of the screen height)
heightScale = 0.25;
imageHeight = screenYpixels * heightScale;

% Load in matrix with image assignments to the target, competitor, and
% baseline conditions. The script 'Create_balanced_target_assignment.m'
% generates this matrix
load('Image_cue_assignment.mat')
% Number of images per category
nCat = 48;
% Total number of images
totImage = nCat*3;
% Number of images per target/competitor conditions
nAssoc = totImage/2;
% Split number of images in each category in half
halfCat = nCat/2;

% Extract assignments specific to current subject
pairings = all_pairings( :, all_pairings( 7, : ) == SubjNum );
% Number of baseline images
nBaseline = sum( pairings(4,:) - 1 );

% Extract word cues and image labels
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

% File names for images and lures
% Set the category names
categoryName = { 'F_'; 'O_'; 'S_' };
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
    images{i} = { [ categoryName{st} 'Image_' num2str(i) '.bmp' ] };
    lures{i} = { [ categoryName{st} 'Lure_' num2str(i) '.bmp' ] };
end;

% Clean up workspace
clear( 'categoryName', 'i', 'st' );

%-------------------%
% Experiment design %
%-------------------%
% Lookup - 04

%{
Task 1: Familiarization phase with all images and lures

3 blocks of 24 images each
    Task 2: Word-picture association (Targets)
    Task 3: Recall and recognition (Targets; 2 cycles)
3 blocks of 24 images each
    Task 2: Word-picture association (Competitors)
    Task 3: Recall and recognition (Competitors; 1 cycle)
%}

% Set the keys for picking the four category options
keyOptions = getKeyAssignments('jkl;',1,300);
% Initialize the current trial value
curTrial = 0;
% Set spacing for formatted text
lnSpace = 1.5;

% Set timing for the stimuli
if debug == 0
    % General settings
    InstructionTime = 1;
    % Familiarization settings
    FamImageTime = 1;
    FamLabelTime = 1.5;
    FamTimeOut = 9.995;
    % Association settings
    AssocInterStimGap = .5;
    AssocLearnTime = 4;
    % Training - test settings
    TrainLearnTime = 4;
    TrainInterStimGap = 1;
    TrainTimeOut = 3.4995;
    TrainTestLearnTime = 1;
    % Selective Retrieval
    SelRetCueTime = 4;
    SelRetTimeOut = 1.5;
    InterStimGapSR = 1;
    SelRetTimeOut2 = 0.9995;
    % Final test phase
    FRTInterStimGap = 1;
    FRTTimeOut = 3.4995;
else
    % General settings
    InstructionTime = .1;
    % Familiarization settings
    FamImageTime = .01;
    FamLabelTime = .01;
    % Association settings
    AssocInterStimGap = .01;
    AssocLearnTime = .01;
    % Training - test settings
    TrainLearnTime = .01;
    TrainInterStimGap = .01;
    TrainTestLearnTime = .01;
    % Selective Retrieval
    SelRetCueTime = .01;
    SelRetTimeOut = .2;
    InterStimGapSR = .01;
    SelRetTimeOut2 = 0.1005;
    % Final test phase
    FRTInterStimGap = .01;
end

% Initialize a matrix to store responses and stimulus/condition info
allResp = [];
% Initialize string array to keep track of images/cues/labels being displayed
stimulusInfo{1} = 'Trial,Cond,Block,ImageFiles,Labels,Cues';

%%% Initial Instructions %%%
% Instruct - 01
instruct = [ 'Welcome to the Visual Images and Memory Experiment\n\n' ...
    'In this experiment we are investigating the effect of \n' ...
    'visual images on memory. The experiment will take less \n' ...
    'than 2 hours and for your participation you will receive \n' ...
    '20 dollars.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );


instruct = [ 'There are 5 stages to the experiment:\n\n' ...
    '(1) Picture recognition (~ 10 min)                     \n' ...
    '(2) Learning to relate words with pictures (~ 30 min)  \n' ...
    '(3) Learning to relate the same words with new pictures\n' ...
    '    (~20 min)                                          \n' ...
    '(4) Creating visual images from words (~ 20 min)       \n' ...
    '(5) Picture memory test (~ 10 min)                     \n\n' ...
    'You will receive detailed instructions at the start of \n' ...
    'each stage. If you have any questions, please find the \n' ...
    'experimenter.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

%-----------------------%
% Familiarization phase %
%-----------------------%
% Lookup - 05

%%% First stage intro instructions %%%
% Instruct - 02
instruct = [ 'Stage 1: Picture recognition\n\n' ...
    'One at a time, you will be shown a picture of a face, \n' ...
    'object, or scene. The name of the pictured item will \n' ...
    'appear below the picture. For each picture, you will be \n' ...
    'asked whether you recognize the pictured item without \n' ...
    'reading this name. Press the "j" key if you recognize \n' ...
    'the picture, otherwise press the "k" key.\n\n' ...
    'Two versions of each pictured item will appear and the \n' ...
    'pictured items you could not recognize will be repeated \n' ...
    'at the end.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

% Variable to indicate which image is being shown
ImageNum = [ 1:totImage, 1:totImage ];
% Variable to indicate the type of image being displayed
% ( 0 = Lure, 1 = Target, 2 = Competitor )
ImageType = [ ones(1,totImage), zeros(1,totImage) ];
ImageType( pairings(3, 1:nAssoc) ) = 2;
% Variable to indicate which images have been assigned to the
% baseline condition ( 0 = selective retrieval, 1 = baseline )
BaseType = zeros(1,totImage*2);
BaseType( pairings(2, pairings(4,:)==2) ) = 1;
BaseType( pairings(3, pairings(4,:)==2) ) = 1;
BaseType( pairings(2, pairings(4,:)==2)+totImage ) = 1;
BaseType( pairings(3, pairings(4,:)==2)+totImage ) = 1;
% Variable to indicate the category to which an image belongs
% 1 = Faces, 2 = Objects, 3 = Scenes
CatType = [ ones(1,nCat), ones(1,nCat)*2, ones(1,nCat)*3 ];
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
% Indicate which experimental phase is being run for logging purposes
expCond = 1;

% Vector to store any images to be repeated
repeat = [];
if debug < 2
    Familiarization % Call separate Matlab script
end

%%% First stage repeat instructions %%%
% Instruct - 03
instruct = [ 'Stage 1: Picture recognition\n\n' ...
    'You now have a chance to restudy images \n' ...
    'you could not initially recognize.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

% Reshow any images that the subject indicates they were not familiar with
if debug < 2
    Repetition % Call separate Matlab script
end

%---------------------------------------%
% Word-picture associations for targets %
%---------------------------------------%
% Lookup - 06

% Randomly shuffle the order in which the associations are displayed
ind = randperm(nAssoc);
% Set the trials to loop through (for ease of demonstration and debugging
% purposes)
st_trials = 1;
end_trials = halfCat;
% Variabe to indicate which row of the 'pairings' matrix to use
image_type = 2;
% Variable to indicate which image is being shown
ImageType = ones( 1, nAssoc );
% Variable to indicate which images have been assigned to the
% baseline condition
BaseType = pairings( 4, : ) - 1;
% Variable to indicate the category to which an image belongs
CatType = pairings( 5, : );

% Loop through blocks of halfCat trials each
for blck = 1:3
    
    % Set the block number
    numBlock = blck;
    
    if blck == 1
        %%% Second stage training instructions %%%
        % Instruct - 04
        instruct = [ 'Stage 2: Learning to relate words with pictures\n' ...
            'One at a time, you will see a picture and a word on the \n' ...
            'screen at the same time. Unlike Stage 1, the word will \n' ...
            'be a random word that is unrelated to the pictured item. \n' ...
            'Your task is to create a new association between the \n' ...
            'word and the picture to aid your memory later in the \n' ...
            'experiment.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
        instruct = [ 'The best way to do this is to imagine the picture changed \n' ...
            'in some manner by the word. For example, you might see the \n' ...
            'word "jump" and a picture of the Dalai Lama. To help you \n' ...
            'remember this combination, you could image the Dalai Lama''' 's \n' ...
            'glasses jumping off his face and hopping around.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    end
    
    % Association training
    instruct = [ 'Stage 2: Learning to relate words with pictures (Block ', num2str(blck), ')\n\n' ...
        'Remember that the best way to associate a picture \n' ...
        'and a word is to imagine that the picture is changed \n' ...
        'in some manner by the word.' ];
    [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    
    % Set the indicator for the experimental condition
    expCond = 1.5;
    if debug < 2
        Associations % Call separate Matlab script
    end
    
    % First test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    expCond = 2;
    ImageNum = pairings(2, ind2);
    
    if blck == 1
        %%% Second stage test instructions %%%
        % Instruct - 04
        instruct = [ 'Stage 2: Testing\n\n' ...
            'Next, your memory for the word-picture associations will \n' ...
            'be tested. First, you see a word by itself. As quickly as \n' ...
            'you can, name aloud the picture that was previously \n' ...
            'studied with this word. It is important that you actually \n' ...
            'speak your answer.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
        instruct = [ 'A few seconds later, two different versions of the correct \n' ...
            'picture will appear on the screen. Your task is to indicate \n' ...
            'which version was the one that you studied with the word. \n' ...
            'Press the "j" key to choose the picture on the left and the \n' ...
            '"k" to choose the picture on the right.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
        instruct = [ 'After you make your selection, the correct picture will \n' ...
            'become enclosed in a green box. Take careful note of the \n' ...
            'correct answer, and attempt to memorize the visual \n' ...
            'differences between the two pictures so that you will be \n' ...
            'correct the next time that you make this choice.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
    end;
    
    % Testing stage
    instruct = [ 'Stage 2: Testing (Block ', num2str(blck), 'a)\n\n' ...
        'Remember that when you see the word, speak out loud what \n' ...
        'picture you associated with it. Then when you see the \n' ...
        'two similar pictures, press "j" if you had previously \n' ...
        'studied the one on the left, and "k" if you had studied \n' ...
        'the picture on the right.' ];
    [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    
    if debug < 2
        Training_test % Call separate Matlab script
    end
    
    % Second test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    expCond = 3;
    ImageNum = pairings(2, ind2);
    
    instruct = [ 'Stage 2: Testing (Block ', num2str(blck), 'b)\n\n' ...
        'Remember that when you see the word, speak out loud what \n' ...
        'picture you associated with it. Then when you see the \n' ...
        'two similar pictures, press "j" if you had previously \n' ...
        'studied the one on the left, and "k" if you had studied \n' ...
        'the picture on the right.' ];
    [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    
    if debug < 2
        Training_test % Call separate Matlab script
    end
    
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%-------------------------------------------%
% Word-picture associations for competitors %
%-------------------------------------------%
% Lookup - 07

% Randomly shuffle the order in which the associations are displayed
ind = randperm(nAssoc);
% Set the trials to loop through (for ease of demonstration and debugging
% purposes)
st_trials = 1;
end_trials = halfCat;
% Forthcoming
image_type = 3;
ImageType = ones( 1, nAssoc )*2;
BaseType = pairings( 4, : ) - 1;
CatType = pairings( 6, : );

% Loop through blocks of halfCat trials each
for blck = 1:3
    
    % Set the block number
    numBlock = blck;
    
    % Association training
    if blck == 1
        %%% Third stage training instructions %%%
        % Instruct - 05
        instruct = [ 'Stage 3: Learning to relate the same words with new pictures\n' ...
            'As in Stage 2, you will be asked to relate a word with a \n' ...
            'picture by imagining the picture changed in some way by \n' ...
            'the word. This will be difficult because the same words \n' ...
            'will be used as in Stage 2.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
        instruct = [ 'It is very important that you use each word in a different \n' ...
            'way than before because later in the experiment you will be \n' ...
            'asked to keep separate in your mind the first picture related \n' ...
            'to each word from the second picture. For example, you might \n' ...
            'see the word "jump" and  a picture of spoon. Because you \n' ...
            'should think of jump in a different way than before, this \n' ...
            'time you might imagine the spoon jumping off of a cliff \n' ...
            'rather than hopping around.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    end;
    
    % Association training
    instruct = [ 'Stage 3: Learning to relate the same words with new pictures (Block ', num2str(blck), ')\n\n' ...
        'Remember that the best way to associate a picture \n' ...
        'and a word is to imagine that the picture is changed \n' ...
        'in some manner by the word. But make sure to imagine \n' ...
        'something different compared to the first pictures in Stage 2.' ];
    [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    
    % Set the indicator for the experimental condition
    expCond = 3.5;
    if debug < 2
        Associations % Call separate Matlab script
    end
    
    % First test cycle
    ind2 = ind(st_trials:end_trials);
    ind2 = ind2( randperm( length(ind2) ) );
    expCond = 4;
    ImageNum = pairings(3, ind2);
    
    if blck == 1
        %%% Third stage test instructions %%%
        % Instruct - 06
        instruct = [ 'Stage 3: Testing\n\n' ...
            'As in Stage 2, now your memory for the word-picture \n' ...
            'associations will be tested. For each word, name aloud \n' ...
            'the picture that you just learned. You will then be given \n' ...
            'a choice between two versions of this picture and you \n' ...
            'should choose the one you just learned. You will then \n' ...
            'know the correct answer by the appearance of a green box \n' ...
            'and you should use this to memorize the visual differences \n' ...
            'between the two pictures so that you will be correct the \n' ...
            'next time that you make this choice.' ];
        [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
        
    end;
    
    instruct = [ 'Stage 3: Testing (Block ', num2str(blck), ')\n\n' ...
        'Remember that when you see the word, speak out loud what \n' ...
        'picture you associated with it. Then when you see the \n' ...
        'two similar pictures, press "j" if you had previously \n' ...
        'studied the one on the left, and "k" if you had studied \n' ...
        'the picture on the right.' ];
    [nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
    
    if debug < 2
        Training_test % Call separate Matlab script
    end
    
    % Increment indices for next block of trials
    st_trials = st_trials + halfCat;
    end_trials = end_trials + halfCat;
end;

%---------------------%
% Selective retrieval %
%---------------------%
% Lookup - 08

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
expCond = 5;

instruct = [ 'Stage 4: Creating visual images from words\n\n' ...
             'Because this experiment concerns the effect of visual \n' ...
             'images on memory, it is very important that you create \n' ...
             'vivid detailed visual images during this next stage. \n' ...
             'You will be shown one of the words you previous studied. \n' ...
             'Keep in mind that these words were previously learned with \n' ...
             'two different pictures (one from Stage 2 and one from \n' ...
             'Stage 3). Your task is to create a mental image of the \n' ...
             'FIRST picture you learned with the word. Do not just think \n' ...
             'of the name of the picture but instead imagine what it \n' ...
             'looks like.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );
         
instruct = [ 'After 4 seconds spent creating this mental image, you will \n' ...
             'be asked the category of the mental image (face, object, \n' ...
             'or scene). This way we can check whether you correctly \n' ...
             'created a mental image of the first picture learned with \n' ...
             'the word, rather than the second picture. Press the "j" key \n' ...
             'if the first picture was a face, the "k" key if the first \n' ...
             'picture was an object, and "l" if the first picture was a \n' ...
             'scene. If you are unable to imagine the correct picture, \n' ...
             'press the ";" key. ' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

instruct = [ 'You will be asked to do this task 4 times for each word. \n' ...
             'It is important that you continue to create vivid detailed \n' ...
             'visual images throughout this task. Try to answer accurately \n' ...
             'but quickly. If you are incorrect, you will see the correct \n' ...
             'choice marked in green, but if you are too slow, you will \n ' ...
             'miss the chance to see this feedback.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

if debug < 2
    Selective_recall % Call separate Matlab script
end

%------------------------%
% Final recognition task %
%------------------------%
% Lookup - 09

% Display all target and competitor images (for both the baseline and
% non-baseline conditions)
% First, all competitors are shown (in random order)
% Next, all targets are shown (in random order)
ind2 = [ randperm(nAssoc) randperm(nAssoc) ];
% Create an index to select only targets or competitors
ImageType = [ ones(1,nAssoc)*2, ones(1,nAssoc) ];
% Create an index to access the correct row in the 'pairings' matrix
image_type = [ ones(1,nAssoc)*3, ones(1,nAssoc)*2 ];

% Additional variable declarations for logging the results
ImageNum = ones(1,totImage); % Index for image number
% Targets
ImageNum( ImageType == 1 ) = pairings( 2, ind2( ImageType == 1 ) );
% Competitors
ImageNum( ImageType == 2 ) = pairings( 3, ind2( ImageType == 2 ) );
% Index for retrieval versus baseline conditions
BaseType = zeros(1,totImage);
BaseType( ImageType == 1 ) = pairings( 4, ind2( ImageType == 1 ) );
BaseType( ImageType == 2 ) = pairings( 4, ind2( ImageType == 2 ) );
BaseType = BaseType - 1;
% Index for category type
CatType = zeros(1,totImage);
CatType( ImageType == 1 ) = pairings( 5, ind2( ImageType == 1 ) );
CatType( ImageType == 2 ) = pairings( 6, ind2( ImageType == 2 ) );
if debug > 0
    robotAccParFRT = ones(1,totImage)*.84;
    robotAccParFRT( ImageType == 2 & BaseType == 1 ) = .79;
end

% Additional variable declarations for the conditional statements
st_trials = 1;
end_trials = totImage;
expCond = 6;

instruct = [ 'Stage 5: Picture memory test\n\n' ...
             'This is the final stage of the experiment. You will no \n' ...
             'longer be shown words at all. Instead, this is a simple \n' ...
             'test of your memory for the pictures. As in the tests of \n' ...
             'Stages 2 and 3, you will see two versions of each picture \n' ...
             'and your task is to choose the version that you originally \n'...
             'studied in Stage 2 or Stage 3. The correct answer is the \n' ...
             'same version that was enclosed by the green box in Stages \n' ...
             '2 or 3.' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

instruct = [ 'Because we want your gut reaction on this choice, make \n' ...
             'your choice as quickly as you can (but make sure to \n' ...
             'actually look at both pictures before selecting one). \n' ...
             'As before, press the "j" to choose the picture on the \n' ...
             'left and the "k" to choose the picture on the right. ' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

if debug < 2
    Final_recognition % Call separate Matlab script
end

%-------------------%
% End of experiment %
%-------------------%
% Lookup - 10

% Record end of experiment
endTime = clock;

% Save results to file
orig_dir = cd('Subjects'); % Change over to folder for subject data

% Subject responses and condition info
fid = fopen(csvOutputFile{1}, 'wt'); % Open for writing
header = [ 'Subject,Trial,Cond,ImageNum,CueNum,Correct,Resp,Accuracy,' ...
    'RT,CueRep,ImageType,Baseline,Category,Block\n' ];
fprintf(fid, header);
for i=1:size(allResp,1)
    fprintf(fid, '%d,', SubjNum );
    fprintf(fid, '%d,', allResp(i,1:(size(allResp,2)-1)));
    fprintf(fid, '%d', allResp(i,size(allResp,2)));
    fprintf(fid, '\n');
end
fclose(fid);
clear header % Clean up workspace

% Stimulus info
fid = fopen(stimOutputFile{1}, 'wt'); % Open for writing
for i=1:size(stimulusInfo,2)
    fprintf(fid, '%d,', SubjNum );
    fprintf(fid, stimulusInfo{i});
    fprintf(fid, '\n');
end
fclose(fid);

% Record Matlab output as well
save(matOutputFile{1},'allResp','startTime','endTime','SubjNum', ...
    'stimulusInfo');

instruct = [ 'Congratulations! You''ve finished the experiment.\n' ...
    'You will now be paid for your participation. \n' ...
    'If you have any questions, contact information \n' ...
    'is provided on the debriefing form. Thanks again for \n' ...
    'your efforts!' ];
[nx,ny,bbox] = displayInstruct( window, instruct, center, lnSpace, InstructionTime, 200 );

% Return to the original directory
cd(orig_dir);

% Clear the screen
clear Screen;
sca;

% Run script to add to experiment log
Experiment_log