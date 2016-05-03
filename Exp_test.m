%-------------------------------------------%
% Wimber et al (2015) replication version 2 %
% Kevin Potter and David Huber              %
% Updated 06/17/2015                        %
%-------------------------------------------%

%{
Purpose:
A Matlab script to run a second version of a replication of the 
experimental design used in Wimber, Alink, Charest, Kriegeskorte, 
and Anderson (2015). This script runs the test part of the 
experiment, after the context change.

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
Lookup - Instruct 0
Lookup - 01: Selective retrieval
Lookup - Instruct 1
Lookup - 02: Final recognition task
Lookup - 03: End of experiment
Lookup - Instruct 2
%}

%-----------------------%
% Variable declarations %
%-----------------------%

%%% Input subject ID number for later output files %%%
% Lookup - 00
Session = '_test';
SubjectIDInput % Call separate Matlab script

% Convert the ID number from a string to a number for later indexing
SubjNum = [];
for i = 9:11
    if ( isempty(str2num(csvOutputFile(i)))==0 )
        SubjNum = [ SubjNum csvOutputFile(i) ];
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

Task 4: Selective recall (Non-baseline images only)

Task 5: Final recognition (All targets and competitors)

%}

%%% Initialize log file %%%
% Lookup - Log 0
log_state = 0;
log_results % Call separate Matlab script for logging results

%%% Instructions %%%
% Lookup - Instruct 6

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Imagery';
instruct{2} = ' ';
instruct{3} = 'This is the first of the last two tasks. The ultimate goal ';
instruct{4} = 'of this study is to see if we can get similar results to ';
instruct{5} = 'studies trying to predict, based on brain activity, which ';
instruct{6} = 'category the image a person has in mind comes from i.e., ';
instruct{7} = 'whether you are currently thinking of a face, a place or an ';
instruct{8} = 'object.';
instruct{9} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'On each trial, you will see one word alone on the screen, ';
instruct{2} = 'and your task is to visualize the very first picture that ';
instruct{3} = 'you initially learned to visualize in response to this word, ';
instruct{4} = 'that is, in the very first of the training sessions. ';
instruct{5} = 'When focusing on the original picture, it is very important ';
instruct{6} = 'that you will not simply think of the correct label of the ';
instruct{7} = 'picture, but that you really try to visualize the correct ';
instruct{8} = 'picture as vividly as possible, exploring all the details ';
instruct{9} = 'of the face, the place, or the object in your mind for the ';
instruct{10} = 'full 4 seconds that the reminder word is on the screen. ';
instruct{11} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'After 4 seconds, you will be asked to press a key indicating ';
instruct{2} = 'which category the picture you have in mind comes from. ';
instruct{3} = 'Please press "j" (index finger) if you are thinking of a ';
instruct{4} = 'face, "k" (middle finger) if you are thinking of an object, ';
instruct{5} = 'and "l" (ring finger) if you are thinking of a scene. If ';
instruct{6} = 'you are unable to come up with the correct image, please ';
instruct{7} = 'indicate by pressing ";" (pinkie). ';
instruct{8} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'Even if you see a word for the third of fourth time, it is ';
instruct{2} = 'very important that you vividly bring the corresponding ';
instruct{3} = 'picture back to mind in all its detail. Likewise, even if ';
instruct{4} = 'you cannot bring up the correct image the first time, keep ';
instruct{5} = 'trying when you see the word for the second, third or ';
instruct{6} = 'fourth time.';
instruct{7} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

%%% Selective retrieval %%%
% Lookup - 05

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
instruct{1} = 'Picture discrimination';
instruct{2} = ' ';
instruct{3} = 'In this last part of the experiment, which will take ';
instruct{4} = 'approximately 10 minutes, you will simultaneously see two ';
instruct{5} = 'versions of each of the pictures you have initially seen ';
instruct{6} = 'together on the screen. Your task is to indicate which of ';
instruct{7} = 'the two versions you had previously associated with a word. ';
instruct{8} = 'Importantly, you have seen all of these pictures at the ';
instruct{9} = 'very beginning of the experiment, so both versions will ';
instruct{10} = 'look familiar to you. It is therefore critical that you ';
instruct{11} = 'think back to the training phases, and chose the picture ';
instruct{12} = 'that you have linked with a word before.';
instruct{13} = 'Press any key to continue';
lenIns = length(instruct);

% Display the instructions
displayInstruct % Call separate Matlab script

% Define the instructions
instruct = num2str( ones(2,1) ); % Create a cell string array
instruct = cellstr(instruct);
% Fill the array with the individual lines of instructions
instruct{1} = 'This task is about speed! Select the correct picture (left ';
instruct{2} = '= "j", right = "k") as fast as possible, and guess if you ';
instruct{3} = 'cannot remember.';
instruct{4} = 'Press any key to continue';
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
instruct{2} = 'You will now be paid for your participation. ';
instruct{3} = 'If you have any questions, contact information ';
instruct{4} = 'is provided on the debriefing form. Thanks again for your ';
instruct{5} = 'efforts!';
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