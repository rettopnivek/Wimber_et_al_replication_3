%----------%
% Log file %
%----------%

%{
Purpose:
A Matlab script to save the stimulus information, choice, and response
times of subjects for each relevant trial.

Requirements:
Matlab
Psychtoolbox
The Exp.m script
A value for log_state

Outputs:
Adds lines to a cell string array that will be converted to the .csv file

Notes:
Structure of the output file for the replication of Wimber et al (2015) 
experiment (Columns in log file)...

Trial number   - Indicates which trial it is out of the total number of 
                 trials
Cond           - Indicates to which condition the current trial belongs
                   1 = Familiarization
                   2 = Cycle 1 target recognition test phase
                   3 = Cycle 2 target recognition test phase
                   4 = Cycle 1 competitor recognition test phase
                   5 = Selective retrieval
                   6 = Final recognition
ImageNum       - The unique number associated with an image and its
                 corresponding lure (in the case of the selective 
                 retrieval task, the image number for the target and the 
                 competitor, in that order, are both given, separated by 
                 a decimal)
Cue            - Provides the cue word (when applicable)
Block          - Provided the current block number for the association
                 recall and recognition test trials
Correct        - For the visual recognition test, indicates the correct
                 side for the image was located (1 = Left, 2 = Right),
                 while for the selective retrieval task, indicates the 
                 correct category, the target image (1 = Face, 2 = Object,
                 3 = Scene).
Response       - The keyboard response the subject made for a given trial,
                 where the values refer to (depending on the condition)...
                   1 = Right, 2 = Left (recognition task)
                   1 = Face, 2 = Object, 3 = Scene, 4 = Unknown (selective
                   retrieval task)
RT             - The response time of the subject, in seconds
Accuracy       - Indicates whether a subject made an accurate response
                 (0 = incorrect, 1 = correct)
cueRep         - The number of times during selective retrieval that a cue
                 word had already been repeated for a given trial
fName          - The file name for the image (or lure) being shown (in 
                 cases where two images were shown, both file names are
                 provided, separated by a hyphen)
ImageType      - Indicates what type an image is, where lure = 0, target = 
                 1, and competitor = 2
Baseline       - Indicates whether an image is in the baseline condition 
                 where 0 = No, 1 = Yes
Category       - The numerical coding for whether an image can be 
                 categorized as a face (1), object (2), or scene (3)

%}

if (log_state == 0)
    % Create header for log file
    log = 'Trial,Cond,ImageNum,Cue,Block,Correct,Response,RT,Accuracy,CueRep,fName,ImageType,Baseline,Category';
    log_file = cellstr( log );
    log_inc = 1;
end;
if (log_state == 1)
    % Log results for familiarization phase
    log_inc = log_inc + 1;
    % Trial, Cond, ImageNum, Cue, Block, Correct, Response, RT, Accuracy, CueRep, fName, ImageType, Baseline, Category
    log = [ num2str(log_inc-1), ',', num2str(expCond), ',', num2str(all_labels(ind(trl))), ',', 'NA', ',', 'NA', ',', 'NA', ',', num2str(resp), ',', num2str(RT), ',', 'NA', ',', 'NA', ',', f_name, ',', num2str(ImageType(ind(trl))), ',', num2str(BaseType(ind(trl))), ',', num2str(CatType(ind(trl))) ];
    log_file{log_inc} = log;
end;
if (log_state == 2)
    % Log results for the test phase
    log_inc = log_inc + 1;
    % Trial, Cond, ImageNum, Cue, Block, Correct, Response, RT, Accuracy, CueRep, fName, ImageType, Baseline, Category
    log = [ num2str(log_inc-1), ',', num2str(expCond), ',', num2str(ImageNum(trl)), ',', cueString, ',', num2str(blck), ',', num2str(Correct), ',', num2str(resp), ',', num2str(RT), ',', num2str(Correct==resp), ',', 'NA', ',', [ i_name, '-' l_name ], ',', num2str(ImageType(ind2(trl))), ',', num2str(BaseType(ind2(trl))), ',', num2str(CatType(ind2(trl))) ];
    log_file{log_inc} = log;
end;
if (log_state == 3)
	% Log results for the selective retrieval phase
    log_inc = log_inc + 1;
    % Trial, Cond, ImageNum, Cue, Block, Correct, Response, RT, Accuracy, CueRep, fName, ImageType, Baseline, Category
    log = [ num2str(log_inc-1), ',', num2str(expCond), ',', num2str(ImageNum(trl)), ',', cueString, ',', 'NA', ',', num2str( pairings( 5, ind2(trl) ) ), ',', num2str(resp), ',', num2str(RT), ',', num2str( pairings( 5, ind2(trl) ) == resp ), ',', num2str( repetition(trl) ), ',', [ i_name, '-' c_name ], ',', 'NA', ',', num2str(BaseType(trl)), ',', num2str(CatType(trl)) ];
    log_file{log_inc} = log;
end;
if (log_state == 4)
	% Log results for the final recognition phase
    log_inc = log_inc + 1;
    % Trial, Cond, ImageNum, Cue, Block, Correct, Response, RT, Accuracy, CueRep, fName, ImageType, Baseline, Category
    log = [ num2str(log_inc-1), ',', num2str(expCond), ',', num2str(ImageNum(trl)), ',', cueString, ',', 'NA', ',', num2str(Correct), ',', num2str(resp), ',', num2str(RT), ',', num2str(Correct==resp), ',', 'NA', ',', [ i_name, '-' l_name ], ',', num2str(ImageType(trl)), ',', num2str(BaseType(trl)), ',', num2str(CatType(trl)) ];
    log_file{log_inc} = log;
end;


