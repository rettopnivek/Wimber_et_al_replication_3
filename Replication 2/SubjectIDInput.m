%------------------%
% Subject ID input %
%------------------%

%{
Purpose:
A Matlab script to input the subject ID number.

Requirements:
Matlab
Psychtoolbox
An call of PTB_initial
The variable 'Session' (either '_train' or '_test')

Outputs:
Creates the file names for saving the output of data collection
%}

% Check if the Subject folder exists. If not, create it.
folderCheck = exist('Subjects','dir');
if (~folderCheck)
    mkdir('Subjects')
end;

% Change over to directory with output files
orig_dir = cd('Subjects');

% Set initial variables
IDconfirm = 0;
confirmOptions = getKeyAssignments('yn',1,300);
enterKey = KbName('Return');
allFiles = dir;

% While loop for the ID number input, as well as the failsafes in case the
% filename is a duplicate.
while (~IDconfirm)
    
    % Set input log to 0
    endIDinput = 0;
    % String to save subject ID number
    IDnum = '_';
    
    % Display prompt text
    DrawFormattedText(window, 'Enter subject ID number and hit the enter key', 'center', 'center', [ 0 0 0 ] );
    Screen('Flip', window);
    
    % Record the key inputs and wait for the enter key input
    while(~endIDinput)
        press = 0;
        % Record keyboard input
        [ press, ~, keynum ] = KbCheck;
        keys = 1:length(keynum);
        if press
            if (keys(keynum==1)==enterKey)
                endIDinput = 1;
                % IDconfirm = 1;
            end;
            string = KbName( keys(keynum==1) );
            if ~(string(1)=='R')
                IDnum = strcat( IDnum, string(1) );
                DrawFormattedText(window, 'Enter subject ID number and hit the enter key', 'center', 'center', [ 0 0 0 ] );
                DrawFormattedText(window, IDnum(2:length(IDnum)), 'center', center(2)+50, [ 0 0 0 ] );
                Screen('Flip', window);
                % WaitSecs(0.1);
            end;
        end;
        WaitSecs(0.1);
    end;
    
	% Create output file names
    csvOutputFile = strcat('Subject',IDnum,Session,'.csv');
    matOutputFile = strcat('Subject',IDnum,Session,'.mat');
    
	% Determine the size of the text on the y-axis
    [dim, extra] = Screen( window, 'TextBounds', IDnum );
    
    % Determine the y-axis coordinates for each line of text
    nameCheck = ones(1,3)*dim(4) + 4;
    nameCheck = ( center(2) - sum(nameCheck)/2 ) + cumsum(nameCheck) - nameCheck(1);
    
    % Reset input log
    endIDinput = 0;
    
    % Double check whether the file names are correct
    DrawFormattedText(window, 'Are these filenames acceptable? (y/n)', 'center', nameCheck(1), [ 0 0 0 ] );
    DrawFormattedText(window, csvOutputFile, 'center', nameCheck(2), [ 0 0 0 ] );
    DrawFormattedText(window, matOutputFile, 'center', nameCheck(3), [ 0 0 0 ] );
    Screen('Flip', window);
    
	% Record the key inputs and wait for the enter key input
    while(~endIDinput)
        press = 0;
        % Record keyboard input
        [ press, ~, keynum ] = KbCheck;
        keys = 1:length(keynum);
        if press
            
            string = KbName( keys(keynum==1) );
            DrawFormattedText(window, 'Are these filenames acceptable? (y/n)', 'center', nameCheck(1), [ 0 0 0 ] );
            DrawFormattedText(window, csvOutputFile, 'center', nameCheck(2), [ 0 0 0 ] );
            DrawFormattedText(window, matOutputFile, 'center', nameCheck(3), [ 0 0 0 ] );
            DrawFormattedText(window, string, 'center', nameCheck(3)+50, [ 0 0 0 ] );
            Screen('Flip', window);
            
            if (keys(keynum==1)==confirmOptions(1) || keys(keynum==1)==confirmOptions(2))
                endIDinput = 1;
                if (keys(keynum==1)==confirmOptions(1))
                    lastCheck = 0;
                    for fn = 1:length(allFiles)
                        lastCheck = ( strcmp(allFiles(fn).name,csvOutputFile) || strcmp(allFiles(fn).name,matOutputFile) );
                    end;
                    if (lastCheck == 0)
                        IDconfirm = 1;
                    else
                        % Indicate that the files are duplicates
                        DrawFormattedText(window, 'Filename already exists; please input a different one.', 'center', 'center', [ 0 0 0 ] );
                        Screen('Flip', window);
                        WaitSecs(1);
                    end;
                end;
            end;
        end;
        WaitSecs(0.1);
    end;
    
end;

FlushEvents('keyDown');

% Clean up workspace
clear folderCheck IDconfirm keys confirmOptions enterKey allFiles;
clear endIDinput IDnum nameCheck press secs keynum fn string;

% Return to the original directory
cd(orig_dir);
