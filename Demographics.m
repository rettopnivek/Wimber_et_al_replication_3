%--------------------------------------------------------%
% Onscreen script to record race/ethnic/sex demographics %
% for Matlab                                             %
% Updated 05/03/2016                                     %
%--------------------------------------------------------%

%{
Purpose:
A Matlab script which will generate a set of dialog boxes that ask 
participants an assortment of questions regarding demographics (in 
compliance with NIH requirements).

Requirements:
An installation of Matlab

Notes:
This file should be run before initializing a window pointer for 
Psychtoolbox.

Outputs:
A text file 'Demographics.txt'
%}

% Set the width and height of the dialog box
boxSize = [ 250, 100 ];
% Create a cellstring array to store the answers
output = [ {'Sex'} {'Ethnicity'} {'Race'} ];

% Display an initial message regarding the NIH
string = [ 'The National Institute of Health requests basic ' ...
           'demographic information (sex, ethnicity, and race) ' ...
           'for clinical or behavioral studies, to the extent ' ...
           'that this information is provided by research ' ...
           'participants.\n\nYou are under no obligation to provide ' ...
           'this information. If you would rather not answer these ' ...
           'questions, you will still receive full compensation ' ...
           'for your participation in this study and the data you ' ...
           'you provide will still be useful for our research.' ];
headerNIH = sprintf(string);
% Must close message to continue
waitfor( msgbox(headerNIH,'Demographics') );

%%% Sex at birth %%%

% Define the initial prompt
stringPrompt = [ {sprintf('1) Sex at birth:')} {''} ];

% Define the choices that can be selected
Choices = [ {'Female'} {'Male'} {'Other'} ];

% Create a list dialog box and determine the selection
sel = listdlg('PromptString',stringPrompt,...
    'SelectionMode','single',... % So people can only pick one option
    'ListString',Choices,...
    'ListSize',boxSize,...
    'CancelString','Rather not say');

% Save output
if ( isempty(sel) )
    output{1} = 'Sex, Rather not say';
else
    output{1} = [ 'Sex, ' Choices{sel} ];
end

%%% Ethnicity %%%

% Define the initial prompt
stringPrompt = [ {sprintf('2) Ethnicity:')} {''} ];

% Define the choices that can be selected
Choices = [ {'Hispanic or Latino'} {'Not Hispanic or Latino'} ];

% Create a list dialog box and determine the selection
sel = listdlg('PromptString',stringPrompt,...
    'SelectionMode','single',...
    'ListString',Choices,...
    'ListSize',boxSize,...
    'CancelString','Rather not say');

% Save output
if ( isempty(sel) )
    output{2} = 'Ethnicity, Rather not say';
else
    output{2} = [ 'Ethnicity, ' Choices{sel} ];
end

%%% Race %%%

% Define the initial prompt
stringPrompt = [ {sprintf('3) Race:')} {''} ];

% Define the choices that can be selected
Choices = [ {'American Indian/Alaska Native'} {'Asian'} {'Native Hawaiian or Other Pacific Islander'} {'Black or African American'} {'White'} ];

% Create a list dialog box and determine the selection
sel = listdlg('PromptString',stringPrompt,...
    'SelectionMode','single',...
    'ListString',Choices,...
    'ListSize',boxSize,...
    'CancelString','Rather not say');

% Save output
if ( isempty(sel) )
    output{3} = 'Race, Rather not say';
else
    output{3} = [ 'Race, ' Choices{sel} ];
end

%%% Age %%%

% Define the initial prompt
stringPrompt = [ {sprintf('4) Age (click "Cancel" to skip):')} ];
num_lines = 1;
Choices = inputdlg(stringPrompt,' ',num_lines);

% Save output
if ( isempty(sel) )
    output{4} = 'Age, Rather not say';
else
    output{4} = [ 'Age, ' Choices{1} ];
end
drawnow; pause(0.05);  % this innocent line prevents the Matlab hang

% Record output to a text file
fid = fopen( 'Demographics.txt', 'wt' );
for i = 1:4
  fprintf( fid, sprintf( [ output{i} '\n' ] ) );
end
fclose(fid);

% Cleans up workspace
clear boxSize output headerNIH stringPrompt Choices sel output i ans fid
clear num_lines string
