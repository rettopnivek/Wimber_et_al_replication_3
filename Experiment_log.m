%--------------------%
% Experiment log     %
% Updated 02/02/2016 %
%--------------------%

%{
Purpose:
A Matlab script which will generate a generate a .csv file logging 
important details of the experiment (e.g. start and finish times, 
output file names, who the experimenter was, and additional notes).

Requirements:
An installation of Matlab

Notes:
This file should be run after clearing all windows and pointers for 
Psychtoolbox.

Outputs:
A file 'Experiment_log.csv'
If the file already exists, adds a new row to the file
%}

% Change directory
orig_dir = cd('Subjects');

% Check if log exists
if exist('Experiment_log.csv', 'file') ~= 2
    % If it doesn't, create it
    fid = fopen('Experiment_log.csv', 'wt'); % Open for writing
    string = 'Output,RNG-number,Start-time,End-time,Initials,Notes\n';
    fprintf(fid,string);
    fclose(fid);
end

% Record experimenter's initials and any notes on the session
prompt = {'Enter initials:','Enter any notes on the session:'};
dlg_title = 'Experiment log';
num_lines = [ 2 40 ];
answer = inputdlg(prompt,dlg_title,num_lines);

% Write times as Year\Month\Day\Hour:Minute AM PM
hr = [ startTime(4) endTime(4) ];
dt{1} = 'AM'; dt{2} = 'AM';
if startTime(4) > 12
    hr(1) = hr(1) - 12;
    dt{1} = 'PM';
end
if endTime(4) > 12
    hr(2) = hr(2) - 12;
    dt{2} = 'PM';
end

% Add a zero for minutes that are only 1 digit long
mn = [ startTime(5) endTime(5) ];
if mn(1) < 10
    mnSt = [ '0' num2str(mn(1)) ];
else
    mnSt = [ num2str(mn(1)) ];
end

if mn(2) < 10
    mnEd = [ '0' num2str(mn(2)) ];
else
    mnEd = [ num2str(mn(2)) ];
end

stTime = [ num2str(startTime(1)) '\' num2str(startTime(2)) '\' ...
    num2str(startTime(3)) '\' num2str(hr(1)) ':' mnSt ...
    ' ' dt{1} ];
edTime = [ num2str(endTime(1)) '\' num2str(endTime(2)) '\' ...
    num2str(endTime(3)) '\' num2str(hr(2)) ':' mnEd ...
    ' ' dt{2} ];

% Output RNG-number Start-time End-time Initials Notes
fid = fopen('Experiment_log.csv', 'at'); % Open for writing
string = [ csvOutputFile{1} '-' matOutputFile{1} ',' ...
    num2str(RNG_seeds(SubjNum)) ',' stTime ',' edTime ',' answer{1} ...
    ',' answer{2} char(10) ];
fprintf(fid, '%s', string);
fclose(fid);

% Clear workspace
clear prompt dlg_title num_lines answer edTime stTime ...
    string hr dt;

% Return to original directory
cd(orig_dir);