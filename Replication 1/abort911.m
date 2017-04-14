function [ ] = abort911( keynum )
% Purpose
% A function to cancel a PTB experiment (adapted from code by Angela 
% Nelson Lowe)
% Arguments
% Forthcoming
% Returns
% Void
escapeKey = KbName('ESCAPE');

if keynum(escapeKey)
    Screen('CloseAll');
	error('Experiment aborted.');

end

