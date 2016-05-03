function [ RT, resp ] = Robot( answer, cond, param )
%Purpose:
% Simulates a response from a subject.
% Arguments:
% answer - the correct answer.
% cond   - the current experimental condition
% param  - a vector of 4 values, the slope and cut-off for the logistic
%          function and the mean and shape parameter for the inverse 
%          gaussian.
% Returns:
% A response time and choice.

% Initialize output
resp = 10; RT = 0;

% Setting for the familiarization phase of the experiment
if cond == 1
    
    % Probability of saying they can remember
    theta = 1 - param(1);
    
    % Simulate a response
    resp = binornd(1,theta) + 1;
    
    % Simulate a response time
    RT = rinvgauss( param(2), param(3) );
    
end

% Setting for the training - test phase of the experiment
if cond == 2
    
    if answer == 1
        theta = 1 - param(1);
    else
        theta = param(1);
    end
    
    % Simulate a response
    resp = binornd(1,theta) + 1;
    
    % Simulate a response time
    RT = rinvgauss( param(2), param(3) );
    
end

if cond == 3
    
    % Initialize theta
    theta = [ 0 0 0 1 ];
    
    % Define probability based on correct answer and competitor
    if answer == 1.2
        theta = [ param(1) param(2) param(3) .05 ];
    elseif answer == 1.3
        theta = [ param(1) param(3) param(2) .05 ];
    elseif answer == 2.1
        theta = [ param(2) param(1) param(3) .05 ];
    elseif answer == 2.3
        theta = [ param(3) param(1) param(2) .05 ];
    elseif answer == 3.1
        theta = [ param(2) param(3) param(1) .05 ];
    elseif answer == 3.2
        theta = [ param(3) param(2) param(1) .05 ];
    end
    
    % Simulate a response
    choices = 1:4;
    sel = mnrnd(1,theta);
    resp = choices( sel == 1 );
    
    % Simulate a response time
    RT = rinvgauss( param(4), param(5) );
end

end