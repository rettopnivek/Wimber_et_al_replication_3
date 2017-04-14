function [ keyNumbers ] = getKeyAssignments( string, stVal, endVal )
%{
Purpose
A function to identify the numbers assigned to specific keyboard keys
Arguments
string: A vector of the characters whose numerical values are needed (e.g.
        'asdf')
stVal:  The numerical value to begin the search
endVal: The numerical value to conclude the search
Returns
keyNumbers: A vector of numbers, which can be used in functions like
            'getResponse' or 'keynum'
%}

    nInputs = length(string);
    allKeys = KbName(stVal:endVal);
    nKeys = length(allKeys);
    keyNumbers = zeros(1,nInputs);
    indexVal = stVal:endVal;
    
    for ni = 1:nInputs
        check = -1*ones(1,nKeys);
        % Stuff
        for nk = 1:nKeys
            % Stuff
            if ( isempty(allKeys{nk}) == 0 )
                check(nk) = sum((allKeys{nk} == string(ni) )-1);
            end;
        end;
        keyNumbers(ni) = indexVal(check > -1);
    end;
    
end

