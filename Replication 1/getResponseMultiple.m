function [ RT, resp ] = getResponseMultiple( keys, valuesAssigned, timeout )
%{
Purpose
A function to measure the choice and response time for a task in which
there can more than 2 responses.
Arguments
keys:           A vector giving the number(s) representing the key(s) to 
                press to a particular choice (use KbName function to 
                determine the number).
valuesAssigned: The response values assigned to each choice.
timeout:        The interval of time to record a response.
Returns
RT:    A response time (in seconds), calculated starting from when the
       function was called.
resp:  Indicates whether the left choice (1) or right choice (2) was
       picked.
%}
    stop = GetSecs + timeout;
    st_time=GetSecs;
    resp = 0;
    RT = 0;
    while ~resp
        [press,secs,keynum]=KbCheck;
        if press        
            if ( sum( keynum( keys ) ) == 1 )
                RT = secs-st_time;
                resp = valuesAssigned( keynum( keys ) == 1 );
                break;
            end
        end
        if (GetSecs > stop)
            break;
        end;
        WaitSecs(0.0005);
    end
    if (RT == 0)
        RT = GetSecs - st_time;
    end;
FlushEvents('keyDown');
end

