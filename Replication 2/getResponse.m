function [ RT, resp ] = getResponse( pickleft, pickright )
%{
Purpose
A function to measure the choice and response time in a 2AFC task
(adapted from code by Angela Nelson Lowe)
Arguments
pickleft:   The number representing the key to press to pick the choice
            on the left (use KbName function to determine the number).
pickright:  The number representing the key to press to pick the choice
            on the right (use KbName function to determine the number).
Returns
RT:    A response time (in seconds), calculated starting from when the
       function was called.
resp:  Indicates whether the left choice (1) or right choice (2) was
       picked.
%}
st_time=GetSecs;
resp=0;
while ~resp
    [press,secs,keynum]=KbCheck;
    if press
        if keynum(pickleft)
           RT=secs-st_time;
           resp=1;
       elseif keynum(pickright)
           RT=secs-st_time;
           resp=2;
        end
       abort911(keynum);
       while KbCheck; end;
   end   
end
FlushEvents('keyDown');

end

