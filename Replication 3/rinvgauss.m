function [ t ] = rinvgauss( mu, lambda )
% Purpose:
% Generate a random draw from the inverse gaussian distribution.
% Arguments:
% mu     - the mean parameter
% lambda - the shape parameter
% Returns:
% A single random deviate

% Generate a random draw
v = normrnd(0.0,1.0);
z = unifrnd(0.0,1.0);
y = v^2;
p1 = mu + (mu^2)*y/(2*lambda);
p2 = mu/(2*lambda);
p3 = sqrt( 4*mu*lambda*y + (mu*y)^2 );
x = p1+p2*p3;
if z <= mu/(mu+x)
    t = x;
else
    t = (mu^2)/x;
end;

end

