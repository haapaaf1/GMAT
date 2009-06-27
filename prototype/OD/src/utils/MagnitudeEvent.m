function [eventValue] = MagnitudeEvent(t,x,desiredValue);

eventValue = norm(x(1:3,1)) - desiredValue;