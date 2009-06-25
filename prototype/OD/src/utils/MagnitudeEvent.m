function [eventValue] = MagnitudeEvent(x,desiredValue);

eventValue = norm(x(1:3,1)) - desiredValue;