function [time,Z,E,bracketTimes,bracketValues] = RK45(frhs,tspan,z0,eventFunc,desiredValue)

%  function [tspan,Z] = RK45(frhs,Time,z0)
%
%  Runge-Kutta fourth order integrator
%
%  Variable I/O
%  UOI - > Units of Input
%  Variable Name    I/0    Units   Dim.       Description
%  frhs              I     None    None       'filename' where filename.m is m-file
%  Time              I     UOI     1x3        [t0 tf tstep]
%  z0                I     UOI     nx1        initial conditions
%  Z                 O     UOI     Nxn        Matrix of states corresponding to time vector T
%  T                 O     UOI     Nx1        Vector of times


N=length(tspan);
n=length(z0);
z0=reshape(z0,n,1);
w=z0;
Z(1,:)= z0';

numPoints = 5;
rootOrder = 3;
oldEventValue = feval(eventFunc,z0,desiredValue);
E(1,1)  = oldEventValue;
time(1,1) = tspan(1,1);
bracketTimes = [];
bracketValues = [];
interpTimes   = time(1,1);
interpVals    = oldEventValue;

for i=2:N
    
    %  Take a step
    h=tspan(i)-tspan(i-1);
    t=tspan(i-1);
    K1=h*feval(frhs,t,w);
    K2=h*feval(frhs,t+h/2,w+K1/2);
    K3=h*feval(frhs,t+h/2,w+K2/2);
    K4=h*feval(frhs,t+h,w+K3);
    w=w+(K1+2*K2+2*K3+K4)/6;
    
    %  Save data
    time(i,1) = tspan(i);
    Z(i,:)    = w';
    E(i,1)    = feval(eventFunc,w,desiredValue);
    
    
    %----- Update function value data
    if i == 2
        newEventValue = feval(eventFunc,w,desiredValue);
    else
        oldEventValue = newEventValue;
        newEventValue = feval(eventFunc,w,desiredValue);
    end
    
    %----- Update interpolation data
    if i < numPoints + 1;
        interpTimes = [interpTimes;tspan(i)];
        interpVals  = [interpVals;newEventValue];
    else
        interpTimes = [interpTimes(2:numPoints); tspan(i)];
        interpVals  = [interpVals(2:numPoints);newEventValue];
    end
    
    %-----  Perform root detection
    if oldEventValue*newEventValue < 0
        %  Check to see if the function changes sign
        bracketTimes  = [bracketTimes; tspan(i-1) tspan(i)];
        bracketValues = [bracketValues; oldEventValue newEventValue];
    else
        %  Check roots of interpolating polyomial to see if a root may have
        %  been skipped.
        
        if size(interpTimes,1) > rootOrder + 1;
            meanTime = mean(interpTimes);
            coeff  = polyfit(interpTimes-meanTime,interpVals/max(interpVals),rootOrder);
            croots = roots(coeff) + meanTime;
            for j = 1:rootOrder
                if ~imag(croots(j))
                    if croots(j) > tspan(i-1) & croots(j) < tspan(i)
                        bracketTimes  = [bracketTimes; tspan(i-1) tspan(i)];
                        bracketValues = [bracketValues; oldEventValue newEventValue];
                    end
                end
            end
        end
    end
    
    
end

