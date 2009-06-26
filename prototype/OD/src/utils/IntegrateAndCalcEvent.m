clc; clf;
x0 = [7100 0 1300 0 7.3 1 318]';
timeVec = [0:150:30000];

frhs          = 'orbitODE';
eventFunction = 'MagnitudeEvent';
desiredValue  = 6900;
[tout, yout, E,bracketTimes,bracketValues] = rk78(frhs, 0, 80000, x0, 1e-9)


%[tspan,Z,E,bracketTimes,bracketValues] = RK45(frhs,timeVec,x0,eventFunction,desiredValue);

figure(1);
plot(tout,E); hold on
plot(bracketTimes(:,1),bracketValues(:,1),'*');
plot(bracketTimes(:,2),bracketValues(:,2),'*');
