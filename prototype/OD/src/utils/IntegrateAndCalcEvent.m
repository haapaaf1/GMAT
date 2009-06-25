x0 = [7100 0 1300 0 7.3 1]';
timeVec = [0:60:10000];

frhs          = 'orbitODE';
eventFunction = 'MagnitudeEvent';
desiredValue  = 7000;

[tspan,Z,E] = RK45(frhs,timeVec,x0,eventFunction,desiredValue);

figure(1);
plot(tspan,E)
figure(2)
plot3(Z(:,1),Z(:,2),Z(:,3))