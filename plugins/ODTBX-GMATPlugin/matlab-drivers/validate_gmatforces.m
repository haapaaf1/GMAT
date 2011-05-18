% Script to analytically validate gmatForces_km and gmatWorld.
startgmat;

mu = 398600.4415
r = [10000; 0; 0];
v = [0; sqrt(mu/r(1)); 0];
state = [r; v]
T = sqrt((4*pi*pi*r(1)^3)/mu)
t = 1;

w = 17;

% note, integ makes a copy of w and uses that copy
[tPlot,xPlot] = integ(@gmatforces_km,[0 T],state,[],w);

figure;
subplot(3,1,1);plot(tPlot,xPlot(1,:)); ylabel('X axis (km)');
subplot(3,1,2);plot(tPlot,xPlot(2,:)); ylabel('Y axis (km)');
subplot(3,1,3);plot(tPlot,xPlot(3,:)); ylabel('Z axis (km)');
xlabel('Time (sec)');
disp('state numerical integration difference at analytical period for circular orbit:');
diff = xPlot(:,end)-xPlot(:,1)

disp('Staring timing tests...');
tlen = [1e2 5e2 1e3 5e3 1e4 5e4 1e5 5e5];
for t = 1:length(tlen)
    tic;
    [tPlot,xPlot] = integ(@gmatforces_km,[0:tlen(t)]*(T/tlen(t)),state,[],w);
    tElapsed(t) = toc;
end
figure;
subplot(2,1,1);loglog(tlen,tElapsed,'bx-');
xlabel('points');
ylabel('Elapsed time (sec)');
subplot(2,1,2);plot(tlen,tElapsed./tlen,'bx-');
xlabel('points');
ylabel('Elapsed time/point');

closegmat;
