clc; clf;
x0 = [12100 0 1300 0 7.3 1]';
timeVec = [0:150:60000];

frhs          = 'orbitODE';
eventFunction = 'MagnitudeEvent';
desiredValue  = 6900;
[tout, yout, E,bracketTimes,bracketValues] = rk78(frhs, 0, 180000, x0, 1e-13)

for i = 1:size(tout,1);
    jd0 = 21545+2430000;
    jd  = jd0 + tout(i)/86400;
    rv_sun = sun(jd)'*149597870.691;
    rv_sunvec(i,:) = rv_sun'/norm(rv_sun)*6378*5;
    dotprod(i) = rv_sunvec(i,1:3)/norm(rv_sunvec)*yout(i,1:3)'/norm(yout(i,1:3));
end



for i = 1:size(tout)
    
    %----- Draw this stuff every time
    hold off
    figure(1);clf
    subplot(2,1,1); 
    plot(tout,E);
    hold on;
    plot(bracketTimes(:,1),bracketValues(:,1),'*');
    plot(bracketTimes(:,2),bracketValues(:,2),'*');
    hold on;
    plot(tout,dotprod,'g-');
    grid on;  
    plot(tout(i),E(i),'k*','MarkerSize',3);


    subplot(2,1,2);
    plot3(yout(:,1),yout(:,2),yout(:,3)); 
    hold on;
    [x,y,z] = sphere(50);
    surf(x*6378,y*6378,z*6378); axis equal;

    plot3([0 rv_sunvec(1,1)],[0 rv_sunvec(1,2)], [0 rv_sunvec(1,3)],'k-','LineWidth',3)
    view(rv_sunvec(1,:))

   %  Draw this 
   hold on
   plot3([yout(i,1)],[yout(i,2)],[yout(i,3)],'k*','MarkerSize',6);
   pause(0.01)
   
end

return
vec = ones(1,1);
[X,Y,Z] = cylinder(vec,15);
surf(X*6378,Y*6378,Z*6378)
axis equal;
