mu = 398600.4415;
state = [9000 .7 1 8 0 0];
ini = 2;
fin = 3;
d2r = pi/80;

newstate = StateConV(state,ini,fin,mu)
newstate = StateConV(newstate,fin,ini,mu)
norm(newstate - state)


% %  Check round-off error
for i = 1:1000
    newstate = StateConV(newstate,ini,fin,398600.4415);
    newstate = StateConV(newstate,fin,ini,398600.4415);
    norm(newstate - state)
end

return
state = [9000 .7 10*d2r 20*d2r 30*d2r 40*d2r]
ini = 2;
fin = 1;

newstate1 = StateConV(state,ini,fin,mu)
temp = state;
t = state(5);
state(5) = state(4);
state(4) = t;
newstate2 = oe2cart(state,mu)