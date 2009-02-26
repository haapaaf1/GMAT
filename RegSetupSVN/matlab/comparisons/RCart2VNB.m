function RotVNB2Cart = RCart2VNB(Cart)

% Initial Cartesian Conditions 
r = Cart(1:3)'; % Position vector
v = Cart(4:6)'; % Velocity Vector

% Calculation of VNB state from initial conditions
h = cross(r,v);
r_hat = r/norm(r);
v_hat = v/norm(v);
h_hat = h/norm(h);
b_hat = cross(v_hat,h_hat);

RotVNB2Cart = [v_hat,h_hat,b_hat]; % Rotation matrix from VNB to Cartesian