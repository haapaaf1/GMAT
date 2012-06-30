%%  Create segment and define what properties are fixed and which are variable
seg = Segment;
seg.SetInitialTime(0);
seg.SetFinalTime(3600);
seg.SetNumCollocationPoints(10);
seg.SetisInitialTimeVariable(0);
seg.SetisFinalTimeVariable(0);
seg.SetisMassVariable(0);
seg.SetisCoastArc(1);
seg.Initialize();

%%  Create initial guess and construct the state vector
x0 = [7000 0 0]';
v0 = [0 7.5 0]';
m0 = 1000;
for i = 1:size(seg.timeVector,1);
   stateMatrix(i,:) = [x0' + v0'*seg.timeVector(i) v0'];
end
decVector = seg.SetdecVector_State(stateMatrix);

%%  Try exctracting all of the state
for pointIdx = 1:seg.numCollocationPoints
   state = seg.GetState(decVector,pointIdx) 
end
%%  Test calling the dynamics
t = 0; 
X = [x0; v0; m0];
T = .001;
[Xdot,dXdotdX,dXdotdu,dXdotdt]  = psDynamics(t,X,[]);

%%  Test getting dynamics constraints and Jacobian
[c,J] = seg.GetDynamicsConstraints(decVector)