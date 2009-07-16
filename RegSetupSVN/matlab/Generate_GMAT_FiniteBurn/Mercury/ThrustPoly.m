% Test Thruster Equation

%Define Thruster Polynomial Coefficients

c1 = 1.23758251293888;
c2 = 0.00730193081644684;
c3 = 1.06710728099668;
c4 = 1.44084613514414;
c5 = 1.12975859384182;
c6 = 0.866449276427312;
c7 = 1.26090987550771;
c8 = 1.12890566239368;
c9 = 1.25439122773649;
c10 = 1.78577524273692;
c11 = 0.523539555272069;
c12 = 1.15120028332336;
c13 = 0.832532168870019;
c14 = 1.26666006242687;
c15 = 1.09502172813843;
c16 = -0.702022868622232;

% Define Tank Pressure
P = 1200;                %Tanks Pressure in kilopascals

% Define Temperatures
T    = 2;                %tank temperature in degrees Celsius
Tref = 12;               %tank reference temperature in degrees Celsius

%Thruster Polynomial Equation
Th = ( c1 + c3 * ( T / Tref ) ^ ( 1 + c15 + c16 * P ) ) + ...
     ( c2 + c4 * ( T / Tref ) ^ ( 1 + c15 + c16 * P ) ) * P + ...
     ( c5 * P^2 + c6 * P^c7 + c8 * P^c9 + c10 * P^c11 + ...
     c12 * c13 ^ (c14 * P) ) * ( T / Tref ) ^ ( 1 + c15 + c16 * P )
 
 