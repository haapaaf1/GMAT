% Calculate the Isp Polynomial

% Input coefficients
k1  = 1.19380722364703;
k2  = 0.24900508127885;
k3  = 1.13275785148817;
k4  = 0.883718634158075;
k5  = 1.12834190490672;
k6  = 1.09178332650158;
k7  = 1.22723805227818;
k8  = 1.67356545761199;
k9  = 1.22744530093833;
k10 = 1.38922191588571;
k11 = 0.593720248185885;
k12 = 1.02790703214479;
k13 = 0.8544232536899;
k14 = 1.22656975016494;
k15 = 1.07915506152261;
k16 = -0.551448194853405;

% Input Pressure of Tank
p = 1200;     % Pressure of Tank in kilopascals

% Input Temperatures
Tfuel = 20;      % Temperature of fuel in Tank in Degrees Celsius
Tref = 100;       % Reference temperature of fuel tank in Degrees Celsius

% Isp Polynomial Equation

Isp = k1 + k2 * p + (k3 + k4 * p + k5 * p^2 + k6 * p^k7 + k8 * p^k9 + ...
    k10 * p^k11 + k12 * k13^(k14*p) ) * (Tfuel / Tref)^(1 + k15 + k16 * p)
