function [rdotv, energy] = CalculateApsidesFromSat(sat)

rmag = sqrt(sat.X^2 + sat.Y^2 + sat.Z^2);
vmag = sqrt(sat.VX^2 + sat.VY^2 + sat.VZ^2);

rdotv=(sat.X*sat.VX+sat.Y*sat.VY+sat.Z*sat.VZ)/(rmag*vmag);

mu = 398600.4415
energy = vmag^2 / 2 - mu / rmag;