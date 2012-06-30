clear classes; clc
lagInt = LagrangeInterpolator;
lagInt.setIndependentVariables([-1:.25:1]);
funcValues = [ones(lagInt.numPoints,1)  cos(lagInt.indVar)  3*(lagInt.indVar) + 2];
derivValues = [zeros(lagInt.numPoints,1) -sin(lagInt.indVar) 3*ones(lagInt.numPoints,1)];
indVar = -.2;
interpValues = lagInt.Interpolate(indVar,funcValues);

D = lagInt.GetDiffMatrix;

% test interpolated values
interpValues - [1 cos(indVar) 3]';

% test derivatives at interpolation points
disp('Approximate Derivative Values')
D*funcValues 
disp('Exact Derivative Values')
derivValues
disp('Error')
D*funcValues - derivValues


data =[21545.00000039794       7100                        0                           1300                        0                            7.35                         1                            
21545.00069484238       7086.464536250401           440.7200320631401           1357.477178439189           -0.4507028005911404          7.336012339785102            0.9153591112483409           
21545.00179038239       7010.441225691242           1131.938118925952           1437.563846673179           -1.153559144663115           7.257649252482808            0.7756543139940341           
21545.00294701277       6858.758554770789           1850.044676353111           1507.417023822887           -1.878649835411161           7.101805996341613            0.6213076027513466           
21545.00411448981       6633.354901384091           2555.369423623841           1561.993531503428           -2.585822287421512           6.870960559877937            0.4600316386294969           
21545.00528682664       6336.789134107926           3236.576694974231           1600.219631802338           -3.264090035133177           6.568133031507412            0.2942590277430324           
21545.00646314781       5972.067087011202           3885.80291586003            1621.576513196519           -3.906115622984577           6.196691738034612            0.1257999123303987           
21545.00764309871       5542.898639870431           4495.832426387871           1625.763256041935           -4.505308357135157           5.760603446018727            -0.04359122574319205         
21545.0088263626        5053.721118986773           5059.879361203987           1612.6714585879             -5.055573146955019           5.264447703862056            -0.2121675700953943          
21545.01001259817       4509.668762150079           5571.631408457334           1582.385582230221           -5.551322676918877           4.71338191209398             -0.3781970265543949          
21545.01120142654       3916.526832799962           6025.321543820922           1535.184275946182           -5.987517286065778           4.11309707211973             -0.5399756329597379          
21545.01157447201       3721.531273392379           6154.722664589231           1516.979841628565           -6.111240254345921           3.915806693172207            -0.5895369031485154          
];

timeData = (data(:,1) - data(1,1))*86400; 
funcValues  = data(:,2:7);
lagInt.setIndependentVariables(timeData);
derivValues = funcValues(:,4:6)
interpValues = lagInt.Interpolate(800,funcValues);
D = lagInt.GetDiffMatrix;
disp('Approximate Derivative Values')
D*funcValues 
disp('Exact Derivative Values')
derivValues
disp('Error')
D*funcValues(:,1:3) - derivValues

D(1,:)*funcValues

