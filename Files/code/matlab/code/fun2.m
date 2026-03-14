%% gives a new section
%type help "FUNCTIONNAME"
clear; clc;
%% Section 2

A=[1,3;2,4];

%put a semicolon at the end, otherwise it will show everything in the command window

B=A*A

C = A.*A

%% Section 3

D = A(1,:);

%double dots gives you all of the first row

E = A(:,2)

D = A(1,:) %returns the first row of matrix A, [1 3].
E = A(:,2) %returns the second column of matrix A, [3 4]T

i = 2:2:10 %creates the row vector i = [2 4 6 8 10] in steps of 2.
F = max(A) %returns the max along the columns of A, [2 4].
G = max(A,[],2) %returns the max along the rows of A, [3 4]T

%% Section 4

%also set the seed correct
% draws random marks for the macro students

% cell of strings with students' names
students = {'Andreas','Aulis','Alex','Lukas', 'Ran'}
n = size(students,2); % class size

for i=1:n % loop over n students
student = students{i}; % reference ith student
mark = randi(100,1,1); % random integer between 1 and 100
if mark>=60
result = 'passed';
else
result = 'failed';
end
% print result for ith student
fprintf('%-7s %-6s the module.\n',student,result)
end

%allocate deterministic marks for the macro students-> Load get_mark
%function-> you can delegate some parts in other files

% cell of strings with student names
students = {'Andreas','Ran','Alex','Lukas','Aulis', 'Peter'};
n = size(students,2); % class size

for i=1:n % loop over n students
    student = students{i};          % reference ith student
    mark    = get_mark(student);    % calls function get_mark()
    if mark>=60
        result = 'passed';
    elseif mark<60
        result = 'failed';
    else
        result = 'not enrolled in'; %thats new
    end
    % print result for ith student
    fprintf('%-7s %-6s the module.\n',student,result)
end


%% Section 5

%Programming style

%obvious names for variables, first make sure the code is correct, then do
%it fast

%Avoid loops when possible. In Matlab loops are generally slower than
%vectorized operations (matrices). On my PC, the code

n = 10^4; m = 10^4; y = zeros(n,m);
tic; for i=1:n, for j=1:m, y(i,j)=i*j; end; end; toc;

tic; y = kron([1:1:10^4]',[1:1:10^4]); toc;

%Running times get even longer for nested loop. You can use tic before and toc after a statement to measure running times.


%%
% Section 6 (Rootfinding, where is a particulary function zero = Steady State.

%Anonymous function-> not located in a separate file

%Use the Solow model (Steady state is when new capital matches the depreciation)

% Set parameters
% Define Anonymous functions
% Use the built-in routine fsolce to find the root(s) of h(k)

% The following solves for the steady-state capital stock in the Solow model
% parameters

alpha = .33    % capital income share
delta = .10    % depreciation rate
s     = .30    % saving rate

% define anonymous functions-> directly here
f = @(k) k.^alpha
h = @(k) s*f(k)-delta*k




%%
%fsolve is the easiest way to solve especially but it works when it is only on variable
%as fsolve not works, I use fzero
%do not close to zero-> get an idea where the stable steady state is->
%trying different guesses
% solve for the root of h(k)
k0 = 5                                % inital guess-> important to make a good guess-> makes a lot easier
options = optimset('Display','iter')   % display iterations
kstar   = fzero(h,k0,options)

%%
% First Example

%Neoclassic Growth Model-> see the script for more details
%Endogenous the saving rates->

clear all
%Value function iteration algorithm
%Iterate the output of the new Value Function and plug it back in until you get the same out as you give in
%Choose functional forms and parameters of preferences, u(·), and technology, f(·).
%Compute the steady-state capital stock, k*=g(k*)
%look at the script for more information
%always have to discretize it

%1. functional forms and parameters
% parameters
alpha = .36;    % capital income share
beta = .95;     % subjective discount factor
delta = .10;    % depreciation rate
sigma = 2;      % relative risk aversion parameter

% functional forms
u = @(c) c.^(1-sigma)/(1-sigma)-1/(1-sigma); % preferences-> CAS Function
f = @(k) k.^alpha; % technology
f1 = @(k) alpha*k.^(alpha-1); % rental rate-> First derivative w.r.t k (if you lend this amout of to another firm) 

%%
% 2. solve for steady-state capital stock
% use anonymous function
steady_state_condition = @(k) beta*(f1(k)+(1-delta))-1;
k0 = 4; % initial guess
options = optimset('Display','iter'); % display iterations
% use built-in rootfinding
kstar = fzero(steady_state_condition,k0,options);
kstar

%Policy Function, Value Function and Consumption Function is to find-> the
%better the guess, the less iterations it takes to converge


%%
%do get more simulations and use the points around the steady state

n = 500 % number of grid points
% set up grid around the steady state
range = .10
kmin = (1-range)*kstar % lower bound of the grid
kmax = (1+range)*kstar % upper bound of the grid
% equally spaced grid (column vector)
kgrid = linspace(kmin,kmax,n)'
% transformation into matrices that can be used for the evaluation
% of functions. kmat varies along columns, kpmat varies along rows.
[kmat,kpmat] = ndgrid(kgrid,kgrid) % kp denotes k'

kmat(1:10, 1)
kpmat(1:10, 1)


%use this output for capital today (kmat) and capital tomorrow (kpmat)

%%
% 4. value function iteration

% initial guess: one-period problem, thus kp=0.
V = u(f(kgrid)+(1-delta)*kgrid);

% continuation values and consumption possibilities for any
% combination of k and kp.
Vpmat = u(f(kpmat)+(1-delta)*kpmat);
cmat = f(kmat)+(1-delta)*kmat-kpmat;

% momentary utility
% map negative consumption values into very low utility level-> so we do not get negative utility
umat = (cmat<=0).*(-10^6)+(cmat>0).*u(cmat);
% set convergence tolerance
tol = 10^(-8);
maxit = 500; %you can also extend-> so you can track how many iterations it needs
fprintf('iter norm \n');

tic
for j=1:maxit
% search for kp = argmax umat+beta*Vmat for each k
% max is along dimension two (row)
[Vnext,indices] = max(umat+beta*Vpmat,[],2);
error = max(abs(Vnext-V));
fprintf('%4i %2.1e \n',j,error);
% stopping rule
if error < tol
fprintf('Elapsed Time = %4.2f Seconds\n',toc);
break;
else
    V = Vnext;
    ctil = f(kgrid)+(1-delta)*kgrid-kgrid(indices);
for h=1:10
    V = u(ctil)+beta*V(indices);
end
Vpmat = repmat(V',n,1);
end
end
k = kgrid; kp = kgrid(indices); c = f(k)+(1-delta)*k-kp;

%gives out the deviation from the updated value function (getting smaller and smaller)

%%

% 5. plot unknown functions
scrsz = get(0,'ScreenSize');
figure('Position',[scrsz(3)*1/4 scrsz(4)*1/4 scrsz(3)*1/2 ...
scrsz(4)*1/2]);

% policy function
subplot(2,2,1)
plot(k,[k,kp]); % plot k and kp at the same time
title('Policy Function for Capital');
xlabel('k'); ylabel('g(k)');
legend('45◦-line','Policy Function','Location','Best');

% value function
subplot(2,2,2)
plot(k,V);
title('Value Function');
xlabel('k'); ylabel('V(k)');

% consumption function
subplot(2,2,3)
plot(k,c);
title('Consumption Function');
xlabel('k'); ylabel('c(k)');
