from sympy import symbols, diff, solve

# Definitions
C, F, L, T, M, p, w, alpha, lambda1, lambda2 = symbols('C F L T M p w alpha lambda1 lambda2')

# Given utility function
utility_function = C**alpha * F     # where 0 < alpha < 1

# Lagrangian function
Lagrangian = utility_function + lambda1 * (T - F - L) + lambda2 * (w*L + M - p*C)

# First order conditions
FOC_C = diff(Lagrangian, C)
FOC_F = diff(Lagrangian, F)
FOC_L = diff(Lagrangian, L)
FOC_lambda1 = diff(Lagrangian, lambda1)
FOC_lambda2 = diff(Lagrangian, lambda2)

# The FOCs are:
# -lambda2*p + C**alpha*F*alpha/C
# C**alpha - lambda1
# -lambda1 + lambda2*w
# -F - L + T
# -C*p + L*w + M

# Solve for demand for consumption C*, demand for leisure time F*, labour supply L*
sol = solve(
    (FOC_C, FOC_F, FOC_L, FOC_lambda1, FOC_lambda2),
    (C, F, L)
)
print(sol)
# C* = (lambda1**(1/alpha)
# F* = M + T*w - lambda1**(1/alpha)*p)/w
# L* = -M + lambda1**(1/alpha)*p)/w

# Q: Is Leisure a normal good?
# F* = M + T*w - lambda1**(1/alpha)*p)/w
# check slope F* w.r.t. M (nonlabor income e.g. wealth)

# F_star = M + T*w - lambda1**(1/alpha)*p)/w

# F_star diff w.r.t  M  yields 1 > 0 -> positive slope = leisure is a normal good.

# Q: Is  Labour Supply Curve Backward-Bending (if higher wage -> does individual work less ?)

# L_star = -M + lambda1**(1/alpha)*p)/w
# check slope L* w.r.t. w (hourly wage)

# Define the expression L_star
L_star = -M + (lambda1**(1/alpha)*p)/w

# Differentiate L_star with respect to w
L_diffW = diff(L_star, w)
print(L_diffW)
# -lambda1**(1/alpha)*p/w**2

# TODO: check effect for different wage ranges

# Calculate an Optimal Leisure-Consumption Bundle for Given Parameters

# Substituting alpha = 0.5, p = 1, T = 24 for the initial scenario
alpha_value = 0.5
p_value = 1
T_value = 24

# Substituting these values into the FOCs
FOC_C_sub = FOC_C.subs({alpha: alpha_value, p: p_value})
FOC_F_sub = FOC_F.subs({alpha: alpha_value})
FOC_L_sub = FOC_L
FOC_lambda1_sub = FOC_lambda1.subs({T: T_value})
FOC_lambda2_sub = FOC_lambda2.subs({p: p_value})

# Solve the system of equations
solutions1 = solve(
    (FOC_C_sub, FOC_F_sub, FOC_L_sub, FOC_lambda1_sub, FOC_lambda2_sub),
    (C, F, L, lambda1, lambda2))
print(solutions1)

# where Scenario 1: M = 12, w = 4
M_value1 = 12
w_value1 = 4

print(solutions1[0][0])

FOC_C_sub_sub1 = solutions1[0][0].subs({M: M_value1, w: w_value1})
FOC_F_sub_sub1 = solutions1[0][1].subs({M: M_value1, w: w_value1})
FOC_L_sub_sub1 = solutions1[0][2].subs({M: M_value1, w: w_value1})
FOC_lambda1_sub_sub1 = solutions1[0][3].subs({M: M_value1, w: w_value1})
FOC_lambda2_sub_sub1 = solutions1[0][4].subs({M: M_value1, w: w_value1})

print(FOC_C_sub_sub1) # 36
print(FOC_F_sub_sub1) # 18
print(FOC_L_sub_sub1) # 6
print(FOC_lambda1_sub_sub1) #6
print(FOC_lambda2_sub_sub1) #1,5

# where Scenario 2:
# wage cut 50%, increased wealth (no labor income):
# M = 30, w = 2
M_value2 = 30
w_value2 = 2

FOC_C_sub_sub2 = solutions1[0][0].subs({M: M_value2, w: w_value2})
FOC_F_sub_sub2 = solutions1[0][1].subs({M: M_value2, w: w_value2})
FOC_L_sub_sub2 = solutions1[0][2].subs({M: M_value2, w: w_value2})
FOC_lambda1_sub_sub2 = solutions1[0][3].subs({M: M_value2, w: w_value2})
FOC_lambda2_sub_sub2 = solutions1[0][4].subs({M: M_value2, w: w_value2})

print(FOC_C_sub_sub2) # 26
print(FOC_F_sub_sub2) # 26
print(FOC_L_sub_sub2) # -2
print(FOC_lambda1_sub_sub2) # 5
print(FOC_lambda2_sub_sub2) # 2,55

### Effect: 36h/w -> 26 h/w

