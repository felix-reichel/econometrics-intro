import sympy as sp

c1, c2, m1, m2, r, lambda_ = sp.symbols('c1 c2 m1 m2 r lambda_')

# Optimization problem
u = sp.ln(c1) + 0.8*sp.ln(c2)
budget_constraint_1 = c1 + (1 + r)*c2 - m1
budget_constraint_2 = c2/(1 + r) - m2

# Lagrangian function
lagrangian = u + lambda_*budget_constraint_1 + lambda_*budget_constraint_2

# FOCs
FOC_c1 = sp.diff(lagrangian, c1)
FOC_c2 = sp.diff(lagrangian, c2)
FOC_lambda = sp.diff(lagrangian, lambda_)
print("First-order condition for c1:")
print(FOC_c1)
print("\nFirst-order condition for c2:")
print(FOC_c2)
print("\nFirst-order condition for lambda:")
print(FOC_lambda)

# Solve systems of equations
optimal_solution = sp.solve(
    (FOC_c1, FOC_c2, FOC_lambda),
    (c1, c2, lambda_))
optimal_c1 = optimal_solution[0][0]
optimal_c2 = optimal_solution[0][1]
print("\nOptimal consumption in period 1 (c1*):", optimal_c1)
print("Optimal consumption in period 2 (c2*):", optimal_c2)

# Calculate MRS / Pure time preference
MRS = - sp.diff(u, c1) / sp.diff(u, c2)
MRS_at_optimal = MRS.subs({c1: 1, c2: 1})
is_pure_time_preference = MRS_at_optimal < -1
print("\nMarginal Rate of Substitution (MRS):", MRS_at_optimal)
print("Does the individual have a pure time preference? ", is_pure_time_preference)
