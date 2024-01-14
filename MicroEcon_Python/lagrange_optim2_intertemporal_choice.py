from sympy import symbols, diff, solve

# Define symbols
c1, c2, s, lambda_, mu, alpha, beta, m1, m2, r = symbols('c1 c2 s lambda mu alpha beta m1 m2 r')

# Utility function
u = c1 ** alpha * c2 ** beta
# Given to periods t = [1,2], mt is the individual's income in each period. r denotes the interest rate.
# Therefore, the budget constraints are
# for t = 1: c1 + s = m1
# for t = 2: c2 = m2 + s*(1+r)

# Define Lagrangian function for utility maximization with budget constraints
L = u + lambda_ * (m1 - c1 - s) + mu * (m2 + s * (1 + r) - c2)

# Calculate first-order conditions
foc_c1 = diff(L, c1)
foc_c2 = diff(L, c2)
foc_s = diff(L, s)
foc_lambda = diff(L, lambda_)
foc_mu = diff(L, mu)

# Solve for optimal consumption in each period
ct_Star_solutions = solve((foc_c1, foc_c2, foc_s, foc_lambda, foc_mu), (c1, c2))

# Display first-order conditions and solutions
print("First-order conditions:")
print("=======================")
print(foc_c1)
print(foc_c2)
print(foc_s)
print(foc_lambda)
print(foc_mu)
print("==============================")
print("Optimal consumption solutions:")
print(ct_Star_solutions)

# Define specific values for parameters
alpha_val = 0.7
beta_val = 0.5
m1_val = 342
m2_val = 297
r_val = 0.1

s_val = m1_val - c1
# Redefine equations with specific values
eq1 = foc_c1.subs({alpha: alpha_val, beta: beta_val, m1: m1_val, m2: m2_val, r: r_val, s: s_val})
eq2 = foc_c2.subs({alpha: alpha_val, beta: beta_val, m1: m1_val, m2: m2_val, r: r_val, s: s_val})
eq3 = foc_s.subs({alpha: alpha_val, beta: beta_val, m1: m1_val, m2: m2_val, r: r_val, s: s_val})
eq4 = foc_lambda.subs({alpha: alpha_val, beta: beta_val, m1: m1_val, m2: m2_val, r: r_val, s: s_val})
eq5 = foc_mu.subs({alpha: alpha_val, beta: beta_val, m1: m1_val, m2: m2_val, r: r_val, s: s_val})
#eq4 = 0

# Solve for consumption bundle
consumption_bundle_sol = solve((eq1, eq2, eq3, eq5), (mu, lambda_, c1, c2))
print(consumption_bundle_sol)

print("Consumption bundle solutions:")
print("=============================")
print(consumption_bundle_sol)
c1_star_sol2 = 357
c2_star_sol2 = 280.5

# Since m1_val - c1_star_sol2 < 0 -> s is negative -> borrower

# Calculate Marginal Rate of Substitution (MRS)
u_c1 = u.diff(c1)
u_c2 = u.diff(c2)
MRS = -u_c1 / u_c2
print(MRS)

# Substitute specific values into MRS
MRS_sub = MRS.subs({alpha: alpha_val, beta: beta_val, c1: m1_val - s, c2: 1.1 * s + m2_val})
print(MRS_sub)

# Calculate a specific instance of MRS
s_val_example = 21.43  # example savings value
MRS_calc = -0.7 * (1.1 * s_val_example + m2_val) / (0.5 * (m1_val - s_val_example))
print(MRS_calc)

# Calculate the rate of pure time preference
rho = -(MRS_calc + 1)
print(rho)
