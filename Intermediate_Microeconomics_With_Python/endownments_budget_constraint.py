# Endownments in the BC

import sympy as sp
import numpy as np
import matplotlib.pyplot as plt

# Define symbols
x1, x2, lambda_, k = sp.symbols('x1 x2 lambda, k')
p1, p2, omega1, omega2 = sp.symbols('p1 p2 omega1 omega2')

# Utility function and Lagrangian
utility = x1 * x2

# Indifference equation
indifference_equation = utility - k

# Solve for x2 in terms of x1
indifference_curve_solution = sp.solve(indifference_equation, x2)
print(indifference_curve_solution)

lagrangian = utility - lambda_ * (p1 * x1 + p2 * x2 - p1 * omega1 - p2 * omega2)

# Solve first-order conditions for an interior optimum
foc_x1 = sp.diff(lagrangian, x1)
foc_x2 = sp.diff(lagrangian, x2)
foc_lambda = sp.diff(lagrangian, lambda_)

optimal_solution = sp.solve([foc_x1, foc_x2, foc_lambda], (x1, x2, lambda_))
# {lambda: (omega1*p1 + omega2*p2)/(2*p1*p2), x1: (omega1*p1 + omega2*p2)/(2*p1), x2: (omega1*p1 + omega2*p2)/(2*p2)}

x1_star = (omega1*p1 + omega2*p2)/(2*p1)
x2_star = (omega1*p1 + omega2*p2)/(2*p2)

print(optimal_solution)

print(f"x1_star: {x1_star}")
print(f"x2_star: {x2_star}")

# Calculate net demand functions
net_demand_x1 = x1_star - omega1
net_demand_x2 = x2_star - omega2

# Display net demand functions
print(f"Net Demand Function for Good 1: {net_demand_x1}")
print(f"Net Demand Function for Good 2: {net_demand_x2}")


# Substitute specific parameter values
p1_val, p2_val = 2, 3
omega1_val, omega2_val = 9, 2

# Substitute values into the general solutions
x1_optimal_val = x1_star.subs({p1: p1_val, p2: p2_val, omega1: omega1_val, omega2: omega2_val})
x2_optimal_val = x2_star.subs({p1: p1_val, p2: p2_val, omega1: omega1_val, omega2: omega2_val})
net_demand_x1_val = net_demand_x1.subs({p1: p1_val, p2: p2_val, omega1: omega1_val, omega2: omega2_val})
net_demand_x2_val = net_demand_x2.subs({p1: p1_val, p2: p2_val, omega1: omega1_val, omega2: omega2_val})

# Display specific solutions
print("\nSpecific Solutions with p1 = 2, p2 = 3, omega1 = 9, omega2 = 2:")
print(f"Optimal Consumption Bundle: x1 = {x1_optimal_val}, x2 = {x2_optimal_val}")
print(f"Net Demand Function for Good 1: {net_demand_x1_val}")
print(f"Net Demand Function for Good 2: {net_demand_x2_val}")

# Plotting the budget line and optimal consumption bundle
x_range = np.linspace(2, 10, 100)
budget_line = (p1_val * omega1_val + p2_val * omega2_val - p1_val * x_range) / p2_val

indifference_curve_solution = (x1_optimal_val*x2_optimal_val) / x_range
plt.plot(x_range, indifference_curve_solution, label='Indifference Curve', linestyle='--')

# Define the new endowment bundles
new_endowment1 = (3, 8)
new_endowment2 = (4.5, 5)

# Plot the new budget lines
budget_line_new1 = (p1_val * new_endowment1[0] + p2_val * new_endowment1[1] - p1_val * x_range) / p2_val
budget_line_new2 = (p1_val * new_endowment2[0] + p2_val * new_endowment2[1] - p1_val * x_range) / p2_val

# Plot the budget lines
plt.plot(x_range, budget_line, label='Initial Budget Line')
plt.plot(x_range, budget_line_new1, label='New Budget Line (Endowment = (3, 8))')
plt.plot(x_range, budget_line_new2, label='New Budget Line (Endowment = (4.5, 5))')

# Plot the optimal consumption bundle
plt.scatter(float(x1_optimal_val), float(x2_optimal_val), color='red', label='Optimal Bundle')

# Plot the initial endowment point
plt.scatter(float(omega1_val), float(omega2_val), color='green', label='Initial Endowment')

# Draw arrows from initial endowment to optimal bundle
arrow_props_black = dict(facecolor='black', edgecolor='black', arrowstyle='->', linewidth=2)
plt.annotate('', xy=(float(x1_optimal_val), float(omega2_val)),
             xytext=(float(omega1_val), float(omega2_val)),
             arrowprops=arrow_props_black, label='Movement on Horizontal Axis')

plt.annotate('', xy=(float(omega1_val + net_demand_x1_val), float(x2_optimal_val)),
             xytext=(float(omega1_val + net_demand_x1_val), float(omega2_val)),
             arrowprops=arrow_props_black, label='Movement on Vertical Axis')

plt.xlabel('Good 1 (x1)')
plt.ylabel('Good 2 (x2)')
plt.title('Optimal Consumption Bundle, Budget Lines, and Movements')
plt.legend()
plt.grid(True)
plt.show()