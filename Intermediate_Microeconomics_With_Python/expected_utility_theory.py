
# Von-Neumann-Morgenstern Expected Utility Theory
# WIP:
# TODO: Fix graphs.

import matplotlib.pyplot as plt
import numpy as np


# Utility function
def expected_utility(y):
    return (12 * y) / (2 + y)


# Linear utility function
def linear_utility(y):
    return y


secure_income = 4.5
price_ticket_A = 0.5
probability_win_A = 0.2
probability_no_win_A = 0.8
prize_win_A = 10

# Contingent consumption plan and expected utility for lottery ticket A
consumption_win_A = secure_income - price_ticket_A + prize_win_A
consumption_no_win_A = secure_income - price_ticket_A
expected_utility_A = probability_win_A * expected_utility(consumption_win_A) + probability_no_win_A * expected_utility(
    consumption_no_win_A)

# Certainty equivalent and risk premium
certainty_equivalent = consumption_no_win_A
risk_premium = expected_utility(secure_income) - certainty_equivalent

# Decision whether to buy lottery ticket A
decision_A = certainty_equivalent >= secure_income

# Plot utility function for A
y_values_A = np.linspace(0, 15, 100)
utility_values_A = expected_utility(y_values_A)

# Plot decision points for A
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
plt.plot(y_values_A, utility_values_A, label='Expected Utility Function (A)', color='blue')
plt.scatter([secure_income, consumption_win_A, consumption_no_win_A],
            [expected_utility(secure_income), expected_utility_A, expected_utility_A],
            color=['red', 'green', 'green'], label='Decision Points (A)')
plt.title('Expected Utility Function and Decision Points (A)')
plt.xlabel('Income (y)')
plt.ylabel('Expected Utility')
plt.axhline(y=0, color='black', linewidth=0.5, linestyle='--', label='Zero Utility')
plt.legend()
plt.grid(True)

# Certainty Equivalent and Risk Premium Plot
plt.subplot(1, 2, 2)
plt.bar(['Certainty Equivalent', 'Risk Premium'], [certainty_equivalent, risk_premium], color=['purple', 'orange'])
plt.title('Certainty Equivalent and Risk Premium (A)')
plt.ylabel('Amount (Euros)')
plt.grid(True)

# Show the plot
plt.tight_layout()
plt.show()

# ---------------------------------------------------------------

# Lottery Ticket B
price_ticket_B = 1.5
probability_win_B = 0.2
probability_no_win_B = 0.8
prize_win_B = 15

# Contingent consumption plan and expected value for lottery ticket B
consumption_win_B = secure_income - price_ticket_B + prize_win_B
consumption_no_win_B = secure_income - price_ticket_B
expected_value_B = probability_win_B * consumption_win_B + probability_no_win_B * consumption_no_win_B
certainty_equivalent_B = consumption_no_win_B

# Decision whether to buy lottery ticket B
decision_B = expected_value_B >= expected_utility(secure_income)

# Plot utility function for B
y_values_B = np.linspace(0, 15, 100)
utility_values_B = expected_utility(y_values_B)

# Plot decision points for B
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
plt.plot(y_values_B, utility_values_B, label='Expected Utility Function (B)', color='blue')
plt.scatter([secure_income, consumption_win_B, consumption_no_win_B],
            [expected_utility(secure_income), expected_utility(expected_value_B), expected_utility(expected_value_B)],
            color=['red', 'orange', 'orange'], label='Decision Points (B)')
plt.title('Expected Utility Function and Decision Points (B)')
plt.xlabel('Income (y)')
plt.ylabel('Expected Utility')
plt.axhline(y=0, color='black', linewidth=0.5, linestyle='--', label='Zero Utility')
plt.legend()
plt.grid(True)

# Certainty Equivalent and Expected Value Plot
plt.subplot(1, 2, 2)
plt.bar(['Certainty Equivalent', 'Expected Value'], [certainty_equivalent_B, expected_value_B],
        color=['purple', 'green'])
plt.title('Certainty Equivalent and Expected Value (B)')
plt.ylabel('Amount (Euros)')
plt.grid(True)

# Show the plot
plt.tight_layout()
plt.show()

# ---------------------------------------------------------------

# Risk-neutral individual with linear utility function
expected_utility_A_risk_neutral = probability_win_A * linear_utility(
    consumption_win_A) + probability_no_win_A * linear_utility(consumption_no_win_A)

# Decision for lottery ticket A for a risk-neutral individual
decision_A_risk_neutral = expected_utility_A_risk_neutral >= secure_income

# Plot utility function for risk-neutral individual
y_values_risk_neutral = np.linspace(0, 15, 100)
utility_values_risk_neutral = linear_utility(y_values_risk_neutral)

# Plot decision points for risk-neutral individual
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
plt.plot(y_values_risk_neutral, utility_values_risk_neutral, label='Linear Utility Function', color='blue')
plt.scatter([secure_income, consumption_win_A, consumption_no_win_A],
            [linear_utility(secure_income), linear_utility(consumption_win_A), linear_utility(consumption_no_win_A)],
            color=['red', 'green', 'green'], label='Decision Points')
plt.title('Linear Utility Function and Decision Points (Risk-Neutral)')
plt.xlabel('Income (y)')
plt.ylabel('Utility')
plt.axhline(y=0, color='black', linewidth=0.5, linestyle='--', label='Zero Utility')
plt.legend()
plt.grid(True)

# Decision for Lottery Ticket A Plot
plt.subplot(1, 2, 2)
plt.bar(['Decision for Lottery A'], [decision_A_risk_neutral], color=['blue'])
plt.title('Decision for Lottery Ticket A (Risk-Neutral)')
plt.grid(True)

# Show the plot
plt.tight_layout()
plt.show()
