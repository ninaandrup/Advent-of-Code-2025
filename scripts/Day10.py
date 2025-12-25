import pulp as pl
import re


def parse_line(line):
    """
    Parse a line from Day10_Example.txt

    Format: [pattern] (indices) (indices) ... {results}

    Returns:
        buttons: list of binary lists based on indices
        results: list of result values
    """
    # Extract the pattern between square brackets to get n
    pattern_match = re.search(r"\[([^\]]+)\]", line)
    if not pattern_match:
        raise ValueError("No pattern found in square brackets")
    n = len(pattern_match.group(1))

    # Extract all parentheses groups for button definitions
    button_groups = re.findall(r"\(([^\)]+)\)", line)
    buttons = []
    for group in button_groups:
        # Create a list of zeros of length n
        button = [0] * n
        # Parse the indices (can be single number or comma-separated)
        indices = [int(x.strip()) for x in group.split(",")]
        # Set the specified indices to 1
        for idx in indices:
            button[idx] = 1
        buttons.append(button)

    # Extract the results from curly braces
    results_match = re.search(r"\{([^\}]+)\}", line)
    if not results_match:
        raise ValueError("No results found in curly braces")
    results = [int(x.strip()) for x in results_match.group(1).split(",")]

    return buttons, results


def solve_problem(buttons, res):
    """
    Solve the optimization problem using PuLP.

    Args:
        n: Length of the pattern
        buttons: List of button definitions (binary lists)
        res: List of result values

    Returns:
        The optimized objective function value.
    """
    # Use the parsed data
    xs = [
        pl.LpVariable("x" + str(i), lowBound=0, cat=pl.LpInteger)
        for i in range(len(buttons))
    ]

    buttonsT = list(map(list, zip(*buttons)))

    prob = pl.LpProblem("Simple_Problem", pl.LpMinimize)
    objF = pl.lpSum(xs)
    prob += objF

    for b, r in zip(buttonsT, res):
        constraint = pl.lpSum([bi * xi for (bi, xi) in zip(b, xs)]) == r
        prob += constraint

    prob.solve()

    # The optimised objective function value is printed to the screen
    return pl.value(prob.objective)


# Read and process the Day10_Example.txt file
with open("input/Day10.txt", "r") as f:
    lines = f.readlines()

results = []
for i, line in enumerate(lines, 1):
    line = line.strip()
    if not line:
        continue

    buttons, res = parse_line(line)
    result = solve_problem(buttons, res)
    print(f"Line {i}: {result}")
    results.append(result)

print()
print(f"Sum of all results: {sum(results)}")
