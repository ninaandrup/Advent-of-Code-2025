#!/usr/bin/env python3
"""Plot 2D points from a file where each line contains x,y coordinates."""

import sys
import matplotlib.pyplot as plt

def plot_points(filename):
    """Read points from file and plot them."""
    points = []
    
    try:
        with open(filename, 'r') as f:
            for line in f:
                line = line.strip()
                if line:  # Skip empty lines
                    x, y = map(int, line.split(','))
                    points.append((x, y))
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)
    except ValueError:
        print("Error: Each line must contain two comma-separated integers (x,y).")
        sys.exit(1)
    
    if not points:
        print("Error: No points found in file.")
        sys.exit(1)
    
    # Extract x and y coordinates
    xs, ys = zip(*points)
    
    # Create the plot
    plt.figure(figsize=(10, 8))
    
    # Draw lines between consecutive points
    for i in range(len(points) - 1):
        x1, y1 = points[i]
        x2, y2 = points[i + 1]
        plt.plot([x1, x2], [y1, y2], 'b-', alpha=0.5)
    
    # Draw points (small, no labels)
    plt.scatter(xs, ys, s=20, alpha=0.8, color='blue', edgecolors='none')
    
    # Add labels and title
    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title(f'2D Points from {filename}')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()

if __name__ == '__main__':
    plot_points("input/Day09.txt")
