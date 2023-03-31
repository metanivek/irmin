import argparse
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import os

parser = argparse.ArgumentParser(description='Generate a plot of the average medians in each file in a directory.')
parser.add_argument('directory', type=str, help='the directory path containing the files')
args = parser.parse_args()
directory = args.directory

def file_info(filename):
    parts = filename.split('.')
    store = parts[0]
    order = parts[1]
    # Parse lru size to remove the run number
    lrusize = parts[2].split('-')[0]
    return (store, order, lrusize)

medians = {}
for filename in os.listdir(directory):
    if filename.endswith('.txt'):
        store, order, lru = file_info(filename)
        key = (store, order, lru)
        with open(os.path.join(directory, filename)) as f:
            numbers = []
            for line in f:
                # Skip lines with a "-" to indicate no number
                if line.strip() == '-':
                    continue
                numbers.append(float(line.strip()))
        med = np.median(numbers)
        if key in medians:
            medians[key].append(med)
        else:
            medians[key] = [med]

averages = {}
for (store, order, lru), medians in medians.items():
    label = "{}-{}".format(store, order)
    if lru not in averages:
        averages[lru] = {}
    if label not in averages[lru]:
        averages[lru][label] = []
    averages[lru][label].append(np.mean(medians))

# Plot
plt.rcParams["font.family"] = "monospace"
plt.rcParams["font.size"] = 16

fig, ax = plt.subplots()
total_width = 0.8
colors = plt.rcParams['axes.prop_cycle'].by_key()['color']
n_bars = len(averages.keys())
bar_width = total_width / n_bars
bars = []
labels = [
    "single_suffix-LH", "single_suffix-HL",
    "small_volume-LH", "small_volume-HL",
    "large_volume-LH", "large_volume-HL",
]
# labels = [
#     "small_volume_no_lower-LH", "small_volume_no_lower-HL",
#     "large_volume_no_lower-LH", "large_volume_no_lower-HL",
# ]
for i, (lru, info) in enumerate(averages.items()):
    x_offset = (i - n_bars / 2) * bar_width + bar_width / 2
    for x, label in enumerate(labels):
        x = x + x_offset
        y = info[label]
        for val in y:
            ax.text(x, val + 50, '{:.2f}'.format(val), horizontalalignment='center')
        bar = ax.bar(x, y, width=bar_width * 0.9, color=colors[i % len(colors)], zorder=2)
    bars.append(bar[0])
ax.yaxis.grid(zorder=0)
ax.legend(bars, averages.keys())
ax.set_title('Comparing commit load time medians for different LRU sizes', pad=25)
ax.set_xlabel('Store-Order', labelpad=25)
ax.set_ylabel('Average Median (ms)', labelpad=25)
ax.set_xticks(range(len(labels)), labels)
plt.show()
