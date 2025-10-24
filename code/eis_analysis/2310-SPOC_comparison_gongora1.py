# Aldair E. Gongora

#%%
import matplotlib.pyplot as plt
import numpy as np

#%%

# create data
x = ['A', 'B', ]
y = [0, 10, 20, 30, 40, 50]

plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = ['Times New Roman']
plt.rcParams['font.size'] = 12

#y1 = np.array([2, 2])
y1 = np.array([2, 2])

#y2 = np.array([1.5, 2])
y2 = np.array([2, 1.5])

#y3 = np.array([8, 32])
y3 = np.array([32, 8])

#y4 = np.array([1.5, 1.5])
y4 = np.array([1.5, 1.5])

#y5 = np.array([1, 5])
y5 = np.array([10, 1])
 
plt.figure(figsize=(5/2,8/2))

# plot bars in stack manner
#plt.bar(x, y1, color='tab:red',edgecolor='k',alpha=0.6)
#plt.bar(x, y2, bottom=y1, color='tab:blue',edgecolor='k',alpha=0.6)
#plt.bar(x, y3, bottom=y1+y2, color='tab:orange',edgecolor='k',alpha=0.6,hatch='/')
#plt.bar(x, y4, bottom=y1+y2+y3, color='tab:green',edgecolor='k',alpha=0.6)
#plt.bar(x, y5, bottom=y1+y2+y3+y4, color='tab:purple',edgecolor='k',alpha=0.6)

plt.bar(x[0], y1[0], color='tab:red',alpha=0.8)
plt.bar(x[1], y1[1], color='tab:red',alpha=0.8)
plt.bar(x[0], y2[0], bottom=y1[0], color='tab:blue',alpha=0.8)
plt.bar(x[1], y2[1], bottom=y1[1], color='tab:blue',alpha=0.8)
plt.bar(x[0], y3[0], bottom=y1[0] + y2[0], color='tab:orange',alpha=0.8)
plt.bar(x[1], y3[1], bottom=y1[1] + y2[1], color='tab:orange',alpha=0.8, edgecolor = 'red', linewidth = 0, hatch = '//')
plt.bar(x[0], y4[0], bottom=y1[0] + y2[0] + y3[0], color='tab:green',alpha=0.8)
plt.bar(x[1], y4[1], bottom=y1[1] + y2[1] + y3[1], color='tab:green',alpha=0.8)
plt.bar(x[0], y5[0], bottom=y1[0] + y2[0] + y3[0] + y4[0], color='tab:purple',alpha=0.8)
plt.bar(x[1], y5[1], bottom=y1[1] + y2[1] + y3[1] + y4[1], color='tab:purple',alpha=0.8)




plt.ylim([0,50])

#plt.xlabel("Approach")
plt.ylabel("Time (hours)")
plt.xticks(x, ['Coin Cell', 'SPOC'])
#plt.yticks(y, labels='[0,10,20,30,40,50]')
#plt.legend(["Round 1", "Round 2"])
#plt.title("Scores by Teams in 4 Rounds")
plt.tight_layout()

plt.savefig('C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\2024_09_26_SPOC_Figure4_ManualvsSpoc_TimeBarplot_v1.png',dpi=600)


#%% bar plot for experiments 

num_uniquesamples = np.array([277,868])
x = ['Coin Cell', 'SPOC']

plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = ['Times New Roman']
plt.rcParams['font.size'] = 12


plt.figure(figsize=(5/2,8/2))
plt.bar(np.array([0,1]), num_uniquesamples,color='tab:grey',edgecolor='k',alpha=0.6)
plt.ylim([0,800])
plt.ylabel("Number of Unique Samples")

plt.xticks(np.arange(len(x)), x)

plt.tight_layout()
plt.savefig('C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\2024_09_26_SPOC_Figure4_ManualvsSpoc_UniqueSamples_v1.png',dpi=600)

#%%
num_measurements = np.array([701,2463])
x = ['Coin Cell', 'SPOC']

plt.figure(figsize=(5/2,8/2))
plt.bar(np.array([0,1]), num_measurements,color='silver',edgecolor='k',alpha=0.6)
plt.ylabel("Number of Measurements")
plt.xticks(np.arange(len(x)), x)

plt.ylim([0,2500])
plt.tight_layout()
plt.savefig('C:\\Users\\jimenez45\\OneDrive - LLNL\\General - High-Throughput Polymer Electrolytes DIW\\Manuscripts\\2409-PlatformFocus\\figs\\2024_09_26_SPOC_Figure4_ManualvsSpoc_NumMeasurements_v1.png',dpi=600)



# %%
