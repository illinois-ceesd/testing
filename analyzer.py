from operator import index
import os
from tkinter import font
from turtle import color
import pandas as pd 
import sqlite3
import glob
import sys 
import pathlib
import argparse
import os
import re
import matplotlib.pyplot as plt
import numpy as np
import random
import string


parser = argparse.ArgumentParser(description="python plot_performance.py -f <first step> -l <last step> ./path/to/my/sqlite/files")
parser.add_argument("-f", type=int, metavar='first step', help= "the first step")
parser.add_argument("-l", type=int, metavar='last step', help= "the last step")
parser.add_argument("-table", type=str, metavar='table', help= "the sqlite folder path")
parser.add_argument("-p", type=str, metavar='path', help= "the sqlite folder path")
parser.add_argument("-type", type=str, metavar='type', help= "sum or avg")
parser.add_argument("-annotate", type=bool, metavar='annotate', help= "true or false")

args = parser.parse_args()
paths = glob.glob(args.p + "*.sqlite")
df = pd.DataFrame() 
my_dict = {}
x_data = []
y_data = []
t_step_data = []
t_init_data = []
memory_usage_python = []
compile_time_data = []

casename = re.split('-', re.split('/', paths[0])[len(re.split('/', paths[0])) - 1])


for ele in paths:
    directory = re.split('/', ele) 
    file = directory[len(directory) - 1]  
    curr_case_name = re.split('-', file)
    con =  sqlite3.connect(ele)
    cur = con.cursor()
    start = curr_case_name[1].index('e')         
    end = curr_case_name[1].index('n',start+1) 
    global_elements = curr_case_name[1][start+1:end] 
    
    if args.table is not None:
        cur.execute(f''' SELECT count(name) FROM sqlite_master WHERE type='table' AND name='{args.table}' ''')
        if cur.fetchone()[0]==1 and curr_case_name[0] == casename[0]:
            other = (pd.read_sql_query(f"SELECT * from {args.table}", con)).iloc[args.f:args.l] # get specific range of steps from sqlite file
            
            if args.type == 'avg' :
                average = other.iloc[:,2].mean()
                y_data.append(average)
                x_data.append(int(global_elements))   
                
            elif args.type == 'sum' :        
                summation = other.iloc[:,2].sum()            
                y_data.append(summation)
                x_data.append(int(global_elements))
                
            else :
                df[global_elements] = other.iloc[:, 2]
    else:
        if args.type == 'sum' :
            x_data.append(int(global_elements))
            t_step_sum = (pd.read_sql_query(f"SELECT * from t_step", con).iloc[args.f:args.l,2]).sum() 
            t_step_data.append(t_step_sum)
            t_init = pd.read_sql_query(f"SELECT * from t_init", con).iloc[:,2]
            t_init_data.append(float(t_init))
            memory_usage_python_max = (pd.read_sql_query(f"SELECT * from memory_usage_python", con).iloc[args.f:args.l,2]).max()
            memory_usage_python.append(memory_usage_python_max)
        else:
            x_data.append(int(global_elements))
            t_step_avg = (pd.read_sql_query(f"SELECT * from t_step", con).iloc[args.f:args.l,2]).mean()
            t_step_data.append(t_step_avg)
            t_init = pd.read_sql_query(f"SELECT * from t_init", con).iloc[:,2]
            t_init_data.append(float(t_init))
            memory_usage_python_max = (pd.read_sql_query(f"SELECT * from memory_usage_python", con).iloc[args.f:args.l,2]).max()
            memory_usage_python.append(memory_usage_python_max)
            compile_time = (pd.read_sql_query(f"SELECT * from t_step", con).iloc[0:1,2])
            print(compile_time)
            compile_time_data.append(float(compile_time))
            
    
    con.close()
    
csfont = {'fontname':'DejaVu Sans'}   
def plot_all():
    fig, axes = plt.subplots(nrows=4, ncols=1)
    my_dict['nelem'] = x_data
    my_dict['t_step'] = t_step_data
    my_dict['t_init'] = t_init_data
    my_dict['memory max'] = memory_usage_python
    my_dict['compile time'] = compile_time_data
    df2 = pd.DataFrame(my_dict)
    df2 = df2.sort_values(by='nelem')

    fig, (ax1,ax2, ax3, ax4) = plt.subplots(nrows=4, sharex=True)
    ax5 = fig.add_subplot(111, zorder=-1)
    for _, spine in ax5.spines.items():
        spine.set_visible(False)
    ax5.tick_params(labelleft=False, labelbottom=False, left=False, right=False )
    ax5.get_shared_x_axes().join(ax5,ax1)
    ax5.get_shared_x_axes().join(ax5,ax2)
    ax5.get_shared_x_axes().join(ax5,ax3)
    ax5.grid(axis="x")

    line1 = ax1.plot(df2['nelem'],
                     df2['t_step'],
                     markerfacecolor='w',
                     ms=4,
                     marker='o',
                     color='orange',
                     label="t_step")
    ax1.set_ylabel("time (s)", csfont, fontsize=8)
    line1 = ax2.plot(df2['nelem'],
                     df2['t_init'],
                     ms=4,
                     markerfacecolor='w',
                     marker='o',
                     color='green',
                     label="t_init")
    ax2.set_ylabel("time (s)", csfont, fontsize=8)
    line1 = ax3.plot(df2['nelem'],
                     df2['memory max'],
                     ms=4,
                     markerfacecolor='w',
                     marker='o',
                     color='blue',
                     label="memory max")
    ax3.set_ylabel("time (s)", csfont, fontsize=8)
    line1 = ax4.plot(df2['nelem'],
                     df2['compile time'],
                     ms=4,
                     markerfacecolor='w',
                     marker='o',
                     color='purple',
                     label="compile time")
    ax4.set_ylabel("time (s)", csfont, fontsize=8)
    fig.legend(loc='upper left', labelspacing=.01, borderaxespad=.1, fontsize='small', markerscale=.6)
    fig.align_ylabels()
    ax1.grid()
    ax2.grid()
    ax3.grid()
    ax4.grid()
    plt.title(f"{casename[0]} with different mesh sizes between steps {args.f} - {args.l}", csfont, fontsize="small")
    plt.xlabel(f"Number of Elements", labelpad=20) 
    plt.show()
    
    
def plot_bar_sum():
    my_dict['nelem'] = x_data
    my_dict['sum'] = y_data 
    df2 = pd.DataFrame(my_dict)
    df2 = df2.sort_values(by='nelem')
    fig, ax = plt.subplots()
    df2.plot(x='nelem',
             y='sum',
             style='.-',
             grid=True,
             marker='o',
             ms=4,
             markerfacecolor='w',
             ax = ax).set(xlabel=f"Steps {args.f} - {args.l} ",
                          ylabel=args.table,
                          title=f"{casename[0]} with different mesh sizes")
    if args.annotate:
        for x,y in zip(x_data,y_data):
            label = x
            plt.annotate(label,
                        (x,y),
                        textcoords="offset points",
                        xytext=(0,10), 
                        ha='center',
                        fontsize=8) 
    plt.show()
    
def plot_bar_avg():
    my_dict['nelem'] = x_data
    my_dict['avg'] = y_data
    df2 = pd.DataFrame(my_dict)
    df2 = df2.sort_values(by='nelem')
    fig, ax = plt.subplots()
    df2.plot(x='nelem',
             y='avg',
             style='.-',
             grid=True,
             marker='o',
             ms=4,
             markerfacecolor='w',
             ax = ax).set(xlabel=f"Steps {args.f} - {args.l} ",
                          ylabel=args.table,
                          title=f"{casename[0]} with different mesh sizes")
    if args.annotate:
        print("whatup")
        for x,y in zip(x_data,y_data):
            label = x
            plt.annotate(label,
                        (x,y),
                        textcoords="offset points",
                        xytext=(0,10), 
                        ha='center',
                        fontsize=8) 
            
    plt.show()
    
def plot_steps():
    ax = df.plot.bar()
    ax.set(xlabel=f"Number of Elements",
           ylabel=args.table,
           title=f"{casename[0]} with different mesh sizes, Steps {args.f} - {args.l}")
    plt.show()
    
def box_plot():
    fig, ax = plt.subplots()
    ax.set_xticklabels(df.columns, rotation=0)
    ax.set(xlabel=f"Steps {args.f} - {args.l} ",
           ylabel=args.table,
           title=f"{casename[0]} with different mesh sizes")
    for n, col in enumerate(df.columns):
        ax.boxplot(df[col], positions=[n+1], notch=True)
    plt.show()
        
def get_x_tick_labels(df, grouped_by):
    tmp = df.groupby([grouped_by]).size()
    return ["{0}: {1}".format(k,v) for k, v in tmp.to_dict().items()]

def series_values_as_dict(series_object):
    tmp = series_object.to_dict().values()
    return [y for y in tmp][0]

def add_values(bp, ax):
    for element in ['whiskers', 'medians', 'caps']:
        for line in bp[element]:
            
            
            (x_l, y),(x_r, _) = line.get_xydata()

            if not np.isnan(y): 
                x_line_center = x_l + (x_r - x_l)/2
                y_line_center = y  # Since it's a line and it's horisontal
                # overlay the value:  on the line, from center to right
                ax.text(x_line_center, y_line_center, # Position
                        '%.3f' % y, # Value (3f = 3 decimal float)
                        verticalalignment='center', # Centered vertically with line 
                        fontsize=16, backgroundcolor="white")
                
if args.type == 'avg' and args.table is not None:
    plot_bar_avg()
elif args.type == 'sum' and args.table is not None:
    plot_bar_sum()
elif args.table is not None:
    box_plot()
else:
    plot_all()
