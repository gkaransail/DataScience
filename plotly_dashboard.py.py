#!/usr/bin/env python
# coding: utf-8

# # Import Libraries

# In[1]:


pip install dash


# In[2]:


import pandas as pd
import plotly.express as px  # (version 4.7.0 or higher)
import plotly.graph_objects as go
from dash import Dash, dcc, html, Input, Output  # pip install dash (version 2.0.0 or higher)


# In[20]:


import dash_html_components as html 
from dash.dependencies import Input, Output
import dash_core_components as dcc


# In[25]:


app = Dash(__name__)


# Importing & Cleaning the data 

# In[22]:


df = pd.read_csv("C:/Users/karan/Downloads/intro_bees.csv")

df


# In[23]:


df = df.groupby(['State', 'ANSI', 'Affected by', 'Year', 'state_code'])[['Pct of Colonies Impacted']].mean()
df.reset_index(inplace=True)
print(df[:5])


# # App Layout 

# All the dropdown, slicer will go in the App layout code

# In[24]:


app.layout = html.Div([

    html.H1("Web Application Dashboards with Dash", style={'text-align': 'center'}),

    dcc.Dropdown(id="slct_year",
                 options=[
                     {"label": "2015", "value": 2015},
                     {"label": "2016", "value": 2016},
                     {"label": "2017", "value": 2017},
                     {"label": "2018", "value": 2018}],
                 multi=False,
                 value=2015,
                 style={'width': "40%"}
                 ),

    html.Div(id='output_container', children=[]),
    html.Br(),

    dcc.Graph(id='my_bee_map', figure={})

])


# In[ ]:




