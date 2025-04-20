import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import plotly.express as px
import plotly.graph_objects as go
import os
from PIL import Image
import glob
from datetime import datetime

# Add this dictionary at the top of the file, after imports but before any functions
# Dictionary of city coordinates for mapping
city_coordinates = {
    # Alabama
    'Athens, AL': (34.8031, -86.9717),
    'Auburn, AL': (32.6099, -85.4808),
    'Daphne, AL': (30.6035, -87.9036),
    'Fairhope, AL': (30.5220, -87.9033),
    'Foley, AL': (30.4065, -87.6836),
    'Opelika, AL': (32.6454, -85.3783),
    'Tuscaloosa, AL': (33.2098, -87.5692),
    
    # Arizona
    'Buckeye, AZ': (33.3703, -112.5838),
    'Casa Grande, AZ': (32.8795, -111.7573),
    'Goodyear, AZ': (33.4353, -112.3576),
    'Kingman, AZ': (35.1894, -114.0530),
    'Marana, AZ': (32.4367, -111.2275),
    'Maricopa, AZ': (33.0581, -112.0476),
    'Queen Creek, AZ': (33.2487, -111.6343),
    'Surprise, AZ': (33.6292, -112.3680),
    
    # Arkansas
    'Bentonville, AR': (36.3728, -94.2088),
    'Conway, AR': (35.0887, -92.4421),
    'Fayetteville, AR': (36.0822, -94.1719),
    
    # California
    'Beaumont, CA': (33.9294, -116.9770),
    'Lathrop, CA': (37.8272, -121.2769),
    'Lincoln, CA': (38.8915, -121.2930),
    'Manteca, CA': (37.7975, -121.2163),
    'Menifee, CA': (33.6953, -117.1851),
    'Merced, CA': (37.3022, -120.4829),
    'Roseville, CA': (38.7521, -121.2880),
    
    # Colorado
    'Castle Rock, CO': (39.3722, -104.8561),
    'Commerce City, CO': (39.8083, -104.9339),
    'Erie, CO': (40.0503, -105.0500),
    'Parker, CO': (39.5186, -104.7613),
    'Windsor, CO': (40.4775, -104.9014),
    
    # Florida
    'Apopka, FL': (28.6763, -81.5098),
    'Cape Coral, FL': (26.5629, -81.9495),
    'Clermont, FL': (28.5494, -81.7729),
    'Daytona Beach, FL': (29.2108, -81.0228),
    'DeLand, FL': (29.0289, -81.3031),
    'Fort Myers, FL': (26.6406, -81.8723),
    'Haines City, FL': (28.1142, -81.6179),
    'Lakeland, FL': (28.0395, -81.9498),
    'Leesburg, FL': (28.8110, -81.8779),
    'North Port, FL': (27.0442, -82.2359),
    'Ocala, FL': (29.1872, -82.1401),
    'Palm Bay, FL': (28.0345, -80.5887),
    'Palm Coast, FL': (29.5844, -81.2079),
    'Parkland, FL': (26.3109, -80.2496),
    'Port St. Lucie, FL': (27.2731, -80.3534),
    'Sanford, FL': (28.8028, -81.2731),
    'St. Cloud, FL': (28.2489, -81.2809),
    'Venice, FL': (27.0998, -82.4543),
    'West Melbourne, FL': (28.0714, -80.6528),
    'West Palm Beach, FL': (26.7153, -80.0534),
    'Winter Haven, FL': (28.0222, -81.7329),
    
    # Georgia
    'Canton, GA': (34.2368, -84.4908),
    'Douglasville, GA': (33.7515, -84.7477),
    'Gainesville, GA': (34.2979, -83.8241),
    'McDonough, GA': (33.4473, -84.1468),
    'Perry, GA': (32.4582, -83.7322),
    'Pooler, GA': (32.1156, -81.2498),
    'Suwanee, GA': (34.0515, -84.0712),
    'Woodstock, GA': (34.1015, -84.5194),
    
    # Idaho
    'Caldwell, ID': (43.6629, -116.6874),
    'Kuna, ID': (43.4918, -116.4201),
    'Meridian, ID': (43.6121, -116.3915),
    'Nampa, ID': (43.5407, -116.5635),
    'Post Falls, ID': (47.7127, -116.9509),
    
    # Illinois
    'Yorkville, IL': (41.6411, -88.4473),
    
    # Indiana
    'Avon, IN': (39.7634, -86.3994),
    'Brownsburg, IN': (39.8436, -86.3972),
    'Greenfield, IN': (39.7856, -85.7694),
    'St. John, IN': (41.4500, -87.4700),
    'Westfield, IN': (40.0428, -86.1275),
    
    # Iowa
    'Ankeny, IA': (41.7317, -93.6001),
    'Waukee, IA': (41.6114, -93.8560),
    
    # Kansas
    'Gardner, KS': (38.8108, -94.9272),
    
    # Maryland
    'Frederick, MD': (39.4143, -77.4105),
    
    # Minnesota
    'Cottage Grove, MN': (44.8278, -92.9440),
    'Shakopee, MN': (44.7980, -93.5269),
    
    # Missouri
    'Nixa, MO': (37.0428, -93.2944),
    'Raymore, MO': (38.8103, -94.4683),
    
    # Montana
    'Kalispell, MT': (48.1920, -114.3168),
    
    # Nevada
    'Mesquite, NV': (36.8050, -114.0632),
    
    # New York
    'Harrison, NY': (40.9703, -73.7265),
    'Kiryas Joel, NY': (41.3401, -74.1601),
    
    # North Carolina
    'Apex, NC': (35.7327, -78.8506),
    'Clayton, NC': (35.6507, -78.4564),
    'Fuquay-Varina, NC': (35.5843, -78.8000),
    'Garner, NC': (35.7113, -78.6142),
    'Holly Springs, NC': (35.6513, -78.8336),
    'Kannapolis, NC': (35.4873, -80.6217),
    'Leland, NC': (34.2154, -78.0197),
    'Monroe, NC': (34.9854, -80.5495),
    'Wake Forest, NC': (35.9799, -78.5097),
    'Waxhaw, NC': (34.9243, -80.7431),
    'Wilmington, NC': (34.2104, -77.8868),
    
    # Ohio
    'Delaware, OH': (40.2987, -83.0679),
    'Marysville, OH': (40.2365, -83.3671),
    'Pickerington, OH': (39.8842, -82.7535),
    
    # Oregon
    'Happy Valley, OR': (45.4469, -122.5136),
    'Redmond, OR': (44.2726, -121.1739),
    'Woodburn, OR': (45.1437, -122.8550),
    
    # Pennsylvania
    'Carlisle, PA': (40.2015, -77.2000),
    
    # South Carolina
    'Bluffton, SC': (32.2371, -80.8601),
    'Conway, SC': (33.8360, -79.0478),
    'Easley, SC': (34.8298, -82.6015),
    'Fort Mill, SC': (35.0068, -80.9450),
    'Greer, SC': (34.9323, -82.2268),
    'Mauldin, SC': (34.7795, -82.3018),
    'Myrtle Beach, SC': (33.6891, -78.8867),
    'North Charleston, SC': (32.8546, -79.9748),
    'Simpsonville, SC': (34.7370, -82.2543),
    
    # Tennessee
    'Clarksville, TN': (36.5298, -87.3595),
    'Columbia, TN': (35.6151, -87.0353),
    'Farragut, TN': (35.8828, -84.1622),
    'Gallatin, TN': (36.3881, -86.4494),
    'Lebanon, TN': (36.2081, -86.2911),
    'Mount Juliet, TN': (36.2001, -86.5186),
    'Murfreesboro, TN': (35.8456, -86.3903),
    'Smyrna, TN': (35.9828, -86.5186),
    'Spring Hill, TN': (35.7512, -86.9300),
    
    # Texas
    'Allen, TX': (33.1031, -96.6705),
    'Belton, TX': (31.0557, -97.4642),
    'Bryan, TX': (30.6744, -96.3698),
    'Burleson, TX': (32.5421, -97.3208),
    'Cibolo, TX': (29.5722, -98.2361),
    'Cleburne, TX': (32.3474, -97.3867),
    'Conroe, TX': (30.3119, -95.4561),
    'Converse, TX': (29.5180, -98.3178),
    'Denton, TX': (33.2148, -97.1331),
    'Ennis, TX': (32.3293, -96.6258),
    'Forney, TX': (32.7479, -96.4736),
    'Frisco, TX': (33.1507, -96.8236),
    'Georgetown, TX': (30.6333, -97.6769),
    'Greenville, TX': (33.1384, -96.1108),
    'Hutto, TX': (30.5427, -97.5467),
    'Katy, TX': (29.7858, -95.8245),
    'Kyle, TX': (29.9891, -97.8772),
    'Leander, TX': (30.5788, -97.8530),
    'Lewisville, TX': (33.0462, -97.0028),
    'Little Elm, TX': (33.1626, -96.9375),
    'Mansfield, TX': (32.5632, -97.1417),
    'Midlothian, TX': (32.4824, -96.9933),
    'New Braunfels, TX': (29.7030, -98.1244),
    'Prosper, TX': (33.2357, -96.8014),
    'Rockwall, TX': (32.9290, -96.4597),
    'Round Rock, TX': (30.5083, -97.6789),
    'Sachse, TX': (32.9762, -96.5939),
    'Seguin, TX': (29.5688, -97.9647),
    'Sherman, TX': (33.6357, -96.6089),
    'Socorro, TX': (31.6546, -106.2569),
    'Temple, TX': (31.0982, -97.3428),
    'Texas City, TX': (29.3839, -94.9027),
    'Waxahachie, TX': (32.3865, -96.8483),
    'Weatherford, TX': (32.7593, -97.7972),
    
    # Utah
    'American Fork, UT': (40.3769, -111.7953),
    'Cedar City, UT': (37.6775, -113.0619),
    'Eagle Mountain, UT': (40.3144, -112.0122),
    'Herriman, UT': (40.5141, -112.0327),
    'Hurricane, UT': (37.1753, -113.2899),
    'Lehi, UT': (40.3916, -111.8507),
    'Payson, UT': (40.0444, -111.7321),
    'South Jordan, UT': (40.5621, -111.9296),
    'St. George, UT': (37.0965, -113.5684),
    'Syracuse, UT': (41.0893, -112.0647),
    'Tooele, UT': (40.5308, -112.2983),
    'Washington, UT': (37.1305, -113.5083),
    
    # Virginia
    'Suffolk, VA': (36.7282, -76.5836),
    
    # Washington
    'Lynnwood, WA': (47.8209, -122.3151),
    'Redmond, WA': (47.6740, -122.1215),
    
    # Oklahoma
    'Yukon, OK': (35.5067, -97.7625)
}

# Set page configuration
st.set_page_config(
    page_title="Housing Market Investment Dashboard",
    page_icon="🏡",
    layout="wide",
    initial_sidebar_state="expanded"
)

# Custom CSS for better aesthetics
st.markdown("""
<style>
    .main {
        background-color: #f8f9fa;
    }
    .stTabs [data-baseweb="tab-list"] {
        gap: 24px;
    }
    .stTabs [data-baseweb="tab"] {
        height: 50px;
        white-space: pre-wrap;
        background-color: #f1f3f6;
        border-radius: 4px 4px 0px 0px;
        gap: 1px;
        padding-top: 10px;
        padding-bottom: 10px;
    }
    .stTabs [aria-selected="true"] {
        background-color: #4e8df5;
        color: white;
    }
    div.block-container {
        padding-top: 1rem;
    }
    h1, h2, h3 {
        color: #1e3a8a;
    }
    .metric-card {
        background-color: white;
        border-radius: 10px;
        padding: 15px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
    }
    .stDataFrame {
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
    }
    .buy {color: green; font-weight: bold;}
    .sell {color: red; font-weight: bold;}
    .hold {color: gray;}
</style>
""", unsafe_allow_html=True)

# Helper functions
def get_signal_paths(city_name):
    """Get paths to signal images for a specific city"""
    safe_city_name = city_name.replace(', ', '_')
    signal_path = f"results/plots/signals/{safe_city_name}_investment_signals.png"
    
    if os.path.exists(signal_path):
        return signal_path
    return None

def load_data():
    """Load data for the dashboard"""
    try:
        # Load investment opportunities
        all_opportunities = pd.read_csv("results/data/all_investment_opportunities.csv")
        
        # Add state column if not present
        if 'state' not in all_opportunities.columns:
            all_opportunities['state'] = all_opportunities['city'].apply(lambda x: x.split(', ')[1] if ', ' in x else 'Unknown')
        
        # Calculate risk-adjusted return if not present
        if 'risk_adjusted_return' not in all_opportunities.columns:
            all_opportunities['risk_adjusted_return'] = all_opportunities['avg_expected_return'] / all_opportunities['avg_risk']
        
        # Load top opportunities
        top_expected_returns = pd.read_csv("results/data/top_expected_returns.csv")
        top_historical_growth = pd.read_csv("results/data/top_historical_growth.csv")
        top_risk_adjusted = pd.read_csv("results/data/top_risk_adjusted_returns.csv")
        
        # Load metrics
        metrics_df = pd.read_csv("results/data/metrics.csv") if os.path.exists("results/data/metrics.csv") else None
        
        # Count signals
        buy_signals = len(all_opportunities[all_opportunities['latest_signal'] == 'buy'])
        sell_signals = len(all_opportunities[all_opportunities['latest_signal'] == 'sell'])
        hold_signals = len(all_opportunities[all_opportunities['latest_signal'] == 'hold'])
        
        return all_opportunities, top_expected_returns, top_historical_growth, top_risk_adjusted, metrics_df, buy_signals, sell_signals, hold_signals
        
    except Exception as e:
        st.error(f"Error loading data: {str(e)}")
        return None, None, None, None, None, 0, 0, 0

# Main function
def main():
    # Load data
    all_opportunities, top_expected_returns, top_historical_growth, top_risk_adjusted, metrics_df, buy_signals, sell_signals, hold_signals = load_data()
    
    # Header
    col1, col2 = st.columns([1, 3])
    with col1:
        st.image("https://cdn-icons-png.flaticon.com/512/2544/2544056.png", width=80)
    with col2:
        st.title("Housing Market Investment Dashboard")
    
    st.markdown(f"<p style='color: #666; font-size: 0.9em;'>Last updated: {datetime.now().strftime('%Y-%m-%d')}</p>", unsafe_allow_html=True)
    
    # Create tabs
    tab1, tab2, tab3, tab4 = st.tabs(["📊 Market Overview", "🏙️ City Analysis", "💰 Investment Opportunities", "⚠️ Disclaimer"])
    
    # Tab 1: Market Overview
    with tab1:
        st.header("Housing Market Overview")
        
        # Key metrics in cards
        col1, col2, col3, col4 = st.columns(4)
        
        with col1:
            st.markdown("""
            <div class="metric-card">
                <h3 style="margin:0; font-size:1em; color:#666;">Cities Analyzed</h3>
                <p style="font-size:2em; font-weight:bold; margin:0; color:#1e3a8a;">{}</p>
            </div>
            """.format(len(all_opportunities)), unsafe_allow_html=True)
            
        with col2:
            st.markdown("""
            <div class="metric-card">
                <h3 style="margin:0; font-size:1em; color:#666;">Buy Signals</h3>
                <p style="font-size:2em; font-weight:bold; margin:0; color:#00CC66;">{} <span style="font-size:0.5em; color:#00CC66;">▲ {:.1f}%</span></p>
            </div>
            """.format(buy_signals, buy_signals/len(all_opportunities)*100), unsafe_allow_html=True)
            
        with col3:
            st.markdown("""
            <div class="metric-card">
                <h3 style="margin:0; font-size:1em; color:#666;">Sell Signals</h3>
                <p style="font-size:2em; font-weight:bold; margin:0; color:#FF5050;">{} <span style="font-size:0.5em; color:#FF5050;">▲ {:.1f}%</span></p>
            </div>
            """.format(sell_signals, sell_signals/len(all_opportunities)*100), unsafe_allow_html=True)
            
        with col4:
            avg_return = all_opportunities['avg_expected_return'].mean() * 100
            st.markdown(f"""
            <div class="metric-card">
                <h3 style="margin:0; font-size:1em; color:#666;">Avg Expected Return</h3>
                <p style="font-size:2em; font-weight:bold; margin:0; color:#1e3a8a;">{avg_return:.2f}%</p>
            </div>
            """, unsafe_allow_html=True)
        
        st.subheader("Investment Signal Distribution")
        
        # Create pie chart for signal distribution
        signal_counts = {
            'buy': buy_signals,
            'sell': sell_signals,
            'hold': hold_signals
        }
        
        signal_df = pd.DataFrame({
            'Signal': list(signal_counts.keys()),
            'Count': list(signal_counts.values())
        })
        
        fig = px.pie(
            signal_df, 
            values='Count', 
            names='Signal',
            color='Signal',
            color_discrete_map={'buy':'#00CC66', 'sell':'#FF5050', 'hold':'#AAAAAA'},
            title="Distribution of Investment Signals"
        )
        
        fig.update_traces(
            textposition='inside', 
            textinfo='percent+label',
            marker=dict(line=dict(color='#FFFFFF', width=2))
        )
        
        fig.update_layout(
            showlegend=True,
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1),
            margin=dict(l=20, r=20, t=40, b=20),
            height=400
        )
        
        st.plotly_chart(fig, use_container_width=True)
        
        # Top cities section
        st.subheader("Top 10 Cities by Expected Return")
        
        # Create a more attractive bar chart
        top_10 = all_opportunities.sort_values('avg_expected_return', ascending=False).head(10)
        top_10['expected_return_pct'] = top_10['avg_expected_return'] * 100
        
        fig = px.bar(
            top_10,
            x='city',
            y='expected_return_pct',
            color='latest_signal',
            color_discrete_map={'buy':'#00CC66', 'sell':'#FF5050', 'hold':'#AAAAAA'},
            labels={'expected_return_pct': 'Expected Return (%)', 'city': 'City', 'latest_signal': 'Signal'},
            text_auto='.2f'
        )
        
        fig.update_layout(
            xaxis_title="City",
            yaxis_title="Expected Return (%)",
            plot_bgcolor='rgba(0,0,0,0)',
            xaxis={'categoryorder':'total descending'},
            margin=dict(l=20, r=20, t=20, b=20),
            height=500
        )
        
        fig.update_traces(
            texttemplate='%{text}%', 
            textposition='outside',
            marker_line_color='rgb(255,255,255)',
            marker_line_width=1.5
        )
        
        st.plotly_chart(fig, use_container_width=True)

        # Add to the Market Overview tab after the top 10 cities bar chart
        st.subheader("National Housing Market Overview")

        # Create a container with a border for the map
        st.markdown("""
        <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
        """, unsafe_allow_html=True)

        # Add coordinates to the dataframe for the overview map
        all_opportunities['lat'] = all_opportunities['city'].apply(lambda x: city_coordinates.get(x, [39.8, -98.5])[0])
        all_opportunities['lon'] = all_opportunities['city'].apply(lambda x: city_coordinates.get(x, [39.8, -98.5])[1])

        # Create a heatmap-style visualization
        # Calculate the percentage of buy signals in each state
        state_data = all_opportunities.groupby('state').agg({
            'city': 'count',
            'latest_signal': lambda x: (x == 'buy').sum() / len(x) * 100,
            'avg_expected_return': 'mean'
        }).reset_index()

        state_data.columns = ['state', 'city_count', 'buy_percentage', 'avg_expected_return']
        state_data['avg_expected_return'] = state_data['avg_expected_return'] * 100

        # Create the choropleth map
        fig = px.choropleth(
            state_data,
            locations='state',
            locationmode='USA-states',
            color='buy_percentage',
            color_continuous_scale=[[0, '#FF5050'], [0.5, '#FFFFFF'], [1, '#00CC66']],
            scope="usa",
            labels={'buy_percentage': 'Buy Signal %', 'avg_expected_return': 'Avg Return %'},
            title="Investment Opportunity Heatmap by State",
            hover_data=['city_count', 'avg_expected_return']
        )

        # Improve hover information
        fig.update_traces(
            hovertemplate="<b>%{location}</b><br>" +
                          "Cities Analyzed: %{customdata[0]}<br>" +
                          "Buy Signal: %{z:.1f}%<br>" +
                          "Avg Expected Return: %{customdata[1]:.2f}%"
        )

        # Add custom data for hover
        fig.update_traces(customdata=state_data[['city_count', 'avg_expected_return']])

        # Improve map appearance
        fig.update_geos(
            showcoastlines=True,
            coastlinecolor="#333333",
            showland=True,
            landcolor="#EAEAEA",
            showocean=True,
            oceancolor="#A8D1E7",
            showlakes=True,
            lakecolor="#A8D1E7",
            showrivers=False,
            showcountries=True,
            countrycolor="#333333",
            showsubunits=True,
            subunitcolor="#DDDDDD"
        )

        # Update layout for better aesthetics
        fig.update_layout(
            paper_bgcolor='white',
            geo=dict(
                bgcolor='white',
            ),
            margin=dict(l=0, r=0, t=40, b=0),
            coloraxis_colorbar=dict(
                title="Buy Signal %",
                ticksuffix="%",
                x=0.01,
                xanchor="left",
                len=0.9
            )
        )

        st.plotly_chart(fig, use_container_width=True)

        # Add a caption with explanation
        st.markdown("""
        <div style="font-size:0.85em; color:#666666; margin-top:-15px;">
        <b>Map Legend:</b> This heatmap shows the percentage of cities with BUY signals in each state. 
        Green states have more buying opportunities, while red states have more selling signals.
        Hover over a state to see detailed statistics.
        </div>
        """, unsafe_allow_html=True)

        # Add a second visualization - bubble map showing concentration of opportunities
        st.subheader("Investment Opportunity Concentration")

        # Create a bubble map showing all cities with color by signal
        fig2 = px.scatter_geo(
            all_opportunities,
            lat='lat',
            lon='lon',
            color='latest_signal',
            size=all_opportunities['avg_expected_return'].abs() * 100 + 5,  # Adjust size for visibility
            color_discrete_map={'buy':'#00CC66', 'sell':'#FF5050', 'hold':'#AAAAAA'},
            scope="usa",
            hover_name='city',
            title="National Distribution of Investment Signals"
        )

        # Customize hover info
        fig2.update_traces(
            hovertemplate="<b>%{hovertext}</b><br>Signal: %{marker.color}<br>"
        )

        # Use a different map style from the one in Investment Opportunities
        fig2.update_geos(
            showcoastlines=True,
            coastlinecolor="#555555",
            showland=True,
            landcolor="#F5F5F5",
            showocean=True,
            oceancolor="#D6EFF5",
            showlakes=True,
            lakecolor="#D6EFF5",
            showrivers=False,
            showcountries=True,
            countrycolor="#555555",
            showsubunits=True,
            subunitcolor="#DDDDDD",
            resolution=50
        )

        # Update layout
        fig2.update_layout(
            paper_bgcolor='white',
            geo=dict(
                bgcolor='white',
                projection_scale=1.1,  # Zoom in slightly
            ),
            margin=dict(l=0, r=0, t=40, b=0),
            legend=dict(
                title="Investment Signal",
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1
            )
        )

        st.plotly_chart(fig2, use_container_width=True)

        # Add a caption
        st.markdown("""
        <div style="font-size:0.85em; color:#666666; margin-top:-15px;">
        This map shows the geographic distribution of all analyzed cities. The color indicates the current investment signal,
        and the size represents the magnitude of the expected return.
        </div>
        """, unsafe_allow_html=True)

        st.markdown("</div>", unsafe_allow_html=True)
    
    # Tab 2: City Analysis
    with tab2:
        st.header("City Analysis")
        
        # Add a decorative divider
        st.markdown("<hr style='height:2px;border-width:0;color:#4e8df5;background-color:#4e8df5;margin-bottom:25px;'>", unsafe_allow_html=True)
        
        # Improved city selection with search and filters
        col1, col2, col3 = st.columns([2, 2, 1])
        
        with col1:
            # Get states for filtering
            states = sorted(all_opportunities['state'].unique())
            selected_state = st.selectbox("Filter by State:", ["All States"] + list(states))
        
        # Filter cities based on state
        filtered_cities = all_opportunities
        if selected_state != "All States":
            filtered_cities = filtered_cities[filtered_cities['state'] == selected_state]
        
        all_filtered_cities = sorted(filtered_cities['city'].unique())
        
        with col2:
            selected_city = st.selectbox("Select a city to analyze:", all_filtered_cities)
        
        with col3:
            st.markdown("<br>", unsafe_allow_html=True)
            analyze_button = st.button("📊 Analyze", use_container_width=True, 
                                      help="Click to analyze the selected city")
            if analyze_button:
                st.session_state.show_analysis = True
        
        if 'show_analysis' not in st.session_state:
            st.session_state.show_analysis = False
            
        if st.session_state.show_analysis and selected_city:
            # Get city data
            city_data = all_opportunities[all_opportunities['city'] == selected_city].iloc[0]
            
            # Create a container with a border
            st.markdown(f"""
            <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
                <h3 style="color:#1e3a8a; margin-bottom:15px;">{selected_city} Analysis</h3>
            </div>
            """, unsafe_allow_html=True)
            
            # Display city metrics in cards
            col1, col2, col3 = st.columns(3)
            
            with col1:
                signal_color = "#00CC66" if city_data['latest_signal'] == 'buy' else "#FF5050" if city_data['latest_signal'] == 'sell' else "#AAAAAA"
                signal_icon = "↗️" if city_data['latest_signal'] == 'buy' else "↘️" if city_data['latest_signal'] == 'sell' else "↔️"
                st.markdown(f"""
                <div class="metric-card">
                    <h3 style="margin:0; font-size:1em; color:#666;">Investment Signal</h3>
                    <p style="font-size:2em; font-weight:bold; margin:0; color:{signal_color};">{signal_icon} {city_data['latest_signal'].upper()}</p>
                </div>
                """, unsafe_allow_html=True)
                
            with col2:
                return_color = "#00CC66" if city_data['avg_expected_return'] > 0 else "#FF5050"
                return_icon = "📈" if city_data['avg_expected_return'] > 0 else "📉"
                st.markdown(f"""
                <div class="metric-card">
                    <h3 style="margin:0; font-size:1em; color:#666;">Expected Return</h3>
                    <p style="font-size:2em; font-weight:bold; margin:0; color:{return_color};">{return_icon} {city_data['avg_expected_return']*100:.2f}%</p>
                </div>
                """, unsafe_allow_html=True)
                
            with col3:
                growth_color = "#00CC66" if city_data['annual_growth_rate'] > 0 else "#FF5050"
                growth_icon = "📈" if city_data['annual_growth_rate'] > 0 else "📉"
                st.markdown(f"""
                <div class="metric-card">
                    <h3 style="margin:0; font-size:1em; color:#666;">Annual Growth Rate</h3>
                    <p style="font-size:2em; font-weight:bold; margin:0; color:{growth_color};">{growth_icon} {city_data['annual_growth_rate']:.2f}%</p>
                </div>
                """, unsafe_allow_html=True)
            
            # Display investment signal chart
            st.subheader(f"Investment Signal History")
            
            signal_path = get_signal_paths(selected_city)
            if signal_path:
                # Add a container with a border for the chart
                st.markdown("""
                <div style="background-color:white; padding:10px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);">
                """, unsafe_allow_html=True)
                st.image(signal_path, use_container_width=True)
                st.markdown("</div>", unsafe_allow_html=True)
            else:
                st.warning(f"No investment signal chart available for {selected_city}")
    
    # Tab 3: Investment Opportunities
    with tab3:
        st.header("Investment Opportunities")
        
        # Add a decorative divider
        st.markdown("<hr style='height:2px;border-width:0;color:#4e8df5;background-color:#4e8df5;margin-bottom:25px;'>", unsafe_allow_html=True)
        
        # Create a container with a border for filters
        st.markdown("""
        <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
            <h3 style="color:#1e3a8a; margin-bottom:15px;">Filter Options</h3>
        """, unsafe_allow_html=True)
        
        # Filter controls
        col1, col2, col3 = st.columns(3)
        
        with col1:
            signal_filter = st.radio(
                "Filter by Signal:",
                ["All Signals", "Buy", "Sell", "Hold"],
                horizontal=True
            )
        
        with col2:
            min_return = st.slider(
                "Minimum Expected Return (%)",
                min_value=float(all_opportunities['avg_expected_return'].min() * 100),
                max_value=float(all_opportunities['avg_expected_return'].max() * 100),
                value=float(all_opportunities['avg_expected_return'].min() * 100),
                step=0.1
            )
        
        with col3:
            states = sorted(all_opportunities['state'].unique())
            selected_states = st.multiselect(
                "Filter by State:",
                states,
                default=[]
            )
        
        st.markdown("</div>", unsafe_allow_html=True)
        
        # Apply filters
        filtered_data = all_opportunities.copy()
        
        # Signal filter
        if signal_filter != "All Signals":
            filtered_data = filtered_data[filtered_data['latest_signal'] == signal_filter.lower()]
        
        # Return filter
        filtered_data = filtered_data[filtered_data['avg_expected_return'] * 100 >= min_return]
        
        # State filter
        if selected_states:
            filtered_data = filtered_data[filtered_data['state'].isin(selected_states)]
        
        # Display top opportunities
        st.subheader(f"Top Cities by Expected Return ({signal_filter})")
        
        # Create a container with a border for the table
        st.markdown("""
        <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
        """, unsafe_allow_html=True)
        
        # Create a table with the top 15 opportunities
        top_n = min(15, len(filtered_data))
        top_opportunities = filtered_data.sort_values('avg_expected_return', ascending=False).head(top_n)
        
        # Format the data for display
        display_data = top_opportunities[['city', 'state', 'avg_expected_return', 'annual_growth_rate', 'latest_signal']].copy()
        display_data['avg_expected_return'] = display_data['avg_expected_return'] * 100
        
        # Add styling to the signals
        def format_signal(signal):
            if signal == 'buy':
                return f"<span style='color:#00CC66; font-weight:bold;'>BUY</span>"
            elif signal == 'sell':
                return f"<span style='color:#FF5050; font-weight:bold;'>SELL</span>"
            else:
                return f"<span style='color:#AAAAAA;'>HOLD</span>"
        
        st.dataframe(
            display_data,
            column_config={
                "city": st.column_config.TextColumn("City", width="medium"),
                "state": st.column_config.TextColumn("State", width="small"),
                "avg_expected_return": st.column_config.NumberColumn("Expected Return (%)", format="%.2f", width="small"),
                "annual_growth_rate": st.column_config.NumberColumn("Annual Growth (%)", format="%.2f", width="small"),
                "latest_signal": st.column_config.TextColumn("Signal", width="small")
            },
            use_container_width=True,
            hide_index=True
        )
        
        st.markdown("</div>", unsafe_allow_html=True)
        
        # Plot
        st.markdown("""
        <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
        """, unsafe_allow_html=True)
        
        fig = px.bar(
            top_opportunities.head(10), 
            x='city', 
            y='avg_expected_return',
            color='latest_signal',
            color_discrete_map={'buy':'#00CC66', 'sell':'#FF5050', 'hold':'#AAAAAA'},
            title=f'Top 10 Cities by Expected Return ({signal_filter})',
            labels={'avg_expected_return': 'Expected Return (%)', 'city': 'City', 'latest_signal': 'Signal'},
            text=top_opportunities.head(10)['avg_expected_return'] * 100
        )
        
        fig.update_layout(
            xaxis_title="City", 
            yaxis_title="Expected Return (%)",
            plot_bgcolor='rgba(0,0,0,0)',
            xaxis={'categoryorder':'total descending'},
            margin=dict(l=20, r=20, t=40, b=20),
            legend_title="Investment Signal",
            font=dict(family="Arial, sans-serif", size=12)
        )
        
        fig.update_traces(
            texttemplate='%{text:.2f}%', 
            textposition='outside',
            marker_line_color='rgb(255,255,255)',
            marker_line_width=1.5
        )
        
        st.plotly_chart(fig, use_container_width=True)

        # Add the map visualization back to the Investment Opportunities tab
        st.subheader("Geographic Distribution of Investment Opportunities")

        # Create a container with a border for the map
        st.markdown("""
        <div style="background-color:white; padding:20px; border-radius:10px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1); margin-bottom:20px;">
        """, unsafe_allow_html=True)

        # Add coordinates to the dataframe
        filtered_data['lat'] = filtered_data['city'].apply(lambda x: city_coordinates.get(x, [39.8, -98.5])[0])
        filtered_data['lon'] = filtered_data['city'].apply(lambda x: city_coordinates.get(x, [39.8, -98.5])[1])

        # Convert expected returns to positive values for sizing and scale for visibility
        filtered_data['plot_size'] = filtered_data['avg_expected_return'].apply(lambda x: max(8, abs(x) * 800))

        # Create the map
        fig = px.scatter_geo(filtered_data,
                            lat='lat',
                            lon='lon',
                            hover_name="city",
                            size="plot_size",
                            color="latest_signal",
                            color_discrete_map={'buy':'#00CC66', 'sell':'#FF5050', 'hold':'#AAAAAA'},
                            scope="usa",
                            title=f"Investment Opportunities Map ({signal_filter} Signals)")

        # Improve the hover information with explicit signal
        fig.update_traces(
            hovertemplate="<b>%{hovertext}</b><br>Signal: <b>%{customdata[1]}</b><br>Expected Return: <b>%{customdata[0]:.2f}%</b>"
        )

        # Add custom data for hover
        filtered_data['hover_return'] = filtered_data['avg_expected_return'] * 100
        fig.update_traces(customdata=filtered_data[['hover_return', 'latest_signal']])

        # Adjust marker appearance
        fig.update_traces(marker=dict(opacity=0.85, line=dict(width=1.5, color='white')))

        # Improve map appearance with more professional colors
        fig.update_geos(
            showcoastlines=True,
            coastlinecolor="#666666",
            showland=True,
            landcolor="#E8F4EA",
            showocean=True,
            oceancolor="#D6EAF8",
            showlakes=True,
            lakecolor="#AED6F1",
            showrivers=True,
            rivercolor="#3498DB",
            showcountries=True,
            countrycolor="#CCCCCC",
            showsubunits=True,
            subunitcolor="#E0E0E0"
        )

        # Update layout for better aesthetics
        fig.update_layout(
            paper_bgcolor='white',
            geo=dict(
                bgcolor='white',
                lakecolor='#AED6F1',
                landcolor='#E8F4EA',
                subunitcolor='#E0E0E0'
            ),
            margin=dict(l=0, r=0, t=40, b=0),
            legend=dict(
                title="Investment Signal",
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="right",
                x=1
            )
        )

        st.plotly_chart(fig, use_container_width=True)

        # Add a caption with explanation
        st.markdown("""
        <div style="font-size:0.85em; color:#666666; margin-top:-15px;">
        <b>Map Legend:</b> Green markers indicate BUY signals, red markers indicate SELL signals, and gray markers indicate HOLD signals. 
        Marker size represents the magnitude of expected returns.
        </div>
        """, unsafe_allow_html=True)

        st.markdown("</div>", unsafe_allow_html=True)

    # Tab 4: Disclaimer
    with tab4:
        st.header("Disclaimer & Data Sources")
        
        # Add a decorative divider
        st.markdown("---")
        
        # Legal disclaimer section
        st.subheader("Legal Disclaimer")
        
        # Create a container with a red border
        disclaimer_container = st.container()
        with disclaimer_container:
            st.error("IMPORTANT: Not Financial Advice")
            st.write("The information contained in this dashboard is for informational and educational purposes only. It is not intended to be and does not constitute financial advice, investment advice, trading advice, or any other advice.")
            
            st.write("**Data Limitations:** This dashboard uses historical housing data that ends in February 2025 and has not been updated since. Real estate market conditions may have changed significantly since this data was collected.")
            
            st.write("**No Guarantee:** Past performance is not indicative of future results. The investment signals, expected returns, and other metrics presented in this dashboard are based on historical data analysis and do not guarantee future performance.")
            
            st.write("**Not a Recommendation:** The \"buy\", \"sell\", or \"hold\" signals generated by our model should not be interpreted as recommendations to purchase, sell, or hold any particular real estate investment. These signals are generated by an algorithm based on historical patterns and may not account for current market conditions, local factors, or your personal financial situation.")
            
            st.write("**Consult Professionals:** Before making any investment decisions, you should consult with qualified financial, legal, and tax professionals. Real estate investments involve significant risks and are not suitable for everyone.")
            
            st.write("**No Liability:** The creators of this dashboard shall not be liable for any losses, damages, or other liabilities related to financial or investment decisions made based on the information presented here.")
        
        # Data sources section
        st.subheader("Data Sources")
        
        data_container = st.container()
        with data_container:
            st.write("This dashboard utilizes data from the following sources:")
            
            col1, col2, col3 = st.columns(3)
            
            with col1:
                st.info("📊 Housing Market Data")
                st.write("Housing market data including prices, inventory, and sales metrics from Redfin Data Center.")
            
            with col2:
                st.info("📈 Interest Rate Data")
                st.write("Monthly interest rate data starting from 2012 used for mortgage rate analysis.")
            
            with col3:
                st.info("💵 Mortgage-Backed Securities Data")
                st.write("Data from Fannie Mae mortgage-backed securities CSV file for yield information.")
            
            st.write("**Data Processing:** The raw data has been processed using statistical models to generate investment signals, expected returns, and risk metrics.")
        
        # Educational project notice
        st.subheader("Educational Project")
        
        project_container = st.container()
        with project_container:
            st.success("College Class Project")
            
            st.write("This dashboard was created as part of a college class project. It is intended to demonstrate data visualization, statistical analysis, and web application development skills.")
            
            st.write("The analysis and visualizations presented here are for educational purposes only and should not be used for actual investment decisions.")
            
            st.write("We would like to thank our professors and classmates for their guidance and feedback throughout the development of this project.")
        
        # Methodology section
        st.subheader("Methodology")
        
        methodology_container = st.container()
        with methodology_container:
            st.write("Our investment signals are generated using the following methodology:")
            
            method_col1, method_col2 = st.columns(2)
            
            with method_col1:
                st.info("1. Data Collection")
                st.write("We collect historical housing price data, interest rates, and mortgage-backed securities data.")
                
                st.info("2. Signal Generation")
                st.write("We apply technical indicators and statistical models to identify potential buying and selling opportunities.")
            
            with method_col2:
                st.info("3. Expected Return")
                st.write("Expected returns are calculated based on historical price patterns and growth rates.")
                
                st.info("4. Risk Assessment")
                st.write("Risk metrics are derived from historical price volatility and market conditions.")
            
            st.write("**Model Limitations:** Our models are based on historical data and may not accurately predict future market movements.")

if __name__ == "__main__":
    main() 