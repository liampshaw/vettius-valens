from flatlib.datetime import Datetime
from flatlib.geopos import GeoPos
from flatlib.chart import Chart
from flatlib.const import const
import julian
import datetime


def getHouse(natal_chart, celestial_body):
    """"Gets the house of a celestial body."""
    return(str(natal_chart.getObject(celestial_body)).split(' ')[1])

# Conversion between zodiac sign and hexadecimal (Boxer's notation)
hexadecimal =  {'Aries':'0',
                'Taurus':'1',
                'Gemini':'2',
                'Cancer':'3',
                'Leo':'4',
                'Virgo':'5',
                'Libra':'6',
                'Scorpio':'7',
                'Sagittarius':'8',
                'Capricorn':'9',
                'Aquarius':'A',
                'Pisces':'B'}

def getZ(natal_chart):
    """Returns the 7-digit Z-code for an astrological chart."""
    bodies = ['Saturn', 'Jupiter', 'Mars', 'Sun', 'Venus', 'Mercury', 'Moon']
    z_code_list = [hexadecimal[getHouse(natal_chart, body)] for body in bodies]
    z_code = ''.join(z_code_list)
    return(z_code)

def getDateFromJulian(jd):
    """Returns date in required format from Julian date."""
    date_list = flatlib.datetime.jdnDate(jd)
    dt = Datetime(str(date_list[0]).split('.')[0]+'/'+str(date_list[1]).split('.')[0]+'/'+str(date_list[2]).split('.')[0], '09:00', '+00:00')
    return(dt)


# Date of LRB publication
LRB_date = Datetime('1979/10/25', '09:00', '+00:00')
# Centre of Russell Square coordinate
LRB_pos = GeoPos('51n55', '0w12')

# Generate chart
LRB_chart = Chart(LRB_date, LRB_pos)

# Get Z-code
LRB_Z_code = getZ(LRB_chart)


# Start at (e.g. 10,000 BC)
# Cycle through every day at (arbitrary) 9AM
# Record Z-code
# Check when LRB Z code appears
# Cycle through JND at
# Need to remember that there is a year 0: straight from 1 BC to 1 CE
bc_1 =  Datetime('0001/12/31', '09:00', '+00:00')
bc_0 =  Datetime('0000/12/31', '09:00', '+00:00')
ce =  Datetime('0001/01/01', '09:00', '+00:00')

# Boxer: 1 BC October 25th is 1721355.875000
Datetime('-0001/10/25', '09:00', '+00:00').jd-1721355.875000
# 364 days out i.e. a whole year
# Boxer has 1 CE October 25th as 1721720.875000
Datetime('0001/10/25', '09:00', '+00:00').jd-1721720.875000
# Why 2 days out? Leap year effects?

# Start from year 0?
jd_start = Datetime('0000/01/01', '09:00', '+00:00').jd
jd_end = Datetime('2020/01/01', '09:00', '+00:00').jd
# Go forward in time
jd = jd_start
Z_codes = []
while jd < jd_end:
    jd +=1
    frac = (jd-jd_start)/jd_end * 100
    print(frac)
    date = getDateFromJulian(jd)
    current_chart_Z = getZ(Chart(date, LRB_pos))
    Z_codes.append(current_chart_Z)

# Print Z code
print(Z_codes.index(LRB_Z_code))
