from flatlib.datetime import Datetime
from flatlib.geopos import GeoPos
from flatlib.chart import Chart
import flatlib
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

# Start at (e.g. 10,000 BC)
# Cycle through every day at (arbitrary) 9AM
# Record Z-code
# Check when LRB Z code appears
# Cycle through JND at
Alexandria_pos =  GeoPos('31n22', '29e96')

# Start from year 0?
jd_start = Datetime('0001/01/01', '12:00', '+00:00').jd
jd_end = Datetime('0200/01/01', '12:00', '+00:00').jd
# Go forward in time
jd = jd_start-1
Z_codes = []
with open('Z-codes.tsv', 'w') as f:
    while jd < jd_end:
        frac = (jd-jd_start)/(jd_end-jd_start) * 100
        print(frac)
        date = getDateFromJulian(jd)
        current_chart_Z = getZ(Chart(date, Alexandria_pos))
        Z_codes.append(current_chart_Z)
        f.write('%d,%s\n' % (jd, current_chart_Z))
        jd +=1
