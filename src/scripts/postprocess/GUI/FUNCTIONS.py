import numpy as np
import math


def calculate_line_equation(point1, point2, log=False):
    '''
    function calculates coefficients m, b in equation y=mx+b,
    based on two points
    http://docs.scipy.org/doc/numpy/reference/generated/numpy.linalg.solve.html
    
    inputs
    -----------------
        point = [x, y]
    ------------------
    '''

    x1 = point1[0]
    y1 = point1[1]

    x2 = point2[0]
    y2 = point2[1]

    M1 = np.array([[x1, 1], [x2, 1]])
    M2 = np.array([y1, y2])
    m, b = np.linalg.solve(M1, M2)

    if log:
        print 'line equation: y={0}*x+{1}'.format(m, b)
    return m, b  # floats


def select_integer_cells_along_line(point1, point2, log=False):
    '''
    function picks griddcells with centers in integer values (i.e. 0, 1, 2 ...),
    along the line constructed based on two points
    
    inputs
    -----------------
        point = [x, y]
    ------------------
    '''

    m, b = calculate_line_equation(point1, point2, log=log)
    
    x1 = point1[0]
    y1 = point1[1]

    x2 = point2[0]
    y2 = point2[1]
    dx = abs(x2-x1)
    dy = abs(y2-y1)

    POINTS = list()
    if dx >= dy:  # cycle over dx
        if x2 > x1:
            for x in xrange(x1, x2+1):  #cycle over cells between x1 and x2, including both of them
                y = int(round(m*x+b))
                POINTS.append([x, y])
        elif x1 > x2:
            for x in xrange(x1, x2-1, -1):  #cycle over cells between x1 and x2, including both of them
                y = int(round(m*x+b))
                POINTS.append([x, y])
        else:
            raise ValueError('Passed x1 = x2. Should be different')
    elif dy > dx:   # cycle over dy
        if y2 > y1:
            for y in xrange(y1, y2+1):   #cycle over cells between y1 and y2, including both of them
                x = int(round((y-b)/m))
                POINTS.append([x, y])
        elif y1 > y2:
            for y in xrange(y1, y2-1, -1):   #cycle over cells between y1 and y2, including both of them
                x = int(round((y-b)/m))
                POINTS.append([x, y])
        else:
            raise ValueError('Passed y1 = y2. Should be different')
    if log:
        print POINTS
    return POINTS






"""
Various geodetic utilities for ObsPy.

:copyright:
    The ObsPy Development Team (devs@obspy.org)
:license:
    GNU Lesser General Public License, Version 3
    (http://www.gnu.org/copyleft/lesser.html)
"""


def calcVincentyInverse(lat1, lon1, lat2, lon2):
    """
    Vincenty Inverse Solution of Geodesics on the Ellipsoid.

    Computes the distance between two geographic points on the WGS84
    ellipsoid and the forward and backward azimuths between these points.

    :param lat1: Latitude of point A in degrees (positive for northern,
        negative for southern hemisphere)
    :param lon1: Longitude of point A in degrees (positive for eastern,
        negative for western hemisphere)
    :param lat2: Latitude of point B in degrees (positive for northern,
        negative for southern hemisphere)
    :param lon2: Longitude of point B in degrees (positive for eastern,
        negative for western hemisphere)
    :return: (Great circle distance in m, azimuth A->B in degrees,
        azimuth B->A in degrees)
    :raises: This method may have no solution between two nearly antipodal
        points; an iteration limit traps this case and a ``StopIteration``
        exception will be raised.

    .. note::
        This code is based on an implementation incorporated in
        Matplotlib Basemap Toolkit 0.9.5 http://sourceforge.net/projects/\
matplotlib/files/matplotlib-toolkits/basemap-0.9.5/
        (matplotlib/toolkits/basemap/greatcircle.py)

        Algorithm from Geocentric Datum of Australia Technical Manual.

        * http://www.icsm.gov.au/gda/gdatm/index.html
        * http://www.icsm.gov.au/gda/gdatm/gdav2.3.pdf, pp. 15

        It states::

            Computations on the Ellipsoid

            There are a number of formulae that are available to calculate
            accurate geodetic positions, azimuths and distances on the
            ellipsoid.

            Vincenty's formulae (Vincenty, 1975) may be used for lines ranging
            from a few cm to nearly 20,000 km, with millimetre accuracy. The
            formulae have been extensively tested for the Australian region, by
            comparison with results from other formulae (Rainsford, 1955 &
            Sodano, 1965).

            * Inverse problem: azimuth and distance from known latitudes and
              longitudes
            * Direct problem: Latitude and longitude from known position,
              azimuth and distance.
    """
    # Check inputs
    if lat1 > 90 or lat1 < -90:
        msg = "Latitude of Point 1 out of bounds! (-90 <= lat1 <=90)"
        raise ValueError(msg)
    while lon1 > 180:
        lon1 -= 360
    while lon1 < -180:
        lon1 += 360
    if lat2 > 90 or lat2 < -90:
        msg = "Latitude of Point 2 out of bounds! (-90 <= lat2 <=90)"
        raise ValueError(msg)
    while lon2 > 180:
        lon2 -= 360
    while lon2 < -180:
        lon2 += 360

    # Data on the WGS84 reference ellipsoid:
    a = 6378137.0          # semimajor axis in m
    f = 1 / 298.257223563  # flattening
    b = a * (1 - f)        # semiminor axis

    if (abs(lat1 - lat2) < 1e-8) and (abs(lon1 - lon2) < 1e-8):
        return 0.0, 0.0, 0.0

    # convert latitudes and longitudes to radians:
    lat1 = lat1 * 2.0 * math.pi / 360.
    lon1 = lon1 * 2.0 * math.pi / 360.
    lat2 = lat2 * 2.0 * math.pi / 360.
    lon2 = lon2 * 2.0 * math.pi / 360.

    TanU1 = (1 - f) * math.tan(lat1)
    TanU2 = (1 - f) * math.tan(lat2)

    U1 = math.atan(TanU1)
    U2 = math.atan(TanU2)

    dlon = lon2 - lon1
    last_dlon = -4000000.0  # an impossible value
    omega = dlon

    # Iterate until no significant change in dlon or iterlimit has been
    # reached (http://www.movable-type.co.uk/scripts/latlong-vincenty.html)
    iterlimit = 100
    try:
        while (last_dlon < -3000000.0 or dlon != 0 and
               abs((last_dlon - dlon) / dlon) > 1.0e-9):
            sqr_sin_sigma = pow(math.cos(U2) * math.sin(dlon), 2) + \
                pow((math.cos(U1) * math.sin(U2) - math.sin(U1) * \
                     math.cos(U2) * math.cos(dlon)), 2)
            Sin_sigma = math.sqrt(sqr_sin_sigma)
            Cos_sigma = math.sin(U1) * math.sin(U2) + math.cos(U1) * \
                math.cos(U2) * math.cos(dlon)
            sigma = math.atan2(Sin_sigma, Cos_sigma)
            Sin_alpha = math.cos(U1) * math.cos(U2) * math.sin(dlon) / \
                math.sin(sigma)
            alpha = math.asin(Sin_alpha)
            Cos2sigma_m = math.cos(sigma) - (2 * math.sin(U1) * \
                math.sin(U2) / pow(math.cos(alpha), 2))
            C = (f / 16) * pow(math.cos(alpha), 2) * \
                (4 + f * (4 - 3 * pow(math.cos(alpha), 2)))
            last_dlon = dlon
            dlon = omega + (1 - C) * f * math.sin(alpha) * (sigma + C * \
                math.sin(sigma) * (Cos2sigma_m + C * math.cos(sigma) * \
                                   (-1 + 2 * pow(Cos2sigma_m, 2))))

            u2 = pow(math.cos(alpha), 2) * (a * a - b * b) / (b * b)
            A = 1 + (u2 / 16384) * (4096 + u2 * (-768 + u2 * (320 - 175 * u2)))
            B = (u2 / 1024) * (256 + u2 * (-128 + u2 * (74 - 47 * u2)))
            delta_sigma = B * Sin_sigma * (Cos2sigma_m + (B / 4) * \
                (Cos_sigma * (-1 + 2 * pow(Cos2sigma_m, 2)) - (B / 6) * \
                Cos2sigma_m * (-3 + 4 * sqr_sin_sigma) * \
                (-3 + 4 * pow(Cos2sigma_m, 2))))

            dist = b * A * (sigma - delta_sigma)
            alpha12 = math.atan2((math.cos(U2) * math.sin(dlon)),
                (math.cos(U1) * math.sin(U2) - math.sin(U1) * math.cos(U2) * \
                 math.cos(dlon)))
            alpha21 = math.atan2((math.cos(U1) * math.sin(dlon)),
                (-math.sin(U1) * math.cos(U2) + math.cos(U1) * math.sin(U2) * \
                 math.cos(dlon)))
            iterlimit -= 1
            if iterlimit < 0:
                # iteration limit reached
                raise StopIteration
    except ValueError:
        # usually "math domain error"
        raise StopIteration

    if alpha12 < 0.0:
        alpha12 = alpha12 + (2.0 * math.pi)
    if alpha12 > (2.0 * math.pi):
        alpha12 = alpha12 - (2.0 * math.pi)

    alpha21 = alpha21 + math.pi

    if alpha21 < 0.0:
        alpha21 = alpha21 + (2.0 * math.pi)
    if alpha21 > (2.0 * math.pi):
        alpha21 = alpha21 - (2.0 * math.pi)

    # convert to degrees:
    alpha12 = alpha12 * 360 / (2.0 * math.pi)
    alpha21 = alpha21 * 360 / (2.0 * math.pi)

    return dist, alpha12, alpha21



if __name__ == '__main__':
    
    select_integer_cells_along_line((0,0), (4, 2), log=True)
    select_integer_cells_along_line((0,0), (-4, -3), log=True)
    select_integer_cells_along_line((5,5), (2, 4), log=True)
