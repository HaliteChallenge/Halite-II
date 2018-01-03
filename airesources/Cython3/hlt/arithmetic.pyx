from libc.math cimport sqrt, atan2

cpdef double distance(double x1, double x2, double y1, double y2) nogil:
    return sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))

cpdef double calculate_angle_between(double x1, double x2, double y1, double y2) nogil:
    cdef double rad
    rad = atan2(y2 - y1, x2 - x1)
    rad = radToDeg(rad)
    rad%=360
    return rad

cpdef double radToDeg(double rad) nogil:
    cdef double deg
    deg=rad/3.141592653589793;
    deg*=180;
    return deg;

cpdef double degToRad(double deg) nogil:
    cdef double rad
    rad=deg*3.141592653589793;
    rad/=180;
    return rad;

