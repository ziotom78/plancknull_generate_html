#include <math.h>
#include <assert.h>
#include <limits.h>
#include <cairo.h>

#define MAX(a,b) ((a) > (b) ? (a) : (b))

/* Vectors `x2pix` and `y2pix` are used by `ang2pix_nest`. */

const int x2pix[] = {
       0,     1,     4,     5,    16,    17,    20,    21,    64,    65,
      68,    69,    80,    81,    84,    85,   256,   257,   260,   261,
     272,   273,   276,   277,   320,   321,   324,   325,   336,   337,
     340,   341,  1024,  1025,  1028,  1029,  1040,  1041,  1044,  1045,
    1088,  1089,  1092,  1093,  1104,  1105,  1108,  1109,  1280,  1281,
    1284,  1285,  1296,  1297,  1300,  1301,  1344,  1345,  1348,  1349,
    1360,  1361,  1364,  1365,  4096,  4097,  4100,  4101,  4112,  4113,
    4116,  4117,  4160,  4161,  4164,  4165,  4176,  4177,  4180,  4181,
    4352,  4353,  4356,  4357,  4368,  4369,  4372,  4373,  4416,  4417,
    4420,  4421,  4432,  4433,  4436,  4437,  5120,  5121,  5124,  5125,
    5136,  5137,  5140,  5141,  5184,  5185,  5188,  5189,  5200,  5201,
    5204,  5205,  5376,  5377,  5380,  5381,  5392,  5393,  5396,  5397,
    5440,  5441,  5444,  5445,  5456,  5457,  5460,  5461, };

const int y2pix[] = {
        0,     2,     8,    10,    32,    34,    40,    42,   128,   130,
      136,   138,   160,   162,   168,   170,   512,   514,   520,   522,
      544,   546,   552,   554,   640,   642,   648,   650,   672,   674,
      680,   682,  2048,  2050,  2056,  2058,  2080,  2082,  2088,  2090,
     2176,  2178,  2184,  2186,  2208,  2210,  2216,  2218,  2560,  2562,
     2568,  2570,  2592,  2594,  2600,  2602,  2688,  2690,  2696,  2698,
     2720,  2722,  2728,  2730,  8192,  8194,  8200,  8202,  8224,  8226,
     8232,  8234,  8320,  8322,  8328,  8330,  8352,  8354,  8360,  8362,
     8704,  8706,  8712,  8714,  8736,  8738,  8744,  8746,  8832,  8834,
     8840,  8842,  8864,  8866,  8872,  8874, 10240, 10242, 10248, 10250,
    10272, 10274, 10280, 10282, 10368, 10370, 10376, 10378, 10400, 10402,
    10408, 10410, 10752, 10754, 10760, 10762, 10784, 10786, 10792, 10794,
    10880, 10882, 10888, 10890, 10912, 10914, 10920, 10922, };

#define NORMALIZE_ANGLE(x)					\
    {								\
	if(x >= 2.0 * M_PI) x = x - 2.0 * M_PI;			\
	if(x <  0.)         x = x + 2.0 * M_PI;			\
    }

typedef long ang2pix_t(unsigned int, double, double);

long ang2pix_nest(unsigned int nside,
		  double theta,
		  double phi)
{
    double z, z_abs, tt, tp, tmp;
    int    face_num,jp,jm;
    long   ifp, ifm;
    int    ix, iy, ix_low, ix_hi, iy_low, iy_hi, ipf, ntt;
    int    ns_max = 8192;

    z  = cos(theta);
    z_abs = fabs(z);
    NORMALIZE_ANGLE(phi);
    tt = phi / (0.5 * M_PI); /* in [0,4[ */

    if(z_abs <= 2./3.)
    {
	jp = (int) floor(ns_max*(0.5 + tt - z*0.75));
	jm = (int) floor(ns_max*(0.5 + tt + z*0.75));

	ifp = jp / ns_max; /* in {0,4} */
	ifm = jm / ns_max;

	if(ifp == ifm) face_num = (int) fmod(ifp, 4) + 4;
	else if(ifp < ifm) face_num = (int) fmod(ifp, 4);
	else face_num = (int) fmod(ifm, 4) + 8;

	ix = (int) fmod(jm, ns_max);
	iy = ns_max - (int) fmod(jp, ns_max) - 1;
    }
    else { /* polar region, z_abs > 2/3 */

	ntt = (int)floor(tt);
	if( ntt>=4 ) ntt = 3;
	tp = tt - ntt;
	tmp = sqrt( 3.*(1. - z_abs) ); /* in ]0,1] */

	jp = (int)floor( ns_max * tp * tmp );

	jm = (int)floor( ns_max * (1. - tp) * tmp );
	jp = (int)(jp < ns_max-1 ? jp : ns_max-1);
	jm = (int)(jm < ns_max-1 ? jm : ns_max-1);

	if( z>=0 ) {
	    face_num = ntt; /* in {0,3} */
	    ix = ns_max - jm - 1;
	    iy = ns_max - jp - 1;
	}
	else {
	    face_num = ntt + 8; /* in {8,11} */
	    ix =  jp;
	    iy =  jm;
	}
    }

    ix_low = (int)fmod(ix, 128);
    ix_hi  = ix / 128;
    iy_low = (int)fmod(iy, 128);
    iy_hi  = iy / 128;

    ipf = (x2pix[ix_hi]  + y2pix[iy_hi]) * (128 * 128)
	+ (x2pix[ix_low] + y2pix[iy_low]);
    ipf = (long) (ipf / pow(ns_max / nside, 2));
    return (long) (ipf + face_num * nside * nside);
}

long ang2pix_ring(unsigned int nside,
		  double theta,
		  double phi)
{
    int nl2, nl4, ncap, npix;

    nl2 = 2*nside;
    nl4 = 4*nside;
    ncap  = nl2*(nside-1);
    npix  = 12*nside*nside;

    int jp, jm, ipix1;
    double z, z_abs, tt, tp, tmp;
    int ir, ip, kshift;

    z = cos(theta);
    z_abs = fabs(z);
    NORMALIZE_ANGLE(phi);
    tt = phi / (0.5 * M_PI);

    if(z_abs <= 2./3.)
    {
	jp = (int)floor(nside * (0.5 + tt - z*0.75));
	jm = (int)floor(nside * (0.5 + tt + z*0.75));

	ir = nside + 1 + jp - jm;
	kshift = 0;
	if(fmod(ir, 2) == 0.)
	    kshift = 1;

	ip = (int) floor((jp + jm - nside + kshift + 1) / 2) + 1;
	if(ip > nl4)
	    ip = ip - nl4;

	ipix1 = ncap + nl4 * (ir - 1) + ip;
    } else {
	tp = tt - floor(tt);
	tmp = sqrt(3. * (1. - z_abs));

	jp = (int) floor(nside * tp * tmp );
	jm = (int) floor(nside * (1. - tp) * tmp);

	ir = jp + jm + 1;
	ip = (int) floor(tt * ir) + 1;
	if(ip > 4 * ir)
	    ip = ip - 4 * ir;

	ipix1 = 2 * ir * (ir - 1) + ip;
	if (z <= 0.)
	{
	    ipix1 = npix - 2*ir*(ir+1) + ip;
	}
    }
    return ipix1 - 1;
}

#define DEFINE_PLOT_MAP(fn_name,c_type,missed_pixel)		\
    void fn_name(const c_type * map, unsigned int nside,	\
		 unsigned int width, unsigned int height,	\
		 int ordering_is_ring,				\
		 c_type * screen_points)			\
    {								\
	unsigned int cur_x;					\
	unsigned int cur_y;					\
	const size_t num_of_screen_points = width * height;	\
	const float center_x = width * 0.5;			\
	const float center_y = height * 0.5;			\
	c_type * cur_screen_point = screen_points;		\
	ang2pix_t *ang2pix_fn =					\
	    ordering_is_ring ? ang2pix_ring : ang2pix_nest;	\
								\
	for(cur_y = 0; cur_y < height; ++cur_y)			\
	{							\
	    for(cur_x = 0; cur_x < width; ++cur_x)		\
	    {							\
		double u = 2 * (cur_x - center_x) / (center_x / 1.02);	\
		double v = (cur_y - center_y) / (center_y / 1.02);	\
		double theta;					\
		double phi;					\
		long pixel_idx;					\
								\
		if(u*u/4 + v*v >= 1)				\
		{						\
		    *cur_screen_point++ = missed_pixel;		\
		    continue;					\
		}						\
								\
		theta = M_PI_2 - asin(2 / M_PI *		\
				      (asin(v) +			\
				       v * sqrt((1 - v) * (1 + v))));	\
		phi = -M_PI_2 * u / MAX(sqrt((1 - v) * (1 + v)), 1e-6);	\
		pixel_idx = ang2pix_fn(nside, theta, phi);		\
		*cur_screen_point++ = map[pixel_idx];			\
	    }								\
	}								\
    }

DEFINE_PLOT_MAP(plot_short_map, short, SHRT_MAX)
DEFINE_PLOT_MAP(plot_ushort_map, unsigned short, USHRT_MAX)
DEFINE_PLOT_MAP(plot_long_map, long, LONG_MAX)
DEFINE_PLOT_MAP(plot_ulong_map, unsigned long, ULONG_MAX)
DEFINE_PLOT_MAP(plot_float_map, float, INFINITY)
DEFINE_PLOT_MAP(plot_double_map, double, INFINITY)

void get_map_extrema(const float * map, unsigned int nside,
		     float * min, float * max)
{
    size_t num_of_pixels = nside * nside * 12;
    const float * end_of_map = map + num_of_pixels;
    const float * cur_pixel = map;
    int is_first = 1;

    while(cur_pixel != end_of_map)
    {
	if(! isinff(*cur_pixel) &&
	   ! isnanf(*cur_pixel) &&
	   *cur_pixel > -1.6e+30)
	{
	    if(! is_first)
	    {
		if(*cur_pixel < *min)
		    *min = *cur_pixel;
		else if(*cur_pixel > *max)
		    *max = *cur_pixel;
	    }
	    else
	    {
		*min = *max = *cur_pixel;
		is_first = 0;
	    }
	}

	cur_pixel++;
    }
}

typedef struct {
    double red;
    double green;
    double blue;
} color_t;

static const double levels[] = { 0.0, 0.15, 0.40, 0.70, 0.90, 1.00 };
static const color_t colors[] = {
    { 0.0, 0.0, 0.5 },
    { 0.0, 0.0, 1.0 },
    { 0.0, 1.0, 1.0 },
    { 1.0, 1.0, 0.0 },
    { 1.0, 0.33, 0.0 },
    { 0.5, 0.0, 0.0 }};
static const size_t num_of_levels = sizeof(levels) / sizeof(levels[0]);

void
get_palette_color(double level, color_t * color_ptr)
{
    size_t idx;
    size_t index0, index1;

    if(level <= 0.0)
    {
	memcpy(color_ptr, (const void *) &colors[0],
	       sizeof(color_t));
	return;
    }

    if(level >= 1.0)
    {
	memcpy(color_ptr, (const void *) &colors[num_of_levels - 1],
	       sizeof(color_t));
	return;
    }

    idx = 0;
    while(level > levels[idx])
	++idx;

    index1 = idx;
    index0 = index1 - 1;

#define INTERPOLATE_COMPONENT(level, comp_name) \
    (  colors[index0].comp_name * (levels[index1] - level) / (levels[index1] - levels[index0]) \
     + colors[index1].comp_name * (level - levels[index0]) / (levels[index1] - levels[index0]))

    color_ptr->red   = INTERPOLATE_COMPONENT(level, red);
    color_ptr->green = INTERPOLATE_COMPONENT(level, green);
    color_ptr->blue  = INTERPOLATE_COMPONENT(level, blue);

#undef INTERPOLATE_COMPONENT
}

void
plot_gradient_bar(cairo_t * cairo_context,
		  double origin_x, double origin_y,
		  double size_x, double size_y,
		  double min_value, double max_value,
		  const char * measure_unit_str,
		  int vertical_flag)
{
    cairo_pattern_t * linear;
    size_t idx;
    char label_min[20], label_max[20];
    cairo_text_extents_t min_te, max_te;
    double bar_origin_x, bar_origin_y;
    double bar_size_x, bar_size_y;
    const double text_margin_factor = 1.1;
    const double tick_height = 6.0;

    if(measure_unit_str != NULL
       && measure_unit_str[0] != '\0')
    {
	sprintf(label_min, "%.4g %s", min_value, measure_unit_str);
	sprintf(label_max, "%.4g %s", max_value, measure_unit_str);
    } else {
	sprintf(label_min, "%.4g", min_value);
	sprintf(label_max, "%.4g", max_value);
    }

    cairo_text_extents (cairo_context, label_min, &min_te);
    cairo_text_extents (cairo_context, label_max, &max_te);

    bar_origin_x = origin_x;
    bar_origin_y = origin_y;
    bar_size_x = size_x;
    bar_size_y = size_y;

    /* If zero is within the range, plot a small thick around it */
    if(max_value > 0.0 && min_value < 0.0)
    {
	double zero_pos = 
	    origin_x + size_x * (0.0 - min_value) / (max_value - min_value);
	cairo_move_to(cairo_context, zero_pos, origin_y);
	cairo_line_to(cairo_context, zero_pos, origin_y + size_y);
	cairo_set_source_rgb(cairo_context, 0.0, 0.0, 0.0);
	cairo_stroke(cairo_context);
    }

    /* Now plot the gradient */
    if(vertical_flag)
    {
	size_x -= MAX(min_te.width, max_te.width) * text_margin_factor; 
	linear = cairo_pattern_create_linear (0.0, bar_origin_y,
					      0.0, bar_origin_y + bar_size_y);
    }
    else
    {
	bar_origin_x += min_te.width * text_margin_factor;
	bar_size_x -= (min_te.width + max_te.width) * text_margin_factor;
	linear = cairo_pattern_create_linear (bar_origin_x, 0.0,
					      bar_origin_x + bar_size_x, 0.0);
    }

    for(idx = 0; idx < num_of_levels; ++idx)
    {
	cairo_pattern_add_color_stop_rgb(linear, levels[idx],
					 colors[idx].red,
					 colors[idx].green,
					 colors[idx].blue);
    }

    cairo_rectangle(cairo_context,
		    bar_origin_x, bar_origin_y + tick_height,
		    bar_size_x, bar_size_y - 2 * tick_height);

    /* Draw the gradient */
    cairo_set_source(cairo_context, linear);
    cairo_fill_preserve(cairo_context);

    cairo_pattern_destroy(linear);

    /* Draw the border */
    cairo_set_source_rgb(cairo_context, 0.0, 0.0, 0.0);
    cairo_stroke(cairo_context);

    /* Draw the labels */
    {
	double baseline =
	    origin_y
	    + size_y * 0.5 
	    - min_te.y_bearing 
	    - min_te.height * 0.5;
	cairo_move_to(cairo_context, origin_x, baseline);
	cairo_show_text(cairo_context, label_min);

	 baseline =
	     origin_y
	     + size_y * 0.5 
	     - max_te.y_bearing 
	     - max_te.height * 0.5;
	 cairo_move_to(cairo_context, origin_x + size_x - max_te.width,
		       baseline);
	 cairo_show_text(cairo_context, label_max);
    }
}

void
plot_bitmap_to_cairo_surface(cairo_t * cairo_context,
			     double origin_x, double origin_y,
			     double size_x, double size_y,
			     double map_min, double map_max,
			     const float * bitmap,
			     unsigned int bitmap_width,
			     unsigned int bitmap_height)
{
    const double pixel_width  = size_x / bitmap_width;
    const double pixel_height = size_y / bitmap_height;
    const double dynamic_range = map_max - map_min;
    const float * cur_pixel = bitmap;

    unsigned int cur_y;

    for(cur_y = bitmap_height; cur_y > 0; --cur_y)
    {
	unsigned int cur_x;
	for(cur_x = 0; cur_x < bitmap_width; ++cur_x)
	{
	    double value = *bitmap++;
	    color_t color;

	    if(isinff(value))
		continue;
	    else if (isnanf(value) || value < -1.6e+30)
		color.red = color.green = color.blue = 0.5;
	    else
	    {
		double normalized_value = (value - map_min) / dynamic_range;
		get_palette_color(normalized_value, &color);
	    }

	    cairo_rectangle(cairo_context,
			    origin_x + (cur_x * size_x) / bitmap_width,
			    origin_y + (cur_y * size_y) / bitmap_height,
			    pixel_width,
			    pixel_height);
	    cairo_set_source_rgb(cairo_context,
				 color.red,
				 color.green,
				 color.blue);
	    cairo_fill(cairo_context);
	}
    }
}
