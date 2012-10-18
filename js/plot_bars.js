/* Look which radio button in the page is selected, and return the
 * corresponding JSON key to be used in the small bar charts on top of
 * the page */
function findSelectedKey() {
    var key = "";
    var list_of_ids = [ "map_std_I",
			"map_p2p_I",
			"removed_monopole_I",
			"dipole_I" ];

    for(var i = 0; i < list_of_ids.length; ++i)
    {
	if(document.getElementById(list_of_ids[i]).checked)
	    return list_of_ids[i];
    }

    return "map_std_I";
}

/* Plot the small bars on the left of the object names at the top of
 * each HTML page. The variable `list_of_objects` is an array of
 * associative arrays, each describing one object. */
function plotBars(list_of_objects)
{
    var key_to_plot = findSelectedKey();

    /* We do this in two steps:
     * 1. Iterate over the objects to find which ones need to be
     *    updated and which not. Collect the values to be plotted and
     *    spot the minimum and maximum.
     * 2. Re-iterate over the objects and draw the rectangles in the
     *    appropriate <canvas> element.
     */

    objects_to_plot = [];
    var min = 0.0;
    var max = 0.0;

    /* Iterate over every object */
    for(var i = 0; i < list_of_objects.length; ++i)
    {
	/* If this object does not have the appropriate key, skip it
	 * (remember that we're iterating over maps *and* spectra) */
	if(! list_of_objects[i][key_to_plot])
	    continue;

	/* There are many <canvas> in this page. We must find the one
	 * which matches the `base_file_name` of this object */
	var base_file_name = list_of_objects[i]["base_file_name"];

	/* This call must match `json-obj->HTML-anchor` in
	 * `json-utils.scm`. It takes into account the fact that a
	 * HTML ID must not contain the '/' character (which we
	 * replace with an underscore). */
	var object_id = "div_" + base_file_name.replace(/[-\/]/g, "_");

	var canvas = document.getElementById(object_id);
	if(!canvas)
	    continue;
	
	var value = list_of_objects[i][key_to_plot];

	if(value instanceof Array)
	{
	    /* We assume here that if it is an array, then it is a
	     * vector for which we're interested in its module */
	    var module = 0.0;
	    for(var component = 0;
		component < value.length;
		++component)
	    {
		module += value[component] * value[component];
	    }

	    value = Math.sqrt(module);
	}

	objects_to_plot.push({ "canvas": canvas,
			       "value": value });

	if(i > 0)
	{
	    if(value < min)
		min = value;
	    else if (value > max)
		max = value;
	} else {
	    min = value;
	    max = value;
	}
    }

    if(max > 0.0 && min > 0.0)
	min = 0.0;

    /* Modify the text over the bar charts to reflect the new extrema */
    document.getElementById("barchart_extrema_text").innerHTML =
	"The extrema for the bar chart are " 
	+ String((min * 1e6).toPrecision(3))
	+ " &#x3BC;K and " 
	+ String((max * 1e6).toPrecision(3))
	+ " &#x3BC;K";

    /* Iterate over every object in `objects_to_plot` and paint the bar */
    for(var i = 0; i < objects_to_plot.length; ++i)
    {
	var context = objects_to_plot[i]["canvas"].getContext("2d");
	var value = objects_to_plot[i]["value"];

	/* We clear the rectangle because this might not be the first
	 * time plotBars has been called on this page */
	context.clearRect(0, 0, canvas.width, canvas.height);

	context.fillStyle = "#8080FF";
	context.fillRect(0, 0,
			 (value - min) / (max - min) * canvas.width,
			 canvas.height);

	if(max > 0.0 && min < 0.0) {
	    /* Draw the "zero" line */
	    context.strokeStyle = "#000000";

	    var zero_x = (-min) / (max - min) * canvas.width;
	    context.moveTo(zero_x, 0);
	    context.lineTo(zero_x, canvas.height);
	    context.stroke();
	}
    }
}
