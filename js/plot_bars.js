/* Not every browser supports Array.indexOf, so we might need to
 * define our own */

function findObjectById(list_of_objects, object_id)
{
    for(var i = -1, j = list_of_objects.length; ++i < j;)
    {
	if(list_of_objects[i][""] == object_id)
	    return list_of_objects[i];
    }
}

var include = Array.prototype.indexOf ?
    function(arr, obj) { return arr.indexOf(obj) !== -1; } :
    function(arr, obj) {
        for(var i = -1, j = arr.length; ++i < j;)
            if(arr[i] === obj) return true;
        return false;
    };

/* Plot the small bars on the left of the object names at the top of
 * each HTML page. The variable `list_of_objects` is an array of
 * associative arrays, each describing one object. */
function plotBars(list_of_objects)
{
    var key_to_plot = "map_std_I";

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
	var object_id = base_file_name.replace(/[-\/]/g, "_");

	/* This call must match `json-obj->HTML-anchor` in
	 * `json-utils.scm`. It takes into account the fact that a
	 * HTML ID must not contain the '/' character (which we
	 * replace with an underscore). */
	var canvas = document.getElementById(object_id);
	if(!canvas)
	    continue;
	
	var value = list_of_objects[i][key_to_plot];
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

    min = 0.0;

    /* Iterate over every object in `objects_to_plot` and paint the bar */
    for(var i = 0; i < objects_to_plot.length; ++i)
    {
	var context = objects_to_plot[i]["canvas"].getContext("2d");
	var value = objects_to_plot[i]["value"];

	context.fillStyle = "#4040FF";
	context.fillRect(0, 0,
			 (value - min) / (max - min) * canvas.width,
			 canvas.height);
    }
}
