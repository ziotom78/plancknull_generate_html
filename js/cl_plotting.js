/* This will point to the jqplot object */
var plot = null;

/* Return the parameters specified in the URL of the currently opened
 * window as a dictionary */
function getURLParameters()
{
    var urlParams = {};
    var match;
    var pl = /\+/g;  // Regex for replacing addition symbol with a space
    var search = /([^&=]+)=?([^&]*)/g;
    var decode = function (s) { return decodeURIComponent(s.replace(pl, " ")); };
    var query = window.location.search.substring(1);

    while (match = search.exec(query))
	urlParams[decode(match[1])] = decode(match[2]);

    return urlParams;
}

/* Use flot to plot a spectrum into a specified <div>. The file_name
 * parameter is used as a key for the global variable cl_data. */
function plotSpectrum(placeholder, data, title)
{
    options = {
	title: title,
	highlighter: {
            show: true,
            sizeAdjust: 7.5
	},
	cursor: {
            show: true,
            zoom: true,
            showTooltip: false
	},
	axesDefaults: {
            labelRenderer: $.jqplot.CanvasAxisLabelRenderer
	},
	seriesDefaults: {
	    lineWidth: 1.0,
	    showMarker: false
	},
	axes: {
	    xaxis: {
		label: "l",
		renderer: $.jqplot.LogAxisRenderer,
		tickDistribution: 'power'
	    },
	    yaxis: {
		label: "Spectral power [muK^2]",
		renderer: $.jqplot.LogAxisRenderer,
		tickDistribution: 'power'
	    }
	},
	series: [
	    { label: "TT" },
	    { label: "EE" },
	    { label: "BB" }
	],
	legend: {
	    show: true,
	    location: "ne"
	}
    };

    // Call flot and plot the spectra
    plot = $.jqplot(placeholder, data, options);
}

function setupPlot()
{
    // Retrieve information about the plot from the URL parameters
    urlParameters = getURLParameters();
    source_file_name = urlParameters["data_file"];

    /* Now we have to dynamically load the JavaScript file containing
     * the definition for `cl_data`. The idea is to add a <script> tag
     * to <head> which takes care of this. Since modern web browsers
     * usually load scripts asynchronously, we can use `cl_data` only
     * once we're sure the script has been fully loaded. The usage of
     * both `onreadystatechange` and `onload` is to make sure the code
     * works with every browser. */
    var head = document.getElementsByTagName('head')[0];
    var script = document.createElement('script');
    script.type = 'text/javascript';
    script.onreadystatechange = function () {
        if (this.readyState == 'complete')
        {
            plotSpectrum("clplot", cl_data, cl_title);
	    // Removing the newly-created <script> keeps things clean
	    // even if the user reloads the page
            head.removeChild(script);
        }
    }
    script.onload = function() {
        plotSpectrum("clplot", cl_data, cl_title);
	// Ditto as above
        head.removeChild(script);
    }
    script.src = "../" + source_file_name;
    head.appendChild(script);
}
