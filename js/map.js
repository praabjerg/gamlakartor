/* **** Leaflet **** */
// This is a modified, configurable variant of the leaflet setup used by gdal2tiles

// Load configuration JSON
function getQueryVariable(variable)
{
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0;i<vars.length;i++) {
        var pair = vars[i].split("=");
        if(pair[0] == variable){return pair[1];}
    }
    return(false);
}

var configurationName = getQueryVariable("configuration");

$.get('configurations/' + configurationName + '/configuration.json',
      function (configuration, textStatus, jqXHR) {
	  // Base layers
	  //  .. OpenStreetMap
	  var osm = L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors', minZoom: data.minZoom, maxZoom: data.maxZoom});

	  //  .. CartoDB Positron
	  var cartodb = L.tileLayer('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="http://cartodb.com/attributions">CartoDB</a>', minZoom: data.minZoom, maxZoom: data.maxZoom});

	  //  .. OSM Toner
	  var toner = L.tileLayer('http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png', {attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', minZoom: data.minZoom, maxZoom: data.maxZoom});

	  //  .. White background
	  var white = L.tileLayer("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQAAAAEAAQMAAABmvDolAAAAA1BMVEX///+nxBvIAAAAH0lEQVQYGe3BAQ0AAADCIPunfg43YAAAAAAAAAAA5wIhAAAB9aK9BAAAAABJRU5ErkJggg==", {minZoom: data.minZoom, maxZoom: data.maxZoom});

	  // Overlay layers (TMS)
	  var lyr = L.tileLayer('./{z}/{x}/{y}.png', {tms: true, opacity: 0.7, attribution: "", minZoom: data.minZoom, maxZoom: data.maxZoom});

	  // Map
	  var map = L.map('map', {
	      center: data.center,
	      zoom: data.maxZoom,
	      minZoom: data.minZoom,
	      maxZoom: data.maxZoom,
	      layers: [osm]
	  });

	  var basemaps = {"OpenStreetMap": osm, "CartoDB Positron": cartodb, "Stamen Toner": toner, "Without background": white}

	  map.addLayer(lyr)
	  var overlaymaps = {"Stockholm": lyr}

	  // Title
	  var title = L.control();
	  title.onAdd = function(map) {
	      this._div = L.DomUtil.create('div', 'ctl title');
	      this.update();
	      return this._div;
	  };
	  title.update = function(props) {
	      this._div.innerHTML = "Stockholm, 1700";
	  };
	  title.addTo(map);

	  // Note
	  var src = 'Generated by <a href="http://www.klokan.cz/projects/gdal2tiles/">GDAL2Tiles</a>, Copyright &copy; 2008 <a href="http://www.klokan.cz/">Klokan Petr Pridal</a>,  <a href="http://www.gdal.org/">GDAL</a> &amp; <a href="http://www.osgeo.org/">OSGeo</a> <a href="http://code.google.com/soc/">GSoC</a>';
	  var title = L.control({position: 'bottomleft'});
	  title.onAdd = function(map) {
	      this._div = L.DomUtil.create('div', 'ctl src');
	      this.update();
	      return this._div;
	  };
	  title.update = function(props) {
	      this._div.innerHTML = src;
	  };
	  title.addTo(map);


	  // Add base layers
	  L.control.layers(basemaps, overlaymaps, {collapsed: false}).addTo(map);

	  // Fit to overlay bounds (SW and NE points with (lat, lon))
	  map.fitBounds(data.bounds);
      });