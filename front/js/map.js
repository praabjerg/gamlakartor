/* This is a modified, configurable variant of the leaflet setup generated by gdal2tiles.
 * Thus, the license notice for this part is not GPL, but copied from the gdal2tiles script.

 * Copyright (c) 2008, Klokan Petr Pridal
 * Copyright (c) 2010-2013, Even Rouault <even dot rouault at mines-paris dot org>
 * Copyright (c) 2020, Palle Raabjerg

 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE. */

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

function getMetaAndPositioning(layerid) {
    var meta;
    var positioning;
    var metaPromise = $.get('layers/' + layerid + '/meta.json')
        .then(function(metaResult) {
            meta = metaResult;
        });
    var posPromise = $.get('layers/' + layerid + '/positioning.json')
        .then(function(posResult) {
            positioning = posResult;
        });
    return Promise.all([metaPromise, posPromise])
        .then(function() {
            return {
                meta: meta,
                positioning: positioning
            };
        });
}

function populateLayerArray(layerids) {
    return Promise.map(layerids, function(layerid) {
        return getMetaAndPositioning(layerid);
    });
}

var configurationName = getQueryVariable("configuration");
var mapConfiguration;
var layerids;
var single = false;
$.get('configurations/' + configurationName + '/configuration.json')
    .then(function (configuration) {
        mapConfiguration = configuration;
        if (configuration.conftype === 'single') {
            layerids = [configuration.layer];
        } else {
            layerids = configuration.layers;
        }

        return populateLayerArray(layerids);
    })
    .then(function (layers) {
        const initLayer = layers[0];
        var minZoomMap = 19;
        var maxZoomMap = 0;
        var mapTitle = '';
        layers.forEach(function(layer) {
            if (layer.meta.minZoom < minZoomMap) {
                minZoomMap = layer.meta.minZoom;
            }
            if (layer.meta.maxZoom > maxZoomMap) {
                maxZoomMap = layer.meta.maxZoom;
            }
        });
        if (mapConfiguration.conftype === 'single') {
            mapTitle = initLayer.meta.title;
        } else {
            mapTitle = mapConfiguration.title;
        }
        // Base layers
        //  .. OpenStreetMap
        var osm = L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors', minZoom: minZoomMap, maxZoom: maxZoomMap});

        //  .. CartoDB Positron
        var cartodb = L.tileLayer('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, &copy; <a href="http://cartodb.com/attributions">CartoDB</a>', minZoom: minZoomMap, maxZoom: maxZoomMap});

        //  .. OSM Toner
        var toner = L.tileLayer('http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png', {attribution: 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>.', minZoom: minZoomMap, maxZoom: maxZoomMap});

        //  .. White background
        var white = L.tileLayer("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQAAAAEAAQMAAABmvDolAAAAA1BMVEX///+nxBvIAAAAH0lEQVQYGe3BAQ0AAADCIPunfg43YAAAAAAAAAAA5wIhAAAB9aK9BAAAAABJRU5ErkJggg==", {minZoom: minZoomMap, maxZoom: maxZoomMap});

        var leafLayers = layers.map(function(layer, index) {
            const layerid = layerids[index];
            const sourceLink = '<a href="' + layer.meta.sourceLink + '">' + layer.meta.sourceName + '</a>';
            var licence = layer.meta.licence;
            if (layer.meta.licenceLink !== undefined) {
                licence = '<a href="' + layer.meta.licenceLink + '">' + layer.meta.licence + '</a>';
            }
            const attribution = "Overlay source: " + sourceLink + ", Licence: " + licence;
            return L.tileLayer('layers/' + layerid + '/{z}/{x}/{y}.png', {tms: true, opacity: 0.7, attribution: attribution, minZoom: layer.meta.minZoom, maxZoom: layer.meta.maxZoom});
        });

        // Map
        const center = [initLayer.positioning.center.lat, initLayer.positioning.center.lon];
        var map = L.map('map', {
            center: center,
            zoom: initLayer.meta.maxZoom,
            minZoom: minZoomMap,
            maxZoom: maxZoomMap,
            layers: [osm]
        });

        var basemaps = {"OpenStreetMap": osm, "CartoDB Positron": cartodb, "Stamen Toner": toner, "Without background": white}

        map.addLayer(leafLayers[0]);
        var overlaymaps = {};
        leafLayers.forEach(function(leafLayer, index) {
            var key = layers[index].meta.place + ", " + layers[index].meta.year;
            if (layers[index].meta.layerTitle !== undefined) {
                key = layers[index].meta.layerTitle;
            }
            overlaymaps[key] = leafLayer;
        });

        // Title
        var title = L.control();
        title.onAdd = function(map) {
            this._div = L.DomUtil.create('div', 'ctl title');
            this.update();
            return this._div;
        };
        title.update = function(props) {
            this._div.innerHTML = mapTitle;
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
        const bounds = [[initLayer.positioning.bounds.nw.lat, initLayer.positioning.bounds.nw.lon],
                        [initLayer.positioning.bounds.se.lat, initLayer.positioning.bounds.se.lon]];
        console.log(center);
        console.log(bounds);
        map.fitBounds(bounds);
    });
