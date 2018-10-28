import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

(function port_map() {
  function moveend() {
    // console.log('moveend');
    var coordinate = ol.proj.toLonLat(view().getCenter());
    app.ports.mapMoved.send({
      center: {
        lon: coordinate[0],
        lat: coordinate[1]
      },
      zoom: view().getZoom()
    });
  }
  
  app.ports.mapPort.subscribe(function(msg) {
    switch (msg.Cmd) {
      case 'Fly':
        fly(msg.lon, msg.lat, msg.zoom);
        place(msg.geoJson, false);
        break;
      case 'Fit':
        place(msg.geoJson, true);
        break;
    }
  });

  function singleclick(event) {
    var coordinate = ol.proj.toLonLat(event.coordinate);
    fly(coordinate[0], coordinate[1]);
  }
  
  function place(geoJson, fit) {
    _places.clear();
    // console.log('geoJson', geoJson);
    if (geoJson) {
      _places.addFeatures(_geoJsonFormat.readFeatures(geoJson));
      if (fit) {
        view().fit(_places.getExtent(), {
          duration: 300,
          maxZoom: 18
        });
      }
    }
  }
  
  function fly(lon, lat, zoom) {
    var coordinate = ol.proj.fromLonLat([lon, lat]);
    view().animate({
      center: coordinate,
      zoom: zoom,
      duration: 300
    });
  }

  var _map;
  var _places = new ol.source.Vector();
  var _geoJsonFormat = new ol.format.GeoJSON({featureProjection: 'EPSG:3857'});
  function view() {
    if (!_map) {
      _map = new ol.Map({
        target: 'map',
        layers: [
            new ol.layer.Tile({
                source: new ol.source.OSM()
            }),
            new ol.layer.Vector({
              source: _places
            })
        ],
        view: new ol.View({
          center: ol.proj.fromLonLat([0, 0]),
          zoom: 16
      })
    });
      _map.on('singleclick', singleclick);
      _map.on('moveend', moveend);
    }
    return _map.getView();
  }
})();

(function port_dom() {
  app.ports.dom.subscribe(function(msg) {
    switch (msg.Cmd) {
      case 'SelectText':
        selectText(msg.id);
        break;
    }

    function selectText(id) {
      var element = document.getElementById(id);
      if (element && element.select) {
        element.select();
      }
    }
  });
})();

// keep touching the visor icon from scrolling the app
document.getElementById('icon-visor').addEventListener('touchmove', function(event) {
  event.preventDefault();
});
