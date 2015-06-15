function js_set_handler(name, cb, obj) {
    obj[name] = function(e) {
        e.persist();
        cb(e);
    };
}

function js_React_DOM(tagName, attrs) {return React.DOM[tagName](attrs);}

function js_empty() {return [];}
function js_push(a,v) {a.push(v);}

// polyfill from http://www.paulirish.com/2011/requestanimationframe-for-smart-animating/
(function() {
    var lastTime = 0;
    var vendors = ['webkit', 'moz'];
    for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
        window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
        window.cancelAnimationFrame =
          window[vendors[x]+'CancelAnimationFrame'] || window[vendors[x]+'CancelRequestAnimationFrame'];
    }

    if (!window.requestAnimationFrame)
        window.requestAnimationFrame = function(callback, element) {
            var currTime = new Date().getTime();
            var timeToCall = Math.max(0, 16 - (currTime - lastTime));
            var id = window.setTimeout(function() { callback(currTime + timeToCall); },
              timeToCall);
            lastTime = currTime + timeToCall;
            return id;
        };

    if (!window.cancelAnimationFrame)
        window.cancelAnimationFrame = function(id) {
            clearTimeout(id);
        };
}());

function js_createClass(obj) {
  // Constructor passes through to React
  var cls = function() { React.Component.apply(this, arguments); };

  // Copy parent prototype
  cls.prototype = Object.create(React.Component.prototype);

  // Define our own methods
  Object.assign(cls.prototype, obj);

  // Point constructor to the right place
  cls.prototype.constructor = cls;
}
