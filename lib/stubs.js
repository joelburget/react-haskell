function js_set_handler(name, cb, obj) {
    obj[name] = function(e) {
        e.persist();
        cb(e);
    };
}

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
