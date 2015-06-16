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

  cls.prototype.render = function() {
    var gimmeResult = {};
    obj.render(gimmeResult);
    return gimmeResult.value;
  };

  cls.displayName = obj.displayName;

  // Point constructor to the right place
  cls.prototype.constructor = cls;

  return cls;
}
