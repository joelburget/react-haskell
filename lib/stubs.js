function js_set_handler(name, cb, obj) {
    obj[name] = function(e) {
        e.persist();
        cb(e);
    };
}

function js_createClass(obj) {
  // Constructor passes through to React
  var cls = function() {
    // super(props)
    React.Component.apply(this, arguments);
  };

  // Copy parent prototype
  cls.prototype = Object.create(React.Component.prototype);

  cls.prototype.render = function() {
    var gimmeResult = {};
    obj.render(this.componentId, gimmeResult);
    return gimmeResult.value;
  };

  cls.displayName = obj.displayName;

  // Point constructor to the right place
  cls.prototype.constructor = cls;

  cls.prototype.componentWillMount = function() {
    var gimmeResult = {};
    obj.componentWillMount(gimmeResult);
    this.componentId = gimmeResult.value;
  };

  cls.prototype.componentWillUnmount = function() {
    obj.componentWillUnmount(this.componentId);
  };

  return cls;
}
