function js_set_handler(cid, name, cb, obj) {
  obj[name] = function(e) {
    e.persist();
    cb(cid, e);
  };
}

function js_createClass(obj) {
  // Constructor passes through to React
  var cls = function() {
    React.Component.apply(this, arguments); // (super(props))
    // TODO(joel) necessary?
    this.dispatch = this.dispatch.bind(this);
  };

  // Copy parent prototype
  cls.prototype = Object.create(React.Component.prototype);

  cls.prototype.render = function() {
    var gimmeResult = {};
    obj.render(this.componentId, gimmeResult);
    return gimmeResult.value;
  };

  cls.prototype.dispatch = function(e, name) {
    cls[name].call(this, e)
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
